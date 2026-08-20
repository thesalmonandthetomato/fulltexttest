import csv, gzip, hashlib, json, os, re, tarfile, time
from pathlib import Path
from urllib.parse import quote
import requests
from lxml import etree
from pypdf import PdfReader

FULLTEXT_TARGET = 100
OPENALEX_CONTENT_LIMIT = 100
MANIFEST = Path('data/living_evidence_map_master.csv')
STATE = Path('data/openalex_free_state.json')
STAGE = Path('daily_openalex_batch')
STAGE.mkdir(exist_ok=True)

session = requests.Session()
session.headers['User-Agent'] = 'fulltexttest/openalex-free-daily/3.0'

def log(msg):
    print(f'PROGRESS: {msg}', flush=True)

def norm(doi):
    return re.sub(r'^(?:https?://doi.org/|doi:)', '', doi.strip(), flags=re.I).rstrip(' .;,').lower()

def request(method, url, **kwargs):
    # Hard bound: no individual HTTP operation can hold the job indefinitely.
    kwargs.setdefault('timeout', (10, 30))
    for attempt in range(4):
        log(f'HTTP {method} {url[:160]} (attempt {attempt+1}/4)')
        try:
            r = session.request(method, url, allow_redirects=True, **kwargs)
        except Exception as e:
            log(f'HTTP ERROR {type(e).__name__}: {e}')
            if attempt == 3:
                return None
            time.sleep(2 ** attempt)
            continue
        if r.status_code not in (429, 500, 502, 503, 504) or attempt == 3:
            return r
        retry = r.headers.get('Retry-After')
        try:
            delay = min(60, float(retry)) if retry else min(60, 2 ** attempt)
        except ValueError:
            delay = min(60, 2 ** attempt)
        log(f'HTTP {r.status_code}; backing off {delay:.0f}s')
        time.sleep(delay)
    return None

def parse_pdf(data):
    p = Path('/tmp/fulltexttest.pdf')
    p.write_bytes(data)
    try:
        reader = PdfReader(str(p))
        return '\n\n'.join(page.extract_text() or '' for page in reader.pages)
    finally:
        p.unlink(missing_ok=True)

def parse_xml(data):
    root = etree.fromstring(data)
    return '\n'.join(' '.join(''.join(n.itertext()).split()) for n in root.xpath('//*[local-name()="body"]')).strip()

def store(doi, work, source, url, data, content_type):
    raw = gzip.decompress(data) if data[:2] == b'\x1f\x8b' else data
    ct = (content_type or '').lower()
    try:
        if raw.startswith(b'%PDF') or 'pdf' in ct:
            text = parse_pdf(raw)
            if len(text) < 3000:
                return None, 'pdf_parse_failure'
            ext, fmt, stored_bytes = 'pdf.gz', 'pdf', gzip.compress(raw, 6)
        elif 'xml' in ct or raw.lstrip().startswith(b'<?xml') or b'<TEI' in raw[:1000]:
            text = parse_xml(raw)
            if len(text) < 3000:
                return None, 'xml_parse_failure'
            ext, fmt, stored_bytes = 'xml.gz', 'xml', gzip.compress(raw, 6)
        else:
            return None, 'unsupported_content_type'
    except Exception as e:
        return None, f'parse_error:{type(e).__name__}'
    checksum = hashlib.sha256(raw).hexdigest()
    slug = hashlib.sha256(doi.encode()).hexdigest()[:20]
    dest = STAGE / slug
    dest.mkdir(parents=True, exist_ok=True)
    (dest / f'fulltext.{ext}').write_bytes(stored_bytes)
    audit = {
        'doi': doi, 'openalex_id': work.get('id'), 'title': work.get('display_name'),
        'publication_year': work.get('publication_year'), 'type': work.get('type'),
        'open_access': work.get('open_access'), 'best_oa_location': work.get('best_oa_location'),
        'locations': work.get('locations', []), 'has_content': work.get('has_content') or {},
        'source': source, 'source_url': url,
        'retrieved_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
        'text_chars': len(text), 'sha256': checksum, 'format': fmt
    }
    (dest / 'metadata.json').write_text(json.dumps(audit, indent=2, ensure_ascii=False), encoding='utf-8')
    return {'doi': doi, 'openalex_id': work.get('id'), 'format': fmt, 'text_chars': len(text), 'sha256': checksum, 'source': source, 'source_url': url}, None

def try_content(doi, work, source, url):
    if not url or not str(url).startswith('http'):
        return None, 'invalid_url'
    r = request('GET', url)
    if r is None:
        return None, 'request_failed'
    if not r.ok or not r.content:
        return None, f'http_{r.status_code}'
    return store(doi, work, source, r.url, r.content, r.headers.get('content-type', ''))

key = os.environ.get('OPENALEX_API_KEY')
token = os.environ.get('ZENODO_TOKEN')
if not key or not token:
    raise SystemExit('OPENALEX_API_KEY and ZENODO_TOKEN are required')
if not MANIFEST.exists():
    raise SystemExit(f'Missing {MANIFEST}')

state = json.loads(STATE.read_text()) if STATE.exists() else {'completed': {}, 'failed': {}}
completed = set(state.get('completed', {}))
rows = []
with MANIFEST.open(newline='', encoding='utf-8') as f:
    for row in csv.DictReader(f):
        doi = norm(row.get('doi', ''))
        if doi and doi not in completed:
            rows.append(doi)

log(f'START queue={len(rows)} target_fulltexts={FULLTEXT_TARGET} openalex_content_limit={OPENALEX_CONTENT_LIMIT}')
successes, failures = [], []
content_requests = 0
start = time.time()

for index, doi in enumerate(rows, 1):
    if len(successes) >= FULLTEXT_TARGET:
        break
    log(f'DOI {index}/{len(rows)} | successes={len(successes)}/{FULLTEXT_TARGET} | openalex_content={content_requests}/{OPENALEX_CONTENT_LIMIT} | {doi}')
    try:
        meta_url = 'https://api.openalex.org/works/https://doi.org/' + quote(doi, safe='')
        log(f'METADATA {doi}')
        r = request('GET', meta_url, params={'api_key': key})
        if r is None or not r.ok:
            status = 'metadata_request_failed' if r is None else f'metadata_http_{r.status_code}'
            failures.append({'doi': doi, 'status': status})
            continue
        work = r.json()
        got = None
        error = None

        # Free external locations only: use explicit PDF URLs. Do not crawl landing pages.
        locations = []
        if work.get('best_oa_location'):
            locations.append(work['best_oa_location'])
        locations.extend(work.get('locations') or [])
        seen = set()
        for loc in locations:
            if not isinstance(loc, dict):
                continue
            u = loc.get('pdf_url')
            if u and u not in seen:
                seen.add(u)
                log(f'EXTERNAL OA PDF {doi}')
                got, error = try_content(doi, work, 'OpenAlex OA location', u)
                if got:
                    break

        # Europe PMC XML, when the DOI maps exactly to a PMCID.
        if not got:
            log(f'EUROPE PMC LOOKUP {doi}')
            e = request('GET', 'https://www.ebi.ac.uk/europepmc/webservices/rest/search',
                        params={'query': f'DOI:"{doi}"', 'resultType': 'core', 'format': 'json'})
            if e is not None and e.ok:
                hits = e.json().get('resultList', {}).get('result', [])
                exact = [h for h in hits if (h.get('doi') or '').strip().lower() == doi]
                if exact and exact[0].get('pmcid'):
                    pmcid = exact[0]['pmcid']
                    log(f'EUROPE PMC XML {doi} -> {pmcid}')
                    got, error = try_content(doi, work, 'Europe PMC XML',
                                             f'https://www.ebi.ac.uk/europepmc/webservices/rest/{pmcid}/fullTextXML')

        # Unpaywall API + explicit PDF URLs only.
        if not got:
            log(f'UNPAYWALL LOOKUP {doi}')
            u = request('GET', 'https://api.unpaywall.org/v2/' + quote(doi, safe=''),
                        params={'email': os.environ.get('UNPAYWALL_EMAIL', 'fulltexttest@example.org')})
            if u is not None and u.ok:
                ud = u.json()
                ulocs = ([ud.get('best_oa_location')] if ud.get('best_oa_location') else []) + (ud.get('oa_locations') or [])
                seen = set()
                for loc in ulocs:
                    if not isinstance(loc, dict):
                        continue
                    url = loc.get('url_for_pdf')
                    if url and url not in seen:
                        seen.add(url)
                        log(f'UNPAYWALL PDF {doi}')
                        got, error = try_content(doi, work, 'Unpaywall OA location', url)
                        if got:
                            break

        # OpenAlex cached content is the final fallback and is strictly capped.
        has = work.get('has_content') or {}
        wid = (work.get('id') or '').rstrip('/').split('/')[-1]
        if not got and content_requests < OPENALEX_CONTENT_LIMIT and wid and (has.get('grobid_xml') or has.get('pdf')):
            kind = 'tei-xml' if has.get('grobid_xml') else 'pdf'
            url = f'https://content.openalex.org/works/{wid}.grobid-xml' if kind == 'tei-xml' else f'https://content.openalex.org/works/{wid}.pdf'
            content_requests += 1
            log(f'OPENALEX CONTENT {content_requests}/{OPENALEX_CONTENT_LIMIT} {kind} {doi}')
            got, error = try_content(doi, work, 'OpenAlex cached content', url)

        if got:
            successes.append(got)
            elapsed = time.time() - start
            log(f'SUCCESS {len(successes)}/{FULLTEXT_TARGET} {doi} source={got["source"]} elapsed={elapsed/60:.1f}m')
        else:
            failures.append({'doi': doi, 'status': error or 'no_fulltext'})
            log(f'FAIL {doi}: {error or "no_fulltext"}')
    except Exception as e:
        failures.append({'doi': doi, 'status': 'unexpected_error', 'error': repr(e)})
        log(f'ERROR {doi}: {e!r}')

log(f'RETRIEVAL COMPLETE successes={len(successes)} openalex_content={content_requests} failures={len(failures)}')
if not successes:
    raise SystemExit('No full texts retrieved; refusing to create an empty archive')

batch_date = time.strftime('%Y-%m-%d', time.gmtime())
run_id = os.environ.get('GITHUB_RUN_ID', 'manual')
archive = Path(f'openalex-free-{batch_date}-{run_id}.tar.gz')
with tarfile.open(archive, 'w:gz') as tf:
    tf.add(STAGE, arcname='fulltext')
log(f'ARCHIVE {archive.name} {archive.stat().st_size/1024/1024:.1f} MiB')

headers = {'Authorization': f'Bearer {token}'}
log('ZENODO CREATE')
r = requests.post('https://zenodo.org/api/deposit/depositions', json={}, headers={**headers, 'Content-Type': 'application/json'}, timeout=(10, 60))
r.raise_for_status()
dep = r.json(); dep_id = dep['id']; bucket = dep['links']['bucket']
log(f'ZENODO CREATED id={dep_id}')
metadata = {'metadata': {'title': f'fulltexttest free full-text batch {batch_date} ({len(successes)} works)', 'upload_type': 'dataset', 'publication_date': batch_date, 'description': 'Daily batch of full texts retrieved without exceeding the OpenAlex free daily content-download allowance. Per-work provenance and SHA-256 checksums are included.', 'access_right': 'restricted', 'access_conditions': 'Restricted to the depositor/project for research analysis.'}}
r = requests.put(f'https://zenodo.org/api/deposit/depositions/{dep_id}', json=metadata, headers={**headers, 'Content-Type': 'application/json'}, timeout=(10, 60)); r.raise_for_status()
log('ZENODO METADATA SAVED')
with archive.open('rb') as fp:
    log(f'ZENODO UPLOAD {archive.stat().st_size/1024/1024:.1f} MiB')
    r = requests.put(f'{bucket}/{archive.name}', data=fp, headers=headers, timeout=(10, 1800)); r.raise_for_status()
log('ZENODO UPLOAD COMPLETE')
r = requests.post(f'https://zenodo.org/api/deposit/depositions/{dep_id}/actions/publish', headers=headers, timeout=(10, 120)); r.raise_for_status()
published = r.json()
log(f'ZENODO PUBLISHED id={published.get("id")} doi={published.get("doi")}')

for item in successes:
    state.setdefault('completed', {})[item['doi']] = {'date': batch_date, 'run_id': run_id, 'zenodo_record': published.get('id'), 'zenodo_doi': published.get('doi'), 'sha256': item['sha256'], 'format': item['format'], 'source': item['source'], 'source_url': item['source_url']}
for item in failures:
    state.setdefault('failed', {})[item['doi']] = {'date': batch_date, 'status': item['status']}
STATE.write_text(json.dumps(state, indent=2, ensure_ascii=False), encoding='utf-8')
Path('daily-results.json').write_text(json.dumps({'date': batch_date, 'run_id': run_id, 'fulltexts_archived': len(successes), 'openalex_content_requests': content_requests, 'failures': failures, 'zenodo': {'record_id': published.get('id'), 'doi': published.get('doi')}}, indent=2, ensure_ascii=False), encoding='utf-8')
log('CHECKPOINT STATE WRITTEN AFTER ZENODO PUBLICATION')
