import csv, gzip, hashlib, json, os, re, tarfile, time
from pathlib import Path
from urllib.parse import quote
import requests
from lxml import etree
from pypdf import PdfReader

FULLTEXT_TARGET = 100
BATCH_SIZE = 100
MANIFEST = Path('data/living_evidence_map_master.csv')
STATE = Path('data/openalex_free_state.json')
STAGE = Path('daily_openalex_batch')
STAGE.mkdir(exist_ok=True)

session = requests.Session()
session.headers['User-Agent'] = 'fulltexttest/openalex-external-oa/4.0'


def log(msg):
    print(f'PROGRESS: {msg}', flush=True)


def norm(doi):
    return re.sub(r'^(?:https?://doi.org/|doi:)', '', doi.strip(), flags=re.I).rstrip(' .;,').lower()


def request(method, url, **kwargs):
    kwargs.setdefault('timeout', (10, 30))
    for attempt in range(4):
        log(f'HTTP {method} {url[:180]} (attempt {attempt + 1}/4)')
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


def store(input_doi, work, source, url, data, content_type):
    raw = gzip.decompress(data) if data[:2] == b'\x1f\x8b' else data
    ct = (content_type or '').lower()
    try:
        if raw.startswith(b'%PDF') or 'pdf' in ct:
            text = parse_pdf(raw)
            if len(text) < 3000:
                return None, 'pdf_parse_failure'
            ext, fmt, stored_bytes = 'pdf.gz', 'pdf', gzip.compress(raw, 6)
        else:
            return None, 'not_pdf'
    except Exception as e:
        return None, f'parse_error:{type(e).__name__}'

    openalex_doi = work.get('doi') or ''
    checksum = hashlib.sha256(raw).hexdigest()
    slug = hashlib.sha256(input_doi.encode()).hexdigest()[:20]
    dest = STAGE / slug
    dest.mkdir(parents=True, exist_ok=True)
    (dest / f'fulltext.{ext}').write_bytes(stored_bytes)

    audit = {
        'input_doi': input_doi,
        'openalex_doi': openalex_doi,
        'doi_match': norm(openalex_doi) == input_doi,
        'openalex_id': work.get('id'),
        'title': work.get('display_name'),
        'publication_year': work.get('publication_year'),
        'type': work.get('type'),
        'open_access': work.get('open_access'),
        'best_oa_location': work.get('best_oa_location'),
        'locations': work.get('locations', []),
        'has_content': work.get('has_content') or {},
        'source': source,
        'source_url': url,
        'retrieved_at': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
        'text_chars': len(text),
        'sha256': checksum,
        'format': fmt,
    }
    (dest / 'metadata.json').write_text(json.dumps(audit, indent=2, ensure_ascii=False), encoding='utf-8')
    return {
        'input_doi': input_doi,
        'openalex_doi': openalex_doi,
        'doi_match': True,
        'openalex_id': work.get('id'),
        'format': fmt,
        'text_chars': len(text),
        'sha256': checksum,
        'source': source,
        'source_url': url,
    }, None


def download_external_pdf(input_doi, work, pdf_url):
    if not pdf_url or not str(pdf_url).startswith('http'):
        return None, 'invalid_pdf_url'
    log(f'PDF DOWNLOAD input={input_doi} openalex={work.get("doi")} url={pdf_url}')
    r = request('GET', pdf_url)
    if r is None:
        return None, 'pdf_request_failed'
    if not r.ok or not r.content:
        return None, f'pdf_http_{r.status_code}'
    result, error = store(input_doi, work, 'OpenAlex-discovered external OA PDF', r.url, r.content, r.headers.get('content-type', ''))
    return result, error


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

log(f'START queue={len(rows)} target_fulltexts={FULLTEXT_TARGET} openalex_content_downloads=0')
successes, failures, pdf_manifest = [], [], []
start = time.time()

for batch_start in range(0, len(rows), BATCH_SIZE):
    if len(successes) >= FULLTEXT_TARGET:
        break
    batch = rows[batch_start:batch_start + BATCH_SIZE]
    batch_no = batch_start // BATCH_SIZE + 1
    log(f'OPENALEX BULK BATCH {batch_no} | input_dois={len(batch)} | successes={len(successes)}/{FULLTEXT_TARGET}')

    doi_values = '|'.join('https://doi.org/' + d for d in batch)
    params = {
        'filter': 'doi:' + doi_values,
        'per_page': 100,
        'api_key': key,
        'select': 'id,doi,display_name,publication_year,publication_date,type,language,open_access,best_oa_location,locations,has_content',
    }
    log(f'OPENALEX METADATA BULK request for {len(batch)} DOIs')
    r = request('GET', 'https://api.openalex.org/works', params=params)
    if r is None:
        failures.extend({'doi': d, 'status': 'bulk_metadata_request_failed'} for d in batch)
        log(f'BULK FAILURE: request failed for batch {batch_no}')
        continue
    if not r.ok:
        reset = r.headers.get('X-RateLimit-Reset')
        remaining = r.headers.get('X-RateLimit-Remaining')
        log(f'BULK FAILURE: HTTP {r.status_code} remaining={remaining} reset={reset}')
        failures.extend({'doi': d, 'status': f'bulk_metadata_http_{r.status_code}'} for d in batch)
        continue

    data = r.json()
    works = data.get('results', [])
    log(f'OPENALEX BULK RETURNED {len(works)} works | remaining={r.headers.get("X-RateLimit-Remaining")} credits_used={r.headers.get("X-RateLimit-Credits-Used")}')
    by_doi = {}
    for work in works:
        od = norm(work.get('doi') or '')
        if od:
            by_doi.setdefault(od, []).append(work)

    for input_doi in batch:
        if len(successes) >= FULLTEXT_TARGET:
            break
        matches = by_doi.get(input_doi, [])
        if not matches:
            pdf_manifest.append({'input_doi': input_doi, 'openalex_doi': '', 'doi_match': False, 'pdf_url': '', 'status': 'no_openalex_match'})
            failures.append({'doi': input_doi, 'status': 'no_openalex_match'})
            log(f'NO MATCH input={input_doi}')
            continue

        work = matches[0]
        openalex_doi = norm(work.get('doi') or '')
        if openalex_doi != input_doi:
            pdf_manifest.append({'input_doi': input_doi, 'openalex_doi': openalex_doi, 'doi_match': False, 'pdf_url': '', 'status': 'doi_mismatch'})
            failures.append({'doi': input_doi, 'status': 'doi_mismatch', 'openalex_doi': openalex_doi})
            log(f'DOI MISMATCH input={input_doi} openalex={openalex_doi}')
            continue

        locations = []
        if isinstance(work.get('best_oa_location'), dict):
            locations.append(work['best_oa_location'])
        locations.extend(x for x in (work.get('locations') or []) if isinstance(x, dict))
        pdf_urls = []
        seen = set()
        for loc in locations:
            u = loc.get('pdf_url')
            if u and u not in seen:
                seen.add(u)
                pdf_urls.append(u)

        if not pdf_urls:
            pdf_manifest.append({'input_doi': input_doi, 'openalex_doi': work.get('doi', ''), 'doi_match': True, 'pdf_url': '', 'status': 'no_pdf_url'})
            failures.append({'doi': input_doi, 'status': 'no_pdf_url'})
            log(f'NO PDF URL input={input_doi}')
            continue

        log(f'PDF URLS input={input_doi} count={len(pdf_urls)} first={pdf_urls[0]}')
        got = None
        last_error = None
        for pdf_url in pdf_urls:
            pdf_manifest.append({'input_doi': input_doi, 'openalex_doi': work.get('doi', ''), 'doi_match': True, 'pdf_url': pdf_url, 'status': 'attempted'})
            got, last_error = download_external_pdf(input_doi, work, pdf_url)
            if got:
                pdf_manifest[-1]['status'] = 'retrieved'
                break
            pdf_manifest[-1]['status'] = last_error or 'download_failed'

        if got:
            successes.append(got)
            elapsed = time.time() - start
            log(f'SUCCESS {len(successes)}/{FULLTEXT_TARGET} input={input_doi} source={got["source"]} elapsed={elapsed/60:.1f}m')
        else:
            failures.append({'doi': input_doi, 'status': last_error or 'external_pdf_failed'})
            log(f'FAIL input={input_doi}: {last_error or "external_pdf_failed"}')

log(f'RETRIEVAL COMPLETE successes={len(successes)} openalex_content_downloads=0 failures={len(failures)}')

batch_date = time.strftime('%Y-%m-%d', time.gmtime())
run_id = os.environ.get('GITHUB_RUN_ID', 'manual')
manifest_path = Path('daily-pdf-manifest.csv')
with manifest_path.open('w', newline='', encoding='utf-8') as f:
    fields = ['input_doi', 'openalex_doi', 'doi_match', 'pdf_url', 'status']
    w = csv.DictWriter(f, fieldnames=fields)
    w.writeheader()
    w.writerows(pdf_manifest)

if not successes:
    Path('daily-results.json').write_text(json.dumps({
        'date': batch_date, 'run_id': run_id, 'fulltexts_archived': 0,
        'openalex_content_downloads': 0, 'failures': failures,
        'pdf_urls_found': sum(1 for x in pdf_manifest if x['pdf_url']),
        'doi_matches': sum(1 for x in pdf_manifest if x['doi_match']),
    }, indent=2, ensure_ascii=False), encoding='utf-8')
    raise SystemExit('No external OA full texts retrieved; metadata manifest has been written')

archive = Path(f'openalex-external-oa-{batch_date}-{run_id}.tar.gz')
with tarfile.open(archive, 'w:gz') as tf:
    tf.add(STAGE, arcname='fulltext')
    tf.add(manifest_path, arcname='daily-pdf-manifest.csv')
log(f'ARCHIVE {archive.name} {archive.stat().st_size/1024/1024:.1f} MiB')

headers = {'Authorization': f'Bearer {token}'}
log('ZENODO CREATE')
r = requests.post('https://zenodo.org/api/deposit/depositions', json={}, headers={**headers, 'Content-Type': 'application/json'}, timeout=(10, 60))
r.raise_for_status()
dep = r.json(); dep_id = dep['id']; bucket = dep['links']['bucket']
log(f'ZENODO CREATED id={dep_id}')
metadata = {
    'metadata': {
        'title': f'fulltexttest external OA full-text batch {batch_date} ({len(successes)} works)',
        'upload_type': 'dataset',
        'publication_date': batch_date,
        'description': 'Daily batch of full texts retrieved from external open-access PDF URLs discovered in OpenAlex metadata. The full texts were not downloaded from OpenAlex content services. Per-work input DOI, matched OpenAlex DOI, source URL, provenance, and SHA-256 checksums are included.',
        'access_right': 'restricted',
        'access_conditions': 'Restricted to the depositor/project for research analysis.',
    }
}
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
    state.setdefault('completed', {})[item['input_doi']] = {
        'date': batch_date,
        'run_id': run_id,
        'zenodo_record': published.get('id'),
        'zenodo_doi': published.get('doi'),
        'sha256': item['sha256'],
        'format': item['format'],
        'source': item['source'],
        'source_url': item['source_url'],
        'openalex_id': item['openalex_id'],
        'openalex_doi': item['openalex_doi'],
        'doi_match': item['doi_match'],
    }
for item in failures:
    state.setdefault('failed', {})[item['doi']] = {
        'date': batch_date,
        'status': item['status'],
        **({'openalex_doi': item['openalex_doi']} if 'openalex_doi' in item else {}),
    }
STATE.write_text(json.dumps(state, indent=2, ensure_ascii=False), encoding='utf-8')
Path('daily-results.json').write_text(json.dumps({
    'date': batch_date,
    'run_id': run_id,
    'fulltexts_archived': len(successes),
    'openalex_content_downloads': 0,
    'doi_matches': sum(1 for x in pdf_manifest if x['doi_match']),
    'pdf_urls_found': sum(1 for x in pdf_manifest if x['pdf_url']),
    'failures': failures,
    'zenodo': {'record_id': published.get('id'), 'doi': published.get('doi')},
}, indent=2, ensure_ascii=False), encoding='utf-8')
log('CHECKPOINT STATE WRITTEN AFTER ZENODO PUBLICATION')
