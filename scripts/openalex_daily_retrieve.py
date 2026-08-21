import csv, gzip, hashlib, json, os, re, tarfile, time
from collections import defaultdict
from pathlib import Path
import requests
from lxml import etree
from pypdf import PdfReader

MANIFEST = Path('data/living_evidence_map_master.csv')
STATE = Path('data/openalex_free_state.json')
STAGE = Path('daily_openalex_batch')
STAGE.mkdir(exist_ok=True)
BATCH_SIZE = 100
session = requests.Session()
session.headers['User-Agent'] = 'fulltexttest/openalex-bulk-pdf/5.0'

def log(msg): print(f'PROGRESS: {msg}', flush=True)
def norm(doi): return re.sub(r'^(?:https?://doi.org/|doi:)', '', doi.strip(), flags=re.I).rstrip(' .;,').lower()
def host(url):
    from urllib.parse import urlparse
    return urlparse(url).netloc.lower()

def request(method, url, **kwargs):
    kwargs.setdefault('timeout', (10, 30))
    for attempt in range(4):
        log(f'HTTP {method} {url[:180]} (attempt {attempt+1}/4)')
        try: r = session.request(method, url, allow_redirects=True, **kwargs)
        except Exception as e:
            log(f'HTTP ERROR {type(e).__name__}: {e}')
            if attempt == 3: return None
            time.sleep(2 ** attempt); continue
        if r.status_code != 429 or attempt == 3: return r
        retry = r.headers.get('Retry-After')
        try: delay = min(60, float(retry)) if retry else min(60, 2 ** attempt)
        except ValueError: delay = min(60, 2 ** attempt)
        log(f'HTTP 429; backing off {delay:.0f}s; remaining={r.headers.get("X-RateLimit-Remaining", "?")} reset={r.headers.get("X-RateLimit-Reset", "?")} credits={r.headers.get("X-RateLimit-Credits-Used", "?")}')
        time.sleep(delay)
    return None

def parse_pdf(data):
    p=Path('/tmp/fulltexttest.pdf'); p.write_bytes(data)
    try: return '\n\n'.join(page.extract_text() or '' for page in PdfReader(str(p)).pages)
    finally: p.unlink(missing_ok=True)

def store(input_doi, work, source, url, data, content_type):
    raw = gzip.decompress(data) if data[:2] == b'\x1f\x8b' else data
    ct=(content_type or '').lower()
    try:
        if raw.startswith(b'%PDF') or 'pdf' in ct:
            text=parse_pdf(raw)
            if len(text)<3000: return None,'pdf_parse_failure'
            ext,fmt,stored='pdf.gz','pdf',gzip.compress(raw,6)
        elif 'xml' in ct or raw.lstrip().startswith(b'<?xml') or b'<TEI' in raw[:1000]:
            root=etree.fromstring(raw); text='\n'.join(' '.join(''.join(n.itertext()).split()) for n in root.xpath('//*[local-name()="body"]')).strip()
            if len(text)<3000: return None,'xml_parse_failure'
            ext,fmt,stored='xml.gz','xml',gzip.compress(raw,6)
        else: return None,'unsupported_content_type'
    except Exception as e: return None,f'parse_error:{type(e).__name__}'
    checksum=hashlib.sha256(raw).hexdigest(); slug=hashlib.sha256(input_doi.encode()).hexdigest()[:20]; dest=STAGE/slug; dest.mkdir(parents=True,exist_ok=True)
    audit={'input_doi':input_doi,'openalex_doi':work.get('doi'),'doi_match':norm(work.get('doi',''))==input_doi,'openalex_id':work.get('id'),'title':work.get('display_name'),'publication_year':work.get('publication_year'),'open_access':work.get('open_access'),'best_oa_location':work.get('best_oa_location'),'locations':work.get('locations',[]),'source':source,'source_url':url,'retrieved_at':time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime()),'text_chars':len(text),'sha256':checksum,'format':fmt}
    (dest/f'fulltext.{ext}').write_bytes(stored); (dest/'metadata.json').write_text(json.dumps(audit,indent=2,ensure_ascii=False),encoding='utf-8')
    return {'input_doi':input_doi,'openalex_doi':work.get('doi'),'doi_match':audit['doi_match'],'openalex_id':work.get('id'),'format':fmt,'text_chars':len(text),'sha256':checksum,'source':source,'source_url':url},None

def download_pdf(input_doi, work, url):
    if not url or not str(url).startswith('http'): return None,'invalid_url',None
    r=request('GET',url)
    if r is None:return None,'request_failed',host(url)
    if not r.ok or not r.content:return None,f'http_{r.status_code}',host(r.url or url)
    got,err=store(input_doi,work,'OpenAlex-discovered OA PDF',r.url,r.content,r.headers.get('content-type',''))
    return got,err,host(r.url or url)

key=os.environ.get('OPENALEX_API_KEY'); token=os.environ.get('ZENODO_TOKEN')
if not key or not token: raise SystemExit('OPENALEX_API_KEY and ZENODO_TOKEN are required')
if not MANIFEST.exists(): raise SystemExit(f'Missing {MANIFEST}')
state=json.loads(STATE.read_text()) if STATE.exists() else {'completed':{},'failed':{}}
completed=set(state.get('completed',{})); rows=[]
with MANIFEST.open(newline='',encoding='utf-8') as f:
    for row in csv.DictReader(f):
        doi=norm(row.get('doi',''))
        if doi and doi not in completed: rows.append(doi)
log(f'START queue={len(rows)} | no daily full-text cap | OpenAlex metadata batch_size={BATCH_SIZE}')
successes=[]; failures=[]; deferred=[]; start=time.time()
# Hosts returning 429 are paused for this run; their PDFs are recorded as deferred rather than retried immediately.
blocked_until=defaultdict(float)
for start_i in range(0,len(rows),BATCH_SIZE):
    batch=rows[start_i:start_i+BATCH_SIZE]
    if not batch: break
    log(f'METADATA BATCH {start_i+1}-{start_i+len(batch)} of {len(rows)}')
    filt='|'.join(batch); url='https://api.openalex.org/works'
    r=request('GET',url,params={'filter':'doi:'+filt,'per-page':100,'api_key':key})
    if r is None or not r.ok:
        status='metadata_request_failed' if r is None else f'metadata_http_{r.status_code}'
        log(f'BATCH FAIL {start_i+1}-{start_i+len(batch)}: {status}')
        failures.extend({'input_doi':d,'status':status} for d in batch); continue
    data=r.json(); works=data.get('results',[]); by_doi={norm(w.get('doi','')):w for w in works if w.get('doi')}
    log(f'METADATA BATCH RETURNED {len(works)} records; matched={sum(1 for d in batch if d in by_doi)}')
    for n,input_doi in enumerate(batch, start_i+1):
        work=by_doi.get(input_doi)
        if not work:
            failures.append({'input_doi':input_doi,'status':'openalex_no_exact_doi_match'}); log(f'NO MATCH {n}: {input_doi}'); continue
        locations=[]
        if work.get('best_oa_location'):locations.append(work['best_oa_location'])
        locations.extend(work.get('locations') or [])
        urls=[];seen=set()
        for loc in locations:
            if isinstance(loc,dict) and loc.get('pdf_url') and loc['pdf_url'] not in seen:seen.add(loc['pdf_url']);urls.append(loc['pdf_url'])
        log(f'DOI {n}/{len(rows)} {input_doi} | exact_match=yes | pdf_urls={len(urls)}')
        got=None; error='no_pdf_url'
        for pdf_url in urls:
            h=host(pdf_url)
            if blocked_until[h] > time.time():
                deferred.append({'input_doi':input_doi,'openalex_doi':work.get('doi'),'status':'host_rate_limited','host':h,'source_url':pdf_url})
                log(f'DEFER {input_doi}: host={h} paused after 429')
                continue
            log(f'PDF GET {input_doi} -> {pdf_url}')
            got,error,h2=download_pdf(input_doi,work,pdf_url)
            if got:break
            if error == 'http_429':
                # Do not hammer the host. Skip it for the rest of this run; other domains continue immediately.
                blocked_until[h2] = time.time() + 3600
                deferred.append({'input_doi':input_doi,'openalex_doi':work.get('doi'),'status':'host_rate_limited','host':h2,'source_url':pdf_url})
                log(f'DEFER HOST {h2}: 429; pausing this host for 60 minutes')
            elif error == 'request_failed':
                log(f'FAIL {input_doi}: request failed after bounded retries')
        if got:
            successes.append(got);log(f'SUCCESS {len(successes)} {input_doi} source=OpenAlex-discovered OA PDF chars={got["text_chars"]:,}')
        elif not any(x['input_doi']==input_doi for x in deferred):
            failures.append({'input_doi':input_doi,'openalex_doi':work.get('doi'),'status':error});log(f'FAIL {input_doi}: {error}')
log(f'RETRIEVAL COMPLETE successes={len(successes)} failures={len(failures)} deferred_host_rate_limits={len(deferred)} elapsed_min={(time.time()-start)/60:.1f}')
if not successes: raise SystemExit('No full texts retrieved; refusing to create empty archive')
Path('daily-pdf-manifest.csv').write_text('input_doi,openalex_doi,doi_match,source_url,status\n' + '\n'.join(f'{x["input_doi"]},{x.get("openalex_doi","")},{x["doi_match"]},{x["source_url"]},success' for x in successes) + '\n' + '\n'.join(f'{x["input_doi"]},{x.get("openalex_doi","")},, {x.get("source_url","")},{x["status"]}' for x in failures+deferred),encoding='utf-8')
batch_date=time.strftime('%Y-%m-%d',time.gmtime());run_id=os.environ.get('GITHUB_RUN_ID','manual');archive=Path(f'openalex-oa-pdfs-{batch_date}-{run_id}.tar.gz')
with tarfile.open(archive,'w:gz') as tf:tf.add(STAGE,arcname='fulltext')
log(f'ARCHIVE {archive.name} {archive.stat().st_size/1024/1024:.1f} MiB')
headers={'Authorization':f'Bearer {token}'};log('ZENODO CREATE');r=requests.post('https://zenodo.org/api/deposit/depositions',json={},headers={**headers,'Content-Type':'application/json'},timeout=(10,60));r.raise_for_status();dep=r.json();dep_id=dep['id'];bucket=dep['links']['bucket'];log(f'ZENODO CREATED id={dep_id}')
meta={'metadata':{'title':f'fulltexttest OpenAlex-discovered OA PDFs {batch_date} ({len(successes)} works)','upload_type':'dataset','publication_date':batch_date,'description':'Full texts retrieved from OA PDF URLs discovered through OpenAlex metadata. Input DOI to OpenAlex DOI matching and provenance are retained. Host-rate-limited URLs are recorded for later retry.','access_right':'restricted','access_conditions':'Restricted to the depositor/project for research analysis.'}}
r=requests.put(f'https://zenodo.org/api/deposit/depositions/{dep_id}',json=meta,headers={**headers,'Content-Type':'application/json'},timeout=(10,60));r.raise_for_status();log('ZENODO METADATA SAVED')
with archive.open('rb') as fp:log(f'ZENODO UPLOAD {archive.stat().st_size/1024/1024:.1f} MiB');r=requests.put(f'{bucket}/{archive.name}',data=fp,headers=headers,timeout=(10,1800));r.raise_for_status()
log('ZENODO UPLOAD COMPLETE');r=requests.post(f'https://zenodo.org/api/deposit/depositions/{dep_id}/actions/publish',headers=headers,timeout=(10,120));r.raise_for_status();published=r.json();log(f'ZENODO PUBLISHED id={published.get("id")} doi={published.get("doi")}')
for item in successes:state.setdefault('completed',{})[item['input_doi']]={'date':batch_date,'run_id':run_id,'zenodo_record':published.get('id'),'zenodo_doi':published.get('doi'),'sha256':item['sha256'],'format':item['format'],'source':item['source'],'source_url':item['source_url'],'openalex_doi':item['openalex_doi']}
for item in failures+deferred:state.setdefault('failed',{})[item['input_doi']]={'date':batch_date,'status':item['status'],'host':item.get('host'),'source_url':item.get('source_url')}
STATE.write_text(json.dumps(state,indent=2,ensure_ascii=False),encoding='utf-8');Path('daily-results.json').write_text(json.dumps({'date':batch_date,'run_id':run_id,'fulltexts_archived':len(successes),'failures':failures,'deferred_host_rate_limits':deferred,'zenodo':{'record_id':published.get('id'),'doi':published.get('doi')}},indent=2,ensure_ascii=False),encoding='utf-8');log('CHECKPOINT STATE WRITTEN AFTER ZENODO PUBLICATION')