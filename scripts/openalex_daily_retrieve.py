import csv, gzip, hashlib, json, os, re, tarfile, time
from collections import defaultdict
from pathlib import Path
from urllib.parse import urlparse

import requests
from lxml import etree
from pypdf import PdfReader

MANIFEST=Path('data/living_evidence_map_master.csv')
STATE=Path('data/openalex_free_state.json')
STAGE=Path('daily_openalex_batch')
STAGE.mkdir(exist_ok=True)
BATCH_SIZE=100
CHECKPOINT_EVERY=25

session=requests.Session()
session.headers.update({'User-Agent':'fulltexttest/openalex-bulk-pdf/5.3'})

def log(x): print(f'PROGRESS: {x}', flush=True)

def norm(x):
    return re.sub(r'^(?:https?://doi.org/|doi:)', '', x.strip(), flags=re.I).rstrip(' .;,').lower()

def host(u): return urlparse(u).netloc.lower()

def doi_prefix(doi):
    return norm(doi).split('/',1)[0]

def request(method,url,**kwargs):
    kwargs.setdefault('timeout',(10,30))
    for attempt in range(4):
        log(f'HTTP {method} {url[:180]} (attempt {attempt+1}/4)')
        try:
            r=session.request(method,url,allow_redirects=True,**kwargs)
        except Exception as e:
            log(f'HTTP ERROR {type(e).__name__}: {e}')
            if attempt==3: return None
            time.sleep(2**attempt)
            continue
        if r.status_code != 429 or attempt==3: return r
        retry=r.headers.get('Retry-After')
        try: delay=min(60,float(retry)) if retry else min(60,2**attempt)
        except ValueError: delay=min(60,2**attempt)
        log(f'HTTP 429; backing off {delay:.0f}s')
        time.sleep(delay)
    return None

def classify_http(status):
    if status==401: return 'oa_location_inaccessible_401'
    if status==403: return 'oa_location_inaccessible_403'
    if 500 <= status < 600: return 'transient_http_error'
    if status==429: return 'retrievable_but_rate_limited'
    return f'http_{status}'

def content_kind(raw,ct):
    raw=raw.lstrip()
    ct=(ct or '').lower()
    if raw.startswith(b'%PDF'): return 'pdf'
    if raw.startswith(b'<?xml') or b'<TEI' in raw[:2000] or 'xml' in ct: return 'xml'
    if raw.startswith(b'<!doctype html') or raw.startswith(b'<html') or 'text/html' in ct: return 'html'
    if 'pdf' in ct: return 'pdf'
    return 'other'

def parse_pdf(data):
    p=Path('/tmp/fulltexttest.pdf'); p.write_bytes(data)
    try: return '\n\n'.join(page.extract_text() or '' for page in PdfReader(str(p)).pages)
    finally: p.unlink(missing_ok=True)

def store(input_doi,work,source,url,data,ct):
    raw=gzip.decompress(data) if data[:2]==b'\x1f\x8b' else data
    kind=content_kind(raw,ct)
    try:
        if kind=='pdf':
            text=parse_pdf(raw)
            if len(text)<3000: return None,'parse_failure_pdf_short'
            ext,fmt,stored='pdf.gz','pdf',gzip.compress(raw,6)
        elif kind=='xml':
            root=etree.fromstring(raw)
            text='\n'.join(' '.join(''.join(n.itertext()).split()) for n in root.xpath('//*[local-name()="body"]')).strip()
            if len(text)<3000: return None,'parse_failure_xml_short'
            ext,fmt,stored='xml.gz','xml',gzip.compress(raw,6)
        elif kind=='html':
            return None,'html_landing_page'
        else:
            return None,'unsupported_content_type'
    except Exception as e:
        return None,f'parse_failure_{type(e).__name__}'
    checksum=hashlib.sha256(raw).hexdigest()
    slug=hashlib.sha256(input_doi.encode()).hexdigest()[:20]
    dest=STAGE/slug; dest.mkdir(parents=True,exist_ok=True)
    audit={'input_doi':input_doi,'openalex_doi':work.get('doi'),'doi_match':norm(work.get('doi',''))==input_doi,'openalex_id':work.get('id'),'title':work.get('display_name'),'publication_year':work.get('publication_year'),'open_access':work.get('open_access'),'best_oa_location':work.get('best_oa_location'),'locations':work.get('locations',[]),'source':source,'source_url':url,'retrieved_at':time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime()),'text_chars':len(text),'sha256':checksum,'format':fmt}
    (dest/f'fulltext.{ext}').write_bytes(stored)
    (dest/'metadata.json').write_text(json.dumps(audit,indent=2,ensure_ascii=False),encoding='utf-8')
    return {'input_doi':input_doi,'openalex_doi':work.get('doi'),'doi_match':audit['doi_match'],'openalex_id':work.get('id'),'format':fmt,'text_chars':len(text),'sha256':checksum,'source':source,'source_url':url},None

def bio_or_med_url(doi, host_name):
    if not norm(doi).startswith('10.1101/'): return None
    suffix=norm(doi).split('/',1)[1]
    base='medrxiv.org' if host_name and 'medrxiv' in host_name else 'biorxiv.org'
    return f'https://www.{base}/content/10.1101/{suffix}.full.pdf'

def download_pdf(input_doi,work,url,source='OpenAlex-discovered OA PDF'):
    if not url or not str(url).startswith('http'): return None,'invalid_url',None
    r=request('GET',url)
    if r is None: return None,'request_failed',host(url)
    h=host(r.url or url)
    if r.status_code==429: return None,'retrievable_but_rate_limited',h
    if not r.ok or not r.content: return None,classify_http(r.status_code),h
    got,err=store(input_doi,work,source,r.url,r.content,r.headers.get('content-type',''))
    return got,err,h

def candidate_urls(input_doi,work):
    locations=[]
    if work.get('best_oa_location'): locations.append(work['best_oa_location'])
    locations.extend(work.get('locations') or [])
    candidates=[]; seen=set()
    def add(url,source):
        if url and str(url).startswith('http') and url not in seen:
            seen.add(url); candidates.append((url,source))
    for loc in locations:
        if not isinstance(loc,dict): continue
        add(loc.get('pdf_url'),'OpenAlex-discovered OA PDF')
    # Only add source-specific preprint routes when OpenAlex already identifies that source.
    for loc in locations:
        if not isinstance(loc,dict): continue
        landing=(loc.get('landing_page_url') or loc.get('source',{}).get('homepage_url') or '')
        h=host(landing)
        if 'biorxiv.org' in h or 'medrxiv.org' in h:
            u=bio_or_med_url(input_doi,h)
            if u: add(u,'bioRxiv/medRxiv source-specific OA PDF')
    return candidates,locations

def write_checkpoint(state, successes, failures, deferred, reason):
    STATE.write_text(json.dumps(state,indent=2,ensure_ascii=False),encoding='utf-8')
    Path('daily-results.json').write_text(json.dumps({'run_id':os.environ.get('GITHUB_RUN_ID','manual'),'checkpoint_reason':reason,'fulltexts_so_far':len(successes),'failures_so_far':len(failures),'deferred_host_rate_limits_so_far':len(deferred),'failure_class_counts':count_statuses(failures+deferred)},indent=2,ensure_ascii=False),encoding='utf-8')
    log(f'CHECKPOINT SAVED reason={reason} completed={len(state.get("completed",{}))} failed={len(state.get("failed",{}))}')

def count_statuses(items):
    out=defaultdict(int)
    for x in items: out[x.get('status','unknown')]+=1
    return dict(sorted(out.items()))

key=os.environ.get('OPENALEX_API_KEY'); token=os.environ.get('ZENODO_TOKEN')
if not key or not token: raise SystemExit('OPENALEX_API_KEY and ZENODO_TOKEN are required')
if not MANIFEST.exists(): raise SystemExit(f'Missing {MANIFEST}')
state=json.loads(STATE.read_text()) if STATE.exists() else {'completed':{},'failed':{}}
completed=set(state.get('completed',{})); rows=[]
with MANIFEST.open(newline='',encoding='utf-8') as f:
    for row in csv.DictReader(f):
        doi=norm(row.get('doi',''))
        if doi and doi not in completed: rows.append(doi)
log(f'START queue={len(rows)} | OpenAlex metadata batch_size={BATCH_SIZE} | checkpoint_every={CHECKPOINT_EVERY}')
successes=[]; failures=[]; deferred=[]; start=time.time(); blocked_until=defaultdict(float); processed_since_checkpoint=0

for start_i in range(0,len(rows),BATCH_SIZE):
    batch=rows[start_i:start_i+BATCH_SIZE]
    log(f'METADATA BATCH {start_i+1}-{start_i+len(batch)} of {len(rows)}')
    r=request('GET','https://api.openalex.org/works',params={'filter':'doi:'+'|'.join(batch),'per-page':100,'api_key':key})
    if r is None or not r.ok:
        status='metadata_request_failed' if r is None else f'metadata_http_{r.status_code}'
        for d in batch: failures.append({'input_doi':d,'status':status})
        processed_since_checkpoint += len(batch)
        if processed_since_checkpoint>=CHECKPOINT_EVERY:
            write_checkpoint(state,successes,failures,deferred,f'after_metadata_batch_{start_i+len(batch)}'); processed_since_checkpoint=0
        continue
    works=r.json().get('results',[]); by_doi={norm(w.get('doi','')):w for w in works if w.get('doi')}
    log(f'METADATA BATCH RETURNED {len(works)} records; matched={sum(1 for d in batch if d in by_doi)}')
    for n,input_doi in enumerate(batch,start_i+1):
        work=by_doi.get(input_doi)
        if not work:
            item={'input_doi':input_doi,'status':'openalex_no_exact_doi_match'}; failures.append(item); state.setdefault('failed',{})[input_doi]=item; processed_since_checkpoint+=1
        else:
            candidates,locations=candidate_urls(input_doi,work)
            got=None; terminal=[]; last_error='no_retrievable_oa'
            log(f'DOI {n}/{len(rows)} {input_doi} | pdf_candidates={len(candidates)}')
            for pdf_url,source in candidates:
                h=host(pdf_url)
                if blocked_until[h]>time.time():
                    deferred.append({'input_doi':input_doi,'openalex_doi':work.get('doi'),'openalex_id':work.get('id'),'status':'retrievable_but_rate_limited','host':h,'source_url':pdf_url,'best_oa_location':work.get('best_oa_location'),'locations':locations}); last_error='retrievable_but_rate_limited'; continue
                got,error,h2=download_pdf(input_doi,work,pdf_url,source)
                if got: break
                last_error=error; terminal.append({'url':pdf_url,'host':h2,'status':error})
                if error=='retrievable_but_rate_limited':
                    blocked_until[h2]=time.time()+3600
                    deferred.append({'input_doi':input_doi,'openalex_doi':work.get('doi'),'openalex_id':work.get('id'),'status':'retrievable_but_rate_limited','host':h2,'source_url':pdf_url,'best_oa_location':work.get('best_oa_location'),'locations':locations})
                elif error in ('oa_location_inaccessible_401','oa_location_inaccessible_403'):
                    # Do not retry the same blocked location; continue only to other OA locations.
                    continue
                elif error=='transient_http_error' or error=='request_failed':
                    continue
            if got:
                successes.append(got)
                state.setdefault('completed',{})[input_doi]={'date':time.strftime('%Y-%m-%d',time.gmtime()),'run_id':os.environ.get('GITHUB_RUN_ID','manual'),'sha256':got['sha256'],'format':got['format'],'source':got['source'],'source_url':got['source_url'],'openalex_doi':got['openalex_doi'],'openalex_id':got['openalex_id']}
                processed_since_checkpoint+=1
            elif last_error=='retrievable_but_rate_limited':
                state.setdefault('failed',{})[input_doi]=deferred[-1]; processed_since_checkpoint+=1
            elif candidates and all(x['status'] in ('oa_location_inaccessible_401','oa_location_inaccessible_403') for x in terminal):
                item={'input_doi':input_doi,'openalex_doi':work.get('doi'),'openalex_id':work.get('id'),'status':'oa_location_inaccessible','attempts':terminal,'best_oa_location':work.get('best_oa_location'),'locations':locations}; failures.append(item); state.setdefault('failed',{})[input_doi]=item; processed_since_checkpoint+=1
            elif candidates and any(x['status'] in ('parse_failure_pdf_short','parse_failure_xml_short') or x['status'].startswith('parse_failure_') for x in terminal):
                item={'input_doi':input_doi,'openalex_doi':work.get('doi'),'openalex_id':work.get('id'),'status':'parse_failure','attempts':terminal,'best_oa_location':work.get('best_oa_location'),'locations':locations}; failures.append(item); state.setdefault('failed',{})[input_doi]=item; processed_since_checkpoint+=1
            elif candidates:
                item={'input_doi':input_doi,'openalex_doi':work.get('doi'),'openalex_id':work.get('id'),'status':last_error,'attempts':terminal,'best_oa_location':work.get('best_oa_location'),'locations':locations}; failures.append(item); state.setdefault('failed',{})[input_doi]=item; processed_since_checkpoint+=1
            else:
                # No OA PDF URL was exposed by OpenAlex. Do not infer that the article is OA merely from the DOI.
                oa=work.get('open_access') or {}
                status='no_retrievable_oa' if not oa.get('is_oa') else 'oa_declared_but_no_pdf_url'
                item={'input_doi':input_doi,'openalex_doi':work.get('doi'),'openalex_id':work.get('id'),'status':status,'best_oa_location':work.get('best_oa_location'),'locations':locations}; failures.append(item); state.setdefault('failed',{})[input_doi]=item; processed_since_checkpoint+=1
        if processed_since_checkpoint>=CHECKPOINT_EVERY:
            write_checkpoint(state,successes,failures,deferred,f'after_doi_{n}'); processed_since_checkpoint=0

write_checkpoint(state,successes,failures,deferred,'retrieval_complete_before_zenodo')
log(f'RETRIEVAL COMPLETE successes={len(successes)} failures={len(failures)} deferred={len(deferred)} elapsed_min={(time.time()-start)/60:.1f}')
if not successes: raise SystemExit('No full texts retrieved; refusing to create empty archive')

with Path('daily-pdf-manifest.csv').open('w',newline='',encoding='utf-8') as f:
    w=csv.writer(f); w.writerow(['input_doi','openalex_doi','doi_match','source_url','status'])
    for x in successes: w.writerow([x['input_doi'],x.get('openalex_doi',''),x['doi_match'],x['source_url'],'success'])

batch_date=time.strftime('%Y-%m-%d',time.gmtime()); run_id=os.environ.get('GITHUB_RUN_ID','manual'); archive=Path(f'openalex-oa-pdfs-{batch_date}-{run_id}.tar.gz')
with tarfile.open(archive,'w:gz') as tf: tf.add(STAGE,arcname='fulltext')
log(f'ARCHIVE {archive.name} {archive.stat().st_size/1024/1024:.1f} MiB')
headers={'Authorization':f'Bearer {token}'}
log('ZENODO CREATE'); r=requests.post('https://zenodo.org/api/deposit/depositions',json={},headers={**headers,'Content-Type':'application/json'},timeout=(10,60)); r.raise_for_status(); dep=r.json(); dep_id=dep['id']; bucket=dep['links']['bucket']
meta={'metadata':{'title':f'fulltexttest OpenAlex-discovered OA PDFs {batch_date} ({len(successes)} works)','upload_type':'dataset','publication_date':batch_date,'description':'Full texts retrieved from OA PDF URLs discovered through OpenAlex metadata. Failed records retain provenance and failure classification for recovery.','access_right':'restricted','access_conditions':'Restricted to the depositor/project for research analysis.'}}
r=requests.put(f'https://zenodo.org/api/deposit/depositions/{dep_id}',json=meta,headers={**headers,'Content-Type':'application/json'},timeout=(10,60)); r.raise_for_status()
with archive.open('rb') as fp:
    r=requests.put(f'{bucket}/{archive.name}',data=fp,headers=headers,timeout=(10,1800)); r.raise_for_status()
r=requests.post(f'https://zenodo.org/api/deposit/depositions/{dep_id}/actions/publish',headers=headers,timeout=(10,120)); r.raise_for_status(); published=r.json()
for item in successes: state.setdefault('completed',{})[item['input_doi']].update({'zenodo_record':published.get('id'),'zenodo_doi':published.get('doi')})
write_checkpoint(state,successes,failures,deferred,'zenodo_published')
log(f'ZENODO PUBLISHED id={published.get("id")} doi={published.get("doi")}')
