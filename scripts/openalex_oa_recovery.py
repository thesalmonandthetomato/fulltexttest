import csv, json, os, re, time, hashlib, gzip, tarfile
from pathlib import Path
from urllib.parse import urlparse
import requests
from pypdf import PdfReader
from lxml import etree

MANIFEST=Path('data/living_evidence_map_master.csv')
STATE=Path('data/openalex_free_state.json')
STAGE=Path('recovery_openalex_oa')
STAGE.mkdir(exist_ok=True)
s=requests.Session(); s.headers.update({'User-Agent':'fulltexttest/oa-recovery/1.0','Accept':'application/pdf,application/xml,text/xml,*/*'})

def norm(x): return re.sub(r'^(?:https?://doi.org/|doi:)','',x.strip(),flags=re.I).rstrip(' .;,').lower()
def host(u): return urlparse(u).netloc.lower()
def log(x): print('RECOVERY: '+x,flush=True)
def fetch(u, timeout=(10,60)):
    try: r=s.get(u,allow_redirects=True,timeout=timeout,headers={'Referer':u,'Accept':'application/pdf,application/xml,text/xml,*/*'})
    except Exception as e: return None,f'error_{type(e).__name__}'
    return r,None

def parse_store(doi,data,ct,url):
    raw=data
    if raw[:2]==b'\x1f\x8b':
        try: raw=gzip.decompress(raw)
        except: pass
    try:
        if raw.startswith(b'%PDF') or 'pdf' in (ct or '').lower():
            p=Path('/tmp/recovery.pdf');p.write_bytes(raw)
            text='\n\n'.join(x.extract_text() or '' for x in PdfReader(str(p)).pages);p.unlink(missing_ok=True)
            if len(text)<3000:return None,'pdf_parse_failure'
            ext,fmt='pdf.gz','pdf';stored=gzip.compress(raw,6)
        elif 'xml' in (ct or '').lower() or raw.lstrip().startswith((b'<?xml',b'<TEI')):
            root=etree.fromstring(raw);text='\n'.join(' '.join(''.join(n.itertext()).split()) for n in root.xpath('//*[local-name()="body"]')).strip()
            if len(text)<3000:return None,'xml_parse_failure'
            ext,fmt='xml.gz','xml';stored=gzip.compress(raw,6)
        else:return None,'not_pdf_or_xml'
    except Exception as e:return None,'parse_error_'+type(e).__name__
    slug=hashlib.sha256(doi.encode()).hexdigest()[:20];d=STAGE/slug;d.mkdir(parents=True,exist_ok=True);(d/f'fulltext.{ext}').write_bytes(stored)
    meta={'input_doi':doi,'source_url':url,'retrieved_at':time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime()),'sha256':hashlib.sha256(raw).hexdigest(),'text_chars':len(text),'format':fmt}
    (d/'recovery.json').write_text(json.dumps(meta,indent=2),encoding='utf-8');return meta,None

# Recovery deliberately does not call OpenAlex content. It only works on failed/deferred PDF URLs.
# For each DOI it tries: alternate OpenAlex locations, direct URL with browser-like headers,
# landing-page fallback, and then Unpaywall/Europe PMC where DOI lookup is appropriate.
with MANIFEST.open(newline='',encoding='utf-8') as f: input_dois=[norm(r.get('doi','')) for r in csv.DictReader(f) if r.get('doi')]
state=json.loads(STATE.read_text()) if STATE.exists() else {'completed':{},'failed':{}}
completed=set(state.get('completed',{})); candidates=[]
for doi,v in state.get('failed',{}).items():
    if doi in completed: continue
    if v.get('source_url') and v.get('status') in {'host_rate_limited','http_403','http_404','http_429','request_failed','request_failed_after_bounded_retries'}:
        candidates.append((doi,v))
log(f'START candidates={len(candidates)}')
results=[];success=[];fail=[]
for i,(doi,v) in enumerate(candidates,1):
    urls=[]
    if v.get('source_url'):urls.append(v['source_url'])
    # Try Unpaywall as a discovery fallback when an email is available via configured environment.
    email=os.environ.get('UNPAYWALL_EMAIL')
    if email:
        r,e=fetch('https://api.unpaywall.org/v2/'+doi+'?email='+email,timeout=(10,30))
        if r is not None and r.ok:
            try:
                j=r.json();loc=j.get('best_oa_location') or {};u=loc.get('url_for_pdf') or loc.get('url');
                if u and u not in urls:urls.append(u)
                for loc in j.get('oa_locations') or []:
                    u=loc.get('url_for_pdf') or loc.get('url');
                    if u and u not in urls:urls.append(u)
            except:pass
    got=None;reason='all_recovery_routes_failed'
    for u in urls:
        log(f'{i}/{len(candidates)} {doi} GET {u}')
        r,e=fetch(u)
        if r is None:reason=e;continue
        if r.status_code==429:reason='http_429_deferred';log(f'{doi} 429 host={host(u)}');continue
        if r.status_code in (403,404):reason=f'http_{r.status_code}';continue
        if not r.ok:reason=f'http_{r.status_code}';continue
        got,err=parse_store(doi,r.content,r.headers.get('content-type',''),r.url)
        if got:
            success.append(got);results.append({'input_doi':doi,'status':'success','source_url':r.url});log(f'SUCCESS {doi} chars={got["text_chars"]:,}');break
        reason=err
    if not got:
        fail.append({'input_doi':doi,'status':reason,'source_url':v.get('source_url','')});results.append({'input_doi':doi,'status':reason,'source_url':v.get('source_url','')});log(f'FAIL {doi}: {reason}')
log(f'COMPLETE successes={len(success)} failures={len(fail)}')
Path('recovery-results.json').write_text(json.dumps({'successes':success,'failures':fail},indent=2),encoding='utf-8')
with Path('recovery-manifest.csv').open('w',newline='',encoding='utf-8') as f:
    w=csv.DictWriter(f,fieldnames=['input_doi','status','source_url']);w.writeheader();w.writerows(results)
if success:
    date=time.strftime('%Y-%m-%d',time.gmtime());rid=os.environ.get('GITHUB_RUN_ID','manual');archive=Path(f'recovery-{date}-{rid}.tar.gz')
    with tarfile.open(archive,'w:gz') as tf:tf.add(STAGE,arcname='fulltext')
    log(f'ARCHIVE {archive.name} {archive.stat().st_size/1024/1024:.1f} MiB')
