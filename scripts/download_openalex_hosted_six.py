import csv,io,json,os,time,hashlib,zipfile
from pathlib import Path
import requests
from pypdf import PdfReader
base=Path('outputs/openalex_hosted_pdf_20261019');base.mkdir(parents=True,exist_ok=True)
key=os.environ.get('OPENALEX_API_KEY');token=os.environ.get('ZENODO_TOKEN')
if not key or not token:raise SystemExit('OPENALEX_API_KEY and ZENODO_TOKEN are required')
records=[r for r in csv.DictReader(open('data/ris_openalex_audit_20261019.csv')) if r['openalex_pdf']=='yes']
if len(records)!=6:raise SystemExit('Expected six OpenAlex PDF candidates, found '+str(len(records)))
manifest=[]
for row in records:
  wid=row['openalex_id'].split('/')[-1]
  url='https://content.openalex.org/works/'+wid+'.pdf'
  rec={k:row.get(k,'') for k in ('ris_record','title','doi','openalex_id')}
  rec.update(status='failed',reason='',source_url=url,pdf_file='',sha256='')
  try:
    response=requests.get(url,params={'api_key':key},timeout=100)
    response.raise_for_status()
    data=response.content
    if not data.startswith(b'%PDF') or len(data)<5000:raise ValueError('not_valid_pdf')
    reader=PdfReader(io.BytesIO(data))
    chars=sum(len(p.extract_text() or '') for p in reader.pages)
    if not reader.pages or chars<500:raise ValueError('unparseable_or_short_pdf')
    filename='ris_'+row['ris_record']+'_'+wid+'.pdf'
    (base/filename).write_bytes(data)
    rec.update(status='success',pdf_file=filename,sha256=hashlib.sha256(data).hexdigest())
  except Exception as e:rec['reason']=str(e)[:200]
  manifest.append(rec)
  print('RIS',row['ris_record'],wid,rec['status'],rec['reason'],flush=True)
with (base/'retrieval_manifest.csv').open('w',newline='') as f:
  w=csv.DictWriter(f,fieldnames=manifest[0].keys());w.writeheader();w.writerows(manifest)
summary={'candidates':len(manifest),'successful':sum(r['status']=='success' for r in manifest),'failed':sum(r['status']!='success' for r in manifest),'zenodo_status':'not_uploaded'}
(base/'summary.json').write_text(json.dumps(summary,indent=2))
if summary['successful']:
  zpath=base/'validated_openalex_pdfs.zip'
  with zipfile.ZipFile(zpath,'w',zipfile.ZIP_DEFLATED) as z:
    for r in manifest:
      if r['status']=='success':z.write(base/r['pdf_file'],r['pdf_file'])
    z.write(base/'retrieval_manifest.csv','retrieval_manifest.csv')
  api='https://zenodo.org/api/deposit/depositions'; headers={'Authorization':'Bearer '+token}
  res=requests.post(api,json={},headers=headers,timeout=60);res.raise_for_status();dep=res.json()
  meta={'metadata':{'title':'OpenAlex PDF retrieval for RIS selection, 19 October 2026','upload_type':'dataset','description':'Restricted PDFs with a provenance and validation manifest. Original works retain their respective copyright.','creators':[{'name':'Haddaway, Neal'}],'access_right':'closed','keywords':['OpenAlex','full text','RIS']}}
  res=requests.put(api+'/'+str(dep['id']),json=meta,headers=headers,timeout=60);res.raise_for_status()
  with zpath.open('rb') as f:
    res=requests.put(dep['links']['bucket']+'/'+zpath.name,data=f,headers=headers,timeout=(30,300));res.raise_for_status()
  summary.update(zenodo_status='restricted_draft_uploaded',zenodo_id=dep['id'],zenodo_url='https://zenodo.org/deposit/'+str(dep['id']))
  (base/'summary.json').write_text(json.dumps(summary,indent=2))
print(json.dumps(summary,indent=2),flush=True)
