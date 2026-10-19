#!/usr/bin/env python3
"""Layer 5: CORE/OpenAIRE repository PDF retrieval, cumulative RIS registry.
PDF-only, strict bibliographic matching, Zenodo unpublished drafts.
"""
import csv,io,json,os,re,time,hashlib,zipfile,unicodedata
from pathlib import Path
from difflib import SequenceMatcher
import requests
from pypdf import PdfReader
ROOT=Path("outputs/ris814972_layer5"); ROOT.mkdir(parents=True,exist_ok=True)
SRC=Path("data/ris_openalex_audit_20261019.csv")
REG=Path("data/ris814972_retrieval_registry.csv")
rows=list(csv.DictReader(SRC.open(encoding="utf-8")))
registry=list(csv.DictReader(REG.open(encoding="utf-8")))
known={r["ris_record"] for r in registry if r["retrieval_status"]=="downloaded"}
assert len(rows)==384 and len(known)==13,(len(rows),len(known))
key=os.getenv("CORE_API_KEY","");token=os.getenv("ZENODO_TOKEN","")
if not token:raise SystemExit("ZENODO_TOKEN required")
sess=requests.Session();sess.headers["User-Agent"]="fulltexttest/ris814972-layer5/1.0 research PDF discovery"
if key:sess.headers["Authorization"]="Bearer "+key
def normalize(x):return re.sub(r"[^a-z0-9]+"," ",unicodedata.normalize("NFKD",str(x or "").lower())).strip()
def doi(x):return re.sub(r"^(https?://(dx\\.)?doi\\.org/|doi:)","",str(x or "").strip(),flags=re.I).lower().strip(" .,;")
def match_title(a,b):return SequenceMatcher(None,normalize(a),normalize(b)).ratio()
def get(url,params=None,timeout=28):
 for trial in range(2):
  try:
   r=sess.get(url,params=params,timeout=(12,timeout),headers={"Authorization":"Bearer "+key} if key and "api.core.ac.uk" in url else {"Authorization":""})
   if r.status_code in (429,500,502,503,504) and trial==0:time.sleep(3);continue
   return r
  except requests.RequestException:
   if trial==0:time.sleep(2)
 return None
def discovered_pdf_urls(item):
 urls=[];seen=set()
 def scan(x,path=""):
  if isinstance(x,dict):
   for k,v in x.items():
    lk=k.lower()
    if isinstance(v,str) and v.startswith(("http://","https://")) and ("url" in lk or "pdf" in lk or "download" in lk):
     # Do not use publisher landing pages. Allow metadata PDF URLs or literal .pdf paths.
     if (".pdf" in v.lower().split("?")[0] or lk in ("downloadurl","url_for_pdf","pdfurl","pdf_url")) and v not in seen:
      seen.add(v);urls.append((path+"."+k,v))
    elif isinstance(v,(dict,list)):scan(v,path+"."+k)
  elif isinstance(x,list):
   for part in x:scan(part,path)
 scan(item)
 return urls[:10]
def is_pdf(data,expected_title,expected_doi):
 if not data.startswith(b"%PDF") or len(data)<5000:return None,"non_pdf"
 try:
  pdf=PdfReader(io.BytesIO(data),strict=False)
  if not pdf.pages:return None,"empty_pdf"
  first=" ".join((p.extract_text() or "") for p in pdf.pages[:min(2,len(pdf.pages))])
  nt=normalize(first)
  # Require title words in the first pages, or an exact DOI shown in the PDF.
  words=[w for w in normalize(expected_title).split() if len(w)>=4]
  coverage=sum(w in nt for w in set(words))/max(1,len(set(words)))
  doikey=re.sub(r"[^a-z0-9]","",expected_doi.lower())
  pdfkey=re.sub(r"[^a-z0-9]","",first.lower())
  if not ((len(words)>=4 and coverage>=0.8) or (len(doikey)>12 and doikey in pdfkey)):
   return None,"pdf_identity_unconfirmed"
  return len(pdf.pages),"valid_identity"
 except Exception as exc:return None,"pdf_parse_failure:"+type(exc).__name__
attempts=[];results=[];pending=[];deposits=[];bytes_pending=0;core_calls=0
def dump():
 for name,data,fields in [("attempts.csv",attempts,["ris_record","provider","url","http","outcome"]),
                           ("results.csv",results,["ris_record","doi","status","reason","filename","sha256","source_url","provider","deposit_id"])]:
  with (ROOT/name).open("w",newline="",encoding="utf-8") as f:
   w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(data)
 (ROOT/"deposits.json").write_text(json.dumps(deposits,indent=2))
def upload():
 global pending,bytes_pending
 if not pending:return
 b=len(deposits)+1
 archive=ROOT/f"ris814972_layer5_batch_{b:03d}.zip"
 with zipfile.ZipFile(archive,"w",zipfile.ZIP_DEFLATED) as z:
  for path,_ in pending:z.write(path,path.name)
  z.writestr("manifest.json",json.dumps([r for _,r in pending],indent=2))
 base="https://zenodo.org/api/deposit/depositions";h={"Authorization":"Bearer "+token}
 r=requests.post(base,json={},headers=h,timeout=60);r.raise_for_status();dep=r.json()
 ident=dep["id"];metadata={"metadata":{"title":f"RIS 814972 repository PDFs Layer 5 batch {b}","upload_type":"dataset","creators":[{"name":"Haddaway, Neal"}],"access_right":"closed","description":"Unpublished restricted verified repository PDFs; source RIS record and checksum traceable in manifest."}}
 r=requests.put(base+"/"+str(ident),json=metadata,headers=h,timeout=60);r.raise_for_status()
 with archive.open("rb") as f:
  r=requests.put(dep["links"]["bucket"]+"/"+archive.name,data=f,headers=h,timeout=(30,600));r.raise_for_status()
 for _,item in pending:item["deposit_id"]=str(ident)
 deposits.append({"id":ident,"count":len(pending),"url":f"https://zenodo.org/deposit/{ident}"})
 print("ZENODO_DRAFT",ident,len(pending),flush=True)
 pending=[];bytes_pending=0;dump()
for n,row in enumerate(rows,1):
 rid=row["ris_record"]
 if rid in known:continue
 rd=doi(row.get("doi"));title=row["title"];year=re.sub("[^0-9]","",row.get("year",""))[:4]
 out={"ris_record":rid,"doi":rd,"status":"not_found","reason":"","filename":"","sha256":"","source_url":"","provider":"","deposit_id":""}
 results.append(out)
 works=[]
 # OpenAIRE V4 exact persistent identifier filter; do not assume every result is a real DOI match.
 if rd:
  r=get("https://api.openaire.eu/graph/v4/research-products",{"filter":"ids.doi:"+rd,"page_size":5},timeout=30)
  if r is not None and r.ok:
   for item in r.json().get("results",[]):
    if match_title(title,item.get("mainTitle") or item.get("title") or "")>=0.85:
     works.append(("OpenAIRE",item))
 # CORE authenticated queries when configured; unauthenticated limited to first 75 records.
 if key or core_calls<75:
  q='doi:"'+rd+'"' if rd else '"'+title[:145]+'"'
  r=get("https://api.core.ac.uk/v3/search/works/",{"q":q,"limit":5},timeout=30);core_calls+=1
  if r is not None and r.ok:
   for item in r.json().get("results",[]):
    candidate_doi=doi(item.get("doi"))
    if rd and candidate_doi and rd!=candidate_doi:continue
    if match_title(title,item.get("title",""))<0.94:continue
    if year and item.get("yearPublished") and str(item["yearPublished"])[:4]!=year:continue
    works.append(("CORE",item))
 tried=set()
 for provider,item in works:
  for origin,url in discovered_pdf_urls(item):
   if url in tried:continue
   tried.add(url)
   response=get(url,timeout=55)
   data=response.content if response is not None and response.ok else b""
   pages,why=is_pdf(data,title,rd) if data else (None,"request_failed")
   attempts.append({"ris_record":rid,"provider":provider,"url":url,"http":response.status_code if response is not None else "error","outcome":why})
   if pages:
    sha=hashlib.sha256(data).hexdigest()
    filename=f"RIS814972_record_{int(rid):04d}_sha_{sha[:14]}.pdf"
    path=ROOT/filename;path.write_bytes(data)
    out.update(status="success",reason="identity_verified_from_pdf",filename=filename,sha256=sha,source_url=response.url,provider=provider)
    if pending and (len(pending)>=75 or bytes_pending+len(data)>200*1024*1024):upload()
    pending.append((path,out));bytes_pending+=len(data);break
  if out["status"]=="success":break
 if n%25==0:
  dump();print("PROGRESS",n,"successes",sum(x["status"]=="success" for x in results),flush=True)
upload();dump()
for result in results:
 if result["status"]=="success":
  registry.append({"ris_record":result["ris_record"],"zenodo_deposit_id":result["deposit_id"],"filename":result["filename"],"sha256":result["sha256"],"retrieval_status":"downloaded","identity_status":"verified_from_pdf"})
with (ROOT/"cumulative_registry.csv").open("w",newline="",encoding="utf-8") as f:
 fields=["ris_record","zenodo_deposit_id","filename","sha256","retrieval_status","identity_status"]
 w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(sorted(registry,key=lambda r:int(r["ris_record"])))
with (ROOT/"remaining_ris_records.csv").open("w",newline="",encoding="utf-8") as f:
 fields=list(rows[0])
 w=csv.DictWriter(f,fieldnames=fields);w.writeheader()
 current={r["ris_record"] for r in registry}
 w.writerows([r for r in rows if r["ris_record"] not in current])
summary={"original_records":384,"previous_downloads":13,"new_downloads":sum(x["status"]=="success" for x in results),"total_downloaded":len(registry),"remaining":384-len(registry),"core_queries":core_calls,"deposits":deposits}
(ROOT/"summary.json").write_text(json.dumps(summary,indent=2))
print(json.dumps(summary,indent=2),flush=True)
