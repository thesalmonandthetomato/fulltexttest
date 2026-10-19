#!/usr/bin/env python3
"""Read-only discovery plus validated PDF-only retrieval for RIS 814972.
Resolve DOI-less titles conservatively via OpenAlex; try repository URLs from
OpenAlex and CORE-style institutional links, excluding previously blocked URLs.
Never uses subscription credentials. Stage to restricted Zenodo drafts only.
"""
import csv,hashlib,io,json,os,re,time,zipfile
from pathlib import Path
from difflib import SequenceMatcher
from urllib.parse import quote,urlparse
import requests
from pypdf import PdfReader
BASE=Path("outputs/ris814972_repository_layer4");BASE.mkdir(parents=True,exist_ok=True)
SOURCE=Path("data/ris_openalex_audit_20261019.csv")
DONE={"28","87","217","303","308","359","124","179","275","306","246","125"}
KNOWN_BAD={
"https://researchonline.lshtm.ac.uk/id/eprint/2550086/1/cowBelgium_temperature_accepted.pdf",
"https://etheses.whiterose.ac.uk/26229/1/Sharma_SS_FoodSci_PhD_2019.pdf"
}
KEY=os.getenv("OPENALEX_API_KEY",""); TOKEN=os.getenv("ZENODO_TOKEN","")
EMAIL=os.getenv("UNPAYWALL_EMAIL","")
sess=requests.Session();sess.headers["User-Agent"]="fulltexttest/4.0 (academic research; PDF-only)"
def norm(s):return re.sub(r"^(?:https?://(?:dx\.)?doi\.org/|doi:)","",(s or "").strip(),flags=re.I).lower().rstrip(" .,;")
def txt(s):return re.sub(r"[^a-z0-9]+"," ",(s or "").lower()).strip()
def req(url,params=None,timeout=35):
 for n in range(2):
  try:
   r=sess.get(url,params=params,timeout=(12,timeout))
   if r.status_code in (429,500,502,503,504) and n==0:time.sleep(2);continue
   return r
  except requests.RequestException:
   if n==0:time.sleep(2)
 return None
def metadata(row):
 doi=norm(row.get("doi"));title=row.get("title","")
 if doi:
  r=req("https://api.openalex.org/works/https://doi.org/"+quote(doi,safe=""),{"api_key":KEY} if KEY else None)
  if r is not None and r.ok:
   x=r.json()
   if norm(x.get("doi"))==doi:return x,"exact_doi"
  return None,"doi_unmatched"
 r=req("https://api.openalex.org/works",dict(search=title,per_page=5,**({"api_key":KEY} if KEY else {})))
 if r is None or not r.ok:return None,"title_lookup_error"
 candidates=[]
 for w in r.json().get("results",[]):
  similarity=SequenceMatcher(None,txt(title),txt(w.get("display_name"))).ratio()
  year=re.sub(r"[^0-9]","",row.get("year",""))[:4]
  if similarity>=0.96 and (not year or str(w.get("publication_year"))==year):
   candidates.append((similarity,w))
 if len(candidates)!=1:return None,"title_ambiguous_or_unmatched"
 return candidates[0][1],"strict_title_and_year"
def candidates(w):
 arr=[];seen=set()
 for loc in [w.get("best_oa_location")]+(w.get("locations") or []):
  if not isinstance(loc,dict):continue
  u=loc.get("pdf_url")
  if not isinstance(u,str) or not u.startswith("http") or u in seen or u in KNOWN_BAD:continue
  seen.add(u)
  arr.append(("OpenAlex_repository_pdf",u))
 return arr
def validated(raw):
 if not raw.startswith(b"%PDF") or len(raw)<5000:return None
 try:
  pdf=PdfReader(io.BytesIO(raw),strict=False)
  if len(pdf.pages)<1:return None
  if sum(len(p.extract_text() or "") for p in pdf.pages[:min(5,len(pdf.pages))])<200:return None
  return len(pdf.pages)
 except Exception:return None
rows=[r for r in csv.DictReader(SOURCE.open(encoding="utf-8",newline="")) if r["ris_record"] not in DONE]
results=[];attempts=[];pending=[];size=0;deposits=[]
def write_outputs():
 for name,data,fields in [
  ("results.csv",results,["ris_record","title","doi","match_method","status","filename","sha256","pages","source_url","deposit_id"]),
  ("attempts.csv",attempts,["ris_record","url","provider","http","status"])]:
  with (BASE/name).open("w",newline="",encoding="utf-8") as f:
   w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(data)
 (BASE/"deposits.json").write_text(json.dumps(deposits,indent=2))
def upload():
 global pending,size
 if not pending:return
 if not TOKEN:raise RuntimeError("No ZENODO_TOKEN, cannot archive")
 z=BASE/f"repository_pdf_batch_{len(deposits)+1:03d}.zip"
 with zipfile.ZipFile(z,"w",zipfile.ZIP_DEFLATED) as archive:
  for p,item in pending:archive.write(p,p.name)
  archive.writestr("provenance.json",json.dumps([x for _,x in pending],indent=2))
 api="https://zenodo.org/api/deposit/depositions";headers={"Authorization":"Bearer "+TOKEN}
 r=requests.post(api,json={},headers=headers,timeout=60);r.raise_for_status();dep=r.json();ident=dep["id"]
 meta={"metadata":{"title":f"RIS 814972 verified repository PDF batch {len(deposits)+1}","upload_type":"dataset","creators":[{"name":"Haddaway, Neal"}],"access_right":"closed","description":"Restricted unpublished draft containing validated repository PDFs and original RIS identifiers."}}
 r=requests.put(api+"/"+str(ident),json=meta,headers=headers,timeout=60);r.raise_for_status()
 with z.open("rb") as f:
  r=requests.put(dep["links"]["bucket"]+"/"+z.name,data=f,headers=headers,timeout=(30,600));r.raise_for_status()
 for _,item in pending:item["deposit_id"]=str(ident)
 deposits.append({"id":ident,"files":len(pending),"url":f"https://zenodo.org/deposit/{ident}"})
 pending=[];size=0;write_outputs()
 print("ZENODO",ident,flush=True)
for i,row in enumerate(rows,1):
 rid=row["ris_record"];work,match=metadata(row)
 item={"ris_record":rid,"title":row["title"],"doi":norm(row.get("doi")),"match_method":match,"status":"unavailable","filename":"","sha256":"","pages":"","source_url":"","deposit_id":""}
 results.append(item)
 if work:
  for provider,url in candidates(work):
   r=req(url,timeout=60);data=r.content if r is not None and r.ok else b""
   pages=validated(data) if data else None
   status="valid_pdf" if pages else ("http_"+str(r.status_code) if r is not None and not r.ok else "not_valid_pdf")
   attempts.append({"ris_record":rid,"url":url,"provider":provider,"http":r.status_code if r is not None else "connection_error","status":status})
   if not pages:continue
   checksum=hashlib.sha256(data).hexdigest()
   filename=f"RIS814972_record_{int(rid):04d}_sha_{checksum[:14]}.pdf"
   file=BASE/filename;file.write_bytes(data)
   item.update(status="success",filename=filename,sha256=checksum,pages=pages,source_url=r.url)
   if pending and (size+len(data)>200*1024*1024 or len(pending)>=75):upload()
   pending.append((file,item));size+=len(data)
   break
 if i%25==0:write_outputs();print(f"PROGRESS {i}/{len(rows)} successes={sum(x['status']=='success' for x in results)}",flush=True)
upload();write_outputs()
summary={"remaining_input":len(rows),"success":sum(x["status"]=="success" for x in results),"still_outstanding":sum(x["status"]!="success" for x in results),"title_matched":sum(x["match_method"]=="strict_title_and_year" for x in results),"deposits":deposits}
(BASE/"summary.json").write_text(json.dumps(summary,indent=2))
print(json.dumps(summary,indent=2),flush=True)
