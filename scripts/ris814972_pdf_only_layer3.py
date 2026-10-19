#!/usr/bin/env python3
"""RIS 814972: exact-DOI Europe PMC, OpenAlex OA and Unpaywall PDF-only retrieval.
Writes independently auditable manifests; uploads validated PDFs in restricted Zenodo drafts.
Never publishes deposits and never accepts HTML or XML as a PDF.
"""
import csv, hashlib, io, json, os, re, time, zipfile
from pathlib import Path
from urllib.parse import quote
import requests
from pypdf import PdfReader

INPUT=Path("data/ris_openalex_audit_20261019.csv")
OUT=Path("outputs/ris814972_pdf_layer3")
OUT.mkdir(parents=True,exist_ok=True)
PREVIOUS={"28","87","217","303","308","359","124","179","275","306"}
MAX_ARCHIVE_BYTES=200*1024*1024
MAX_FILES=75
ZENODO_TOKEN=os.environ.get("ZENODO_TOKEN","")
OA_KEY=os.environ.get("OPENALEX_API_KEY","")
UNPAYWALL_EMAIL=os.environ.get("UNPAYWALL_EMAIL","")
SESSION=requests.Session()
SESSION.headers.update({"User-Agent":"fulltexttest/ris814972-pdf-only/1.0 (research PDF retrieval)"})
rows=[r for r in csv.DictReader(INPUT.open(encoding="utf-8",newline="")) if r["ris_record"] not in PREVIOUS]
manifest=[];attempts=[];pending=[];archive_bytes=0;deposits=[]
def norm(x):
    return re.sub(r"^(https?://(dx\.)?doi\.org/|doi:)","",(x or "").strip(),flags=re.I).lower().rstrip(" .;,")
def get(url,params=None,timeout=30):
    for n in range(3):
        try:
            r=SESSION.get(url,params=params,timeout=(12,timeout),allow_redirects=True)
            if r.status_code in (429,500,502,503,504) and n<2:
                time.sleep(2**n);continue
            return r
        except requests.RequestException:
            if n<2:time.sleep(2**n)
    return None
def inspect_pdf(data):
    if not data.startswith(b"%PDF") or len(data)<5000:return None
    try:
        pdf=PdfReader(io.BytesIO(data),strict=False)
        pages=len(pdf.pages)
        chars=sum(len(p.extract_text() or "") for p in pdf.pages[:min(3,pages)])
        if pages<1 or chars<150:return None
        return pages,chars
    except Exception:return None
def write_logs():
    with (OUT/"attempts.csv").open("w",newline="",encoding="utf-8") as f:
        names=["ris_record","doi","provider","url","status","detail"]
        w=csv.DictWriter(f,fieldnames=names);w.writeheader();w.writerows(attempts)
    with (OUT/"record_results.csv").open("w",newline="",encoding="utf-8") as f:
        names=["ris_record","title","doi","status","provider","file_name","sha256","pages","source_url","deposit_id"]
        w=csv.DictWriter(f,fieldnames=names);w.writeheader();w.writerows(manifest)
    (OUT/"deposit_index.json").write_text(json.dumps(deposits,indent=2))
def flush():
    global pending,archive_bytes
    if not pending:return
    write_logs()
    if not ZENODO_TOKEN:raise RuntimeError("ZENODO_TOKEN missing: cannot archive PDFs")
    dep_url="https://zenodo.org/api/deposit/depositions"
    headers={"Authorization":"Bearer "+ZENODO_TOKEN}
    rr=requests.post(dep_url,json={},headers=headers,timeout=60);rr.raise_for_status()
    dep=rr.json();ident=dep["id"]
    depsize=sum(f.stat().st_size for f,_ in pending)
    title=f"RIS 814972 PDF-only OA retrieval batch {len(deposits)+1}"
    metadata={"metadata":{"title":title,"upload_type":"dataset","description":"Unpublished restricted deposit of provenance-checked PDF-only retrievals. File name contains original RIS record number; per-record manifest and SHA-256 hashes stored in ZIP. Do not publish without checking copyright and permissions.","creators":[{"name":"Haddaway, Neal"}],"access_right":"closed"}}
    rr=requests.put(f"{dep_url}/{ident}",json=metadata,headers=headers,timeout=60);rr.raise_for_status()
    zpath=OUT/f"ris814972_pdf_batch_{len(deposits)+1:03d}.zip"
    with zipfile.ZipFile(zpath,"w",zipfile.ZIP_DEFLATED) as z:
        for path,record in pending:z.write(path,path.name)
        z.writestr("batch_manifest.json",json.dumps([rec for _,rec in pending],indent=2))
    with zpath.open("rb") as f:
        rr=requests.put(dep["links"]["bucket"]+"/"+zpath.name,data=f,headers=headers,timeout=(30,600));rr.raise_for_status()
    for _,rec in pending:rec["deposit_id"]=str(ident)
    deposits.append({"deposit_id":ident,"url":f"https://zenodo.org/deposit/{ident}","record_count":len(pending),"raw_pdf_bytes":depsize,"zip_name":zpath.name,"status":"restricted_draft_uploaded"})
    print("ZENODO DRAFT",ident,"files",len(pending),flush=True)
    pending=[];archive_bytes=0;write_logs()
for i,r in enumerate(rows,1):
    rid=r["ris_record"];doi=norm(r.get("doi"));title=r.get("title","")
    record={"ris_record":rid,"title":title,"doi":doi,"status":"not_found","provider":"","file_name":"","sha256":"","pages":"","source_url":"","deposit_id":""}
    manifest.append(record)
    urls=[];seen=set()
    def add(provider,url):
        if isinstance(url,str) and url.startswith(("https://","http://")) and url not in seen:
            seen.add(url);urls.append((provider,url))
    if doi:
        # Europe PMC exact DOI match, including the fullTextUrlList; XML is deliberately ignored.
        resp=get("https://www.ebi.ac.uk/europepmc/webservices/rest/search",{"query":f'DOI:"{doi}"',"resultType":"core","format":"json","pageSize":10})
        if resp is not None and resp.ok:
            for h in resp.json().get("resultList",{}).get("result",[]):
                if norm(h.get("doi"))!=doi:continue
                for u in (h.get("fullTextUrlList") or {}).get("fullTextUrl",[]):
                    link=u.get("url","")
                    if u.get("documentStyle","").lower()=="pdf" or link.lower().split("?")[0].endswith(".pdf"):
                        add("EuropePMC",link)
                pmcid=h.get("pmcid","")
                if pmcid and h.get("isOpenAccess","").upper()=="Y":
                    add("EuropePMC_PMC_PDF",f"https://pmc.ncbi.nlm.nih.gov/articles/{pmcid}/pdf/")
        # OpenAlex: external locations, including non-.pdf suffixes (validated by PDF signature).
        resp=get("https://api.openalex.org/works/https://doi.org/"+quote(doi,safe=""),({"api_key":OA_KEY} if OA_KEY else {}))
        if resp is not None and resp.ok:
            work=resp.json()
            if norm(work.get("doi"))==doi:
                for loc in ([work.get("best_oa_location")]+(work.get("locations") or [])):
                    if isinstance(loc,dict):add("OpenAlex_OA_location",loc.get("pdf_url"))
        # Unpaywall may expose alternative PDF links.
        if UNPAYWALL_EMAIL:
            resp=get("https://api.unpaywall.org/v2/"+quote(doi,safe=""),{"email":UNPAYWALL_EMAIL})
            if resp is not None and resp.ok:
                doc=resp.json()
                for loc in ([doc.get("best_oa_location")]+(doc.get("oa_locations") or [])):
                    if isinstance(loc,dict):add("Unpaywall",loc.get("url_for_pdf"))
    else:
        # No automatic title-guess PDF attachment: preserve identity safety.
        record["status"]="missing_doi_needs_verified_title_match"
    for provider,url in urls:
        response=get(url,timeout=55)
        status=str(response.status_code) if response is not None else "connection_error"
        data=response.content if response is not None and response.ok else b""
        valid=inspect_pdf(data) if data else None
        attempts.append({"ris_record":rid,"doi":doi,"provider":provider,"url":url,"status":status,"detail":"valid_pdf" if valid else "failed_or_not_valid_pdf"})
        if not valid:continue
        digest=hashlib.sha256(data).hexdigest()
        filename=f"RIS814972_record_{int(rid):04d}_sha_{digest[:14]}.pdf"
        path=OUT/filename;path.write_bytes(data)
        record.update(status="success",provider=provider,file_name=filename,sha256=digest,pages=valid[0],source_url=response.url)
        if pending and (archive_bytes+len(data)>MAX_ARCHIVE_BYTES or len(pending)>=MAX_FILES):flush()
        pending.append((path,record));archive_bytes+=len(data)
        break
    if i%25==0:
        write_logs();print(f"PROGRESS {i}/{len(rows)} successes={sum(z['status']=='success' for z in manifest)}",flush=True)
flush();write_logs()
summary={"input_remaining":len(rows),"successful":sum(z["status"]=="success" for z in manifest),"not_retrieved":sum(z["status"]!="success" for z in manifest),"deposits":deposits,"policy":"PDF only, no XML, no public publication"}
(OUT/"summary.json").write_text(json.dumps(summary,indent=2))
print(json.dumps(summary,indent=2),flush=True)
