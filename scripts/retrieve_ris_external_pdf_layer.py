#!/usr/bin/env python3
"""Layer 2: retrieve confirmed .pdf-path OA links, preserve RIS provenance; stage privately to Zenodo."""
import csv, hashlib, io, json, os, re, time, zipfile
from pathlib import Path
from collections import defaultdict
import requests
from pypdf import PdfReader

ROOT=Path("outputs/ris_814972_external_pdf_layer")
ROOT.mkdir(parents=True,exist_ok=True)
SOURCE=Path("data/ris_814972_external_pdf_candidates_20261019.csv")
rows=list(csv.DictReader(SOURCE.open(encoding="utf-8",newline="")))
if len(rows)!=10: raise SystemExit(f"Expected 10 audited direct-PDF URL entries, got {len(rows)}")
# Only previously validated OpenAlex-hosted successes are skipped. No presumed successes.
PREVIOUSLY_COMPLETE={"28","87","217","303","308","359"}
results=[]
success={}
session=requests.Session()
session.headers.update({"User-Agent":"fulltexttest/ris-oa-pdf-retrieval/1.0","Accept":"application/pdf"})
for i,row in enumerate(rows,1):
    ris=row["ris_record"].strip()
    url=row["url"].strip()
    out=dict(row)
    out.update(attempt_number=i,status="not_attempted",reason="",saved_filename="",sha256="",bytes="",pages="",extracted_characters="",resolved_url="",zenodo_id="")
    if ris in PREVIOUSLY_COMPLETE:
        out.update(status="already_retrieved_openalex",reason="previously successful hosted PDF")
    elif ris in success:
        out.update(status="skipped_record_already_succeeded",reason=success[ris])
    else:
        try:
            response=session.get(url,timeout=(15,75),allow_redirects=True)
            out["resolved_url"]=response.url
            response.raise_for_status()
            data=response.content
            if not data.startswith(b"%PDF"):raise ValueError("Response not a PDF (checked file signature)")
            if len(data)<5000:raise ValueError("PDF too small")
            reader=PdfReader(io.BytesIO(data),strict=False)
            if not len(reader.pages):raise ValueError("PDF has zero pages")
            chars=sum(len(page.extract_text() or "") for page in reader.pages)
            if chars<500:raise ValueError("PDF text too short for automated content validation")
            ident=hashlib.sha256(data).hexdigest()
            doi_slug=re.sub(r"[^a-zA-Z0-9]+","_",row.get("doi","") or "no_doi").strip("_")[:70]
            filename=f"RIS814972_record_{int(ris):04d}_doi_{doi_slug}_sha_{ident[:12]}.pdf"
            (ROOT/filename).write_bytes(data)
            out.update(status="success",saved_filename=filename,sha256=ident,bytes=len(data),pages=len(reader.pages),extracted_characters=chars)
            success[ris]=filename
        except Exception as ex:
            out.update(status="failed",reason=(type(ex).__name__+": "+str(ex))[:350])
    results.append(out)
    print(f"{i}/10 RIS {ris}: {out['status']} {out['reason']}",flush=True)

fields=list(results[0].keys())
with (ROOT/"attempt_manifest.csv").open("w",encoding="utf-8",newline="") as f:
    wr=csv.DictWriter(f,fieldnames=fields);wr.writeheader();wr.writerows(results)
with (ROOT/"new_successful_records.csv").open("w",encoding="utf-8",newline="") as f:
    wr=csv.DictWriter(f,fieldnames=["ris_record","doi","saved_filename","sha256","url"])
    wr.writeheader()
    for r in results:
        if r["status"]=="success":wr.writerow({k:r[k] for k in wr.fieldnames})
summary={"candidate_urls":len(rows),"candidate_records":len({r["ris_record"] for r in rows}),"previously_completed_records":sorted({r["ris_record"] for r in results if r["status"]=="already_retrieved_openalex"}),"newly_downloaded_records":len(success),"failed_attempts":sum(r["status"]=="failed" for r in results),"zenodo":"not_uploaded"}
(ROOT/"summary.json").write_text(json.dumps(summary,indent=2))
if success:
    archive=ROOT/"ris814972_external_pdf_layer_successes.zip"
    with zipfile.ZipFile(archive,"w",zipfile.ZIP_DEFLATED) as z:
        for filename in success.values():z.write(ROOT/filename,filename)
        z.write(ROOT/"attempt_manifest.csv","attempt_manifest.csv")
        z.write(ROOT/"new_successful_records.csv","new_successful_records.csv")
    token=os.environ.get("ZENODO_TOKEN")
    if not token:raise SystemExit("ZENODO_TOKEN missing; PDFs retained as Actions artifact only")
    headers={"Authorization":"Bearer "+token}
    base="https://zenodo.org/api/deposit/depositions"
    resp=requests.post(base,json={},headers=headers,timeout=60);resp.raise_for_status()
    deposit=resp.json();deposit_id=deposit["id"]
    summary.update(zenodo="draft_created",zenodo_id=deposit_id,zenodo_url=f"https://zenodo.org/deposit/{deposit_id}")
    (ROOT/"summary.json").write_text(json.dumps(summary,indent=2))
    meta={"metadata":{"title":"RIS 814972 external open-access PDF retrieval layer, 19 October 2026","upload_type":"dataset","description":"Validated PDFs from externally hosted OA URLs discovered in the OpenAlex audit, with source RIS record IDs and provenance manifest. Restricted unpublished draft.","creators":[{"name":"Haddaway, Neal"}],"access_right":"closed"}}
    resp=requests.put(base+"/"+str(deposit_id),json=meta,headers=headers,timeout=60);resp.raise_for_status()
    with archive.open("rb") as fh:
        resp=requests.put(deposit["links"]["bucket"]+"/"+archive.name,data=fh,headers=headers,timeout=(30,600));resp.raise_for_status()
    summary["zenodo"]="restricted_draft_uploaded"
    (ROOT/"summary.json").write_text(json.dumps(summary,indent=2))
print(json.dumps(summary,indent=2),flush=True)
