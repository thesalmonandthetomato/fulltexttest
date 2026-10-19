#!/usr/bin/env python3
"""Read-only OpenAlex metadata availability audit for an RIS-derived inventory."""
import csv, json, os, time, urllib.parse, urllib.request, urllib.error
from concurrent.futures import ThreadPoolExecutor
from difflib import SequenceMatcher
from pathlib import Path

SOURCE=Path("data/ris_openalex_audit_20261019.csv")
OUT=Path("outputs/openalex_availability")
OUT.mkdir(parents=True, exist_ok=True)
API_KEY=os.environ.get("OPENALEX_API_KEY","")
def normal(s): return " ".join((s or "").lower().split())
def query(url):
    if API_KEY: url += ("&" if "?" in url else "?")+"api_key="+urllib.parse.quote(API_KEY)
    for attempt in range(4):
        try:
            req=urllib.request.Request(url,headers={"User-Agent":"fulltexttest-availability-audit/1.0 (scholarly research)","Accept":"application/json"})
            with urllib.request.urlopen(req,timeout=25) as response:
                return json.load(response),None
        except (urllib.error.HTTPError,urllib.error.URLError,TimeoutError) as exc:
            reason=str(exc)
            if isinstance(exc,urllib.error.HTTPError) and exc.code in (400,404):return None,reason
            if attempt<3:time.sleep(2**attempt)
    return None,reason

def check(row):
    doi=(row.get("doi") or "").strip().lower()
    title=(row.get("title") or "").strip()
    out=dict(row)
    out.update(openalex_match="no",match_method="",openalex_id="",openalex_doi="",matched_title="",title_similarity="",openalex_pdf="no",openalex_xml="no",external_oa_pdf="no",external_oa_pdf_urls="",oa_status="",query_error="")
    work=None
    if doi:
        link="https://api.openalex.org/works/https://doi.org/"+urllib.parse.quote(doi,safe="")
        work,error=query(link)
        out["match_method"]="exact_doi"
        if error and "404" not in error:out["query_error"]=error
        if work and normal(work.get("doi","")).removeprefix("https://doi.org/") != doi:work=None
    elif title:
        endpoint="https://api.openalex.org/works?search="+urllib.parse.quote(title)+"&per-page=5"
        data,error=query(endpoint)
        if error:out["query_error"]=error
        options=(data or {}).get("results") or []
        scored=sorted(((SequenceMatcher(None,normal(title),normal(w.get("display_name",""))).ratio(),w) for w in options),key=lambda x:x[0],reverse=True)
        if scored:
            out["title_similarity"]=f"{scored[0][0]:.3f}"
            if scored[0][0]>=0.95:
                work=scored[0][1]
                out["match_method"]="high_confidence_title"
            else:out["match_method"]="title_candidate_unconfirmed"
    if work:
        out["openalex_match"]="yes"
        out["openalex_id"]=work.get("id","")
        out["openalex_doi"]=work.get("doi","")
        out["matched_title"]=work.get("display_name","")
        content=work.get("has_content") or {}
        out["openalex_pdf"]="yes" if content.get("pdf") else "no"
        out["openalex_xml"]="yes" if content.get("grobid_xml") else "no"
        out["oa_status"]=(work.get("open_access") or {}).get("oa_status","")
        locations=[work.get("best_oa_location")]+(work.get("locations") or [])
        urls=sorted(set(x.get("pdf_url") for x in locations if isinstance(x,dict) and x.get("pdf_url")))
        out["external_oa_pdf"]="yes" if urls else "no"
        out["external_oa_pdf_urls"]=" | ".join(urls)
    return out

with SOURCE.open(encoding="utf-8-sig",newline="") as f: rows=list(csv.DictReader(f))
print(f"Checking {len(rows)} records against OpenAlex; metadata only",flush=True)
with ThreadPoolExecutor(max_workers=4) as pool:
    results=list(pool.map(check,rows))
    if len(results)!=len(rows):raise RuntimeError("Mismatch in result count")
fields=list(results[0].keys())
with (OUT/"record_availability.csv").open("w",newline="",encoding="utf-8") as f:
    writer=csv.DictWriter(f,fieldnames=fields);writer.writeheader();writer.writerows(results)
def count(k):return sum(r[k]=="yes" for r in results)
summary={"total_records":len(results),"doi_records":sum(bool(r.get("doi")) for r in results),
"matched_openalex":count("openalex_match"),"openalex_hosted_pdf":count("openalex_pdf"),
"openalex_hosted_xml":count("openalex_xml"),"external_oa_pdf_link":count("external_oa_pdf"),
"any_reported_pdf":sum(r["openalex_pdf"]=="yes" or r["external_oa_pdf"]=="yes" for r in results),
"no_reported_fulltext":sum(r["openalex_match"]=="yes" and r["openalex_pdf"]=="no" and r["openalex_xml"]=="no" and r["external_oa_pdf"]=="no" for r in results),
"unmatched":sum(r["openalex_match"]=="no" and not r["query_error"] for r in results),
"query_errors":sum(bool(r["query_error"]) for r in results),
"downloaded_pdfs":0}
(OUT/"summary.json").write_text(json.dumps(summary,indent=2)+"\n",encoding="utf-8")
print(json.dumps(summary,indent=2),flush=True)
