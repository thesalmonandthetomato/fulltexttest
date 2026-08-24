#!/usr/bin/env python3
"""
Audit which DOI-bearing records in the main database have downloadable
full-text content in OpenAlex.

This script DOES NOT download PDFs or TEI XML.
It only performs OpenAlex metadata lookups, so it can be run safely before
starting the paid content-download stage.

Outputs:
  data/openalex_content_audit.csv
  data/openalex_content_audit_summary.json
  data/openalex_content_audit_state.json

The audit records, for each input DOI:
  - whether OpenAlex found an exact DOI match
  - OpenAlex work ID
  - has_content.pdf
  - has_content.grobid_xml
  - has_fulltext
  - OA status / license
  - content URLs (when exposed by the API)

OpenAlex permits up to 100 DOI values in one OR filter. Metadata list/filter
requests are inexpensive; this script intentionally does not call the
Content API.
"""

import csv
import json
import os
import re
import time
from pathlib import Path
from urllib.parse import quote

import requests


MANIFEST = Path("data/living_evidence_map_master.csv")
OUT = Path("data/openalex_content_audit.csv")
SUMMARY = Path("data/openalex_content_audit_summary.json")
STATE = Path("data/openalex_content_audit_state.json")

BATCH_SIZE = 100
CHECKPOINT_EVERY = 10
API_URL = "https://api.openalex.org/works"

session = requests.Session()
session.headers.update({
    "User-Agent": "fulltexttest/openalex-content-audit/1.0"
})


def log(message):
    print(f"PROGRESS: {message}", flush=True)


def norm_doi(value):
    if value is None:
        return ""
    value = str(value).strip()
    value = re.sub(r"^https?://(?:dx\.)?doi\.org/", "", value, flags=re.I)
    value = re.sub(r"^doi:\s*", "", value, flags=re.I)
    return value.rstrip(" .;,").lower()


def oa_status(work):
    oa = work.get("open_access") or {}
    return oa.get("oa_status") or ""


def license_name(work):
    loc = work.get("best_oa_location") or {}
    return loc.get("license") or ""


def bool_value(obj, key):
    value = obj.get(key)
    return bool(value) if value is not None else False


def request_metadata(dois, api_key=None):
    """Fetch up to 100 DOI matches in one OpenAlex metadata request."""
    doi_filter = "|".join(
        "https://doi.org/" + quote(doi, safe="/:._-()")
        for doi in dois
    )

    params = {
        "filter": f"doi:{doi_filter}",
        "per-page": len(dois),
        "select": (
            "id,doi,display_name,publication_year,"
            "open_access,best_oa_location,has_content,"
            "has_fulltext,content_urls"
        ),
    }

    if api_key:
        params["api_key"] = api_key

    for attempt in range(4):
        log(
            f"METADATA GET batch={len(dois)} "
            f"attempt={attempt + 1}/4"
        )

        try:
            response = session.get(
                API_URL,
                params=params,
                timeout=(15, 60),
            )
        except requests.RequestException as exc:
            log(f"NETWORK ERROR: {type(exc).__name__}: {exc}")
            if attempt == 3:
                raise
            time.sleep(2 ** attempt)
            continue

        if response.status_code == 429:
            retry_after = response.headers.get("Retry-After")
            try:
                delay = min(60, float(retry_after)) if retry_after else 2 ** attempt
            except ValueError:
                delay = 2 ** attempt
            log(f"HTTP 429; sleeping {delay:.0f}s")
            time.sleep(delay)
            continue

        if not response.ok:
            raise RuntimeError(
                f"OpenAlex metadata request failed: "
                f"HTTP {response.status_code}: {response.text[:500]}"
            )

        payload = response.json()
        return payload.get("results", []), payload.get("meta", {})

    raise RuntimeError("OpenAlex metadata request exhausted retries")


def make_row(input_doi, work=None, error_status=""):
    if work is None:
        return {
            "input_doi": input_doi,
            "openalex_id": "",
            "openalex_doi": "",
            "doi_exact_match": False,
            "title": "",
            "publication_year": "",
            "oa_status": "",
            "oa_is_oa": False,
            "oa_license": "",
            "has_pdf": False,
            "has_grobid_xml": False,
            "has_fulltext": False,
            "pdf_url": "",
            "grobid_xml_url": "",
            "status": error_status,
        }

    has_content = work.get("has_content") or {}
    content_urls = work.get("content_urls") or {}
    openalex_doi = norm_doi(work.get("doi", ""))

    return {
        "input_doi": input_doi,
        "openalex_id": work.get("id", ""),
        "openalex_doi": openalex_doi,
        "doi_exact_match": openalex_doi == input_doi,
        "title": work.get("display_name", ""),
        "publication_year": work.get("publication_year", ""),
        "oa_status": oa_status(work),
        "oa_is_oa": bool_value(work.get("open_access") or {}, "is_oa"),
        "oa_license": license_name(work),
        "has_pdf": bool_value(has_content, "pdf"),
        "has_grobid_xml": bool_value(has_content, "grobid_xml"),
        "has_fulltext": bool_value(work, "has_fulltext"),
        "pdf_url": content_urls.get("pdf") or "",
        "grobid_xml_url": content_urls.get("grobid_xml") or "",
        "status": "matched",
    }


def save_rows(rows):
    OUT.parent.mkdir(parents=True, exist_ok=True)

    fieldnames = [
        "input_doi",
        "openalex_id",
        "openalex_doi",
        "doi_exact_match",
        "title",
        "publication_year",
        "oa_status",
        "oa_is_oa",
        "oa_license",
        "has_pdf",
        "has_grobid_xml",
        "has_fulltext",
        "pdf_url",
        "grobid_xml_url",
        "status",
    ]

    with OUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def save_state(rows, next_index):
    STATE.parent.mkdir(parents=True, exist_ok=True)
    STATE.write_text(
        json.dumps(
            {
                "next_index": next_index,
                "rows_completed": len(rows),
                "updated_at_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            },
            indent=2,
        ),
        encoding="utf-8",
    )


def write_summary(rows, metadata_calls):
    total = len(rows)
    exact = sum(r["doi_exact_match"] for r in rows)
    pdf = sum(r["has_pdf"] for r in rows)
    grobid = sum(r["has_grobid_xml"] for r in rows)
    either = sum(r["has_pdf"] or r["has_grobid_xml"] for r in rows)
    both = sum(r["has_pdf"] and r["has_grobid_xml"] for r in rows)
    oa = sum(r["oa_is_oa"] for r in rows)

    summary = {
        "total_doi_records": total,
        "openalex_exact_doi_matches": exact,
        "openalex_exact_match_percent": round(100 * exact / total, 2) if total else 0,
        "openalex_oa_records": oa,
        "openalex_oa_percent": round(100 * oa / total, 2) if total else 0,
        "openalex_pdf_available": pdf,
        "openalex_pdf_percent": round(100 * pdf / total, 2) if total else 0,
        "openalex_grobid_xml_available": grobid,
        "openalex_grobid_xml_percent": round(100 * grobid / total, 2) if total else 0,
        "openalex_pdf_or_grobid": either,
        "openalex_pdf_or_grobid_percent": round(100 * either / total, 2) if total else 0,
        "openalex_pdf_and_grobid": both,
        "openalex_pdf_and_grobid_percent": round(100 * both / total, 2) if total else 0,
        "metadata_calls": metadata_calls,
        "estimated_list_filter_cost_usd": round(metadata_calls * 0.0001, 4),
        "generated_at_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
    }

    SUMMARY.write_text(
        json.dumps(summary, indent=2),
        encoding="utf-8",
    )

    return summary


def main():
    if not MANIFEST.exists():
        raise SystemExit(f"Missing input file: {MANIFEST}")

    api_key = os.environ.get("OPENALEX_API_KEY", "").strip() or None

    # Read DOI-bearing records and deduplicate by normalized DOI.
    dois = []
    seen = set()
    with MANIFEST.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            doi = norm_doi(row.get("doi", ""))
            if doi and doi not in seen:
                seen.add(doi)
                dois.append(doi)

    log(f"INPUT unique DOI records={len(dois)}")

    # Resume from the saved checkpoint if present and consistent.
    rows = []
    next_index = 0
    metadata_calls = 0

    if STATE.exists() and OUT.exists():
        try:
            state = json.loads(STATE.read_text(encoding="utf-8"))
            next_index = int(state.get("next_index", 0))
            with OUT.open(newline="", encoding="utf-8") as handle:
                rows = list(csv.DictReader(handle))
            # CSV booleans were written as strings; normalize them.
            for row in rows:
                for key in ("doi_exact_match", "oa_is_oa", "has_pdf", "has_grobid_xml", "has_fulltext"):
                    row[key] = str(row[key]).lower() == "true"
            log(f"RESUMING rows={len(rows)} next_index={next_index}")
        except Exception as exc:
            log(f"Could not resume checkpoint cleanly: {exc}")
            rows = []
            next_index = 0

    for start in range(next_index, len(dois), BATCH_SIZE):
        batch = dois[start:start + BATCH_SIZE]
        log(f"BATCH {start + 1}-{start + len(batch)} of {len(dois)}")

        works, meta = request_metadata(batch, api_key=api_key)
        metadata_calls += 1

        by_doi = {
            norm_doi(work.get("doi", "")): work
            for work in works
            if work.get("doi")
        }

        for doi in batch:
            work = by_doi.get(doi)
            if work is None:
                rows.append(make_row(doi, error_status="no_exact_openalex_match"))
            else:
                rows.append(make_row(doi, work=work))

        save_rows(rows)
        save_state(rows, start + len(batch))

        # Summary is useful even if the script is interrupted later.
        summary = write_summary(rows, metadata_calls)
        log(
            "CHECKPOINT "
            f"rows={len(rows)} "
            f"pdf={summary['openalex_pdf_available']} "
            f"grobid={summary['openalex_grobid_xml_available']} "
            f"either={summary['openalex_pdf_or_grobid']}"
        )

    summary = write_summary(rows, metadata_calls)

    # Produce the exact download candidate lists for the next stage.
    pdf_candidates = [r for r in rows if r["has_pdf"] and r["doi_exact_match"]]
    grobid_candidates = [r for r in rows if r["has_grobid_xml"] and r["doi_exact_match"]]
    either_candidates = [
        r for r in rows
        if (r["has_pdf"] or r["has_grobid_xml"]) and r["doi_exact_match"]
    ]

    def write_candidate_file(path, candidates):
        with path.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=rows[0].keys() if rows else ["input_doi"])
            writer.writeheader()
            writer.writerows(candidates)

    write_candidate_file(Path("data/openalex_pdf_candidates.csv"), pdf_candidates)
    write_candidate_file(Path("data/openalex_grobid_candidates.csv"), grobid_candidates)
    write_candidate_file(Path("data/openalex_pdf_or_grobid_candidates.csv"), either_candidates)

    log("============================================")
    log("OPENALEX CONTENT AUDIT COMPLETE")
    log(f"DOIs checked:       {summary['total_doi_records']}")
    log(f"Exact OA matches:   {summary['openalex_exact_doi_matches']}")
    log(f"PDF available:      {summary['openalex_pdf_available']}")
    log(f"GROBID XML:         {summary['openalex_grobid_xml_available']}")
    log(f"PDF OR GROBID:      {summary['openalex_pdf_or_grobid']}")
    log(f"PDF AND GROBID:     {summary['openalex_pdf_and_grobid']}")
    log(f"Metadata calls:     {summary['metadata_calls']}")
    log(f"Est. metadata cost: ${summary['estimated_list_filter_cost_usd']:.4f}")
    log("No content files were downloaded by this script.")
    log("============================================")


if __name__ == "__main__":
    main()
