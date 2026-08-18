# Master extraction audit — 2026-08-18

## Scope
Audited every row currently represented in the master extraction dataset before further batch extraction.

## Findings
- 16 physical rows were present in the current master extraction CSV.
- 15 unique record numbers were represented because record `75078` appeared twice.
- 8 unique records have independently verified usable full text.
- 7 records had substantive extraction fields despite no verified usable full text. These are **not valid extractions** and must be cleared from the canonical master: `72927`, `77054`, `82654`, `81696`, `82603`, `79345`, `74229`.
- `81696` is specifically downgraded: the JAMA page verifies the article and abstract, but that is not sufficient evidence of retrieved full text under this project's rule.

## Verified full-text records
`104-310-034-385-768`, `81121`, `75078`, `81025`, `82667`, `091-162-729-204-060`, `79863`, `81839`.

## Corrective rule
No study-content extraction is valid unless `full_text_verified=true`. Abstract-only, metadata-only, search snippets, publisher landing pages, repository landing pages, and author-request pages do not satisfy this criterion.

## New mandatory provenance fields
Every future master row must include:
- `retrieval_status`
- `full_text_source`
- `full_text_verified`
- `extraction_status`
- `extraction_source_section`
- `extraction_evidence`
- `extraction_confidence`
- `human_review_required`
- `audit_status`
- `audit_notes`

## Next action
The canonical master must be regenerated from the 100-record index with exactly one row per record. Existing unsupported extraction values must not be carried forward. Retrieval and extraction must then proceed in batches of ten, with every batch committed and auditable.
