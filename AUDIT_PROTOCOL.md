# Full-text extraction audit protocol

## Purpose
This repository is the auditable record of the 100-study full-text extraction task. The master dataset is one row per study. No parallel spreadsheets are authoritative.

## Source of truth
`data/master_100_extractions.csv` and `data/master_100_extractions.json` are the canonical datasets. `data/pilot_index_100.csv` is the source index. Every study retains its original `record_number`.

## Required status separation
Each row must distinguish:
- `retrieval_status`: not_attempted / searching / obtained / unobtainable / located_not_obtained
- `extraction_status`: not_started / extracted / blocked
- `full_text_url`: URL actually used where available
- `retrieval_attempts`: concise record of routes attempted
- `last_checked`: date checked

Never treat a promising URL, abstract, metadata page, or repository landing page as obtained full text. `obtained` requires usable full-text content that was actually retrieved and parsed.

## Extraction rules
1. Extract only what the source supports.
2. Do not infer a controlled category merely because it is plausible.
3. Preserve source terminology in detail fields.
4. Controlled vocabularies are fixed unless a new value is flagged for human review.
5. Novel categories use `new_code_candidate=true`; they are not silently added.
6. Explicit comparisons are recorded as extraction data, not model summaries.
7. `study_system` identifies the system studied; `farmed_species` identifies the farmed species. They may be different.
8. Facility classifications such as RAS require explicit source support; generic recirculation does not equal RAS.
9. Perspective, opinion, news, review and other non-empirical papers remain eligible for extraction; inapplicable fields may be `not_applicable`.
10. Methods/results are prioritised for empirical extraction, while introduction/discussion/reference material is used only where necessary to establish a supported field.

## Reporting rules
Never report extraction progress using a different denominator. Progress reports must state:
- total studies = 100
- retrieval status counts
- extraction status counts
- number of rows with full-text evidence
- number of candidate new codes

Do not describe records merely written to an output file as 'completed' unless `extraction_status=extracted`.

## Audit trail
Every substantive batch must be committed. Commit messages must identify the operation (retrieval, extraction, correction, ontology change, or audit). Existing extracted values must not be silently overwritten: corrections should be documented in `CHANGELOG.md`.

## Human review
Human-review flags are mandatory for:
- candidate new controlled-vocabulary values;
- ambiguous source classification;
- conflicting evidence;
- extraction confidence below high.

A human review decision should be recorded before a candidate code is promoted into the ontology.
