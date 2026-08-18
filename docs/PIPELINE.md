# Auditable pipeline contract

## 1. Canonical inputs and outputs

The canonical data are `data/master_100_extractions.csv` and `data/master_100_extractions.json`. They must contain the same records and the same ordered schema.

## 2. Retrieval

Retrieval attempts are recorded per study. A full text is considered obtained only when a usable document is actually retrieved and its contents can be parsed/inspected. Landing pages, abstracts, metadata, snippets and author-request pages do not qualify.

## 3. Extraction

Substantive extraction is allowed only after `full_text_verified=true`. Methods and Results are prioritised. Introduction, Discussion and References are not primary evidence for study-characteristic extraction.

## 4. Controlled vocabulary

Fields governed by the ontology use restricted codes. If a genuinely new concept is encountered that is not represented, record it as `new_code_candidate=true` with parent/label and stop for human review before incorporating it into the controlled vocabulary.

## 5. Validation

Every data-changing run must execute schema validation. Validation must fail on:

- ragged CSV rows;
- duplicate record numbers;
- CSV/JSON schema mismatch;
- CSV/JSON record-count mismatch;
- substantive extraction without verified full text.

Validation must not silently repair data.

## 6. Audit trail

Every run records the Git commit SHA, GitHub Actions run ID, input hashes, script version, validation result and output hashes. Generated reports belong under `outputs/`; canonical data are not overwritten by reports.

## 7. Human review

Human intervention is reserved for ontology/codebook decisions and flagged extraction uncertainties. It is not required to discover whether a malformed file passed validation: the automated validator must catch that first.
