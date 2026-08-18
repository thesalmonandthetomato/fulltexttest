# Full-text retrieval pipeline

Reproducible R/GitHub workflow for discovering, retrieving, validating, and auditing complete scholarly full texts.

## Principle

A HTTP 200 response is not a full text. A record is `verified_complete` only when the retrieved PDF, HTML, or XML contains substantial article content and identifiable references.

No extraction is permitted unless `full_text_status == verified_complete`.

## Layout

- `scripts/` executable workflow entry points
- `R/` reusable retrieval/validation functions
- `data/` input records and schemas
- `outputs/` generated retrieval manifests and downloaded full texts
- `docs/` protocol and data dictionary
- `.github/workflows/` auditable GitHub Actions
