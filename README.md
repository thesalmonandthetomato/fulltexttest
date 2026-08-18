# Full-text extraction pipeline

This repository is the auditable, reproducible working environment for the 100-record full-text retrieval and structured extraction pilot.

## Architecture

- `data/` — canonical CSV/JSON data only.
- `scripts/` — executable pipeline code; scripts must be deterministic and non-destructive by default.
- `R/` — R analysis/validation functions.
- `docs/` — ontology, protocol, schema, retrieval and extraction documentation.
- `outputs/` — generated reports, validation results and run summaries. Outputs are never treated as authoritative input data.
- `.github/workflows/` — GitHub Actions used to validate and run pipeline activities.
- `archive/` — superseded artefacts retained for provenance, not for current analysis.

## Core rule

The canonical dataset is one 100-row study table represented in both CSV and JSON. A row is retained whether or not full text is obtainable. Substantive extraction is permitted only when `full_text_verified=true`. Failed retrieval never becomes an inferred extraction.

## Reproducibility

Every pipeline run must record commit SHA, run ID, input file hashes, script/version information, retrieval status, extraction status, validation results, and generated output paths.

GitHub Actions should fail rather than silently repair malformed data. Any transformation of canonical data must be explicit, reviewable and validated before commit.
