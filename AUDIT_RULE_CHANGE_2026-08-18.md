# Audit rule change — 2026-08-18

## User instruction
All 100 study rows must be retained in the canonical master dataset.

If exhaustive retrieval fails and usable full text is finally unobtainable:
- retain the study row and all bibliographic/retrieval metadata;
- set `retrieval_status=unobtainable`;
- set `extraction_status=not_started` (or `blocked` where operationally necessary);
- do **not** populate substantive full-text extraction fields from abstracts, metadata, snippets, secondary descriptions, or inference;
- retain the retrieval audit trail so the failed routes are inspectable.

This rule does not require deletion of the row. It requires stopping substantive extraction once full text is finally unobtainable.

## Audit implication
The canonical database therefore remains exactly 100 study rows throughout the project. The number of extracted records is always a subset of those 100 and must never be confused with the number of rows in the master dataset.
