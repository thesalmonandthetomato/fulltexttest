# Retrieval protocol

## Definition of a complete full text

A record is `verified_complete` only if the retrieved object is PDF, HTML, or XML and contains substantial article text plus an identifiable references section with multiple reference entries.

A DOI landing page, metadata page, abstract-only page, ORCID page, repository metadata record, or search result does not qualify.

## Retrieval routes

The pipeline first uses an explicitly supplied full-text URL, then OA discovery through Unpaywall and OpenAlex, followed by DOI resolution. Additional discovery routes can be added as isolated functions. Google Scholar may be used for discovery but the system never attempts to bypass CAPTCHA or human validation.

## Audit requirements

Every candidate URL is logged with HTTP status, content type, byte count, validation result, format, and failure reason. The selected source URL is retained in the status manifest.

## Extraction gate

Data extraction must refuse records whose `full_text_status` is not `verified_complete`.
