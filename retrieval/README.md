# Clean R full-text retrieval pipeline

This pipeline is intentionally independent of the previous retrieval implementation.

## Contract

Input: `data/living_evidence_map_master.csv`.

For each selected record, the pipeline:

1. reads the record as character data;
2. builds candidate locators from the record's URL/DOI fields;
3. makes bounded HTTP requests with explicit timeouts;
4. identifies PDF, XML and HTML responses;
5. validates whether the response contains substantial article text and a references section;
6. stores the original response, parsed text and a machine-readable audit trail;
7. never treats an abstract, metadata page or DOI landing page as a complete full text.

Human validation, CAPTCHA and paywall bypasses are not attempted.
