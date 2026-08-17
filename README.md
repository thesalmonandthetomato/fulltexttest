# Living Evidence Map Full-Text Extraction

This repository is for the one-time full-text extraction pilot on the included salmon aquaculture evidence base.

## Purpose

Develop and validate controlled ontologies and a reproducible full-text extraction pipeline before scaling to the full corpus.

## Extraction principle

The primary evidence source for study-characteristic extraction is the **Methods**, followed by **Results**. Introduction, Discussion and References are not primary evidence for study characteristics and should be used only where explicitly justified for corroboration or interpretation.

Every extracted field should, where possible, retain supporting text evidence and an extraction-confidence/uncertainty flag.

## Pilot

Initial pilot target: the first 100 sampled records, with approximately 73 currently having an accessible full-text document from the retrieval tests.

## Planned stages

1. Finalise extraction ontology and coding rules.
2. Build and validate text retrieval and section extraction.
3. Run a 100-record pilot on obtainable full texts.
4. Manually audit a sample of outputs and revise the ontology/rules.
5. Scale the validated pipeline to the full included corpus.
