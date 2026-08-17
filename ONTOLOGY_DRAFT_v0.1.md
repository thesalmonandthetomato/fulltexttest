# Full-text extraction ontology — draft v0.1

**Status:** locked pilot draft; subject to revision after the first five obtainable records.

## Purpose

Describe what each included paper actually investigated, how the evidence was generated, and what outcomes were assessed. This extends the existing abstract-level evidence-map ontology rather than replacing it.

## Core fields

### 1. Farmed species
Mandatory anchor for every included record. Use the existing species ontology. Full-text extraction independently verifies the existing annotation; discrepancies are flagged rather than silently overwritten.

### 2. Study system
The system, environment, population, community, institution, or social/economic setting that is the **primary object or setting of investigation**. Do not simply repeat the farmed species because it is the species under study. Use the farmed animals/species as the study system only when they themselves constitute the primary system being investigated. Examples include RAS, benthic community, microbial community, pathogen/parasite population, surrounding ecosystem, human/local community, farmers/workers, economic/market system, production system, or farm–environment interaction. Multiple values are permitted.

### 3. Geography
Study location(s), retaining the existing evidence-map geography logic. Distinguish study location from author affiliation and background mentions.

## Purpose and nature of the paper

### 4. Study objective
A concise 1–2 sentence summary of the primary objective/research question/aim of the paper. Based primarily on the stated objective, research question or aim; do not simply copy the abstract and do not confuse background or recommendations with the study objective.

### 5. Paper type
Broad interdisciplinary classification:
- empirical research
- review/synthesis
- modelling/simulation
- methodological/technical
- perspective/opinion
- conceptual/theoretical
- commentary/editorial
- policy/governance analysis
- case study
- other
- unclear

### 6. Evidence generation
How the paper generates or derives its evidence:
- empirical
- modelled
- conceptual
- secondary/review
- mixed
- unclear

### 7. Study approach
Broad methodological orientation:
- quantitative
- qualitative
- mixed methods
- descriptive
- modelling/simulation
- review/synthesis
- methodological/technical
- other
- unclear

Multiple values may apply.

### 8. Study method
What the researchers actually did, where applicable:
- experiment
- observational study
- field study
- laboratory study
- survey/questionnaire
- interview
- focus group
- ethnographic/participatory study
- case study
- monitoring/assessment
- comparative study
- modelling/simulation
- laboratory/analytical analysis
- genetic/genomic analysis
- review
- meta-analysis
- methodological development
- perspective/conceptual analysis
- other
- not applicable
- unclear

Multiple values may apply. `not applicable` is legitimate for papers such as perspectives/opinion pieces where methods are not relevant.

## Study context and biological/production descriptors

### 9. Study context
Where/how the study is situated:
- commercial aquaculture
- research aquaculture
- hatchery
- laboratory
- experimental facility
- commercial farm
- field/natural environment
- community/social setting
- administrative/institutional setting
- documentary/literature
- multiple
- other
- unclear

### 10. Life stage
Life stage(s) of the farmed species actually studied. Retain both author-reported terminology and standardised terminology where useful. Do not infer life stage solely from incidental mentions or unsupported body-weight assumptions.

### 11. Fish origin
Origin of the farmed study animals:
- commercial farm
- research facility
- hatchery
- experimental stock
- wild-derived
- wild-caught
- multiple
- unclear

### 12. Facility / production system
Physical or production system used by the farmed species, where applicable. Use the existing evidence-map terminology where available; otherwise classify conservatively. Examples include sea cage/net pen, tank, raceway, pond, hatchery unit, RAS/recirculating system, flow-through system, land-based facility, IMTA, other, not applicable, unclear.

### 13. Temporal scope
Study/acquisition period and duration where relevant. Do not force qualitative or documentary studies into an experimental duration framework.

### 14. Study unit
The entity at which observation, measurement, intervention, comparison or analysis occurs, e.g. individual fish, group of fish, tank, cage, pond, farm, site, ecosystem/community, household, individual person, organisation, document, population, other, unclear.

## What the study investigates

### 15. Focal factor
The factor, phenomenon, exposure, practice, condition, intervention, issue or subject being investigated, compared, assessed or described. This is deliberately interdisciplinary. Examples include diet/feed, temperature, salinity, oxygen, stocking density, disease/pathogen, parasite, husbandry/production practice, facility/system, environmental condition, genetic factor, management practice, social factor, economic factor, policy/regulation, other, none/observational, unclear.

**Do not use a separate focal-factor-role field.** Whether something is manipulated, observed, compared, described, reported or modelled should be captured through study method/approach and the study description itself.

### 16. Comparison
Record an **explicit comparative analysis/design** performed by the authors. Do not infer comparison merely because multiple categories, treatments, systems, places or studies are mentioned. Where applicable, record the actual things being compared, with a controlled comparison type where useful:
- control/reference
- treatment/comparison group
- before/after
- spatial comparison
- temporal comparison
- species comparison
- system comparison
- dose/gradient comparison
- other
- no explicit comparison
- unclear

Multiple comparisons are permitted. The extraction should preserve the substantive comparison (e.g. `alternative protein sources: fishmeal vs plant/alternative sources`) rather than merely returning `yes`.

### 17. Study outcome
The outcome, response, characteristic, perception, attitude, behaviour, experience, practice, condition, ecological response, production result or other result that the study assesses or seeks to characterise. This is **not restricted to quantitative measurements**. Qualitative constructs such as perceptions/attitudes are valid study outcomes. Multiple outcomes are permitted.

## Existing evidence-map classification

### 18–21. Topic hierarchy
Retain and align with the existing abstract-level evidence-map hierarchy:
- Broad topic
- Subtopic
- Feature
- Component

The full-text process should not create a competing topic hierarchy. It may refine/verify existing topic assignments using the same substantive-topic framework.

## Evidence and quality control

### 22. Evidence section
Where supporting evidence occurs:
- Methods
- Results
- Methods + Results
- Supplementary material
- other

### 23. Evidence passage
Short supporting passage(s) from the full text. Each important extracted characteristic should have traceable supporting evidence where practicable.

### 24. Extraction confidence
- high
- medium
- low

### 25. Review required
Boolean flag indicating that the extraction is ambiguous, contradictory, poorly supported or otherwise merits human review.

### 26. Full-text quality/status
Record whether the full text is available and whether extraction quality is good, acceptable, poor or unusable; also record whether Methods and Results sections were detected.

## General extraction rules

1. Prioritise Methods and Results for factual study-characteristic extraction.
2. Use Introduction/Discussion only as corroborating context unless the relevant information is genuinely unavailable elsewhere.
3. Do not treat references or background mentions as evidence that the focal study investigated something.
4. Never infer a characteristic when the full text does not support it; use `unclear`/`not applicable` as appropriate.
5. Preserve the distinction between the farmed species and the study system; do not duplicate species in study system without a substantive reason.
6. Preserve multiple values when a paper genuinely contains multiple species, systems, stages, experiments, methods, comparisons or outcomes.
7. A paper may be qualitative, conceptual or opinion-based; absence of conventional experimental methods is not an error.
8. Do not silently overwrite existing manually curated database annotations. Full-text results should initially be stored as independent verification/extraction fields.
9. For papers containing multiple distinct experiments or study components, retain experiment/component-level distinctions where necessary rather than collapsing incompatible characteristics into one value.
10. `not applicable` is preferred where a field genuinely does not apply; `unclear` means the field applies but the full text does not support a confident assignment.
