# Full-text extraction ontology — draft v0.2

**Status:** locked pilot draft for records 1–8, subject to human review of candidate new codes.

## Extraction architecture

Each categorical field should use a **restricted controlled vocabulary** for filtering, aggregation and visualisation. Where the source contains useful terminology that is more specific than the controlled vocabulary, preserve it in a separate `*_detail` field. Do not silently create new controlled categories during extraction.

When a paper appears to require a category not represented by the codebook, use `other`/`other_candidate`, preserve the source wording in the detail field, and flag `new_code_candidate = true`. A human reviewer will approve, reject or modify the proposed new code before it enters the controlled vocabulary.

The extraction therefore follows:

**full text → source-supported extraction → controlled code(s) → optional detail → candidate new code → human approval → codebook update**

## Core fields

### 1. Farmed species
Mandatory anchor for every included record. Use the existing species ontology/codebook. Full-text extraction independently verifies the existing annotation; discrepancies are flagged rather than silently overwritten.

### 2. Study system
The system, environment, population, community, institution, or social/economic setting that is the **primary object or setting of investigation**. Do not simply repeat the farmed species because it is the species under study. Use the farmed animals/species as the study system only when they themselves constitute the primary system being investigated.

**Controlled vocabulary:**
- `farmed_animal`
- `aquaculture_production_system`
- `environmental_system`
- `biological_population`
- `social_system`
- `economic_system`
- `policy_governance_system`
- `multiple`
- `other`
- `unclear`

**Subcodes/examples:**
- aquaculture production: `marine_cage`, `land_based_system`, `pond`, `raceway`, `RAS`, `hatchery`, `other_aquaculture_system`
- environmental: `freshwater`, `marine`, `sediment_benthic`, `ecosystem`, `other_environmental_system`
- biological population: `wild_population`, `microbial_community`, `pathogen_parasite_population`, `other_population`
- social: `local_community`, `farmers_workers`, `consumers`, `other_social_group`

Retain precise source terminology in `study_system_detail`.

### 3. Geography
Study location(s), retaining the existing evidence-map geography logic. Distinguish study location from author affiliation and background mentions.

## Purpose and nature of the paper

### 4. Study objective
A concise summary of the primary objective/research question/aim. This is a free-text field, based primarily on the stated objective, research question or aim.

### 5. Paper type
**Controlled vocabulary:**
- `empirical_research`
- `review_synthesis`
- `modelling_simulation`
- `methodological_technical`
- `perspective_opinion`
- `conceptual_theoretical`
- `commentary_editorial`
- `news_analysis`
- `policy_governance_analysis`
- `case_study`
- `other`
- `unclear`

### 6. Evidence generation
**Controlled vocabulary:**
- `empirical`
- `modelled`
- `conceptual`
- `secondary_review`
- `reported_secondary_information`
- `mixed`
- `unclear`

`reported_secondary_information` is for news articles and similar pieces reporting information from external sources without being a formal review/synthesis.

### 7. Study approach
**Controlled vocabulary:**
- `quantitative`
- `qualitative`
- `mixed_methods`
- `descriptive`
- `modelling`
- `review_synthesis`
- `methodological`
- `conceptual`
- `other`
- `unclear`

Multiple values may apply.

### 8. Study method
**Controlled vocabulary:**
- `experiment`
- `observational_study`
- `field_study`
- `laboratory_study`
- `survey_questionnaire`
- `interview`
- `focus_group`
- `ethnographic_participatory`
- `case_study`
- `monitoring_assessment`
- `comparative_study`
- `modelling_simulation`
- `laboratory_analytical_analysis`
- `genetic_genomic_analysis`
- `review`
- `meta_analysis`
- `methodological_development`
- `perspective_conceptual_analysis`
- `other`
- `not_applicable`
- `unclear`

Multiple values may apply.

## Study context and production descriptors

### 9. Study context
**Controlled vocabulary:**
- `commercial_aquaculture`
- `research_aquaculture`
- `hatchery`
- `laboratory`
- `experimental_research`
- `commercial_farm`
- `field_natural_environment`
- `semi_natural_environment`
- `community_social_setting`
- `administrative_institutional_setting`
- `documentary_literature`
- `multiple`
- `other`
- `unclear`

### 10. Life stage
Use the existing standardised life-stage codebook where available. Preserve author terminology in `life_stage_detail`. Multiple values are permitted.

### 11. Fish origin
**Controlled vocabulary:**
- `commercial_farm`
- `research_facility`
- `hatchery`
- `experimental_stock`
- `wild_derived`
- `wild_caught`
- `multiple`
- `not_applicable`
- `unclear`

### 12. Facility / production system
Use a restricted codebook aligned with the existing evidence-map terminology. Examples include:
- `marine_cage_net_pen`
- `tank`
- `raceway`
- `pond`
- `hatchery_unit`
- `RAS`
- `flow_through`
- `land_based_facility`
- `IMTA`
- `laboratory_system`
- `semi_natural_stream_channel`
- `other`
- `not_applicable`
- `unclear`

Preserve exact facility terminology in `facility_detail`.

### 13. Temporal scope
Free text for study/acquisition period and duration where relevant, with structured dates/duration added where reliably extractable. Do not force qualitative or documentary studies into an experimental-duration framework.

### 14. Study unit
**Controlled vocabulary:**
- `individual_fish`
- `fish_group`
- `tank`
- `cage`
- `pond`
- `farm`
- `site`
- `ecosystem_community`
- `household`
- `individual_person`
- `organisation`
- `document`
- `population`
- `other`
- `not_applicable`
- `unclear`

## What the study investigates

### 15. Focal factor
**Controlled vocabulary:**
- `diet_feed`
- `temperature`
- `salinity`
- `oxygen`
- `stocking_density`
- `disease_pathogen`
- `parasite`
- `husbandry_production_practice`
- `facility_system`
- `environmental_condition`
- `genetic_factor`
- `management_practice`
- `social_factor`
- `economic_factor`
- `policy_regulation`
- `other`
- `none_observational`
- `unclear`

Preserve the actual factor/stressor/intervention in `focal_factor_detail`.

### 16. Comparison
Record an **explicit comparative analysis/design** performed by the authors. Do not infer comparison merely because multiple categories, treatments, systems, places or studies are mentioned.

**Controlled comparison types:**
- `control_reference`
- `treatment_comparison_group`
- `before_after`
- `spatial_comparison`
- `temporal_comparison`
- `species_comparison`
- `system_comparison`
- `dose_gradient_comparison`
- `other`
- `no_explicit_comparison`
- `unclear`

Record the actual things compared in `comparison_detail`. Multiple comparisons are permitted.

### 17. Study outcome
Use a restricted hierarchical outcome codebook. Initial controlled domains:
- `physiological`
- `production`
- `health_disease`
- `behaviour_welfare`
- `microbiology`
- `ecological`
- `environmental`
- `genetic_genomic`
- `economic`
- `social`
- `policy_governance`
- `perception_attitude`
- `other`
- `not_applicable`
- `unclear`

Where useful, use a controlled subcode (e.g. `physiological → metabolism`; `production → growth`; `microbiology → community_structure`). Preserve the actual measured/assessed outcome in `study_outcome_detail`. Multiple outcomes are permitted.

## Existing evidence-map classification

### 18–21. Topic hierarchy
Retain and align with the existing abstract-level evidence-map hierarchy:
- Broad topic
- Subtopic
- Feature
- Component

The full-text process should not create a competing topic hierarchy.

## Evidence and quality control

### 22. Evidence section
**Controlled vocabulary:** `methods`, `results`, `methods_results`, `supplementary_material`, `other`, `not_applicable`.

### 23. Evidence passage
Short source-supported passage(s) from the full text. Do not substitute model summaries for source evidence.

### 24. Extraction confidence
`high`, `medium`, `low`.

### 25. Review required
Boolean flag indicating ambiguity, contradiction, poor support or a candidate new code.

### 26. New code candidate
Boolean plus proposed parent/code/label/detail fields. A proposed code is **not part of the controlled vocabulary until human approval**.

### 27. Full-text quality/status
Record full-text availability/quality and whether Methods and Results sections were detected.

## General extraction rules

1. Prioritise Methods and Results for factual study-characteristic extraction.
2. Use Introduction/Discussion only as corroborating context unless the relevant information is genuinely unavailable elsewhere.
3. Do not treat references or background mentions as evidence that the focal study investigated something.
4. Never infer a characteristic when the full text does not support it; use `unclear`/`not_applicable` as appropriate.
5. Preserve the distinction between farmed species and study system; do not duplicate species in study system without a substantive reason.
6. Preserve multiple values when a paper genuinely contains multiple species, systems, stages, experiments, methods, comparisons or outcomes.
7. A paper may be qualitative, conceptual or opinion-based; absence of conventional experimental methods is not an error.
8. Do not silently overwrite existing manually curated database annotations.
9. For papers containing multiple distinct experiments or study components, retain distinctions where necessary.
10. New controlled codes require human approval. Until approved, use `other`/`unclear`, preserve the source wording, and flag `new_code_candidate=true`.
11. The model must not invent controlled labels merely to make a paper fit the codebook.
12. Keep controlled code, source-supported detail, and evidence passage as separate data elements.
