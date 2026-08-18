#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(readr))
CSV <- 'data/master_100_extractions.csv'
EXPECTED <- 45
x <- read_csv(CSV, show_col_types=FALSE)
stopifnot(nrow(x)==100)
stopifnot(ncol(x)==EXPECTED)
stopifnot(!anyDuplicated(x$record_number))
required <- c('record_number','title','retrieval_status','extraction_status','full_text_verified','audit_status')
stopifnot(all(required %in% names(x)))
substantive <- c('farmed_species','study_system_code','study_system_subcode','study_system_detail','geography','study_objective','paper_type','evidence_generation','study_approach','study_method','study_context','life_stage','fish_origin','facility_code','facility_detail','temporal_scope','study_unit','focal_factor_code','focal_factor_detail','comparison_type','comparison_detail','study_outcome_code','study_outcome_subcode','study_outcome_detail','evidence_section','evidence_passage')
bad <- apply(x,1,function(r) tolower(r[['full_text_verified']])!='true' && any(nzchar(trimws(unname(r[substantive])))))
stopifnot(!any(bad))
cat(sprintf('R VALIDATION PASSED: %d rows x %d columns; no duplicate IDs; no extraction without verified full text.\n',nrow(x),ncol(x)))
