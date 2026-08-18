#!/usr/bin/env Rscript
# Build a rectangular 100-row canonical CSV from the authoritative pilot index.
# Existing extraction data can only be joined by record_number and must never
# create/delete rows. Unverified rows retain metadata but have no substantive extraction.

suppressPackageStartupMessages(library(readr))

source_path <- 'data/pilot_index_100.csv'
out_path <- 'data/master_100_extractions.csv'

idx <- read_csv(source_path, show_col_types = FALSE)
stopifnot(nrow(idx) == 100, !anyDuplicated(idx$record_number))

cols <- c(
'record_number','title','doi','retrieval_status','extraction_status','farmed_species',
'study_system_code','study_system_subcode','study_system_detail','geography','study_objective',
'paper_type','evidence_generation','study_approach','study_method','study_context','life_stage',
'fish_origin','facility_code','facility_detail','temporal_scope','study_unit','focal_factor_code',
'focal_factor_detail','comparison_type','comparison_detail','study_outcome_code',
'study_outcome_subcode','study_outcome_detail','evidence_section','evidence_passage',
'extraction_confidence','review_required','new_code_candidate','new_code_parent','new_code_label',
'full_text_url','retrieval_attempts','last_checked','full_text_source','full_text_verified',
'extraction_source_section','extraction_evidence','audit_status','audit_notes'
)

master <- data.frame(matrix('', nrow=nrow(idx), ncol=length(cols), dimnames=list(NULL, cols)), stringsAsFactors=FALSE)
master$record_number <- idx$record_number
master$title <- idx$title
master$retrieval_status <- ifelse(tolower(idx$full_text_obtained)=='yes','retrieval_pending','retrieval_pending')
master$extraction_status <- 'not_started'
master$full_text_verified <- 'false'
master$review_required <- 'false'
master$new_code_candidate <- 'false'
master$audit_status <- 'source_indexed'
master$audit_notes <- 'Row retained from authoritative 100-record index; substantive extraction requires verified full text.'

write_csv(master, out_path, na='')
check <- read_csv(out_path, show_col_types=FALSE)
stopifnot(nrow(check)==100, ncol(check)==45, !anyDuplicated(check$record_number))
cat(sprintf('Built canonical master CSV: %d rows x %d columns\n', nrow(check), ncol(check)))
