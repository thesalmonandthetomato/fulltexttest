#!/usr/bin/env Rscript
# Build a rectangular 100-row canonical CSV from the authoritative pilot index.
# Existing extraction/retrieval data are preserved by record_number.
# The build may add rows, but must never delete or invent substantive extraction.

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

empty <- function(n) data.frame(matrix('', nrow=n, ncol=length(cols), dimnames=list(NULL, cols)), stringsAsFactors=FALSE)
master <- empty(nrow(idx))
master$record_number <- as.character(idx$record_number)
master$title <- as.character(idx$title)
master$retrieval_status <- 'retrieval_pending'
master$extraction_status <- 'not_started'
master$full_text_verified <- 'false'
master$review_required <- 'false'
master$new_code_candidate <- 'false'
master$audit_status <- 'source_indexed'
master$audit_notes <- 'Row retained from authoritative 100-record index; substantive extraction requires verified full text.'

# Preserve every non-empty value already present in the canonical master.
if (file.exists(out_path)) {
  old <- tryCatch(read_csv(out_path, show_col_types=FALSE), error=function(e) NULL)
  if (!is.null(old) && 'record_number' %in% names(old) && !anyDuplicated(old$record_number)) {
    common <- intersect(cols, names(old))
    old <- as.data.frame(old, stringsAsFactors=FALSE)
    for (i in seq_len(nrow(master))) {
      j <- match(master$record_number[i], as.character(old$record_number))
      if (!is.na(j)) {
        for (nm in setdiff(common, c('record_number','title'))) {
          v <- old[[nm]][j]
          if (!is.na(v) && nzchar(trimws(as.character(v)))) master[[nm]][i] <- as.character(v)
        }
        if (nzchar(trimws(as.character(old$title[j])))) master$title[i] <- as.character(old$title[j])
      }
    }
  }
}

# Pilot-index metadata may safely fill blank DOI/status fields, but cannot overwrite extraction.
if ('doi' %in% names(idx)) {
  for (i in seq_len(nrow(master))) if (!nzchar(master$doi[i]) && !is.na(idx$doi[i])) master$doi[i] <- as.character(idx$doi[i])
}

write_csv(master, out_path, na='')
check <- read_csv(out_path, show_col_types=FALSE)
stopifnot(nrow(check)==100, ncol(check)==45, !anyDuplicated(check$record_number))
cat(sprintf('Built canonical master CSV: %d rows x %d columns; preserved existing extraction values.\n', nrow(check), ncol(check)))
