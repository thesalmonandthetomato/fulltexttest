# Validate the canonical master dataset using the same schema assumptions as scripts/validate_master.py

validate_master <- function(csv_path = "data/master_100_extractions.csv",
                            json_path = "data/master_100_extractions.json") {
  x <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE,
                na.strings = character())
  stopifnot(ncol(x) == 45L)
  stopifnot(!anyDuplicated(x$record_number))
  stopifnot(all(c("record_number", "title", "retrieval_status",
                  "extraction_status", "full_text_verified", "audit_status") %in% names(x)))
  verified <- tolower(trimws(x$full_text_verified)) == "true"
  substantive <- c("farmed_species", "study_system_code", "study_system_subcode",
                   "study_system_detail", "study_objective", "paper_type",
                   "evidence_generation", "study_approach", "study_method",
                   "study_context", "life_stage", "fish_origin", "facility_code",
                   "facility_detail", "temporal_scope", "study_unit",
                   "focal_factor_code", "focal_factor_detail", "comparison_type",
                   "comparison_detail", "study_outcome_code", "study_outcome_subcode",
                   "study_outcome_detail", "evidence_section", "evidence_passage")
  bad <- which(!verified & apply(x[, substantive, drop = FALSE], 1,
                                function(z) any(trimws(z) != "")))
  stopifnot(length(bad) == 0L)
  invisible(TRUE)
}
