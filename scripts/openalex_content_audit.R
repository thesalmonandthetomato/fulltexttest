# ============================================================
# OpenAlex content audit
#
# PURPOSE
#   Check which DOI-bearing records in the main database have
#   downloadable PDF and/or GROBID XML content in OpenAlex.
#
# IMPORTANT
#   This script ONLY queries OpenAlex metadata.
#   It does NOT download PDFs or GROBID XML and therefore does
#   not consume the OpenAlex Content API allowance.
#
# INPUT
#   data/living_evidence_map_master.csv
#
# OUTPUT
#   data/openalex_content_audit.csv
#   data/openalex_content_audit_summary.csv
#   data/openalex_content_audit_state.csv
#   data/openalex_pdf_candidates.csv
#   data/openalex_grobid_candidates.csv
#   data/openalex_pdf_or_grobid_candidates.csv
#
# ============================================================

# Install once if required:
# install.packages(c("httr2", "readr", "dplyr", "purrr", "stringr", "tibble"))

library(httr2)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

INPUT_FILE <- "data/living_evidence_map_master.csv"

OUT_FILE <- "data/openalex_content_audit.csv"
SUMMARY_FILE <- "data/openalex_content_audit_summary.csv"
STATE_FILE <- "data/openalex_content_audit_state.csv"

PDF_CANDIDATES <- "data/openalex_pdf_candidates.csv"
GROBID_CANDIDATES <- "data/openalex_grobid_candidates.csv"
EITHER_CANDIDATES <- "data/openalex_pdf_or_grobid_candidates.csv"

BATCH_SIZE <- 100

API_URL <- "https://api.openalex.org/works"

# Optional: put your OpenAlex API key in the environment as
# OPENALEX_API_KEY. The script will also work without one.
OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY", unset = "")

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

normalise_doi <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_remove(
    x,
    regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE)
  )
  x <- str_remove(
    x,
    regex("^doi:\\s*", ignore_case = TRUE)
  )
  x <- str_remove(x, "[[:space:]]+$")
  x <- str_remove(x, "[.;,]+$")
  str_to_lower(x)
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  as.character(x[[1]])
}

safe_bool <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(FALSE)
  isTRUE(x[[1]])
}

# ------------------------------------------------------------
# READ MAIN DATABASE
# ------------------------------------------------------------

if (!file.exists(INPUT_FILE)) {
  stop("Cannot find input file: ", INPUT_FILE, call. = FALSE)
}

message("Reading: ", INPUT_FILE)

master <- read_csv(
  INPUT_FILE,
  show_col_types = FALSE
)

if (!"doi" %in% names(master)) {
  stop(
    "The main database does not contain a column called 'doi'. Columns found: ",
    paste(names(master), collapse = ", "),
    call. = FALSE
  )
}

DOIS <- master |>
  mutate(
    source_row = row_number(),
    doi = normalise_doi(doi)
  ) |>
  filter(!is.na(doi), doi != "") |>
  distinct(doi, .keep_all = TRUE) |>
  select(source_row, doi)

message("Unique DOI records: ", nrow(DOIS))

# ------------------------------------------------------------
# RESUME SUPPORT
# ------------------------------------------------------------

if (file.exists(OUT_FILE) && file.exists(STATE_FILE)) {

  message("Existing audit found; attempting to resume.")

  results <- read_csv(
    OUT_FILE,
    show_col_types = FALSE
  )

  state <- read_csv(
    STATE_FILE,
    show_col_types = FALSE
  )

  completed_n <- max(state$completed_n, na.rm = TRUE)

} else {

  results <- tibble()
  completed_n <- 0L
}

# ------------------------------------------------------------
# OPENALEX REQUEST FUNCTION
# ------------------------------------------------------------

get_openalex_batch <- function(dois) {

  # OpenAlex accepts up to 100 DOI values in a filter.
  # The DOI filter is OR-separated.
  doi_filter <- paste(
    paste0("https://doi.org/", URLencode(dois, reserved = TRUE)),
    collapse = "|"
  )

  req <- request(API_URL) |>
    req_url_query(
      filter = paste0("doi:", doi_filter),
      `per-page` = length(dois),
      select = paste(
        c(
          "id",
          "doi",
          "display_name",
          "publication_year",
          "open_access",
          "best_oa_location",
          "has_content",
          "has_fulltext",
          "content_urls"
        ),
        collapse = ","
      )
    ) |>
    req_user_agent("fulltexttest/openalex-content-audit/1.0") |>
    req_timeout(60)

  if (nzchar(OPENALEX_API_KEY)) {
    req <- req |>
      req_url_query(api_key = OPENALEX_API_KEY)
  }

  for (attempt in 1:4) {

    message(
      "  OpenAlex metadata request: ",
      length(dois),
      " DOIs; attempt ",
      attempt,
      "/4"
    )

    result <- tryCatch(
      req_perform(req),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      if (attempt == 4) stop(result)
      Sys.sleep(2 ^ (attempt - 1))
      next
    }

    status <- resp_status(result)

    if (status == 429) {
      retry_after <- resp_header(result, "retry-after")
      delay <- suppressWarnings(as.numeric(retry_after))
      if (is.na(delay)) delay <- 2 ^ (attempt - 1)
      delay <- min(delay, 60)
      message("  HTTP 429; waiting ", delay, " seconds")
      Sys.sleep(delay)
      next
    }

    if (status >= 500 && status < 600) {
      if (attempt == 4) {
        resp_check_status(result)
      }
      Sys.sleep(2 ^ (attempt - 1))
      next
    }

    resp_check_status(result)
    return(resp_body_json(result, simplifyVector = FALSE))
  }

  stop("OpenAlex request failed after four attempts.", call. = FALSE)
}

# ------------------------------------------------------------
# CONVERT ONE OPENALEX WORK TO ONE AUDIT ROW
# ------------------------------------------------------------

work_to_row <- function(input_doi, work) {

  if (is.null(work)) {
    return(tibble(
      input_doi = input_doi,
      openalex_id = NA_character_,
      openalex_doi = NA_character_,
      doi_exact_match = FALSE,
      title = NA_character_,
      publication_year = NA_integer_,
      oa_is_oa = FALSE,
      oa_status = NA_character_,
      oa_license = NA_character_,
      has_pdf = FALSE,
      has_grobid_xml = FALSE,
      has_fulltext = FALSE,
      pdf_url = NA_character_,
      grobid_xml_url = NA_character_,
      status = "no_exact_openalex_match"
    ))
  }

  openalex_doi <- normalise_doi(safe_chr(work$doi))

  oa <- work$open_access
  if (is.null(oa)) oa <- list()

  best <- work$best_oa_location
  if (is.null(best)) best <- list()

  content <- work$has_content
  if (is.null(content)) content <- list()

  urls <- work$content_urls
  if (is.null(urls)) urls <- list()

  tibble(
    input_doi = input_doi,
    openalex_id = safe_chr(work$id),
    openalex_doi = openalex_doi,
    doi_exact_match = identical(openalex_doi, input_doi),
    title = safe_chr(work$display_name),
    publication_year = suppressWarnings(as.integer(safe_chr(work$publication_year))),
    oa_is_oa = safe_bool(oa$is_oa),
    oa_status = safe_chr(oa$oa_status),
    oa_license = safe_chr(best$license),
    has_pdf = safe_bool(content$pdf),
    has_grobid_xml = safe_bool(content$grobid_xml),
    has_fulltext = safe_bool(work$has_fulltext),
    pdf_url = safe_chr(urls$pdf),
    grobid_xml_url = safe_chr(urls$grobid_xml),
    status = "matched"
  )
}

# ------------------------------------------------------------
# PROCESS BATCHES
# ------------------------------------------------------------

if (completed_n < nrow(DOIS)) {

  start_positions <- seq(
    from = completed_n + 1L,
    to = nrow(DOIS),
    by = BATCH_SIZE
  )

  for (start in start_positions) {

    end <- min(
      start + BATCH_SIZE - 1L,
      nrow(DOIS)
    )

    batch <- DOIS[start:end, ]

    message("")
    message(
      "===================================================="
    )
    message(
      "DOI BATCH: ",
      start,
      "-",
      end,
      " of ",
      nrow(DOIS)
    )
    message(
      "===================================================="
    )

    payload <- get_openalex_batch(batch$doi)

    works <- payload$results %||% list()

    by_doi <- setNames(
      works,
      map_chr(
        works,
        ~ normalise_doi(safe_chr(.x$doi))
      )
    )

    batch_results <- map_dfr(
      batch$doi,
      function(doi) {
        work <- by_doi[[doi]]
        work_to_row(doi, work)
      }
    )

    results <- bind_rows(
      results,
      batch_results
    ) |>
      distinct(input_doi, .keep_all = TRUE)

    write_csv(
      results,
      OUT_FILE
    )

    completed_n <- end

    write_csv(
      tibble(
        completed_n = completed_n,
        updated_at_utc = format(
          Sys.time(),
          tz = "UTC"
        )
      ),
      STATE_FILE
    )

    message(
      "Checkpoint saved: ",
      completed_n,
      " / ",
      nrow(DOIS)
    )
  }

} else {
  message("Audit already complete; no metadata requests required.")
}

# ------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------

n <- nrow(results)

summary <- tibble(
  metric = c(
    "DOI records checked",
    "Exact OpenAlex DOI matches",
    "OpenAlex OA records",
    "PDF available",
    "GROBID XML available",
    "PDF OR GROBID available",
    "PDF AND GROBID available"
  ),
  count = c(
    n,
    sum(results$doi_exact_match, na.rm = TRUE),
    sum(results$oa_is_oa, na.rm = TRUE),
    sum(results$has_pdf, na.rm = TRUE),
    sum(results$has_grobid_xml, na.rm = TRUE),
    sum(results$has_pdf | results$has_grobid_xml, na.rm = TRUE),
    sum(results$has_pdf & results$has_grobid_xml, na.rm = TRUE)
  )
) |>
  mutate(
    percent_of_doi_records = round(
      100 * count / n,
      2
    )
  )

write_csv(
  summary,
  SUMMARY_FILE
)

# ------------------------------------------------------------
# CANDIDATE LISTS FOR DOWNLOAD STAGE
# ------------------------------------------------------------

pdf_candidates <- results |>
  filter(
    doi_exact_match,
    has_pdf
  )

grobid_candidates <- results |>
  filter(
    doi_exact_match,
    has_grobid_xml
  )

either_candidates <- results |>
  filter(
    doi_exact_match,
    has_pdf | has_grobid_xml
  )

write_csv(pdf_candidates, PDF_CANDIDATES)
write_csv(grobid_candidates, GROBID_CANDIDATES)
write_csv(either_candidates, EITHER_CANDIDATES)

# ------------------------------------------------------------
# FINAL REPORT
# ------------------------------------------------------------

message("")
message("====================================================")
message("OPENALEX CONTENT AUDIT COMPLETE")
message("====================================================")

print(summary)

message("")
message("Audit:       ", OUT_FILE)
message("Summary:     ", SUMMARY_FILE)
message("PDF list:    ", PDF_CANDIDATES)
message("GROBID list: ", GROBID_CANDIDATES)
message("Either list: ", EITHER_CANDIDATES)
message("")
message("NO PDFs OR GROBID FILES WERE DOWNLOADED.")
message("====================================================")

# Helper used above for NULL list elements
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
