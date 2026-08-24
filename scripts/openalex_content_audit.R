# ============================================================
# OpenAlex content audit
# ============================================================
# Metadata-only audit: no PDF/XML content is downloaded.
# Resumable and resilient to transient OpenAlex timeouts.
# ============================================================

library(httr2)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

INPUT_FILE <- "data/living_evidence_map_master.csv"
OUT_FILE <- "data/openalex_content_audit.csv"
SUMMARY_FILE <- "data/openalex_content_audit_summary.csv"
STATE_FILE <- "data/openalex_content_audit_state.csv"
PDF_CANDIDATES <- "data/openalex_pdf_candidates.csv"
GROBID_CANDIDATES <- "data/openalex_grobid_candidates.csv"
EITHER_CANDIDATES <- "data/openalex_pdf_or_grobid_candidates.csv"
BATCH_SIZE <- 100
API_URL <- "https://api.openalex.org/works"
OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY", unset = "")

normalise_doi <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_remove(x, regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE))
  x <- str_remove(x, regex("^doi:\\s*", ignore_case = TRUE))
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

`%||%` <- function(x, y) if (is.null(x)) y else x

if (!file.exists(INPUT_FILE)) stop("Cannot find input file: ", INPUT_FILE, call. = FALSE)

master <- read_csv(INPUT_FILE, show_col_types = FALSE)
if (!"doi" %in% names(master)) {
  stop("The main database does not contain a column called 'doi'. Columns found: ",
       paste(names(master), collapse = ", "), call. = FALSE)
}

DOIS <- master |>
  mutate(source_row = row_number(), doi = normalise_doi(doi)) |>
  filter(!is.na(doi), doi != "") |>
  distinct(doi, .keep_all = TRUE) |>
  select(source_row, doi)

message("Unique DOI records: ", nrow(DOIS))

# IMPORTANT: because the previous version constructed the DOI filter incorrectly,
# an old audit must not be resumed. Rename/delete the old audit files before running
# this corrected version. We detect the old state explicitly below.
if (file.exists(STATE_FILE) && file.exists(OUT_FILE)) {
  old_state <- suppressWarnings(read_csv(STATE_FILE, show_col_types = FALSE))
  if ("script_version" %in% names(old_state) && any(old_state$script_version == "2")) {
    results <- read_csv(OUT_FILE, show_col_types = FALSE)
    completed_n <- max(old_state$completed_n, na.rm = TRUE)
    message("Resuming corrected audit from DOI ", completed_n + 1L, ".")
  } else {
    stop(
      "Existing audit files were produced by the previous (incorrect) DOI query. ",
      "Delete/rename these files before starting the corrected audit:\n",
      "  ", OUT_FILE, "\n",
      "  ", STATE_FILE, "\n",
      "Then run this script again from DOI 1.",
      call. = FALSE
    )
  }
} else {
  results <- tibble()
  completed_n <- 0L
}

# ------------------------------------------------------------
# ONE OPENALEX REQUEST
# ------------------------------------------------------------
# Use raw normalized DOI values in filter=doi:, not https://doi.org/ URLs.
# This is the documented OpenAlex DOI filter syntax.

request_openalex <- function(dois) {
  doi_filter <- paste(dois, collapse = "|")

  req <- request(API_URL) |>
    req_url_query(
      filter = paste0("doi:", doi_filter),
      `per-page` = length(dois),
      select = paste(c(
        "id", "doi", "display_name", "publication_year",
        "open_access", "best_oa_location", "has_content",
        "has_fulltext", "content_urls"
      ), collapse = ",")
    ) |>
    req_user_agent("fulltexttest/openalex-content-audit/2.0") |>
    req_timeout(90)

  if (nzchar(OPENALEX_API_KEY)) {
    req <- req |> req_url_query(api_key = OPENALEX_API_KEY)
  }

  for (attempt in 1:4) {
    message("  OpenAlex metadata request: ", length(dois),
            " DOIs; attempt ", attempt, "/4")

    result <- tryCatch(req_perform(req), error = function(e) e)

    if (inherits(result, "error")) {
      if (attempt == 4) return(NULL)
      delay <- min(60, 2 ^ (attempt - 1) * 2)
      message("  Request error; waiting ", delay, " seconds")
      Sys.sleep(delay)
      next
    }

    status <- resp_status(result)

    if (status == 429 || (status >= 500 && status < 600)) {
      if (attempt == 4) return(NULL)
      retry_after <- suppressWarnings(as.numeric(resp_header(result, "retry-after")))
      delay <- if (!is.na(retry_after)) retry_after else min(60, 2 ^ (attempt - 1) * 2)
      message("  HTTP ", status, "; waiting ", delay, " seconds")
      Sys.sleep(delay)
      next
    }

    resp_check_status(result)
    return(resp_body_json(result, simplifyVector = FALSE))
  }

  NULL
}

# ------------------------------------------------------------
# RESILIENT BATCH REQUEST
# ------------------------------------------------------------

get_openalex_batch <- function(dois) {
  payload <- request_openalex(dois)
  if (!is.null(payload)) return(payload)

  if (length(dois) == 1L) {
    stop("OpenAlex request failed repeatedly for DOI: ", dois[[1]], call. = FALSE)
  }

  midpoint <- floor(length(dois) / 2)
  left <- dois[seq_len(midpoint)]
  right <- dois[(midpoint + 1):length(dois)]

  message("  Request failed after retries; splitting ", length(dois),
          " DOIs into ", length(left), " + ", length(right))

  left_payload <- get_openalex_batch(left)
  right_payload <- get_openalex_batch(right)

  list(
    meta = list(count = 0L),
    results = c(left_payload$results %||% list(),
                right_payload$results %||% list())
  )
}

work_to_row <- function(input_doi, work) {
  if (is.null(work)) {
    return(tibble(
      input_doi = input_doi, openalex_id = NA_character_,
      openalex_doi = NA_character_, doi_exact_match = FALSE,
      title = NA_character_, publication_year = NA_integer_,
      oa_is_oa = FALSE, oa_status = NA_character_, oa_license = NA_character_,
      has_pdf = FALSE, has_grobid_xml = FALSE, has_fulltext = FALSE,
      pdf_url = NA_character_, grobid_xml_url = NA_character_,
      status = "no_exact_openalex_match"
    ))
  }

  openalex_doi <- normalise_doi(safe_chr(work$doi))
  oa <- work$open_access %||% list()
  best <- work$best_oa_location %||% list()
  content <- work$has_content %||% list()
  urls <- work$content_urls %||% list()

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
  start_positions <- seq(from = completed_n + 1L, to = nrow(DOIS), by = BATCH_SIZE)

  for (start in start_positions) {
    end <- min(start + BATCH_SIZE - 1L, nrow(DOIS))
    batch <- DOIS[start:end, ]

    message("")
    message("====================================================")
    message("DOI BATCH: ", start, "-", end, " of ", nrow(DOIS))
    message("====================================================")

    payload <- get_openalex_batch(batch$doi)
    works <- payload$results %||% list()

    # Sanity check: a zero-result batch is suspicious and must not be silently
    # interpreted as 100 missing OpenAlex records.
    if (length(works) == 0L) {
      stop(
        "OpenAlex returned ZERO works for a batch of ", length(batch$doi),
        " DOIs (", start, "-", end, "). This is treated as a query failure,",
        " not as 100 genuine non-matches. No checkpoint was written for this batch.",
        call. = FALSE
      )
    }

    by_doi <- setNames(
      works,
      map_chr(works, ~ normalise_doi(safe_chr(.x$doi)))
    )

    batch_results <- map_dfr(batch$doi, function(doi) {
      work <- by_doi[[doi]]
      work_to_row(doi, work)
    })

    results <- bind_rows(results, batch_results) |>
      distinct(input_doi, .keep_all = TRUE)

    write_csv(results, OUT_FILE)

    completed_n <- end
    write_csv(
      tibble(
        script_version = "2",
        completed_n = completed_n,
        updated_at_utc = format(Sys.time(), tz = "UTC")
      ),
      STATE_FILE
    )

    message("Checkpoint saved: ", completed_n, " / ", nrow(DOIS))
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
    "DOI records checked", "Exact OpenAlex DOI matches", "OpenAlex OA records",
    "PDF available", "GROBID XML available", "PDF OR GROBID available",
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
  mutate(percent_of_doi_records = round(100 * count / n, 2))

write_csv(summary, SUMMARY_FILE)

pdf_candidates <- results |> filter(doi_exact_match, has_pdf)
grobid_candidates <- results |> filter(doi_exact_match, has_grobid_xml)
either_candidates <- results |> filter(doi_exact_match, has_pdf | has_grobid_xml)

write_csv(pdf_candidates, PDF_CANDIDATES)
write_csv(grobid_candidates, GROBID_CANDIDATES)
write_csv(either_candidates, EITHER_CANDIDATES)

message("")
message("====================================================")
message("OPENALEX CONTENT AUDIT COMPLETE")
message("====================================================")
print(summary)
message("")
message("NO PDFs OR GROBID FILES WERE DOWNLOADED.")
message("====================================================")
