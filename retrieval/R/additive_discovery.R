# Additive discovery/audit layer.
# The established fulltext.R retriever remains the baseline. This layer runs only
# after that baseline fails, then records every discovery/retrieval outcome.

.normalise_title_words <- function(title) {
  x <- tolower(as.character(title %||% ""))
  x <- gsub("[^[:alnum:]]+", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

.title_terms <- function(title) {
  x <- unlist(strsplit(.normalise_title_words(title), " ", fixed = TRUE))
  unique(x[nchar(x) >= 3L])
}

.strict_url <- function(x) {
  x <- trimws(as.character(x))
  if (!length(x) || !nzchar(x) || grepl("[[:space:]<>\\\"'`()]", x) ||
      !grepl("^https?://", x, ignore.case = TRUE)) return(NA_character_)
  x <- gsub("\\\\/", "/", x, fixed = FALSE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- utils::URLdecode(x)
  if (!grepl("^https?://[^/[:space:]]+", x, ignore.case = TRUE)) return(NA_character_)
  x
}

.safe_curl_text <- function(url, timeout = 20L) {
  url <- .strict_url(url)
  if (is.na(url)) return("")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  args <- c("-L", "--fail", "--silent", "--show-error", "--max-time", as.character(timeout),
            "--connect-timeout", "10", "--retry", "1", "--retry-delay", "3",
            "--user-agent", "Mozilla/5.0 (X11; Linux x86_64) fulltexttest/1.0",
            "-o", tmp, url)
  status <- suppressWarnings(system2("curl", args, stdout = FALSE, stderr = FALSE))
  if (status != 0L || !file.exists(tmp)) return("")
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

.hrefs <- function(html) {
  if (!nzchar(html)) return(character())
  m <- regmatches(html, gregexpr("(?i)href[[:space:]]*=[[:space:]]*[\\\"']([^\\\"']+)[\\\"']", html, perl = TRUE))[[1]]
  if (!length(m)) return(character())
  x <- sub("(?i)^href[[:space:]]*=[[:space:]]*[\\\"']", "", m, perl = TRUE)
  x <- sub("[\\\"']$", "", x)
  x <- x[grepl("^https?://", x, ignore.case = TRUE)]
  unique(na.omit(vapply(x, .strict_url, character(1))))
}

.search_google <- function(q) {
  u <- paste0("https://www.google.com/search?q=", utils::URLencode(q, reserved = TRUE), "&num=10")
  h <- .safe_curl_text(u, 20L)
  x <- .hrefs(h)
  x[!grepl("google\\.", x, ignore.case = TRUE)]
}

.search_scholar <- function(q) {
  u <- paste0("https://scholar.google.com/scholar?q=", utils::URLencode(q, reserved = TRUE), "&hl=en")
  h <- .safe_curl_text(u, 20L)
  x <- .hrefs(h)
  x[!grepl("scholar.google", x, ignore.case = TRUE)]
}

.openalex_candidates <- function(title, doi = "") {
  q <- utils::URLencode(.normalise_title_words(title), reserved = TRUE)
  u <- paste0("https://api.openalex.org/works?search=", q, "&per-page=10")
  h <- .safe_curl_text(u, 20L)
  if (!nzchar(h)) return(character())
  # OpenAlex JSON can contain escaped URLs. Extract URL-bearing fields only.
  hits <- regmatches(h, gregexpr('"(?:pdf_url|landing_page_url|url_for_pdf)"\\s*:\\s*"https?[^"\\r\\n]*"', h, perl = TRUE))[[1]]
  if (!length(hits)) return(character())
  vals <- sub('^"[^\"]+"\\s*:\\s*"', '', hits, perl = TRUE)
  vals <- sub('"$', '', vals)
  unique(na.omit(vapply(vals, .strict_url, character(1))))
}

.other_api_candidates <- function(title, doi = "") {
  out <- character()
  q <- utils::URLencode(.normalise_title_words(title), reserved = TRUE)
  ss <- .safe_curl_text(paste0("https://api.semanticscholar.org/graph/v1/paper/search?query=", q,
                               "&limit=10&fields=title,openAccessPdf,url,externalIds"), 20L)
  if (nzchar(ss)) {
    hits <- regmatches(ss, gregexpr('"url"\\s*:\\s*"https?[^"\\r\\n]+"', ss, perl = TRUE))[[1]]
    if (length(hits)) {
      v <- sub('^"url"\\s*:\\s*"', '', hits, perl = TRUE); v <- sub('"$', '', v)
      out <- c(out, v)
    }
    hits <- regmatches(ss, gregexpr('"openAccessPdf"\\s*:\\s*\\{[^}]*"url"\\s*:\\s*"https?[^"\\r\\n]+"', ss, perl = TRUE))[[1]]
    if (length(hits)) out <- c(out, sub('.*"url"\\s*:\\s*"', '', sub('"$', '', hits)))
  }
  if (nzchar(doi)) {
    uw <- .safe_curl_text(paste0("https://api.unpaywall.org/v2/", utils::URLencode(doi, reserved = TRUE),
                                 "?email=fulltexttest@example.org"), 20L)
    if (nzchar(uw)) {
      hits <- regmatches(uw, gregexpr('"url_for_pdf"\\s*:\\s*"https?[^"\\r\\n]+"', uw, perl = TRUE))[[1]]
      if (length(hits)) out <- c(out, sub('^"url_for_pdf"\\s*:\\s*"', '', sub('"$', '', hits)))
    }
  }
  unique(na.omit(vapply(out, .strict_url, character(1))))
}

.identity_score <- function(text, title) {
  terms <- .title_terms(title)
  if (!length(terms)) return(0)
  z <- tolower(gsub("[^[:alnum:]]+", " ", text, perl = TRUE))
  mean(terms %in% z)
}

.classify_failure <- function(status, content_type, validation_reason, bytes = 0L) {
  if (identical(validation_reason, "identity_failure")) return("identity_failure")
  if (grepl("^request_error|^http_404$|^http_410$|^http_301$|^http_302$", validation_reason)) return("resolution_failure")
  if (grepl("^http_(401|403|429)$|timeout|access|forbidden", validation_reason, ignore.case = TRUE)) return("access_failure")
  if (identical(validation_reason, "no_candidate_urls")) return("discovery_failure")
  if (identical(validation_reason, "unsupported_response")) return("format_failure")
  if (grepl("insufficient_text|no_reference_section|few_reference_markers|extraction", validation_reason, ignore.case = TRUE)) return("extraction_or_validation_failure")
  if (bytes > 0L && !grepl("pdf|html|xml|text", content_type, ignore.case = TRUE)) return("format_failure")
  "discovery_failure"
}

additive_run_one <- function(row, out_dir, timeout_seconds = 30L) {
  # First run the proven retriever exactly as-is.
  baseline <- baseline_run_one(row, out_dir, timeout_seconds)
  status_col <- find_first_column(names(baseline), c("full_text_status", "status"))
  if (!is.null(status_col) && identical(as.character(baseline[[status_col]][1]), "verified_complete")) return(baseline)

  rid_col <- find_first_column(names(row), c("record_id", "id")); rid <- row[[rid_col]]
  title_col <- find_first_column(names(row), c("title", "short_title", "title_normalised")); title <- if (!is.null(title_col)) row[[title_col]] else ""
  doi_col <- find_first_column(names(row), c("doi")); doi <- if (!is.null(doi_col)) row[[doi_col]] else ""
  title_words <- .normalise_title_words(title)

  # Stage-specific candidate sets. Keep provenance so we can calculate yield.
  stages <- list(
    google_pdf = .search_google(paste(title_words, "filetype:pdf")),
    google_scholar = .search_scholar(title_words),
    openalex = .openalex_candidates(title, doi),
    other_resources = .other_api_candidates(title, doi)
  )
  candidates <- unique(unlist(lapply(names(stages), function(s) {
    x <- stages[[s]]; if (!length(x)) return(character()); paste(s, x, sep = "||")
  }), use.names = FALSE))

  attempts <- list()
  if (!length(candidates)) {
    attempts[[1]] <- data.frame(record_id = rid, stage = "discovery", candidate_url = "", status = NA_integer_,
      content_type = "", bytes = 0L, failure_category = "discovery_failure", reason = "no_candidate_urls", stringsAsFactors = FALSE)
  }

  winner <- NULL
  for (item in candidates) {
    sp <- strsplit(item, "||", fixed = TRUE)[[1]]; stage <- sp[[1]]; u <- sp[[2]]
    if (!grepl("^https?://", u, ignore.case = TRUE)) next
    r <- safe_request(u, timeout_seconds)
    if (r$ok) {
      v <- parse_response(r$body, r$type, r$final_url)
      score <- if (!is.null(v$parsed_text)) .identity_score(v$parsed_text, title) else 0
      if (isTRUE(v$ok) && score < 0.15) {
        v$ok <- FALSE; v$reason <- "identity_failure"
      }
    } else {
      v <- list(ok = FALSE, chars = 0L, reference_markers = 0L, reason = if (is.na(r$status)) paste0("request_error: ", r$error) else paste0("http_", r$status), format = "unknown", parsed_text = "")
    }
    attempts[[length(attempts) + 1L]] <- data.frame(record_id = rid, stage = stage, candidate_url = u,
      final_url = r$final_url, status = r$status, content_type = r$type, bytes = length(r$body),
      failure_category = if (isTRUE(v$ok)) "success" else .classify_failure(r$status, r$type, v$reason, length(r$body)),
      reason = v$reason, identity_score = if (exists("score")) score else NA_real_, stringsAsFactors = FALSE)
    if (isTRUE(v$ok)) { winner <- list(response = r, validation = v); break }
  }

  audit_dir <- file.path(out_dir, "audit"); dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  audit <- do.call(rbind, attempts)
  utils::write.csv(audit, file.path(audit_dir, paste0(rid, "_discovery_attempts.csv")), row.names = FALSE, na = "")

  if (is.null(winner)) return(baseline)
  ext <- winner$validation$extension
  dir.create(file.path(out_dir, "documents"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "parsed"), recursive = TRUE, showWarnings = FALSE)
  writeBin(winner$response$body, file.path(out_dir, "documents", paste0(rid, ext)))
  writeLines(winner$validation$parsed_text, file.path(out_dir, "parsed", paste0(rid, ".txt")), useBytes = TRUE)
  data.frame(record_id = rid, full_text_status = "verified_complete", format = winner$validation$format,
             source_url = winner$response$final_url, text_chars = winner$validation$chars,
             reference_markers = winner$validation$reference_markers, stringsAsFactors = FALSE)
}
