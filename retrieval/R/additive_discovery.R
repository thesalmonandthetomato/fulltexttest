# Additive discovery/audit layer.
# The established baseline retriever is untouched. This layer is deliberately
# self-contained at its network boundary so discovery cannot inherit malformed
# URL handling or shell behaviour from the baseline.

.normalise_title_words <- function(title) {
  x <- tolower(as.character(title %||% ""))
  x <- gsub("[^[:alnum:]]+", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

.strict_url <- function(x) {
  x <- trimws(as.character(x))
  if (!length(x) || !nzchar(x)) return(NA_character_)
  # Split accidental concatenation before validation.
  starts <- gregexpr("https?://", x, ignore.case = TRUE, perl = TRUE)[[1]]
  if (!length(starts) || starts[1] < 0L) return(NA_character_)
  if (length(starts) > 1L) x <- substring(x, starts[1], starts[2] - 1L)
  x <- gsub("\\\\/", "/", x, fixed = FALSE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- utils::URLdecode(x)
  x <- sub("[),.;]+$", "", x)
  if (grepl("[[:space:]<>\\\"'`(){}]", x) || !grepl("^https?://[^/[:space:]]+", x, ignore.case = TRUE)) return(NA_character_)
  x
}

.unique_urls <- function(x) {
  if (!length(x)) return(character())
  out <- unlist(lapply(as.character(x), function(z) {
    z <- trimws(z)
    starts <- gregexpr("https?://", z, ignore.case = TRUE, perl = TRUE)[[1]]
    if (!length(starts) || starts[1] < 0L) return(character())
    ends <- c(starts[-1L] - 1L, nchar(z))
    vapply(seq_along(starts), function(i) .strict_url(substring(z, starts[i], ends[i])), character(1))
  }), use.names = FALSE)
  unique(out[!is.na(out) & nzchar(out)])
}

.discovery_request <- function(url, timeout = 30L, binary = FALSE) {
  url <- .strict_url(url)
  if (is.na(url)) return(list(ok = FALSE, status = NA_integer_, type = "", body = raw(), final_url = "", error = "invalid_url", elapsed = 0))
  tmp <- tempfile(fileext = if (binary) ".bin" else ".txt")
  hdr <- tempfile(fileext = ".headers")
  on.exit(unlink(c(tmp, hdr)), add = TRUE)
  args <- c("-L", "--fail-with-body", "--silent", "--show-error",
            "--max-time", as.character(timeout), "--connect-timeout", "15",
            "--retry", "2", "--retry-delay", "3", "--user-agent",
            "Mozilla/5.0 fulltexttest-discovery/2.0", "-D", hdr, "-o", tmp, url)
  started <- Sys.time()
  status <- suppressWarnings(system2("curl", args, stdout = FALSE, stderr = FALSE))
  body <- if (file.exists(tmp)) readBin(tmp, "raw", n = file.info(tmp)$size) else raw()
  headers <- if (file.exists(hdr)) readLines(hdr, warn = FALSE) else character()
  hs <- tail(grep("^HTTP/", headers, value = TRUE), 1L)
  code <- if (length(hs)) suppressWarnings(as.integer(sub("^HTTP/[0-9.]+\\s+([0-9]+).*$", "\\1", hs))) else NA_integer_
  ct <- tail(grep("^Content-Type:", headers, ignore.case = TRUE, value = TRUE), 1L)
  ctype <- if (length(ct)) trimws(sub("^Content-Type:\\s*", "", ct, ignore.case = TRUE)) else ""
  loc <- tail(grep("^Location:", headers, ignore.case = TRUE, value = TRUE), 1L)
  final <- if (length(loc)) .strict_url(sub("^Location:\\s*", "", loc, ignore.case = TRUE)) else url
  list(ok = status == 0L && !is.na(code) && code >= 200L && code < 300L,
       status = code, type = ctype, body = body, final_url = final %||% url,
       error = if (status != 0L) paste0("curl_exit_", status) else "",
       elapsed = as.numeric(difftime(Sys.time(), started, units = "secs")))
}

.raw_text <- function(body) tryCatch(rawToChar(body), error = function(e) "")

.hrefs <- function(html, base_url = "") {
  if (!nzchar(html)) return(character())
  m <- regmatches(html, gregexpr("(?is)href[[:space:]]*=[[:space:]]*[\\\"']([^\\\"']+)[\\\"']", html, perl = TRUE))[[1]]
  if (!length(m)) return(character())
  x <- sub("(?is)^href[[:space:]]*=[[:space:]]*[\\\"']", "", m, perl = TRUE)
  x <- sub("[\\\"']$", "", x)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  # Keep absolute URLs and simple root-relative links on the same host.
  abs <- x[grepl("^https?://", x, ignore.case = TRUE)]
  if (nzchar(base_url)) {
    origin <- sub("^((?:https?://[^/]+)).*$", "\\1", base_url, perl = TRUE)
    rel <- x[grepl("^/(?!/)", x, perl = TRUE)]
    abs <- c(abs, paste0(origin, rel))
  }
  .unique_urls(abs)
}

.landing_candidates <- function(url, timeout = 30L) {
  r <- .discovery_request(url, timeout)
  if (!r$ok || !length(r$body)) return(list(urls = character(), response = r))
  ct <- tolower(r$type)
  if (grepl("pdf", ct) || grepl("xml", ct)) return(list(urls = r$final_url, response = r))
  html <- .raw_text(r$body)
  links <- .hrefs(html, r$final_url)
  # Prioritise likely document/download links but retain all same-page candidates.
  score <- grepl("pdf|download|full.?text|bitstream|article|view|file|content", links, ignore.case = TRUE)
  list(urls = unique(c(links[score], links[!score])), response = r)
}

.search_google <- function(q) {
  u <- paste0("https://www.google.com/search?q=", utils::URLencode(q, reserved = TRUE), "&num=10")
  p <- .discovery_request(u, 25L)
  if (!p$ok) return(character())
  x <- .hrefs(.raw_text(p$body), p$final_url)
  .unique_urls(x[!grepl("google\\.", x, ignore.case = TRUE)])
}

.search_scholar <- function(q) {
  u <- paste0("https://scholar.google.com/scholar?q=", utils::URLencode(q, reserved = TRUE), "&hl=en")
  p <- .discovery_request(u, 25L)
  if (!p$ok) return(character())
  x <- .hrefs(.raw_text(p$body), p$final_url)
  .unique_urls(x[!grepl("scholar\\.google", x, ignore.case = TRUE)])
}

.json_url_fields <- function(txt, fields) {
  if (!nzchar(txt)) return(character())
  out <- character()
  for (field in fields) {
    pat <- paste0('"', field, '"[[:space:]]*:[[:space:]]*"(https?[^"\\r\\n]*)"')
    hits <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
    if (length(hits)) {
      vals <- sub(paste0('^"', field, '"[[:space:]]*:[[:space:]]*"'), '', hits, perl = TRUE)
      vals <- sub('"$', '', vals)
      out <- c(out, vals)
    }
  }
  .unique_urls(out)
}

.openalex_candidates <- function(title) {
  q <- utils::URLencode(.normalise_title_words(title), reserved = TRUE)
  r <- .discovery_request(paste0("https://api.openalex.org/works?search=", q, "&per-page=10"), 25L)
  if (!r$ok) return(character())
  txt <- .raw_text(r$body)
  .json_url_fields(txt, c("pdf_url", "landing_page_url", "url_for_pdf"))
}

.other_api_candidates <- function(title, doi = "") {
  out <- character(); q <- utils::URLencode(.normalise_title_words(title), reserved = TRUE)
  r <- .discovery_request(paste0("https://api.semanticscholar.org/graph/v1/paper/search?query=", q,
                                 "&limit=10&fields=title,openAccessPdf,url,externalIds"), 25L)
  if (r$ok) out <- c(out, .json_url_fields(.raw_text(r$body), c("url")))
  if (nzchar(doi)) {
    r <- .discovery_request(paste0("https://api.unpaywall.org/v2/", utils::URLencode(doi, reserved = TRUE),
                                   "?email=fulltexttest@example.org"), 25L)
    if (r$ok) out <- c(out, .json_url_fields(.raw_text(r$body), c("url_for_pdf", "url")))
  }
  .unique_urls(out)
}

.identity_score <- function(text, title) {
  terms <- unique(unlist(strsplit(.normalise_title_words(title), " ", fixed = TRUE)))
  terms <- terms[nchar(terms) >= 3L]
  if (!length(terms)) return(0)
  z <- tolower(gsub("[^[:alnum:]]+", " ", text, perl = TRUE))
  mean(terms %in% strsplit(z, " ", fixed = TRUE)[[1]])
}

.classify_discovery_failure <- function(r, reason = "") {
  if (grepl("identity", reason, ignore.case = TRUE)) return("identity_failure")
  if (identical(reason, "no_candidate_urls")) return("discovery_failure")
  if (!is.na(r$status) && r$status %in% c(401L,403L,429L)) return("access_failure")
  if (!is.na(r$status) && r$status %in% c(404L,410L,301L,302L)) return("resolution_failure")
  if (grepl("pdf|html|xml", r$type, ignore.case = TRUE) && length(r$body) > 0L) return("extraction_or_validation_failure")
  "format_failure"
}

additive_run_one <- function(row, out_dir, timeout_seconds = 30L) {
  # The baseline is authoritative and unchanged.
  baseline <- baseline_run_one(row, out_dir, timeout_seconds)
  status_col <- find_first_column(names(baseline), c("full_text_status", "status"))
  if (!is.null(status_col) && identical(as.character(baseline[[status_col]][1]), "verified_complete")) return(baseline)

  rid_col <- find_first_column(names(row), c("record_id", "id")); rid <- row[[rid_col]]
  title_col <- find_first_column(names(row), c("title", "short_title", "title_normalised")); title <- if (!is.null(title_col)) normalise(row[[title_col]]) else ""
  doi_col <- find_first_column(names(row), c("doi")); doi <- if (!is.null(doi_col)) normalise(row[[doi_col]]) else ""
  url_col <- find_first_column(names(row), c("url_raw", "url", "full_text_url", "source_url"))
  seed_urls <- if (!is.null(url_col)) .unique_urls(unlist(lapply(row[[url_col]], .unique_urls))) else character()

  # Stage 1: existing URLs and DOI locations, including landing-page expansion.
  candidates <- list()
  add_stage <- function(stage, urls) if (length(urls)) candidates[[stage]] <<- unique(candidates[[stage]], urls)
  add_stage("existing_url", seed_urls)
  if (nzchar(doi)) add_stage("doi", paste0("https://doi.org/", doi))
  expanded <- unique(unlist(lapply(unique(unlist(candidates)), function(u) .landing_candidates(u, min(timeout_seconds, 40L))$urls)))
  add_stage("landing_page", expanded)

  # Stage 2: title-word discovery exactly as specified.
  title_words <- .normalise_title_words(title)
  add_stage("google_pdf", .search_google(paste(title_words, "filetype:pdf")))
  add_stage("google_scholar", .search_scholar(title_words))
  add_stage("openalex", .openalex_candidates(title))
  add_stage("other_resources", .other_api_candidates(title, doi))

  rows <- list(); winner <- NULL
  all_items <- unique(unlist(lapply(names(candidates), function(stage) paste(stage, candidates[[stage]], sep = "||")), use.names = FALSE))
  if (!length(all_items)) {
    rows[[1]] <- data.frame(record_id=rid, stage="discovery", candidate_url="", final_url="", status=NA_integer_, content_type="", bytes=0L, failure_category="discovery_failure", reason="no_candidate_urls", identity_score=NA_real_, stringsAsFactors=FALSE)
  }
  for (item in all_items) {
    p <- strsplit(item, "||", fixed=TRUE)[[1]]; stage <- p[[1]]; u <- p[[2]]
    r <- .discovery_request(u, max(timeout_seconds, 45L), binary=TRUE)
    score <- NA_real_; reason <- if (!r$ok) if (is.na(r$status)) paste0("request_error: ", r$error) else paste0("http_", r$status) else ""
    v <- NULL
    if (r$ok) {
      v <- tryCatch(parse_response(r$body, r$type, r$final_url), error=function(e) list(ok=FALSE, chars=0L, reference_markers=0L, reason=paste0("extraction: ", conditionMessage(e)), format="unknown", extension=".bin", parsed_text=""))
      score <- if (!is.null(v$parsed_text)) .identity_score(v$parsed_text, title) else 0
      if (isTRUE(v$ok) && score < 0.15) { v$ok <- FALSE; v$reason <- "identity_failure" }
      reason <- v$reason
    }
    category <- if (isTRUE(v$ok)) "success" else .classify_discovery_failure(r, reason)
    rows[[length(rows)+1L]] <- data.frame(record_id=rid, stage=stage, candidate_url=u, final_url=r$final_url, status=r$status, content_type=r$type, bytes=length(r$body), failure_category=category, reason=reason, identity_score=score, stringsAsFactors=FALSE)
    if (isTRUE(v$ok)) { winner <- list(response=r, validation=v); break }
  }

  audit_dir <- file.path(out_dir, "audit"); dir.create(audit_dir, recursive=TRUE, showWarnings=FALSE)
  utils::write.csv(do.call(rbind, rows), file.path(audit_dir, paste0(rid, "_discovery_attempts.csv")), row.names=FALSE, na="")
  if (is.null(winner)) return(baseline)
  dir.create(file.path(out_dir, "documents"), recursive=TRUE, showWarnings=FALSE)
  dir.create(file.path(out_dir, "parsed"), recursive=TRUE, showWarnings=FALSE)
  writeBin(winner$response$body, file.path(out_dir, "documents", paste0(rid, winner$validation$extension)))
  writeLines(winner$validation$parsed_text, file.path(out_dir, "parsed", paste0(rid, ".txt")), useBytes=TRUE)
  data.frame(record_id=rid, full_text_status="verified_complete", format=winner$validation$format, source_url=winner$response$final_url, text_chars=winner$validation$chars, reference_markers=winner$validation$reference_markers, stringsAsFactors=FALSE)
}
