# Runtime-safe replacements for network helpers.
# These functions are sourced after fulltext.R so that shell metacharacters
# in discovered URLs cannot be interpreted by /bin/sh.

safe_request <- function(url, timeout_seconds = 30L) {
  started <- Sys.time()
  tmp <- tempfile(fileext = ".bin")
  hdr <- tempfile(fileext = ".headers")
  on.exit(unlink(c(tmp, hdr)), add = TRUE)
  args <- c("-L", "--fail-with-body", "--silent", "--show-error",
            "--max-time", as.character(timeout_seconds), "--connect-timeout", "15",
            "--user-agent", "fulltexttest-clean-r/4.1", "-D", shQuote(hdr),
            "-o", shQuote(tmp), shQuote(url))
  status <- suppressWarnings(system2("curl", args, stdout = FALSE, stderr = FALSE))
  body <- if (file.exists(tmp)) readBin(tmp, "raw", n = file.info(tmp)$size) else raw()
  headers <- if (file.exists(hdr)) readLines(hdr, warn = FALSE) else character()
  status_line <- tail(grep("^HTTP/", headers, value = TRUE), 1L)
  code <- if (length(status_line)) suppressWarnings(as.integer(sub("^HTTP/[0-9.]+\\s+([0-9]+).*$", "\\1", status_line))) else NA_integer_
  ct_hits <- grep("^Content-Type:", headers, ignore.case = TRUE, value = TRUE)
  ctype <- if (length(ct_hits)) trimws(sub("^Content-Type:\\s*", "", tail(ct_hits, 1L), ignore.case = TRUE)) else ""
  loc_hits <- grep("^Location:", headers, ignore.case = TRUE, value = TRUE)
  final_url <- if (length(loc_hits)) trimws(sub("^Location:\\s*", "", tail(loc_hits, 1L), ignore.case = TRUE)) else url
  list(ok = status == 0L && !is.na(code) && code >= 200L && code < 300L,
       status = code, type = ctype, body = body, final_url = final_url,
       error = if (status != 0L) paste0("curl_exit_", status) else "",
       elapsed = as.numeric(difftime(Sys.time(), started, units = "secs")))
}

discover_search_urls <- function(row, timeout_seconds = 20L) {
  title_col <- find_first_column(names(row), c("title", "short_title", "title_normalised"))
  author_col <- find_first_column(names(row), c("authors", "author", "first_author"))
  doi_col <- find_first_column(names(row), c("doi"))
  title <- if (!is.null(title_col)) normalise(row[[title_col]]) else ""
  authors <- if (!is.null(author_col)) normalise(row[[author_col]]) else ""
  doi <- if (!is.null(doi_col)) normalise(row[[doi_col]]) else ""
  if (!nzchar(title)) return(character())

  raw_json <- function(url) {
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    args <- c("-L", "--fail", "--silent", "--show-error", "--max-time", as.character(timeout_seconds),
              "--connect-timeout", "10", "--user-agent", "fulltexttest-clean-r/4.1", "-o", shQuote(tmp), shQuote(url))
    status <- suppressWarnings(system2("curl", args, stdout = FALSE, stderr = FALSE))
    if (status != 0L || !file.exists(tmp)) return("")
    paste(readLines(tmp, warn = FALSE), collapse = "")
  }

  q <- utils::URLencode(paste(title, authors), reserved = FALSE)
  out <- character()

  oa <- raw_json(paste0("https://api.openalex.org/works?search=", q, "&per-page=5"))
  if (nzchar(oa)) {
    hits <- regmatches(oa, gregexpr('"pdf_url":"[^"]+"', oa, perl = TRUE))[[1]]
    if (length(hits)) out <- c(out, sub('^"pdf_url":"', '', sub('"$', '', hits)))
    hits <- regmatches(oa, gregexpr('"landing_page_url":"[^"]+"', oa, perl = TRUE))[[1]]
    if (length(hits)) out <- c(out, sub('^"landing_page_url":"', '', sub('"$', '', hits)))
  }

  ss <- raw_json(paste0("https://api.semanticscholar.org/graph/v1/paper/search?query=", q,
                       "&limit=5&fields=title,openAccessPdf,url,externalIds"))
  if (nzchar(ss)) {
    hits <- regmatches(ss, gregexpr('"url":"https?[^" ]+"', ss, perl = TRUE))[[1]]
    if (length(hits)) out <- c(out, sub('^"url":"', '', sub('"$', '', hits)))
  }

  cr <- raw_json(paste0("https://api.crossref.org/works?query.title=", utils::URLencode(title, reserved = FALSE), "&rows=5"))
  if (nzchar(cr)) {
    hits <- regmatches(cr, gregexpr('"URL":"https?[^" ]+"', cr, perl = TRUE))[[1]]
    if (length(hits)) out <- c(out, sub('^"URL":"', '', sub('"$', '', hits)))
  }

  if (nzchar(doi)) {
    out <- c(out,
             paste0("https://api.openalex.org/works/https://doi.org/", utils::URLencode(doi, reserved = TRUE)),
             paste0("https://api.unpaywall.org/v2/", utils::URLencode(doi, reserved = TRUE),
                    "?email=fulltexttest@example.org"))
  }

  # Search-engine discovery through Google's public HTML endpoint. This is a
  # fallback only; no assumptions are made about Google Scholar availability.
  search_q <- utils::URLencode(paste0("\"", title, "\" full text PDF"), reserved = FALSE)
  google <- tryCatch(raw_json(paste0("https://www.google.com/search?q=", search_q, "&num=10")), error = function(e) "")
  if (nzchar(google)) {
    links <- regmatches(google, gregexpr("https?://[^\\\"<> ]+", google, perl = TRUE))[[1]]
    if (length(links)) out <- c(out, links)
  }

  unique(out[nzchar(out)])
}
