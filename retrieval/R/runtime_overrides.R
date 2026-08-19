# Runtime-safe replacements for network helpers.
# system2() arguments are passed directly; do NOT add shell quoting to individual
# arguments, because the quote characters become part of the URL and cause
# curl exit 3. We also normalise JSON-escaped URLs before retrieval.

clean_discovered_url <- function(x) {
  x <- as.character(x)
  x <- gsub('\\\\/', '/', x, fixed = FALSE)
  x <- gsub('\\\\u0026', '&', x, fixed = TRUE)
  x <- gsub('\\\\u003d', '=', x, fixed = TRUE)
  x <- gsub('\\\\u003f', '?', x, fixed = TRUE)
  x <- gsub('\\\\u003a', ':', x, fixed = TRUE)
  x <- gsub('\\\\u0022', '"', x, fixed = TRUE)
  x <- sub('[),.;]+$', '', x)
  trimws(x)
}

json_urls <- function(txt, fields = c('url_for_pdf', 'pdf_url', 'landing_page_url', 'url')) {
  if (!nzchar(txt)) return(character())
  out <- character()
  for (field in fields) {
    pat <- paste0('"', field, '"\\s*:\\s*"(https?[^"\\\\]+)"')
    m <- regexec(pat, txt, perl = TRUE)
    hits <- regmatches(txt, m)
    if (length(hits)) {
      vals <- vapply(hits, function(z) if (length(z) >= 2) z[[2]] else '', character(1))
      out <- c(out, vals[nzchar(vals)])
    }
  }
  unique(clean_discovered_url(out[nzchar(out)]))
}

safe_request <- function(url, timeout_seconds = 30L) {
  started <- Sys.time()
  tmp <- tempfile(fileext = ".bin")
  hdr <- tempfile(fileext = ".headers")
  on.exit(unlink(c(tmp, hdr)), add = TRUE)
  args <- c("-L", "--fail-with-body", "--silent", "--show-error",
            "--max-time", as.character(timeout_seconds), "--connect-timeout", "15",
            "--user-agent", "fulltexttest-clean-r/4.2", "-D", hdr,
            "-o", tmp, url)
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

  raw_json <- function(url, ext = ".json") {
    tmp <- tempfile(fileext = ext)
    on.exit(unlink(tmp), add = TRUE)
    args <- c("-L", "--fail", "--silent", "--show-error", "--max-time", as.character(timeout_seconds),
              "--connect-timeout", "10", "--user-agent", "fulltexttest-clean-r/4.2", "-o", tmp, url)
    status <- suppressWarnings(system2("curl", args, stdout = FALSE, stderr = FALSE))
    if (status != 0L || !file.exists(tmp)) return("")
    paste(readLines(tmp, warn = FALSE), collapse = "")
  }

  out <- character()
  q <- utils::URLencode(paste(title, authors), reserved = FALSE)

  # OpenAlex: extract actual OA PDF/landing URLs, not just the API endpoint.
  oa <- raw_json(paste0("https://api.openalex.org/works?search=", q, "&per-page=5"))
  if (nzchar(oa)) out <- c(out, json_urls(oa, c("pdf_url", "landing_page_url")))

  # DOI-specific OpenAlex record is useful when title search is noisy.
  if (nzchar(doi)) {
    oa_one <- raw_json(paste0("https://api.openalex.org/works/https://doi.org/", utils::URLencode(doi, reserved = TRUE)))
    if (nzchar(oa_one)) out <- c(out, json_urls(oa_one, c("url_for_pdf", "pdf_url", "landing_page_url")))
  }

  # Unpaywall: this is the strongest source for legally available repository copies.
  if (nzchar(doi)) {
    uw <- raw_json(paste0("https://api.unpaywall.org/v2/", utils::URLencode(doi, reserved = TRUE), "?email=fulltexttest@example.org"))
    if (nzchar(uw)) out <- c(out, json_urls(uw, c("url_for_pdf", "url")))
  }

  # Semantic Scholar: repository/author manuscript URL when present.
  ss <- raw_json(paste0("https://api.semanticscholar.org/graph/v1/paper/search?query=", q,
                       "&limit=5&fields=title,openAccessPdf,url,externalIds"))
  if (nzchar(ss)) out <- c(out, json_urls(ss, c("url")))

  # Crossref publisher URLs are useful as landing-page candidates.
  cr <- raw_json(paste0("https://api.crossref.org/works?query.title=", utils::URLencode(title, reserved = FALSE), "&rows=5"))
  if (nzchar(cr)) out <- c(out, json_urls(cr, c("URL")))

  # Search-engine discovery. DuckDuckGo's HTML endpoint is much more useful in
  # Actions than Google's JS-heavy page and returns result links in plain HTML.
  search_q <- utils::URLencode(paste0('"', title, '" "full text"'), reserved = FALSE)
  ddg <- raw_json(paste0("https://html.duckduckgo.com/html/?q=", search_q), ext = ".html")
  if (nzchar(ddg)) {
    hrefs <- regmatches(ddg, gregexpr('uddg=https?[^&"<> ]+', ddg, perl = TRUE))[[1]]
    if (length(hrefs)) {
      vals <- sub('^.*uddg=', '', hrefs)
      vals <- utils::URLdecode(vals)
      out <- c(out, vals)
    }
    direct <- regmatches(ddg, gregexpr('https?://[^"<> ]+', ddg, perl = TRUE))[[1]]
    if (length(direct)) out <- c(out, direct)
  }

  # Add DOI resolution last; this is useful when discovery APIs return no OA URL.
  if (nzchar(doi)) out <- c(out, paste0("https://doi.org/", doi))
  out <- clean_discovered_url(out)
  unique(out[nzchar(out) & grepl('^https?://', out)])
}
