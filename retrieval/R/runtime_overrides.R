# Runtime-safe replacements for network helpers.
# All curl arguments are passed directly to system2(); no shell interpolation.

clean_discovered_url <- function(x) {
  x <- as.character(x)
  x <- gsub('\\\\/', '/', x, fixed = FALSE)
  x <- gsub('\\\\u0026', '&', x, fixed = TRUE)
  x <- gsub('\\\\u003d', '=', x, fixed = TRUE)
  x <- gsub('\\\\u003f', '?', x, fixed = TRUE)
  x <- gsub('\\\\u003a', ':', x, fixed = TRUE)
  x <- gsub('&amp;', '&', x, fixed = TRUE)
  x <- utils::URLdecode(x)
  x <- sub('[),.;]+$', '', x)
  trimws(x)
}

# Extract only actual href targets. Never scan arbitrary search-page text:
# author names/prose must not become curl candidates.
html_href_urls <- function(txt) {
  if (!nzchar(txt)) return(character())
  hits <- regmatches(txt, gregexpr('(?i)href[[:space:]]*=[[:space:]]*["\\x27]([^"\\x27]+)["\\x27]', txt, perl = TRUE))[[1]]
  if (!length(hits)) return(character())
  vals <- sub('(?i)^href[[:space:]]*=[[:space:]]*["\\x27]', '', hits, perl = TRUE)
  vals <- sub('["\\x27]$', '', vals, perl = TRUE)
  vals <- vals[grepl('^https?://', vals, ignore.case = TRUE)]
  unique(clean_discovered_url(vals))
}

json_urls <- function(txt, fields = c('url_for_pdf', 'pdf_url', 'landing_page_url', 'url', 'URL')) {
  if (!nzchar(txt)) return(character())
  out <- character()
  for (field in fields) {
    pat <- paste0('"', field, '"\\s*:\\s*"(https?[^"\\r\\n]*)"')
    hits <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
    if (length(hits)) {
      vals <- sub(paste0('^"', field, '"\\s*:\\s*"'), '', hits, perl = TRUE)
      vals <- sub('"$', '', vals)
      out <- c(out, vals)
    }
  }
  unique(clean_discovered_url(out[nzchar(out)]))
}

safe_request <- function(url, timeout_seconds = 120L) {
  url <- clean_discovered_url(url)
  if (!nzchar(url) || !grepl('^https?://', url, ignore.case = TRUE)) {
    return(list(ok = FALSE, status = NA_integer_, type = '', body = raw(), final_url = url, error = 'invalid_url', elapsed = 0))
  }
  started <- Sys.time(); tmp <- tempfile(fileext = '.bin'); hdr <- tempfile(fileext = '.headers')
  on.exit(unlink(c(tmp, hdr)), add = TRUE)
  args <- c('-L', '--fail-with-body', '--silent', '--show-error', '--max-time', as.character(timeout_seconds),
            '--connect-timeout', '20', '--retry', '3', '--retry-delay', '4', '--retry-max-time', '45',
            '--retry-all-errors', '--user-agent', 'fulltexttest-clean-r/4.3', '-D', hdr, '-o', tmp, url)
  status <- suppressWarnings(system2('curl', args, stdout = FALSE, stderr = FALSE))
  body <- if (file.exists(tmp)) readBin(tmp, 'raw', n = file.info(tmp)$size) else raw()
  headers <- if (file.exists(hdr)) readLines(hdr, warn = FALSE) else character()
  status_line <- tail(grep('^HTTP/', headers, value = TRUE), 1L)
  code <- if (length(status_line)) suppressWarnings(as.integer(sub('^HTTP/[0-9.]+\\s+([0-9]+).*$', '\\1', status_line))) else NA_integer_
  ct_hits <- grep('^Content-Type:', headers, ignore.case = TRUE, value = TRUE)
  ctype <- if (length(ct_hits)) trimws(sub('^Content-Type:\\s*', '', tail(ct_hits, 1L), ignore.case = TRUE)) else ''
  loc_hits <- grep('^Location:', headers, ignore.case = TRUE, value = TRUE)
  final_url <- if (length(loc_hits)) clean_discovered_url(sub('^Location:\\s*', '', tail(loc_hits, 1L), ignore.case = TRUE)) else url
  list(ok = status == 0L && !is.na(code) && code >= 200L && code < 300L, status = code, type = ctype, body = body,
       final_url = final_url, error = if (status != 0L) paste0('curl_exit_', status) else '',
       elapsed = as.numeric(difftime(Sys.time(), started, units = 'secs')))
}

.discovery_curl <- function(url, timeout_seconds = 20L, tries = 2L) {
  r <- safe_request(url, min(as.integer(timeout_seconds), 60L))
  if (isTRUE(r$ok) && length(r$body)) tryCatch(rawToChar(r$body), error = function(e) '') else ''
}

discover_search_urls <- function(row, timeout_seconds = 30L) {
  title_col <- find_first_column(names(row), c('title', 'short_title', 'title_normalised'))
  author_col <- find_first_column(names(row), c('authors', 'author', 'first_author'))
  doi_col <- find_first_column(names(row), c('doi'))
  title <- if (!is.null(title_col)) normalise(row[[title_col]]) else ''
  authors <- if (!is.null(author_col)) normalise(row[[author_col]]) else ''
  doi <- if (!is.null(doi_col)) normalise(row[[doi_col]]) else ''
  if (!nzchar(title)) return(character())

  out <- character()
  queries <- c(paste0('"', title, '" filetype:pdf'), paste0('"', title, '" ', if (nzchar(authors)) authors else '', ' full text'))
  for (q in queries) {
    enc <- utils::URLencode(q, reserved = TRUE)
    ddg <- .discovery_curl(paste0('https://html.duckduckgo.com/html/?q=', enc), min(timeout_seconds, 25L))
    if (nzchar(ddg)) {
      hrefs <- html_href_urls(ddg)
      hrefs <- hrefs[!grepl('duckduckgo\\.com|duck\\.co', hrefs, ignore.case = TRUE)]
      out <- c(out, hrefs)
    }
    Sys.sleep(2)
  }

  if (nzchar(doi)) {
    oa <- .discovery_curl(paste0('https://api.openalex.org/works/https://doi.org/', utils::URLencode(doi, reserved = TRUE),
                                 '?mailto=fulltexttest@example.org'), min(timeout_seconds, 25L))
    if (nzchar(oa)) out <- c(out, json_urls(oa, c('url_for_pdf', 'pdf_url', 'landing_page_url', 'url')))
    uw <- .discovery_curl(paste0('https://api.unpaywall.org/v2/', utils::URLencode(doi, reserved = TRUE),
                                 '?email=fulltexttest@example.org'), min(timeout_seconds, 25L))
    if (nzchar(uw)) out <- c(out, json_urls(uw, c('url_for_pdf', 'url')))
    out <- c(out, paste0('https://doi.org/', doi))
  }

  out <- clean_discovered_url(out)
  unique(out[nzchar(out) & grepl('^https?://', out, ignore.case = TRUE)])
}
