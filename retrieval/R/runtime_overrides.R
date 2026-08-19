# Supplemental discovery only. The established retrieval/validation implementation in
# fulltext.R remains authoritative. This file must only add candidates.

clean_discovered_url <- function(x) {
  x <- as.character(x)
  x <- gsub('\\\\/', '/', x, fixed = FALSE)
  x <- gsub('\\\\u0026', '&', x, fixed = TRUE)
  x <- gsub('\\\\u003d', '=', x, fixed = TRUE)
  x <- gsub('\\\\u003f', '?', x, fixed = TRUE)
  x <- gsub('\\\\u003a', ':', x, fixed = TRUE)
  x <- gsub('&amp;', '&', x, fixed = TRUE)
  x <- utils::URLdecode(x)
  x <- trimws(x)
  x <- sub('[),.;]+$', '', x)
  x
}

valid_http_urls <- function(x) {
  x <- clean_discovered_url(x)
  x <- x[grepl('^https?://', x, ignore.case = TRUE)]
  x <- x[!grepl('[[:space:]<>"`\\x27]', x)]
  x <- x[!grepl('^https?://[^/]*https?://', x, ignore.case = TRUE)]
  unique(x[nzchar(x)])
}

split_concatenated_urls <- function(x) {
  x <- clean_discovered_url(x)
  if (!nzchar(x)) return(character())
  starts <- gregexpr('https?://', x, ignore.case = TRUE, perl = TRUE)[[1]]
  if (starts[1] < 0) return(character())
  ends <- c(starts[-1] - 1L, nchar(x))
  valid_http_urls(substring(x, starts, ends))
}

html_href_urls <- function(txt) {
  if (!nzchar(txt)) return(character())
  hits <- regmatches(txt, gregexpr('(?i)href[[:space:]]*=[[:space:]]*["\\x27]([^"\\x27]+)["\\x27]', txt, perl=TRUE))[[1]]
  if (!length(hits)) return(character())
  vals <- sub('(?i)^href[[:space:]]*=[[:space:]]*["\\x27]', '', hits, perl=TRUE)
  vals <- sub('["\\x27]$', '', vals, perl=TRUE)
  valid_http_urls(vals)
}

json_url_values <- function(txt, fields = c('pdf_url','url_for_pdf','landing_page_url','url','URL')) {
  if (!nzchar(txt)) return(character())
  out <- character()
  for (field in fields) {
    pat <- paste0('"', field, '"[[:space:]]*:[[:space:]]*"(https?[^"\\r\\n]*)"')
    hits <- regmatches(txt, gregexpr(pat, txt, perl=TRUE))[[1]]
    if (length(hits)) {
      vals <- sub(paste0('^"', field, '"[[:space:]]*:[[:space:]]*"'), '', hits, perl=TRUE)
      vals <- sub('"$', '', vals)
      out <- c(out, vals)
    }
  }
  valid_http_urls(out)
}

title_words <- function(title) {
  x <- tolower(normalise(title))
  x <- gsub('[^[:alnum:]]+', ' ', x, perl=TRUE)
  x <- gsub('\\s+', ' ', x, perl=TRUE)
  trimws(x)
}

.discovery_raw <- function(url, timeout_seconds = 25L) {
  url <- valid_http_urls(url)
  if (length(url) != 1L) return('')
  tmp <- tempfile(fileext='.txt')
  on.exit(unlink(tmp), add=TRUE)
  status <- suppressWarnings(system2('curl', c('-L','--fail','--silent','--show-error','--max-time',as.character(timeout_seconds),'--connect-timeout','10','--retry','2','--retry-delay','3','--user-agent','fulltexttest-discovery/2.0','-o',tmp,url), stdout=FALSE, stderr=FALSE))
  if (status != 0L || !file.exists(tmp)) return('')
  paste(readLines(tmp,warn=FALSE),collapse='')
}

discover_from_landing_page <- function(url, timeout_seconds = 20L) {
  txt <- .discovery_raw(url, timeout_seconds)
  if (!nzchar(txt)) return(character())
  hrefs <- html_href_urls(txt)
  hrefs <- hrefs[grepl('pdf|download|full.?text|bitstream|article|view|file', hrefs, ignore.case=TRUE)]
  unique(hrefs)
}

discover_search_urls <- function(row, timeout_seconds = 25L) {
  title_col <- find_first_column(names(row), c('title','short_title','title_normalised'))
  author_col <- find_first_column(names(row), c('authors','author','first_author'))
  doi_col <- find_first_column(names(row), c('doi'))
  url_col <- find_first_column(names(row), c('url_raw','url','full_text_url','source_url'))
  title <- if (!is.null(title_col)) normalise(row[[title_col]]) else ''
  authors <- if (!is.null(author_col)) normalise(row[[author_col]]) else ''
  doi <- if (!is.null(doi_col)) normalise(row[[doi_col]]) else ''
  if (!nzchar(title)) return(character())

  out <- character()
  if (!is.null(url_col) && nzchar(row[[url_col]])) {
    seeds <- split_concatenated_urls(row[[url_col]])
    out <- c(out, seeds)
    for (u in seeds) out <- c(out, discover_from_landing_page(u, min(timeout_seconds,20L)))
  }

  words <- title_words(title)
  q <- utils::URLencode(paste(words, 'filetype:pdf'), reserved=TRUE)
  google <- .discovery_raw(paste0('https://www.google.com/search?q=', q), min(timeout_seconds,20L))
  if (nzchar(google)) out <- c(out, html_href_urls(google))

  oa <- .discovery_raw(paste0('https://api.openalex.org/works?search=',utils::URLencode(words,reserved=TRUE),'&per-page=10'), min(timeout_seconds,20L))
  if (nzchar(oa)) out <- c(out, json_url_values(oa))

  ss <- .discovery_raw(paste0('https://api.semanticscholar.org/graph/v1/paper/search?query=',utils::URLencode(words,reserved=TRUE),'&limit=10&fields=title,openAccessPdf,url,externalIds'), min(timeout_seconds,20L))
  if (nzchar(ss)) out <- c(out, json_url_values(ss, c('url')))
  if (nzchar(ss)) {
    hits <- regmatches(ss, gregexpr('"openAccessPdf"[[:space:]]*:[[:space:]]*\\{[^}]*\\}', ss, perl=TRUE))[[1]]
    if (length(hits)) for (h in hits) out <- c(out, json_url_values(h, c('url')))
  }

  if (nzchar(doi)) {
    oa1 <- .discovery_raw(paste0('https://api.openalex.org/works/https://doi.org/',utils::URLencode(doi,reserved=TRUE)), min(timeout_seconds,20L))
    if (nzchar(oa1)) out <- c(out, json_url_values(oa1))
    uw <- .discovery_raw(paste0('https://api.unpaywall.org/v2/',utils::URLencode(doi,reserved=TRUE),'?email=fulltexttest@example.org'), min(timeout_seconds,20L))
    if (nzchar(uw)) out <- c(out, json_url_values(uw, c('url_for_pdf','url')))
  }

  out <- valid_http_urls(unlist(lapply(out, split_concatenated_urls), use.names=FALSE))
  unique(out)
}
