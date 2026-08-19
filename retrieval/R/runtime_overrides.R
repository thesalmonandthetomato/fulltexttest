# Supplemental discovery only. The established retrieval/validation implementation in
# fulltext.R remains authoritative; this file changes only fallback discovery.

clean_discovered_url <- function(x) {
  x <- as.character(x)
  x <- gsub('\\\\/', '/', x, fixed = FALSE)
  x <- gsub('\\\\u0026', '&', x, fixed = TRUE)
  x <- gsub('\\\\u003d', '=', x, fixed = TRUE)
  x <- gsub('\\\\u003f', '?', x, fixed = TRUE)
  x <- gsub('\\\\u003a', ':', x, fixed = TRUE)
  x <- gsub('&amp;', '&', x, fixed = TRUE)
  x <- utils::URLdecode(x)
  trimws(sub('[),.;]+$', '', x))
}

valid_http_urls <- function(x) {
  x <- clean_discovered_url(x)
  x[nzchar(x) & grepl('^https?://[^/[:space:]"<>`]+(?:/[^[:space:]"<>`]*)?$', x, ignore.case = TRUE)]
}

html_href_urls <- function(txt) {
  if (!nzchar(txt)) return(character())
  hits <- regmatches(txt, gregexpr('(?i)href[[:space:]]*=[[:space:]]*["\\x27]([^"\\x27]+)["\\x27]', txt, perl=TRUE))[[1]]
  if (!length(hits)) return(character())
  vals <- sub('(?i)^href[[:space:]]*=[[:space:]]*["\\x27]', '', hits, perl=TRUE)
  vals <- sub('["\\x27]$', '', vals, perl=TRUE)
  valid_http_urls(vals)
}

# Preserve the original fulltext.R safe_request semantics. This is deliberately
# not overridden: the direct-record path was the proven 5/15 baseline.
# Only discover_search_urls below is replaced.

discover_search_urls <- function(row, timeout_seconds = 20L) {
  title_col <- find_first_column(names(row), c('title','short_title','title_normalised'))
  author_col <- find_first_column(names(row), c('authors','author','first_author'))
  doi_col <- find_first_column(names(row), c('doi'))
  title <- if (!is.null(title_col)) normalise(row[[title_col]]) else ''
  authors <- if (!is.null(author_col)) normalise(row[[author_col]]) else ''
  doi <- if (!is.null(doi_col)) normalise(row[[doi_col]]) else ''
  if (!nzchar(title)) return(character())

  # Discovery requests use a separate temporary curl helper. Nothing from the
  # response body is ever passed to curl except href/JSON URL values.
  raw <- function(url) {
    url <- valid_http_urls(url)
    if (length(url) != 1L) return('')
    tmp <- tempfile(fileext='.txt'); on.exit(unlink(tmp), add=TRUE)
    status <- suppressWarnings(system2('curl', c('-L','--fail','--silent','--show-error','--max-time',as.character(timeout_seconds),
      '--connect-timeout','10','--user-agent','fulltexttest-discovery/1.0','-o',tmp,url), stdout=FALSE, stderr=FALSE))
    if(status != 0L || !file.exists(tmp)) return('')
    paste(readLines(tmp,warn=FALSE),collapse='')
  }

  out <- character()
  q <- utils::URLencode(paste(title,authors),reserved=TRUE)
  oa <- raw(paste0('https://api.openalex.org/works?search=',q,'&per-page=5'))
  if(nzchar(oa)) {
    h <- regmatches(oa,gregexpr('"pdf_url":"[^"]+"',oa,perl=TRUE))[[1]]
    if(length(h)) out <- c(out,sub('^"pdf_url":"','',sub('"$','',h)))
    h <- regmatches(oa,gregexpr('"landing_page_url":"[^"]+"',oa,perl=TRUE))[[1]]
    if(length(h)) out <- c(out,sub('^"landing_page_url":"','',sub('"$','',h)))
  }
  if(nzchar(doi)) {
    oa1 <- raw(paste0('https://api.openalex.org/works/https://doi.org/',utils::URLencode(doi,reserved=TRUE)))
    if(nzchar(oa1)) {
      h <- regmatches(oa1,gregexpr('"pdf_url":"[^"]+"',oa1,perl=TRUE))[[1]]
      if(length(h)) out <- c(out,sub('^"pdf_url":"','',sub('"$','',h)))
    }
    uw <- raw(paste0('https://api.unpaywall.org/v2/',utils::URLencode(doi,reserved=TRUE),'?email=fulltexttest@example.org'))
    if(nzchar(uw)) {
      h <- regmatches(uw,gregexpr('"url_for_pdf":"[^"]+"',uw,perl=TRUE))[[1]]
      if(length(h)) out <- c(out,sub('^"url_for_pdf":"','',sub('"$','',h)))
    }
  }
  ddg <- raw(paste0('https://html.duckduckgo.com/html/?q=',utils::URLencode(paste0('"',title,'" filetype:pdf'),reserved=TRUE)))
  if(nzchar(ddg)) out <- c(out,html_href_urls(ddg))

  out <- valid_http_urls(out)
  unique(out)
}
