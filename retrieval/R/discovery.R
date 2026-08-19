# Broad fallback discovery layer.
# This file is sourced after fulltext.R so run_one() uses this implementation.
# Discovery is deliberately best-effort: search engines and public APIs are
# treated as candidate generators; the existing downloader + validation remains
# the authority on whether a document is usable.

.discovery_curl <- function(url, timeout_seconds = 20L, tries = 3L) {
  for (i in seq_len(tries)) {
    tmp <- tempfile(fileext = ".txt")
    hdr <- tempfile(fileext = ".headers")
    on.exit(unlink(c(tmp, hdr)), add = TRUE)
    status <- suppressWarnings(system2("curl", c("-L", "--silent", "--show-error",
      "--max-time", as.character(timeout_seconds), "--connect-timeout", "10",
      "--retry", "2", "--retry-delay", as.character(min(2^i, 8)),
      "--user-agent", "Mozilla/5.0 (compatible; fulltexttest/1.0; +https://github.com/thesalmonandthetomato/fulltexttest)",
      "-D", hdr, "-o", tmp, url), stdout = FALSE, stderr = FALSE))
    if (status == 0L && file.exists(tmp) && file.info(tmp)$size > 0) {
      return(paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
    }
    Sys.sleep(min(2^i, 8))
  }
  ""
}

.discovery_urls_from_html <- function(html) {
  if (!nzchar(html)) return(character())
  # DuckDuckGo exposes result links as href attributes. Google/Bing results
  # often wrap destinations in redirect URLs; the regex below extracts both
  # direct http(s) links and common /url?q= or uddg= destinations.
  hits <- unlist(regmatches(html, gregexpr("https?://[^\\\"'<>[:space:]]+", html, perl = TRUE)), use.names = FALSE)
  if (!length(hits)) return(character())
  hits <- sub("[&]amp;", "&", hits, fixed = TRUE)
  hits <- gsub("\\\\u0026", "&", hits, fixed = TRUE)
  hits <- sub("[)\\],;]+$", "", hits)
  hits <- hits[!grepl("duckduckgo\\.com|google\\.com/search|google\\.co\\.[^/]+/search|bing\\.com/search", hits, ignore.case = TRUE)]
  unique(hits[nzchar(hits)])
}

.discovery_search_engine <- function(query, timeout_seconds = 20L) {
  q <- utils::URLencode(query, reserved = TRUE)
  urls <- character()
  # DuckDuckGo's HTML endpoint is substantially less brittle than scraping
  # Google Scholar directly and commonly surfaces Scholar-indexed author and
  # repository copies (including ResearchGate and institutional repositories).
  ddg <- .discovery_curl(paste0("https://html.duckduckgo.com/html/?q=", q), timeout_seconds)
  urls <- c(urls, .discovery_urls_from_html(ddg))

  # Use Google web search as a second independent discovery route. We do not
  # scrape Google Scholar itself because CAPTCHA/rate limiting makes it an
  # unreliable CI dependency; the normal Google index frequently contains the
  # same repository/PDF copies linked from Scholar.
  google <- .discovery_curl(paste0("https://www.google.com/search?num=10&q=", q), timeout_seconds)
  urls <- c(urls, .discovery_urls_from_html(google))

  # Jina Reader is a final search-page fallback when the CI runner cannot reach
  # either search engine directly. It is used only to discover URLs, never to
  # supply article text.
  if (length(urls) < 3L) {
    jina <- .discovery_curl(paste0("https://r.jina.ai/http://www.google.com/search?num=10&q=", q), timeout_seconds)
    urls <- c(urls, .discovery_urls_from_html(jina))
  }
  unique(urls[nzchar(urls)])
}

.discovery_api_urls <- function(title, authors = "", doi = "", timeout_seconds = 20L) {
  out <- character()
  q <- utils::URLencode(title, reserved = TRUE)
  # OpenAlex: request a small, title-focused result set and ask politely with
  # contact information so the service can identify the client.
  oa <- .discovery_curl(paste0("https://api.openalex.org/works?search=", q,
                               "&per-page=5&mailto=fulltexttest@example.org"), timeout_seconds)
  if (nzchar(oa)) {
    for (pat in c('"pdf_url":"[^"]+"', '"landing_page_url":"[^"]+"')) {
      h <- regmatches(oa, gregexpr(pat, oa, perl = TRUE))[[1]]
      if (length(h)) out <- c(out, sub('^"[^:]+":"', '', sub('"$', '', h)))
    }
  }

  # Crossref is used only as metadata discovery. Its URL is a useful route to
  # publisher pages that may expose a PDF even when the master URL has expired.
  cr <- .discovery_curl(paste0("https://api.crossref.org/works?query.title=", q,
                               "&rows=5"), timeout_seconds)
  if (nzchar(cr)) {
    h <- regmatches(cr, gregexpr('"URL":"https?[^" ]+"', cr, perl = TRUE))[[1]]
    if (length(h)) out <- c(out, sub('^"URL":"', '', sub('"$', '', h)))
  }

  # Unpaywall is specifically designed to locate legal open-access copies.
  # It does not require an API key; an email is required by its API policy.
  if (nzchar(doi)) {
    u <- paste0("https://api.unpaywall.org/v2/", utils::URLencode(doi, reserved = TRUE),
                "?email=fulltexttest@example.org")
    uw <- .discovery_curl(u, timeout_seconds)
    if (nzchar(uw)) {
      h <- regmatches(uw, gregexpr('"url_for_pdf":"https?[^" ]+"', uw, perl = TRUE))[[1]]
      if (length(h)) out <- c(out, sub('^"url_for_pdf":"', '', sub('"$', '', h)))
      h <- regmatches(uw, gregexpr('"url":"https?[^" ]+"', uw, perl = TRUE))[[1]]
      if (length(h)) out <- c(out, sub('^"url":"', '', sub('"$', '', h)))
    }
  }

  unique(out[nzchar(out)])
}

discover_search_urls <- function(row, timeout_seconds = 20L) {
  title_col <- find_first_column(names(row), c("title", "short_title", "title_normalised"))
  author_col <- find_first_column(names(row), c("authors", "author", "first_author"))
  doi_col <- find_first_column(names(row), c("doi"))
  title <- if (!is.null(title_col)) normalise(row[[title_col]]) else ""
  authors <- if (!is.null(author_col)) normalise(row[[author_col]]) else ""
  doi <- if (!is.null(doi_col)) normalise(row[[doi_col]]) else ""
  if (!nzchar(title)) return(character())

  queries <- c(
    paste0('"', title, '"'),
    paste0('"', title, '" filetype:pdf'),
    paste0('"', title, '" ', if (nzchar(authors)) authors else "", " full text")
  )

  out <- character()
  for (q in queries) {
    out <- c(out, .discovery_search_engine(q, timeout_seconds))
    Sys.sleep(1.5)
  }
  out <- c(out, .discovery_api_urls(title, authors, doi, timeout_seconds))

  # Add common repository/search landing patterns when a DOI is available.
  if (nzchar(doi)) {
    out <- c(out,
      paste0("https://api.openalex.org/works/https://doi.org/", utils::URLencode(doi, reserved = TRUE)),
      paste0("https://doi.org/", doi))
  }
  unique(out[nzchar(out)])
}
