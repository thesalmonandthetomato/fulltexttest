`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

http_get <- function(url, accept='*/*', timeout=20) {
  tryCatch({
    r <- httr2::request(url) |>
      httr2::req_headers(Accept=accept) |>
      httr2::req_user_agent('fulltexttest/0.1 (+reproducible scholarly retrieval)') |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries=2) |>
      httr2::req_perform()
    list(ok=httr2::resp_status(r) >= 200 && httr2::resp_status(r) < 300,
         status=httr2::resp_status(r),
         content_type=httr2::resp_headers(r)[['content-type']] %||% '',
         bytes=httr2::resp_body_raw(r), error='')
  }, error=function(e) list(ok=FALSE,status=NA_integer_,content_type='',bytes=raw(),error=conditionMessage(e)))
}

json_get <- function(url) {
  z <- http_get(url, 'application/json')
  if (!z$ok) return(list(ok=FALSE, data=NULL, error=z$error, status=z$status))
  d <- tryCatch(jsonlite::fromJSON(rawToChar(z$bytes), simplifyVector=FALSE), error=function(e) NULL)
  if (is.null(d)) return(list(ok=FALSE, data=NULL, error='invalid_json', status=z$status))
  list(ok=TRUE, data=d, error='', status=z$status)
}

candidate_urls <- function(record) {
  out <- character()
  add <- function(x) if (is.character(x) && length(x) == 1L && nzchar(trimws(x)) && grepl('^https?://', x)) out <<- c(out, x)
  add(as.character(record$url_raw %||% ''))
  doi <- trimws(as.character(record$doi %||% ''))
  title <- trimws(as.character(record$title %||% ''))
  if (nzchar(doi)) {
    add(paste0('https://doi.org/', doi))
    u <- json_get(paste0('https://api.unpaywall.org/v2/', URLencode(doi, reserved=TRUE), '?email=research@example.org'))
    if (u$ok) {
      locs <- u$data$oa_locations %||% list()
      if (is.list(locs)) for (loc in locs) if (is.list(loc)) { add(loc$url_for_pdf %||% ''); add(loc$url %||% '') }
    }
    a <- json_get(paste0('https://api.openalex.org/works/https://doi.org/', URLencode(doi, reserved=TRUE)))
    if (a$ok) {
      locs <- list(a$data$best_oa_location %||% list(), a$data$primary_location %||% list())
      for (loc in locs) if (is.list(loc)) { add(loc$pdf_url %||% ''); add(loc$landing_page_url %||% '') }
    }
    e <- json_get(paste0('https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=DOI:', URLencode(doi, reserved=TRUE), '&format=json&pageSize=5'))
    if (e$ok) {
      rs <- e$data$resultList$result %||% list()
      if (is.list(rs)) for (item in rs) if (is.list(item) && nzchar(as.character(item$pmcid %||% ''))) add(paste0('https://www.ebi.ac.uk/europepmc/webservices/rest/', item$pmcid, '/fullTextXML'))
    }
  } else if (nzchar(title)) {
    q <- json_get(paste0('https://api.crossref.org/works?query.title=', URLencode(title, reserved=TRUE), '&rows=3'))
    if (q$ok) {
      items <- q$data$message$items %||% list()
      if (is.list(items) && length(items)) for (item in items) if (is.list(item) && nzchar(as.character(item$DOI %||% ''))) { add(paste0('https://doi.org/', item$DOI)); break }
    }
  }
  unique(out)
}

pdf_text <- function(bytes) {
  exe <- Sys.which('pdftotext')
  if (!nzchar(exe)) return('')
  f <- tempfile(fileext='.pdf'); on.exit(unlink(f), add=TRUE); writeBin(bytes, f)
  x <- tryCatch(system2(exe, c('-layout', f, '-'), stdout=TRUE, stderr=FALSE), error=function(e) character())
  paste(x, collapse='\n')
}

markup_text <- function(bytes) {
  x <- tryCatch(rawToChar(bytes), error=function(e) '')
  x <- gsub('<script[^>]*>.*?</script>', ' ', x, ignore.case=TRUE, perl=TRUE)
  x <- gsub('<style[^>]*>.*?</style>', ' ', x, ignore.case=TRUE, perl=TRUE)
  x <- gsub('<[^>]+>', ' ', x, perl=TRUE)
  gsub('\\s+', ' ', x)
}

validate_fulltext <- function(bytes, content_type) {
  ct <- tolower(content_type %||% '')
  is_pdf <- grepl('pdf', ct) || (length(bytes) >= 4L && identical(rawToChar(bytes[1:4]), '%PDF'))
  if (is_pdf) {
    txt <- pdf_text(bytes); low <- tolower(txt)
    refs <- grepl('(^|\\n)\\s*(references|bibliography|literature cited)\\s*($|:)', low, perl=TRUE)
    nref <- length(unlist(regmatches(txt, gregexpr('(?m)\\n\\s*(\\[[0-9]{1,4}\\]|[0-9]{1,4}\\.)\\s+', txt, perl=TRUE))))
    return(list(ok=nchar(txt) >= 3000 && refs && nref >= 3, format='pdf', text=txt, references=nref, reason=if (nchar(txt)<3000) 'too_little_text' else if (!refs) 'no_references_section' else if (nref<3) 'too_few_references' else 'complete'))
  }
  if (grepl('html|xml', ct)) {
    txt <- markup_text(bytes); low <- tolower(txt)
    refs <- grepl('references|bibliography|literature cited', low)
    nref <- length(unlist(regmatches(txt, gregexpr('(references|bibliography).*?([0-9]{1,4}\\.|\\[[0-9]{1,4}\\])', low, perl=TRUE))))
    fmt <- if (grepl('xml', ct)) 'xml' else 'html'
    return(list(ok=nchar(txt) >= 5000 && refs && nref >= 3, format=fmt, text=txt, references=nref, reason=if (nchar(txt)<5000) 'too_little_text' else if (!refs) 'no_references_section' else if (nref<3) 'too_few_references' else 'complete'))
  }
  list(ok=FALSE, format='unknown', text='', references=0L, reason='unsupported_content_type')
}
