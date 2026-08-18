safe_get <- function(url, timeout=15, accept='*/*') {
  tryCatch({
    r <- httr2::request(url) |>
      httr2::req_headers(Accept=accept) |>
      httr2::req_user_agent('fulltexttest/0.1 reproducible-research') |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform()
    list(ok=httr2::resp_status(r)>=200 && httr2::resp_status(r)<300,
         status=httr2::resp_status(r),
         content_type=httr2::resp_headers(r)[['content-type']] %||% '',
         bytes=httr2::resp_body_raw(r), error='')
  }, error=function(e) list(ok=FALSE,status=NA,content_type='',bytes=raw(),error=conditionMessage(e)))
}

`%||%` <- function(x,y) if(is.null(x) || length(x)==0 || all(is.na(x))) y else x

candidate_urls <- function(doi, title) {
  urls <- character()
  if (nzchar(doi)) {
    u <- safe_get(paste0('https://api.unpaywall.org/v2/', URLencode(doi, reserved=TRUE), '?email=research@example.org'), accept='application/json')
    if (u$ok) {
      j <- tryCatch(jsonlite::fromJSON(rawToChar(u$bytes), simplifyVector=FALSE), error=function(e) NULL)
      if (!is.null(j)) {
        locs <- j$oa_locations %||% list()
        for (x in locs) {
          if (is.list(x)) for (nm in c('url_for_pdf','url')) if (!is.null(x[[nm]]) && is.character(x[[nm]])) urls <- c(urls, x[[nm]])
        }
      }
    }
    oa <- safe_get(paste0('https://api.openalex.org/works/https://doi.org/', URLencode(doi,reserved=TRUE)), accept='application/json')
    if (oa$ok) {
      j <- tryCatch(jsonlite::fromJSON(rawToChar(oa$bytes), simplifyVector=FALSE), error=function(e) NULL)
      for (nm in c('best_oa_location','open_access')) {
        x <- j[[nm]] %||% list()
        if (is.list(x)) for (k in c('pdf_url','landing_page_url','url')) if (!is.null(x[[k]]) && is.character(x[[k]])) urls <- c(urls,x[[k]])
      }
    }
    urls <- c(urls, paste0('https://doi.org/', doi))
  }
  unique(urls[nzchar(urls)])
}
