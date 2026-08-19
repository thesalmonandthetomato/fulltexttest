# v10 discovery overrides: filter infrastructure URLs and keep discovery strictly additive.
.v4_urls <- function(x) {
  x <- as.character(x); x <- x[!is.na(x)]
  out <- unlist(lapply(x, function(z) {
    m <- gregexpr("https?://[^[:space:]<>\\\"']+", z, perl=TRUE)[[1]]
    if (m[1] < 0) return(character())
    regmatches(z, list(m))[[1]]
  }), use.names=FALSE)
  out <- unique(gsub("&amp;", "&", sub("[),.;]+$", "", out), fixed=TRUE))
  out[grepl("^https?://", out, ignore.case=TRUE) &
        !grepl("^(mailto:|javascript:)", out, ignore.case=TRUE) &
        !grepl("(^|//)(google\\.[^/]+|scholar\\.google\\.[^/]+)/", out, ignore.case=TRUE)]
}

# Correct the previously broad early-return semantics: baseline success is valid
# only if it contains an explicit identity result from the strict parser.
additive_run_one <- function(row, out_dir, timeout_seconds=30L) {
  baseline <- baseline_run_one(row, out_dir, timeout_seconds)
  if (isTRUE(as.character(baseline$full_text_status[1]) == "verified_complete") &&
      nzchar(as.character(baseline$identity_method[1] %||% "")) &&
      isTRUE(as.numeric(baseline$identity_score[1]) >= 0.85)) return(baseline)

  ridc <- find_first_column(names(row), c("record_id","id")); rid <- row[[ridc]][1]
  title_raw <- if("title" %in% names(row)) as.character(row[["title"]][1]) else ""
  title <- .v4_title(title_raw)
  doi_c <- find_first_column(names(row), c("doi")); doi <- if(!is.null(doi_c)) normalise(row[[doi_c]][1]) else ""

  # DOI fallback is deliberately after direct DOI/URL retrieval.
  if(nzchar(doi)) {
    pf <- .v4_package_download(doi, rid, out_dir, max(180L, timeout_seconds*4L))
    if(isTRUE(pf$ok) && file.exists(pf$path)) {
      raw <- tryCatch(readBin(pf$path, "raw", n=file.info(pf$path)$size), error=function(e) raw())
      ct <- if(grepl("\\.pdf$",pf$path,ignore.case=TRUE)) "application/pdf" else if(grepl("\\.xml$",pf$path,ignore.case=TRUE)) "application/xml" else "text/html"
      v <- tryCatch(parse_response(raw,ct,pf$path,title,doi),error=function(e) NULL)
      dir.create(file.path(out_dir,"audit"),recursive=TRUE,showWarnings=FALSE)
      if(!is.null(v) && isTRUE(v$ok)) {
        ext <- v$extension %||% ".pdf"
        file.copy(pf$path,file.path(out_dir,"documents",paste0(rid,"_package",ext)),overwrite=TRUE)
        writeLines(v$parsed_text,file.path(out_dir,"parsed",paste0(rid,".txt")),useBytes=TRUE)
        return(data.frame(record_id=rid,full_text_status="verified_complete",format=v$format,
          source_url=paste0("package:fulltext-article-downloader:",doi),text_chars=v$chars,
          reference_markers=v$reference_markers,identity_method=v$identity_method,
          identity_score=v$identity_score,observed_title=v$observed_title,observed_doi=v$observed_doi,
          stringsAsFactors=FALSE))
      }
    }
  }

  phrase8 <- .v4_phrase8(title_raw)
  q <- .v4_title(title_raw)
  enc <- utils::URLencode(q,reserved=TRUE)
  encphrasepdf <- utils::URLencode(paste0('"', phrase8, '" filetype:pdf'),reserved=TRUE)
  jobs <- list(
    .v4_one("google_pdf_search",paste0('"',phrase8,'" filetype:pdf'),paste0("https://www.google.com/search?q=",encphrasepdf,"&num=10"),.v4_google),
    .v4_one("google_scholar_search",q,paste0("https://scholar.google.com/scholar?q=",enc,"&hl=en&num=20"),function(x) .v4_hrefs(x,"https://scholar.google.com")),
    .v4_one("openalex_search",q,paste0("https://api.openalex.org/works?search=",enc,"&per-page=10"),function(x) .v4_api(x,c("pdf_url","landing_page_url","url"))),
    .v4_one("semantic_scholar_search",q,paste0("https://api.semanticscholar.org/graph/v1/paper/search?query=",enc,"&limit=10&fields=title,openAccessPdf,url,externalIds"),function(x) .v4_api(x,c("url"))),
    .v4_one("crossref_search",q,paste0("https://api.crossref.org/works?query.title=",enc,"&rows=5"),function(x) .v4_api(x,c("URL")))
  )
  if(nzchar(doi)) jobs[[length(jobs)+1L]] <- .v4_one("unpaywall_search",doi,paste0("https://api.unpaywall.org/v2/",utils::URLencode(doi,reserved=TRUE),"?email=fulltexttest@example.org"),function(x) .v4_api(x,c("url_for_pdf","url")))
  seeds <- head(.v4_urls(unlist(lapply(unname(row),.v4_urls),use.names=FALSE)),20L)
  for(u in seeds) jobs[[length(jobs)+1L]] <- .v4_one("landing_page","",u,.v4_hrefs,40L)
  candidates <- head(unique(.v4_urls(unlist(lapply(jobs,function(j)j$urls),use.names=FALSE))),50L)
  audit <- do.call(rbind,lapply(jobs,function(j).v4_audit_row(rid,title,j)))
  dir.create(file.path(out_dir,"audit"),recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(audit,file.path(out_dir,"audit",paste0(rid,"_discovery_attempts.csv")),row.names=FALSE,na="")
  if(!length(candidates)) return(baseline)
  augmented <- row; augmented$discovered_urls <- paste(candidates,collapse=" ")
  result <- baseline_run_one(augmented,out_dir,max(timeout_seconds,60L))
  result$discovery_candidates <- length(candidates); result$discovery_title_used <- title
  result
}
