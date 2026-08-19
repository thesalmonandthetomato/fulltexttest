# Additive discovery/audit layer.
# Baseline retrieval remains in fulltext.R and is called first.
# This file owns discovery networking and records every search/retrieval stage.

.normalise_title_words <- function(title) {
  x <- tolower(as.character(title %||% "")); x[is.na(x)] <- ""
  x <- gsub("[^[:alnum:]]+", " ", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x, perl = TRUE))
}

.strict_url <- function(x) {
  x <- trimws(as.character(x)); if (!length(x) || !nzchar(x)) return(NA_character_)
  starts <- gregexpr("https?://", x, ignore.case = TRUE, perl = TRUE)[[1]]
  if (length(starts) && starts[1] > 0L) {
    if (length(starts) > 1L) x <- substring(x, starts[1], starts[2] - 1L)
  } else return(NA_character_)
  x <- gsub("\\\\/", "/", x); x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- sub("[),.;]+$", "", x)
  if (grepl("[[:space:]<>\\\"'`(){}]", x) || !grepl("^https?://[^/[:space:]]+", x, ignore.case = TRUE)) return(NA_character_)
  x
}

.unique_urls <- function(x) {
  if (!length(x)) return(character())
  out <- unlist(lapply(as.character(x), function(z) {
    z <- trimws(z); starts <- gregexpr("https?://", z, ignore.case = TRUE, perl = TRUE)[[1]]
    if (!length(starts) || starts[1] < 0L) return(character())
    ends <- c(starts[-1L] - 1L, nchar(z))
    vapply(seq_along(starts), function(i) .strict_url(substring(z, starts[i], ends[i])), character(1))
  }), use.names = FALSE)
  unique(out[!is.na(out) & nzchar(out)])
}

.discovery_request <- function(url, timeout = 30L, binary = FALSE) {
  url <- .strict_url(url)
  if (is.na(url)) return(list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url="",error="invalid_url",elapsed=0))
  tmp <- tempfile(fileext=if (binary) ".bin" else ".txt"); hdr <- tempfile(fileext=".headers")
  on.exit(unlink(c(tmp,hdr)), add=TRUE)
  # Deliberately do NOT use --fail/--fail-with-body or --retry: HTTP failures are data.
  args <- c("-L","--silent","--show-error","--max-time",as.character(timeout),
            "--connect-timeout","15","--user-agent","Mozilla/5.0 fulltexttest-discovery/3.0",
            "-D",hdr,"-o",tmp,url)
  started <- Sys.time(); exit <- suppressWarnings(system2("curl",args,stdout=FALSE,stderr=FALSE))
  body <- if (file.exists(tmp)) readBin(tmp,"raw",n=file.info(tmp)$size) else raw()
  headers <- if (file.exists(hdr)) readLines(hdr,warn=FALSE) else character()
  hs <- tail(grep("^HTTP/",headers,value=TRUE),1L)
  code <- if (length(hs)) suppressWarnings(as.integer(sub("^HTTP/[0-9.]+\\s+([0-9]+).*$","\\1",hs))) else NA_integer_
  ct <- tail(grep("^Content-Type:",headers,ignore.case=TRUE,value=TRUE),1L)
  ctype <- if (length(ct)) trimws(sub("^Content-Type:\\s*","",ct,ignore.case=TRUE)) else ""
  loc <- tail(grep("^Location:",headers,ignore.case=TRUE,value=TRUE),1L)
  final <- if (length(loc)) .strict_url(sub("^Location:\\s*","",loc,ignore.case=TRUE)) else url
  list(ok=!is.na(code) && code >= 200L && code < 300L,status=code,type=ctype,body=body,
       final_url=final %||% url,error=if (exit != 0L) paste0("curl_exit_",exit) else "",
       elapsed=as.numeric(difftime(Sys.time(),started,units="secs")))
}

.raw_text <- function(body) tryCatch(rawToChar(body),error=function(e) "")

.hrefs <- function(html, base_url="") {
  if (!nzchar(html)) return(character())
  m <- regmatches(html,gregexpr("(?is)href[[:space:]]*=[[:space:]]*[\\\"']([^\\\"']+)[\\\"']",html,perl=TRUE))[[1]]
  if (!length(m)) return(character())
  x <- sub("(?is)^href[[:space:]]*=[[:space:]]*[\\\"']","",m,perl=TRUE); x <- sub("[\\\"']$","",x)
  x <- gsub("&amp;","&",x,fixed=TRUE)
  origin <- if (nzchar(base_url)) sub("^((?:https?://[^/]+)).*$","\\1",base_url,perl=TRUE) else ""
  abs <- x[grepl("^https?://",x,ignore.case=TRUE)]
  if (nzchar(origin)) abs <- c(abs,paste0(origin,x[grepl("^/(?!/)",x,perl=TRUE)]))
  .unique_urls(abs)
}

.google_external_links <- function(html) {
  x <- .hrefs(html,"https://www.google.com")
  # Google commonly wraps results in /url?q=...; unwrap those before filtering.
  wrapped <- x[grepl("google\\.com/(?:url|search|aclk)\\?",x,ignore.case=TRUE,perl=TRUE)]
  if (length(wrapped)) {
    q <- sub("^.*[?&]q=([^&]+).*$","\\1",wrapped,perl=TRUE)
    q <- utils::URLdecode(q); x <- c(x,q)
  }
  x <- .unique_urls(x)
  x[!grepl("(^|//)([^/]+\\.)?google\\.",x,ignore.case=TRUE,perl=TRUE)]
}

.landing_candidates <- function(url,timeout=30L) {
  r <- .discovery_request(url,timeout)
  if (!r$ok || !length(r$body)) return(list(urls=character(),response=r))
  ct <- tolower(r$type)
  if (grepl("pdf",ct) || grepl("xml",ct)) return(list(urls=r$final_url,response=r))
  html <- .raw_text(r$body); links <- .hrefs(html,r$final_url)
  # Also inspect common publisher/repository metadata PDF fields.
  meta <- regmatches(html,gregexpr("(?is)(?:citation_pdf_url|citation_fulltext_html_url|download_url|pdf_url)[^>]+content=[\\\"']([^\\\"']+)[\\\"']",html,perl=TRUE))[[1]]
  if (length(meta)) links <- c(links,sub("(?is)^.*content=[\\\"']([^\\\"']+)[\\\"'].*$","\\1",meta,perl=TRUE))
  score <- grepl("pdf|download|full.?text|bitstream|article|view|file|content",links,ignore.case=TRUE)
  list(urls=.unique_urls(c(links[score],links[!score])),response=r)
}

.search_google <- function(q) {
  u <- paste0("https://www.google.com/search?q=",utils::URLencode(q,reserved=TRUE),"&num=10")
  p <- .discovery_request(u,25L); list(urls=if (p$ok) .google_external_links(.raw_text(p$body)) else character(),response=p,query=q)
}

.search_scholar <- function(q) {
  u <- paste0("https://scholar.google.com/scholar?q=",utils::URLencode(q,reserved=TRUE),"&hl=en&num=20")
  p <- .discovery_request(u,25L)
  links <- if (p$ok) .hrefs(.raw_text(p$body),p$final_url) else character()
  links <- links[!grepl("(^|//)([^/]+\\.)?google\\.",links,ignore.case=TRUE,perl=TRUE)]
  list(urls=.unique_urls(links),response=p,query=q)
}

.json_urls <- function(txt,fields=c("pdf_url","landing_page_url","url_for_pdf","url")) {
  if (!nzchar(txt)) return(character()); out <- character()
  for (field in fields) {
    pat <- paste0('"',field,'"[[:space:]]*:[[:space:]]*"(https?[^"\\r\\n]*)"')
    hits <- regmatches(txt,gregexpr(pat,txt,perl=TRUE))[[1]]
    if (length(hits)) out <- c(out,sub('"$','',sub(paste0('^"',field,'"[[:space:]]*:[[:space:]]*"'),'',hits,perl=TRUE)))
  }
  .unique_urls(out)
}

.openalex_candidates <- function(title) {
  q <- utils::URLencode(.normalise_title_words(title),reserved=TRUE)
  u <- paste0("https://api.openalex.org/works?search=",q,"&per-page=10")
  r <- .discovery_request(u,25L); list(urls=if (r$ok) .json_urls(.raw_text(r$body),c("pdf_url","landing_page_url","url")) else character(),response=r,query=u)
}

.other_api_candidates <- function(title,doi="") {
  out <- list(); q <- utils::URLencode(.normalise_title_words(title),reserved=TRUE)
  su <- paste0("https://api.semanticscholar.org/graph/v1/paper/search?query=",q,"&limit=10&fields=title,openAccessPdf,url,externalIds")
  sr <- .discovery_request(su,25L); out$semantic_scholar <- list(urls=if(sr$ok) .json_urls(.raw_text(sr$body),c("url")) else character(),response=sr,query=su)
  if (nzchar(doi)) {
    uu <- paste0("https://api.unpaywall.org/v2/",utils::URLencode(doi,reserved=TRUE),"?email=fulltexttest@example.org")
    ur <- .discovery_request(uu,25L); out$unpaywall <- list(urls=if(ur$ok) .json_urls(.raw_text(ur$body),c("url_for_pdf","url")) else character(),response=ur,query=uu)
  }
  cu <- paste0("https://api.crossref.org/works?query.title=",utils::URLencode(title,reserved=TRUE),"&rows=5")
  cr <- .discovery_request(cu,25L); out$crossref <- list(urls=if(cr$ok) .json_urls(.raw_text(cr$body),c("URL")) else character(),response=cr,query=cu)
  out
}

.identity_score <- function(text,title) {
  terms <- unique(unlist(strsplit(.normalise_title_words(title)," ",fixed=TRUE))); terms <- terms[nchar(terms)>=3L]
  if (!length(terms)) return(0)
  z <- tolower(gsub("[^[:alnum:]]+"," ",text,perl=TRUE)); toks <- strsplit(z," ",fixed=TRUE)[[1]]
  mean(terms %in% toks)
}

.classify_discovery_failure <- function(r,reason="") {
  if (grepl("identity",reason,ignore.case=TRUE)) return("identity_failure")
  if (!is.na(r$status) && r$status %in% c(401L,403L,429L)) return("access_failure")
  if (!is.na(r$status) && r$status %in% c(404L,410L)) return("resolution_failure")
  if (grepl("insufficient|reference|extraction|pdftotext",reason,ignore.case=TRUE)) return("extraction_or_validation_failure")
  if (grepl("html|json|unsupported",reason,ignore.case=TRUE)) return("format_failure")
  "discovery_failure"
}

additive_run_one <- function(row,out_dir,timeout_seconds=30L) {
  baseline <- baseline_run_one(row,out_dir,timeout_seconds)
  status_col <- find_first_column(names(baseline),c("full_text_status","status"))
  if (!is.null(status_col) && identical(as.character(baseline[[status_col]][1]),"verified_complete")) return(baseline)
  rid_col <- find_first_column(names(row),c("record_id","id")); rid <- row[[rid_col]]
  title_col <- find_first_column(names(row),c("title","short_title","title_normalised")); title <- if(!is.null(title_col)) normalise(row[[title_col]]) else ""
  doi_col <- find_first_column(names(row),c("doi")); doi <- if(!is.null(doi_col)) normalise(row[[doi_col]]) else ""
  seed <- .unique_urls(unlist(lapply(unname(row),.unique_urls)))
  if (nzchar(doi)) seed <- unique(c(seed,paste0("https://doi.org/",doi)))
  stages <- list(); add <- function(name,urls) if(length(urls)) stages[[name]] <<- unique(c(stages[[name]],urls))
  add("existing_url",seed)
  landing <- lapply(seed,.landing_candidates,timeout=min(timeout_seconds,40L))
  add("landing_page",unique(unlist(lapply(landing,`[[`,"urls"))))
  title_words <- .normalise_title_words(title)
  g <- .search_google(paste(title_words,"filetype:pdf")); add("google_pdf",g$urls)
  s <- .search_scholar(title_words); add("google_scholar",s$urls)
  oa <- .openalex_candidates(title); add("openalex",oa$urls)
  other <- .other_api_candidates(title,doi)
  for(n in names(other)) add(n,other[[n]]$urls)

  rows <- list(); add_audit <- function(stage,url="",r=NULL,category="",reason="",score=NA_real_) {
    rows[[length(rows)+1L]] <<- data.frame(record_id=rid,stage=stage,candidate_url=url,final_url=if(is.null(r)) "" else r$final_url,
      status=if(is.null(r)) NA_integer_ else r$status,content_type=if(is.null(r)) "" else r$type,
      bytes=if(is.null(r)) 0L else length(r$body),failure_category=category,reason=reason,identity_score=score,stringsAsFactors=FALSE)
  }
  # Record search-service outcomes even when they return zero candidates.
  add_audit("google_pdf_search",g$query,g$response,if(!g$response$ok) .classify_discovery_failure(g$response,"search") else if(!length(g$urls)) "discovery_failure" else "candidate_found",if(!g$response$ok) paste0("http_",g$response$status) else paste0("candidates=",length(g$urls)))
  add_audit("google_scholar_search",s$query,s$response,if(!s$response$ok) .classify_discovery_failure(s$response,"search") else if(!length(s$urls)) "discovery_failure" else "candidate_found",if(!s$response$ok) paste0("http_",s$response$status) else paste0("candidates=",length(s$urls)))
  add_audit("openalex_search",oa$query,oa$response,if(!oa$response$ok) .classify_discovery_failure(oa$response,"search") else if(!length(oa$urls)) "discovery_failure" else "candidate_found",if(!oa$response$ok) paste0("http_",oa$response$status) else paste0("candidates=",length(oa$urls)))
  for(n in names(other)) add_audit(paste0(n,"_search"),other[[n]]$query,other[[n]]$response,if(!other[[n]]$response$ok) .classify_discovery_failure(other[[n]]$response,"search") else if(!length(other[[n]]$urls)) "discovery_failure" else "candidate_found",if(!other[[n]]$response$ok) paste0("http_",other[[n]]$response$status) else paste0("candidates=",length(other[[n]]$urls)))

  all_items <- unique(unlist(lapply(names(stages),function(stage) paste(stage,stages[[stage]],sep="||")),use.names=FALSE)); winner <- NULL
  for(item in all_items) {
    p <- strsplit(item,"||",fixed=TRUE)[[1]]; stage <- p[[1]]; u <- p[[2]]
    r <- .discovery_request(u,max(timeout_seconds,60L),binary=TRUE); v <- NULL
    reason <- if(!r$ok) paste0("http_",r$status %||% "NA") else ""
    score <- NA_real_
    if(r$ok) {
      v <- tryCatch(parse_response(r$body,r$type,r$final_url),error=function(e) list(ok=FALSE,chars=0L,reference_markers=0L,reason=paste0("extraction: ",conditionMessage(e)),format="unknown",extension=".bin",parsed_text=""))
      score <- if(!is.null(v$parsed_text)) .identity_score(v$parsed_text,title) else 0
      if(isTRUE(v$ok) && score < 0.15) {v$ok <- FALSE; v$reason <- "identity_failure"}
      reason <- v$reason
    }
    cat <- if(isTRUE(v$ok)) "success" else .classify_discovery_failure(r,reason)
    add_audit(stage,u,r,cat,reason,score)
    if(isTRUE(v$ok)) {winner <- list(response=r,validation=v);break}
  }
  if(!length(rows)) add_audit("discovery","",NULL,"discovery_failure","no_candidate_urls")
  dir.create(file.path(out_dir,"audit"),recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(do.call(rbind,rows),file.path(out_dir,"audit",paste0(rid,"_discovery_attempts.csv")),row.names=FALSE,na="")
  if(is.null(winner)) return(baseline)
  dir.create(file.path(out_dir,"documents"),recursive=TRUE,showWarnings=FALSE); dir.create(file.path(out_dir,"parsed"),recursive=TRUE,showWarnings=FALSE)
  writeBin(winner$response$body,file.path(out_dir,"documents",paste0(rid,winner$validation$extension)))
  writeLines(winner$validation$parsed_text,file.path(out_dir,"parsed",paste0(rid,".txt")),useBytes=TRUE)
  data.frame(record_id=rid,full_text_status="verified_complete",format=winner$validation$format,source_url=winner$response$final_url,text_chars=winner$validation$chars,reference_markers=winner$validation$reference_markers,stringsAsFactors=FALSE)
}
