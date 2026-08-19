# Additive discovery v4: Python handles every HTTP transaction.
# Column G is supplied by run.R as row$title and is the only title used.

.v4_urls <- function(x) {
  x <- as.character(x); x <- x[!is.na(x)]
  out <- unlist(lapply(x, function(z) {
    m <- gregexpr("https?://[^[:space:]<>\\\"']+", z, perl = TRUE)[[1]]
    if (m[1] < 0) return(character())
    regmatches(z, list(m))[[1]]
  }), use.names = FALSE)
  unique(gsub("&amp;", "&", sub("[),.;]+$", "", out), fixed = TRUE))
}
.v4_title <- function(x) { x <- as.character(x %||% ""); x[is.na(x)] <- ""; trimws(gsub("\\s+", " ", gsub("[^[:alnum:]]+", " ", x, perl = TRUE))) }
.v4_http <- function(url, timeout=30L) {
  py <- file.path(getwd(),"retrieval","PYTHON","discovery_runner.py")
  inp <- tempfile(); out <- tempfile(); on.exit(unlink(c(inp,out)),add=TRUE)
  writeLines(jsonlite::toJSON(list(url=url,timeout=timeout),auto_unbox=TRUE),inp,useBytes=TRUE)
  started <- Sys.time(); exit <- suppressWarnings(system2("python3",c(py),stdin=inp,stdout=out,stderr=FALSE))
  txt <- if(file.exists(out)) paste(readLines(out,warn=FALSE),collapse="") else ""
  r <- tryCatch(jsonlite::fromJSON(txt),error=function(e) NULL)
  if(exit != 0L || is.null(r)) return(list(ok=FALSE,status=NA_integer_,type="",text="",final_url=url,error=paste0("python_exit_",exit),bytes=0L,elapsed=as.numeric(difftime(Sys.time(),started,units="secs")),candidates=character()))
  r$elapsed <- as.numeric(difftime(Sys.time(),started,units="secs")); r
}
.v4_hrefs <- function(html,base="") {
  if(!nzchar(html)) return(character())
  m <- regmatches(html,gregexpr("(?is)href[[:space:]]*=[[:space:]]*[\\\"']([^\\\"']+)[\\\"']",html,perl=TRUE))[[1]]
  if(!length(m)) return(character())
  x <- sub("(?is)^.*?href[[:space:]]*=[[:space:]]*[\\\"']","",m,perl=TRUE); x <- sub("[\\\"']$","",x); x <- gsub("&amp;","&",x,fixed=TRUE)
  origin <- sub("^((?:https?://[^/]+)).*$","\\1",base,perl=TRUE)
  x <- c(x[grepl("^https?://",x,ignore.case=TRUE)],if(nzchar(origin)) paste0(origin,x[grepl("^/",x)]) else character())
  .v4_urls(x)
}
.v4_google <- function(html) { x <- .v4_hrefs(html,"https://www.google.com"); q <- x[grepl("google\\.[^/]+/(?:url|aclk)\\?",x,ignore.case=TRUE,perl=TRUE)]; if(length(q)){q<-sub("^.*[?&]q=([^&]+).*$","\\1",q,perl=TRUE);x<-c(x,utils::URLdecode(q))}; x <- .v4_urls(x); x[!grepl("google\\.",x,ignore.case=TRUE)] }
.v4_api <- function(text, fields) { out<-character(); for(f in fields){m<-regmatches(text,gregexpr(paste0('"',f,'"[[:space:]]*:[[:space:]]*"(https?:[^"\\r\\n]+)"'),text,perl=TRUE))[[1]]; if(length(m)) out<-c(out,sub(paste0('^"',f,'"[[:space:]]*:[[:space:]]*"'),' ',sub('"$','',m),perl=TRUE))}; .v4_urls(utils::URLdecode(out)) }
.v4_one <- function(stage,q,url,parser=function(x) character(),timeout=30L) { r<-.v4_http(url,timeout); urls<-if(length(r$candidates)) r$candidates else if(isTRUE(r$ok)) parser(r$text) else character(); list(stage=stage,query=q,url=url,r=r,urls=urls) }
.v4_cat <- function(r,n) if(n) "candidate_found" else if(!is.null(r$status)&&!is.na(r$status)&&r$status%in%c(401,403,429)) "access_failure" else if(!is.null(r$status)&&!is.na(r$status)&&r$status%in%c(404,410)) "resolution_failure" else if(nzchar(r$error%||%"")) "transport_failure" else "discovery_failure"
.v4_audit_row <- function(rid,title,j) {
  data.frame(record_id=rid,title_used=title,stage=j$stage,query=j$query,request_url=j$url,
             status=if(is.null(j$r$status)) NA_integer_ else j$r$status,
             content_type=if(is.null(j$r$type)) "" else j$r$type,
             bytes=if(is.null(j$r$bytes)) 0L else j$r$bytes,
             candidate_count=length(j$urls),
             failure_category=.v4_cat(j$r,length(j$urls)),
             reason=if(length(j$urls)) "candidate_found" else if(isTRUE(j$r$ok)) "no_candidates" else paste0("http_",j$r$status),
             error=if(is.null(j$r$error)) "" else j$r$error,
             elapsed_seconds=if(is.null(j$r$elapsed)) NA_real_ else j$r$elapsed,
             stringsAsFactors=FALSE)
}

additive_run_one <- function(row,out_dir,timeout_seconds=30L) {
  baseline <- baseline_run_one(row,out_dir,timeout_seconds)
  sc <- find_first_column(names(baseline),c("full_text_status","status")); if(!is.null(sc)&&identical(as.character(baseline[[sc]][1]),"verified_complete")) return(baseline)
  ridc <- find_first_column(names(row),c("record_id","id")); rid <- row[[ridc]][1]; title <- if("title"%in%names(row)) .v4_title(row[["title"]][1]) else ""; doi_c <- find_first_column(names(row),c("doi")); doi <- if(!is.null(doi_c)) normalise(row[[doi_c]][1]) else ""
  q <- .v4_title(title); enc <- utils::URLencode(q,reserved=TRUE); encpdf <- utils::URLencode(paste(q,"filetype:pdf"),reserved=TRUE)
  jobs <- list(
    .v4_one("google_pdf_search",paste(q,"filetype:pdf"),paste0("https://www.google.com/search?q=",encpdf,"&num=10"),.v4_google),
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
  dir.create(file.path(out_dir,"audit"),recursive=TRUE,showWarnings=FALSE); utils::write.csv(audit,file.path(out_dir,"audit",paste0(rid,"_discovery_attempts.csv")),row.names=FALSE,na="")
  if(!length(candidates)) return(baseline)
  augmented <- row; augmented$discovered_urls <- paste(candidates,collapse=" "); result <- baseline_run_one(augmented,out_dir,max(timeout_seconds,60L)); result$discovery_candidates <- length(candidates); result$discovery_title_used <- title; result
}
