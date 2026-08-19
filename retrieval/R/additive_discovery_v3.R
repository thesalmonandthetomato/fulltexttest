# Additive discovery layer v3.
# Baseline retrieval remains authoritative and is run first.
# Discovery HTTP is delegated to Python requests to avoid R/system2 shell
# interpretation of query-string ampersands.

.disc_normalise_title <- function(x) {
  x <- as.character(x %||% ""); x[is.na(x)] <- ""
  x <- gsub("[^[:alnum:]]+", " ", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x, perl = TRUE))
}

.disc_urls <- function(x) {
  if (!length(x)) return(character())
  out <- unlist(lapply(as.character(x), function(z) {
    z <- trimws(z); if (!nzchar(z)) return(character())
    starts <- gregexpr("https?://", z, ignore.case=TRUE, perl=TRUE)[[1]]
    if (!length(starts) || starts[1] < 0L) return(character())
    ends <- c(starts[-1L]-1L, nchar(z))
    vapply(seq_along(starts), function(i) {
      u <- substring(z, starts[i], ends[i]); u <- gsub("&amp;", "&", u, fixed=TRUE); sub("[),.;]+$", "", u)
    }, character(1))
  }), use.names=FALSE)
  unique(out[nzchar(out) & grepl("^https?://[^[:space:]<>]+$", out, ignore.case=TRUE)])
}

.disc_request <- function(url, timeout=30L) {
  py <- file.path(getwd(), "retrieval", "PYTHON", "discovery_client.py")
  if (!file.exists(py)) return(list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url=url,error="python_client_missing",elapsed=0,bytes=0))
  inp <- jsonlite::toJSON(list(url=url, timeout=timeout), auto_unbox=TRUE)
  tmpin <- tempfile(); tmpout <- tempfile(); on.exit(unlink(c(tmpin,tmpout)), add=TRUE)
  writeLines(inp, tmpin, useBytes=TRUE)
  started <- Sys.time()
  exit <- suppressWarnings(system2("python3", c(py), stdin=tmpin, stdout=tmpout, stderr=FALSE))
  txt <- if (file.exists(tmpout)) paste(readLines(tmpout,warn=FALSE),collapse="") else ""
  if (exit != 0L || !nzchar(txt)) return(list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url=url,error=paste0("python_exit_",exit),elapsed=as.numeric(difftime(Sys.time(),started,units="secs")),bytes=0))
  r <- tryCatch(jsonlite::fromJSON(txt), error=function(e) NULL)
  if (is.null(r)) return(list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url=url,error="invalid_python_response",elapsed=as.numeric(difftime(Sys.time(),started,units="secs")),bytes=0))
  body <- if (nzchar(r$body_b64 %||% "")) { h <- r$body_b64; if (requireNamespace("openssl",quietly=TRUE)) openssl::hex2bin(h) else raw() } else raw()
  list(ok=isTRUE(r$ok),status=if(is.null(r$status)) NA_integer_ else as.integer(r$status),type=r$type %||% "",body=body,final_url=r$final_url %||% url,error=r$error %||% "",elapsed=as.numeric(difftime(Sys.time(),started,units="secs")),bytes=as.integer(r$bytes %||% 0L))
}

.disc_text <- function(body) tryCatch(rawToChar(body), error=function(e) "")
.disc_hrefs <- function(html,base="") { if(!nzchar(html)) return(character()); m<-regmatches(html,gregexpr("(?is)href[[:space:]]*=[[:space:]]*[\\\"']([^\\\"']+)[\\\"']",html,perl=TRUE))[[1]]; if(!length(m)) return(character()); x<-sub("(?is)^href[[:space:]]*=[[:space:]]*[\\\"']","",m,perl=TRUE); x<-sub("[\\\"']$","",x); x<-gsub("&amp;","&",x,fixed=TRUE); origin<-if(nzchar(base)) sub("^((?:https?://[^/]+)).*$","\\1",base,perl=TRUE) else ""; abs<-x[grepl("^https?://",x,ignore.case=TRUE)]; if(nzchar(origin)) abs<-c(abs,paste0(origin,x[grepl("^/[^/]",x,perl=TRUE)])); .disc_urls(abs) }
.disc_google_links <- function(html) { x<-.disc_hrefs(html,"https://www.google.com"); wrapped<-x[grepl("google\\.[^/]+/(?:url|aclk)\\?",x,ignore.case=TRUE,perl=TRUE)]; if(length(wrapped)){q<-sub("^.*[?&]q=([^&]+).*$","\\1",wrapped,perl=TRUE); x<-c(x,utils::URLdecode(q))}; x<-.disc_urls(x); x[!grepl("(^|//)([^/]+\\.)?google\\.",x,ignore.case=TRUE,perl=TRUE)] }
.disc_api_urls <- function(txt,fields) { if(!nzchar(txt)) return(character()); out<-character(); for(field in fields){pat<-paste0('"',field,'"[[:space:]]*:[[:space:]]*"(https?:[^"\\r\\n]+)"'); hits<-regmatches(txt,gregexpr(pat,txt,perl=TRUE))[[1]]; if(length(hits)) out<-c(out,sub(paste0('^"',field,'"[[:space:]]*:[[:space:]]*"'),' ',sub('"$','',hits),perl=TRUE))}; .disc_urls(utils::URLdecode(out)) }

.disc_search_google <- function(title) { q<-paste(.disc_normalise_title(title),"filetype:pdf"); u<-paste0("https://www.google.com/search?q=",utils::URLencode(q,reserved=TRUE),"&num=10"); r<-.disc_request(u); list(stage="google_pdf_search",query=q,request_url=u,response=r,urls=if(r$ok) .disc_google_links(.disc_text(r$body)) else character()) }
.disc_search_scholar <- function(title) { q<-.disc_normalise_title(title); u<-paste0("https://scholar.google.com/scholar?q=",utils::URLencode(q,reserved=TRUE),"&hl=en&num=20"); r<-.disc_request(u); links<-if(r$ok) .disc_hrefs(.disc_text(r$body),r$final_url) else character(); links<-links[!grepl("(^|//)([^/]+\\.)?google\\.",links,ignore.case=TRUE,perl=TRUE)]; list(stage="google_scholar_search",query=q,request_url=u,response=r,urls=.disc_urls(links)) }
.disc_search_openalex <- function(title) { q<-.disc_normalise_title(title); u<-paste0("https://api.openalex.org/works?search=",utils::URLencode(q,reserved=TRUE),"&per-page=10"); r<-.disc_request(u); list(stage="openalex_search",query=q,request_url=u,response=r,urls=if(r$ok) .disc_api_urls(.disc_text(r$body),c("pdf_url","landing_page_url","url")) else character()) }
.disc_search_semantic <- function(title) { q<-.disc_normalise_title(title); u<-paste0("https://api.semanticscholar.org/graph/v1/paper/search?query=",utils::URLencode(q,reserved=TRUE),"&limit=10&fields=title,openAccessPdf,url,externalIds"); r<-.disc_request(u); list(stage="semantic_scholar_search",query=q,request_url=u,response=r,urls=if(r$ok) .disc_api_urls(.disc_text(r$body),c("url","openAccessPdf")) else character()) }
.disc_search_crossref <- function(title) { q<-.disc_normalise_title(title); u<-paste0("https://api.crossref.org/works?query.title=",utils::URLencode(q,reserved=TRUE),"&rows=5"); r<-.disc_request(u); list(stage="crossref_search",query=q,request_url=u,response=r,urls=if(r$ok) .disc_api_urls(.disc_text(r$body),c("URL")) else character()) }
.disc_search_unpaywall <- function(doi) { if(!nzchar(doi)) return(list(stage="unpaywall_search",query="",request_url="",response=list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url="",error="no_doi",elapsed=0,bytes=0),urls=character())); u<-paste0("https://api.unpaywall.org/v2/",utils::URLencode(doi,reserved=TRUE),"?email=fulltexttest@example.org"); r<-.disc_request(u); list(stage="unpaywall_search",query=doi,request_url=u,response=r,urls=if(r$ok) .disc_api_urls(.disc_text(r$body),c("url_for_pdf","url")) else character()) }
.disc_expand_landing <- function(url) { r<-.disc_request(url,40L); if(!r$ok||!length(r$body)) return(list(stage="landing_page",urls=character(),response=r)); if(grepl("pdf",tolower(r$type))||(length(r$body)>=4&&identical(rawToChar(r$body[1:4]),"%PDF"))) return(list(stage="landing_page",urls=r$final_url,response=r)); html<-.disc_text(r$body); links<-.disc_hrefs(html,r$final_url); .disc_urls(links); list(stage="landing_page",urls=links,response=r) }
.disc_category <- function(r,reason="") { if(identical(reason,"candidate_found")) return("candidate_found"); if(!is.na(r$status)&&r$status%in%c(401L,403L,429L)) return("access_failure"); if(!is.na(r$status)&&r$status%in%c(404L,410L)) return("resolution_failure"); if(grepl("python_exit|timeout|invalid_url",r$error%||%"",ignore.case=TRUE)) return("transport_failure"); if(grepl("html|json",tolower(r$type%||%""))) return("format_failure"); "discovery_failure" }

additive_run_one <- function(row,out_dir,timeout_seconds=30L) {
  baseline<-baseline_run_one(row,out_dir,timeout_seconds); status_col<-find_first_column(names(baseline),c("full_text_status","status")); if(!is.null(status_col)&&identical(as.character(baseline[[status_col]][1]),"verified_complete")) return(baseline)
  rid_col<-find_first_column(names(row),c("record_id","id")); rid<-row[[rid_col]]; title<-if("title"%in%names(row)) normalise(row[["title"]]) else ""; doi_col<-find_first_column(names(row),c("doi")); doi<-if(!is.null(doi_col)) normalise(row[[doi_col]]) else ""; seed<-.disc_urls(unlist(lapply(unname(row),.disc_urls),use.names=FALSE))
  searches<-list(.disc_search_google(title),.disc_search_scholar(title),.disc_search_openalex(title),.disc_search_semantic(title),.disc_search_crossref(title),.disc_search_unpaywall(doi)); landing_seeds<-head(seed,20L); landing<-if(length(landing_seeds)) lapply(landing_seeds,.disc_expand_landing) else list(); landing_urls<-unique(unlist(lapply(landing,`[[`,"urls"),use.names=FALSE)); discovered<-head(.disc_urls(c(unlist(lapply(searches,`[[`,"urls"),use.names=FALSE),landing_urls)),40L)
  dir.create(file.path(out_dir,"audit"),recursive=TRUE,showWarnings=FALSE); audit<-list(); add_audit<-function(stage,query,url,r,n,reason){audit[[length(audit)+1L]]<<-data.frame(record_id=rid,stage=stage,query=query,request_url=url,candidate_count=n,status=r$status,content_type=r$type,bytes=length(r$body),failure_category=.disc_category(r,reason),reason=reason,error=r$error,elapsed_seconds=r$elapsed,stringsAsFactors=FALSE)}
  for(s in searches) add_audit(s$stage,s$query,s$request_url,s$response,length(s$urls),if(length(s$urls))"candidate_found"else if(s$response$ok)"no_candidates"else paste0("http_",s$response$status)); for(l in landing) add_audit("landing_page","","",l$response,length(l$urls),if(length(l$urls))"candidate_found"else if(l$response$ok)"no_candidates"else paste0("http_",l$response$status)); audit_df<-do.call(rbind,audit); audit_df$title_used<-title; utils::write.csv(audit_df,file.path(out_dir,"audit",paste0(rid,"_discovery_attempts.csv")),row.names=FALSE,na="")
  if(!length(discovered)) return(baseline); augmented<-row; augmented$discovered_urls<-paste(discovered,collapse=" "); result<-baseline_run_one(augmented,out_dir,max(timeout_seconds,60L)); result$discovery_candidates<-length(discovered); result$discovery_title_used<-title; result
}
