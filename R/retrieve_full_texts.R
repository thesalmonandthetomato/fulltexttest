#!/usr/bin/env Rscript
# Auditable full-text retrieval. Metadata/abstract/API JSON is never counted as full text.
args <- commandArgs(trailingOnly=TRUE)
start <- if(length(args)>=1) as.integer(args[[1]]) else 1L
size  <- if(length(args)>=2) as.integer(args[[2]]) else 10L
suppressPackageStartupMessages({ library(jsonlite); library(httr2); library(readr) })
idx <- read_csv('data/pilot_index_100.csv', show_col_types=FALSE, na='')
stopifnot(nrow(idx)==100, start>=1L, size>=1L, start+size-1L<=nrow(idx))
b <- as.data.frame(idx[start:(start+size-1L),,drop=FALSE], stringsAsFactors=FALSE)
if (file.exists('data/master_100_extractions.csv')) {
  m <- tryCatch(read_csv('data/master_100_extractions.csv', show_col_types=FALSE, na=''), error=function(e) NULL)
  if (!is.null(m) && 'record_number' %in% names(m)) {
    mi <- match(as.character(b$record_number), as.character(m$record_number))
    for (nm in c('doi','full_text_url','retrieval_status','full_text_verified')) if (nm %in% names(m)) {
      if (!nm %in% names(b)) b[[nm]] <- ''
      v <- m[[nm]][mi]; fill <- !is.na(v) & nzchar(trimws(as.character(v))); b[[nm]][fill] <- as.character(v[fill])
    }
  }
}
dir.create('outputs/retrieval', recursive=TRUE, showWarnings=FALSE); dir.create('outputs/full_text', recursive=TRUE, showWarnings=FALSE)
`%||%` <- function(x,y) if(is.null(x)||length(x)==0||is.na(x)) y else x
safe_get <- function(url, accept='*/*') tryCatch({
  resp <- request(url) |> req_headers(Accept=accept) |> req_user_agent('fulltexttest/1.0 reproducible research retrieval') |> req_timeout(30) |> req_retry(max_tries=2) |> req_perform()
  list(ok=resp_status(resp)>=200&&resp_status(resp)<300,status=resp_status(resp),content_type=resp_headers(resp)[['content-type']]%||%'',bytes=resp_body_raw(resp),error='')
},error=function(e) list(ok=FALSE,status=NA,content_type='',bytes=raw(),error=conditionMessage(e)))
is_pdf <- function(ct,bytes) grepl('pdf',tolower(ct)) || (length(bytes)>=4&&identical(rawToChar(bytes[1:4]),'%PDF'))
is_doc <- function(ct,bytes) is_pdf(ct,bytes)||grepl('html|xml|text',tolower(ct))
attempt_row <- function(id,route,url,res,candidate='',note='') data.frame(record_number=id,route=route,url=url,status=res$status,content_type=res$content_type,usable_full_text=isTRUE(res$ok&&is_doc(res$content_type,res$bytes)),candidate_url=candidate,note=note,error=res$error,stringsAsFactors=FALSE)
url_from_openalex <- function(j){loc<-j$locations;if(is.null(loc))return(character());out<-character();for(x in loc){if(!is.null(x$pdf_url)&&nzchar(x$pdf_url))out<-c(out,x$pdf_url);if(!is.null(x$landing_page_url)&&nzchar(x$landing_page_url))out<-c(out,x$landing_page_url)};unique(out)}
url_from_unpaywall <- function(j){loc<-j$best_oa_location;if(is.null(loc))return(character());unique(c(loc$url_for_pdf,loc$url))}
all_attempts<-list();statuses<-list()
for(i in seq_len(nrow(b))){r<-b[i,];id<-as.character(r$record_number);title<-as.character(r$title);doi<-if('doi'%in%names(r))trimws(as.character(r$doi))else'';if(is.na(doi))doi<-'';attempts<-list();saved<-FALSE;verified_url<-''
log_attempt<-function(route,url,res,candidate='',note=''){attempts[[length(attempts)+1L]]<<-attempt_row(id,route,url,res,candidate,note)}
if('full_text_url'%in%names(r)&&nzchar(trimws(as.character(r$full_text_url)))&&!is.na(r$full_text_url)){u<-as.character(r$full_text_url);res<-safe_get(u);log_attempt('existing_canonical_url',u,res,u);if(isTRUE(res$ok&&is_doc(res$content_type,res$bytes))){verified_url<-u;saved<-TRUE}}
if(!nzchar(doi)){u<-paste0('https://api.crossref.org/works?query.title=',URLencode(title,reserved=TRUE),'&rows=3');res<-safe_get(u,'application/json');log_attempt('crossref_title_discovery',u,res);if(res$ok){j<-tryCatch(fromJSON(rawToChar(res$bytes)),error=function(e)NULL);items<-tryCatch(j$message$items,error=function(e)NULL);if(!is.null(items)&&nrow(items)>0)doi<-as.character(items$DOI[1]%||%'')}}
if(nzchar(doi)&&!saved){u<-paste0('https://doi.org/',doi);res<-safe_get(u);log_attempt('doi_publisher',u,res);if(isTRUE(res$ok&&is_doc(res$content_type,res$bytes))){verified_url<-u;saved<-TRUE}}
if(nzchar(doi)&&!saved){u<-paste0('https://api.unpaywall.org/v2/',URLencode(doi,reserved=TRUE),'?email=research@example.org');res<-safe_get(u,'application/json');log_attempt('unpaywall_discovery',u,res);if(res$ok){j<-tryCatch(fromJSON(rawToChar(res$bytes)),error=function(e)NULL);for(cu in url_from_unpaywall(j))if(nzchar(cu)&&!saved){rr<-safe_get(cu);log_attempt('unpaywall_full_text',cu,rr,cu);if(isTRUE(rr$ok&&is_doc(rr$content_type,rr$bytes))){verified_url<-cu;saved<-TRUE}}}}
if(nzchar(doi)&&!saved){u<-paste0('https://api.openalex.org/works/https://doi.org/',URLencode(doi,reserved=TRUE));res<-safe_get(u,'application/json');log_attempt('openalex_discovery',u,res);if(res$ok){j<-tryCatch(fromJSON(rawToChar(res$bytes)),error=function(e)NULL);for(cu in url_from_openalex(j))if(nzchar(cu)&&!saved){rr<-safe_get(cu);log_attempt('openalex_full_text',cu,rr,cu);if(isTRUE(rr$ok&&is_doc(rr$content_type,rr$bytes))){verified_url<-cu;saved<-TRUE}}}}
q<-if(nzchar(doi))paste0('DOI:',doi)else paste0('TITLE:"',gsub('"','',title),'"');u<-paste0('https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=',URLencode(q,reserved=TRUE),'&format=json&pageSize=5');res<-safe_get(u,'application/json');log_attempt('europepmc_discovery',u,res);if(res$ok&&!saved){j<-tryCatch(fromJSON(rawToChar(res$bytes)),error=function(e)NULL);results<-tryCatch(j$resultList$result,error=function(e)NULL);pmcids<-character();if(!is.null(results)&&nrow(results)>0&&'pmcid'%in%names(results))pmcids<-na.omit(as.character(results$pmcid));for(pmc in pmcids)if(nzchar(pmc)&&!saved){cu<-paste0('https://www.ebi.ac.uk/europepmc/webservices/rest/',pmc,'/fullTextXML');rr<-safe_get(cu,'application/xml');log_attempt('europepmc_full_text',cu,rr,cu);if(isTRUE(rr$ok&&is_doc(rr$content_type,rr$bytes))){verified_url<-cu;saved<-TRUE}}}}
if(!saved){cu<-paste0('https://scholar.google.com/scholar?hl=en&q=',URLencode(paste0('"',title,'"'),reserved=TRUE));rr<-safe_get(cu,'text/html');log_attempt('google_scholar_title_search',cu,rr,cu,'Single request; CAPTCHA/bot protections are not bypassed')}
if(saved){rr<-safe_get(verified_url);log_attempt('verified_document_download',verified_url,rr,verified_url);if(isTRUE(rr$ok&&is_doc(rr$content_type,rr$bytes))){ext<-if(is_pdf(rr$content_type,rr$bytes))'.pdf'else'.html';saved_path<-file.path('outputs/full_text',paste0(gsub('[^A-Za-z0-9._-]','_',id),ext));writeBin(rr$bytes,saved_path)}else{saved<-FALSE;verified_url<-''}}
statuses[[length(statuses)+1L]]<-data.frame(record_number=id,title=title,doi=doi,retrieval_status=if(saved)'obtained'else'unobtainable',full_text_verified=saved,full_text_url=verified_url,extraction_status=if(saved)'pending_extraction'else'not_started',stringsAsFactors=FALSE);all_attempts[[length(all_attempts)+1L]]<-do.call(rbind,attempts)}
attempt_df<-do.call(rbind,all_attempts);status_df<-do.call(rbind,statuses);prefix<-sprintf('outputs/retrieval/batch_%03d_%03d',start,start+nrow(b)-1L);write_csv(attempt_df,paste0(prefix,'_attempts.csv'));write_csv(status_df,paste0(prefix,'_status.csv'));write_json(list(batch_start=start,batch_end=start+nrow(b)-1L,records=status_df),paste0(prefix,'_status.json'),auto_unbox=TRUE,pretty=TRUE);cat(sprintf('Completed retrieval batch %d-%d: %d obtained, %d unobtainable\n',start,start+nrow(b)-1L,sum(status_df$full_text_verified),sum(!status_df$full_text_verified)))
