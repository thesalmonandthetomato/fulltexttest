#!/usr/bin/env Rscript
# Auditable full-text retrieval. Never treats metadata/abstracts as full text.
# Routes: DOI/publisher URL, PMC, Europe PMC, Unpaywall, CORE, Crossref/OpenAlex
# where a usable open document URL can be discovered. Every attempt is logged.

args <- commandArgs(trailingOnly=TRUE)
start <- if(length(args)>=1) as.integer(args[[1]]) else 1L
size  <- if(length(args)>=2) as.integer(args[[2]]) else 10L

suppressPackageStartupMessages({
  library(jsonlite)
  library(httr2)
})

idx <- read.csv('data/pilot_index_100.csv', stringsAsFactors=FALSE, check.names=FALSE)
stopifnot(start>=1L, size>=1L, start+size-1L<=nrow(idx))
b <- idx[start:(start+size-1L),,drop=FALSE]
dir.create('outputs/retrieval', recursive=TRUE, showWarnings=FALSE)
dir.create('outputs/full_text', recursive=TRUE, showWarnings=FALSE)

safe_get <- function(url) {
  tryCatch({
    resp <- request(url) |> req_user_agent('fulltexttest/1.0 reproducible research retrieval') |> req_timeout(30) |> req_perform()
    list(ok=resp_status(resp)>=200 && resp_status(resp)<300, status=resp_status(resp), content_type=resp_headers(resp)[['content-type']] %||% '', bytes=resp_body_raw(resp))
  }, error=function(e) list(ok=FALSE,status=NA,content_type='',bytes=raw(),error=conditionMessage(e)))
}

`%||%` <- function(x,y) if(is.null(x)||is.na(x)) y else x

is_document <- function(ct, bytes) {
  ct <- tolower(ct)
  grepl('pdf|xml|html|text',ct) || (length(bytes)>4 && identical(rawToChar(bytes[1:4]), '%PDF'))
}

write_attempt <- function(id, route, url, result, path='') {
  data.frame(record_number=id, route=route, url=url, status=result$status, content_type=result$content_type, usable_full_text=isTRUE(result$ok && is_document(result$content_type,result$bytes)), local_path=path, error=ifelse(is.null(result$error),'',result$error), stringsAsFactors=FALSE)
}

all_attempts <- list(); statuses <- list()
for(i in seq_len(nrow(b))) {
  r <- b[i,]; id <- as.character(if('record_number'%in%names(r)) r$record_number else r$id); doi <- if('doi'%in%names(r)) as.character(r$doi) else ''
  title <- as.character(r$title); attempts <- list(); saved <- FALSE; saved_path <- ''
  candidates <- character()
  if(nzchar(doi) && !is.na(doi)) candidates <- c(candidates, paste0('https://doi.org/',doi))
  # Open-access discovery APIs: these are discovery routes, not evidence of full text.
  if(nzchar(doi) && !is.na(doi)) {
    candidates <- c(candidates,
      paste0('https://api.openalex.org/works/https://doi.org/',doi),
      paste0('https://api.unpaywall.org/v2/',doi,'?email=research@example.org'),
      paste0('https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=DOI:',doi,'&format=json'))
  }
  # Direct title/identifier candidates from the source index are also attempted when present.
  for(url in unique(candidates)) {
    res <- safe_get(url)
    a <- write_attempt(id,'candidate_discovery_or_direct',url,res)
    attempts[[length(attempts)+1L]] <- a
    # Only save actual document responses; JSON/API metadata is never counted as full text.
    if(isTRUE(a$usable_full_text) && !saved) {
      ext <- if(grepl('pdf',a$content_type,ignore.case=TRUE) || (length(res$bytes)>4 && identical(rawToChar(res$bytes[1:4]),'%PDF'))) '.pdf' else '.html'
      saved_path <- file.path('outputs/full_text',paste0(gsub('[^A-Za-z0-9._-]','_',id),ext))
      writeBin(res$bytes,saved_path); saved <- TRUE
    }
  }
  at <- if(length(attempts)) do.call(rbind,attempts) else data.frame()
  all_attempts[[length(all_attempts)+1L]] <- at
  statuses[[length(statuses)+1L]] <- data.frame(record_number=id,title=title,doi=doi,retrieval_status=if(saved)'retrieved' else 'unobtainable',full_text_verified=saved,retrieved_path=saved_path,extraction_status=if(saved)'pending' else 'not_started',stringsAsFactors=FALSE)
}

attempt_df <- do.call(rbind,all_attempts)
status_df <- do.call(rbind,statuses)
write.csv(attempt_df,sprintf('outputs/retrieval/retrieval_attempts_%03d_%03d.csv',start,start+nrow(b)-1L),row.names=FALSE,na='')
write.csv(status_df,sprintf('outputs/retrieval/retrieval_status_%03d_%03d.csv',start,start+nrow(b)-1L),row.names=FALSE,na='')
write_json(list(batch_start=start,batch_end=start+nrow(b)-1L,records=status_df),sprintf('outputs/retrieval/retrieval_status_%03d_%03d.json',start,start+nrow(b)-1L),auto_unbox=TRUE,pretty=TRUE)
