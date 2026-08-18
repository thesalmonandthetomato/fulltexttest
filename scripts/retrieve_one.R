#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop('Usage: Rscript scripts/retrieve_one.R RECORD_ID')
id <- args[[1]]
suppressPackageStartupMessages({library(httr2); library(jsonlite)})
source('R/validation.R'); source('R/retrieval.R')
records <- read.csv('data/pilot_records.csv', stringsAsFactors=FALSE, check.names=FALSE)
r <- records[as.character(records$record_id)==id,,drop=FALSE]
if(nrow(r)!=1) stop('record_id not found or not unique: ', id)
dir.create('outputs/retrieval', recursive=TRUE, showWarnings=FALSE)
dir.create('outputs/full_text', recursive=TRUE, showWarnings=FALSE)
log <- data.frame(); verified <- FALSE; chosen <- ''; info <- NULL
urls <- unique(c(if('full_text_url'%in%names(r)) as.character(r$full_text_url) else character(), candidate_urls(if('doi'%in%names(r)) as.character(r$doi) else '', as.character(r$title))))
urls <- urls[nzchar(urls) & !is.na(urls)]
for (u in urls) {
  z <- safe_get(u)
  v <- if(z$ok) validate_document(z$bytes,z$content_type) else list(ok=FALSE,format='unknown',reason=paste0('http_',z$status),text_chars=0,references=0)
  log <- rbind(log,data.frame(record_id=id,url=u,status=z$status,content_type=z$content_type,bytes=length(z$bytes),verified_complete=isTRUE(v$ok),format=v$format,reason=v$reason,stringsAsFactors=FALSE))
  if (z$ok && isTRUE(v$ok)) { verified <- TRUE; chosen <- u; info <- v; ext <- paste0('.',v$format); writeBin(z$bytes,file.path('outputs/full_text',paste0(id,ext))); break }
}
status <- data.frame(record_id=id,title=r$title,doi=if('doi'%in%names(r))r$doi else '',full_text_status=if(verified)'verified_complete'else'unobtainable',document_format=if(verified)info$format else '',references_verified=verified,source_url=chosen,stringsAsFactors=FALSE)
write.csv(log,paste0('outputs/retrieval/',id,'_attempts.csv'),row.names=FALSE,na='')
write.csv(status,paste0('outputs/retrieval/',id,'_status.csv'),row.names=FALSE,na='')
write_json(status,paste0('outputs/retrieval/',id,'_status.json'),auto_unbox=TRUE,pretty=TRUE)
cat(jsonlite::toJSON(status,auto_unbox=TRUE,pretty=TRUE), '\n')
