#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(httr2))
suppressPackageStartupMessages(library(jsonlite))
token <- Sys.getenv("ZENODO_TOKEN")
if(!nzchar(token)) stop("Missing ZENODO_TOKEN")
dir.create("outputs/historical39",recursive=TRUE,showWarnings=FALSE)
dep <- 23021043
u <- sprintf("https://zenodo.org/api/deposit/depositions/%s",dep)
rsp <- req_perform(request(u) |> req_headers(Authorization=paste("Bearer",token)) |> req_timeout(60))
d <- fromJSON(resp_body_string(rsp),simplifyVector=FALSE)
fs <- d$files
if(length(fs)!=1L) stop("Expected one ZIP in batch39, got ",length(fs))
f <- fs[[1]]
nm <- as.character(f$filename)
if(!grepl("\\.zip$",nm,ignore.case=TRUE)) stop("Not ZIP: ",nm)
url <- f$links$download
if(is.null(url)||!nzchar(url)) stop("No download URL")
target <- tempfile(fileext=".zip")
message("Downloading existing batch 39 ZIP (only one archive; no new retrieval)")
req_perform(request(url) |> req_headers(Authorization=paste("Bearer",token)) |> req_timeout(900) |> req_retry(max_tries=3),path=target)
z <- unzip(target,list=TRUE)
write.csv(z,"outputs/historical39/archive_contents.csv",row.names=FALSE)
message("Archive members ",nrow(z)," PDF members ",sum(grepl("\\.pdf$",z$Name,ignore.case=TRUE)))
cand <- z$Name[grepl("download_manifest\\.csv$|fulltext_manifest\\.csv$|manifest\\.csv$",z$Name,ignore.case=TRUE)]
if(!length(cand)){message("No manifest, exporting PDF names only");quit(status=0)}
extract <- tempfile("manifest-dir-");dir.create(extract)
unzip(target,files=cand[1],exdir=extract)
p <- file.path(extract,cand[1])
m <- read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)
m$zenodo_deposit_id <- dep
m$github_run_id <- 36457592665
write.csv(m,"outputs/historical39/download_manifest.csv",row.names=FALSE,na="")
message("Manifest rows ",nrow(m)," columns ",paste(names(m),collapse=","))
