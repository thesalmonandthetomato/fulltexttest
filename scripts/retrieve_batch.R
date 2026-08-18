#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
start <- if(length(args)>=1) as.integer(args[[1]]) else 1L
n <- if(length(args)>=2) as.integer(args[[2]]) else 3L
records <- read.csv('data/pilot_records.csv', stringsAsFactors=FALSE, check.names=FALSE)
stopifnot(start >= 1L, n >= 1L, start+n-1L <= nrow(records))
for (id in as.character(records$record_id[start:(start+n-1L)])) {
  system2('Rscript', c('scripts/retrieve_one.R', shQuote(id)))
}
