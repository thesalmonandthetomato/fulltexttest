#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
start <- if (length(args) >= 1) as.integer(args[[1]]) else 1L
size  <- if (length(args) >= 2) as.integer(args[[2]]) else 10L
idx <- read.csv("data/pilot_index_100.csv", stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(start >= 1L, size >= 1L, start + size - 1L <= nrow(idx))
b <- idx[start:(start + size - 1L), , drop = FALSE]
ids <- if ("record_number" %in% names(b)) b$record_number else if ("id" %in% names(b)) b$id else b$record_id
out <- data.frame(record_number=ids, title=b$title, doi=ifelse("doi" %in% names(b), b$doi, ""), retrieval_status="pending", full_text_verified=FALSE, retrieval_attempts="", extraction_status="not_started", stringsAsFactors=FALSE)
dir.create("outputs", showWarnings=FALSE)
jsonlite::write_json(list(batch_start=start, batch_end=start+nrow(b)-1L, records=out), sprintf("outputs/retrieval_batch_%03d_%03d.json", start, start+nrow(b)-1L), auto_unbox=TRUE, pretty=TRUE)
cat(sprintf("Prepared R retrieval batch %d-%d (%d records)\n", start, start+nrow(b)-1L, nrow(b)))
