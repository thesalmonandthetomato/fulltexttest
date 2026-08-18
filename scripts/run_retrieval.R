#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
ids_arg <- if (length(args) >= 1L) args[[1L]] else ''

suppressPackageStartupMessages({library(readr); library(httr2); library(jsonlite)})
source('R/check_dataset.R')
source('R/retrieval.R')

path <- 'data/living_evidence_map_master.csv'
records <- check_dataset(path)

# A deterministic pilot selector used only by the auditable pilot workflow.
# It selects the first three records, in source-file order, that have a
# non-empty record_id and at least one locator (DOI or URL). Manual runs can
# continue to provide explicit comma-separated record IDs.
if (identical(ids_arg, '__FIRST3__')) {
  eligible <- !is.na(records$record_id) & nzchar(trimws(as.character(records$record_id))) &
    ((!is.na(records$doi) & nzchar(trimws(as.character(records$doi)))) |
       (!is.na(records$url_raw) & nzchar(trimws(as.character(records$url_raw)))))
  ids <- unique(as.character(records$record_id[eligible]))[seq_len(min(3L, sum(eligible)))]
  if (length(ids) < 3L) stop('Fewer than three eligible records are available for the pilot')
  selection_mode <- 'deterministic_first_three_eligible'
} else {
  if (!nzchar(ids_arg)) stop('Provide comma-separated record IDs or __FIRST3__')
  ids <- unique(trimws(strsplit(ids_arg, ',', fixed=TRUE)[[1L]]))
  ids <- ids[nzchar(ids)]
  selection_mode <- 'explicit_record_ids'
}

sel <- records[as.character(records$record_id) %in% ids, , drop=FALSE]
missing <- setdiff(ids, as.character(sel$record_id))
if (length(missing)) stop('Requested record IDs not found: ', paste(missing, collapse=', '))

dir.create('outputs/retrieval', recursive=TRUE, showWarnings=FALSE)
dir.create('outputs/full_text', recursive=TRUE, showWarnings=FALSE)
dir.create('outputs/parsed_text', recursive=TRUE, showWarnings=FALSE)

all_attempts <- list(); all_status <- list()
for (i in seq_len(nrow(sel))) {
  r <- sel[i, , drop=FALSE]
  id <- as.character(r$record_id); title <- as.character(r$title); doi <- as.character(r$doi)
  urls <- candidate_urls(r)
  attempts <- list(); verified <- FALSE; chosen <- ''; parsed <- ''; fmt <- ''; refs <- 0L
  for (u in urls) {
    z <- http_get(u)
    v <- if (z$ok) validate_fulltext(z$bytes, z$content_type) else list(ok=FALSE,format='unknown',text='',references=0L,reason=ifelse(is.na(z$status), z$error, paste0('http_',z$status)))
    attempts[[length(attempts)+1L]] <- data.frame(record_id=id, url=u, http_status=z$status, content_type=z$content_type, bytes=length(z$bytes), verified_complete=isTRUE(v$ok), format=v$format, references=v$references, reason=v$reason, error=z$error, stringsAsFactors=FALSE)
    if (z$ok && isTRUE(v$ok)) {
      verified <- TRUE; chosen <- u; parsed <- v$text; fmt <- v$format; refs <- v$references
      ext <- if (fmt == 'pdf') '.pdf' else if (fmt == 'xml') '.xml' else '.html'
      writeBin(z$bytes, file.path('outputs/full_text', paste0(id, ext)))
      writeLines(parsed, file.path('outputs/parsed_text', paste0(id, '.txt')), useBytes=TRUE)
      break
    }
  }
  all_attempts[[length(all_attempts)+1L]] <- do.call(rbind, attempts)
  all_status[[length(all_status)+1L]] <- data.frame(record_id=id, title=title, doi=doi, full_text_status=if (verified) 'verified_complete' else 'unobtainable', document_format=fmt, references_verified=verified, reference_count=refs, source_url=chosen, extraction_status=if (verified) 'eligible' else 'blocked', stringsAsFactors=FALSE)
}

attempts <- do.call(rbind, all_attempts); status <- do.call(rbind, all_status)
write_csv(attempts, 'outputs/retrieval/attempts.csv')
write_csv(status, 'outputs/retrieval/status.csv')
write_json(list(record_ids=ids, selection_mode=selection_mode, status=status), 'outputs/retrieval/status.json', auto_unbox=TRUE, pretty=TRUE)
cat('Selection mode: ', selection_mode, '\n', sep='')
cat('Record IDs: ', paste(ids, collapse=', '), '\n', sep='')
cat('Retrieved ', sum(status$full_text_status == 'verified_complete'), ' complete full texts; ', sum(status$full_text_status != 'verified_complete'), ' not verified.\n', sep='')
