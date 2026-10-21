#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(httr2))
suppressPackageStartupMessages(library(jsonlite))
token <- Sys.getenv("ZENODO_TOKEN")
if (!nzchar(token)) stop("ZENODO_TOKEN secret is missing")
dir.create("outputs/zenodo_inventory",recursive=TRUE,showWarnings=FALSE)
get_page <- function(page) {
  req <- request("https://zenodo.org/api/deposit/depositions") |>
    req_headers(Authorization=paste("Bearer",token)) |>
    req_url_query(page=page,size=100,sort="mostrecent") |>
    req_timeout(50) |> req_retry(max_tries=4)
  resp <- req_perform(req)
  fromJSON(resp_body_string(resp),simplifyVector=FALSE)
}
val <- function(x) if (is.null(x)||!length(x)) "" else as.character(x[[1]])
dep_rows <- list(); file_rows <- list(); page <- 1L
repeat {
  deps <- get_page(page)
  if(!length(deps)) break
  for(dep in deps) {
    id <- val(dep$id); title <- val(dep$metadata$title)
    fs <- dep$files
    dep_rows[[length(dep_rows)+1L]] <- data.frame(
      deposition_id=id,title=title,state=val(dep$state),created=val(dep$created),
      modified=val(dep$modified),file_count=length(fs))
    if(!length(fs)) next
    for(f in fs) {
      nm <- val(f$filename)
      if(!nzchar(nm)) nm <- val(f$key)
      mt <- regexpr("10\\.[0-9]{4,9}/[^[:space:]]+",nm,perl=TRUE,ignore.case=TRUE)
      doi <- if(mt[1]>0) regmatches(nm,mt) else ""
      doi <- sub("\\.pdf$","",doi,ignore.case=TRUE)
      file_rows[[length(file_rows)+1L]] <- data.frame(
        deposition_id=id,deposition_title=title,deposition_state=val(dep$state),
        deposition_created=val(dep$created),file_name=nm,file_size_bytes=val(f$filesize),
        file_checksum=val(f$checksum),file_id=val(f$id),
        is_pdf=grepl("\\.pdf$",nm,ignore.case=TRUE),
        doi_from_filename=doi,
        doi_match_method=if(nzchar(doi)) "literal_filename" else "unresolved")
    }
  }
  message("Page ",page,": ",length(deps)," deposits, cumulative files ",length(file_rows))
  if(length(deps)<100L) break
  page <- page+1L
  if(page>1000L) stop("Pagination safety limit")
}
empty <- function(cols) setNames(as.data.frame(matrix(nrow=0,ncol=length(cols))),cols)
dcols <- c("deposition_id","title","state","created","modified","file_count")
fcols <- c("deposition_id","deposition_title","deposition_state","deposition_created","file_name","file_size_bytes","file_checksum","file_id","is_pdf","doi_from_filename","doi_match_method")
dd <- if(length(dep_rows)) do.call(rbind,dep_rows) else empty(dcols)
df <- if(length(file_rows)) do.call(rbind,file_rows) else empty(fcols)
write.csv(dd,"outputs/zenodo_inventory/zenodo_depositions.csv",row.names=FALSE,na="")
write.csv(df,"outputs/zenodo_inventory/zenodo_all_files.csv",row.names=FALSE,na="")
pdfs <- df[df$is_pdf,,drop=FALSE]
write.csv(pdfs,"outputs/zenodo_inventory/zenodo_pdfs_doi_log.csv",row.names=FALSE,na="")
report <- c(paste("Depositions:",nrow(dd)),paste("Files:",nrow(df)),
  paste("PDF files:",nrow(pdfs)),
  paste("PDFs with DOI in filename:",sum(nzchar(pdfs$doi_from_filename))),
  paste("PDFs requiring DOI reconciliation:",sum(!nzchar(pdfs$doi_from_filename))),
  "File listing does not validate PDF contents or count unsuccessful retrieval attempts.",
  "Only deposits accessible to this credential are included.")

# Identify historical archive deposits by the consistent retrieval titles and compressed-file names.
archive_title <- grepl("openalex|full.?text|salmon|living.?evidence", dd$title, ignore.case=TRUE)
archive_file <- grepl("\\.(zip|tar\\.gz|tgz)$",df$file_name,ignore.case=TRUE)
candidate_ids <- unique(c(dd$deposition_id[archive_title],df$deposition_id[archive_file]))
candidates <- dd[dd$deposition_id %in% candidate_ids,,drop=FALSE]
candidate_files <- df[df$deposition_id %in% candidate_ids,,drop=FALSE]
write.csv(candidates,"outputs/zenodo_inventory/candidate_archive_deposits.csv",row.names=FALSE,na="")
write.csv(candidate_files,"outputs/zenodo_inventory/candidate_archive_files.csv",row.names=FALSE,na="")
cat("HISTORICAL ARCHIVE CANDIDATES ",nrow(candidates)," DEPOSITS, ",sum(archive_file)," COMPRESSED FILES\\n",sep="")
if(nrow(candidates)) {
  for(i in seq_len(nrow(candidates))) {
    id <- candidates$deposition_id[i]
    ff <- candidate_files$file_name[candidate_files$deposition_id==id]
    cat("CANDIDATE | ",id," | ",candidates$title[i]," | ",paste(ff,collapse="; "), "\\n",sep="")
  }
}

writeLines(report,"outputs/zenodo_inventory/summary.txt")
cat(paste(report,collapse="\n"),"\n")
