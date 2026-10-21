#!/usr/bin/env Rscript
dir.create("outputs/historical_final",recursive=TRUE,showWarnings=FALSE)
base <- read.csv("audit_results/historical_fulltext_doi_log_39_batches.csv",stringsAsFactors=FALSE,check.names=FALSE)
p <- "downloaded_audit/download_manifest.csv"
if(!file.exists(p)) stop("Missing downloaded batch 39 audit manifest: ",p)
m <- read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)
archive <- read.csv("downloaded_audit/archive_contents.csv",stringsAsFactors=FALSE,check.names=FALSE)
pdf_archive <- sum(grepl("\\.pdf$",archive$Name,ignore.case=TRUE))
stopifnot(nrow(m)==100, pdf_archive==100, all(m$format=="pdf"), all(m$status=="downloaded"))
add <- data.frame(batch=m$batch, github_run_id=m$github_run_id, zenodo_deposit_id=m$zenodo_deposit_id,
  record_index=m$batch_position,doi=m$doi,openalex_id=m$openalex_id,format=m$format,
  source_url=m$url,status=m$status,bytes=m$bytes,sha256=m$sha256,filename=m$file,
  stringsAsFactors=FALSE)
stopifnot(identical(names(base),names(add)))
all <- rbind(base,add)
all <- all[order(all$batch,all$record_index),,drop=FALSE]
stopifnot(nrow(all)==3935, length(unique(paste(all$batch,all$record_index)))==3935)
normdoi <- function(s) tolower(sub("^https?://(?:dx\\.)?doi\\.org/","",trimws(s),ignore.case=TRUE))
all$doi <- normdoi(all$doi)
ok <- all$status %in% c("downloaded","already_present")
pdf <- all[ok & all$format=="pdf",,drop=FALSE]
xml <- all[ok & all$format=="grobid_xml",,drop=FALSE]
write.csv(all,"outputs/historical_final/historical_fulltext_all_records.csv",row.names=FALSE,na="")
write.csv(pdf,"outputs/historical_final/historical_pdf_only_doi_log.csv",row.names=FALSE,na="")
batches <- split(all,all$batch)
per <- do.call(rbind,lapply(batches,function(x) data.frame(
 batch=x$batch[1],run_id=x$github_run_id[1],zenodo_deposit=x$zenodo_deposit_id[1],
 attempted=nrow(x),downloaded=sum(x$status=="downloaded"),already_present=sum(x$status=="already_present"),
 failed=sum(!(x$status %in% c("downloaded","already_present"))),
 pdf=sum(x$format=="pdf" & x$status %in% c("downloaded","already_present")),
 xml=sum(x$format=="grobid_xml" & x$status %in% c("downloaded","already_present")))))
write.csv(per,"outputs/historical_final/historical_batch_summary.csv",row.names=FALSE,na="")
summary <- c(
 paste("Records attempted:",nrow(all)),
 paste("Successfully retrieved or already present:",sum(ok)),
 paste("Fresh downloads:",sum(all$status=="downloaded")),
 paste("Already present:",sum(all$status=="already_present")),
 paste("Failed records:",sum(!ok)),
 paste("PDF records:",nrow(pdf)),
 paste("GROBID XML records:",nrow(xml)),
 paste("Unique successful DOIs:",length(unique(all$doi[ok & nzchar(all$doi)]))),
 paste("Unique PDF DOIs:",length(unique(pdf$doi[nzchar(pdf$doi)]))),
 paste("Full-text success rate:",sprintf("%.4f%%",100*mean(ok))),
 paste("PDF retrieval rate:",sprintf("%.4f%%",100*nrow(pdf)/nrow(all))),
 "These are 40 batches of the historical OpenAlex retrieval operation, not the full evidence-map corpus.",
 "The report uses archived DOI manifests and recorded upload receipts, not new publisher downloads."
)
writeLines(summary,"outputs/historical_final/summary.txt")
cat(paste(summary,collapse="\n"),"\n")
