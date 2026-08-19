source("retrieval/R/fulltext.R")

baseline_run_one <- run_one
if (file.exists("retrieval/R/additive_discovery_v4.R")) source("retrieval/R/additive_discovery_v4.R")
run_one <- additive_run_one

args <- commandArgs(trailingOnly = TRUE)
ids <- if (length(args)) args[[1]] else "__FIRST3__"
out <- "outputs/clean_retrieval"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

master <- read_master("data/living_evidence_map_master.csv")
if (ncol(master) < 7L) stop("Master database has fewer than 7 columns; cannot use column G as title")
master[["title"]] <- normalise(master[[7L]])
rid <- find_first_column(names(master), c("record_id", "id"))
sel <- record_ids(master, ids)
if (!length(sel)) stop("No records selected")
manifest <- master[match(sel, master[[rid]]), , drop = FALSE]
utils::write.csv(data.frame(record_id = sel), file.path(out, "selection.csv"), row.names = FALSE, na = "")
utils::write.csv(manifest, file.path(out, "selected_records.csv"), row.names = FALSE, na = "")
results <- lapply(sel, function(id) {
  ans <- run_one(master[master[[rid]] == id, , drop = FALSE][1, ], out)
  # Ensure every record contributes the same result schema, even when baseline
  # returns an audit-only object or discovery returns a shortened result.
  ans[["record_id"]] <- as.character(id)
  ans
})
required <- c("record_id", "full_text_status", "format", "source_url", "text_chars", "reference_markers")
results <- lapply(results, function(x) {
  for (nm in setdiff(required, names(x))) x[[nm]] <- NA
  x[required]
})
final <- do.call(rbind, results)
utils::write.csv(final, file.path(out, "retrieval_status.csv"), row.names = FALSE, na = "")
utils::write.csv(data.frame(record_id = sel, stringsAsFactors = FALSE), file.path(out, "completed_records.csv"), row.names = FALSE)
cat("Selected:", length(sel), "\n")
print(final)
