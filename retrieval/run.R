source("retrieval/R/fulltext.R")
# The legacy discovery.R is intentionally not sourced. Its search/curl path is
# superseded by runtime_overrides.R and was the source of malformed candidates.
if (file.exists("retrieval/R/runtime_overrides.R")) source("retrieval/R/runtime_overrides.R")

args <- commandArgs(trailingOnly = TRUE)
ids <- if (length(args)) args[[1]] else "__FIRST3__"
out <- "outputs/clean_retrieval"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

master <- read_master("data/living_evidence_map_master.csv")
rid <- find_first_column(names(master), c("record_id", "id"))
sel <- record_ids(master, ids)
if (!length(sel)) stop("No records selected")

manifest <- master[match(sel, master[[rid]]), , drop = FALSE]
utils::write.csv(data.frame(record_id = sel), file.path(out, "selection.csv"), row.names = FALSE, na = "")
utils::write.csv(manifest, file.path(out, "selected_records.csv"), row.names = FALSE, na = "")

results <- lapply(sel, function(id) run_one(master[master[[rid]] == id, , drop = FALSE][1, ], out))
final <- do.call(rbind, results)
utils::write.csv(final, file.path(out, "retrieval_status.csv"), row.names = FALSE, na = "")
utils::write.csv(data.frame(record_id = sel, stringsAsFactors = FALSE), file.path(out, "completed_records.csv"), row.names = FALSE)
cat("Selected:", length(sel), "\n")
print(final)
