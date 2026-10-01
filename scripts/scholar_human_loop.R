#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

usage <- function() {
  cat(
    paste0(
      "Usage:\n",
      "  Rscript scripts/scholar_human_loop.R INPUT.csv [DOI_COLUMN] [DOWNLOAD_DIR] [OUTPUT_DIR] [STATUS_CSV]\n\n",
      "Defaults:\n",
      "  DOI_COLUMN   auto-detected (doi / DOI)\n",
      "  DOWNLOAD_DIR ~/Downloads\n",
      "  OUTPUT_DIR   retrieval/scholar_pdfs\n",
      "  STATUS_CSV   retrieval/scholar_status.csv\n"
    )
  )
}

if (length(args) < 1L) {
  usage()
  quit(status = 2L)
}

input_csv <- normalizePath(args[[1]], mustWork = TRUE)
doi_col_arg <- if (length(args) >= 2L && nzchar(args[[2]])) args[[2]] else NA_character_
download_dir <- path.expand(if (length(args) >= 3L && nzchar(args[[3]])) args[[3]] else "~/Downloads")
output_dir <- if (length(args) >= 4L && nzchar(args[[4]])) args[[4]] else "retrieval/scholar_pdfs"
status_csv <- if (length(args) >= 5L && nzchar(args[[5]])) args[[5]] else "retrieval/scholar_status.csv"

dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(status_csv), recursive = TRUE, showWarnings = FALSE)

clean_doi <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x, ignore.case = TRUE)
  x <- sub("^doi\\s*:\\s*", "", x, ignore.case = TRUE)
  trimws(x)
}

doi_filename <- function(doi) {
  x <- clean_doi(doi)
  x <- gsub("[/\\\\:*?\"<>|]", "_", x)
  paste0(x, ".pdf")
}

is_pdf <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 5) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  identical(rawToChar(readBin(con, "raw", n = 5L)), "%PDF-")
}

write_status <- function(df) {
  tmp <- paste0(status_csv, ".tmp")
  write.csv(df, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, status_csv)) {
    file.copy(tmp, status_csv, overwrite = TRUE)
    unlink(tmp)
  }
}

append_status <- function(status, doi, state, filename = "", original = "", note = "") {
  row <- data.frame(
    doi = doi,
    status = state,
    source = "google_scholar_human",
    filename = filename,
    original_download_name = original,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    note = note,
    stringsAsFactors = FALSE
  )
  rbind(status, row)
}

find_recent_pdf <- function(since) {
  paths <- list.files(download_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  if (!length(paths)) return(character())
  info <- file.info(paths)
  keep <- !is.na(info$mtime) & info$mtime >= since
  paths <- paths[keep]
  if (!length(paths)) return(character())
  info <- file.info(paths)
  paths[order(info$mtime, decreasing = TRUE)]
}


open_url <- function(url) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    status <- suppressWarnings(system2("open", c("-a", "Google Chrome", url), stdout = FALSE, stderr = FALSE))
    if (identical(status, 0L)) return(invisible(TRUE))
  }
  utils::browseURL(url)
  invisible(TRUE)
}

move_pdf <- function(src, dest) {
  if (file.exists(dest)) return(TRUE)
  ok <- file.rename(src, dest)
  if (!ok) {
    ok <- file.copy(src, dest, overwrite = FALSE)
    if (ok) unlink(src)
  }
  ok
}

dat <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)

if (is.na(doi_col_arg)) {
  hit <- which(tolower(names(dat)) == "doi")
  if (!length(hit)) {
    stop("Could not auto-detect a DOI column. Pass the DOI column name as the second argument.")
  }
  doi_col <- names(dat)[hit[[1]]]
} else {
  if (!doi_col_arg %in% names(dat)) {
    stop(sprintf("DOI column '%s' not found. Columns: %s", doi_col_arg, paste(names(dat), collapse = ", ")))
  }
  doi_col <- doi_col_arg
}

dois <- clean_doi(dat[[doi_col]])
dois <- unique(dois[nzchar(dois) & !is.na(dois)])

if (file.exists(status_csv)) {
  status <- read.csv(status_csv, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  status <- data.frame(
    doi = character(),
    status = character(),
    source = character(),
    filename = character(),
    original_download_name = character(),
    timestamp = character(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

final_states <- c("downloaded", "not_found")
done <- unique(clean_doi(status$doi[status$status %in% final_states]))
queue <- dois[!dois %in% done]

cat(sprintf("\nInput: %s\n", input_csv))
cat(sprintf("DOIs found: %d\n", length(dois)))
cat(sprintf("Already finalised: %d\n", length(intersect(dois, done))))
cat(sprintf("Remaining: %d\n", length(queue)))
cat(sprintf("Watching downloads: %s\n", normalizePath(download_dir, mustWork = FALSE)))
cat(sprintf("Saving PDFs to: %s\n", normalizePath(output_dir, mustWork = FALSE)))
cat(sprintf("Checkpoint CSV: %s\n\n", normalizePath(status_csv, mustWork = FALSE)))

if (!length(queue)) {
  cat("Nothing left to process.\n")
  quit(status = 0L)
}

for (i in seq_along(queue)) {
  doi <- queue[[i]]
  target_name <- doi_filename(doi)
  target_path <- file.path(output_dir, target_name)

  if (file.exists(target_path) && is_pdf(target_path)) {
    status <- append_status(status, doi, "downloaded", target_name, "", "PDF already present in output directory")
    write_status(status)
    next
  }

  scholar_url <- paste0(
    "https://scholar.google.com/scholar?q=",
    utils::URLencode(doi, reserved = TRUE)
  )
  doi_url <- paste0("https://doi.org/", utils::URLencode(doi, reserved = TRUE))

  cat("\n", paste(rep("=", 72), collapse = ""), "\n", sep = "")
  cat(sprintf("[%d/%d] %s\n", i, length(queue), doi))
  cat(sprintf("Target filename: %s\n", target_name))
  cat("Opening Google Scholar in your default browser...\n")

  search_started <- Sys.time()
  open_url(scholar_url)

  repeat {
    cat(
      "\nIn Scholar, click an accessible [PDF] or other legitimate full-text link.\n",
      "Then use one of these commands here:\n",
      "  Enter = detect the downloaded PDF and continue\n",
      "  n     = mark not found\n",
      "  s     = skip for later\n",
      "  o     = reopen Scholar\n",
      "  d     = open DOI landing page\n",
      "  q     = save progress and quit\n",
      sep = ""
    )

    cmd <- tolower(trimws(readline("> ")))

    if (cmd == "q") {
      write_status(status)
      cat("Progress saved.\n")
      quit(status = 0L)
    }

    if (cmd == "o") {
      open_url(scholar_url)
      next
    }

    if (cmd == "d") {
      open_url(doi_url)
      next
    }

    if (cmd == "n") {
      status <- append_status(status, doi, "not_found", "", "", "No accessible full text found during manual Scholar check")
      write_status(status)
      break
    }

    if (cmd == "s") {
      status <- append_status(status, doi, "skipped", "", "", "Deferred for later")
      write_status(status)
      break
    }

    if (!nzchar(cmd)) {
      candidates <- find_recent_pdf(search_started)

      if (!length(candidates)) {
        cat("No new PDF detected in the Downloads folder yet.\n")
        next
      }

      valid <- candidates[vapply(candidates, is_pdf, logical(1))]
      if (!length(valid)) {
        cat("A recent .pdf file was found, but it did not pass the PDF header check. Wait for the download to finish and press Enter again.\n")
        next
      }

      src <- valid[[1]]
      original <- basename(src)

      if (!move_pdf(src, target_path)) {
        cat(sprintf("Could not move '%s' to '%s'.\n", src, target_path))
        next
      }

      if (!is_pdf(target_path)) {
        cat("The moved file did not pass the PDF header check. It has not been marked complete.\n")
        next
      }

      status <- append_status(
        status,
        doi,
        "downloaded",
        target_name,
        original,
        "Human selected full text from Google Scholar results; script detected, validated and renamed the PDF"
      )
      write_status(status)
      cat(sprintf("Saved: %s\n", target_path))
      break
    }

    cat("Unknown command.\n")
  }
}

cat("\nQueue complete. Progress is in: ", status_csv, "\n", sep = "")
