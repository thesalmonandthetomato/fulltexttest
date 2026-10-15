#!/usr/bin/env Rscript
# University of Johannesburg browser retrieval experiment.
# Requires an already-running dedicated Chrome instance on localhost:9222.
# R-only; does not handle credentials, MFA, proxies, or authentication bypass.

suppressPackageStartupMessages({
  library(chromote)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
opt <- function(key, default = NULL) {
  x <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (!length(x)) default else sub(paste0("^--", key, "="), "", tail(x, 1L))
}
input <- opt("input", "dois.csv")
output <- path.expand(opt("output", "~/fulltexttest-pdfs"))
mode <- opt("mode", "inspect")
delay <- suppressWarnings(as.numeric(opt("delay", "30")))
port <- suppressWarnings(as.integer(opt("port", "9222")))
if (!mode %in% c("inspect", "download")) stop("mode must be inspect or download")
if (!is.finite(delay) || delay < 10) stop("delay must be >= 10 seconds")
if (is.na(port) || port < 1 || port > 65535) stop("invalid port")
if (!file.exists(input)) stop("Input CSV not found: ", input)
dois <- read.csv(input, stringsAsFactors = FALSE)
if (!"doi" %in% names(dois)) stop("Input CSV needs a 'doi' column")
dois$doi <- trimws(sub("^https?://(dx\\.)?doi\\.org/", "", dois$doi, ignore.case = TRUE))
dois <- dois[nzchar(dois$doi) & !is.na(dois$doi), , drop = FALSE]
if (!nrow(dois)) stop("No DOIs to retrieve")
if (any(!grepl("^10\\.[0-9]{4,9}/\\S+$", dois$doi))) stop("Invalid DOI in input CSV")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
manifest <- file.path(output, "retrieval_manifest.csv")
if (!file.exists(manifest)) {
  write.csv(data.frame(doi=character(), status=character(), page_url=character(),
                       page_title=character(), pdf_candidate=character(),
                       file=character(), details=character()),
            manifest, row.names = FALSE)
}
completed <- read.csv(manifest, stringsAsFactors = FALSE)
# Reattempt non-successful cases on a subsequent run.
done <- completed$doi[completed$status == "pdf_downloaded"]
dois <- dois[!dois$doi %in% done, , drop = FALSE]

browser <- Chromote$new(browser = ChromeRemote$new(host = "127.0.0.1", port = port))
session <- browser$new_session()
on.exit(try(session$close(), silent = TRUE), add = TRUE)

if (mode == "download") {
  # This affects downloads in this dedicated browser session; PDFs opened in
  # the built-in viewer might not produce a downloaded file.
  session$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(output))
}

page_info <- function() {
  js <- paste0(
    "JSON.stringify({url:location.href,title:document.title,",
    "links:Array.from(document.querySelectorAll('a[href]'))",
    ".map(a=>({href:a.href,label:(a.innerText||a.getAttribute('aria-label')||'').trim()}))",
    ".filter(a=>/\\.pdf(?:[?#]|$)|pdf|download full text/i.test(a.href+' '+a.label))",
    ".slice(0,25)})"
  )
  r <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  fromJSON(r$result$value, simplifyVector = FALSE)
}
is_pdf <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 5) return(FALSE)
  identical(readBin(path, "raw", n = 5), charToRaw("%PDF-"))
}
save_result <- function(doi, status, info, candidate = "", file = "", details = "") {
  row <- data.frame(doi=doi, status=status,
                    page_url=if (is.null(info$url)) "" else info$url,
                    page_title=if (is.null(info$title)) "" else info$title,
                    pdf_candidate=candidate, file=file, details=details)
  write.table(row, manifest, sep=",", row.names=FALSE, col.names=FALSE,
              append=TRUE, qmethod="double", na="")
  message(doi, ": ", status, if (nzchar(details)) paste0(" (", details, ")") else "")
}
for (i in seq_len(nrow(dois))) {
  doi <- dois$doi[i]
  message("Visiting ", i, "/", nrow(dois), ": ", doi)
  info <- list(url="", title="")
  tryCatch({
    url <- paste0("https://doi.org/", utils::URLencode(doi, reserved=TRUE))
    session$Page$navigate(url = url)
    # Leave time for redirects/extension and institutional gateway to settle.
    Sys.sleep(8)
    info <- page_info()
    links <- info$links
    candidates <- Filter(function(x) {
      grepl("\\.pdf(?:[?#]|$)|pdf|download full text", paste(x$href, x$label),
            ignore.case = TRUE)
    }, links)
    candidate <- if (length(candidates)) candidates[[1]]$href else ""
    if (grepl("login|signin|sign-in|openathens|authenticate", info$url,
              ignore.case = TRUE)) {
      save_result(doi, "authentication_required", info, candidate)
    } else if (mode == "inspect") {
      save_result(doi, "inspected", info, candidate,
                  details=paste("Candidate links:", length(candidates)))
    } else if (!nzchar(candidate)) {
      save_result(doi, "no_pdf_link", info)
    } else {
      before <- list.files(output, full.names = TRUE)
      # Avoid scripted fetch() here: browser navigation preserves auth context.
      session$Page$navigate(url = candidate)
      Sys.sleep(12)
      after <- setdiff(list.files(output, full.names = TRUE), before)
      valid <- after[vapply(after, is_pdf, logical(1))]
      if (length(valid)) {
        save_result(doi, "pdf_downloaded", info, candidate,
                    file=basename(valid[1]))
      } else {
        save_result(doi, "needs_manual_or_adapter", info, candidate,
                    details="No validated PDF download; Chrome viewer or provider-specific flow")
      }
    }
  }, error = function(e) {
    save_result(doi, "error", info, details=conditionMessage(e))
  })
  if (i < nrow(dois)) Sys.sleep(delay)
}
message("Manifest: ", manifest)
