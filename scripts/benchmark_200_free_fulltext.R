#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr); library(httr2); library(jsonlite); library(dplyr)
  library(purrr); library(stringr); library(tidyr)
})
"%||%" <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
set.seed(20261006)
canonical_url <- "https://raw.githubusercontent.com/thesalmonandthetomato/LivingEvidenceMap/main/data/master/current/living_evidence_map_master.csv"
out_dir <- "results/benchmark_200_free"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

clean_doi <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  x <- sub("[[:space:][:punct:]]+$", "", x)
  x
}

message("Reading authoritative canonical master...")
master <- read_csv(canonical_url, show_col_types = FALSE, progress = FALSE)
names_lc <- tolower(names(master))
doi_idx <- which(names_lc == "doi")
if (!length(doi_idx)) stop("No DOI column found in canonical master")
doi_col <- names(master)[doi_idx[[1]]]
sample_pool <- master |>
  mutate(.doi = clean_doi(.data[[doi_col]])) |>
  filter(!is.na(.doi), nzchar(.doi), str_detect(.doi, "^10\\.[0-9]{4,9}/")) |>
  distinct(.doi, .keep_all = TRUE)
if (nrow(sample_pool) < 200) stop("Fewer than 200 unique DOI-bearing records in canonical master")
sample_200 <- sample_pool |> slice_sample(n = 200)
sample_tbl <- tibble(sample_id = seq_len(nrow(sample_200)), doi = sample_200$.doi)
if ("title" %in% names_lc) {
  title_col <- names(master)[match("title", names_lc)]
  sample_tbl$title <- sample_200[[title_col]]
}
write_csv(sample_tbl, file.path(out_dir, "sample_200.csv"))

ua <- "fulltexttest/200-doi-free-benchmark (https://github.com/thesalmonandthetomato/fulltexttest)"
unpaywall_email <- Sys.getenv("UNPAYWALL_EMAIL", "fulltexttest@example.org")
openalex_key <- Sys.getenv("OPENALEX_API_KEY", "")
springer_key <- Sys.getenv("SPRINGER_API_KEY", "")
core_key <- Sys.getenv("CORE_API_KEY", "")

safe_get <- function(url, query = list(), headers = list(), timeout_s = 45) {
  tryCatch({
    req <- request(url) |> req_user_agent(ua) |> req_timeout(timeout_s) |>
      req_retry(max_tries = 4, backoff = ~ min(8, 2^.x))
    if (length(query)) req <- do.call(req_url_query, c(list(req), query))
    if (length(headers)) req <- do.call(req_headers, c(list(req), headers))
    resp <- req_perform(req)
    list(ok = resp_status(resp) >= 200 && resp_status(resp) < 300,
         status = resp_status(resp), content_type = resp_header(resp, "content-type") %||% "",
         url = resp_url(resp), body = resp_body_raw(resp), error = NA_character_)
  }, error = function(e) {
    list(ok = FALSE, status = NA_integer_, content_type = "", url = url, body = raw(), error = conditionMessage(e))
  })
}

classify_body <- function(raw_body, content_type = "") {
  n <- length(raw_body)
  if (!n) return(list(ok = FALSE, filetype = NA_character_, bytes = 0L))
  is_pdf <- n >= 5 && identical(rawToChar(raw_body[1:4]), "%PDF")
  head_txt <- tryCatch(rawToChar(raw_body[seq_len(min(n, 5000L))]), error = function(e) "")
  ct <- tolower(content_type %||% "")
  is_xml <- grepl("xml", ct, fixed = TRUE) || grepl("<article|<book-part|<\\?xml", head_txt, ignore.case = TRUE)
  is_html <- grepl("html", ct, fixed = TRUE) || grepl("<html|<!doctype html|<article", head_txt, ignore.case = TRUE)
  if (is_pdf && n >= 10000) return(list(ok = TRUE, filetype = "PDF", bytes = n))
  if (is_xml && n >= 5000 && grepl("<(article|body|book-part)", head_txt, ignore.case = TRUE))
    return(list(ok = TRUE, filetype = "XML/JATS", bytes = n))
  if (is_html && n >= 15000 && grepl("<(article|main|body)", head_txt, ignore.case = TRUE))
    return(list(ok = TRUE, filetype = "HTML", bytes = n))
  list(ok = FALSE, filetype = NA_character_, bytes = n)
}

result_row <- function(doi, source, status, filetype = NA_character_, url = NA_character_,
                       http_status = NA_integer_, bytes = NA_integer_, error = NA_character_) {
  tibble(doi = doi, source = source, status = status, filetype = filetype,
         url = url, http_status = http_status, bytes = bytes, error = error)
}
try_document <- function(doi, source, url) {
  if (is.na(url) || !nzchar(url)) return(result_row(doi, source, "not_found"))
  z <- safe_get(url)
  if (!z$ok) return(result_row(doi, source, "download_failed", url = z$url, http_status = z$status, error = z$error))
  cl <- classify_body(z$body, z$content_type)
  result_row(doi, source, if (cl$ok) "retrieved" else "not_fulltext", cl$filetype, z$url, z$status, cl$bytes)
}
first_success <- function(rows, doi, source) {
  if (!length(rows)) return(result_row(doi, source, "not_found"))
  x <- bind_rows(rows)
  hit <- x |> filter(status == "retrieved") |> slice(1)
  if (nrow(hit)) hit else x |> slice(1)
}

probe_openalex <- function(doi) {
  z <- safe_get(paste0("https://api.openalex.org/works/https://doi.org/", URLencode(doi, reserved = TRUE)))
  if (!z$ok) return(result_row(doi, "OpenAlex", "api_failed", http_status = z$status, error = z$error))
  d <- tryCatch(fromJSON(rawToChar(z$body), simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(d)) return(result_row(doi, "OpenAlex", "api_parse_failed"))
  urls <- character()
  if (!is.null(d$best_oa_location$pdf_url)) urls <- c(urls, d$best_oa_location$pdf_url)
  if (!is.null(d$locations)) for (loc in d$locations) if (!is.null(loc$pdf_url)) urls <- c(urls, loc$pdf_url)
  wid <- sub(".*/", "", d$id %||% "")
  if (nzchar(openalex_key) && nzchar(wid) && isTRUE(d$has_content$pdf))
    urls <- c(paste0("https://content.openalex.org/works/", wid, ".pdf?api_key=", URLencode(openalex_key, reserved = TRUE)), urls)
  urls <- unique(urls[nzchar(urls)])
  first_success(lapply(urls, function(u) try_document(doi, "OpenAlex", u)), doi, "OpenAlex")
}
probe_europepmc <- function(doi) {
  z <- safe_get("https://www.ebi.ac.uk/europepmc/webservices/rest/search",
                query = list(query = paste0('DOI:"', doi, '"'), resultType = "core", format = "json"))
  if (!z$ok) return(result_row(doi, "Europe PMC", "api_failed", http_status = z$status, error = z$error))
  d <- tryCatch(fromJSON(rawToChar(z$body), simplifyVector = FALSE), error = function(e) NULL)
  hits <- d$resultList$result %||% list()
  exact <- Filter(function(x) identical(tolower(x$doi %||% ""), doi), hits)
  if (!length(exact) || is.null(exact[[1]]$pmcid)) return(result_row(doi, "Europe PMC", "not_found"))
  try_document(doi, "Europe PMC", paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/", exact[[1]]$pmcid, "/fullTextXML"))
}
probe_unpaywall <- function(doi) {
  z <- safe_get(paste0("https://api.unpaywall.org/v2/", URLencode(doi, reserved = TRUE)), query = list(email = unpaywall_email))
  if (!z$ok) return(result_row(doi, "Unpaywall", "api_failed", http_status = z$status, error = z$error))
  d <- tryCatch(fromJSON(rawToChar(z$body), simplifyVector = FALSE), error = function(e) NULL)
  locs <- c(if (!is.null(d$best_oa_location)) list(d$best_oa_location) else list(), d$oa_locations %||% list())
  urls <- unique(unlist(lapply(locs, function(x) x$url_for_pdf %||% NA_character_)))
  urls <- urls[!is.na(urls) & nzchar(urls)]
  first_success(lapply(urls, function(u) try_document(doi, "Unpaywall", u)), doi, "Unpaywall")
}
probe_semantic <- function(doi) {
  z <- safe_get(paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", URLencode(doi, reserved = TRUE)),
                query = list(fields = "title,openAccessPdf"))
  if (!z$ok) return(result_row(doi, "Semantic Scholar", "api_failed", http_status = z$status, error = z$error))
  d <- tryCatch(fromJSON(rawToChar(z$body), simplifyVector = FALSE), error = function(e) NULL)
  try_document(doi, "Semantic Scholar", d$openAccessPdf$url %||% NA_character_)
}
probe_crossref <- function(doi) {
  z <- safe_get(paste0("https://api.crossref.org/works/", URLencode(doi, reserved = TRUE)))
  if (!z$ok) return(result_row(doi, "Crossref links", "api_failed", http_status = z$status, error = z$error))
  d <- tryCatch(fromJSON(rawToChar(z$body), simplifyVector = FALSE), error = function(e) NULL)
  links <- d$message$link %||% list()
  urls <- unique(unlist(lapply(links, function(x) {
    ct <- tolower(x[["content-type"]] %||% "")
    if (grepl("pdf|xml|html", ct)) x$URL %||% NA_character_ else NA_character_
  })))
  urls <- urls[!is.na(urls) & nzchar(urls)]
  first_success(lapply(urls, function(u) try_document(doi, "Crossref links", u)), doi, "Crossref links")
}
probe_springer <- function(doi) {
  if (!nzchar(springer_key)) return(result_row(doi, "Springer OA API", "skipped_missing_api_key"))
  z <- safe_get("https://api.springernature.com/openaccess/jats",
                query = list(q = paste0("doi:", doi), api_key = springer_key))
  if (!z$ok) return(result_row(doi, "Springer OA API", "api_failed", http_status = z$status, error = z$error))
  cl <- classify_body(z$body, z$content_type)
  result_row(doi, "Springer OA API", if (cl$ok) "retrieved" else "not_found", cl$filetype, z$url, z$status, cl$bytes)
}
probe_core <- function(doi) {
  if (!nzchar(core_key)) return(result_row(doi, "CORE", "skipped_missing_api_key"))
  z <- safe_get("https://api.core.ac.uk/v3/search/works",
                query = list(q = paste0('doi:"', doi, '"'), limit = 5),
                headers = list(Authorization = paste("Bearer", core_key)))
  if (!z$ok) return(result_row(doi, "CORE", "api_failed", http_status = z$status, error = z$error))
  d <- tryCatch(fromJSON(rawToChar(z$body), simplifyVector = FALSE), error = function(e) NULL)
  hits <- d$results %||% list()
  urls <- unique(unlist(lapply(hits, function(x) c(x$downloadUrl %||% NA_character_, x$fullTextIdentifier %||% NA_character_))))
  urls <- urls[!is.na(urls) & nzchar(urls) & grepl("^https?://", urls)]
  first_success(lapply(urls, function(u) try_document(doi, "CORE", u)), doi, "CORE")
}
probes <- list(probe_openalex, probe_europepmc, probe_unpaywall, probe_semantic, probe_crossref, probe_springer, probe_core)

all_results <- vector("list", nrow(sample_tbl))
for (i in seq_len(nrow(sample_tbl))) {
  doi <- sample_tbl$doi[[i]]
  message(sprintf("[%d/200] %s", i, doi))
  all_results[[i]] <- bind_rows(lapply(probes, function(f) tryCatch(f(doi), error = function(e)
    result_row(doi, "unknown", "probe_error", error = conditionMessage(e)))))
  Sys.sleep(0.15)
}
results <- bind_rows(all_results)
write_csv(results, file.path(out_dir, "source_results.csv"))
summary <- results |> count(source, status, filetype, name = "n") |> arrange(source, status, filetype)
write_csv(summary, file.path(out_dir, "summary_by_source_filetype.csv"))
retrieved <- results |> filter(status == "retrieved")
preference <- c("PDF" = 1L, "XML/JATS" = 2L, "HTML" = 3L)
best <- retrieved |> mutate(format_rank = unname(preference[filetype])) |> arrange(doi, format_rank) |>
  group_by(doi) |> slice(1) |> ungroup() |> select(-format_rank)
best_complete <- sample_tbl |> left_join(best, by = "doi") |> mutate(retrieved_any = !is.na(status) & status == "retrieved")
write_csv(best_complete, file.path(out_dir, "best_retrieval_per_doi.csv"))
coverage <- tibble(
  metric = c("sample_n", "retrieved_any", "retrieved_pdf", "retrieved_xml_jats", "retrieved_html"),
  n = c(nrow(sample_tbl), n_distinct(retrieved$doi), n_distinct(retrieved$doi[retrieved$filetype == "PDF"]),
        n_distinct(retrieved$doi[retrieved$filetype == "XML/JATS"]), n_distinct(retrieved$doi[retrieved$filetype == "HTML"]))
)
write_csv(coverage, file.path(out_dir, "coverage.csv"))
cat("\nCoverage:\n"); print(coverage)
cat("\nSource/filetype summary:\n"); print(summary)
