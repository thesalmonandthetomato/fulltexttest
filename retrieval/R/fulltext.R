`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

normalise <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  trimws(x)
}

find_first_column <- function(nms, candidates) {
  hit <- nms[tolower(nms) %in% tolower(candidates)]
  if (length(hit)) hit[[1]] else NULL
}

read_master <- function(path) {
  if (!file.exists(path)) stop("Master file not found: ", path)
  out <- utils::read.csv(path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                          colClasses = "character", quote = "\"", na.strings = character(),
                          comment.char = "", fill = FALSE, strip.white = FALSE)
  if (!nrow(out) || !ncol(out)) stop("Master file contains no parsed records")
  out[] <- lapply(out, normalise)
  out
}

record_ids <- function(df, ids = "__FIRST3__") {
  rid_col <- find_first_column(names(df), c("record_id", "id"))
  if (is.null(rid_col)) stop("No record_id column found")
  if (identical(ids, "__FIRST3__")) {
    doi_col <- find_first_column(names(df), c("doi"))
    url_col <- find_first_column(names(df), c("url_raw", "url", "full_text_url", "source_url"))
    eligible <- nzchar(df[[rid_col]])
    if (!is.null(doi_col)) eligible <- eligible & nzchar(df[[doi_col]])
    if (!is.null(url_col)) eligible <- eligible | nzchar(df[[url_col]])
    idx <- which(eligible)
    return(df[[rid_col]][idx[seq_len(min(3L, length(idx)))]])
  }
  requested <- trimws(strsplit(ids, ",", fixed = TRUE)[[1]])
  requested[requested %in% df[[rid_col]]]
}

extract_urls <- function(x) {
  x <- normalise(x)
  if (!nzchar(x)) return(character())
  hits <- regmatches(x, gregexpr("https?://[^[:space:]<>\\\"]+", x, perl = TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  hits <- sub("[),.;]+$", "", hits)
  unique(hits[nzchar(hits)])
}

extract_dois <- function(x) {
  x <- normalise(x)
  if (!nzchar(x)) return(character())
  hits <- regmatches(x, gregexpr("(?i)(?:doi:\\s*)?10\\.\\d{4,9}/[-._;()/:a-z0-9]+", x, perl = TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  hits <- sub("(?i)^doi:\\s*", "", hits, perl = TRUE)
  hits <- sub("[),.;]+$", "", hits)
  unique(tolower(hits[nzchar(hits)]))
}

candidate_urls <- function(row) {
  out <- character()
  for (value in unname(row)) out <- c(out, extract_urls(value))
  dois <- character()
  for (value in unname(row)) dois <- c(dois, extract_dois(value))
  dois <- unique(dois[nzchar(dois)])
  if (length(dois)) out <- c(out, paste0("https://doi.org/", dois))
  unique(out[nzchar(out)])
}

safe_request <- function(url, timeout_seconds = 30L) {
  started <- Sys.time()
  res <- tryCatch({
    h <- curl::new_handle(useragent = "fulltexttest-clean-r/2.0", timeout = timeout_seconds, connecttimeout = min(15L, timeout_seconds), followlocation = TRUE, maxredirs = 10L)
    req <- curl::curl_fetch_memory(url, handle = h)
    list(ok = TRUE, status = req$status_code, type = req$type %||% "", body = req$content,
         final_url = req$url %||% url, error = "", elapsed = as.numeric(difftime(Sys.time(), started, units = "secs")))
  }, error = function(e) list(ok = FALSE, status = NA_integer_, type = "", body = raw(), final_url = url,
                              error = conditionMessage(e), elapsed = as.numeric(difftime(Sys.time(), started, units = "secs"))))
  res
}

strip_html <- function(x) {
  x <- gsub("<script[^>]*>.*?</script>", " ", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("<style[^>]*>.*?</style>", " ", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

validate_text <- function(text, format) {
  text <- gsub("\\s+", " ", text, perl = TRUE)
  lower <- tolower(text)
  refs <- regexpr("\\b(references|bibliography|literature cited)\\b", lower, perl = TRUE)[1] > 0
  reference_markers <- sum(lengths(regmatches(text, gregexpr("(?:\\[[0-9]{1,3}\\]|(?:^|\\s)[0-9]{1,3}\\.)", text, perl = TRUE))))
  chars <- nchar(text)
  ok <- chars >= 5000 && refs && reference_markers >= 3
  reason <- if (chars < 5000) "insufficient_text" else if (!refs) "no_reference_section" else if (reference_markers < 3) "few_reference_markers" else "complete"
  list(ok = ok, chars = chars, reference_markers = reference_markers, reason = reason, format = format)
}

parse_response <- function(body, content_type, url) {
  ct <- tolower(content_type %||% "")
  is_pdf <- grepl("application/pdf", ct, fixed = TRUE) || (length(body) >= 4 && identical(rawToChar(body[1:4]), "%PDF"))
  if (is_pdf) {
    f <- tempfile(fileext = ".pdf")
    writeBin(body, f)
    on.exit(unlink(f), add = TRUE)
    txt <- tryCatch(system2("pdftotext", c("-layout", shQuote(f), "-"), stdout = TRUE, stderr = FALSE), error = function(e) character())
    parsed <- paste(txt, collapse = "\n")
    v <- validate_text(parsed, "pdf")
    return(c(v, list(extension = ".pdf", parsed_text = parsed)))
  }
  raw_text <- tryCatch(rawToChar(body), error = function(e) "")
  if (grepl("xml", ct, fixed = TRUE)) {
    parsed <- strip_html(raw_text)
    v <- validate_text(parsed, "xml")
    return(c(v, list(extension = ".xml", parsed_text = parsed)))
  }
  if (grepl("html", ct, fixed = TRUE) || grepl("text/html", tolower(raw_text), fixed = TRUE)) {
    parsed <- strip_html(raw_text)
    v <- validate_text(parsed, "html")
    return(c(v, list(extension = ".html", parsed_text = parsed)))
  }
  list(ok = FALSE, chars = 0L, reference_markers = 0L, reason = "unsupported_response", format = "unknown", extension = ".bin", parsed_text = "")
}

run_one <- function(row, out_dir, timeout_seconds = 30L) {
  rid_col <- find_first_column(names(row), c("record_id", "id"))
  rid <- row[[rid_col]]
  urls <- candidate_urls(row)
  dir.create(file.path(out_dir, "documents"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "parsed"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "audit"), recursive = TRUE, showWarnings = FALSE)
  attempts <- list()
  winner <- NULL
  for (u in urls) {
    r <- safe_request(u, timeout_seconds)
    v <- if (r$ok && r$status >= 200 && r$status < 300) parse_response(r$body, r$type, r$final_url) else list(ok = FALSE, chars = 0L, reference_markers = 0L, reason = if (is.na(r$status)) paste0("request_error: ", r$error) else paste0("http_", r$status), format = "unknown", extension = ".bin", parsed_text = "")
    attempts[[length(attempts) + 1L]] <- data.frame(record_id = rid, candidate_url = u, final_url = r$final_url, status = r$status, content_type = r$type, bytes = length(r$body), verified_complete = isTRUE(v$ok), format = v$format, reason = v$reason, elapsed_seconds = r$elapsed, stringsAsFactors = FALSE)
    if (isTRUE(v$ok)) { winner <- list(url = u, response = r, validation = v); break }
  }
  audit <- if (length(attempts)) do.call(rbind, attempts) else data.frame(record_id = rid, candidate_url = NA_character_, final_url = NA_character_, status = NA_integer_, content_type = "", bytes = 0L, verified_complete = FALSE, format = "", reason = "no_candidate_urls", elapsed_seconds = 0, stringsAsFactors = FALSE)
  utils::write.csv(audit, file.path(out_dir, "audit", paste0(rid, "_attempts.csv")), row.names = FALSE, na = "")
  if (!is.null(winner)) {
    ext <- winner$validation$extension
    writeBin(winner$response$body, file.path(out_dir, "documents", paste0(rid, ext)))
    writeLines(winner$validation$parsed_text, file.path(out_dir, "parsed", paste0(rid, ".txt")), useBytes = TRUE)
    status <- data.frame(record_id = rid, full_text_status = "verified_complete", format = winner$validation$format, source_url = winner$response$final_url, text_chars = winner$validation$chars, reference_markers = winner$validation$reference_markers, stringsAsFactors = FALSE)
  } else status <- data.frame(record_id = rid, full_text_status = "unobtainable", format = "", source_url = "", text_chars = 0L, reference_markers = 0L, stringsAsFactors = FALSE)
  utils::write.csv(status, file.path(out_dir, "audit", paste0(rid, "_status.csv")), row.names = FALSE, na = "")
  invisible(status)
}
