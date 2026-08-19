# v9 fulltext baseline: retrieval plus document identity validation.
# Discovery is handled separately by additive_discovery_v4.R.

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
  out <- utils::read.csv(path, header=TRUE, stringsAsFactors=FALSE,
                         check.names=FALSE, colClasses="character", quote="\"",
                         na.strings=character(), comment.char="", fill=FALSE,
                         strip.white=FALSE)
  if (!nrow(out) || !ncol(out)) stop("Master file contains no parsed records")
  out[] <- lapply(out, normalise)
  out
}

record_ids <- function(df, ids="__FIRST3__") {
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
  requested <- trimws(strsplit(ids, ",", fixed=TRUE)[[1]])
  requested[requested %in% df[[rid_col]]]
}

extract_urls <- function(x) {
  x <- normalise(x)
  if (!nzchar(x)) return(character())
  hits <- regmatches(x, gregexpr("https?://[^[:space:]<>\\\"']+", x, perl=TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  pieces <- unlist(lapply(hits, function(h) {
    starts <- gregexpr("https?://", h, perl=TRUE)[[1]]
    if (length(starts) <= 1L) return(h)
    ends <- c(starts[-1L] - 1L, nchar(h))
    substring(h, starts, ends)
  }), use.names=FALSE)
  pieces <- sub("[),.;]+$", "", pieces)
  unique(pieces[nzchar(pieces)])
}

extract_dois <- function(x) {
  x <- normalise(x)
  if (!nzchar(x)) return(character())
  hits <- regmatches(x, gregexpr("(?i)(?:doi:\\s*)?10\\.\\d{4,9}/[-._;()/:a-z0-9]+", x, perl=TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  hits <- sub("(?i)^doi:\\s*", "", hits, perl=TRUE)
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

safe_request <- function(url, timeout_seconds=30L) {
  started <- Sys.time()
  tmp <- tempfile(fileext=".bin")
  hdr <- tempfile(fileext=".headers")
  meta <- tempfile(fileext=".meta")
  errf <- tempfile(fileext=".stderr")
  on.exit(unlink(c(tmp, hdr, meta, errf)), add=TRUE)

  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) {
    return(list(ok=FALSE, status=NA_integer_, type="", body=raw(),
                final_url=url, error="curl_not_found", elapsed=0))
  }

  args <- c("-L", "--silent", "--show-error", "--max-time", as.character(timeout_seconds),
            "--connect-timeout", "15", "--user-agent", "fulltexttest-clean-r/9.0",
            "-D", hdr, "-o", tmp, "-w", "%{http_code}\\t%{url_effective}", url)

  exit_code <- suppressWarnings(system2(curl_bin, args, stdout=meta, stderr=errf))
  body <- if (file.exists(tmp)) readBin(tmp, "raw", n=file.info(tmp)$size) else raw()
  meta_txt <- if (file.exists(meta)) paste(readLines(meta, warn=FALSE), collapse="") else ""
  final_url <- url
  code <- NA_integer_

  if (nzchar(meta_txt)) {
    bits <- strsplit(meta_txt, "\\t", fixed=FALSE)[[1]]
    if (length(bits)) code <- suppressWarnings(as.integer(bits[[1]]))
    if (length(bits) >= 2L && nzchar(bits[[2]])) final_url <- bits[[2]]
  }

  headers <- if (file.exists(hdr)) readLines(hdr, warn=FALSE) else character()
  if (is.na(code)) {
    status_line <- tail(grep("^HTTP/", headers, value=TRUE), 1L)
    if (length(status_line)) code <- suppressWarnings(as.integer(sub("^HTTP/[0-9.]+\\s+([0-9]+).*$", "\\1", status_line)))
  }

  ct_lines <- grep("^Content-Type:", headers, ignore.case=TRUE, value=TRUE)
  content_type <- if (length(ct_lines)) trimws(sub("^Content-Type:\\s*", "", tail(ct_lines, 1L), ignore.case=TRUE)) else ""
  errtxt <- if (file.exists(errf)) paste(readLines(errf, warn=FALSE), collapse=" ") else ""
  error <- if (exit_code != 0L) paste0("curl_exit_", exit_code, if (nzchar(errtxt)) paste0(":", errtxt) else "") else ""

  list(ok=!is.na(code) && code >= 200L && code < 300L && length(body) > 0L,
       status=code, type=content_type, body=body, final_url=final_url,
       error=error, elapsed=as.numeric(difftime(Sys.time(), started, units="secs")))
}

strip_html <- function(x) {
  x <- gsub("(?is)<script[^>]*>.*?</script>", " ", x, perl=TRUE)
  x <- gsub("(?is)<style[^>]*>.*?</style>", " ", x, perl=TRUE)
  x <- gsub("(?is)<noscript[^>]*>.*?</noscript>", " ", x, perl=TRUE)
  x <- gsub("<[^>]+>", " ", x, perl=TRUE)
  x <- gsub("&nbsp;", " ", x, fixed=TRUE)
  x <- gsub("&amp;", "&", x, fixed=TRUE)
  x <- gsub("&lt;", "<", x, fixed=TRUE)
  x <- gsub("&gt;", ">", x, fixed=TRUE)
  x <- gsub("\\s+", " ", x, perl=TRUE)
  trimws(x)
}

normalise_title <- function(x) {
  x <- tolower(normalise(x))
  x <- gsub("[^[:alnum:]]+", " ", x, perl=TRUE)
  x <- gsub("\\s+", " ", x, perl=TRUE)
  trimws(x)
}

title_score <- function(expected, observed) {
  a <- unlist(strsplit(normalise_title(expected), " ", fixed=TRUE))
  b <- unlist(strsplit(normalise_title(observed), " ", fixed=TRUE))
  a <- unique(a[nchar(a) >= 3L]); b <- unique(b[nchar(b) >= 3L])
  if (!length(a) || !length(b)) return(0)
  sum(a %in% b) / length(a)
}

extract_html_metadata <- function(raw_text) {
  title <- character(); doi <- character()
  pats <- c(
    "(?is)<meta[^>]+(?:name|property)=[\\\"'](?:citation_title|og:title|dc.title)[\\\"'][^>]+content=[\\\"']([^\\\"']+)[\\\"']",
    "(?is)<meta[^>]+content=[\\\"']([^\\\"']+)[\\\"'][^>]+(?:name|property)=[\\\"'](?:citation_title|og:title|dc.title)[\\\"']"
  )
  for (p in pats) {
    m <- regmatches(raw_text, gregexpr(p, raw_text, perl=TRUE))[[1]]
    if (length(m)) { title <- c(title, sub("(?is)^.*content=[\\\"']([^\\\"']+)[\\\"'].*$", "\\1", m, perl=TRUE)) }
  }
  m <- regmatches(raw_text, gregexpr("(?is)<title[^>]*>(.*?)</title>", raw_text, perl=TRUE))[[1]]
  if (length(m)) title <- c(title, sub("(?is)^.*<title[^>]*>(.*?)</title>.*$", "\\1", m, perl=TRUE))
  doi <- extract_dois(raw_text)
  list(title=if(length(title)) title[[1]] else "", doi=if(length(doi)) doi[[1]] else "")
}

identity_check <- function(expected_title, expected_doi, observed_title, observed_doi, final_url, parsed_text) {
  edoi <- tolower(sub("^https?://doi.org/", "", normalise(expected_doi)))
  candidates <- tolower(c(observed_doi, extract_dois(final_url), head(extract_dois(substr(parsed_text, 1L, 20000L)), 5L)))
  candidates <- candidates[nzchar(candidates)]
  if (nzchar(edoi) && length(candidates) && any(candidates == edoi, fixed=TRUE)) {
    return(list(ok=TRUE, method="doi", score=1, observed_title=observed_title, observed_doi=edoi))
  }
  score <- title_score(expected_title, observed_title)
  if (score < 0.70) score <- max(score, title_score(expected_title, substr(parsed_text, 1L, 5000L)))
  list(ok=isTRUE(score >= 0.70), method=if(score >= 0.70) "title" else "none", score=score,
       observed_title=observed_title, observed_doi=if(length(candidates)) candidates[[1]] else "")
}

validate_text <- function(text, format) {
  text <- gsub("\\s+", " ", text, perl=TRUE)
  lower <- tolower(text)
  refs <- regexpr("\\b(references|bibliography|literature cited)\\b", lower, perl=TRUE)[1] > 0
  ref_matches <- gregexpr("(?:\\[[0-9]{1,3}\\]|(?:^|\\s)[0-9]{1,3}\\.)", text, perl=TRUE)
  reference_markers <- sum(lengths(regmatches(text, ref_matches)))
  chars <- nchar(text)
  ok <- chars >= 5000 && refs && reference_markers >= 3
  reason <- if (chars < 5000) "insufficient_text" else if (!refs) "no_reference_section" else if (reference_markers < 3) "few_reference_markers" else "complete"
  list(ok=ok, chars=chars, reference_markers=reference_markers, reason=reason, format=format)
}

parse_response <- function(body, content_type, url, expected_title, expected_doi) {
  ct <- tolower(content_type %||% "")
  is_pdf <- grepl("application/pdf", ct, fixed=TRUE) || (length(body) >= 4 && identical(rawToChar(body[1:4]), "%PDF"))
  observed_title <- ""; observed_doi <- ""

  if (is_pdf) {
    f <- tempfile(fileext=".pdf")
    writeBin(body, f); on.exit(unlink(f), add=TRUE)
    txt <- tryCatch(system2("pdftotext", c("-layout", f, "-"), stdout=TRUE, stderr=FALSE), error=function(e) character())
    parsed <- paste(txt, collapse="\n")
    v <- validate_text(parsed, "pdf")
    observed_doi <- if(length(extract_dois(substr(parsed,1L,20000L)))) extract_dois(substr(parsed,1L,20000L))[[1]] else ""
    id <- identity_check(expected_title, expected_doi, "", observed_doi, url, parsed)
    v$identity_ok <- isTRUE(id$ok); v$identity_method <- id$method; v$identity_score <- id$score
    v$observed_title <- ""; v$observed_doi <- observed_doi
    v$ok <- isTRUE(v$ok) && isTRUE(id$ok)
    if (!id$ok && v$reason == "complete") v$reason <- "wrong_document"
    return(c(v, list(extension=".pdf", parsed_text=parsed)))
  }

  raw_text <- tryCatch(rawToChar(body), error=function(e) "")
  meta <- extract_html_metadata(raw_text)
  observed_title <- meta$title; observed_doi <- meta$doi
  if (grepl("html", ct, fixed=TRUE) || grepl("<html|<body|<title", raw_text, ignore.case=TRUE, perl=TRUE)) {
    parsed <- strip_html(raw_text)
    v <- validate_text(parsed, "html")
    id <- identity_check(expected_title, expected_doi, observed_title, observed_doi, url, parsed)
    v$identity_ok <- isTRUE(id$ok); v$identity_method <- id$method; v$identity_score <- id$score
    v$observed_title <- observed_title; v$observed_doi <- if(nzchar(observed_doi)) observed_doi else id$observed_doi
    v$ok <- isTRUE(v$ok) && isTRUE(id$ok)
    if (!id$ok && v$reason == "complete") v$reason <- "wrong_document"
    return(c(v, list(extension=".html", parsed_text=parsed)))
  }

  list(ok=FALSE, chars=0L, reference_markers=0L, reason="unsupported_response", format="unknown", extension=".bin", parsed_text="", identity_ok=FALSE, identity_method="none", identity_score=0, observed_title="", observed_doi="")
}

run_one <- function(row, out_dir, timeout_seconds=30L) {
  rid_col <- find_first_column(names(row), c("record_id", "id")); rid <- row[[rid_col]]
  title_col <- find_first_column(names(row), c("title")); expected_title <- if(!is.null(title_col)) row[[title_col]] else ""
  doi_col <- find_first_column(names(row), c("doi")); expected_doi <- if(!is.null(doi_col)) row[[doi_col]] else ""
  urls <- candidate_urls(row)

  dir.create(file.path(out_dir, "documents"), recursive=TRUE, showWarnings=FALSE)
  dir.create(file.path(out_dir, "parsed"), recursive=TRUE, showWarnings=FALSE)
  dir.create(file.path(out_dir, "audit"), recursive=TRUE, showWarnings=FALSE)

  attempts <- list(); winner <- NULL
  for (u in urls) {
    r <- safe_request(u, timeout_seconds)
    v <- if (r$ok) parse_response(r$body, r$type, r$final_url, expected_title, expected_doi) else list(
      ok=FALSE, chars=0L, reference_markers=0L,
      reason=if (is.na(r$status)) paste0("request_error: ", r$error) else paste0("http_", r$status),
      format="unknown", extension=".bin", parsed_text="", identity_ok=FALSE,
      identity_method="none", identity_score=0, observed_title="", observed_doi=""
    )
    attempts[[length(attempts)+1L]] <- data.frame(
      record_id=rid, expected_title=expected_title, expected_doi=expected_doi,
      candidate_url=u, final_url=r$final_url, status=r$status, content_type=r$type,
      bytes=length(r$body), content_complete=isTRUE(v$ok) || (isTRUE(v$chars >= 5000) && v$reason == "complete"),
      identity_ok=isTRUE(v$identity_ok), identity_method=v$identity_method, identity_score=v$identity_score,
      observed_title=v$observed_title, observed_doi=v$observed_doi,
      verified_complete=isTRUE(v$ok), format=v$format, reason=v$reason, elapsed_seconds=r$elapsed,
      stringsAsFactors=FALSE
    )
    if (isTRUE(v$ok)) { winner <- list(url=u, response=r, validation=v); break }
  }

  audit <- if(length(attempts)) do.call(rbind, attempts) else data.frame(record_id=rid, expected_title=expected_title, expected_doi=expected_doi, candidate_url=NA_character_, final_url=NA_character_, status=NA_integer_, content_type="", bytes=0L, content_complete=FALSE, identity_ok=FALSE, identity_method="none", identity_score=0, observed_title="", observed_doi="", verified_complete=FALSE, format="", reason="no_candidate_urls", elapsed_seconds=0, stringsAsFactors=FALSE)
  utils::write.csv(audit, file.path(out_dir, "audit", paste0(rid, "_attempts.csv")), row.names=FALSE, na="")

  if(!is.null(winner)) {
    ext <- winner$validation$extension
    writeBin(winner$response$body, file.path(out_dir, "documents", paste0(rid, ext)))
    writeLines(winner$validation$parsed_text, file.path(out_dir, "parsed", paste0(rid, ".txt")), useBytes=TRUE)
    data.frame(record_id=rid, full_text_status="verified_complete", format=winner$validation$format,
               source_url=winner$response$final_url, text_chars=winner$validation$chars,
               reference_markers=winner$validation$reference_markers, identity_method=winner$validation$identity_method,
               identity_score=winner$validation$identity_score, observed_title=winner$validation$observed_title,
               observed_doi=winner$validation$observed_doi, stringsAsFactors=FALSE)
  } else data.frame(record_id=rid, full_text_status="unobtainable", format="", source_url="", text_chars=0L, reference_markers=0L,
                    identity_method="none", identity_score=0, observed_title="", observed_doi="", stringsAsFactors=FALSE)
}