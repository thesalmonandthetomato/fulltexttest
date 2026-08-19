# Network safety overrides for additive discovery.
# Discovery must never leak curl diagnostics into the workflow log and must
# treat HTTP errors as structured audit outcomes rather than fatal failures.

.discovery_request <- function(url, timeout = 30L, binary = FALSE) {
  url <- .strict_url(url)
  if (is.na(url)) {
    return(list(ok = FALSE, status = NA_integer_, type = "", body = raw(),
                final_url = "", error = "invalid_url", elapsed = 0))
  }

  tmp <- tempfile(fileext = if (binary) ".bin" else ".txt")
  hdr <- tempfile(fileext = ".headers")
  err <- tempfile(fileext = ".stderr")
  on.exit(unlink(c(tmp, hdr, err)), add = TRUE)

  args <- c(
    "-L", "--silent", "--show-error", "--max-time", as.character(timeout),
    "--connect-timeout", "15", "--retry", "0", "--user-agent",
    "Mozilla/5.0 (compatible; fulltexttest-discovery/2.1)",
    "-D", hdr, "-o", tmp, url
  )

  started <- Sys.time()
  status <- suppressWarnings(system2("curl", args, stdout = FALSE, stderr = err))
  body <- if (file.exists(tmp)) readBin(tmp, "raw", n = file.info(tmp)$size) else raw()
  headers <- if (file.exists(hdr)) readLines(hdr, warn = FALSE) else character()
  stderr_text <- if (file.exists(err)) paste(readLines(err, warn = FALSE), collapse = " ") else ""

  hs <- tail(grep("^HTTP/", headers, value = TRUE), 1L)
  code <- if (length(hs)) suppressWarnings(as.integer(sub("^HTTP/[0-9.]+\\s+([0-9]+).*$", "\\1", hs))) else NA_integer_
  ct <- tail(grep("^Content-Type:", headers, ignore.case = TRUE, value = TRUE), 1L)
  ctype <- if (length(ct)) trimws(sub("^Content-Type:\\s*", "", ct, ignore.case = TRUE)) else ""
  loc <- tail(grep("^Location:", headers, ignore.case = TRUE, value = TRUE), 1L)
  final <- if (length(loc)) .strict_url(sub("^Location:\\s*", "", loc, ignore.case = TRUE)) else url

  # HTTP status is authoritative when available. curl's exit status is only
  # used when there is no HTTP response at all (DNS, timeout, malformed URL).
  ok <- !is.na(code) && code >= 200L && code < 300L
  if (ok) {
    err_class <- ""
  } else if (!is.na(code)) {
    err_class <- paste0("http_", code)
  } else if (status != 0L) {
    err_class <- paste0("curl_exit_", status)
  } else {
    err_class <- "no_http_response"
  }

  list(ok = ok, status = code, type = ctype, body = body,
       final_url = if (!is.na(final) && nzchar(final)) final else url,
       error = err_class, curl_stderr = stderr_text,
       elapsed = as.numeric(difftime(Sys.time(), started, units = "secs")))
}
