# Discovery HTTP client fix.
# Must be sourced after additive_discovery_v3.R so the discovery layer has a
# reliable curl executable and records transport failures separately.
.disc_request <- function(url, timeout = 25L) {
  url <- .disc_urls(url)[1L]
  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) return(list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url="",error="curl_not_found",elapsed=0,curl_path=""))
  if (is.na(url) || !nzchar(url)) return(list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url="",error="invalid_url",elapsed=0,curl_path=curl_bin))
  body_file <- tempfile(fileext=".bin"); header_file <- tempfile(fileext=".headers")
  on.exit(unlink(c(body_file, header_file)), add=TRUE)
  args <- c("-L", "--silent", "--show-error", "--max-time", as.character(timeout),
            "--connect-timeout", "10", "--user-agent", "Mozilla/5.0 fulltexttest-discovery/5.0",
            "-D", header_file, "-o", body_file, "-w", "%{http_code}", url)
  started <- Sys.time()
  stderr_file <- tempfile(fileext=".stderr")
  on.exit(unlink(stderr_file), add=TRUE)
  exit <- suppressWarnings(system2(curl_bin, args, stdout=stderr_file, stderr=stderr_file))
  body <- if (file.exists(body_file)) readBin(body_file, "raw", n=file.info(body_file)$size) else raw()
  headers <- if (file.exists(header_file)) readLines(header_file, warn=FALSE) else character()
  hs <- tail(grep("^HTTP/", headers, value=TRUE), 1L)
  status <- if (length(hs)) suppressWarnings(as.integer(sub("^HTTP/[0-9.]+\\s+([0-9]+).*$", "\\1", hs))) else NA_integer_
  ct <- tail(grep("^Content-Type:", headers, ignore.case=TRUE, value=TRUE), 1L)
  type <- if (length(ct)) trimws(sub("^Content-Type:\\s*", "", ct, ignore.case=TRUE)) else ""
  loc <- tail(grep("^Location:", headers, ignore.case=TRUE, value=TRUE), 1L)
  final <- if (length(loc)) sub("^Location:\\s*", "", loc, ignore.case=TRUE) else url
  errtxt <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn=FALSE), collapse=" ") else ""
  err <- if (exit != 0L) paste0("curl_exit_", exit, if(nzchar(errtxt)) paste0(":", errtxt) else "") else ""
  list(ok=!is.na(status) && status >= 200L && status < 300L,
       status=status, type=type, body=body, final_url=final, error=err,
       elapsed=as.numeric(difftime(Sys.time(), started, units="secs")), curl_path=curl_bin)
}
