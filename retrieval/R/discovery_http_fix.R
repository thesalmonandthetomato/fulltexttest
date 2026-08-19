# Discovery HTTP client. Uses system2 with an explicit curl binary and a
# response file for the HTTP status, avoiding shell interpretation of '&'.
.disc_request <- function(url, timeout = 25L) {
  urls <- tryCatch(.disc_urls(url), error=function(e) character())
  url <- if (length(urls)) urls[[1L]] else ""
  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) return(list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url="",error="curl_not_found",elapsed=0,curl_path=""))
  if (!nzchar(url)) return(list(ok=FALSE,status=NA_integer_,type="",body=raw(),final_url="",error="invalid_url",elapsed=0,curl_path=curl_bin))
  body_file <- tempfile(fileext=".bin"); header_file <- tempfile(fileext=".headers"); meta_file <- tempfile(fileext=".meta"); stderr_file <- tempfile(fileext=".stderr")
  on.exit(unlink(c(body_file,header_file,meta_file,stderr_file)), add=TRUE)
  args <- c("-L", "--silent", "--show-error", "--max-time", as.character(timeout),
            "--connect-timeout", "10", "--user-agent", "Mozilla/5.0 fulltexttest-discovery/6.0",
            "-D", header_file, "-o", body_file, "-w", "%{http_code}\\t%{url_effective}", url)
  started <- Sys.time()
  exit <- suppressWarnings(system2(curl_bin, args, stdout=meta_file, stderr=stderr_file))
  body <- if (file.exists(body_file)) readBin(body_file, "raw", n=file.info(body_file)$size) else raw()
  meta <- if (file.exists(meta_file)) paste(readLines(meta_file, warn=FALSE), collapse="") else ""
  headers <- if (file.exists(header_file)) readLines(header_file, warn=FALSE) else character()
  status <- NA_integer_; effective <- url
  if (nzchar(meta)) {
    bits <- strsplit(meta, "\\t", fixed=FALSE)[[1L]]
    if (length(bits)) status <- suppressWarnings(as.integer(bits[[1L]]))
    if (length(bits) >= 2L && nzchar(bits[[2L]])) effective <- bits[[2L]]
  }
  if (is.na(status)) {
    hs <- tail(grep("^HTTP/", headers, value=TRUE), 1L)
    if (length(hs)) status <- suppressWarnings(as.integer(sub("^HTTP/[0-9.]+\\s+([0-9]+).*$", "\\1", hs)))
  }
  ct <- tail(grep("^Content-Type:", headers, ignore.case=TRUE, value=TRUE), 1L)
  type <- if (length(ct)) trimws(sub("^Content-Type:\\s*", "", ct, ignore.case=TRUE)) else ""
  errtxt <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn=FALSE), collapse=" ") else ""
  err <- if (exit != 0L) paste0("curl_exit_", exit, if(nzchar(errtxt)) paste0(":",errtxt) else "") else ""
  list(ok=!is.na(status) && status >= 200L && status < 300L, status=status, type=type,
       body=body, final_url=effective, error=err,
       elapsed=as.numeric(difftime(Sys.time(),started,units="secs")), curl_path=curl_bin)
}
