# v10 identity overrides: mandatory document identity for every accepted candidate.

pdf_observed_title <- function(parsed, expected_title) {
  lines <- trimws(unlist(strsplit(parsed, "\n", fixed=TRUE)))
  lines <- lines[nzchar(lines) & nchar(lines) >= 12L & nchar(lines) <= 240L]
  if (!length(lines) || !nzchar(normalise(expected_title))) return("")
  lines <- lines[seq_len(min(length(lines), 80L))]
  scores <- vapply(lines, function(x) title_score(expected_title, x), numeric(1))
  i <- which.max(scores)
  if (length(i) && scores[i] >= 0.45) lines[i] else ""
}

identity_check <- function(expected_title, expected_doi, observed_title, observed_doi, final_url, parsed_text) {
  edoi <- tolower(normalise(expected_doi))
  edoi <- sub("^https?://doi.org/", "", edoi, ignore.case=TRUE)
  if (nzchar(edoi)) {
    candidates <- unique(tolower(c(extract_dois(final_url), extract_dois(observed_doi), extract_dois(substr(parsed_text, 1L, 20000L)))))
    candidates <- candidates[nzchar(candidates)]
    if (any(candidates == edoi, fixed=TRUE)) return(list(ok=TRUE, method="doi", score=1, observed_title=observed_title, observed_doi=edoi))
  }
  score <- title_score(expected_title, observed_title)
  if (score < 0.85) score <- max(score, title_score(expected_title, substr(parsed_text, 1L, 1500L)))
  list(ok=isTRUE(score >= 0.85), method=if(score >= 0.85) "title" else "none", score=score,
       observed_title=observed_title, observed_doi="")
}

parse_response <- function(body, content_type, url, expected_title, expected_doi) {
  ct <- tolower(content_type %||% "")
  is_pdf <- grepl("application/pdf", ct, fixed=TRUE) || (length(body) >= 4 && identical(rawToChar(body[1:4]), "%PDF"))
  if (is_pdf) {
    f <- tempfile(fileext=".pdf"); writeBin(body, f); on.exit(unlink(f), add=TRUE)
    txt <- tryCatch(system2("pdftotext", c("-layout", f, "-"), stdout=TRUE, stderr=FALSE), error=function(e) character())
    parsed <- paste(txt, collapse="\n")
    v <- validate_text(parsed, "pdf")
    observed_title <- pdf_observed_title(parsed, expected_title)
    id <- identity_check(expected_title, expected_doi, observed_title, "", url, parsed)
    v$identity_ok <- isTRUE(id$ok); v$identity_method <- id$method; v$identity_score <- id$score
    v$observed_title <- observed_title; v$observed_doi <- id$observed_doi
    v$ok <- isTRUE(v$ok) && isTRUE(id$ok)
    if (!id$ok && v$reason == "complete") v$reason <- "wrong_document"
    return(c(v, list(extension=".pdf", parsed_text=parsed)))
  }
  raw_text <- tryCatch(rawToChar(body), error=function(e) "")
  meta <- extract_html_metadata(raw_text)
  observed_title <- meta$title; observed_doi <- meta$doi
  if (grepl("html|xml", ct) || grepl("<html|<body|<title", raw_text, ignore.case=TRUE, perl=TRUE)) {
    parsed <- strip_html(raw_text)
    v <- validate_text(parsed, if(grepl("xml", ct)) "xml" else "html")
    id <- identity_check(expected_title, expected_doi, observed_title, observed_doi, url, parsed)
    v$identity_ok <- isTRUE(id$ok); v$identity_method <- id$method; v$identity_score <- id$score
    v$observed_title <- observed_title; v$observed_doi <- id$observed_doi
    v$ok <- isTRUE(v$ok) && isTRUE(id$ok)
    if (!id$ok && v$reason == "complete") v$reason <- "wrong_document"
    return(c(v, list(extension=if(grepl("xml", ct)) ".xml" else ".html", parsed_text=parsed)))
  }
  list(ok=FALSE, chars=0L, reference_markers=0L, reason="unsupported_response", format="unknown", extension=".bin", parsed_text="", identity_ok=FALSE, identity_method="none", identity_score=0, observed_title="", observed_doi="")
}
