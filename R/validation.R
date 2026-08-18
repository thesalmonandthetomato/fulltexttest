validate_pdf <- function(bytes, pdftotext = Sys.which('pdftotext')) {
  if (length(bytes) < 4L || rawToChar(bytes[1:4]) != '%PDF') return(list(ok=FALSE, format='unknown', reason='not_pdf'))
  if (!nzchar(pdftotext)) return(list(ok=FALSE, format='pdf', reason='pdftotext_unavailable'))
  f <- tempfile(fileext='.pdf'); on.exit(unlink(f), add=TRUE); writeBin(bytes, f)
  txt <- tryCatch(system2(pdftotext, c('-layout', f, '-'), stdout=TRUE, stderr=FALSE), error=function(e) character())
  txt <- paste(txt, collapse='\n')
  low <- tolower(txt)
  refs <- grepl('(^|\n)\\s*(references|bibliography|literature cited)\\s*($|:)', low, perl=TRUE)
  nref <- length(unlist(regmatches(txt, gregexpr('(?m)\\n\\s*(\\[[0-9]{1,4}\\]|[0-9]{1,4}\\.)\\s+', txt, perl=TRUE))))
  list(ok=nchar(txt) >= 3000 && refs && nref >= 3, format='pdf', reason=ifelse(nchar(txt)<3000,'too_little_text',ifelse(!refs,'no_references_section',ifelse(nref<3,'too_few_references','complete'))), text_chars=nchar(txt), references=nref)
}

validate_markup <- function(bytes, format) {
  txt <- tryCatch(rawToChar(bytes), error=function(e) '')
  plain <- gsub('\\s+', ' ', gsub('<[^>]+>', ' ', txt))
  low <- tolower(plain)
  refs <- grepl('references|bibliography|literature cited', low)
  nref <- length(unlist(regmatches(plain, gregexpr('(references|bibliography).*?([0-9]{1,4}\\.|\\[[0-9]{1,4}\\])', low, perl=TRUE))))
  list(ok=nchar(plain) >= 5000 && refs && nref >= 3, format=format, reason=ifelse(nchar(plain)<5000,'too_little_text',ifelse(!refs,'no_references_section',ifelse(nref<3,'too_few_references','complete'))), text_chars=nchar(plain), references=nref)
}

validate_document <- function(bytes, content_type='') {
  ct <- tolower(content_type)
  if (grepl('pdf', ct) || (length(bytes)>=4L && rawToChar(bytes[1:4]) == '%PDF')) return(validate_pdf(bytes))
  if (grepl('xml', ct)) return(validate_markup(bytes, 'xml'))
  if (grepl('html', ct)) return(validate_markup(bytes, 'html'))
  list(ok=FALSE, format='unknown', reason='unsupported_content_type')
}
