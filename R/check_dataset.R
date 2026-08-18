check_dataset <- function(path) {
  if (!file.exists(path)) stop('Dataset not found: ', path)

  # The master CSV contains heterogeneous fields, including free-text columns.
  # Read every field as character so legitimate text such as "It's clear these
  # are the study countries" cannot be misclassified as a logical value.
  x <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = 'minimal'
  )

  # At this point problems represent structural CSV failures (e.g. malformed
  # quoting or inconsistent field counts), rather than type inference noise.
  probs <- readr::problems(x)
  if (nrow(probs) > 0) {
    print(utils::head(probs, 20))
    stop('CSV contains ', nrow(probs), ' structural parsing problems; retrieval is blocked')
  }

  required <- c('record_id', 'title', 'doi', 'url_raw')
  missing <- setdiff(required, names(x))
  if (length(missing)) stop('Missing required columns: ', paste(missing, collapse=', '))
  if (nrow(x) == 0) stop('Dataset contains zero records')

  ids <- trimws(x$record_id)
  if (any(is.na(ids) | !nzchar(ids))) stop('record_id contains missing/blank values')
  if (anyDuplicated(ids)) stop('record_id is not unique')

  invisible(x)
}
