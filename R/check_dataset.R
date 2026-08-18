check_dataset <- function(path) {
  if (!file.exists(path)) stop('Dataset not found: ', path)
  x <- readr::read_csv(path, show_col_types=FALSE, progress=FALSE, name_repair='minimal')
  probs <- readr::problems(x)
  if (nrow(probs) > 0) {
    print(utils::head(probs, 20))
    stop('CSV contains ', nrow(probs), ' parsing problems; retrieval is blocked until the master dataset is clean')
  }
  required <- c('record_id','title','doi','url_raw')
  missing <- setdiff(required, names(x))
  if (length(missing)) stop('Missing required columns: ', paste(missing, collapse=', '))
  if (nrow(x) == 0) stop('Dataset contains zero records')
  ids <- trimws(as.character(x$record_id))
  if (any(!nzchar(ids) | is.na(ids))) stop('record_id contains missing/blank values')
  if (anyDuplicated(ids)) stop('record_id is not unique')
  invisible(x)
}
