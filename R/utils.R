#' Format Geographic Identifiers
#'
#' @param data_frame Data frame to format
#' @param geod_col Column name for geographic identifier
#' @return Data frame with formatted geographic IDs
#'
#' @keywords internal
format_geographic_ids <- function(data_frame, geod_col) {
  geod_numeric <- as.numeric(data_frame[[geod_col]])
  data_frame[[geod_col]] <- sprintf("%05d", geod_numeric)
  return(data_frame)
}