#' Ensure all specified columns exist in a data frame
#'
#' This internal helper function checks whether all specified column names are present
#' in a data frame. If any requested columns are missing, they are automatically
#' created and filled with \code{NA}. The function then returns the data frame
#' restricted to the specified column order.
#'
#' This is useful when working with Wonde datasets or other API responses where
#' field availability can vary between schools. It ensures consistent column
#' structures across all resulting data frames.
#'
#' @param df A data frame to check and modify.
#' @param cols Character vector of expected column names.
#'
#' @return A data frame containing all requested columns (\code{cols}), in the same order,
#'   with any missing columns added and filled with \code{NA}.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = 1:3, b = 4:6)
#' ensure_columns(df, c("a", "b", "c"))
#' # Returns a data frame with columns a, b, c (column c filled with NA)
#' }
#'
#' @keywords internal
ensure_columns <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    for (col in missing) df[[col]] <- NA
  }
  df[, cols, drop = FALSE]
}
