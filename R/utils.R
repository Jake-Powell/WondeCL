
#' Combine data frames with non-identical columns
#'
#' @description
#' Safely combines a list of data frames that may have different column names,
#' missing columns, or contain NULL or empty elements.
#' Any missing columns are added to each data frame (filled with `NA`)
#' before applying `rbind()`. NULL and empty data frames are skipped automatically.
#'
#' @param list_of_data.frames A list of data frames to combine. NULL or zero-row
#'   data frames will be ignored.
#' @param warn_on_skip Logical, default `TRUE`. If `TRUE`, prints informative
#'   messages when NULL or empty data frames are skipped.
#'
#' @return
#' A single data frame containing all rows and all unique columns from
#' the input list. Missing columns are filled with `NA`.
#' If no valid data frames are present, returns an empty data frame.
#'
#' @details
#' This function identifies all unique column names across all data frames in
#' `list_of_data.frames`, ensures each has those columns (adding any missing
#' columns filled with `NA`), and then performs a row bind.
#'
#' The function will:
#' \itemize{
#'   \item Remove NULL elements before combining.
#'   \item Remove data frames with zero rows.
#'   \item Optionally print a message for each skipped element when
#'         `warn_on_skip = TRUE`.
#'   \item Return an empty data frame if nothing valid remains.
#' }
#'
#' @examples
#' df1 <- data.frame(a = 1:3, b = 4:6)
#' df2 <- data.frame(a = 7:8, c = c("x", "y"))
#' df3 <- NULL
#'
#' result <- rbind_aggro(list(df1, df2, df3))
#' print(result)
#'
#' @export
rbind_aggro <- function(list_of_data.frames, warn_on_skip = TRUE) {
  # ----------------------------------------------------------------
  # 1. Remove NULL elements
  # ----------------------------------------------------------------
  null_flags <- sapply(list_of_data.frames, is.null)
  null_count <- sum(null_flags)
  if (null_count > 0 && warn_on_skip) {
    message("rbind_aggro: Skipping ", null_count, " NULL data frame(s).")
  }
  list_of_data.frames <- list_of_data.frames[!null_flags]
  
  # ----------------------------------------------------------------
  # 2. Remove zero-row data frames (if any)
  # ----------------------------------------------------------------
  empty_flags <- sapply(list_of_data.frames, function(x) nrow(x) == 0)
  empty_count <- sum(empty_flags)
  if (empty_count > 0 && warn_on_skip) {
    message("rbind_aggro: Skipping ", empty_count, " empty data frame(s).")
  }
  list_of_data.frames <- list_of_data.frames[!empty_flags]
  
  # ----------------------------------------------------------------
  # 3. Handle edge cases
  # ----------------------------------------------------------------
  if (length(list_of_data.frames) == 0) {
    if (warn_on_skip) message("rbind_aggro: No data frames to combine. Returning empty data frame.")
    return(data.frame())
  }
  
  if (length(list_of_data.frames) == 1) {
    return(list_of_data.frames[[1]])
  }
  
  # ----------------------------------------------------------------
  # 4. Get all unique column names across all data frames
  # ----------------------------------------------------------------
  all_fields <- unique(unlist(lapply(list_of_data.frames, names)))
  
  # ----------------------------------------------------------------
  # 5. Standardize columns and order
  # ----------------------------------------------------------------
  data_frame_format <- lapply(list_of_data.frames, function(df) {
    missing_names <- setdiff(all_fields, names(df))
    if (length(missing_names) > 0) {
      for (name in missing_names) {
        df[[name]] <- NA
      }
    }
    df <- df[, match(all_fields, names(df)), drop = FALSE]
    df
  })
  
  # ----------------------------------------------------------------
  # 6. Combine and return
  # ----------------------------------------------------------------
  combined_df <- do.call(rbind, data_frame_format)
  rownames(combined_df) <- NULL
  return(combined_df)
}


#' Convert list-column elements into data frames
#'
#' @description
#' Ensures that all elements within a specified list-column of a data frame
#' are valid data frames.  
#' This function is particularly useful when working with nested data
#' structures returned by APIs or list-based imports (such as Wonde data),
#' where some entries may contain empty lists instead of empty data frames.
#'
#' Each element of the specified column is checked; if the element is an empty
#' list, it is replaced with an empty data frame. This helps prevent downstream
#' errors in functions that expect every list element to be a data frame.
#'
#' @param data A data frame containing at least one list-column to process.
#' @param column_to_unnest Character string giving the name of the column
#'   whose elements should be checked and converted.
#'
#' @return
#' The input \code{data} with the specified column modified such that any empty
#' list elements are replaced by empty data frames.
#'
#' @details
#' This function is typically used as a preprocessing step to standardize
#' nested Wonde API outputs or other hierarchical data sources prior to
#' unnesting, binding, or summarising operations.
#'
#' @examples
#' \dontrun{
#' # Example with an empty list element
#' df <- data.frame(info = I(list(data.frame(a = 1), list(), data.frame(a = 2))))
#' cleaned <- convert_list_element_to_df(df, "info")
#' str(cleaned$info)
#' }
#'
#' @export
convert_list_element_to_df <- function(data, column_to_unnest) {
  for (i in seq_len(nrow(data))) {
    val <- data[[column_to_unnest]][i][[1]]
    if (is.list(val) && length(val) == 0) {
      data[[column_to_unnest]][i][[1]] <- data.frame()
    }
  }
  data
}



#' Combine two Wonde raw data lists element-wise
#'
#' @description
#' Merges two lists of raw Wonde school datasets (\code{a} and \code{b}) into a
#' single combined list.  
#' This is typically used to append newly retrieved school data (e.g., additional
#' Wonde API extracts) to an existing dataset that was pulled earlier.
#'
#' Each element of list \code{b} is added to list \code{a} using its name as the
#' key. If a name already exists in \code{a}, it will be overwritten by the
#' corresponding element from \code{b}.
#'
#' @param a A named list of Wonde raw datasets, usually the original data pulled
#'   via \code{\link{get_primary_school_student_data}()} or
#'   \code{\link{get_secondary_school_student_data}()}.
#' @param b A second named list of Wonde raw datasets to be appended or merged
#'   into \code{a}.
#'
#' @return
#' A single combined list containing all elements from both \code{a} and
#' \code{b}.  
#' The resulting structure is suitable for downstream processing functions such
#' as \code{\link{extract_class_list}()} or \code{\link{summarise_school_data}()}.
#'
#' @details
#' This function is commonly used when some schools are pulled from the Wonde API
#' at a later time (for example, schools missing from the original retrieval).  
#' Instead of re-pulling all data, you can append the new raw extracts directly
#' using this helper.  
#'
#' The combination is done by name: each element in \code{b} is added to
#' \code{a} under its existing name (usually the Wonde school ID). If duplicate
#' names are present, entries from \code{b} will overwrite those in \code{a}.
#'
#' @examples
#' \dontrun{
#' # Original Wonde data
#' raw_data_1 <- get_secondary_school_student_data(school_ids = c("A100001", "A100002"))
#'
#' # Later, new schools become available
#' raw_data_2 <- get_secondary_school_student_data(school_ids = c("A100003", "A100004"))
#'
#' # Combine both into one object
#' combined_raw <- combine_raw_data(raw_data_1, raw_data_2)
#'
#' # The result can then be used in extract_class_list()
#' class_list <- extract_class_list(WondeData = combined_raw, ...)
#' }
#'
#' @export
combine_raw_data <- function(a, b) {
  for (i in seq_along(b)) {
    a[[names(b)[i]]] <- b[[i]]
  }
  return(a)
}


#' Clean names
#'
#' @param x name to clean
#' @param rm_whit Flag (TRUE/FALSE) for whether whitespace wants to be removed from the names
#'
#' @return cleaned name
#' @export
#'
#' @examples
#'  clean_name('John Leonard Knapp')
#'  clean_name('Pierre-Joseph Redouté')
#'
#' @details
#' "Cleans" name by changing to lower case and translating to Latin-ASCII (i.e removing accents, etc). Can also remove whitespace by setting rm_whit = T.
clean_name <- function(x, rm_whit = F){
  if(rm_whit == T){ x = x |> stringi::stri_replace_all_charclass(pattern = "\\p{WHITE_SPACE}", "")}
  x |> tolower() |> stringi::stri_trans_general(id = "Latin-ASCII")
}
