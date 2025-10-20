
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


#' convert sublist element of data frame to a data frame
#'
#' @param data data
#' @param column_to_unnest column_to_unnest
#'
#'
convert_list_element_to_df <- function(data, column_to_unnest){
  for(i in 1:nrow(data)){
    val = data[[column_to_unnest]][i][[1]]
    if(is.list(val) &length(val) == 0){
      data[[column_to_unnest]][i][[1]] = data.frame()
    }
  }
  data
}


#' combine two lists element-wise
#'
#' @param a list
#' @param b list
#'
#' @return a and b combined into one list (i,e a[[1]], a[[2]], ... a[[N]], b[[1]], b[[2]], ... b[[M]].)
#'
#' @examplesIf FALSE
#' list1 = list(A=1, B=2, C=3)
#' list2 = list(D=1, E=2, F=3)
#' combine_raw_data(list1, list2)
#'
combine_raw_data <- function(a, b){
  for(i in 1:length(b)){
    a[[names(b)[i]]] = b[[i]]
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
