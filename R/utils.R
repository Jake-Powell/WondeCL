
#' rbind dataframes with non-identical columns
#'
#' @param list_of_data.frames list of data frames
#'
#' @return rbinded data frame
#' @export
#'
#' @details
#' This function finds all unique column names across all data frames in `list_of_data.frames`
#' and in each data frame in the list adjoins the missing columns whose values are set to NA. Then rbind is applied to the list of data frames.
#'
rbind_aggro <- function(list_of_data.frames){
  if(length(list_of_data.frames) == 1){
    return(list_of_data.frames[[1]])
  }
  all_fields = lapply(list_of_data.frames, function(x)names(x)) |> unlist() |> as.vector() |> unique()
  data_frame_format = lapply(list_of_data.frames, function(df){
    if(nrow(df) == 0){
      return(NULL)
    }
    missing_names = all_fields[!all_fields %in% names(df)]
    if(length(missing_names) > 0){
      for(i in 1:length(missing_names)){
        df[[missing_names[i]]] = NA
      }
    }
    df = df[,match(all_fields, names(df))]
    df
  })
  do.call(rbind, data_frame_format)
}

convert_list_element_to_df <- function(data, column_to_unnest){
  for(i in 1:nrow(data)){
    val = data[[column_to_unnest]][i][[1]]
    if(is.list(val) &length(val) == 0){
      data[[column_to_unnest]][i][[1]] = data.frame()
    }
  }
  data
}

