
#' Add Student IDs from a Master List
#'
#' @description
#' Reuses existing OME_ID values from a Student Master List by matching
#' students in a new class list.
#'
#' Matching is attempted first using UPN. Remaining students are matched
#' by name within the same establishment using OMELink::match_person_to_data(),
#' with an exact-name fallback if OMELink is unavailable.
#'
#' Column names in the class list are resolved using the package's
#' column configuration. Master-list column names are specified
#' independently using master_columns.
#'
#' @param new_class_list Current class-list data frame.
#' @param master_list Student Master List data frame.
#' @param master_columns Named character vector mapping internal field
#'   names to columns in the master list. By default, FNS and LNS
#'   contain historical names separated by "---".
#' @param school_id_column Internal or configured school identifier
#'   in the class list. Defaults to "est_id".
#' @param verbose Logical. If TRUE, displays progress information.
#' @param fuzzy_match Logical. If TRUE, performs name matching for
#'   remaining unmatched students. Defaults to TRUE.
#' @param max_dist Maximum edit distance passed to
#'   OMELink::match_person_to_data().
#' @param method Matching method passed to
#'   OMELink::match_person_to_data().
#'
#' @return
#' Updated class list containing OME_ID, MATCH_METHOD,
#' OLD_STUDENT_FIRST_NAME, OLD_STUDENT_LAST_NAME and OLD_DOB.
#'
#' @details
#' The original matching methodology is retained. UPN matching
#' uses match(), taking the first matching master-list record.
#' Name-based matching is restricted to the selected school.
#'
#' OLD_STUDENT_FIRST_NAME and OLD_STUDENT_LAST_NAME are always
#' returned in lowercase. For UPN matches, these contain the
#' complete historical name strings from FNS and LNS. For
#' OMELink matches, they contain the matched name candidates.
#'
#' Historical DoBs separated by "---" are considered individually
#' when checking for mismatches for fuzzy/partial name matches.
#'
#' Additional master-list fields are not used in matching.
#'
#' @export
add_student_ids_from_previous <- function(
    new_class_list,
    master_list,
    master_columns = c(
      ome_id = "OME_ID",
      sfn = "FNS",
      sln = "LNS",
      dob = "DOBS",
      upn = "UPN",
      est_id = "EST_ID"
    ),
    school_id_column = "est_id",
    verbose = FALSE,
    fuzzy_match = TRUE,
    max_dist = 2,
    method = "osa") {
  
  # ------------------------------------------------------------
  # 1. Convert class-list columns to internal names
  # ------------------------------------------------------------
  new_class_list <- rename_to_internal_names(new_class_list)
  
  # ------------------------------------------------------------
  # 2. Resolve the class-list school identifier
  # ------------------------------------------------------------
  column_names <- get_column_names()
  
  if (school_id_column %in% names(column_names)) {
    
    school_col <- school_id_column
    
  } else if (school_id_column %in% unname(column_names)) {
    
    school_col <- names(column_names)[
      match(school_id_column, unname(column_names))
    ]
    
  } else {
    
    stop(
      "Unknown school identifier column: ",
      school_id_column,
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 3. Convert master-list columns to internal names
  # ------------------------------------------------------------
  required_master_keys <- c(
    "ome_id", "sfn", "sln", "est_id"
  )
  
  if (!all(required_master_keys %in% names(master_columns))) {
    stop(
      "master_columns must include: ",
      paste(required_master_keys, collapse = ", "),
      call. = FALSE
    )
  }
  
  missing_master_cols <- setdiff(
    unname(master_columns[required_master_keys]),
    names(master_list)
  )
  
  if (length(missing_master_cols) > 0L) {
    stop(
      "Missing columns in master_list: ",
      paste(missing_master_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  master <- as.data.frame(master_list)
  
  for (key in names(master_columns)) {
    
    original_name <- unname(master_columns[[key]])
    
    if (original_name %in% names(master)) {
      names(master)[names(master) == original_name] <- key
    }
  }
  
  # ------------------------------------------------------------
  # 4. Validate school identifier and class-list columns
  # ------------------------------------------------------------
  if (!school_col %in% names(master)) {
    stop(
      "Master list does not contain a mapped school identifier ",
      "for: ", school_col,
      call. = FALSE
    )
  }
  
  required_class_cols <- c(
    "sfn", "sln", school_col
  )
  
  missing_class_cols <- setdiff(
    required_class_cols,
    names(new_class_list)
  )
  
  if (length(missing_class_cols) > 0L) {
    stop(
      "Missing columns in new_class_list: ",
      paste(missing_class_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # 5. Ensure target columns exist
  # ------------------------------------------------------------
  if (!"ome_id" %in% names(new_class_list)) {
    new_class_list$ome_id <- rep(
      NA_character_,
      nrow(new_class_list)
    )
  }
  
  new_class_list$MATCH_METHOD <- rep(
    "None",
    nrow(new_class_list)
  )
  
  new_class_list$OLD_STUDENT_FIRST_NAME <- rep(
    NA_character_,
    nrow(new_class_list)
  )
  
  new_class_list$OLD_STUDENT_LAST_NAME <- rep(
    NA_character_,
    nrow(new_class_list)
  )
  
  new_class_list$OLD_DOB <- rep(
    NA_character_,
    nrow(new_class_list)
  )
  
  # ------------------------------------------------------------
  # 6. UPN-based matching
  # ------------------------------------------------------------
  do_upn_match <- (
    "upn" %in% names(new_class_list) &&
      "upn" %in% names(master) &&
      any(
        !is.na(new_class_list$upn) &
          as.character(new_class_list$upn) != ""
      )
  )
  
  if (do_upn_match) {
    
    # Retain the original exclusion of blank and "NA" UPNs
    ocl <- master[
      which(
        !is.na(master$upn) &
          !as.character(master$upn) %in% c("", "NA")
      ),
      ,
      drop = FALSE
    ]
    
    non_na_upn <- which(
      !is.na(new_class_list$upn) &
        as.character(new_class_list$upn) != ""
    )
    
    upn_match_index <- match(
      new_class_list$upn[non_na_upn],
      ocl$upn
    )
    
    valid <- non_na_upn[
      !is.na(upn_match_index)
    ]
    
    if (length(valid) > 0L) {
      
      matched_old <- upn_match_index[
        !is.na(upn_match_index)
      ]
      
      new_class_list$ome_id[valid] <-
        ocl$ome_id[matched_old]
      
      new_class_list$MATCH_METHOD[valid] <- "UPN"
      
      # Always return old first names in lowercase
      if ("sfn" %in% names(ocl)) {
        new_class_list$OLD_STUDENT_FIRST_NAME[valid] <-
          tolower(as.character(
            ocl$sfn[matched_old]
          ))
      }
      
      # Always return old surnames in lowercase
      if ("sln" %in% names(ocl)) {
        new_class_list$OLD_STUDENT_LAST_NAME[valid] <-
          tolower(as.character(
            ocl$sln[matched_old]
          ))
      }
      
      if ("dob" %in% names(ocl)) {
        new_class_list$OLD_DOB[valid] <-
          as.character(ocl$dob[matched_old])
      }
    }
    
    if (isTRUE(verbose)) {
      cli::cli_inform(
        paste0(
          "Matched ", length(valid),
          " students by UPN."
        )
      )
    }
    
  } else if (isTRUE(verbose)) {
    
    cli::cli_inform(
      "Skipping UPN matching (column missing or all values NA/blank)."
    )
  }
  
  # ------------------------------------------------------------
  # 7. Name-based matching for remaining students
  # ------------------------------------------------------------
  remaining <- which(
    is.na(new_class_list$ome_id) |
      as.character(new_class_list$ome_id) == ""
  )
  
  # Preserve original behaviour: fuzzy_match = FALSE
  # skips the entire name-based matching stage.
  if (length(remaining) > 0L && fuzzy_match) {
    
    if (isTRUE(verbose)) {
      cli::cli_inform(
        paste0(
          "Attempting name-based matching for ",
          length(remaining),
          " remaining students..."
        )
      )
    }
    
    if (requireNamespace("pbapply", quietly = TRUE) &&
        isTRUE(verbose)) {
      
      pblapply_fun <- pbapply::pblapply
      
    } else {
      
      pblapply_fun <- lapply
    }
    
    match_rows <- pblapply_fun(
      seq_along(remaining),
      function(j) {
        
        i <- remaining[j]
        
        FN <- new_class_list$sfn[i]
        LN <- new_class_list$sln[i]
        school <- new_class_list[[school_col]][i]
        
        # Restrict search to the same school
        old_data_school <- master[
          !is.na(master[[school_col]]) &
            master[[school_col]] == school,
          ,
          drop = FALSE
        ]
        
        if (nrow(old_data_school) == 0L) {
          
          return(list(
            ID = NA_character_,
            msg = "No school data",
            old_FN = NA_character_,
            old_LN = NA_character_,
            old_DoB = NA_character_
          ))
        }
        
        # ------------------------------------------------------
        # 7.1 Original name-matching methodology (OMELink)
        # ------------------------------------------------------
        if (requireNamespace("OMELink", quietly = TRUE)) {
          
          out <- OMELink::match_person_to_data(
            FN = FN,
            LN = LN,
            data = old_data_school,
            FN_column = "sfn",
            LN_column = "sln",
            UPI_column = "ome_id",
            max_dist = max_dist,
            method = method,
            show_all_fuzzy = FALSE
          )
          
          if (length(out$UPI) == 0L) {
            out$UPI <- "MULTIPLE"
          }
          
          # OMELink returns individual matched name candidates.
          # Always convert these to lowercase.
          old_FN <- tolower(as.character(
            out$people[["sfn"]][1]
          ))
          
          old_LN <- tolower(as.character(
            out$people[["sln"]][1]
          ))
          
          # Retain the original DoB extraction, if available
          old_DoB <- if ("dob" %in% names(out$people)) {
            out$people[["dob"]][1]
          } else {
            NA_character_
          }
          
          # ----------------------------------------------------
          # 7.2 DoB mismatch check
          # ----------------------------------------------------
          msg <- out$message
          
          if ("dob" %in% names(new_class_list) &&
              !is.na(new_class_list$dob[i]) &&
              !is.na(old_DoB) &&
              msg %in% c(
                "FUZZY MATCH (best both FN & LN)",
                "FUZZY PARTIAL MATCH (best FN or LN)",
                "FN only",
                "LN only",
                "LN only (Swap)",
                "FN only (Swap)"
              )) {
            
            # The master list can contain multiple dates of birth
            # separated by "---".
            known_dobs <- trimws(
              stringr::str_split(
                as.character(old_DoB),
                "---"
              )[[1]]
            )
            
            if (!as.character(new_class_list$dob[i]) %in%
                known_dobs) {
              
              msg <- paste0(
                msg,
                "; DoB not matched"
              )
            }
          }
          
          list(
            ID = out$UPI,
            msg = msg,
            old_FN = old_FN,
            old_LN = old_LN,
            old_DoB = old_DoB
          )
          
        } else {
          
          # ----------------------------------------------------
          # 7.3 Original fallback: exact name match only
          # ----------------------------------------------------
          idx <- which(
            toupper(old_data_school$sfn) == toupper(FN) &
              toupper(old_data_school$sln) == toupper(LN)
          )
          
          if (length(idx) == 1L) {
            
            list(
              ID = old_data_school$ome_id[idx],
              msg = "EXACT (fallback)",
              
              # Always return lowercase names
              old_FN = tolower(as.character(
                old_data_school$sfn[idx]
              )),
              old_LN = tolower(as.character(
                old_data_school$sln[idx]
              )),
              
              old_DoB = if ("dob" %in% names(old_data_school)) {
                old_data_school$dob[idx]
              } else {
                NA_character_
              }
            )
            
          } else {
            
            list(
              ID = NA_character_,
              msg = "No match",
              old_FN = NA_character_,
              old_LN = NA_character_,
              old_DoB = NA_character_
            )
          }
        }
      }
    )
    
    # ----------------------------------------------------------
    # 8. Combine matching results
    # ----------------------------------------------------------
    match_df <- do.call(
      rbind,
      lapply(
        match_rows,
        function(x) {
          data.frame(
            OME_ID = x$ID,
            MatchMethod = x$msg,
            OldFN = x$old_FN,
            OldLN = x$old_LN,
            OldDoB = x$old_DoB,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    
    # ----------------------------------------------------------
    # 9. Apply matching results
    # ----------------------------------------------------------
    new_class_list$ome_id[remaining] <-
      match_df$OME_ID
    
    new_class_list$MATCH_METHOD[remaining] <-
      match_df$MatchMethod
    
    # Enforce lowercase for all matched names
    new_class_list$OLD_STUDENT_FIRST_NAME[remaining] <-
      tolower(as.character(match_df$OldFN))
    
    new_class_list$OLD_STUDENT_LAST_NAME[remaining] <-
      tolower(as.character(match_df$OldLN))
    
    new_class_list$OLD_DOB[remaining] <-
      as.character(match_df$OldDoB)
    
    if (isTRUE(verbose)) {
      cli::cli_inform(
        paste0(
          "Matched ",
          sum(!is.na(match_df$OME_ID)),
          " additional students by name (same school)."
        )
      )
    }
  }
  
  # ------------------------------------------------------------
  # 10. Final progress message
  # ------------------------------------------------------------
  if (isTRUE(verbose)) {
    
    total_matched <- sum(
      new_class_list$MATCH_METHOD != "None"
    )
    
    cli::cli_inform(
      paste0(
        "Finished assigning OME_IDs. ",
        total_matched,
        " total matched."
      )
    )
  }
  
  # ------------------------------------------------------------
  # 11. Return configured column names
  # ------------------------------------------------------------
  rename_to_configured_names(new_class_list)
}
