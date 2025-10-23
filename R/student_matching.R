#' @title Add Student_IDs (and original data) from a previous class list
#'
#' @description
#' Reuses existing \code{Student_ID} values from a previous class list by matching
#' students in a new class list. Matching is first attempted by UPN (for rows
#' with a valid UPN), then by name (first and last name) restricted to the same
#' school (by URN). If both datasets have date of birth information, mismatches
#' are flagged by appending "DoB not matched" to the match method description.
#'
#' @param new_class_list Data frame of the current class list.
#' @param old_class_list Data frame of the previous class list (must contain \code{Student_ID}).
#' @param FN_column Column name for pupil first names (default: "Pupil First Name").
#' @param LN_column Column name for pupil last names (default: "Pupil Last Name").
#' @param DoB_column Column name for pupil date of birth (default: "DoB").
#' @param UPN_column Column name for pupil UPNs (default: "UPN").
#' @param URN_column Column name for school URNs (default: "URN").
#' @param Student_ID_column Column name for student IDs (default: "Student_ID").
#' @param verbose Logical; if TRUE, prints progress information.
#' @param fuzzy_match Logical; if TRUE, performs fuzzy name matching using
#'   \code{OMEManage::match_person_to_data()} for remaining unmatched students.
#' @param max_dist Maximum allowed edit distance for fuzzy name matching (default: 2).
#' @param method Method passed to \code{stringdist::stringdist()} (default: "osa").
#'
#' @return
#' The updated new_class_list data frame with:
#' \itemize{
#'   \item Student_ID filled where possible from the previous dataset
#'   \item Match Method column ("UPN", "EXACT", "FUZZY MATCH", etc.)
#'   \item Old data columns: Old Pupil First Name, Old Pupil Last Name, and Old DoB (if available)
#' }
#'
#' @export
add_student_ids_from_previous <- function(new_class_list,
                                          old_class_list,
                                          FN_column = "Pupil First Name",
                                          LN_column = "Pupil Last Name",
                                          DoB_column = "DoB",
                                          UPN_column = "UPN",
                                          URN_column = "URN",
                                          Student_ID_column = "Student_ID",
                                          verbose = FALSE,
                                          fuzzy_match = TRUE,
                                          max_dist = 2,
                                          method = "osa") {
  
  # --- Column checks ---
  for (col in c(FN_column, LN_column, URN_column)) {
    if (!col %in% names(new_class_list))
      stop(paste("Missing column in new_class_list:", col))
  }
  if (!Student_ID_column %in% names(old_class_list))
    stop("Old class list must contain Student_ID column")
  
  # --- Ensure target columns exist ---
  if (!Student_ID_column %in% names(new_class_list))
    new_class_list[[Student_ID_column]] <- NA
  
  new_class_list$`Match Method` <- rep("None", nrow(new_class_list))
  new_class_list$`Old Pupil First Name` <- NA
  new_class_list$`Old Pupil Last Name` <- NA
  new_class_list$`Old DoB` <- NA
  
  # --------------------------------------------------------------------------
  # 1. UPN-based matching (only for non-NA and non-empty UPNs)
  # --------------------------------------------------------------------------
  do_upn_match <- (
    UPN_column %in% names(new_class_list) &&
      UPN_column %in% names(old_class_list) &&
      any(!is.na(new_class_list[[UPN_column]]) & new_class_list[[UPN_column]] != "")
  )
  
  if (do_upn_match) {
    non_na_upn <- which(!is.na(new_class_list[[UPN_column]]) & new_class_list[[UPN_column]] != "")
    upn_match_index <- match(new_class_list[[UPN_column]][non_na_upn],
                             old_class_list[[UPN_column]])
    valid <- non_na_upn[!is.na(upn_match_index)]
    
    new_class_list[[Student_ID_column]][valid] <-
      old_class_list[[Student_ID_column]][upn_match_index[!is.na(upn_match_index)]]
    new_class_list$`Match Method`[valid] <- "UPN"
    
    if (FN_column %in% names(old_class_list))
      new_class_list$`Old Pupil First Name`[valid] <-
      old_class_list[[FN_column]][upn_match_index[!is.na(upn_match_index)]]
    if (LN_column %in% names(old_class_list))
      new_class_list$`Old Pupil Last Name`[valid] <-
      old_class_list[[LN_column]][upn_match_index[!is.na(upn_match_index)]]
    if (DoB_column %in% names(old_class_list))
      new_class_list$`Old DoB`[valid] <-
      old_class_list[[DoB_column]][upn_match_index[!is.na(upn_match_index)]]
    
    if (verbose)
      cli::cli_inform(paste0("Matched ", length(valid), " students by UPN."))
  } else if (verbose) {
    cli::cli_inform("Skipping UPN matching (column missing or all values NA/blank).")
  }
  
  # --------------------------------------------------------------------------
  # 2. Name-based matching for remaining students
  # --------------------------------------------------------------------------
  remaining <- which(is.na(new_class_list[[Student_ID_column]]) |
                       new_class_list[[Student_ID_column]] == "")
  
  if (length(remaining) > 0 && fuzzy_match) {
    if (verbose)
      cli::cli_inform(paste0("Attempting name-based matching for ",
                             length(remaining), " remaining students..."))
    
    if (requireNamespace("pbapply", quietly = TRUE) && verbose) {
      pblapply_fun <- pbapply::pblapply
    } else {
      pblapply_fun <- lapply
    }
    
    match_rows <- pblapply_fun(seq_along(remaining), function(j) {
      i <- remaining[j]
      FN <- new_class_list[[FN_column]][i]
      LN <- new_class_list[[LN_column]][i]
      urn <- new_class_list[[URN_column]][i]
      
      # Restrict search to same school
      old_data_school <- old_class_list[old_class_list[[URN_column]] == urn, ]
      
      if (nrow(old_data_school) == 0)
        return(list(ID = NA, msg = "No school data", old_FN = NA, old_LN = NA, old_DoB = NA))
      
      # --- Use fuzzy/exact matching ---
      if (requireNamespace("OMEManage", quietly = TRUE)) {
        out <- OMEManage::match_person_to_data(
          FN = FN,
          LN = LN,
          data = old_data_school,
          FN_column = FN_column,
          LN_column = LN_column,
          UPI_column = Student_ID_column,
          max_dist = max_dist,
          method = method,
          show_all_fuzzy = FALSE
        )
        
        old_FN <- out$people[[FN_column]][1]
        old_LN <- out$people[[LN_column]][1]
        old_DoB <- if (DoB_column %in% names(out$people))
          out$people[[DoB_column]][1] else NA
        
        # --- Check DoB mismatch only for partial / fuzzy matches ---
        msg <- out$message
        if (!is.na(new_class_list[[DoB_column]][i]) &&
            !is.na(old_DoB) &&
            msg %in% c("FUZZY MATCH (best both FN & LN)",
                       "FUZZY PARTIAL MATCH (best FN or LN)",
                       "FN only", "LN only", "LN only (Swap)",
                       "FN only (Swap)")) {
          if (!identical(as.character(new_class_list[[DoB_column]][i]),
                         as.character(old_DoB))) {
            msg <- paste0(msg, "; DoB not matched")
          }
        }
        
        list(ID = out$UPI,
             msg = msg,
             old_FN = old_FN,
             old_LN = old_LN,
             old_DoB = old_DoB)
        
      } else {
        # --- Fallback: exact match only ---
        idx <- which(toupper(old_data_school[[FN_column]]) == toupper(FN) &
                       toupper(old_data_school[[LN_column]]) == toupper(LN))
        if (length(idx) == 1) {
          list(ID = old_data_school[[Student_ID_column]][idx],
               msg = "EXACT (fallback)",
               old_FN = old_data_school[[FN_column]][idx],
               old_LN = old_data_school[[LN_column]][idx],
               old_DoB = if (DoB_column %in% names(old_data_school))
                 old_data_school[[DoB_column]][idx] else NA)
        } else {
          list(ID = NA, msg = "No match", old_FN = NA, old_LN = NA, old_DoB = NA)
        }
      }
    })
    
    match_df <- do.call(rbind, lapply(match_rows, function(x)
      data.frame(Student_ID = x$ID, MatchMethod = x$msg,
                 OldFN = x$old_FN, OldLN = x$old_LN, OldDoB = x$old_DoB,
                 stringsAsFactors = FALSE)))
    
    # Apply results
    new_class_list[[Student_ID_column]][remaining] <- match_df$Student_ID
    new_class_list$`Match Method`[remaining] <- match_df$MatchMethod
    new_class_list$`Old Pupil First Name`[remaining] <- match_df$OldFN
    new_class_list$`Old Pupil Last Name`[remaining] <- match_df$OldLN
    new_class_list$`Old DoB`[remaining] <- match_df$OldDoB
    
    if (verbose)
      cli::cli_inform(paste0("Matched ",
                             sum(!is.na(match_df$Student_ID)),
                             " additional students by name (same school)."))
  }
  
  if (verbose) {
    total_matched <- sum(new_class_list$`Match Method` != "None")
    cli::cli_inform(paste0("Finished assigning Student_IDs. ",
                           total_matched, " total matched."))
  }
  
  new_class_list
}
