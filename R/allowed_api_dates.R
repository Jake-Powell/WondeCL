
# ============================================================
# WondeCL API date restrictions
# ============================================================

# Package-wide default windows.
# Use character(0) to deny all queries unless configured
# through .Rprofile or set_allowed_api_dates().
.default_allowed_api_dates <- character(0)

# Session-specific overrides.
# NULL means use .Rprofile settings or package defaults.
.allowed_api_dates <- new.env(parent = emptyenv())
.allowed_api_dates$windows <- NULL


# ------------------------------------------------------------
# Validate an individual day/month
# ------------------------------------------------------------

.parse_api_day_month <- function(x) {
  
  if (!is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !grepl("^[0-9]{2}/[0-9]{2}$", x)) {
    stop(
      "Dates must use DD/MM format.",
      call. = FALSE
    )
  }
  
  day <- as.integer(substr(x, 1L, 2L))
  month <- as.integer(substr(x, 4L, 5L))
  
  # Use a leap year to permit 29/02.
  days_in_month <- c(
    31L, 29L, 31L, 30L, 31L, 30L,
    31L, 31L, 30L, 31L, 30L, 31L
  )
  
  if (month < 1L ||
      month > 12L) {
    stop(
      "Invalid calendar date: ", x,
      call. = FALSE
    )
  }
  
  if (day < 1L ||
      day > days_in_month[month]) {
    stop(
      "Invalid calendar date: ", x,
      call. = FALSE
    )
  }
  
  # Numeric MMDD representation.
  month * 100L + day
}


# ------------------------------------------------------------
# Validate a vector of date windows
# ------------------------------------------------------------

.validate_allowed_api_dates <- function(
    dates,
    allow_empty = FALSE) {
  
  if (!is.character(dates) ||
      anyNA(dates)) {
    stop(
      "Allowed API dates must be a character vector.",
      call. = FALSE
    )
  }
  
  if (length(dates) == 0L) {
    
    if (isTRUE(allow_empty)) {
      return(character(0))
    }
    
    stop(
      "At least one allowed API date range is required.",
      call. = FALSE
    )
  }
  
  dates <- trimws(dates)
  
  pattern <- paste0(
    "^[0-9]{2}/[0-9]{2}",
    "\\s*-\\s*",
    "[0-9]{2}/[0-9]{2}$"
  )
  
  if (any(!grepl(pattern, dates))) {
    stop(
      "Date ranges must use DD/MM-DD/MM format.",
      call. = FALSE
    )
  }
  
  # Normalise spaces around the separator.
  dates <- gsub(
    "\\s*-\\s*",
    "-",
    dates
  )
  
  # Validate all ranges before returning.
  for (window in dates) {
    
    parts <- strsplit(
      window,
      "-",
      fixed = TRUE
    )[[1]]
    
    .parse_api_day_month(parts[1])
    .parse_api_day_month(parts[2])
  }
  
  unique(dates)
}


#' Get Allowed Wonde API Dates
#'
#' @description
#' Returns the currently active annual date windows for
#' querying the Wonde API.
#'
#' Settings are resolved in priority order:
#'
#' 1. Session overrides created with `set_allowed_api_dates()`.
#' 2. The `WondeCL.allowed_api_dates` R option.
#' 3. The package's built-in defaults.
#'
#' The R option can be set in `.Rprofile` to persist
#' across R sessions.
#'
#' @param source Logical. If TRUE, returns the configuration
#'   source together with the active date windows.
#'
#' @return A character vector of date windows, or a list
#'   containing `source` and `dates` when `source = TRUE`.
#'
#' @examples
#' get_allowed_api_dates()
#' get_allowed_api_dates(source = TRUE)
#'
#' @export
get_allowed_api_dates <- function(source = FALSE) {
  
  if (!is.logical(source) ||
      length(source) != 1L ||
      is.na(source)) {
    stop(
      "source must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  # Priority 1: session override
  if (!is.null(.allowed_api_dates$windows)) {
    
    dates <- .allowed_api_dates$windows
    origin <- "session"
    
  } else {
    
    # Priority 2: user or organisation .Rprofile
    user_dates <- getOption(
      "WondeCL.allowed_api_dates",
      default = NULL
    )
    
    if (!is.null(user_dates)) {
      
      dates <- .validate_allowed_api_dates(
        user_dates,
        allow_empty = TRUE
      )
      
      origin <- "R option"
      
    } else {
      
      # Priority 3: package defaults
      dates <- .validate_allowed_api_dates(
        .default_allowed_api_dates,
        allow_empty = TRUE
      )
      
      origin <- "package defaults"
    }
  }
  
  if (isTRUE(source)) {
    return(list(
      source = origin,
      dates = dates
    ))
  }
  
  dates
}


#' Set Allowed Wonde API Dates
#'
#' @description
#' Sets a temporary session-level override for the
#' permitted Wonde API query dates.
#'
#' The override takes precedence over the user's
#' `.Rprofile` configuration and package defaults.
#'
#' @param dates Character vector of annual date windows,
#'   formatted as DD/MM-DD/MM.
#'
#' @return Invisibly returns the configured windows.
#'
#' @examples
#' set_allowed_api_dates(
#'   c("01/09-30/09", "24/11-24/12")
#' )
#' 
#' options(
# WondeCL.allowed_api_dates = c(
#   "01/09-30/09",
#   "24/11-24/12"
# )
# )
#'
#' reset_allowed_api_dates()
#'
#' @export
set_allowed_api_dates <- function(dates) {
  
  dates <- .validate_allowed_api_dates(
    dates,
    allow_empty = FALSE
  )
  
  .allowed_api_dates$windows <- dates
  
  invisible(dates)
}


#' Reset Allowed Wonde API Dates
#'
#' @description
#' Removes the session-level override and restores
#' the dates configured in `.Rprofile`, or the
#' package defaults if no R option exists.
#'
#' This does not modify the user's `.Rprofile`.
#'
#' @return Invisibly returns the currently active dates.
#'
#' @export
reset_allowed_api_dates <- function() {
  
  .allowed_api_dates$windows <- NULL
  
  invisible(get_allowed_api_dates())
}


# ------------------------------------------------------------
# Determine whether a date is permitted
# ------------------------------------------------------------

.is_allowed_api_date <- function(
    date = as.Date(Sys.time(), tz = "Europe/London")) {
  
  if (!inherits(date, "Date") ||
      length(date) != 1L ||
      is.na(date)) {
    stop(
      "date must be a single valid Date.",
      call. = FALSE
    )
  }
  
  windows <- get_allowed_api_dates()
  
  if (!length(windows)) {
    return(FALSE)
  }
  
  # Numeric month/day representation.
  current <- as.integer(
    format(date, "%m%d")
  )
  
  any(vapply(windows, function(window) {
    
    parts <- strsplit(
      window,
      "-",
      fixed = TRUE
    )[[1]]
    
    start <- .parse_api_day_month(parts[1])
    end <- .parse_api_day_month(parts[2])
    
    if (start <= end) {
      
      # Normal window, e.g. 01/09-30/09.
      current >= start && current <= end
      
    } else {
      
      # Window crossing New Year, e.g. 20/12-10/01.
      current >= start || current <= end
    }
    
  }, logical(1)))
}


# ------------------------------------------------------------
# Enforce the date restriction
# ------------------------------------------------------------

.check_allowed_api_date <- function(
    date = as.Date(Sys.time(), tz = "Europe/London")) {
  
  if (.is_allowed_api_date(date)) {
    return(invisible(TRUE))
  }
  
  config <- get_allowed_api_dates(
    source = TRUE
  )
  
  allowed_text <- if (length(config$dates)) {
    paste(config$dates, collapse = ", ")
  } else {
    "None configured"
  }
  
  stop(
    "Wonde API queries are not permitted on ",
    format(date, "%d/%m/%Y"),
    ".\nAllowed date ranges: ",
    allowed_text,
    ".\nConfiguration source: ",
    config$source,
    ".",
    call. = FALSE
  )
}
