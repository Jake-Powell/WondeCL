
# ============================================================
# Tests: revert_ignored_withdrawals()
# ============================================================

testthat::test_that("Ignored withdrawals have their flags reverted", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", "Y", ""),
    DOB = c("", "2010-02-02", "2010-03-03"),
    UPN = c("", "B", "C"),
    PRINT_ID = c("", "P2", "P3"),
    OME_ID = c("", "O2", "O3")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = c("Y", NA),
    CLASS_LIST_INDEX = c(1, 2)
  )
  
  testthat::expect_message(
    result <- revert_ignored_withdrawals(df, withdraw),
    "1 withdrawals reverted"
  )
  
  # Withdrawal flag removed only for ignored records
  testthat::expect_equal(
    result$WITHDRAWN,
    c("", "Y", "")
  )
  
  # Blank identifying fields become NA
  testthat::expect_true(is.na(result$DOB[1]))
  testthat::expect_true(is.na(result$UPN[1]))
  testthat::expect_true(is.na(result$PRINT_ID[1]))
  testthat::expect_true(is.na(result$OME_ID[1]))
  
  # Other records remain unchanged
  testthat::expect_equal(
    result[2:3, ],
    df[2:3, ]
  )
  
  # Number of rows is preserved
  testthat::expect_equal(nrow(result), nrow(df))
  
})


testthat::test_that("Internal column names are supported", {
  
  reset_column_names()
  
  df <- data.frame(
    withdrawn = c("Y", "Y"),
    dob = c("", "2010-02-02"),
    upn = c("", "B"),
    hex_id = c("", "P2"),
    ome_id = c("", "O2")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = "Y",
    CLASS_LIST_INDEX = 1
  )
  
  result <- suppressMessages(
    revert_ignored_withdrawals(df, withdraw)
  )
  
  # Output uses configured column names
  testthat::expect_true(
    all(
      c(
        "WITHDRAWN",
        "DOB",
        "UPN",
        "PRINT_ID",
        "OME_ID"
      ) %in% names(result)
    )
  )
  
  testthat::expect_equal(
    result$WITHDRAWN,
    c("", "Y")
  )
  
  testthat::expect_true(is.na(result$DOB[1]))
  testthat::expect_true(is.na(result$UPN[1]))
  testthat::expect_true(is.na(result$PRINT_ID[1]))
  testthat::expect_true(is.na(result$OME_ID[1]))
  
  # Non-ignored record remains unchanged
  testthat::expect_equal(result$DOB[2], "2010-02-02")
  testthat::expect_equal(result$UPN[2], "B")
  testthat::expect_equal(result$PRINT_ID[2], "P2")
  testthat::expect_equal(result$OME_ID[2], "O2")
  
})


testthat::test_that("Custom configured column names are supported", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    withdrawn = "WITHDRAWAL_STATUS",
    dob = "DATE_OF_BIRTH",
    upn = "PUPIL_UPN",
    hex_id = "SURVEY_PRINT_ID",
    ome_id = "STUDENT_OME_ID"
  )
  
  df <- data.frame(
    WITHDRAWAL_STATUS = c("Y", "Y"),
    DATE_OF_BIRTH = c("", "2010-02-02"),
    PUPIL_UPN = c("", "B"),
    SURVEY_PRINT_ID = c("", "P2"),
    STUDENT_OME_ID = c("", "O2")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = "Y",
    CLASS_LIST_INDEX = 1
  )
  
  result <- suppressMessages(
    revert_ignored_withdrawals(df, withdraw)
  )
  
  # Check configured output names
  testthat::expect_identical(
    names(result),
    names(df)
  )
  
  testthat::expect_true(
    all(
      c(
        "WITHDRAWAL_STATUS",
        "DATE_OF_BIRTH",
        "PUPIL_UPN",
        "SURVEY_PRINT_ID",
        "STUDENT_OME_ID"
      ) %in% names(result)
    )
  )
  
  testthat::expect_false(
    "hex_id" %in% names(result)
  )
  
  # Check withdrawal has been reverted
  testthat::expect_equal(
    result$WITHDRAWAL_STATUS,
    c("", "Y")
  )
  
  testthat::expect_true(is.na(result$DATE_OF_BIRTH[1]))
  testthat::expect_true(is.na(result$PUPIL_UPN[1]))
  testthat::expect_true(is.na(result$SURVEY_PRINT_ID[1]))
  testthat::expect_true(is.na(result$STUDENT_OME_ID[1]))
  
  # Check non-ignored record
  testthat::expect_equal(
    result$DATE_OF_BIRTH[2],
    "2010-02-02"
  )
  
  testthat::expect_equal(
    result$PUPIL_UPN[2],
    "B"
  )
  
  testthat::expect_equal(
    result$SURVEY_PRINT_ID[2],
    "P2"
  )
  
  testthat::expect_equal(
    result$STUDENT_OME_ID[2],
    "O2"
  )
  
})


testthat::test_that("No ignored withdrawals leaves data unchanged", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", ""),
    DOB = c("2010-01-01", "2010-02-02")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = c("N", NA),
    CLASS_LIST_INDEX = c(1, 2)
  )
  
  testthat::expect_message(
    result <- revert_ignored_withdrawals(df, withdraw),
    "No ignored withdrawals found"
  )
  
  testthat::expect_equal(result, df)
  
})


testthat::test_that("Invalid class list indices are ignored safely", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", "Y")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = c("Y", "Y", "Y", "Y"),
    CLASS_LIST_INDEX = c(1, 999, 0, -1)
  )
  
  testthat::expect_warning(
    result <- suppressMessages(
      revert_ignored_withdrawals(df, withdraw)
    ),
    "invalid"
  )
  
  # Only the valid index should be modified
  testthat::expect_equal(
    result$WITHDRAWN,
    c("", "Y")
  )
  
})


testthat::test_that("NA class list indices are ignored", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", "Y")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = c("Y", "Y"),
    CLASS_LIST_INDEX = c(NA_real_, 2)
  )
  
  result <- suppressMessages(
    revert_ignored_withdrawals(df, withdraw)
  )
  
  testthat::expect_equal(
    result$WITHDRAWN,
    c("Y", "")
  )
  
})


testthat::test_that("Duplicate class list indices are processed once", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", "Y")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = c("Y", "Y", "Y"),
    CLASS_LIST_INDEX = c(1, 1, 2)
  )
  
  testthat::expect_message(
    result <- revert_ignored_withdrawals(df, withdraw),
    "2 withdrawals reverted"
  )
  
  testthat::expect_equal(
    result$WITHDRAWN,
    c("", "")
  )
  
})


testthat::test_that("Non-empty identifying fields are preserved", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = "Y",
    DOB = "2010-01-01",
    UPN = "A123",
    PRINT_ID = "P001",
    OME_ID = "O001"
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = "Y",
    CLASS_LIST_INDEX = 1
  )
  
  result <- suppressMessages(
    revert_ignored_withdrawals(df, withdraw)
  )
  
  testthat::expect_equal(result$WITHDRAWN, "")
  testthat::expect_equal(result$DOB, "2010-01-01")
  testthat::expect_equal(result$UPN, "A123")
  testthat::expect_equal(result$PRINT_ID, "P001")
  testthat::expect_equal(result$OME_ID, "O001")
  
})


testthat::test_that("Missing optional identifying columns are allowed", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", "Y"),
    UPN = c("", "B")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = "Y",
    CLASS_LIST_INDEX = 1
  )
  
  result <- suppressMessages(
    revert_ignored_withdrawals(df, withdraw)
  )
  
  testthat::expect_equal(
    result$WITHDRAWN,
    c("", "Y")
  )
  
  testthat::expect_true(is.na(result$UPN[1]))
  testthat::expect_false("DOB" %in% names(result))
  testthat::expect_false("PRINT_ID" %in% names(result))
  testthat::expect_false("OME_ID" %in% names(result))
  
})


testthat::test_that("Missing withdrawal report columns produce an error", {
  
  reset_column_names()
  
  df <- data.frame(WITHDRAWN = "Y")
  
  # Missing CLASS_LIST_INDEX
  withdraw <- data.frame(
    IGNORE_RESULT = "Y"
  )
  
  testthat::expect_error(
    revert_ignored_withdrawals(df, withdraw),
    "Missing required withdrawal columns"
  )
  
  # Missing IGNORE_RESULT
  withdraw <- data.frame(
    CLASS_LIST_INDEX = 1
  )
  
  testthat::expect_error(
    revert_ignored_withdrawals(df, withdraw),
    "Missing required withdrawal columns"
  )
  
})


testthat::test_that("Missing withdrawal status column produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    DOB = "2010-01-01",
    UPN = "A"
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = "Y",
    CLASS_LIST_INDEX = 1
  )
  
  testthat::expect_error(
    revert_ignored_withdrawals(df, withdraw),
    "Withdrawal status column not found"
  )
  
})


testthat::test_that("Custom names work when no withdrawals are ignored", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    withdrawn = "WITHDRAWAL_STATUS",
    hex_id = "SURVEY_PRINT_ID",
    ome_id = "STUDENT_OME_ID"
  )
  
  df <- data.frame(
    WITHDRAWAL_STATUS = c("Y", ""),
    SURVEY_PRINT_ID = c("P001", "P002"),
    STUDENT_OME_ID = c("O001", "O002")
  )
  
  withdraw <- data.frame(
    IGNORE_RESULT = c("N", NA),
    CLASS_LIST_INDEX = c(1, 2)
  )
  
  result <- suppressMessages(
    revert_ignored_withdrawals(df, withdraw)
  )
  
  testthat::expect_identical(
    names(result),
    names(df)
  )
  
  testthat::expect_equal(
    result$WITHDRAWAL_STATUS,
    c("Y", "")
  )
  
  testthat::expect_equal(
    result$SURVEY_PRINT_ID,
    c("P001", "P002")
  )
  
  testthat::expect_equal(
    result$STUDENT_OME_ID,
    c("O001", "O002")
  )
  
})
