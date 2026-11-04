
# ============================================================
# Tests: apply_withdrawal() and withdrawal helpers
# ============================================================

testthat::test_that("Unique name match withdraws one student", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "B"),
    EST_WONDE_NAME = c("School A", "School A", "School B"),
    PUPIL_FIRST_NAME = c("Jane", "Alex", "Jane"),
    PUPIL_LAST_NAME = c("Doe", "Smith", "Doe"),
    DOB = c("2012-01-01", "2012-02-02", "2012-03-03"),
    stringsAsFactors = FALSE
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    c("Y", "", "")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    1L
  )
  
  testthat::expect_equal(
    result$withdraw$MATCH_DETAIL,
    "Exact first and last name match (to one student)"
  )
  
  testthat::expect_equal(
    result$withdraw$MATCHED_FIRST_NAME,
    "Jane"
  )
  
  testthat::expect_equal(
    result$withdraw$MATCHED_LAST_NAME,
    "Doe"
  )
  
  testthat::expect_equal(
    result$withdraw$MATCHED_SCHOOL_NAME,
    "School A"
  )
  
})


testthat::test_that("Withdrawal report has standardised column names", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    EST_WONDE_NAME = "School A",
    PUPIL_FIRST_NAME = "Jane",
    PUPIL_LAST_NAME = "Doe"
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_identical(
    names(result$withdraw),
    c(
      "FIRST_NAME",
      "LAST_NAME",
      "DOB",
      "EST_ID",
      "MATCH_DETAIL",
      "SCHOOL_CHECK",
      "CLASS_LIST_INDEX",
      "MATCHED_FIRST_NAME",
      "MATCHED_LAST_NAME",
      "MATCHED_SCHOOL_NAME",
      "IGNORE_RESULT"
    )
  )
  
  testthat::expect_false(
    "CLASS_LIST_INDICES" %in% names(result$withdraw)
  )
  
  testthat::expect_true(
    is.na(result$withdraw$IGNORE_RESULT)
  )
  
})


testthat::test_that("Multiple exact name matches are ambiguous", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A"),
    EST_WONDE_NAME = rep("School A", 3),
    PUPIL_FIRST_NAME = c("Jane", "Jane", "Alex"),
    PUPIL_LAST_NAME = c("Doe", "Doe", "Smith"),
    DOB = c("2012-01-01", "2012-01-01", "2012-02-02")
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_true(
    all(result$class_list$WITHDRAWN == "")
  )
  
  testthat::expect_equal(
    result$withdraw$MATCH_DETAIL,
    "Multiple first and last name match, do not remove any"
  )
  
  testthat::expect_true(
    is.na(result$withdraw$CLASS_LIST_INDEX)
  )
  
})


testthat::test_that("Conflicting dates do not resolve ambiguous names", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    EST_WONDE_NAME = rep("School A", 2),
    PUPIL_FIRST_NAME = c("Jane", "Jane"),
    PUPIL_LAST_NAME = c("Doe", "Doe"),
    DOB = c("2012-01-01", "2012-02-02")
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_true(
    all(result$class_list$WITHDRAWN == "")
  )
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Multiple first and last name match"
  )
  
})


testthat::test_that("Conflicting UPNs do not resolve ambiguous names", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    EST_WONDE_NAME = rep("School A", 2),
    PUPIL_FIRST_NAME = c("Jane", "Jane"),
    PUPIL_LAST_NAME = c("Doe", "Doe"),
    UPN = c("U1", "U2")
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_true(
    all(result$class_list$WITHDRAWN == "")
  )
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Multiple first and last name match"
  )
  
})


testthat::test_that("Withdrawal only affects selected establishment", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "B", "B"),
    EST_WONDE_NAME = c(
      "School A", "School A", "School B", "School B"
    ),
    PUPIL_FIRST_NAME = c("Jane", "Alex", "Jane", "Alex"),
    PUPIL_LAST_NAME = c("Doe", "Smith", "Doe", "Smith")
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    c("Y", "", "", "")
  )
  
  testthat::expect_equal(
    result$withdraw$SCHOOL_CHECK,
    "A"
  )
  
})


testthat::test_that("Unknown establishment does not withdraw students", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Jane",
    PUPIL_LAST_NAME = "Doe"
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "B",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    ""
  )
  
  testthat::expect_equal(
    result$withdraw$MATCH_DETAIL,
    "School not found in class list"
  )
  
  testthat::expect_true(
    is.na(result$withdraw$CLASS_LIST_INDEX)
  )
  
})


testthat::test_that("Non-withdrawal responses are filtered when mixed", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Jane", "Alex"),
    PUPIL_LAST_NAME = c("Doe", "Smith")
  )
  
  requests <- data.frame(
    `Type of withdrawal` = c(
      "I want to withdraw my child from the study",
      "Yes, my child can participate"
    ),
    `Your child's first name` = c("Jane", "Alex"),
    `Your child's surname` = c("Doe", "Smith"),
    EstablishmentID = c("A", "A"),
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_equal(
    nrow(result$withdraw),
    1L
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    c("Y", "")
  )
  
})


testthat::test_that("Existing withdrawal statuses are preserved", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A"),
    PUPIL_FIRST_NAME = c("Jane", "Alex", "Sam"),
    PUPIL_LAST_NAME = c("Doe", "Smith", "Jones"),
    WITHDRAWN = c("", "Y", "")
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    c("Y", "Y", "")
  )
  
})


testthat::test_that("Missing ID fields are created using configured names", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Jane",
    PUPIL_LAST_NAME = "Doe"
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_true(
    all(c(
      "WITHDRAWN",
      "PRINT_ID",
      "OME_ID"
    ) %in% names(result$class_list))
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    "Y"
  )
  
})


testthat::test_that("Alternate school identifier is supported", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_ID = c("W1", "W2"),
    PUPIL_FIRST_NAME = c("Jane", "Jane"),
    PUPIL_LAST_NAME = c("Doe", "Doe")
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "W2",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(
      df,
      requests,
      school_id_column = "est_wonde_id"
    )
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    c("", "Y")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    2L
  )
  
})


testthat::test_that("Custom configured columns are supported", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    est_id = "SCHOOL_IDENTIFIER",
    est_wonde_name = "SCHOOL_NAME",
    sfn = "CHILD_FIRST_NAME",
    sln = "CHILD_LAST_NAME",
    dob = "CHILD_DOB",
    withdrawn = "IS_WITHDRAWN",
    hex_id = "HEX_IDENTIFIER",
    ome_id = "STUDENT_IDENTIFIER"
  )
  
  df <- data.frame(
    SCHOOL_IDENTIFIER = c("A", "A", "B"),
    SCHOOL_NAME = c("School A", "School A", "School B"),
    CHILD_FIRST_NAME = c("Jane", "Alex", "Jane"),
    CHILD_LAST_NAME = c("Doe", "Smith", "Doe"),
    CHILD_DOB = c(
      "2012-01-01",
      "2012-02-02",
      "2012-03-03"
    )
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_equal(
    result$class_list$IS_WITHDRAWN,
    c("Y", "", "")
  )
  
  testthat::expect_true(
    all(c(
      "SCHOOL_IDENTIFIER",
      "CHILD_FIRST_NAME",
      "CHILD_LAST_NAME",
      "IS_WITHDRAWN",
      "HEX_IDENTIFIER",
      "STUDENT_IDENTIFIER"
    ) %in% names(result$class_list))
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    1L
  )
  
  # Report fields remain fixed uppercase names.
  testthat::expect_true(
    "MATCH_DETAIL" %in% names(result$withdraw)
  )
  
})


testthat::test_that("Clearing removes withdrawn identifying data", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    UPN = c("u1", "u2"),
    DOB = c("2012-01-01", "2012-02-02"),
    PRINT_ID = c("p1", "p2"),
    OME_ID = c("o1", "o2"),
    WITHDRAWN = c("Y", "")
  )
  
  result <- clear_withdrawn_student_data(df)
  
  testthat::expect_true(
    all(is.na(result[1, c(
      "UPN", "DOB", "PRINT_ID", "OME_ID"
    )]))
  )
  
  testthat::expect_equal(
    result$UPN[2],
    "u2"
  )
  
  testthat::expect_equal(
    result$DOB[2],
    "2012-02-02"
  )
  
  testthat::expect_equal(
    result$PRINT_ID[2],
    "p2"
  )
  
  testthat::expect_equal(
    result$OME_ID[2],
    "o2"
  )
  
})


testthat::test_that("Clearing accepts configured column arguments", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", ""),
    UPN = c("u1", "u2"),
    DOB = c("2012-01-01", "2012-02-02")
  )
  
  result <- clear_withdrawn_student_data(
    df,
    WithdrawColumn = "WITHDRAWN",
    columns_to_clear = c("UPN", "DOB")
  )
  
  testthat::expect_true(
    is.na(result$UPN[1])
  )
  
  testthat::expect_true(
    is.na(result$DOB[1])
  )
  
  testthat::expect_equal(
    result$UPN[2],
    "u2"
  )
  
})


testthat::test_that("Reverting an ignored withdrawal clears its flag", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Jane", "Alex"),
    PUPIL_LAST_NAME = c("Doe", "Smith"),
    DOB = c("2012-01-01", "2012-02-02"),
    WITHDRAWN = c("", "")
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    c("Y", "")
  )
  
  result$withdraw$IGNORE_RESULT <- "Y"
  
  reverted <- suppressMessages(
    revert_ignored_withdrawals(
      result$class_list,
      result$withdraw
    )
  )
  
  testthat::expect_equal(
    reverted$WITHDRAWN,
    c("", "")
  )
  
  testthat::expect_equal(
    reverted$DOB[1],
    "2012-01-01"
  )
  
})


testthat::test_that("Reverting converts blank identifying fields to NA", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", "Y"),
    DOB = c("", "2012-02-02"),
    UPN = c("", "u2"),
    PRINT_ID = c("", "p2"),
    OME_ID = c("", "o2")
  )
  
  report <- data.frame(
    CLASS_LIST_INDEX = 1L,
    IGNORE_RESULT = "Y"
  )
  
  result <- suppressMessages(
    revert_ignored_withdrawals(df, report)
  )
  
  testthat::expect_equal(
    result$WITHDRAWN,
    c("", "Y")
  )
  
  testthat::expect_true(
    all(is.na(result[1, c(
      "DOB", "UPN", "PRINT_ID", "OME_ID"
    )]))
  )
  
  testthat::expect_equal(
    result$UPN[2],
    "u2"
  )
  
})


testthat::test_that("Reverting without ignored rows leaves flags unchanged", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = c("Y", "")
  )
  
  report <- data.frame(
    CLASS_LIST_INDEX = 1L,
    IGNORE_RESULT = ""
  )
  
  result <- suppressMessages(
    revert_ignored_withdrawals(df, report)
  )
  
  testthat::expect_equal(
    result$WITHDRAWN,
    c("Y", "")
  )
  
})


testthat::test_that("Missing withdrawal report fields produce an error", {
  
  reset_column_names()
  
  df <- data.frame(
    WITHDRAWN = "Y"
  )
  
  report <- data.frame(
    CLASS_LIST_INDEX = 1L
  )
  
  testthat::expect_error(
    revert_ignored_withdrawals(df, report),
    "Missing required withdrawal columns"
  )
  
})


testthat::test_that("Missing withdrawal form fields produce an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Jane",
    PUPIL_LAST_NAME = "Doe"
  )
  
  requests <- data.frame(
    EstablishmentID = "A"
  )
  
  testthat::expect_error(
    suppressMessages(apply_withdrawal(df, requests)),
    "Missing withdrawal form columns"
  )
  
})


testthat::test_that("Invalid matching method produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Jane",
    PUPIL_LAST_NAME = "Doe"
  )
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  testthat::expect_error(
    apply_withdrawal(
      df,
      requests,
      school_dob_match = NA
    ),
    "school_dob_match must be TRUE or FALSE"
  )
  
})


testthat::test_that("Original class list is not modified", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Jane",
    PUPIL_LAST_NAME = "Doe"
  )
  
  original <- df
  
  requests <- data.frame(
    `Type of withdrawal` =
      "I want to withdraw my child from the study",
    `Your child's first name` = "Jane",
    `Your child's surname` = "Doe",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(df, requests)
  )
  
  testthat::expect_identical(df, original)
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    "Y"
  )
  
})



testthat::test_that(
  "Withdrawal creates the configured hex_id column",
  {
    
    reset_column_names()
    on.exit(reset_column_names(), add = TRUE)
    
    set_column_names(
      hex_id = "CUSTOM_PRINT_ID"
    )
    
    df <- data.frame(
      EST_ID = "A",
      PUPIL_FIRST_NAME = "Jane",
      PUPIL_LAST_NAME = "Doe"
    )
    
    requests <- data.frame(
      `Type of withdrawal` =
        "I want to withdraw my child from the study",
      `Your child's first name` = "Jane",
      `Your child's surname` = "Doe",
      EstablishmentID = "A",
      check.names = FALSE
    )
    
    result <- suppressMessages(
      apply_withdrawal(df, requests)
    )
    
    testthat::expect_true(
      "CUSTOM_PRINT_ID" %in% names(result$class_list)
    )
    
    testthat::expect_false(
      "print_id" %in% names(result$class_list)
    )
    
    testthat::expect_equal(
      result$class_list$WITHDRAWN,
      "Y"
    )
    
  }
)


testthat::test_that(
  "Clearing and reverting use configured hex_id",
  {
    
    reset_column_names()
    on.exit(reset_column_names(), add = TRUE)
    
    set_column_names(
      hex_id = "CUSTOM_PRINT_ID"
    )
    
    df <- data.frame(
      WITHDRAWN = c("Y", ""),
      CUSTOM_PRINT_ID = c("ABC", "DEF"),
      UPN = c("u1", "u2"),
      DOB = c("2012-01-01", "2012-02-02"),
      OME_ID = c("o1", "o2")
    )
    
    # Clear withdrawn data
    cleared <- clear_withdrawn_student_data(df)
    
    testthat::expect_true(
      is.na(cleared$CUSTOM_PRINT_ID[1])
    )
    
    testthat::expect_equal(
      cleared$CUSTOM_PRINT_ID[2],
      "DEF"
    )
    
    # Revert ignored withdrawal with blank identifiers
    df$CUSTOM_PRINT_ID[1] <- ""
    
    report <- data.frame(
      CLASS_LIST_INDEX = 1L,
      IGNORE_RESULT = "Y"
    )
    
    reverted <- suppressMessages(
      revert_ignored_withdrawals(df, report)
    )
    
    testthat::expect_equal(
      reverted$WITHDRAWN,
      c("", "")
    )
    
    testthat::expect_true(
      is.na(reverted$CUSTOM_PRINT_ID[1])
    )
    
    testthat::expect_equal(
      reverted$CUSTOM_PRINT_ID[2],
      "DEF"
    )
    
  }
)

