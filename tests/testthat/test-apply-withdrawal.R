
# ============================================================
# Tests: apply_withdrawal()
# ============================================================

testthat::test_that("Current withdrawal method is the default", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones"),
    WITHDRAWN = c("", "")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  testthat::expect_message(
    result <- apply_withdrawal(
      class_list,
      withdraw
    ),
    "Routing to internal current"
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    1L
  )
  
  testthat::expect_true(
    "EST_ID" %in% names(result$withdraw)
  )
  
})


testthat::test_that("Current method can be selected explicitly", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = c("A", "A", "B"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Charlie"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Brown"),
    WITHDRAWN = c("", "", "")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  testthat::expect_message(
    result <- apply_withdrawal(
      class_list,
      withdraw,
      school_dob_match = FALSE
    ),
    "Routing to internal current"
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "", "")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    1L
  )
  
})


testthat::test_that("Legacy method can be selected explicitly", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  
  links <- data.frame(
    EstablishmentName = "Oakfield Primary",
    URN = "123456"
  )
  
  school_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(school_file), add = TRUE)
  openxlsx::write.xlsx(links, school_file)
  
  class_list <- data.frame(
    URN = c("123456", "123456"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones"),
    WITHDRAWN = c("", "")
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  testthat::expect_message(
    result <- apply_withdrawal(
      class_list,
      withdraw,
      school_dob_match = TRUE,
      legacy_school_link = school_file
    ),
    "Routing to internal legacy"
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    1L
  )
  
  testthat::expect_equal(
    result$withdraw$SCHOOL_NAME,
    "OAKFIELD PRIMARY"
  )
  
})


testthat::test_that("Both methods return standardised report headers", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    URN = "123456",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  current_withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  legacy_withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  links <- data.frame(
    EstablishmentName = "Oakfield Primary",
    URN = "123456"
  )
  
  school_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(school_file), add = TRUE)
  openxlsx::write.xlsx(links, school_file)
  
  current_result <- suppressMessages(
    apply_withdrawal(
      class_list,
      current_withdraw
    )
  )
  
  legacy_result <- suppressMessages(
    apply_withdrawal(
      class_list,
      legacy_withdraw,
      school_dob_match = TRUE,
      legacy_school_link = school_file
    )
  )
  
  required_cols <- c(
    "FIRST_NAME",
    "LAST_NAME",
    "DOB",
    "MATCH_DETAIL",
    "SCHOOL_CHECK",
    "CLASS_LIST_INDEX",
    "MATCHED_FIRST_NAME",
    "MATCHED_LAST_NAME",
    "MATCHED_SCHOOL_NAME",
    "IGNORE_RESULT"
  )
  
  testthat::expect_true(
    all(required_cols %in% names(current_result$withdraw))
  )
  
  testthat::expect_true(
    all(required_cols %in% names(legacy_result$withdraw))
  )
  
  testthat::expect_true(
    "EST_ID" %in% names(current_result$withdraw)
  )
  
  testthat::expect_true(
    "SCHOOL_NAME" %in% names(legacy_result$withdraw)
  )
  
  testthat::expect_true(
    all(grepl(
      "^[A-Z][A-Z0-9_]*$",
      names(current_result$withdraw)
    ))
  )
  
  testthat::expect_true(
    all(grepl(
      "^[A-Z][A-Z0-9_]*$",
      names(legacy_result$withdraw)
    ))
  )
  
})


testthat::test_that("Current method accepts alternative school identifiers", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = c("A", "B"),
    EST_WONDE_ID = c("W001", "W002"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Bob",
    "Your child's surname" = "Jones",
    EstablishmentID = "W002",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(
      class_list,
      withdraw,
      school_id_column = "est_wonde_id"
    )
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("", "Y")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    2L
  )
  
  result2 <- suppressMessages(
    apply_withdrawal(
      class_list,
      withdraw,
      school_id_column = "EST_WONDE_ID"
    )
  )
  
  testthat::expect_identical(
    result2$class_list$WITHDRAWN,
    c("", "Y")
  )
  
})


testthat::test_that("Legacy reference column arguments are forwarded", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  
  links <- data.frame(
    SCHOOL_NAME = "Oakfield Primary",
    SCHOOL_URN = "123456"
  )
  
  school_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(school_file), add = TRUE)
  openxlsx::write.xlsx(links, school_file)
  
  class_list <- data.frame(
    URN = "123456",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(
      class_list,
      withdraw,
      school_dob_match = TRUE,
      legacy_school_link = school_file,
      reference_school_name_column = "SCHOOL_NAME",
      reference_school_id_column = "SCHOOL_URN"
    )
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    "Y"
  )
  
  testthat::expect_equal(
    result$withdraw$MATCHED_SCHOOL_NAME,
    "Oakfield Primary"
  )
  
})


testthat::test_that("Custom class-list column names work through wrapper", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    est_id = "SCHOOL_IDENTIFIER",
    sfn = "FIRST_NAME",
    sln = "LAST_NAME",
    withdrawn = "WITHDRAWAL_STATUS"
  )
  
  class_list <- data.frame(
    SCHOOL_IDENTIFIER = c("A", "A"),
    FIRST_NAME = c("Alice", "Bob"),
    LAST_NAME = c("Smith", "Jones"),
    WITHDRAWAL_STATUS = c("", "Y")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(class_list, withdraw)
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWAL_STATUS,
    c("Y", "Y")
  )
  
  testthat::expect_true(
    "SCHOOL_IDENTIFIER" %in% names(result$class_list)
  )
  
  testthat::expect_false(
    "WITHDRAWN" %in% names(result$class_list)
  )
  
  testthat::expect_equal(
    result$withdraw$EST_ID,
    "A"
  )
  
})


testthat::test_that("Existing withdrawal statuses are preserved", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = rep("A", 4),
    PUPIL_FIRST_NAME = c(
      "Alice", "Bob", "Charlie", "Diana"
    ),
    PUPIL_LAST_NAME = c(
      "Smith", "Jones", "Brown", "Evans"
    ),
    WITHDRAWN = c("", "Y", NA_character_, "N")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(class_list, withdraw)
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "Y", NA_character_, "N")
  )
  
  testthat::expect_identical(
    class_list$WITHDRAWN,
    c("", "Y", NA_character_, "N")
  )
  
})


testthat::test_that("Custom withdrawal form columns are forwarded", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  withdraw <- data.frame(
    RESPONSE = "Withdraw",
    CHILD_FIRST = "Alice",
    CHILD_LAST = "Smith",
    SCHOOL_CODE = "A"
  )
  
  result <- suppressMessages(
    apply_withdrawal(
      class_list,
      withdraw,
      keep_grep = "^Withdraw$",
      TypeOfWithdrawalColumn = "RESPONSE",
      FirstNameColumn = "CHILD_FIRST",
      LastNameColumn = "CHILD_LAST",
      EstablishmentIDColumn = "SCHOOL_CODE"
    )
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    "Y"
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    1L
  )
  
  testthat::expect_equal(
    result$withdraw$FIRST_NAME,
    "ALICE"
  )
  
  testthat::expect_equal(
    result$withdraw$EST_ID,
    "A"
  )
  
})


testthat::test_that("Invalid routing arguments produce errors", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  for (invalid in list(
    NA,
    "TRUE",
    1,
    logical(0),
    c(TRUE, FALSE)
  )) {
    
    testthat::expect_error(
      apply_withdrawal(
        class_list,
        withdraw,
        school_dob_match = invalid
      ),
      "school_dob_match must be TRUE or FALSE"
    )
  }
  
})


testthat::test_that("Errors from current method are propagated", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  testthat::expect_error(
    suppressMessages(
      apply_withdrawal(
        class_list,
        withdraw,
        school_id_column = "INVALID_COLUMN"
      )
    ),
    "Unknown school identifier column"
  )
  
})
