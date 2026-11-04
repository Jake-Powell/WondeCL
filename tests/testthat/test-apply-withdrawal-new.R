
# ============================================================
# Tests: .apply_withdrawal_new()
# ============================================================

testthat::test_that("Exact student name matching works", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = c("A", "A", "B"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Smith"),
    EST_WONDE_NAME = c("School A", "School A", "School B"),
    WITHDRAWN = c("", "", "")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "", "")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    1L
  )
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Exact first and last name"
  )
  
  testthat::expect_equal(
    result$withdraw$MATCHED_SCHOOL_NAME,
    "School A"
  )
  
  testthat::expect_equal(
    result$withdraw$FIRST_NAME,
    "ALICE"
  )
  
  testthat::expect_equal(
    result$withdraw$LAST_NAME,
    "SMITH"
  )
  
  testthat::expect_equal(
    result$withdraw$EST_ID,
    "A"
  )
  
  testthat::expect_true(
    all(c("IGNORE_RESULT", "CLASS_LIST_INDEX") %in%
          names(result$withdraw))
  )
  
})


testthat::test_that("Withdrawal report uses uppercase snake-case names", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith",
    EST_WONDE_NAME = "School A"
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  expected_names <- c(
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
  
  testthat::expect_setequal(
    names(result$withdraw),
    expected_names
  )
  
  testthat::expect_true(
    all(grepl("^[A-Z][A-Z0-9_]*$", names(result$withdraw)))
  )
  
  testthat::expect_false(
    "EstablishmentID" %in% names(result$withdraw)
  )
  
})


testthat::test_that("Partial first name matching works", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Anna Maria",
    PUPIL_LAST_NAME = "Smith"
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Anna",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_equal(result$class_list$WITHDRAWN, "Y")
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Partial first name"
  )
  
})


testthat::test_that("Partial surname matching works", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith Jones"
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_equal(result$class_list$WITHDRAWN, "Y")
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Partial last name"
  )
  
})


testthat::test_that("Partial first and last names work", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Anna Maria",
    PUPIL_LAST_NAME = "Smith Jones"
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Anna",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_equal(result$class_list$WITHDRAWN, "Y")
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Partial last name, partial first name"
  )
  
})


testthat::test_that("Unique surname-only matching works", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Unknown",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "")
  )
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "surname only"
  )
  
})


testthat::test_that("Duplicate exact name matches are not withdrawn", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Alice", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Smith")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("", "")
  )
  
  testthat::expect_true(
    is.na(result$withdraw$CLASS_LIST_INDEX)
  )
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Multiple first and last name match"
  )
  
})


testthat::test_that("Matching is restricted to the correct school", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = c("A", "B"),
    PUPIL_FIRST_NAME = c("Alice", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Smith")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "B",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("", "Y")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    2L
  )
  
})


testthat::test_that("Unknown school identifiers do not match", {
  
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
    EstablishmentID = "Z",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_equal(result$class_list$WITHDRAWN, "")
  
  testthat::expect_true(
    is.na(result$withdraw$CLASS_LIST_INDEX)
  )
  
  testthat::expect_equal(
    result$withdraw$MATCH_DETAIL,
    "School not found in class list"
  )
  
})


testthat::test_that("Alternative school identifiers are supported", {
  
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
  
  result <- .apply_withdrawal_new(
    class_list,
    withdraw,
    school_id_column = "est_wonde_id"
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("", "Y")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    2L
  )
  
  result2 <- .apply_withdrawal_new(
    class_list,
    withdraw,
    school_id_column = "EST_WONDE_ID"
  )
  
  testthat::expect_identical(
    result2$class_list$WITHDRAWN,
    c("", "Y")
  )
  
})


testthat::test_that("Internal class-list column names are supported", {
  
  reset_column_names()
  
  class_list <- data.frame(
    est_id = "A",
    sfn = "Alice",
    sln = "Smith",
    withdrawn = ""
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_equal(
    result$class_list$WITHDRAWN,
    "Y"
  )
  
  testthat::expect_true(
    all(c(
      "EST_ID",
      "PUPIL_FIRST_NAME",
      "PUPIL_LAST_NAME"
    ) %in% names(result$class_list))
  )
  
})


testthat::test_that("Custom configured column names are supported", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    est_id = "SCHOOL_IDENTIFIER",
    sfn = "FIRST_NAME",
    sln = "LAST_NAME",
    withdrawn = "WITHDRAWAL_STATUS"
  )
  
  class_list <- data.frame(
    SCHOOL_IDENTIFIER = "A",
    FIRST_NAME = "Alice",
    LAST_NAME = "Smith"
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(
    class_list,
    withdraw,
    school_id_column = "est_id"
  )
  
  testthat::expect_equal(
    result$class_list$WITHDRAWAL_STATUS,
    "Y"
  )
  
  testthat::expect_true(
    "SCHOOL_IDENTIFIER" %in% names(result$class_list)
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    1L
  )
  
})


testthat::test_that("Withdrawal responses are filtered", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = c(
      "I want to withdraw my child from the study",
      "I consent to participate"
    ),
    "Your child's first name" = c("Alice", "Bob"),
    "Your child's surname" = c("Smith", "Jones"),
    EstablishmentID = c("A", "A"),
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "")
  )
  
  testthat::expect_equal(
    nrow(result$withdraw),
    1L
  )
  
})


testthat::test_that("Missing withdrawal form columns produce an error", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  withdraw <- data.frame(
    EstablishmentID = "A"
  )
  
  testthat::expect_error(
    .apply_withdrawal_new(class_list, withdraw),
    "Missing withdrawal form columns"
  )
  
})


testthat::test_that("Missing class-list columns produce an error", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice"
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  testthat::expect_error(
    .apply_withdrawal_new(class_list, withdraw),
    "Missing class list columns"
  )
  
})


testthat::test_that("Invalid school_id_column produces an error", {
  
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
    .apply_withdrawal_new(
      class_list,
      withdraw,
      school_id_column = "INVALID_COLUMN"
    ),
    "Unknown school identifier column"
  )
  
})


testthat::test_that("Input class list is not modified", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  original <- class_list
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_equal(class_list, original)
  
})


# ============================================================
# Preservation of existing withdrawal status
# ============================================================

testthat::test_that("Existing withdrawal flags are preserved", {
  
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
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "Y", NA_character_, "N")
  )
  
  testthat::expect_identical(
    class_list$WITHDRAWN,
    c("", "Y", NA_character_, "N")
  )
  
})


testthat::test_that("Existing withdrawn students remain withdrawn", {
  
  reset_column_names()
  
  class_list <- data.frame(
    EST_ID = rep("A", 3),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Charlie"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Brown"),
    WITHDRAWN = c("Y", "", "Y")
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Bob",
    "Your child's surname" = "Jones",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "Y", "Y")
  )
  
  testthat::expect_equal(
    result$withdraw$CLASS_LIST_INDEX,
    2L
  )
  
})


testthat::test_that("Custom withdrawal status column is preserved", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    withdrawn = "WITHDRAWAL_STATUS"
  )
  
  class_list <- data.frame(
    EST_ID = rep("A", 3),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Charlie"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Brown"),
    WITHDRAWAL_STATUS = c("Y", "", NA_character_)
  )
  
  withdraw <- data.frame(
    "Type of withdrawal" = "I want to withdraw my child from the study",
    "Your child's first name" = "Bob",
    "Your child's surname" = "Jones",
    EstablishmentID = "A",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_new(class_list, withdraw)
  
  testthat::expect_true(
    "WITHDRAWAL_STATUS" %in% names(result$class_list)
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWAL_STATUS,
    c("Y", "Y", NA_character_)
  )
  
  testthat::expect_false(
    "WITHDRAWN" %in% names(result$class_list)
  )
  
})
