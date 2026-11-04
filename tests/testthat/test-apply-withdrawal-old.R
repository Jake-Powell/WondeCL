
# ============================================================
# Tests: .apply_withdrawal_old()
# ============================================================

testthat::test_that("Exact student names and school match correctly", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  
  links <- data.frame(
    EstablishmentName = c("Oakfield Primary", "Riverside Primary"),
    URN = c("123456", "654321")
  )
  
  school_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(school_file), add = TRUE)
  openxlsx::write.xlsx(links, school_file)
  
  class_list <- data.frame(
    URN = c("123456", "123456", "654321"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Smith"),
    DOB = c("2020-01-01", "2020-02-02", "2020-01-01"),
    WITHDRAWN = c("", "", "")
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    "Your child's date of birth" = "01/01/2020",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
  )
  
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
    "Oakfield Primary"
  )
  
  testthat::expect_true(
    all(c("IGNORE_RESULT", "CLASS_LIST_INDEX") %in%
          names(result$withdraw))
  )
  
  # The default hex_id mapping is PRINT_ID, not print_id.
  testthat::expect_true(
    all(c("PRINT_ID", "OME_ID") %in% names(result$class_list))
  )
  
})


testthat::test_that("Legacy report uses uppercase snake-case names", {
  
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
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
  )
  
  expected_names <- c(
    "FIRST_NAME",
    "LAST_NAME",
    "DOB",
    "SCHOOL_NAME",
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
  
})


testthat::test_that("Partial first name matching works", {
  
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
    URN = "123456",
    PUPIL_FIRST_NAME = "Anna Maria",
    PUPIL_LAST_NAME = "Smith"
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Anna",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
  )
  
  testthat::expect_equal(result$class_list$WITHDRAWN, "Y")
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Partial first name"
  )
  
})


testthat::test_that("Partial surname matching works", {
  
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
    URN = "123456",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith Jones"
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
  )
  
  testthat::expect_equal(result$class_list$WITHDRAWN, "Y")
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Partial last name, exact first name"
  )
  
})


testthat::test_that("Partial first and last name matching works", {
  
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
    URN = "123456",
    PUPIL_FIRST_NAME = "Anna Maria",
    PUPIL_LAST_NAME = "Smith Jones"
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Anna",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
  )
  
  testthat::expect_equal(result$class_list$WITHDRAWN, "Y")
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Partial last name, partial first name"
  )
  
})


testthat::test_that("Date of birth resolves a surname mismatch", {
  
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
    DOB = c("2020-01-01", "2020-02-02")
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Unknown",
    "Name of school" = "Oakfield Primary",
    "Your child's date of birth" = "01/01/2020",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file,
    expected_years = 2020
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "")
  )
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "DoB match"
  )
  
})


testthat::test_that("Unique surname-only matching works", {
  
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
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Unknown",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("Y", "")
  )
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "surname only"
  )
  
})


testthat::test_that("Duplicate exact matches are not withdrawn", {
  
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
    PUPIL_FIRST_NAME = c("Alice", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Smith")
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWN,
    c("", "")
  )
  
  testthat::expect_true(
    is.na(result$withdraw$CLASS_LIST_INDEX)
  )
  
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL,
    "Multiple first and last name"
  )
  
})


testthat::test_that("Existing withdrawal statuses are preserved", {
  
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
    URN = rep("123456", 4),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Charlie", "Diana"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Brown", "Evans"),
    WITHDRAWN = c("", "Y", NA_character_, "N")
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
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


testthat::test_that("Custom configured class-list names work", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    urn = "SCHOOL_URN",
    sfn = "FIRST_NAME",
    sln = "LAST_NAME",
    withdrawn = "WITHDRAWAL_STATUS"
  )
  
  links <- data.frame(
    EstablishmentName = "Oakfield Primary",
    URN = "123456"
  )
  
  school_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(school_file), add = TRUE)
  openxlsx::write.xlsx(links, school_file)
  
  class_list <- data.frame(
    SCHOOL_URN = c("123456", "123456"),
    FIRST_NAME = c("Alice", "Bob"),
    LAST_NAME = c("Smith", "Jones"),
    WITHDRAWAL_STATUS = c("", "Y")
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list,
    withdraw,
    school_link = school_file
  )
  
  testthat::expect_identical(
    result$class_list$WITHDRAWAL_STATUS,
    c("Y", "Y")
  )
  
  testthat::expect_true(
    "SCHOOL_URN" %in% names(result$class_list)
  )
  
  testthat::expect_false(
    "WITHDRAWN" %in% names(result$class_list)
  )
  
})


testthat::test_that("Custom school reference column names work", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  
  links <- data.frame(
    SCHOOL_NAME = c("Oakfield Primary", "Riverside Primary"),
    SCHOOL_URN = c("123456", "654321")
  )
  
  school_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(school_file), add = TRUE)
  openxlsx::write.xlsx(links, school_file)
  
  class_list <- data.frame(
    URN = c("123456", "654321"),
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
  
  result <- .apply_withdrawal_old(
    class_list = class_list,
    withdrawn_students = withdraw,
    school_link = school_file,
    reference_school_name_column = "SCHOOL_NAME",
    reference_school_id_column = "SCHOOL_URN"
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
    result$withdraw$MATCHED_SCHOOL_NAME,
    "Oakfield Primary"
  )
  
})


testthat::test_that("Missing custom reference columns produce an error", {
  
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
  
  testthat::expect_error(
    .apply_withdrawal_old(
      class_list,
      withdraw,
      school_link = school_file,
      reference_school_name_column = "INVALID_NAME",
      reference_school_id_column = "SCHOOL_URN"
    ),
    "Missing school reference columns"
  )
  
})


testthat::test_that("Missing class-list columns produce an error", {
  
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
    URN = "123456",
    PUPIL_FIRST_NAME = "Alice"
  )
  
  withdraw <- data.frame(
    "Your child's first name" = "Alice",
    "Your child's surname" = "Smith",
    "Name of school" = "Oakfield Primary",
    check.names = FALSE
  )
  
  testthat::expect_error(
    .apply_withdrawal_old(
      class_list,
      withdraw,
      school_link = school_file
    ),
    "Missing class list columns"
  )
  
})


# ============================================================
# Column configuration regression tests
# ============================================================

testthat::test_that("Legacy matching creates PRINT_ID via hex_id mapping", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  links <- data.frame(
    EstablishmentName = "Oakfield Primary",
    URN = "123456"
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
    `Your child's first name` = "Alice",
    `Your child's surname` = "Smith",
    `Name of school` = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list, withdraw, school_link = school_file
  )
  
  testthat::expect_true(
    all(c("WITHDRAWN", "PRINT_ID", "OME_ID") %in%
          names(result$class_list))
  )
  testthat::expect_false("print_id" %in% names(result$class_list))
  testthat::expect_equal(result$class_list$WITHDRAWN, "Y")
})


testthat::test_that("Legacy matching respects custom hex_id and ome_id names", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    hex_id = "CUSTOM_PRINT_ID",
    ome_id = "CUSTOM_STUDENT_ID",
    withdrawn = "WITHDRAWAL_STATUS"
  )
  
  links <- data.frame(
    EstablishmentName = "Oakfield Primary",
    URN = "123456"
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
    `Your child's first name` = "Alice",
    `Your child's surname` = "Smith",
    `Name of school` = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- .apply_withdrawal_old(
    class_list, withdraw, school_link = school_file
  )
  
  testthat::expect_true(
    all(c("CUSTOM_PRINT_ID", "CUSTOM_STUDENT_ID",
          "WITHDRAWAL_STATUS") %in% names(result$class_list))
  )
  testthat::expect_false("print_id" %in% names(result$class_list))
  testthat::expect_equal(result$class_list$WITHDRAWAL_STATUS, "Y")
})


testthat::test_that("Public router selects the legacy matching method", {
  
  testthat::skip_if_not_installed("openxlsx")
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  links <- data.frame(
    EstablishmentName = "Oakfield Primary",
    URN = "123456"
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
    `Your child's first name` = "Alice",
    `Your child's surname` = "Smith",
    `Name of school` = "Oakfield Primary",
    check.names = FALSE
  )
  
  result <- suppressMessages(
    apply_withdrawal(
      class_list,
      withdraw,
      school_dob_match = TRUE,
      legacy_school_link = school_file
    )
  )
  
  testthat::expect_equal(result$class_list$WITHDRAWN, "Y")
  testthat::expect_equal(result$withdraw$CLASS_LIST_INDEX, 1L)
  testthat::expect_match(
    result$withdraw$MATCH_DETAIL, "Exact first and last name"
  )
})
