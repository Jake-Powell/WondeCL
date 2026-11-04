
# ============================================================
# Tests: remove_school_from_class_list()
# ============================================================

testthat::test_that("Schools can be removed by name", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School A", "School B", "School C"),
    URN = c("111", "111", "222", "333"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Chris", "Diana")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "School A"
  )
  
  testthat::expect_equal(nrow(result), 2L)
  
  testthat::expect_equal(
    result$EST_NAME,
    c("School B", "School C")
  )
  
  testthat::expect_equal(
    result$PUPIL_FIRST_NAME,
    c("Chris", "Diana")
  )
  
})


testthat::test_that("Schools can be removed by URN", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B", "School C"),
    URN = c("111", "222", "333")
  )
  
  result <- remove_school_from_class_list(
    df,
    URNs = c(111, 333)
  )
  
  testthat::expect_equal(
    result$EST_NAME,
    "School B"
  )
  
  testthat::expect_equal(
    result$URN,
    "222"
  )
  
})


testthat::test_that("Names and URNs are combined using OR matching", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B", "School C"),
    URN = c("111", "222", "333")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "School A",
    URNs = 333
  )
  
  testthat::expect_equal(
    result$EST_NAME,
    "School B"
  )
  
})


testthat::test_that("School names preferentially use est_name", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B"),
    EST_WONDE_NAME = c("Old A", "Old B"),
    URN = c("111", "222")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "School A"
  )
  
  testthat::expect_equal(
    result$EST_NAME,
    "School B"
  )
  
  testthat::expect_equal(
    result$EST_WONDE_NAME,
    "Old B"
  )
  
})


testthat::test_that("Falls back to est_wonde_name when needed", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = c("School A", "School B"),
    URN = c("111", "222")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "School A"
  )
  
  testthat::expect_equal(
    result$EST_WONDE_NAME,
    "School B"
  )
  
})


testthat::test_that("URN-only removal does not require school names", {
  
  reset_column_names()
  
  df <- data.frame(
    URN = c("111", "222", "333"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Chris")
  )
  
  result <- remove_school_from_class_list(
    df,
    URNs = "222"
  )
  
  testthat::expect_equal(
    result$URN,
    c("111", "333")
  )
  
})


testthat::test_that("School-name removal does not require URN", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "School A"
  )
  
  testthat::expect_equal(
    result$EST_NAME,
    "School B"
  )
  
})


testthat::test_that("Internal column names are supported", {
  
  reset_column_names()
  
  df <- data.frame(
    est_name = c("School A", "School B"),
    urn = c("111", "222"),
    sfn = c("Alice", "Bob")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "School A"
  )
  
  testthat::expect_true(
    all(c("EST_NAME", "URN", "PUPIL_FIRST_NAME") %in%
          names(result))
  )
  
  testthat::expect_equal(
    result$EST_NAME,
    "School B"
  )
  
})


testthat::test_that("Custom configured columns are supported", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    est_name = "SCHOOL_NAME",
    urn = "SCHOOL_URN",
    sfn = "CHILD_FIRST_NAME"
  )
  
  df <- data.frame(
    SCHOOL_NAME = c("School A", "School B"),
    SCHOOL_URN = c("111", "222"),
    CHILD_FIRST_NAME = c("Alice", "Bob")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "School A"
  )
  
  testthat::expect_identical(
    names(result),
    names(df)
  )
  
  testthat::expect_equal(
    result$SCHOOL_NAME,
    "School B"
  )
  
  testthat::expect_equal(
    result$CHILD_FIRST_NAME,
    "Bob"
  )
  
})


testthat::test_that("Explicit configured school column is supported", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B"),
    EST_WONDE_NAME = c("Old A", "Old B")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "Old A",
    SchoolNameColumn = "EST_WONDE_NAME"
  )
  
  testthat::expect_equal(
    result$EST_NAME,
    "School B"
  )
  
})


testthat::test_that("Explicit internal school column is supported", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B"),
    EST_WONDE_NAME = c("Old A", "Old B")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "Old A",
    SchoolNameColumn = "est_wonde_name"
  )
  
  testthat::expect_equal(
    result$EST_NAME,
    "School B"
  )
  
})


testthat::test_that("Custom URN identifier column is supported", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    urn = "SCHOOL_URN"
  )
  
  df <- data.frame(
    SCHOOL_URN = c("111", "222", "333")
  )
  
  result <- remove_school_from_class_list(
    df,
    URNs = "222",
    URNColumn = "SCHOOL_URN"
  )
  
  testthat::expect_equal(
    result$SCHOOL_URN,
    c("111", "333")
  )
  
})


testthat::test_that("Literal unconfigured columns can be selected", {
  
  reset_column_names()
  
  df <- data.frame(
    CUSTOM_SCHOOL = c("School A", "School B"),
    URN = c("111", "222")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "School A",
    SchoolNameColumn = "CUSTOM_SCHOOL"
  )
  
  testthat::expect_equal(
    result$CUSTOM_SCHOOL,
    "School B"
  )
  
})


testthat::test_that("No matching school preserves all rows", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B"),
    URN = c("111", "222")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = "Unknown"
  )
  
  testthat::expect_equal(result, df)
  
})


testthat::test_that("All schools can be removed", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B"),
    URN = c("111", "222")
  )
  
  result <- remove_school_from_class_list(
    df,
    URNs = c("111", "222")
  )
  
  testthat::expect_equal(nrow(result), 0L)
  
  testthat::expect_identical(
    names(result),
    names(df)
  )
  
})


testthat::test_that("NA identifiers are not treated as matches", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", NA, "School B"),
    URN = c("111", NA, "222")
  )
  
  result <- remove_school_from_class_list(
    df,
    school_names = c("School A", NA_character_),
    URNs = c("111", NA_character_)
  )
  
  testthat::expect_equal(nrow(result), 2L)
  
  testthat::expect_true(
    is.na(result$EST_NAME[1])
  )
  
  testthat::expect_equal(
    result$EST_NAME[2],
    "School B"
  )
  
})


testthat::test_that("Missing identifiers produce an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = "School A",
    URN = "111"
  )
  
  testthat::expect_error(
    remove_school_from_class_list(df),
    "At least one"
  )
  
})


testthat::test_that("Missing requested school column produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    URN = "111"
  )
  
  testthat::expect_error(
    remove_school_from_class_list(
      df,
      school_names = "School A"
    ),
    "Missing school-name column"
  )
  
})


testthat::test_that("Missing requested URN column produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = "School A"
  )
  
  testthat::expect_error(
    remove_school_from_class_list(
      df,
      URNs = "111"
    ),
    "URN column not found"
  )
  
})


testthat::test_that("Invalid inputs produce appropriate errors", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = "School A",
    URN = "111"
  )
  
  testthat::expect_error(
    remove_school_from_class_list(
      df,
      school_names = 123
    ),
    "school_names must be a character vector"
  )
  
  testthat::expect_error(
    remove_school_from_class_list(
      df,
      URNs = TRUE
    ),
    "URNs must be a character or numeric vector"
  )
  
  testthat::expect_error(
    remove_school_from_class_list(
      df,
      URNs = "111",
      verbose = NA
    ),
    "verbose must be TRUE or FALSE"
  )
  
  testthat::expect_error(
    remove_school_from_class_list(
      df,
      school_names = "School A",
      SchoolNameColumn = "UNKNOWN_COLUMN"
    ),
    "Unknown column"
  )
  
})


testthat::test_that("Original data frame is not modified", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_NAME = c("School A", "School B"),
    URN = c("111", "222")
  )
  
  original <- df
  
  result <- remove_school_from_class_list(
    df,
    URNs = "111"
  )
  
  testthat::expect_identical(df, original)
  
  testthat::expect_equal(
    result$EST_NAME,
    "School B"
  )
  
})

