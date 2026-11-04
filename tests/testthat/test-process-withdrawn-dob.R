
# ============================================================
# Tests: .process_withdrawn_dob()
# ============================================================

testthat::test_that("Missing DoB column returns NA", {
  
  df <- data.frame(
    Student = c("A", "B", "C")
  )
  
  result <- .process_withdrawn_dob(
    df,
    expected_years = c(2019, 2020)
  )
  
  testthat::expect_equal(
    result,
    rep(NA_character_, 3)
  )
  
})


testthat::test_that("Numeric date formats are parsed correctly", {
  
  df <- data.frame(
    "Your child's date of birth" = c(
      "01/03/2020",
      "01-03-2020",
      "01.03.2020",
      "01/03/20",
      "01032020"
    ),
    check.names = FALSE
  )
  
  result <- .process_withdrawn_dob(
    df,
    expected_years = 2020
  )
  
  testthat::expect_equal(
    result,
    rep("2020-03-01", 5)
  )
  
})


testthat::test_that("Dates with month names are parsed", {
  
  df <- data.frame(
    "Your child's date of birth" = c(
      "1 January 2020",
      "January 1 2020",
      "Jan 1 2020",
      "1st January 2020"
    ),
    check.names = FALSE
  )
  
  result <- .process_withdrawn_dob(
    df,
    expected_years = 2020
  )
  
  testthat::expect_equal(
    result,
    rep("2020-01-01", 4)
  )
  
})


testthat::test_that("Dates outside expected years are removed", {
  
  df <- data.frame(
    "Your child's date of birth" = c(
      "01/01/2018",
      "01/01/2019",
      "01/01/2020",
      "01/01/2021"
    ),
    check.names = FALSE
  )
  
  result <- .process_withdrawn_dob(
    df,
    expected_years = c(2019, 2020)
  )
  
  testthat::expect_identical(
    result,
    c(
      NA_character_,
      "2019-01-01",
      "2020-01-01",
      NA_character_
    )
  )
  
})


testthat::test_that("Invalid and missing dates return NA", {
  
  df <- data.frame(
    "Your child's date of birth" = c(
      NA,
      "",
      " ",
      "Not known",
      "Invalid date"
    ),
    check.names = FALSE
  )
  
  result <- .process_withdrawn_dob(
    df,
    expected_years = 2020
  )
  
  testthat::expect_true(all(is.na(result)))
  
})


testthat::test_that("Input data is not modified", {
  
  df <- data.frame(
    "Your child's date of birth" = c(
      "1st January 2020",
      "01/03/2020"
    ),
    check.names = FALSE
  )
  
  original <- df
  
  .process_withdrawn_dob(
    df,
    expected_years = 2020
  )
  
  testthat::expect_equal(df, original)
  
})


testthat::test_that("Invalid expected years produce an error", {
  
  df <- data.frame(
    "Your child's date of birth" = "01/01/2020",
    check.names = FALSE
  )
  
  testthat::expect_error(
    .process_withdrawn_dob(
      df,
      expected_years = numeric(0)
    ),
    "expected_years"
  )
  
})
