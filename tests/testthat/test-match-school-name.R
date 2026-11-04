
# ============================================================
# Tests: .match_school_name()
# ============================================================

testthat::test_that("Exact school names are matched", {
  
  schools <- data.frame(
    EstablishmentName = c(
      "Lavandula Primary School",
      "Digitalis Purpurea Primary School"
    ),
    URN = c("123456", "654321")
  )
  
  result <- .match_school_name(
    "Lavandula Primary School",
    schools
  )
  
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$URN, "123456")
  testthat::expect_equal(
    result$SchoolName,
    "Lavandula Primary School"
  )
  
})


testthat::test_that("Exact matching ignores case and punctuation", {
  
  schools <- data.frame(
    EstablishmentName = "St. Mary's Primary School",
    URN = "123456"
  )
  
  result <- .match_school_name(
    "ST MARYS PRIMARY SCHOOL",
    schools
  )
  
  testthat::expect_equal(result$URN, "123456")
  
})


testthat::test_that("Partial names use shared word matching", {
  
  schools <- data.frame(
    EstablishmentName = c(
      "Lavandula Primary School",
      "Digitalis Purpurea Primary School",
      "Oakfield Academy"
    ),
    URN = c("123456", "654321", "111111")
  )
  
  result <- .match_school_name(
    "Digitalis Purpurea",
    schools
  )
  
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$URN, "654321")
  
})


testthat::test_that("Tied matches are all returned", {
  
  schools <- data.frame(
    EstablishmentName = c(
      "Lavandula Primary School",
      "Digitalis Primary School"
    ),
    URN = c("123456", "654321")
  )
  
  result <- .match_school_name(
    "Primary School",
    schools
  )
  
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_setequal(
    result$URN,
    c("123456", "654321")
  )
  
})


testthat::test_that("No shared words returns no match", {
  
  schools <- data.frame(
    EstablishmentName = c(
      "Lavandula Primary School",
      "Digitalis Primary School"
    ),
    URN = c("123456", "654321")
  )
  
  result <- .match_school_name(
    "Oakfield Academy",
    schools
  )
  
  testthat::expect_equal(nrow(result), 0L)
  
})


testthat::test_that("Missing school names return empty results", {
  
  schools <- data.frame(
    EstablishmentName = "Lavandula Primary School",
    URN = "123456"
  )
  
  result_na <- .match_school_name(NA_character_, schools)
  result_blank <- .match_school_name("", schools)
  
  testthat::expect_equal(nrow(result_na), 0L)
  testthat::expect_equal(nrow(result_blank), 0L)
  
})


testthat::test_that("Alternative school identifiers are supported", {
  
  schools <- data.frame(
    EST_NAME = c(
      "Lavandula Primary School",
      "Digitalis Primary School"
    ),
    EST_ID = c("A", "B")
  )
  
  result <- .match_school_name(
    "Lavandula Primary School",
    schools,
    school_name_column = "EST_NAME",
    school_id_column = "EST_ID"
  )
  
  testthat::expect_equal(result$URN, "A")
  testthat::expect_equal(
    result$SchoolName,
    "Lavandula Primary School"
  )
  
})


testthat::test_that("Internal lookup names are supported", {
  
  reset_column_names()
  
  schools <- data.frame(
    EST_WONDE_NAME = "Lavandula Primary School",
    EST_WONDE_URN = "123456"
  )
  
  result <- .match_school_name(
    "Lavandula Primary School",
    schools,
    school_name_column = "est_wonde_name",
    school_id_column = "est_wonde_urn"
  )
  
  testthat::expect_equal(result$URN, "123456")
  
})


testthat::test_that("Missing required columns produce an error", {
  
  schools <- data.frame(
    EstablishmentName = "Lavandula Primary School"
  )
  
  testthat::expect_error(
    .match_school_name(
      "Lavandula Primary School",
      schools
    ),
    "Missing required school reference columns"
  )
  
})


testthat::test_that("Input school reference data is unchanged", {
  
  schools <- data.frame(
    EstablishmentName = "Lavandula Primary School",
    URN = "123456"
  )
  
  original <- schools
  
  .match_school_name(
    "Lavandula Primary School",
    schools
  )
  
  testthat::expect_equal(schools, original)
  
})
