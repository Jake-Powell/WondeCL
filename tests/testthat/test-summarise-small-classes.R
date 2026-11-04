
# ============================================================
# Tests: summarise_small_classes()
# ============================================================

testthat::test_that("Small classes are summarised correctly", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = rep("School A", 7),
    URN = rep("123456", 7),
    CLASS_WONDE_NAME = c(
      "C1", "C1",
      "C2", "C2", "C2", "C2",
      "C3"
    )
  )
  
  result <- summarise_small_classes(
    df,
    threshold = 2
  )
  
  testthat::expect_equal(result$TotalClasses, 3L)
  testthat::expect_equal(result$SmallClasses, 2L)
  testthat::expect_equal(result$StudentsInSmallClasses, 3)
  testthat::expect_equal(result$SmallClassSizes, "2, 1")
  testthat::expect_equal(result$TotalStudents, 7)
  testthat::expect_equal(
    result$PercentInSmallClasses,
    round(100 * 3 / 7, 2)
  )
  
})


testthat::test_that("Distinct student IDs are counted within classes", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = rep("School A", 5),
    URN = rep("123456", 5),
    CLASS_WONDE_NAME = c("C1", "C1", "C1", "C2", "C2"),
    OME_ID = c("S1", "S1", "S2", "S3", "S4")
  )
  
  result <- summarise_small_classes(
    df,
    unique_id_col = "ome_id",
    threshold = 2
  )
  
  testthat::expect_equal(result$TotalStudents, 4)
  testthat::expect_equal(result$SmallClasses, 2)
  testthat::expect_equal(result$StudentsInSmallClasses, 4)
  
})


testthat::test_that("Configured identifier name works", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = rep("School A", 3),
    URN = rep("123456", 3),
    CLASS_WONDE_NAME = c("C1", "C1", "C1"),
    OME_ID = c("S1", "S1", "S2")
  )
  
  result <- summarise_small_classes(
    df,
    unique_id_col = "OME_ID",
    threshold = 2
  )
  
  testthat::expect_equal(result$TotalStudents, 2)
  testthat::expect_equal(result$SmallClasses, 1)
  
})


testthat::test_that("Custom column configuration works", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    est_wonde_name = "SCHO_NAME",
    urn = "SCHOOL_URN",
    c_wonde_name = "CLA_NAME",
    ome_id = "STUDENT_IDENTIFIER"
  )
  
  df <- data.frame(
    SCHO_NAME = rep("School A", 3),
    SCHOOL_URN = rep("123456", 3),
    CLA_NAME = c("C1", "C1", "C2"),
    STUDENT_IDENTIFIER = c("S1", "S2", "S3")
  )
  
  result <- summarise_small_classes(
    df,
    unique_id_col = "STUDENT_IDENTIFIER",
    threshold = 2
  )
  
  testthat::expect_true(
    all(c(
      "SCHO_NAME"
    ) %in% names(result))
  )
  
  testthat::expect_equal(result$TotalClasses, 2)
  testthat::expect_equal(result$SmallClasses, 2)
  testthat::expect_equal(result$TotalStudents, 3)
  
})


testthat::test_that("Internal column names are supported", {
  
  reset_column_names()
  
  df <- data.frame(
    est_wonde_name = rep("School A", 2),
    urn = rep("123456", 2),
    c_wonde_name = c("C1", "C1")
  )
  
  result <- summarise_small_classes(df)
  
  testthat::expect_true(
    all(c("EST_WONDE_NAME") %in% names(result))
  )
  
  testthat::expect_equal(result$TotalStudents, 2)
  
})



testthat::test_that("Invalid thresholds produce errors", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = "School A",
    URN = "123456",
    CLASS_WONDE_NAME = "C1"
  )
  
  for (x in list(-1, NA_real_, Inf, "three", c(2, 3))) {
    testthat::expect_error(
      summarise_small_classes(df, threshold = x),
      "threshold must be"
    )
  }
  
})



testthat::test_that("EST_ID and CLASS_NAME take priority", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A", "A"),
    EST_WONDE_NAME = c(
      "School One", "School Two",
      "School One", "School Two"
    ),
    CLASS_NAME = c("C1", "C1", "C2", "C2"),
    CLASS_WONDE_NAME = c("W1", "W2", "W1", "W2")
  )
  
  result <- summarise_small_classes(df, threshold = 2)
  
  # School grouping should use EST_ID, not EST_WONDE_NAME
  testthat::expect_equal(nrow(result), 1L)
  
  testthat::expect_equal(result$EST_ID, "A")
  testthat::expect_equal(result$TotalClasses, 2L)
  testthat::expect_equal(result$TotalStudents, 4L)
  
  testthat::expect_false(
    "EST_WONDE_NAME" %in% names(result)
  )
  
})


testthat::test_that("Falls back to EST_WONDE_NAME", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = c("School A", "School A", "School B"),
    CLASS_NAME = c("C1", "C1", "C2")
  )
  
  result <- summarise_small_classes(df)
  
  testthat::expect_equal(nrow(result), 2L)
  
  testthat::expect_true(
    "EST_WONDE_NAME" %in% names(result)
  )
  
  testthat::expect_false(
    "EST_ID" %in% names(result)
  )
  
})


testthat::test_that("Falls back to CLASS_WONDE_NAME", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A", "A"),
    CLASS_WONDE_NAME = c("W1", "W1", "W2", "W2")
  )
  
  result <- summarise_small_classes(df)
  
  testthat::expect_equal(result$TotalClasses, 2L)
  testthat::expect_equal(result$TotalStudents, 4L)
  
})


testthat::test_that("Both fallback columns can be used", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = c("School A", "School A"),
    CLASS_WONDE_NAME = c("W1", "W2")
  )
  
  result <- summarise_small_classes(df, threshold = 1)
  
  testthat::expect_equal(
    result$EST_WONDE_NAME,
    "School A"
  )
  
  testthat::expect_equal(result$TotalClasses, 2L)
  testthat::expect_equal(result$SmallClasses, 2L)
  
})


testthat::test_that("Preferred columns are used even with missing values", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", NA_character_),
    EST_WONDE_NAME = c("School A", "School B"),
    CLASS_NAME = c("C1", NA_character_),
    CLASS_WONDE_NAME = c("W1", "W2")
  )
  
  result <- summarise_small_classes(df)
  
  testthat::expect_true(
    "EST_ID" %in% names(result)
  )
  
  testthat::expect_equal(nrow(result), 2L)
  
  testthat::expect_equal(
    sum(result$TotalClasses),
    2L
  )
  
})


testthat::test_that("Missing both school identifiers produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    CLASS_NAME = "C1"
  )
  
  testthat::expect_error(
    summarise_small_classes(df),
    "Missing school identifier"
  )
  
})


testthat::test_that("Missing both class identifiers produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A"
  )
  
  testthat::expect_error(
    summarise_small_classes(df),
    "Missing class identifier"
  )
  
})


testthat::test_that("Custom configured priority columns work", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    est_id = "SCHOOL_IDENTIFIER",
    class_name = "MY_CLASS",
    ome_id = "STUDENT_IDENTIFIER"
  )
  
  df <- data.frame(
    SCHOOL_IDENTIFIER = c("A", "A", "A"),
    MY_CLASS = c("C1", "C1", "C2"),
    STUDENT_IDENTIFIER = c("S1", "S1", "S2")
  )
  
  result <- summarise_small_classes(
    df,
    unique_id_col = "ome_id",
    threshold = 1
  )
  
  testthat::expect_true(
    "SCHOOL_IDENTIFIER" %in% names(result)
  )
  
  testthat::expect_equal(result$TotalClasses, 2L)
  testthat::expect_equal(result$TotalStudents, 2L)
  
})
