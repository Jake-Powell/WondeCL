
# ============================================================
# Tests: summarise_class_sizes()
# ============================================================

testthat::test_that("Class sizes and percentages are correct", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 5),
    CLASS_NAME = c("C1", "C1", "C2", "C2", "C2")
  )
  
  result <- summarise_class_sizes(df)
  result <- result[order(result$CLASS_NAME), ]
  
  testthat::expect_equal(
    result$STUDENTS_IN_CLASS,
    c(2L, 3L)
  )
  
  testthat::expect_equal(
    result$TOTAL_STUDENTS_IN_SCHOOL,
    c(5L, 5L)
  )
  
  testthat::expect_equal(
    result$PERCENT_OF_SCHOOL,
    c(40, 60)
  )
})


testthat::test_that("All returned column names are uppercase snake case", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "B"),
    CLASS_NAME = c("C1", "C1", "C2")
  )
  
  result <- summarise_class_sizes(df)
  
  testthat::expect_identical(
    names(result),
    c(
      "EST_ID",
      "CLASS_NAME",
      "STUDENTS_IN_CLASS",
      "TOTAL_STUDENTS_IN_SCHOOL",
      "PERCENT_OF_SCHOOL"
    )
  )
  
  testthat::expect_true(
    all(grepl(
      "^[A-Z][A-Z0-9_]*$",
      names(result)
    ))
  )
})


testthat::test_that("Multiple establishment columns are used together", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 4),
    EST_WONDE_NAME = c(
      "School One", "School One",
      "School Two", "School Two"
    ),
    CLASS_NAME = c("C1", "C1", "C1", "C2")
  )
  
  result <- summarise_class_sizes(df)
  
  testthat::expect_equal(nrow(result), 3L)
  
  testthat::expect_true(
    all(c("EST_ID", "EST_WONDE_NAME") %in% names(result))
  )
  
  school_one <- result[
    result$EST_WONDE_NAME == "School One", ,
    drop = FALSE
  ]
  
  school_two <- result[
    result$EST_WONDE_NAME == "School Two", ,
    drop = FALSE
  ]
  
  testthat::expect_equal(
    school_one$TOTAL_STUDENTS_IN_SCHOOL,
    2L
  )
  
  testthat::expect_true(
    all(school_two$TOTAL_STUDENTS_IN_SCHOOL == 2L)
  )
  
  testthat::expect_equal(
    sum(school_two$PERCENT_OF_SCHOOL),
    100
  )
})


testthat::test_that("Multiple class columns are used together", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 5),
    CLASS_NAME = c("C1", "C1", "C1", "C2", "C2"),
    CLASS_WONDE_NAME = c("W1", "W1", "W2", "W2", "W2")
  )
  
  result <- summarise_class_sizes(df)
  
  testthat::expect_equal(nrow(result), 3L)
  
  testthat::expect_true(
    all(c("CLASS_NAME", "CLASS_WONDE_NAME") %in%
          names(result))
  )
  
  testthat::expect_equal(
    sort(result$STUDENTS_IN_CLASS),
    c(1L, 2L, 2L)
  )
  
  testthat::expect_true(
    all(result$TOTAL_STUDENTS_IN_SCHOOL == 5L)
  )
  
  testthat::expect_equal(
    sum(result$PERCENT_OF_SCHOOL),
    100
  )
})


testthat::test_that("Multiple school and class columns work together", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A", "B", "B"),
    EST_WONDE_NAME = c(
      "School A", "School A", "School A",
      "School B", "School B"
    ),
    CLASS_NAME = c("C1", "C1", "C2", "C1", "C1"),
    CLASS_WONDE_NAME = c("W1", "W1", "W2", "W3", "W3")
  )
  
  result <- summarise_class_sizes(df)
  
  testthat::expect_equal(nrow(result), 3L)
  
  testthat::expect_setequal(
    names(result),
    c(
      "EST_ID",
      "EST_WONDE_NAME",
      "CLASS_NAME",
      "CLASS_WONDE_NAME",
      "STUDENTS_IN_CLASS",
      "TOTAL_STUDENTS_IN_SCHOOL",
      "PERCENT_OF_SCHOOL"
    )
  )
  
  school_a <- result[result$EST_ID == "A", ]
  school_b <- result[result$EST_ID == "B", ]
  
  testthat::expect_equal(
    sort(school_a$STUDENTS_IN_CLASS),
    c(1L, 2L)
  )
  
  testthat::expect_true(
    all(school_a$TOTAL_STUDENTS_IN_SCHOOL == 3L)
  )
  
  testthat::expect_equal(
    school_b$STUDENTS_IN_CLASS,
    2L
  )
  
  testthat::expect_equal(
    school_b$PERCENT_OF_SCHOOL,
    100
  )
})


testthat::test_that("Falls back to EST_WONDE_NAME", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = c("School A", "School A", "School B"),
    CLASS_NAME = c("C1", "C1", "C2")
  )
  
  result <- summarise_class_sizes(df)
  
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true("EST_WONDE_NAME" %in% names(result))
  testthat::expect_false("EST_ID" %in% names(result))
  
  testthat::expect_equal(
    sort(result$STUDENTS_IN_CLASS),
    c(1L, 2L)
  )
})


testthat::test_that("Falls back to CLASS_WONDE_NAME", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A"),
    CLASS_WONDE_NAME = c("W1", "W1", "W2")
  )
  
  result <- summarise_class_sizes(df)
  
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true("CLASS_WONDE_NAME" %in% names(result))
  testthat::expect_false("CLASS_NAME" %in% names(result))
  
  testthat::expect_equal(
    sort(result$STUDENTS_IN_CLASS),
    c(1L, 2L)
  )
})


testthat::test_that("Both fallback columns work together", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = c("School A", "School A"),
    CLASS_WONDE_NAME = c("W1", "W2")
  )
  
  result <- summarise_class_sizes(df)
  
  testthat::expect_equal(nrow(result), 2L)
  
  testthat::expect_true(
    all(c("EST_WONDE_NAME", "CLASS_WONDE_NAME") %in%
          names(result))
  )
  
  testthat::expect_true(
    all(result$TOTAL_STUDENTS_IN_SCHOOL == 2L)
  )
})


testthat::test_that("Explicit grouping columns override defaults", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A"),
    EST_WONDE_NAME = c(
      "School One", "School Two", "School Two"
    ),
    CLASS_NAME = c("C1", "C1", "C1"),
    CLASS_WONDE_NAME = c("W1", "W2", "W3")
  )
  
  result <- summarise_class_sizes(
    df,
    establishment_cols = "est_id",
    class_cols = "class_name"
  )
  
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$STUDENTS_IN_CLASS, 3L)
  testthat::expect_equal(result$PERCENT_OF_SCHOOL, 100)
  
  testthat::expect_identical(
    names(result),
    c(
      "EST_ID",
      "CLASS_NAME",
      "STUDENTS_IN_CLASS",
      "TOTAL_STUDENTS_IN_SCHOOL",
      "PERCENT_OF_SCHOOL"
    )
  )
})


testthat::test_that("Configured names can be passed explicitly", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "B"),
    CLASS_NAME = c("C1", "C1", "C2")
  )
  
  result <- summarise_class_sizes(
    df,
    establishment_cols = "EST_ID",
    class_cols = "CLASS_NAME"
  )
  
  testthat::expect_equal(nrow(result), 2L)
  
  testthat::expect_true(
    all(c("EST_ID", "CLASS_NAME") %in% names(result))
  )
})


testthat::test_that("Additional grouping columns are supported", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A"),
    EST_CATEGORY = c("Primary", "Primary", "Secondary"),
    CLASS_NAME = c("C1", "C1", "C1"),
    CLASS_GROUP = c("G1", "G1", "G2")
  )
  
  result <- summarise_class_sizes(
    df,
    establishment_cols = c("est_id", "EST_CATEGORY"),
    class_cols = c("class_name", "CLASS_GROUP")
  )
  
  testthat::expect_equal(nrow(result), 2L)
  
  testthat::expect_true(
    all(c(
      "EST_ID",
      "EST_CATEGORY",
      "CLASS_NAME",
      "CLASS_GROUP"
    ) %in% names(result))
  )
})


testthat::test_that("Distinct student IDs are counted within classes", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 5),
    CLASS_NAME = c("C1", "C1", "C1", "C2", "C2"),
    OME_ID = c("S1", "S1", "S2", "S3", "S4")
  )
  
  result <- summarise_class_sizes(
    df,
    unique_id_col = "ome_id"
  )
  
  result <- result[order(result$CLASS_NAME), ]
  
  testthat::expect_equal(
    result$STUDENTS_IN_CLASS,
    c(2L, 2L)
  )
  
  testthat::expect_equal(
    result$TOTAL_STUDENTS_IN_SCHOOL,
    c(4L, 4L)
  )
  
  testthat::expect_equal(
    result$PERCENT_OF_SCHOOL,
    c(50, 50)
  )
})


testthat::test_that("Configured unique ID name works", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 3),
    CLASS_NAME = rep("C1", 3),
    UPN = c("U1", "U1", "U2")
  )
  
  result <- summarise_class_sizes(
    df,
    unique_id_col = "UPN"
  )
  
  testthat::expect_equal(
    result$STUDENTS_IN_CLASS,
    2L
  )
})


testthat::test_that("Missing unique ID falls back to row counts", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_NAME = c("C1", "C1")
  )
  
  result <- summarise_class_sizes(
    df,
    unique_id_col = "ome_id"
  )
  
  testthat::expect_equal(
    result$STUDENTS_IN_CLASS,
    2L
  )
})


testthat::test_that("NA counts as one distinct student ID", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 3),
    CLASS_NAME = rep("C1", 3),
    OME_ID = c("S1", NA_character_, NA_character_)
  )
  
  result <- summarise_class_sizes(
    df,
    unique_id_col = "ome_id"
  )
  
  testthat::expect_equal(
    result$STUDENTS_IN_CLASS,
    2L
  )
})


testthat::test_that("Students in multiple classes count per class", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 4),
    CLASS_NAME = c("C1", "C1", "C2", "C2"),
    OME_ID = c("S1", "S2", "S1", "S3")
  )
  
  result <- summarise_class_sizes(
    df,
    unique_id_col = "ome_id"
  )
  
  testthat::expect_true(
    all(result$TOTAL_STUDENTS_IN_SCHOOL == 4L)
  )
  
  testthat::expect_equal(
    sum(result$STUDENTS_IN_CLASS),
    4L
  )
})


testthat::test_that("Custom column configuration is supported", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    est_id = "SchoolIdentifier",
    est_wonde_name = "SchoolDisplayName",
    class_name = "ClassIdentifier",
    c_wonde_name = "WondeClassName",
    ome_id = "StudentIdentifier"
  )
  
  df <- data.frame(
    SchoolIdentifier = c("A", "A", "A"),
    SchoolDisplayName = rep("School A", 3),
    ClassIdentifier = c("C1", "C1", "C2"),
    WondeClassName = c("W1", "W1", "W2"),
    StudentIdentifier = c("S1", "S1", "S2")
  )
  
  result <- summarise_class_sizes(
    df,
    unique_id_col = "ome_id"
  )
  
  testthat::expect_equal(nrow(result), 2L)
  
  testthat::expect_identical(
    names(result),
    c(
      "SCHOOL_IDENTIFIER",
      "SCHOOL_DISPLAY_NAME",
      "CLASS_IDENTIFIER",
      "WONDE_CLASS_NAME",
      "STUDENTS_IN_CLASS",
      "TOTAL_STUDENTS_IN_SCHOOL",
      "PERCENT_OF_SCHOOL"
    )
  )
  
  testthat::expect_true(
    all(result$TOTAL_STUDENTS_IN_SCHOOL == 2L)
  )
})


testthat::test_that("Internal column names are supported", {
  
  reset_column_names()
  
  df <- data.frame(
    est_id = c("A", "A"),
    class_name = c("C1", "C1"),
    ome_id = c("S1", "S2")
  )
  
  result <- summarise_class_sizes(
    df,
    unique_id_col = "ome_id"
  )
  
  testthat::expect_true(
    all(c("EST_ID", "CLASS_NAME") %in% names(result))
  )
  
  testthat::expect_equal(
    result$STUDENTS_IN_CLASS,
    2L
  )
})


testthat::test_that("Missing establishment identifiers produce an error", {
  
  reset_column_names()
  
  df <- data.frame(CLASS_NAME = "C1")
  
  testthat::expect_error(
    summarise_class_sizes(df),
    "No establishment identifier columns found"
  )
})


testthat::test_that("Missing class identifiers produce an error", {
  
  reset_column_names()
  
  df <- data.frame(EST_ID = "A")
  
  testthat::expect_error(
    summarise_class_sizes(df),
    "No class identifier columns found"
  )
})


testthat::test_that("Invalid grouping arguments produce errors", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1"
  )
  
  for (invalid in list(
    character(0),
    NA_character_,
    1,
    c("est_id", "")
  )) {
    
    testthat::expect_error(
      summarise_class_sizes(
        df,
        establishment_cols = invalid
      ),
      "establishment_cols must be"
    )
    
    testthat::expect_error(
      summarise_class_sizes(
        df,
        class_cols = invalid
      ),
      "class_cols must be"
    )
  }
})


testthat::test_that("Invalid unique ID arguments produce errors", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1"
  )
  
  for (invalid in list(
    character(0),
    NA_character_,
    c("UPN", "OME_ID"),
    1
  )) {
    
    testthat::expect_error(
      summarise_class_sizes(
        df,
        unique_id_col = invalid
      ),
      "unique_id_col must be"
    )
  }
})


testthat::test_that("Original class list is not modified", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_NAME = c("C1", "C2")
  )
  
  original <- df
  
  result <- summarise_class_sizes(df)
  
  testthat::expect_identical(df, original)
  testthat::expect_equal(nrow(result), 2L)
})
