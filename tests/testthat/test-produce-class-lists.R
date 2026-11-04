
# ============================================================
# Tests: produce_class_lists()
# ============================================================

# ------------------------------------------------------------
# Test data helper
# ------------------------------------------------------------

make_produce_class_lists_data <- function() {
  
  data.frame(
    EST_ID = c("A", "A", "A", "A", "B"),
    EST_WONDE_ID = c("WA", "WA", "WA", "WA", "WB"),
    EST_WONDE_NAME = c(
      "School A", "School A", "School A",
      "School A", "School B"
    ),
    EST_WONDE_URN = c("111", "111", "111", "111", "222"),
    STUDENT_WONDE_ID = c("S1", "S1", "S2", "S2", "S1"),
    PUPIL_FIRST_NAME = c(
      "ALICE", "ALICE", "BOB", "BOB", "ALICE"
    ),
    PUPIL_LAST_NAME = c(
      "SMITH", "SMITH", "JONES", "JONES", "SMITH"
    ),
    DOB = c(
      "2012-01-01", "2012-01-01",
      "2012-02-02", "2012-02-02",
      "2012-03-03"
    ),
    UPN = c("U1", "U1", "U2", "U2", "U3"),
    CLASS_NAME = c(
      "Blue", "Red", "Green", "Yellow", "Orange"
    ),
    CLASS_WONDE_NAME = c(
      "Wonde Blue", "Wonde Red",
      "Wonde Green", "Wonde Yellow", "Wonde Orange"
    ),
    TEACHER_FULL_NAME = c(
      "Other Teacher",
      "Main Teacher",
      "First Teacher",
      "Second Teacher",
      "School B Teacher"
    ),
    TEACHER_FIRST_NAME = c(
      "Other", "Main", "First", "Second", "School B"
    ),
    TEACHER_LAST_NAME = rep("Teacher", 5),
    TEACHER_MAIN = c(FALSE, TRUE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
}


testthat::test_that("Default configured columns are returned", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  result <- produce_class_lists(df)
  
  expected_names <- c(
    "EST_ID",
    "EST_WONDE_NAME",
    "EST_WONDE_URN",
    "CLASS_NAME",
    "TEACHER_FULL_NAME",
    "PUPIL_FIRST_NAME",
    "PUPIL_LAST_NAME",
    "DOB",
    "UPN",
    "TEACHER_FIRST_NAME",
    "TEACHER_LAST_NAME"
  )
  
  testthat::expect_identical(
    names(result),
    expected_names
  )
  
  testthat::expect_equal(
    nrow(result),
    3L
  )
  
  testthat::expect_false(
    "STUDENT_WONDE_ID" %in% names(result)
  )
  
  testthat::expect_false(
    "TEACHER_MAIN" %in% names(result)
  )
  
})


testthat::test_that("Multiple classes are combined per student", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    result$CLASS_NAME,
    c(
      "Blue, Red",
      "Green, Yellow",
      "Orange"
    )
  )
  
  testthat::expect_equal(
    result$PUPIL_FIRST_NAME,
    c("Alice", "Bob", "Alice")
  )
  
  testthat::expect_equal(
    result$PUPIL_LAST_NAME,
    c("Smith", "Jones", "Smith")
  )
  
})


testthat::test_that("Duplicate class names are only included once", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  # Student S1 appears in the same class more than once.
  df$CLASS_NAME[2] <- "Blue"
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    result$CLASS_NAME[1],
    "Blue"
  )
  
})


testthat::test_that("Main teacher is prioritised", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  result <- produce_class_lists(df)
  
  # Student S1 has two teacher records.
  # The second record is marked as the main teacher.
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[1],
    "Main Teacher"
  )
  
  testthat::expect_equal(
    result$TEACHER_FIRST_NAME[1],
    "Main"
  )
  
  testthat::expect_equal(
    result$TEACHER_LAST_NAME[1],
    "Teacher"
  )
  
  # Both classes are retained independently.
  testthat::expect_equal(
    result$CLASS_NAME[1],
    "Blue, Red"
  )
  
})


testthat::test_that("First teacher is selected when no main teacher exists", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  result <- produce_class_lists(df)
  
  # Student S2 has two non-main teachers.
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[2],
    "First Teacher"
  )
  
  testthat::expect_equal(
    result$TEACHER_FIRST_NAME[2],
    "First"
  )
  
})


testthat::test_that("Multiple main teachers use the first record", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  df$TEACHER_MAIN[1:2] <- TRUE
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[1],
    "Other Teacher"
  )
  
  testthat::expect_equal(
    result$TEACHER_FIRST_NAME[1],
    "Other"
  )
  
})


testthat::test_that("Missing main teacher column is supported", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  df$TEACHER_MAIN <- NULL
  
  result <- produce_class_lists(df)
  
  # Without main teacher information, first record wins.
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[1],
    "Other Teacher"
  )
  
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[2],
    "First Teacher"
  )
  
})


testthat::test_that("Missing main teacher values are treated as FALSE", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  df$TEACHER_MAIN[1] <- NA
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[1],
    "Main Teacher"
  )
  
})


testthat::test_that("Students with the same ID in different schools remain separate", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  result <- produce_class_lists(df)
  
  # Student S1 exists in both schools.
  testthat::expect_equal(
    nrow(result),
    3L
  )
  
  testthat::expect_equal(
    result$EST_ID,
    c("A", "A", "B")
  )
  
  testthat::expect_equal(
    result$CLASS_NAME[1],
    "Blue, Red"
  )
  
  testthat::expect_equal(
    result$CLASS_NAME[3],
    "Orange"
  )
  
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[3],
    "School B Teacher"
  )
  
})


testthat::test_that("Missing teacher names use default values", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  # Modify the selected main teacher.
  df$TEACHER_FULL_NAME[2] <- NA_character_
  df$TEACHER_FIRST_NAME[2] <- ""
  df$TEACHER_LAST_NAME[2] <- NA_character_
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[1],
    "Reception teacher"
  )
  
  testthat::expect_equal(
    result$TEACHER_FIRST_NAME[1],
    "Reception"
  )
  
  testthat::expect_equal(
    result$TEACHER_LAST_NAME[1],
    "Teacher"
  )
  
})


testthat::test_that("Custom default teacher names are supported", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  df$TEACHER_FULL_NAME[2] <- ""
  df$TEACHER_FIRST_NAME[2] <- NA_character_
  df$TEACHER_LAST_NAME[2] <- ""
  
  result <- produce_class_lists(
    df,
    default_teacher = "Unknown Teacher",
    default_teacher_FN = "Unknown",
    default_teacher_SN = "Teacher"
  )
  
  testthat::expect_equal(
    result$TEACHER_FULL_NAME[1],
    "Unknown Teacher"
  )
  
  testthat::expect_equal(
    result$TEACHER_FIRST_NAME[1],
    "Unknown"
  )
  
  testthat::expect_equal(
    result$TEACHER_LAST_NAME[1],
    "Teacher"
  )
  
})


testthat::test_that("First non-missing student information is retained", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  df$PUPIL_FIRST_NAME[1] <- NA_character_
  df$PUPIL_LAST_NAME[1] <- ""
  df$DOB[1] <- NA_character_
  df$UPN[1] <- ""
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    result$PUPIL_FIRST_NAME[1],
    "Alice"
  )
  
  testthat::expect_equal(
    result$PUPIL_LAST_NAME[1],
    "Smith"
  )
  
  testthat::expect_equal(
    result$DOB[1],
    "2012-01-01"
  )
  
  testthat::expect_equal(
    result$UPN[1],
    "U1"
  )
  
})


testthat::test_that("Student names are converted to title case", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    result$PUPIL_FIRST_NAME,
    c("Alice", "Bob", "Alice")
  )
  
  testthat::expect_equal(
    result$PUPIL_LAST_NAME,
    c("Smith", "Jones", "Smith")
  )
  
})


testthat::test_that("Alternative class-name column is supported", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  result <- produce_class_lists(
    df,
    class_name_column = "c_wonde_name"
  )
  
  testthat::expect_equal(
    result$CLASS_NAME,
    c(
      "Wonde Blue, Wonde Red",
      "Wonde Green, Wonde Yellow",
      "Wonde Orange"
    )
  )
  
})


testthat::test_that("Configured class-name column can be supplied", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  result <- produce_class_lists(
    df,
    class_name_column = "CLASS_WONDE_NAME"
  )
  
  testthat::expect_equal(
    result$CLASS_NAME,
    c(
      "Wonde Blue, Wonde Red",
      "Wonde Green, Wonde Yellow",
      "Wonde Orange"
    )
  )
  
})


testthat::test_that("Internal column names are supported", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  cols <- get_column_names()
  
  # Convert input names to internal keys.
  names(df) <- vapply(names(df), function(x) {
    index <- match(x, unname(cols))
    
    if (!is.na(index)) {
      names(cols)[index]
    } else {
      x
    }
  }, character(1))
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    nrow(result),
    3L
  )
  
  testthat::expect_true(
    all(c(
      "EST_ID",
      "CLASS_NAME",
      "TEACHER_FULL_NAME",
      "PUPIL_FIRST_NAME",
      "PUPIL_LAST_NAME"
    ) %in% names(result))
  )
  
  testthat::expect_false(
    "est_id" %in% names(result)
  )
  
})


testthat::test_that("Custom configured column names are supported", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  # Start with internal columns so they can be mapped to
  # the new configuration.
  df <- make_produce_class_lists_data()
  df <- rename_to_internal_names(df)
  
  set_column_names(
    est_id = "SCHOOL_IDENTIFIER",
    est_wonde_name = "SCHOOL_NAME",
    sfn = "STUDENT_FIRST",
    sln = "STUDENT_LAST",
    class_name = "MY_CLASS_NAME",
    t_full = "MY_TEACHER",
    tfn = "MY_TEACHER_FIRST",
    tln = "MY_TEACHER_LAST"
  )
  
  # Convert to the custom configured names.
  df <- rename_to_configured_names(df)
  
  result <- produce_class_lists(
    df,
    class_name_column = "MY_CLASS_NAME"
  )
  
  testthat::expect_true(
    all(c(
      "SCHOOL_IDENTIFIER",
      "SCHOOL_NAME",
      "STUDENT_FIRST",
      "STUDENT_LAST",
      "MY_CLASS_NAME",
      "MY_TEACHER",
      "MY_TEACHER_FIRST",
      "MY_TEACHER_LAST"
    ) %in% names(result))
  )
  
  testthat::expect_equal(
    result$MY_CLASS_NAME,
    c(
      "Blue, Red",
      "Green, Yellow",
      "Orange"
    )
  )
  
  testthat::expect_equal(
    result$MY_TEACHER[1],
    "Main Teacher"
  )
  
  testthat::expect_equal(
    result$STUDENT_FIRST,
    c("Alice", "Bob", "Alice")
  )
  
})


testthat::test_that("Output is sorted by school, class and student", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  # Reorder the input.
  df <- df[c(5, 4, 2, 3, 1), , drop = FALSE]
  
  result <- produce_class_lists(df)
  
  testthat::expect_equal(
    result$EST_WONDE_NAME,
    c("School A", "School A", "School B")
  )
  
  testthat::expect_equal(
    result$CLASS_NAME,
    c(
      "Red, Blue",
      "Yellow, Green",
      "Orange"
    )
  )
  
  # Class aggregation preserves first-seen order,
  # even when the final rows are sorted.
  testthat::expect_equal(
    result$PUPIL_FIRST_NAME,
    c("Alice", "Bob", "Alice")
  )
  
})


testthat::test_that("Missing class names are excluded from aggregation", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  df$CLASS_NAME[1] <- NA_character_
  
  result <- produce_class_lists(df)
  
  index <- which(
    result$EST_ID == "A" &
      result$PUPIL_FIRST_NAME == "Alice" &
      result$PUPIL_LAST_NAME == "Smith"
  )
  
  testthat::expect_length(index, 1L)
  
  testthat::expect_equal(
    result$CLASS_NAME[index],
    "Red"
  )
  
})


testthat::test_that("Missing required columns produce an error", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  df$STUDENT_WONDE_ID <- NULL
  
  testthat::expect_error(
    produce_class_lists(df),
    "Missing required columns"
  )
  
  testthat::expect_error(
    produce_class_lists(df),
    "s_wonde_id"
  )
  
})


testthat::test_that("Missing teacher columns produce an error", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  df$TEACHER_FULL_NAME <- NULL
  
  testthat::expect_error(
    produce_class_lists(df),
    "Missing required columns"
  )
  
})


testthat::test_that("Unknown class-name column produces an error", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  testthat::expect_error(
    produce_class_lists(
      df,
      class_name_column = "UNKNOWN_CLASS"
    ),
    "Unknown class_name_column"
  )
  
})


testthat::test_that("Original input is not modified", {
  
  reset_column_names()
  
  df <- make_produce_class_lists_data()
  
  original <- df
  
  result <- produce_class_lists(df)
  
  testthat::expect_identical(
    df,
    original
  )
  
  testthat::expect_equal(
    nrow(result),
    3L
  )
  
})

