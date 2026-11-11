# Place in tests/testthat/test-manual-class-list-edits.R
# Run with devtools::test(filter = "manual-class-list-edits")

# The package dataset already uses the default configured names.
# Avoid relying on column positions or randomly assigned surnames.
.make_manual_edit_fixture <- function() {
  df <- WondeCL::class_list_example
  required <- c(
    "EST_ID", "EST_NAME", "URN", "EST_WONDE_ID", "EST_WONDE_NAME",
    "CLASS_NAME", "CLASS_WONDE_NAME", "CLASS_WONDE_ID",
    "STUDENT_WONDE_ID", "PUPIL_FIRST_NAME", "PUPIL_LAST_NAME",
    "TEACHER_WONDE_ID", "TEACHER_FIRST_NAME",
    "TEACHER_LAST_NAME", "TEACHER_FULL_NAME", "DOB", "UPN"
  )
  testthat::expect_true(all(required %in% names(df)))
  df
}

.cn <- function(key) unname(WondeCL::get_column_names(key))

.school_class_rows <- function(df, school, class_name) {
  df[
    as.character(df[[.cn("est_id")]]) == school &
      as.character(df[[.cn("class_name")]]) == class_name,
    , drop = FALSE
  ]
}

# Select a real student from the fixture, without relying on seed-dependent
# pairings of randomly sampled surnames.
.example_student <- function(df, school = "A", class_name = "French") {
  rows <- .school_class_rows(df, school, class_name)
  stopifnot(nrow(rows) >= 1L)
  rows[1, , drop = FALSE]
}

testthat::test_that("fixture uses the updated configured example dataset", {
  WondeCL::reset_column_names()
  df <- .make_manual_edit_fixture()
  testthat::expect_equal(nrow(df), 60L)
  testthat::expect_setequal(unique(df$EST_ID), c("A", "B"))
  testthat::expect_equal(nrow(.school_class_rows(df, "A", "Fernleaf")), 15L)
  testthat::expect_equal(nrow(.school_class_rows(df, "A", "French")), 15L)
  testthat::expect_equal(nrow(.school_class_rows(df, "B", "Foxglove")), 30L)
  testthat::expect_equal(
    unique(.school_class_rows(df, "A", "Fernleaf")$CLASS_WONDE_ID),
    "WONDE-C001"
  )
  testthat::expect_false(anyDuplicated(df$STUDENT_WONDE_ID) > 0L)
})

testthat::test_that("add_student_to_school_class accepts URN and adds one student", {
  WondeCL::reset_column_names()
  df <- .make_manual_edit_fixture()
  out <- WondeCL::add_student_to_school_class(
    df, first_name = "Jimmy", last_name = "Powell",
    class_name = "French", dob = "1910-05-07",
    URN = 123456, verbose = FALSE
  )
  testthat::expect_equal(nrow(out), 61L)
  jimmy <- out[out$PUPIL_FIRST_NAME == "Jimmy" &
                 out$PUPIL_LAST_NAME == "Powell", , drop = FALSE]
  testthat::expect_equal(nrow(jimmy), 1L)
  testthat::expect_equal(jimmy$CLASS_NAME, "French")
  testthat::expect_equal(jimmy$EST_ID, "A")
  testthat::expect_equal(jimmy$DOB, "1910-05-07")
  testthat::expect_true(is.na(jimmy$STUDENT_WONDE_ID))
  testthat::expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
})

testthat::test_that("update_student_class takes class and teacher from same school", {
  WondeCL::reset_column_names()
  df <- .make_manual_edit_fixture()
  pupil <- .example_student(df, "A", "French")
  out <- WondeCL::update_student_class(
    df,
    first_name = pupil$PUPIL_FIRST_NAME,
    last_name = pupil$PUPIL_LAST_NAME,
    class_name = "French", new_class_name = "Fernleaf",
    EST_ID = "A"
  )
  changed <- out[out$STUDENT_WONDE_ID == pupil$STUDENT_WONDE_ID, , drop = FALSE]
  testthat::expect_equal(nrow(changed), 1L)
  testthat::expect_equal(changed$CLASS_NAME, "Fernleaf")
  testthat::expect_equal(changed$CLASS_WONDE_ID, "WONDE-C001")
  testthat::expect_equal(changed$TEACHER_FULL_NAME, "Miss Katherine")
  testthat::expect_equal(nrow(.school_class_rows(out, "A", "French")), 14L)
  testthat::expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
})

testthat::test_that("update_class_name edits only selected school and class", {
  WondeCL::reset_column_names()
  df <- .make_manual_edit_fixture()
  out <- WondeCL::update_class_name(
    df, class_name = "French", new_name = "Maths 7A",
    est_wonde_id = "WONDE-E001"
  )
  testthat::expect_equal(nrow(.school_class_rows(out, "A", "Maths 7A")), 15L)
  testthat::expect_equal(nrow(.school_class_rows(out, "A", "French")), 0L)
  testthat::expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
  testthat::expect_equal(nrow(out), nrow(df))
})

testthat::test_that("update_school_name supports multiple schools in one call", {
  WondeCL::reset_column_names()
  df <- .make_manual_edit_fixture()
  out <- WondeCL::update_school_name(
    df, new_name = c("School Alpha", "School Beta"),
    EST_ID = c("A", "B")
  )
  testthat::expect_setequal(
    unique(out$EST_WONDE_NAME), c("School Alpha", "School Beta")
  )
  testthat::expect_true(all(out$EST_WONDE_NAME[out$EST_ID == "A"] == "School Alpha"))
  testthat::expect_true(all(out$EST_WONDE_NAME[out$EST_ID == "B"] == "School Beta"))
})

testthat::test_that("remove_class respects the school and accepts multiple classes", {
  WondeCL::reset_column_names()
  df <- .make_manual_edit_fixture()
  out <- WondeCL::remove_class(df, class_name = "French", EST_ID = "A")
  testthat::expect_equal(nrow(out), 45L)
  testthat::expect_equal(nrow(.school_class_rows(out, "A", "Fernleaf")), 15L)
  testthat::expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
  
  all_a_removed <- WondeCL::remove_class(
    df, class_name = c("Fernleaf", "French"), EST_ID = "A"
  )
  testthat::expect_equal(nrow(all_a_removed), 30L)
  testthat::expect_true(all(all_a_removed$EST_ID == "B"))
})

testthat::test_that("remove_student_from_class removes the selected student only", {
  WondeCL::reset_column_names()
  df <- .make_manual_edit_fixture()
  pupil <- .example_student(df, "A", "French")
  out <- WondeCL::remove_student_from_class(
    df, student_id = pupil$STUDENT_WONDE_ID,
    class_name = "French", EST_ID = "A"
  )
  testthat::expect_equal(nrow(out), 59L)
  testthat::expect_false(pupil$STUDENT_WONDE_ID %in% out$STUDENT_WONDE_ID)
  testthat::expect_equal(nrow(.school_class_rows(out, "A", "French")), 14L)
  testthat::expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
})

testthat::test_that("configured names can change without changing edit function calls", {
  WondeCL::reset_column_names()
  default_df <- .make_manual_edit_fixture()
  internal <- WondeCL:::rename_to_internal_names(default_df)
  on.exit(WondeCL::reset_column_names(), add = TRUE)
  
  WondeCL::set_column_names(
    est_id = "SCHOOL_CODE_CUSTOM",
    class_name = "CLASS_NAME_CUSTOM",
    sfn = "FORENAME_CUSTOM"
  )
  df <- WondeCL:::rename_to_configured_names(internal)
  testthat::expect_true(all(c(
    "SCHOOL_CODE_CUSTOM", "CLASS_NAME_CUSTOM", "FORENAME_CUSTOM"
  ) %in% names(df)))
  
  out <- WondeCL::update_class_name(
    df, class_name = "French", new_name = "New French", est_id = "A"
  )
  testthat::expect_equal(sum(out$CLASS_NAME_CUSTOM == "New French"), 15L)
  testthat::expect_true("SCHOOL_CODE_CUSTOM" %in% names(out))
  
  out2 <- WondeCL::remove_class(
    df, class_name = "French", SCHOOL_CODE_CUSTOM = "A"
  )
  testthat::expect_equal(nrow(out2), 45L)
})

testthat::test_that("invalid selectors and absent classes are handled", {
  WondeCL::reset_column_names()
  df <- .make_manual_edit_fixture()
  testthat::expect_error(
    WondeCL::remove_class(df, class_name = "French", NOT_A_COLUMN = "A"),
    "Unknown school identifier"
  )
  testthat::expect_error(
    WondeCL::remove_class(df, class_name = "French", STUDENT_WONDE_ID = "WONDE-S001"),
    "not a supported school identifier"
  )
  testthat::expect_error(
    WondeCL::remove_class(df, class_name = "French"),
    "exactly one named school identifier"
  )
  testthat::expect_equal(
    WondeCL::remove_class(df, class_name = "Nonexistent", EST_ID = "A"), df
  )
})

# ============================================================
# Tests: withdraw_student_from_school_class()
# ============================================================

testthat::test_that("Student can be withdrawn using URN", {
  
  reset_column_names()
  
  df <- data.frame(
    URN = c("123456", "123456", "654321"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Smith")
  )
  
  result <- withdraw_student_from_school_class(
    df,
    first_name = "Alice",
    last_name = "Smith",
    URN = "123456"
  )
  
  testthat::expect_identical(
    result$WITHDRAWN,
    c("Y", "", "")
  )
  
})


testthat::test_that("Student can be withdrawn using EST_ID", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "B"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Smith")
  )
  
  result <- withdraw_student_from_school_class(
    df,
    first_name = "Alice",
    last_name = "Smith",
    EST_ID = "A"
  )
  
  testthat::expect_identical(
    result$WITHDRAWN,
    c("Y", "", "")
  )
  
})


testthat::test_that("Student can be withdrawn using EST_WONDE_ID", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_ID = c("W001", "W001", "W002"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Smith")
  )
  
  result <- withdraw_student_from_school_class(
    df,
    first_name = "Alice",
    last_name = "Smith",
    EST_WONDE_ID = "W002"
  )
  
  testthat::expect_identical(
    result$WITHDRAWN,
    c("", "", "Y")
  )
  
})


testthat::test_that("Matching is restricted to the selected school", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "B"),
    PUPIL_FIRST_NAME = c("Alice", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Smith")
  )
  
  result <- withdraw_student_from_school_class(
    df,
    first_name = "Alice",
    last_name = "Smith",
    EST_ID = "B"
  )
  
  testthat::expect_identical(
    result$WITHDRAWN,
    c("", "Y")
  )
  
})


testthat::test_that("Existing withdrawal statuses are preserved", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 4),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Charlie", "Diana"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Brown", "Evans"),
    WITHDRAWN = c("", "Y", NA_character_, "N")
  )
  
  result <- withdraw_student_from_school_class(
    df,
    first_name = "Alice",
    last_name = "Smith",
    EST_ID = "A"
  )
  
  testthat::expect_identical(
    result$WITHDRAWN,
    c("Y", "Y", NA_character_, "N")
  )
  
  testthat::expect_identical(
    df$WITHDRAWN,
    c("", "Y", NA_character_, "N")
  )
  
})


testthat::test_that("Missing WITHDRAWN column is created", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  result <- withdraw_student_from_school_class(
    df,
    first_name = "Bob",
    last_name = "Jones",
    EST_ID = "A"
  )
  
  testthat::expect_true("WITHDRAWN" %in% names(result))
  
  testthat::expect_identical(
    result$WITHDRAWN,
    c("", "Y")
  )
  
})


testthat::test_that("No matching student leaves data unchanged", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  testthat::expect_warning(
    result <- withdraw_student_from_school_class(
      df,
      first_name = "Unknown",
      last_name = "Smith",
      EST_ID = "A"
    ),
    "No student found"
  )
  
  testthat::expect_identical(result, df)
  
})


testthat::test_that("Multiple matching students are not withdrawn", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Alice", "Alice"),
    PUPIL_LAST_NAME = c("Smith", "Smith"),
    WITHDRAWN = c("", "")
  )
  
  testthat::expect_warning(
    result <- withdraw_student_from_school_class(
      df,
      first_name = "Alice",
      last_name = "Smith",
      EST_ID = "A"
    ),
    "Multiple matching students"
  )
  
  testthat::expect_identical(result, df)
  
})


testthat::test_that("Matching is case-sensitive", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  testthat::expect_warning(
    result <- withdraw_student_from_school_class(
      df,
      first_name = "alice",
      last_name = "Smith",
      EST_ID = "A"
    ),
    "No student found"
  )
  
  testthat::expect_identical(result, df)
  
})


testthat::test_that("Internal column names are supported", {
  
  reset_column_names()
  
  df <- data.frame(
    est_id = "A",
    sfn = "Alice",
    sln = "Smith"
  )
  
  result <- withdraw_student_from_school_class(
    df,
    first_name = "Alice",
    last_name = "Smith",
    EST_ID = "A"
  )
  
  testthat::expect_equal(result$WITHDRAWN, "Y")
  
  testthat::expect_true(
    all(c(
      "EST_ID",
      "PUPIL_FIRST_NAME",
      "PUPIL_LAST_NAME"
    ) %in% names(result))
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
  
  df <- data.frame(
    SCHOOL_IDENTIFIER = c("A", "A"),
    FIRST_NAME = c("Alice", "Bob"),
    LAST_NAME = c("Smith", "Jones"),
    WITHDRAWAL_STATUS = c("", "Y")
  )
  
  result <- withdraw_student_from_school_class(
    df,
    first_name = "Alice",
    last_name = "Smith",
    SCHOOL_IDENTIFIER = "A"
  )
  
  testthat::expect_identical(
    result$WITHDRAWAL_STATUS,
    c("Y", "Y")
  )
  
  testthat::expect_true(
    "SCHOOL_IDENTIFIER" %in% names(result)
  )
  
  testthat::expect_false(
    "WITHDRAWN" %in% names(result)
  )
  
})


testthat::test_that("Missing student-name columns produce an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice"
  )
  
  testthat::expect_error(
    withdraw_student_from_school_class(
      df,
      first_name = "Alice",
      last_name = "Smith",
      EST_ID = "A"
    ),
    "Missing required columns"
  )
  
})



testthat::test_that(
  "update_class_name preserves original Wonde class names", {
    
    WondeCL::reset_column_names()
    
    df <- WondeCL::class_list_example
    
    out <- WondeCL::update_class_name(
      df,
      class_name = "French",
      new_name = "Maths 7A",
      EST_ID = "A"
    )
    
    selected <- out[
      out$EST_ID == "A" &
        out$CLASS_NAME == "Maths 7A",
      ,
      drop = FALSE
    ]
    
    testthat::expect_equal(nrow(selected), 15L)
    
    testthat::expect_true(
      all(selected$CLASS_WONDE_NAME == "French")
    )
    
    testthat::expect_equal(
      nrow(out),
      nrow(df)
    )
  }
)


testthat::test_that(
  "update_student_class copies destination class and teacher fields", {
    
    WondeCL::reset_column_names()
    
    df <- WondeCL::class_list_example
    
    pupil <- df[
      which(
        df$EST_ID == "A" &
          df$CLASS_NAME == "French"
      )[1L],
      ,
      drop = FALSE
    ]
    
    out <- WondeCL::update_student_class(
      df,
      first_name = pupil$PUPIL_FIRST_NAME,
      last_name = pupil$PUPIL_LAST_NAME,
      class_name = "French",
      new_class_name = "Fernleaf",
      EST_ID = "A"
    )
    
    changed <- out[
      out$STUDENT_WONDE_ID == pupil$STUDENT_WONDE_ID,
      ,
      drop = FALSE
    ]
    
    testthat::expect_equal(
      changed$CLASS_NAME,
      "Fernleaf"
    )
    
    testthat::expect_equal(
      changed$CLASS_WONDE_NAME,
      "Fernleaf"
    )
    
    testthat::expect_equal(
      changed$CLASS_WONDE_ID,
      "WONDE-C001"
    )
    
    testthat::expect_equal(
      changed$TEACHER_FULL_NAME,
      "Miss Katherine"
    )
    
    testthat::expect_equal(
      changed$TEACHER_WONDE_ID,
      "WONDE-T001"
    )
  }
)
