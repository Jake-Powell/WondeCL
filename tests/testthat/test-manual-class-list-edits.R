# Run with devtools::test(filter = "manual-class-list-edits")

# WondeCL::class_list_example uses an older, 16-column example schema.
# Construct a test fixture with the current configured class-list headers.
# The positions below refer to the 16 columns of that example, in order:
# 1 school code, 2 school name, 3 URN, 6 class ID, 7 class name,
# 8 teacher ID, 9/10 teacher names, 12 student ID, 13 UPN,
# 14/15 student names, 16 DOB.
.make_manual_edit_fixture <- function() {
  raw <- WondeCL::class_list_example
  stopifnot(ncol(raw) == 16L)
  
  internal <- data.frame(
    est_id = as.character(raw[[1]]),
    est_wonde_id = paste0("W-", as.character(raw[[1]])),
    est_wonde_name = as.character(raw[[2]]),
    est_wonde_urn = as.character(raw[[3]]),
    urn = as.character(raw[[3]]),
    c_wonde_id = as.character(raw[[6]]),
    c_wonde_name = as.character(raw[[7]]),
    t_wonde_ID = as.character(raw[[8]]),
    tfn = as.character(raw[[9]]),
    tln = as.character(raw[[10]]),
    t_main = TRUE,
    t_full = paste(raw[[9]], raw[[10]]),
    s_wonde_id = as.character(raw[[12]]),
    upn = as.character(raw[[13]]),
    sfn = as.character(raw[[14]]),
    sln = as.character(raw[[15]]),
    dob = as.character(raw[[16]]),
    stringsAsFactors = FALSE
  )
  WondeCL:::rename_to_configured_names(internal)
}

# Fetch a configured column name via its stable internal key.
.cn <- function(key) unname(WondeCL::get_column_names(key))

# Avoid tests relying on row numbers: return matching rows, including 0 rows.
.school_class_rows <- function(df, school, class_name) {
  df[as.character(df[[.cn("est_id")]]) == school &
       df[[.cn("c_wonde_name")]] == class_name, , drop = FALSE]
}

test_that("fixture represents two schools and the expected classes", {
  df <- .make_manual_edit_fixture()
  expect_equal(nrow(df), 60L)
  expect_setequal(unique(df[[.cn("est_id")]]), c("A", "B"))
  expect_equal(nrow(.school_class_rows(df, "A", "Fernleaf")), 15L)
  expect_equal(nrow(.school_class_rows(df, "A", "French")), 15L)
  expect_equal(nrow(.school_class_rows(df, "B", "Foxglove")), 30L)
})

test_that("add_student_to_school_class accepts URN and adds one student", {
  df <- .make_manual_edit_fixture()
  out <- WondeCL::add_student_to_school_class(
    df, first_name = "Jimmy", last_name = "Powell",
    class_name = "French", dob = "1910-05-07",
    URN = 123456, verbose = FALSE
  )
  expect_equal(nrow(out), nrow(df) + 1L)
  jimmy <- out[out[[.cn("sfn")]] == "Jimmy" &
                 out[[.cn("sln")]] == "Powell", , drop = FALSE]
  expect_equal(nrow(jimmy), 1L)
  expect_equal(jimmy[[.cn("c_wonde_name")]], "French")
  expect_equal(jimmy[[.cn("est_id")]], "A")
  expect_equal(jimmy[[.cn("dob")]], "1910-05-07")
  expect_true(is.na(jimmy[[.cn("s_wonde_id")]]))
  expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
})

test_that("update_student_class uses teacher and class from same school", {
  df <- .make_manual_edit_fixture()
  out <- WondeCL::update_student_class(
    df, first_name = "Kandra", last_name = "Marionberry",
    class_name = "French", new_class_name = "Fernleaf",
    EST_ID = "A"
  )
  pupil <- out[out[[.cn("sfn")]] == "Kandra" &
                 out[[.cn("sln")]] == "Marionberry", , drop = FALSE]
  expect_equal(nrow(pupil), 1L)
  expect_equal(pupil[[.cn("c_wonde_name")]], "Fernleaf")
  expect_equal(pupil[[.cn("c_wonde_id")]], "449073-1")
  expect_equal(pupil[[.cn("t_full")]], "Miss Katherine")
  expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
})

test_that("update_class_name only edits selected school and class", {
  df <- .make_manual_edit_fixture()
  out <- WondeCL::update_class_name(
    df, class_name = "French", new_name = "Maths 7A",
    est_wonde_id = "W-A"
  )
  expect_equal(nrow(.school_class_rows(out, "A", "Maths 7A")), 15L)
  expect_equal(nrow(.school_class_rows(out, "A", "French")), 0L)
  expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
})

test_that("update_school_name supports multiple schools in one call", {
  df <- .make_manual_edit_fixture()
  out <- WondeCL::update_school_name(
    df, new_name = c("School Alpha", "School Beta"),
    EST_ID = c("A", "B")
  )
  expect_setequal(unique(out[[.cn("est_wonde_name")]]),
                  c("School Alpha", "School Beta"))
  expect_true(all(out[[.cn("est_wonde_name")]][out[[.cn("est_id")]] == "A"] == "School Alpha"))
  expect_true(all(out[[.cn("est_wonde_name")]][out[[.cn("est_id")]] == "B"] == "School Beta"))
})

test_that("remove_class respects selected school and accepts multiple classes", {
  df <- .make_manual_edit_fixture()
  out <- WondeCL::remove_class(df, class_name = "French", EST_ID = "A")
  expect_equal(nrow(out), 45L)
  expect_equal(nrow(.school_class_rows(out, "A", "Fernleaf")), 15L)
  expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
  all_a_removed <- WondeCL::remove_class(
    df, class_name = c("Fernleaf", "French"), EST_ID = "A"
  )
  expect_equal(nrow(all_a_removed), 30L)
  expect_true(all(all_a_removed[[.cn("est_id")]] == "B"))
})

test_that("remove_student_from_class removes only requested student", {
  df <- .make_manual_edit_fixture()
  target <- df[[.cn("s_wonde_id")]][df[[.cn("sfn")]] == "Kandra"]
  expect_length(target, 1L)
  out <- WondeCL::remove_student_from_class(
    df, student_id = target, class_name = "French", EST_ID = "A"
  )
  expect_equal(nrow(out), 59L)
  expect_false(target %in% out[[.cn("s_wonde_id")]])
  expect_equal(nrow(.school_class_rows(out, "A", "French")), 14L)
  expect_equal(nrow(.school_class_rows(out, "B", "Foxglove")), 30L)
})

test_that("configured column names can change without changing function calls", {
  # Restore configuration after this test to avoid leaking state to other tests.
  on.exit(WondeCL::reset_column_names(), add = TRUE)
  WondeCL::set_column_names(
    est_id = "SCHOOL_CODE_CUSTOM",
    c_wonde_name = "CLASS_NAME_CUSTOM",
    sfn = "FORENAME_CUSTOM"
  )
  df <- .make_manual_edit_fixture()
  expect_true(all(c("SCHOOL_CODE_CUSTOM", "CLASS_NAME_CUSTOM", "FORENAME_CUSTOM") %in% names(df)))
  
  out <- WondeCL::update_class_name(
    df, class_name = "French", new_name = "New French",
    est_id = "A"  # stable internal identifier
  )
  expect_equal(sum(out$CLASS_NAME_CUSTOM == "New French"), 15L)
  expect_true("SCHOOL_CODE_CUSTOM" %in% names(out))
  
  out2 <- WondeCL::remove_class(
    df, class_name = "French", SCHOOL_CODE_CUSTOM = "A" # configured external name
  )
  expect_equal(nrow(out2), 45L)
})

test_that("invalid selectors and nonexistent classes are handled", {
  df <- .make_manual_edit_fixture()
  expect_error(
    WondeCL::remove_class(df, class_name = "French", NOT_A_COLUMN = "A"),
    "Unknown school identifier"
  )
  expect_error(
    WondeCL::remove_class(df, class_name = "French", STUDENT_WONDE_ID = "1"),
    "not a supported school identifier"
  )
  expect_error(
    WondeCL::remove_class(df, class_name = "French"),
    "exactly one named school identifier"
  )
  expect_equal(
    WondeCL::remove_class(df, class_name = "Nonexistent", EST_ID = "A"),
    df
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
