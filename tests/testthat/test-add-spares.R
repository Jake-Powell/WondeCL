
# ============================================================
# Tests: add_spares()
# ============================================================

testthat::test_that("Adds requested spare rows to each class", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = rep("A", 5),
    CLASS_NAME = c("C1", "C1", "C2", "C2", "C2"),
    PUPIL_FIRST_NAME = c(
      "Alice", "Bob", "Charlie", "Diana", "Eve"
    ),
    PUPIL_LAST_NAME = c(
      "Smith", "Jones", "Brown", "Evans", "White"
    )
  )
  
  result <- add_spares(
    df,
    no_spares = 3,
    include_spare_column = TRUE
  )
  
  spare_col <- get_column_names()[["spare"]]
  
  testthat::expect_equal(nrow(result), 11L)
  
  testthat::expect_equal(
    sum(result[[spare_col]] == "Y"),
    6L
  )
  
  testthat::expect_equal(
    sum(result$CLASS_NAME == "C1" &
          result[[spare_col]] == "Y"),
    3L
  )
  
  testthat::expect_equal(
    sum(result$CLASS_NAME == "C2" &
          result[[spare_col]] == "Y"),
    3L
  )
})


testthat::test_that("Spare column uses default configured name", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  spare_col <- get_column_names()[["spare"]]
  
  testthat::expect_equal(spare_col, "SPARE")
  
  testthat::expect_true(
    spare_col %in% names(result)
  )
  
  testthat::expect_false(
    "Spare?" %in% names(result)
  )
  
  testthat::expect_equal(
    sum(result[[spare_col]] == "Y"),
    2L
  )
})


testthat::test_that("Custom configured spare column name works", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    spare = "SPARE_INDICATOR"
  )
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_NAME = c("C1", "C2"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  testthat::expect_true(
    "SPARE_INDICATOR" %in% names(result)
  )
  
  testthat::expect_false(
    "SPARE" %in% names(result)
  )
  
  testthat::expect_identical(
    names(result),
    c(names(df), "SPARE_INDICATOR")
  )
  
  testthat::expect_equal(
    sum(result$SPARE_INDICATOR == "Y"),
    4L
  )
})


testthat::test_that("Existing configured spare flags are preserved", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    spare = "SPARE_INDICATOR"
  )
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_NAME = c("C1", "C1"),
    PUPIL_FIRST_NAME = c("Alice", ""),
    PUPIL_LAST_NAME = c("Smith", ""),
    SPARE_INDICATOR = c("", "Y")
  )
  
  result <- add_spares(
    df,
    no_spares = 1,
    include_spare_column = TRUE
  )
  
  testthat::expect_equal(nrow(result), 3L)
  
  testthat::expect_equal(
    sum(result$SPARE_INDICATOR == "Y"),
    2L
  )
  
  testthat::expect_equal(
    sum(result$SPARE_INDICATOR == ""),
    1L
  )
  
  testthat::expect_equal(
    sum(names(result) == "SPARE_INDICATOR"),
    1L
  )
})


testthat::test_that("Internal spare column is renamed to configured name", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    spare = "SPARE_INDICATOR"
  )
  
  df <- data.frame(
    est_id = c("A", "A"),
    class_name = c("C1", "C1"),
    sfn = c("Alice", ""),
    sln = c("Smith", ""),
    spare = c("", "Y")
  )
  
  result <- add_spares(
    df,
    no_spares = 1,
    include_spare_column = TRUE
  )
  
  testthat::expect_identical(
    names(result),
    c(
      "est_id",
      "class_name",
      "sfn",
      "sln",
      "SPARE_INDICATOR"
    )
  )
  
  testthat::expect_equal(
    sum(result$SPARE_INDICATOR == "Y"),
    2L
  )
})


testthat::test_that("Spare column is optional", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = FALSE
  )
  
  testthat::expect_equal(nrow(result), 3L)
  
  testthat::expect_false(
    get_column_names()[["spare"]] %in% names(result)
  )
  
  testthat::expect_identical(
    names(result),
    names(df)
  )
})


testthat::test_that("Existing spare column remains when flagging disabled", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith",
    SPARE = ""
  )
  
  result <- add_spares(
    df,
    no_spares = 1,
    include_spare_column = FALSE
  )
  
  testthat::expect_true(
    "SPARE" %in% names(result)
  )
  
  testthat::expect_equal(
    sum(result$SPARE == "Y"),
    0L
  )
})


testthat::test_that("Student-specific fields remain blank", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith",
    UPN = "U001",
    OME_ID = "O001",
    PRINT_ID = "P001"
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  spare <- result[
    result$SPARE == "Y",
    ,
    drop = FALSE
  ]
  
  for (col in c(
    "PUPIL_FIRST_NAME",
    "PUPIL_LAST_NAME",
    "UPN",
    "OME_ID",
    "PRINT_ID"
  )) {
    testthat::expect_true(
      all(spare[[col]] == "")
    )
  }
  
  testthat::expect_true(
    all(spare$EST_ID == "A")
  )
  
  testthat::expect_true(
    all(spare$CLASS_NAME == "C1")
  )
})


testthat::test_that("School and class information is retained", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "B"),
    EST_WONDE_NAME = c("School A", "School B"),
    URN = c("123456", "654321"),
    CLASS_NAME = c("C1", "C2"),
    CLASS_WONDE_NAME = c("W1", "W2"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  result <- add_spares(
    df,
    no_spares = 1,
    include_spare_column = TRUE
  )
  
  spare <- result[
    result$SPARE == "Y",
    ,
    drop = FALSE
  ]
  
  spare <- spare[
    order(spare$EST_ID),
    ,
    drop = FALSE
  ]
  
  testthat::expect_equal(
    spare$EST_ID,
    c("A", "B")
  )
  
  testthat::expect_equal(
    spare$EST_WONDE_NAME,
    c("School A", "School B")
  )
  
  testthat::expect_equal(
    spare$URN,
    c("123456", "654321")
  )
  
  testthat::expect_equal(
    spare$CLASS_NAME,
    c("C1", "C2")
  )
  
  testthat::expect_equal(
    spare$CLASS_WONDE_NAME,
    c("W1", "W2")
  )
})


testthat::test_that("Teacher information is retained", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith",
    t_full = "Jane Teacher",
    tfn = "Jane",
    tln = "Teacher",
    t_main = "Y"
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  spare <- result[
    result$SPARE == "Y",
    ,
    drop = FALSE
  ]
  
  testthat::expect_true(
    all(spare$t_full == "Jane Teacher")
  )
  
  testthat::expect_true(
    all(spare$tfn == "Jane")
  )
  
  testthat::expect_true(
    all(spare$tln == "Teacher")
  )
  
  testthat::expect_true(
    all(spare$t_main == "Y")
  )
})


testthat::test_that("Distinct teachers generate separate spare groups", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_NAME = c("C1", "C1"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones"),
    t_full = c("Teacher One", "Teacher Two")
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  # Two distinct teacher combinations, two spares each.
  testthat::expect_equal(nrow(result), 6L)
  
  testthat::expect_equal(
    sum(result$SPARE == "Y"),
    4L
  )
  
  spare <- result[result$SPARE == "Y", ]
  
  testthat::expect_equal(
    table(spare$t_full) |> as.character(),
    c('2', '2')
  )
})


testthat::test_that("Falls back to URN for school identification", {
  
  reset_column_names()
  
  df <- data.frame(
    URN = c("123456", "654321"),
    CLASS_NAME = c("C1", "C1"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  testthat::expect_equal(nrow(result), 6L)
  
  testthat::expect_equal(
    sum(result$SPARE == "Y"),
    4L
  )
})


testthat::test_that("Falls back to EST_WONDE_NAME", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_WONDE_NAME = c("School A", "School B"),
    CLASS_NAME = c("C1", "C1"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  result <- add_spares(
    df,
    no_spares = 1,
    include_spare_column = TRUE
  )
  
  testthat::expect_equal(nrow(result), 4L)
  
  testthat::expect_equal(
    sum(result$SPARE == "Y"),
    2L
  )
})


testthat::test_that("Falls back to CLASS_WONDE_NAME", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_WONDE_NAME = c("W1", "W2"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  testthat::expect_equal(nrow(result), 6L)
  
  testthat::expect_equal(
    sum(result$SPARE == "Y"),
    4L
  )
})


testthat::test_that("Original column names and order are preserved", {
  
  reset_column_names()
  
  df <- data.frame(
    PUPIL_LAST_NAME = c("Smith", "Jones"),
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    CLASS_NAME = c("C1", "C1"),
    UPN = c("U001", "U002")
  )
  
  result <- add_spares(
    df,
    no_spares = 1,
    include_spare_column = TRUE
  )
  
  testthat::expect_identical(
    names(result),
    c(names(df), get_column_names()[["spare"]])
  )
})


testthat::test_that("Custom class-list and spare names work together", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    est_id = "SCHOOL_IDENTIFIER",
    class_name = "MY_CLASS",
    sfn = "CHILD_FIRST",
    sln = "CHILD_LAST",
    spare = "IS_SPARE"
  )
  
  df <- data.frame(
    SCHOOL_IDENTIFIER = c("A", "A"),
    MY_CLASS = c("C1", "C2"),
    CHILD_FIRST = c("Alice", "Bob"),
    CHILD_LAST = c("Smith", "Jones")
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  testthat::expect_identical(
    names(result),
    c(names(df), "IS_SPARE")
  )
  
  testthat::expect_equal(nrow(result), 6L)
  
  testthat::expect_equal(
    sum(result$IS_SPARE == "Y"),
    4L
  )
})


testthat::test_that("Internal column names are preserved", {
  
  reset_column_names()
  
  df <- data.frame(
    est_id = "A",
    class_name = "C1",
    sfn = "Alice",
    sln = "Smith"
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  testthat::expect_identical(
    names(result),
    c(names(df), "SPARE")
  )
  
  testthat::expect_equal(
    sum(result$SPARE == "Y"),
    2L
  )
})


testthat::test_that("Spare rows sort after original students", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_NAME = c("C1", "C1"),
    PUPIL_FIRST_NAME = c("Alice", "Bob"),
    PUPIL_LAST_NAME = c("Smith", "Jones")
  )
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  testthat::expect_identical(
    tail(result$SPARE, 2),
    c("Y", "Y")
  )
  
  testthat::expect_identical(
    result$PUPIL_LAST_NAME[1:2],
    c("Jones", "Smith")
  )
})


testthat::test_that("Zero spares adds no rows", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  result <- add_spares(
    df,
    no_spares = 0,
    include_spare_column = TRUE
  )
  
  testthat::expect_equal(nrow(result), nrow(df))
  
  testthat::expect_equal(
    result[[get_column_names()[["spare"]]]],
    ""
  )
})


testthat::test_that("Invalid no_spares values produce errors", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  for (invalid in list(
    -1,
    1.5,
    NA_real_,
    Inf,
    "three",
    c(1, 2)
  )) {
    
    testthat::expect_error(
      add_spares(df, no_spares = invalid),
      "no_spares must be"
    )
  }
})


testthat::test_that("Invalid include_spare_column produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  for (invalid in list(
    NA,
    "TRUE",
    1,
    logical(0)
  )) {
    
    testthat::expect_error(
      add_spares(
        df,
        include_spare_column = invalid
      ),
      "include_spare_column must be"
    )
  }
})


testthat::test_that("Missing school identifier produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  testthat::expect_error(
    add_spares(df),
    "Missing school identifier"
  )
})


testthat::test_that("Missing class identifier produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  testthat::expect_error(
    add_spares(df),
    "Missing class identifier"
  )
})


testthat::test_that("Missing student name columns produce an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice"
  )
  
  testthat::expect_error(
    add_spares(df),
    "Missing required student columns"
  )
})


testthat::test_that("Input data frame is not modified", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    CLASS_NAME = "C1",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  original <- df
  
  result <- add_spares(
    df,
    no_spares = 2,
    include_spare_column = TRUE
  )
  
  testthat::expect_identical(df, original)
  testthat::expect_equal(nrow(result), 3L)
})
