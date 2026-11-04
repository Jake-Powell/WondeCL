
testthat::test_that("Works with CLASS_NAME only", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "A"),
    CLASS_NAME = c("C1", "C1", "C2"),
    PUPIL_FIRST_NAME = c("Alice", "Bob", "Charlie"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Brown"),
    t_full = rep("Old Teacher", 3),
    tfn = rep("Old", 3),
    tln = rep("Teacher", 3)
  )
  
  result <- update_class_teacher(
    df,
    EST_ID = "A",
    class_name = "C1",
    first_name = "Jane",
    last_name = "Smith"
  )
  
  result <- rename_to_internal_names(result)
  
  testthat::expect_equal(
    result$t_full,
    c("Jane Smith", "Jane Smith", "Old Teacher")
  )
  
  testthat::expect_equal(
    result$tfn,
    c("Jane", "Jane", "Old")
  )
  
  testthat::expect_equal(
    result$tln,
    c("Smith", "Smith", "Teacher")
  )
})


testthat::test_that("Falls back to CLASS_WONDE_NAME", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_WONDE_NAME = c("W1", "W2"),
    t_full = c("Old Teacher", "Old Teacher")
  )
  
  result <- update_class_teacher(
    df,
    EST_ID = "A",
    class_name = "W1",
    first_name = "Jane",
    last_name = "Smith"
  )
  
  result <- rename_to_internal_names(result)
  
  testthat::expect_equal(
    result$t_full,
    c("Jane Smith", "Old Teacher")
  )
})


testthat::test_that("CLASS_NAME takes priority when both exist", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_NAME = c("C1", "C2"),
    CLASS_WONDE_NAME = c("W1", "W1"),
    t_full = c("Old Teacher", "Old Teacher")
  )
  
  result <- update_class_teacher(
    df,
    EST_ID = "A",
    class_name = "C1",
    first_name = "Jane",
    last_name = "Smith"
  )
  
  result <- rename_to_internal_names(result)
  
  testthat::expect_equal(
    result$t_full,
    c("Jane Smith", "Old Teacher")
  )
})


testthat::test_that("NA class_name updates all classes in school", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A", "B"),
    CLASS_NAME = c("C1", "C2", "C1"),
    t_full = rep("Old Teacher", 3)
  )
  
  result <- update_class_teacher(
    df,
    EST_ID = "A",
    class_name = NA,
    first_name = "Jane",
    last_name = "Smith"
  )
  
  result <- rename_to_internal_names(result)
  
  testthat::expect_equal(
    result$t_full,
    c("Jane Smith", "Jane Smith", "Old Teacher")
  )
})


testthat::test_that("Appending teacher duplicates only matching rows", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = c("A", "A"),
    CLASS_NAME = c("C1", "C2"),
    t_full = c("Old Teacher", "Old Teacher")
  )
  
  result <- update_class_teacher(
    df,
    EST_ID = "A",
    class_name = "C1",
    first_name = "Jane",
    last_name = "Smith",
    overwrite = FALSE
  )
  
  result <- rename_to_internal_names(result)
  
  testthat::expect_equal(nrow(result), 3L)
  
  testthat::expect_equal(
    result$t_full,
    c("Old Teacher", "Old Teacher", "Jane Smith")
  )
})


testthat::test_that("Missing both class identifiers produces an error", {
  
  reset_column_names()
  
  df <- data.frame(
    EST_ID = "A",
    t_full = "Old Teacher"
  )
  
  testthat::expect_error(
    update_class_teacher(
      df,
      EST_ID = "A",
      class_name = "C1",
      first_name = "Jane",
      last_name = "Smith"
    ),
    "Missing class identifier"
  )
})

