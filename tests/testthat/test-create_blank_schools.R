# Tests for create_school_blanks() using the package column configuration.

testthat::test_that("master uses configured default column names and order", {
  reset_column_names()
  schools <- data.frame(SchoolName = "Oak Primary", URN = "123456",
                        NoStudents = 35, stringsAsFactors = FALSE)
  out <- create_school_blanks(schools, type = 20, class_size = 30)
  testthat::expect_identical(names(out), c(
    "EST_NAME", "URN", "CLASS_NAME", "TEACHER_FIRST_NAME",
    "TEACHER_LAST_NAME", "PUPIL_FIRST_NAME", "PUPIL_LAST_NAME", "DOB",
    "UPN", "SPARE", "OME_ID", "PRINT_ID", "WITHDRAWN", "EST_ID",
    "WITHDRAWAL_DATE"
  ))
  testthat::expect_equal(nrow(out), 40L)
  testthat::expect_equal(unique(out$CLASS_NAME),
                         c("Reception Class 1", "Reception Class 2"))
  testthat::expect_equal(out$PUPIL_FIRST_NAME, seq_len(40))
  testthat::expect_true(all(is.na(out$PRINT_ID)))
  testthat::expect_true(all(is.na(out$OME_ID)))
  testthat::expect_true(all(is.na(out$WITHDRAWN)))
  testthat::expect_true(all(out$EST_NAME == "Oak Primary"))
  testthat::expect_true(all(is.na(out$WITHDRAWAL_DATE)))
  testthat::expect_true(all(out$TEACHER_FIRST_NAME == "Reception"))
  testthat::expect_true(all(out$TEACHER_LAST_NAME == "Teacher"))
})

testthat::test_that("print preset uses current configured names", {
  reset_column_names()
  schools <- data.frame(SchoolName = "Oak", URN = "123456",
                        NoStudents = 10)
  out <- create_school_blanks(schools, class_list_columns = "print")
  testthat::expect_identical(names(out), c(
    "EST_NAME", "CLASS_NAME", "TEACHER_FIRST_NAME",
    "TEACHER_LAST_NAME", "PUPIL_FIRST_NAME", "PUPIL_LAST_NAME",
    "DOB", "UPN", "PRINT_ID", "WITHDRAWN", "SPARE", "EST_ID"
  ))
  testthat::expect_equal(nrow(out), 30L)
  testthat::expect_equal(unique(out$EST_NAME), "Oak")
})

testthat::test_that("ClassInfo controls exact class names and sizes", {
  reset_column_names()
  schools <- data.frame(SchoolName = "Oak", URN = "123456",
                        NoStudents = 100,
                        ClassInfo = "Foxes -> 2, Owls -> 3")
  out <- create_school_blanks(schools, class_list_columns = "master")
  testthat::expect_equal(nrow(out), 5L)
  testthat::expect_equal(as.character(out$CLASS_NAME),
                         c("Foxes", "Foxes", "Owls", "Owls", "Owls"))
})

testthat::test_that("teacher names are assigned per class", {
  reset_column_names()
  schools <- data.frame(
    SchoolName = "Oak", URN = "123456", NoStudents = 5,
    ClassInfo = "Foxes -> 2, Owls -> 3",
    TeacherInfo = "Jane Smith, Alex Jones"
  )
  out <- create_school_blanks(schools, class_list_columns = c(
    "class_name", "tfn", "tln", "t_full"
  ))
  testthat::expect_equal(out$TEACHER_FIRST_NAME,
                         c("Jane", "Jane", "Alex", "Alex", "Alex"))
  testthat::expect_equal(out$TEACHER_LAST_NAME,
                         c("Smith", "Smith", "Jones", "Jones", "Jones"))
  testthat::expect_equal(out$TEACHER_FULL_NAME[1], "Jane Smith")
})

testthat::test_that("a single provided teacher is reused for classes", {
  reset_column_names()
  schools <- data.frame(
    SchoolName = "Oak", URN = "123456", NoStudents = 5,
    ClassInfo = "A -> 2, B -> 3", TeacherInfo = "Jane Smith"
  )
  out <- create_school_blanks(schools, class_list_columns = c("tfn", "tln"))
  testthat::expect_equal(out$TEACHER_FIRST_NAME, rep("Jane", 5))
  testthat::expect_equal(out$TEACHER_LAST_NAME, rep("Smith", 5))
})

testthat::test_that("numbering_type class restarts numbering", {
  reset_column_names()
  schools <- data.frame(SchoolName = "Oak", URN = "123456",
                        NoStudents = 5, ClassInfo = "A -> 2, B -> 3")
  out <- create_school_blanks(schools, numbering_type = "class",
                              class_list_columns = c("sfn", "class_name"))
  testthat::expect_equal(out$PUPIL_FIRST_NAME, c(1L, 2L, 1L, 2L, 3L))
})

testthat::test_that("school numbering resets at each school", {
  reset_column_names()
  schools <- data.frame(SchoolName = c("Oak", "Elm"),
                        URN = c("123456", "234567"),
                        NoStudents = c(2, 3),
                        ClassInfo = c("A -> 2", "B -> 3"))
  out <- create_school_blanks(schools, class_list_columns = c("urn", "sfn"))
  testthat::expect_equal(out$PUPIL_FIRST_NAME, c(1L, 2L, 1L, 2L, 3L))
  testthat::expect_equal(out$URN, c(rep("123456", 2), rep("234567", 3)))
})

testthat::test_that("auto type uses rounded class size and buffer", {
  reset_column_names()
  schools <- data.frame(SchoolName = "Oak", URN = "123456",
                        NoStudents = 35)
  out <- create_school_blanks(schools, type = "auto", class_size = 30,
                              class_buffer = 2)
  testthat::expect_equal(nrow(out), 40L) # 2 x (ceiling(35 / 2) + 2)
})

testthat::test_that("configured input and output names work", {
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  set_column_names(est_name = "MY_SCHOOL", urn = "MY_URN",
                   class_name = "MY_CLASS", sfn = "MY_PUPIL_NO",
                   hex_id = "MY_PRINT_ID", est_id = "MY_EST_ID",
                   withdrawal_date = "MY_WITHDRAWAL_DATE")
  schools <- data.frame(MY_SCHOOL = "Oak", MY_URN = "123456",
                        NoStudents = 2, MY_EST_ID = "009")
  out <- create_school_blanks(
    schools, type = 2,
    class_list_columns = c("est_name", "urn", "class_name",
                           "sfn", "hex_id", "est_id", "withdrawal_date")
  )
  testthat::expect_identical(names(out), c(
    "MY_SCHOOL", "MY_URN", "MY_CLASS", "MY_PUPIL_NO",
    "MY_PRINT_ID", "MY_EST_ID", "MY_WITHDRAWAL_DATE"
  ))
  testthat::expect_equal(out$MY_EST_ID, rep("009", 2))
  testthat::expect_equal(out$MY_PUPIL_NO, 1:2)
  testthat::expect_equal(out$MY_SCHOOL, rep("Oak", 2))
  testthat::expect_true(all(is.na(out$MY_WITHDRAWAL_DATE)))
})

testthat::test_that("custom requested literal columns are filled with NA", {
  reset_column_names()
  schools <- data.frame(SchoolName = "Oak", URN = "123456", NoStudents = 1)
  out <- create_school_blanks(schools, type = 1,
                              class_list_columns = c("urn", "CUSTOM_FIELD"))
  testthat::expect_identical(names(out), c("URN", "CUSTOM_FIELD"))
  testthat::expect_true(is.na(out$CUSTOM_FIELD))
})

testthat::test_that("empty input and zero-student school give empty output", {
  reset_column_names()
  schools <- data.frame(SchoolName = character(), URN = character(),
                        NoStudents = integer())
  out <- create_school_blanks(schools)
  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_true("PRINT_ID" %in% names(out))
  schools <- data.frame(SchoolName = "Oak", URN = "123456", NoStudents = 0)
  out <- create_school_blanks(schools)
  testthat::expect_equal(nrow(out), 0L)
})

testthat::test_that("master preset respects configured establishment and withdrawal dates", {
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  set_column_names(
    est_name = "DISPLAY_SCHOOL",
    withdrawal_date = "WITHDRAWAL_DATE"
  )
  schools <- data.frame(
    DISPLAY_SCHOOL = "Elm Primary", URN = "987654", NoStudents = 2
  )
  out <- create_school_blanks(schools, type = 2)
  testthat::expect_identical(names(out)[1], "DISPLAY_SCHOOL")
  testthat::expect_identical(tail(names(out), 1), "WITHDRAWAL_DATE")
  testthat::expect_equal(out$DISPLAY_SCHOOL, rep("Elm Primary", 2))
  testthat::expect_true(all(is.na(out$WITHDRAWAL_DATE)))
})


testthat::test_that("internal establishment name input is accepted", {
  reset_column_names()
  schools <- data.frame(
    est_name = "Elm Primary", urn = "987654", NoStudents = 2
  )
  out <- create_school_blanks(
    schools, type = 2,
    class_list_columns = c("est_name", "withdrawal_date")
  )
  testthat::expect_identical(names(out), c("EST_NAME", "WITHDRAWAL_DATE"))
  testthat::expect_equal(out$EST_NAME, rep("Elm Primary", 2))
  testthat::expect_true(all(is.na(out$WITHDRAWAL_DATE)))
})


testthat::test_that("invalid inputs are rejected", {
  reset_column_names()
  schools <- data.frame(SchoolName = "Oak", URN = "123456", NoStudents = 10)
  testthat::expect_error(create_school_blanks(schools, class_size = 0),
                         "class_size")
  testthat::expect_error(create_school_blanks(schools, numbering_type = "x"),
                         "numbering_type")
  testthat::expect_error(create_school_blanks(schools, teacher_name = "Jane"),
                         "teacher_name")
  testthat::expect_error(create_school_blanks(schools, type = -1),
                         "Numeric type")
  testthat::expect_error(create_school_blanks(schools, class_buffer = -1),
                         "class_buffer")
  schools$ClassInfo <- "A 20"
  testthat::expect_error(create_school_blanks(schools), "Invalid ClassInfo")
  schools$ClassInfo <- "A -> 2, B -> 3"
  schools$TeacherInfo <- "Jane Smith, Alex Jones, Bob Brown"
  testthat::expect_error(create_school_blanks(schools), "TeacherInfo")
})

