
# ============================================================
# Tests: add_student_ids_from_previous()
#
# Master list uses FNS, LNS and DOBS.
# Name outputs are expected to be lowercase.
# ============================================================


testthat::test_that("UPN matching transfers OME_ID from master list", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = c("O001", "O002", "O003"),
    PRINT_ID = c("P001", "P002", "P003"),
    FIRST_NAME = c("Alice", "Bob", "Charlie"),
    LAST_NAME = c("Smith", "Jones", "Brown"),
    EST_ID = c("A", "A", "B"),
    EST_CATEGORY = c("Primary", "Primary", "Secondary"),
    CLASS_ID = c("C1", "C1", "C2"),
    UPN = c("U001", "U002", "U003"),
    FNS = c("Alice", "Bob", "Charlie"),
    LNS = c("Smith", "Jones", "Brown"),
    DOBS = c("2015-01-01", "2015-02-02", "2014-03-03")
  )
  
  new <- data.frame(
    EST_ID = c("A", "A", "B"),
    PUPIL_FIRST_NAME = c("Alicia", "Robert", "Charles"),
    PUPIL_LAST_NAME = c("Smith", "Jones", "Brown"),
    UPN = c("U001", "U002", "U003")
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_identical(
    result$OME_ID,
    c("O001", "O002", "O003")
  )
  
  testthat::expect_identical(
    result$MATCH_METHOD,
    rep("UPN", 3)
  )
  
  testthat::expect_identical(
    result$OLD_STUDENT_FIRST_NAME,
    c("alice", "bob", "charlie")
  )
  
  testthat::expect_identical(
    result$OLD_STUDENT_LAST_NAME,
    c("smith", "jones", "brown")
  )
  
  testthat::expect_identical(
    result$OLD_DOB,
    master$DOBS
  )
})


testthat::test_that("UPN matching takes priority over names", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = c("O001", "O002"),
    FIRST_NAME = c("Alice", "Alicia"),
    LAST_NAME = c("Smith", "Jones"),
    EST_ID = c("A", "A"),
    UPN = c("U001", "U002"),
    FNS = c("Alice---Ally", "Alicia---Alice"),
    LNS = c("Smith", "Jones---Smith"),
    DOBS = c("2015-01-01", "2015-02-02")
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith",
    UPN = "U002"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = TRUE
  )
  
  testthat::expect_equal(result$OME_ID, "O002")
  testthat::expect_equal(result$MATCH_METHOD, "UPN")
  
  testthat::expect_equal(
    result$OLD_STUDENT_FIRST_NAME,
    "alicia---alice"
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_LAST_NAME,
    "jones---smith"
  )
})


testthat::test_that("UPN matching is not restricted to establishment", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    FIRST_NAME = "Alice",
    LAST_NAME = "Smith",
    EST_ID = "A",
    UPN = "U001",
    FNS = "Alice",
    LNS = "Smith",
    DOBS = "2015-01-01"
  )
  
  new <- data.frame(
    EST_ID = "B",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith",
    UPN = "U001"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_equal(result$OME_ID, "O001")
  testthat::expect_equal(result$MATCH_METHOD, "UPN")
})


testthat::test_that("Missing and blank master UPNs are excluded", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = c("O001", "O002", "O003"),
    FIRST_NAME = c("Alice", "Bob", "Charlie"),
    LAST_NAME = c("Smith", "Jones", "Brown"),
    EST_ID = c("A", "A", "A"),
    UPN = c("", "NA", "U003"),
    FNS = c("Alice", "Bob", "Charlie"),
    LNS = c("Smith", "Jones", "Brown")
  )
  
  new <- data.frame(
    EST_ID = rep("A", 4),
    PUPIL_FIRST_NAME = c(
      "Alice", "Bob", "Charlie", "Diana"
    ),
    PUPIL_LAST_NAME = c(
      "Smith", "Jones", "Brown", "Evans"
    ),
    UPN = c("", "NA", "U003", NA_character_)
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_identical(
    result$OME_ID,
    c(NA_character_, NA_character_, "O003", NA_character_)
  )
  
  testthat::expect_identical(
    result$MATCH_METHOD,
    c("None", "None", "UPN", "None")
  )
})


testthat::test_that("Duplicate UPNs use the first master-list match", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = c("O001", "O002"),
    FIRST_NAME = c("Alice", "Bob"),
    LAST_NAME = c("Smith", "Jones"),
    EST_ID = c("A", "B"),
    UPN = c("U001", "U001"),
    FNS = c("Alice", "Bob"),
    LNS = c("Smith", "Jones")
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Bob",
    PUPIL_LAST_NAME = "Jones",
    UPN = "U001"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  # Preserve the original match() methodology.
  testthat::expect_equal(result$OME_ID, "O001")
  testthat::expect_equal(result$MATCH_METHOD, "UPN")
  
  testthat::expect_equal(
    result$OLD_STUDENT_FIRST_NAME,
    "alice"
  )
})


testthat::test_that("Multiple historical names are retained for UPN matches", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    FIRST_NAME = "William",
    LAST_NAME = "Smith",
    EST_ID = "A",
    UPN = "U001",
    FNS = "William---Will---Billy",
    LNS = "Smith---Jones",
    DOBS = "2015-01-01---2015-01-02"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Billy",
    PUPIL_LAST_NAME = "Jones",
    UPN = "U001"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_equal(result$OME_ID, "O001")
  testthat::expect_equal(result$MATCH_METHOD, "UPN")
  
  testthat::expect_equal(
    result$OLD_STUDENT_FIRST_NAME,
    "william---will---billy"
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_LAST_NAME,
    "smith---jones"
  )
  
  testthat::expect_equal(
    result$OLD_DOB,
    "2015-01-01---2015-01-02"
  )
})


testthat::test_that("fuzzy_match FALSE skips name matching", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    FIRST_NAME = "Alice",
    LAST_NAME = "Smith",
    EST_ID = "A",
    FNS = "Alice",
    LNS = "Smith"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_true(is.na(result$OME_ID))
  testthat::expect_equal(result$MATCH_METHOD, "None")
})


testthat::test_that("OMELink matches names from FNS and LNS", {
  
  testthat::skip_if_not_installed("OMELink")
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    FIRST_NAME = "William",
    LAST_NAME = "Smith",
    EST_ID = "A",
    FNS = "William---Will---Billy",
    LNS = "Smith---Jones",
    DOBS = "2015-01-01"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Billy",
    PUPIL_LAST_NAME = "Jones"
  )
  
  result <- add_student_ids_from_previous(
    new,
    master,
    fuzzy_match = TRUE,
    max_dist = 2,
    method = "osa"
  )
  
  testthat::expect_equal(result$OME_ID, "O001")
  
  testthat::expect_match(
    result$MATCH_METHOD,
    "MATCH"
  )
  
  testthat::expect_true(
    result$OLD_STUDENT_FIRST_NAME %in%
      c("william", "will", "billy")
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_LAST_NAME,
    "jones"
  )
  
  testthat::expect_identical(
    result$OLD_STUDENT_FIRST_NAME,
    tolower(result$OLD_STUDENT_FIRST_NAME)
  )
  
  testthat::expect_identical(
    result$OLD_STUDENT_LAST_NAME,
    tolower(result$OLD_STUDENT_LAST_NAME)
  )
})


testthat::test_that("OMELink name matching is restricted to school", {
  
  testthat::skip_if_not_installed("OMELink")
  reset_column_names()
  
  master <- data.frame(
    OME_ID = c("O001", "O002"),
    FIRST_NAME = c("Alice", "Bob"),
    LAST_NAME = c("Smith", "Jones"),
    EST_ID = c("A", "B"),
    FNS = c("Alice", "Bob"),
    LNS = c("Smith", "Jones")
  )
  
  new <- data.frame(
    EST_ID = "B",
    PUPIL_FIRST_NAME = "Bob",
    PUPIL_LAST_NAME = "Jones"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = TRUE
  )
  
  testthat::expect_equal(result$OME_ID, "O002")
  
  testthat::expect_equal(
    result$OLD_STUDENT_FIRST_NAME,
    "bob"
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_LAST_NAME,
    "jones"
  )
})


testthat::test_that("Missing school data prevents name matching", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    FIRST_NAME = "Alice",
    LAST_NAME = "Smith",
    EST_ID = "A",
    FNS = "Alice",
    LNS = "Smith"
  )
  
  new <- data.frame(
    EST_ID = "B",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = TRUE
  )
  
  testthat::expect_true(is.na(result$OME_ID))
  
  testthat::expect_equal(
    result$MATCH_METHOD,
    "No school data"
  )
})


testthat::test_that("FNS and LNS are independent of primary name columns", {
  
  testthat::skip_if_not_installed("OMELink")
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    FIRST_NAME = "William",
    LAST_NAME = "Smith",
    EST_ID = "A",
    FNS = "William---Will---Billy",
    LNS = "Smith---Jones"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Billy",
    PUPIL_LAST_NAME = "Jones"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = TRUE
  )
  
  testthat::expect_equal(result$OME_ID, "O001")
  
  testthat::expect_true(
    result$OLD_STUDENT_FIRST_NAME %in%
      c("william", "will", "billy")
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_LAST_NAME,
    "jones"
  )
})


testthat::test_that("Additional master-list columns are ignored", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    PRINT_ID = "P001",
    FIRST_NAME = "Alice",
    LAST_NAME = "Smith",
    EST_ID = "A",
    EST_CATEGORY = "Primary",
    CLASS_ID = "C001",
    UPN = "U001",
    FNS = "Alice---Alicia",
    LNS = "Smith---Jones",
    DOBS = "2015-01-01---2015-01-02",
    EXTRA_COLUMN = "Additional information"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith",
    UPN = "U001"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_equal(result$OME_ID, "O001")
  
  testthat::expect_equal(
    result$OLD_STUDENT_FIRST_NAME,
    "alice---alicia"
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_LAST_NAME,
    "smith---jones"
  )
  
  testthat::expect_false(
    "EXTRA_COLUMN" %in% names(result)
  )
  
  testthat::expect_false(
    "PRINT_ID" %in% names(result)
  )
})


testthat::test_that("Master-list headers can be configured independently", {
  
  reset_column_names()
  
  master <- data.frame(
    MASTER_OME_ID = "O001",
    MASTER_FN_HISTORY = "Alice---Alicia",
    MASTER_LN_HISTORY = "Smith---Jones",
    MASTER_EST = "A",
    MASTER_UPN = "U001",
    MASTER_DOB_HISTORY = "2015-01-01"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alicia",
    PUPIL_LAST_NAME = "Jones",
    UPN = "U001"
  )
  
  result <- add_student_ids_from_previous(
    new,
    master,
    master_columns = c(
      ome_id = "MASTER_OME_ID",
      sfn = "MASTER_FN_HISTORY",
      sln = "MASTER_LN_HISTORY",
      dob = "MASTER_DOB_HISTORY",
      upn = "MASTER_UPN",
      est_id = "MASTER_EST"
    ),
    fuzzy_match = FALSE
  )
  
  testthat::expect_equal(result$OME_ID, "O001")
  testthat::expect_equal(result$MATCH_METHOD, "UPN")
  
  testthat::expect_equal(
    result$OLD_STUDENT_FIRST_NAME,
    "alice---alicia"
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_LAST_NAME,
    "smith---jones"
  )
  
  testthat::expect_equal(
    result$OLD_DOB,
    "2015-01-01"
  )
})


testthat::test_that("Custom class-list column names are supported", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    ome_id = "STUDENT_IDENTIFIER",
    sfn = "CHILD_FIRST",
    sln = "CHILD_LAST",
    est_id = "SCHOOL_IDENTIFIER",
    upn = "PUPIL_UPN",
    dob = "CHILD_DOB"
  )
  
  master <- data.frame(
    OME_ID = "O001",
    EST_ID = "A",
    UPN = "U001",
    FNS = "Alice---Alicia",
    LNS = "Smith---Jones",
    DOBS = "2015-01-01"
  )
  
  new <- data.frame(
    SCHOOL_IDENTIFIER = "A",
    CHILD_FIRST = "Alicia",
    CHILD_LAST = "Jones",
    PUPIL_UPN = "U001",
    CHILD_DOB = "2015-01-01"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_equal(
    result$STUDENT_IDENTIFIER,
    "O001"
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_FIRST_NAME,
    "alice---alicia"
  )
  
  testthat::expect_equal(
    result$OLD_STUDENT_LAST_NAME,
    "smith---jones"
  )
  
  testthat::expect_true(
    all(c(
      "SCHOOL_IDENTIFIER",
      "CHILD_FIRST",
      "CHILD_LAST",
      "PUPIL_UPN",
      "CHILD_DOB"
    ) %in% names(result))
  )
  
  testthat::expect_false(
    "OME_ID" %in% names(result)
  )
})


testthat::test_that("Internal class-list names are supported", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    EST_ID = "A",
    UPN = "U001",
    FNS = "Alice",
    LNS = "Smith",
    DOBS = "2015-01-01"
  )
  
  new <- data.frame(
    est_id = "A",
    sfn = "Alice",
    sln = "Smith",
    upn = "U001"
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_equal(result$OME_ID, "O001")
  
  testthat::expect_equal(
    result$OLD_STUDENT_FIRST_NAME,
    "alice"
  )
  
  testthat::expect_true(
    all(c(
      "EST_ID",
      "PUPIL_FIRST_NAME",
      "PUPIL_LAST_NAME"
    ) %in% names(result))
  )
})


testthat::test_that("Alternative school identifier is supported", {
  
  testthat::skip_if_not_installed("OMELink")
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    EST_ID = "A",
    URN = "123456",
    FNS = "Alice",
    LNS = "Smith"
  )
  
  new <- data.frame(
    URN = "123456",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  result <- add_student_ids_from_previous(
    new,
    master,
    master_columns = c(
      ome_id = "OME_ID",
      sfn = "FNS",
      sln = "LNS",
      est_id = "EST_ID",
      urn = "URN"
    ),
    school_id_column = "urn",
    fuzzy_match = TRUE
  )
  
  testthat::expect_equal(result$OME_ID, "O001")
})


testthat::test_that("Unknown school identifier produces an error", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    EST_ID = "A",
    FNS = "Alice",
    LNS = "Smith"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  testthat::expect_error(
    add_student_ids_from_previous(
      new,
      master,
      school_id_column = "INVALID_COLUMN",
      fuzzy_match = FALSE
    ),
    "Unknown school identifier column"
  )
})


testthat::test_that("Missing class-list columns produce an error", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    EST_ID = "A",
    FNS = "Alice",
    LNS = "Smith"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice"
  )
  
  testthat::expect_error(
    add_student_ids_from_previous(
      new, master, fuzzy_match = FALSE
    ),
    "Missing columns in new_class_list"
  )
})


testthat::test_that("Missing master-list columns produce an error", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    EST_ID = "A",
    FNS = "Alice"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  testthat::expect_error(
    add_student_ids_from_previous(
      new, master, fuzzy_match = FALSE
    ),
    "Missing columns in master_list"
  )
})


testthat::test_that("Missing required master mapping produces an error", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    EST_ID = "A",
    FNS = "Alice",
    LNS = "Smith"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith"
  )
  
  testthat::expect_error(
    add_student_ids_from_previous(
      new,
      master,
      master_columns = c(
        ome_id = "OME_ID",
        sfn = "FNS",
        sln = "LNS"
      ),
      fuzzy_match = FALSE
    ),
    "master_columns must include"
  )
})


testthat::test_that("Input data frames remain unchanged", {
  
  reset_column_names()
  
  master <- data.frame(
    OME_ID = "O001",
    EST_ID = "A",
    UPN = "U001",
    FNS = "Alice---Alicia",
    LNS = "Smith---Jones",
    DOBS = "2015-01-01"
  )
  
  new <- data.frame(
    EST_ID = "A",
    PUPIL_FIRST_NAME = "Alice",
    PUPIL_LAST_NAME = "Smith",
    UPN = "U001"
  )
  
  original_master <- master
  original_new <- new
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = FALSE
  )
  
  testthat::expect_identical(master, original_master)
  testthat::expect_identical(new, original_new)
  
  testthat::expect_equal(result$OME_ID, "O001")
})


testthat::test_that("Matched old names are always lowercase", {
  
  testthat::skip_if_not_installed("OMELink")
  reset_column_names()
  
  master <- data.frame(
    OME_ID = c("O001", "O002"),
    EST_ID = c("A", "A"),
    UPN = c("U001", "U002"),
    FNS = c("ALICE---ALICIA", "WILLIAM---WILL---BILLY"),
    LNS = c("SMITH---JONES", "BROWN---EVANS")
  )
  
  new <- data.frame(
    EST_ID = c("A", "A"),
    PUPIL_FIRST_NAME = c("Alice", "Billy"),
    PUPIL_LAST_NAME = c("Smith", "Evans"),
    UPN = c("U001", NA_character_)
  )
  
  result <- add_student_ids_from_previous(
    new, master, fuzzy_match = TRUE
  )
  
  testthat::expect_equal(
    result$OME_ID,
    c("O001", "O002")
  )
  
  testthat::expect_identical(
    result$OLD_STUDENT_FIRST_NAME,
    tolower(result$OLD_STUDENT_FIRST_NAME)
  )
  
  testthat::expect_identical(
    result$OLD_STUDENT_LAST_NAME,
    tolower(result$OLD_STUDENT_LAST_NAME)
  )
  
  testthat::expect_equal(
    result$MATCH_METHOD[1],
    "UPN"
  )
  
  testthat::expect_match(
    result$MATCH_METHOD[2],
    "MATCH"
  )
})

