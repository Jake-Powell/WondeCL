
testthat::test_that("Default source column uses configured ome_id", {
  
  reset_column_names()
  
  df <- data.frame(
    OME_ID = c(101, 255, 4096)
  )
  
  result <- add_hex_id(df)
  
  hex_col <- get_column_names()[["hex_id"]]
  
  testthat::expect_equal(
    result[[hex_col]],
    as.character(as.hexmode(df$OME_ID))
  )
})


testthat::test_that("Default source supports internal ome_id", {
  
  reset_column_names()
  
  df <- data.frame(
    ome_id = c(101, 255, 4096)
  )
  
  result <- add_hex_id(df)
  
  testthat::expect_equal(
    result[[get_column_names()[["hex_id"]]]],
    as.character(as.hexmode(df$ome_id))
  )
})


testthat::test_that("Custom ome_id and hex_id configuration works by default", {
  
  reset_column_names()
  on.exit(reset_column_names(), add = TRUE)
  
  set_column_names(
    ome_id = "STUDENT_IDENTIFIER",
    hex_id = "HEX_IDENTIFIER"
  )
  
  df <- data.frame(
    STUDENT_IDENTIFIER = c(101, 255, 4096)
  )
  
  result <- add_hex_id(df)
  
  testthat::expect_identical(
    names(result),
    c("STUDENT_IDENTIFIER", "HEX_IDENTIFIER")
  )
  
  testthat::expect_equal(
    result$HEX_IDENTIFIER,
    as.character(as.hexmode(df$STUDENT_IDENTIFIER))
  )
})


testthat::test_that("Explicit source ID overrides default ome_id", {
  
  reset_column_names()
  
  df <- data.frame(
    OME_ID = c(10, 20),
    ID = c(255, 4096)
  )
  
  result <- add_hex_id(
    df,
    ID_column = "ID"
  )
  
  testthat::expect_equal(
    result[[get_column_names()[["hex_id"]]]],
    as.character(as.hexmode(df$ID))
  )
})

