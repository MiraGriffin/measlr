
test_that("summary_urban returns expected output", {

  result <- summary_urban(cases_year_urban_pop_df)

  # Check object type
  expect_s3_class(result, "tbl_df")

  # Check column names
  expect_equal(
    names(result),
    c("urban_type", "mean_by_type", "sd_by_type")
  )

  # Check number of rows
  expect_equal(nrow(result), 3)

  # Check urban type labels
  expect_true(all(
    result$urban_type %in%
      c(
        "Highly Urbanized",
        "Mixed",
        "Primarily Rural"
      )
  ))
})
