test_that("output is a data frame", {

  result <- mean_measles_per_month_for_country(test_data, "China")

  expect_s3_class(result, "data.frame")
})

