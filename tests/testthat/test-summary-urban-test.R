test_that("output is a data frame", {

  result <- mean_measles_per_month_for_country("China")

  expect_s3_class(result, "data.frame")

  expect_equal(names(result), c("country",
                                "month",
                                "mean_measles_incidence",
                                "sd_measles_incidence"))

  expect_equal(result$country[1], "China")
  expect_equal(result$month[1], 1)
  expect_equal(result$mean_measles_incidence[1], round(729.714, 3)
  expect_equal(result$sd_measles_incidence[1], round(922.900, 3)
})

