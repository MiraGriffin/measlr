test_that("country_incidence_summary returns summary statistics", {
  # set up
  result <- country_incidence_summary("DZA")

  # returning a data frame
  expect_s3_class(result, "data.frame")

  # returning one row
  expect_equal(nrow(result), 1)

  # has the correct columns
  expect_true(all(c("iso3", "urban_type", "mean_incident", "sd_incident")
                  %in% names(result)
                  ))

  # mean and sd are numeric
  expect_true(is.numeric(result$mean_incident))
  expect_true(is.numeric(result$sd_incident))

})

test_that("country_incidence_summary handles invalid iso3 codes", {
  expect_error(country_incidence_summary("XYZABC"),
               "Country ISO3 code not found")
})
