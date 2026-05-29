test_that("mean_measles_by_country returns a ggplot", {
  data_urban   <- urban_data
  countries   <- c("United States", "India")
  country <- "United States"
  wrong_countries <- c("United States", "Denver")

  # Tests that a vector of countries works
  multiple_countries <- mean_measles_by_country(dat_urban, countries)

  #Tests that an input of a single country works
  one_country <- mean_measles_by_country(dat_urban, countries)

  expect_s3_class(multiple_countries, "ggplot")
  expect_s3_class(one_countries, "ggplot")

  # Tests that an error is thrown if at least one of the countries inputted is
  # not found in the dataset.
  expect_error(
    mean_measles_by_country(measles_data, urban_data, wrong_countries),
    "One or more countries not found in dataset"
  )
})
