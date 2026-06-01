test_that("mean_measles_by_country returns a ggplot", {

  #Set up

  data_urban <- urban_data()
  countries <- c("China", "India")
  country <- "China"
  wrong_countries <- c("China", "Denver")

  multiple_countries <- mean_measles_by_country(data_urban, countries)
  one_country <- mean_measles_by_country(data_urban, country)

  # Tests passing in multiple countries in a vector
  expect_s3_class(multiple_countries, "ggplot")

  #Tests passing in only a single country
  expect_s3_class(one_country, "ggplot")

  # Tests passing in countries that are not in the data file
  expect_error(
    mean_measles_by_country(data_urban, wrong_countries),
    "One or more countries not found in dataset"
  )
})
