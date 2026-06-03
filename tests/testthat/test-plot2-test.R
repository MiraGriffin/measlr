test_that("country_plot returns a ggplot", {

  cases_month <- load_month_data()
  country_name <- "China"
  wrong_country <- "Denver"

  one_country <- country_plot(country_name)

  #Tests passing in only a single country
  expect_s3_class(one_country, "ggplot")

  # Tests passing in countries that are not in the data file
  expect_error(
    country_plot(wrong_country),
    "Country not found in dataset"
  )
})
