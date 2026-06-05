test_that("prepare_mean_measles_data filters and summarizes correctly", {

  urban <- urban_data()
  countries <- unique(urban$country)[1:2]

  out <- prepare_mean_measles_data(urban, countries)

  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)

  expect_true(all(c(
    "country",
    "iso3",
    "urban_type",
    "mean_incident",
    "mean_by_type"
  ) %in% names(out)))

  expect_true(all(out$country %in% countries))
  expect_error(
    prepare_mean_measles_data(urban, c("China", "Atlantis")),
    "One or more countries not found in dataset"
  )
})

