test_that("measles_model_setup works correctly", {

  df <- measles_model_setup()

  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)

  expect_true(all(c(
    "iso3",
    "prop_tot_measles_per_month",
    "urban_population"
  ) %in% names(df)))

  iso3_vals <- unique(df$iso3)[1:2]
  df_filtered <- df[df$iso3 %in% iso3_vals, ]

  expect_true(all(df_filtered$iso3 %in% iso3_vals))

  expect_error(
    measles_model_setup("NotARealCountry"),
    "No data available"
  )

})

test_that("make_measles_model_table works correctly", {
  tbl1 <- make_measles_model_table()
  expect_s3_class(tbl1, "gt_tbl")

  df <- measles_model_setup()
  countries <- unique(df$country)[1:2] |> na.omit()

  tbl2 <- make_measles_model_table(countries)
  expect_s3_class(tbl2, "gt_tbl")

  expect_error(
    make_measles_model_table("NotARealCountry"),
    "No data available"
  )
})
