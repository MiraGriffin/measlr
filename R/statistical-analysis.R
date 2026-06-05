#' Build a regression model table for measles proportion vs. urban population
#'
#' @param countries Optional character vector of countries to include.
#'
#' @return A gt table summarizing the regression model.
#' @export
make_measles_model_table <- function(countries = NULL) {
  measles_model <- lm(
    prop_tot_measles_per_month ~ urban_population,
    data = measles_model_setup(countries)
  )

  # ---- 5. Extract and format coefficients ----
  model_df <- as.data.frame(
    summary(measles_model)$coefficients,
    check.names = FALSE
  ) |>
    tibble::rownames_to_column("term") |>
    dplyr::filter(term != "Intercept") |>
    dplyr::arrange(dplyr::desc(abs(Estimate))) |>
    dplyr::mutate(
      term = gsub("country", "", term),
      Estimate = round(Estimate, 3),
      `Std. Error` = round(`Std. Error`, 3),
      `t value` = round(`t value`, 2),
      `p value` = round(`Pr(>|t|)`, 3)
    ) |>
    dplyr::select(term, Estimate, `Std. Error`, `t value`, `p value`)




  model_table <- model_df |>
    gt::gt() |>
    gt::tab_header(
      title = gt::md("Multiple Linear Regression Model for Measles Proportion")
    ) |>
    gt::tab_caption(
      caption = gt::md("Table: Estimates of differences in measles proportion")
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::fmt_percent(columns = Estimate) |>
    gt::cols_label(
      term = "Country",
      Estimate = "Estimate",
      `Std. Error` = "Std Error",
      `t value` = "t-value",
      `p value` = "p-value"
    ) |>
    gt::fmt_number(
      columns = -c(term, Estimate),
      decimals = 4
    ) |>
    gt::text_transform(
      locations = gt::cells_body(columns = term),
      fn = function(x) {
        dplyr::recode(
          x,
          "(Intercept)" = "Baseline",
          "urban_population" = "Urban Population Rate"
        )
      }
    )

  return(model_table)
}

#' Sets up data to be used to create model table
#'
#' @param countries Optional character vector of countries to include.
#'
#' @return A dataframe with measles proportion per month and urban population rates for specified countries
measles_model_setup <- function(countries = NULL) {

  data <- load_data()
  cases_month <- data$cases_month
  cases_year <- data$cases_year
  urban_df <- urban_data()

  measles_prop_month <- cases_month |>
    dplyr::group_by(iso3, country, month) |>
    dplyr::mutate(
      prop_tot_measles_per_month =
        measles_total / (measles_total + rubella_total)
    ) |>
    dplyr::filter(
      is.finite(prop_tot_measles_per_month),
      !is.na(prop_tot_measles_per_month)
    ) |>
    dplyr::select(country, iso3, month, prop_tot_measles_per_month) |>
    dplyr::ungroup()

  if (!is.null(countries)) {
    measles_prop_month <- measles_prop_month |>
      dplyr::filter(country %in% countries)

    urban_df <- urban_df |>
      dplyr::filter(country %in% countries)
  }

  merged <- dplyr::left_join(
    measles_prop_month,
    urban_df,
    by = "iso3"
  )

  if (nrow(merged) == 0) {
    stop("No data available for the selected countries after merging.")
  }

  return(merged)

}


