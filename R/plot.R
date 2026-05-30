#' A lollipop graph of each countries mean incidence rate.
#'
#' @param countries
#'
#' @return A graph
#'
#'

load_urban_data <- function(){
  urban_df <- readr::read_csv("Urban Population Percentage Dataframe.csv")

  cases_year_urban_pop_df <- dplyr::left_join(
    cases_year,
    urban_df,
    by = "iso3"
  )

  return(cases_year_urban_pop_df)
}

urban_type_incident <- function(df, country_iso3) {

  df |>
    dplyr::filter(iso3 == country_iso3) |>
    dplyr::mutate(
      urban_type = dplyr::case_when(
        urban_population > 80 ~ "Highly Urbanized",
        urban_population > 50 ~ "Mixed",
        TRUE ~ "Primarily Rural"
      )
    ) |>
    dplyr::summarise(
      iso3 = country_iso3,
      urban_type = dplyr::first(urban_type),
      mean_incident = mean(measles_incidence_rate_per_1000000_total_population)
    )
}

urban_data <- function() {
  df <- load_urban_data()

  urban_type_df <- purrr::map_dfr(
    unique(df$iso3),
    ~ urban_type_incident(df, .x)
  )

  urban_pop_type_df <- df |>
    dplyr::left_join(urban_type_df, by = "iso3") |>
    dplyr::select(country, iso3, urban_population, urban_type,
      measles_incidence_rate_per_1000000_total_population,
      mean_incident
    )

  return(urban_pop_type_df)
}


mean_measles_by_country <- function(urban_data, countries){

  if (!all(countries %in% unique(urban_data$country))) {
    stop("One or more countries not found in dataset")
  }

  df <- urban_data |>
    dplyr::filter(country %in% countries)

  urban_type_means <- df |>
    dplyr::group_by(urban_type) |>
    dplyr::summarise(
      mean_by_type = mean(measles_incidence_rate_per_1000000_total_population, na.rm = TRUE),
      .groups = "drop"
    )

  country_means <- df |>
    dplyr::group_by(country, iso3, urban_type) |>
    dplyr::summarise(
      mean_incident = mean(measles_incidence_rate_per_1000000_total_population, na.rm = TRUE),
      .groups = "drop"
    )

  plot_df <- country_means |>
    dplyr::left_join(urban_type_means, by = "urban_type")

  ggplot2::ggplot(plot_df, ggplot2::aes(x = iso3, y = mean_incident, color = urban_type)) +
    ggplot2::geom_segment(
      aes(xend = iso3, y = 0, yend = mean_incident),
      color = "gray50"
    ) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_hline(
      data = urban_type_means,
      aes(yintercept = mean_by_type, color = urban_type),
      linetype = "dashed",
      size = 0.8,
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      title = "Mean Measles Incidence Rate by Country",
      subtitle = "Horizontal lines show mean incidence per urban classification",
      x = "",
      y = "Mean Measles Incidence Rate",
      color = "Urban Classification"
    ) +
    ggplot2::scale_color_brewer(palette = "Spectral") +
    ggplot2::theme_minimal(base_size = 14)
}









#' #' A plot of suspected measles cases per month for a selected country
#' #'
#' #' @param selected_country
#' #'
#' #' @return A graph
#' #'
#' #'
#'
#' summ_sus_measles_per_month <- function(country) {
#'   data |>
#'     dplyr::filter(country == selected_country) |>
#'     dplyr::group_by(country, month) |>
#'     dplyr::summarise(
#'       mean = mean(suspected_measles_cases, na.rm = TRUE),
#'       SD = sd(suspected_measles_cases, na.rm = TRUE)
#'     )
#' }
#'
#'
#' plot_sus_measles_per_month <- function(data, selected_country) {
#'     tabledata <- summ_sus_measles_per_month(data, selected_country) |>
#'       dplyr::mutate(
#'         month = factor(
#'           substr(month, 1, 3),
#'           ordered = TRUE
#'         )
#'       )
#'
#'     ggplot2::ggplot(tabledata, ggplot2::aes(x = month, y = mean)) +
#'       ggplot2::geom_col(fill = "#FC8D59") +
#'       ggplot2::labs(
#'         title = paste("Suspected Measles Cases per Month for", selected_country),
#'         x = "Month",
#'         y = "Mean Cases"
#'       ) +
#'       ggplot2::theme_minimal(base_size = 14)
#'   }
