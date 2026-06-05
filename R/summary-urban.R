#' A table for the selected country that includes mean and sd measles incidence.
#'
#' @param country_name A country name
#'
#' @return A data frame with the country, mean incidence, and standard deviation of incidence.
#' @export

mean_measles_per_month_for_country <- function(country_name = "United States") {
  data <- load_data()
  country_data <- data$cases_month |>
    dplyr::filter(country == country_name)

  country_data |>
    dplyr::filter(.data$country == country_name) |>
    dplyr::group_by(.data$country, .data$month) |>
    dplyr::summarise(
      mean_measles_incidence = mean(measles_total, na.rm = TRUE),
      sd_measles_incidence = sd(measles_total, na.rm = TRUE))
}

