#' A table for the selected country that includes mean and sd measles incidence.
#'
#' @param country
#'
#' @return A data frame with the country, mean incidence, and standard deviation of incidence.
#' @export

mean_measles_per_month_for_country <- function(country_name) {

  country_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv') |>
    dplyr::filter(country == country_name)

  country_data |>
    dplyr::filter(.data$country == country_name) |>
    dplyr::group_by(.data$country, .data$month) |>
    dplyr::summarise(
      mean_measles_incidence = mean(measles_total, na.rm = TRUE),
      sd_measles_incidence = sd(measles_total, na.rm = TRUE))
}

