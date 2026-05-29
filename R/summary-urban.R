#' Summarize measles incidence rates by urban population classification
#'
#' @param country_iso3 countries
#'
#' @return A tibble
#'
#' @importFrom dplyr mutate summarise left_join
#' @export



summary_urban <- function(country) {
  load_data() |>
    dplyr::mutate(
      urban_type = dplyr::case_when(
        urban_population > 80 ~ "Highly Urbanized",
        urban_population > 50 ~ "Mixed",
        TRUE ~ "Primarily Rural"
      )
    ) |>
    dplyr::group_by(urban_type) |>
    dplyr::summarise(
      mean_by_type = mean(
        measles_incidence_rate_per_1000000_total_population,
        na.rm = TRUE
      ),
      sd_by_type = sd(
        measles_incidence_rate_per_1000000_total_population,
        na.rm = TRUE
      )
    )
}
