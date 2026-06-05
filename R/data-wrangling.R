#' Merges urban data with cases_year data
#'
#' @return a data frame

load_urban_data <- function(){
  data <- load_data()
  urban_df <- data$urban
  cases_year_urban_pop_df <- dplyr::left_join(
    data$cases_year,
    urban_df,
    by = "iso3"
  )

  return(cases_year_urban_pop_df)
}

#' Selects countries from urban data set and gives urban classification and mean measles incident rate
#'
#' @param df dataframe with urban populations
#' @param country_iso3 iso3 of countries to be analyzed
#'
#' @return a data frame
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

#' Joins the urban data set with urban classifications and the merged urban data set
#'
#' @return a dataframe
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

#' Merges and filters data for mean measles incidence plot
#'
#' @param urban_data A data frame from urban_data()
#' @param countries A character vector of country names
#'
#' @return A data frame
#' @export
prepare_mean_measles_data <- function(urban_data, countries) {

  if (!all(countries %in% unique(urban_data$country))) {
    stop("One or more countries not found in dataset")
  }

  df <- urban_data |>
    dplyr::filter(country %in% countries)

  urban_type_means <- df |>
    dplyr::group_by(urban_type) |>
    dplyr::summarise(
      mean_by_type = mean(
        measles_incidence_rate_per_1000000_total_population,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  country_means <- df |>
    dplyr::group_by(country, iso3, urban_type) |>
    dplyr::summarise(
      mean_incident = mean(
        measles_incidence_rate_per_1000000_total_population,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  plot_df <- country_means |>
    dplyr::left_join(urban_type_means, by = "urban_type")

  return(plot_df)
}
