#' Summarize measles incidence rate per 1000000 of the population for a country
#'
#'
#'@param country_iso3 iso3 of country to be analyzed
#'
#'@return a data frame containing the country's urban classification
#' the mean and standard deviation of measles
#' incidence rates for a selected country
#'
#'@importFrom data.table as.data.table
#'@importFrom data.table first
#'@export
country_incidence_summary <- function(country_iso3) {
  df<- load_urban_data()
  df <- data.table::as.data.table(df)

  if(!(country_iso3 %in% df$iso3)){
    stop("Country ISO3 code not found")
  }

  df[
    ,
    urban_type := ifelse(
      urban_population > 80,
      "Highly Urbanized",
      ifelse(
        urban_population > 50,
        "Mixed",
        "Primarily Rural"
      )
    )
  ]

  df[
    iso3 == country_iso3,
    list(
      urban_type = data.table::first(urban_type),
      mean_incident = mean(
        measles_incidence_rate_per_1000000_total_population, na.rm = TRUE
      ),
      sd_incident = sd(
        measles_incidence_rate_per_1000000_total_population, na.rm = TRUE
      )
    ),
    by = iso3
  ]

}

