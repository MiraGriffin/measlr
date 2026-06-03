#' load data
#'
#' @return a data frame
#'
#' @export

load_month_data <- function() {
  data <- load_data()
  return(data$cases_month)
}

#' cleans data
#'
#' @param country_name A character vector of country names
#'
#' @return a tibble
#'
#' @export
#'

summ_sus_measles_per_month <- function(country_name) {
  cases_month <- load_month_data()

  country_data <- cases_month |>
    dplyr::filter(country == country_name)

  if (nrow(country_data) == 0) {
    stop("Country not found in dataset")}

  summ_sus_measles <- tibble::tibble()

  for(mon in 1:12) {
    mon_data <- country_data |>
      dplyr::filter(month == mon)

    summ_sus_measles <- dplyr::bind_rows(summ_sus_measles, tibble::tibble(
      country = country_name,
      month = month.name[mon],
      mean = mean(mon_data$measles_suspect, na.rm = TRUE),
      SD = sd(mon_data$measles_suspect, na.rm = TRUE)
    ))
  }

  return(summ_sus_measles)
}


#' A graph of each countries measles cases per month
#'
#' @param country_name A character vector of country names
#'
#' @return A graph
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export

# plot 2 function

country_plot <- function(country_name){

  tabledata <- summ_sus_measles_per_month(country_name) |>
    dplyr::mutate(
      month = factor(
        substr(month, 1, 3),                 # Jan, Feb, Mar...
        levels = month.abb,                  # Jan–Dec in order
        ordered = TRUE
      )
    )

  ggplot2::ggplot(tabledata, aes(x = month, y = mean)) +
    ggplot2::geom_col(fill = "#FC8D59") +
    ggplot2::labs(
      title = paste("Suspected Measles Cases per Month for", country_name),
      x = "Month",
      y = "Mean Cases"
    ) +
    theme_minimal(base_size = 14)
}

