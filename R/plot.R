#' A plot of suspected measles cases per month for a selected country
#'
#' @param selected_country
#'
#' @return A graph
#'
#'

summ_sus_measles_per_month <- function(country) {
  data |>
    dplyr::filter(country == selected_country) |>
    dplyr::group_by(country, month) |>
    dplyr::summarise(
      mean = mean(suspected_measles_cases, na.rm = TRUE),
      SD = sd(suspected_measles_cases, na.rm = TRUE)
    )
}


plot_sus_measles_per_month <- function(data, selected_country) {
    tabledata <- summ_sus_measles_per_month(data, selected_country) |>
      dplyr::mutate(
        month = factor(
          substr(month, 1, 3),
          ordered = TRUE
        )
      )

    ggplot2::ggplot(tabledata, ggplot2::aes(x = month, y = mean)) +
      ggplot2::geom_col(fill = "#FC8D59") +
      ggplot2::labs(
        title = paste("Suspected Measles Cases per Month for", selected_country),
        x = "Month",
        y = "Mean Cases"
      ) +
      ggplot2::theme_minimal(base_size = 14)
  }
