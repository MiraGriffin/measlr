#' A lollipop graph of each countries mean incidence rate.
#'
#' @param countries A character vector of country names
#'
#' @return A graph
#'
#' @import ggplot2
#'
#' @export
mean_measles_by_country <- function(urban_data, countries){

  if (!all(countries %in% unique(urban_data$country))) {
    stop("One or more countries not found in dataset")
  }

  plot_df <- prepare_mean_measles_data(urban_data, countries)

  urban_type_means <- dplyr::distinct(
    plot_df,
    urban_type,
    mean_by_type
  )

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




