#' Load measles data from a CSV URL
#'
#' This function downloads and reads the measles data sets used in the analysis.
#'
#' @return A tibble with the loaded dataset.
#'
#' @importFrom readr read_csv
#' @export
load_data <- function(){
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv')
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')
  readr::read_csv('Urban Population Percentage Dataframe.csv')
  readr::read_csv('data-table.csv')
}

country <- function() {
  load_data() |>
    distinct(country) |>
    pull(country) |>
    as.character()
}



