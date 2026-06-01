#' Load measles data from a CSV URL
#'
#' This function downloads and reads the measles data sets used in the analysis.
#'
#' @return A tibble with the loaded dataset.
#'
#' @importFrom arrow read_parquet
#' @export
load_data <- function(){
  path1 <- system.file("extdata", "cases_month.parquet", package = "measlr")
  cases_month <- read_parquet(path1)
  path2 <- system.file("extdata", "cases_year.parquet", package = "measlr")
  cases_year <- read_parquet(path2)
  path3 <- system.file("extdata", "urban.parquet", package = "measlr")
  urban <- read_parquet(path3)
  list(
    cases_month = cases_month,
    cases_year = cases_year,
    urban = urban
  )
}




