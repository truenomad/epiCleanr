#' @title Simulated Epidemiological Dataset for Togo
#' @description A dataset representing simulated epidemiological data
#'   for malaria and cholera across various states in Togo from 2018-2022.
#'
#' @format A data frame with 900 rows and 8 variables:
#' \describe{
#'   \item{\code{country}}{The country of the data. Always 'Togo' in this
#'   dataset.}
#'   \item{\code{district}}{The state in Togo where the data was collected.
#'   Includes 15 different districts}
#' \item{\code{region}}{The region in Togo where the data was collected.
#'   Includes 4 different regions}
#'   \item{\code{month}}{The month when the data was recorded, ranging from
#'   1 to 12.}
#'   \item{\code{year}}{The year when the data was recorded, ranging from 2018
#'   to 2022.}
#'   \item{\code{malaria_tests}}{The number of tests conducted for malaria.
#'   Includes random missing values and outliers.}
#'   \item{\code{malaria_cases}}{The number of confirmed cases for malaria.
#'   Includes random missing values and outliers.}
#'   \item{\code{cholera_tests}}{The number of tests conducted for cholera.
#'   Includes random missing values and outliers.}
#'   \item{\code{cholera_cases}}{The number of confirmed cases for cholera.
#'   Includes random missing values and outliers.}
#' }
#'
#' @details
#' The dataset was generated using various steps:
#' \itemize{
#'   \item The dataset was initiated using `dplyr::expand_grid()` to create a
#'   combination of districts, region, months, and years.
#'   \item Random values were generated for \code{malaria_tests},
#'   \code{malaria_cases}, \code{cholera_tests}, and \code{cholera_cases} using
#'   a normal distribution.
#'   \item Outliers were introduced for 5% of the data in \code{cholera_tests},
#'   \code{malaria_cases}, and \code{cholera_cases}.
#'   \item 24% of the data in all 'tests' and 'cases' columns was randomly set
#'   to \code{NA}.
#'   \item The number of 'cases' was adjusted to make sure they are less than
#'   the number of 'tests'.
#'   \item Any negative 'cases' values were converted to their absolute values.
#' }
#'
#' @usage data("fake_epi_df_togo")
#'
#' @examples
#' data('fake_epi_df_togo')
#'
#' head(fake_epi_df_togo)
#'
#'
#' @name fake_epi_df_togo
#' @docType data
#' @keywords datasets
#' @export

# Create the initial data frame
set.seed(42)  # for reproducibility (need this here for tests)
fake_epi_df_togo <-
  # set up state and
  tidyr::expand_grid(
    country = "Togo",
    district = c("Lom\u00E9", "Kaloto", "Zioo", "Vo", "Yotto", "Ke\u00E9ran", "Dankben",
                 "Bas-Mono", "Tchamaba", "Sotouboua", "Centrale", "Cinkaasi",
                 "Bliita", "Ogou", "East-Mono"),
    month = 1:12, year = 2018:2022) |>
  # match regions to districts
  dplyr::mutate(
    region = dplyr::case_when(
      district %in% c("Lom\u00E9", "Zioo", "Vo", "Yotto", "Bas-Mono") ~ "Marieime",
      district %in% c("Kaloto", "Bliita", "Ogou", "East-Mono") ~ "Plateaeux",
      district %in% c("Ke\u00E9ran") ~ "Kara",
      district %in% c("Tchamaba", "Sotouboua", "Centrale") ~ "Centralee"
    )
  ) |>
  dplyr::mutate(
    malaria_tests = abs(rnorm(900, mean = 106, sd = 20)),
    malaria_cases = abs(rnorm(900, mean = 12, sd = 20)),
    cholera_tests = abs(rnorm(900, mean = 116, sd = 35)),
    cholera_cases = abs(rnorm(900, mean = 20, sd = 15))
  ) |>
  # Introduce outliers
  dplyr::rowwise() |>
  dplyr::mutate(
    dplyr::across(
      c(cholera_tests, malaria_cases, cholera_cases),
      ~ {
        na_indices <- sample(1:length(.), size = floor(length(.) * 0.05))
        outlier_indices <- sample(
          setdiff(1:length(.),
                  na_indices),
          size = floor((length(.) - length(na_indices)) * 0.25))
        .[na_indices] <- NA_real_
        .[outlier_indices] <- .[outlier_indices] + 45678
        return(.)
      }
    )) |>dplyr::ungroup() |>
  # # Randomly set 10% of the data in each disease column to NA
  dplyr::mutate(
    dplyr::across(
      c(cholera_tests, malaria_cases, malaria_cases, cholera_cases),
      ~ replace(., sample(1:length(.), size = length(.) * 0.24), NA)
    )) |>
  dplyr::mutate(
    # Make sure the tests are more than the case
    malaria_cases = ifelse(malaria_tests > malaria_cases,
                           malaria_cases -  rnorm(900, 1, 2),
                           abs(malaria_cases)),
    cholera_cases = ifelse(cholera_tests > cholera_cases,
                           cholera_cases -  rnorm(900, 2, 2),
                           abs(cholera_cases)),
    # Make sure the cases are all positive values
    malaria_cases = ifelse(malaria_cases < 0,
                           abs(malaria_cases), malaria_cases),
    cholera_cases = ifelse(cholera_cases < 0,
                           abs(cholera_cases), cholera_cases)
  )  |>
  # Round all values
  dplyr::mutate(
    dplyr::across(
      tidyselect::contains("tests") | tidyselect::contains("cases"), round)) |>
  dplyr::select(country, region,
                district, year, month, tidyselect::everything()) |>
  dplyr::arrange(country, region,
                 district, year, month)

# save data
# save(fake_epi_df_togo, file = "data/fake_epi_df_togo.rda")
