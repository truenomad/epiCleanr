#' Retrieve Administrative Names from GeoNames
#'
#' This function grabs administrative region names (such as districts,
#' provinces, etc.) for a given country from the GeoNames website. It accepts
#' both country names and various country coding schemes.
#'
#' @param country_name_or_code Character or numeric. The name or code of the
#' country for which administrative names are to be retrieved. This can be in
#' various formats such as country name, ISO codes, UN codes, etc., see
#' \code{\link[https://github.com/vincentarelbundock/countrycode]{countrycode::codelist()}} for the full
#' list of codes and naming conventions taken
#'
#' @return A list containing administrative region names and details for
#'         different administrative levels (e.g., ADM1, ADM2, etc.). Each
#'         element of the list corresponds to a different administrative level
#'         and contains a data frame with columns such as country_code,  ascii
#'         name, alternate names, latitude, longitude, and date last updated.
#'
#' @examples
#' \dontrun{
#'   # example using different naming/code conventions
#'   somalia_admins <- get_admin_names("SOM")       # using 3 digit iso codes
#'   kenya_admins <- get_admin_names("KE")          # using 2 digit iso codes
#'   albania_admins <- get_admin_names("KE")        # using UN codes
#'   usa_admis <-  get_admin_names("United States") # using full names
#' }
#'
#' @seealso \url{https://download.geonames.org} for the source of admin
#' names data
#'
#' @importFrom dplyr select filter mutate all_of
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_wider
#' @importFrom utils download.file unzip
#' @importFrom rlang .data
#'
#'
#' @export

get_admin_names <- function(country_name_or_code) {

  # Convert name/code to ISO code to use for districtr name data----------------

  char_coding_schemes <- c(
    'cctld', 'country.name', 'country.name.de', 'country.name.fr',
    'country.name.it', 'genc2c', 'genc3c', 'iso2c', 'iso3c'
  )

  num_coding_schemes <- c(
    'cowc', 'cown', 'dhs', 'ecb', 'eurostat', 'fao', 'fips',
    'gaul', 'genc3n', 'gwc', 'gwn', 'imf', 'ioc', 'iso3n', 'p5n',
    'p5c', 'p4n', 'p4c', 'unicode.symbol', 'unhcr', 'unpd', 'vdem',
    'wb', 'wvs', "un"
  )

  # Choose the appropriate coding schemes based on the type of input
  if (is.character(country_name_or_code)) {
    coding_schemes <- char_coding_schemes
  } else {
    coding_schemes <- c(char_coding_schemes, num_coding_schemes)
  }

  # Convert country name or code to ISO-3166 2-letter country code
  iso_code <- NA
  for (scheme in coding_schemes) {
    iso_code <- suppressWarnings(
      countrycode::countrycode(
        country_name_or_code,
        origin = scheme, destination = "iso2c"))
    if (!is.na(iso_code)) break
  }

  if (is.na(iso_code)) {
    stop("Country name or code not recognized.")
  }

  # retrieve admin names from geonames website ---------------------------------

  # time out after 3 min if not responding
  options(timeout = 180)

  url <- paste0("https://download.geonames.org/export/dump/",
                iso_code, ".zip")
  temp_file <- tempfile(fileext = ".zip")
  download.file(url, destfile = temp_file,  quiet = TRUE)

  temp_dir <- tempdir()

  unzip(temp_file, exdir = temp_dir)

  # Read the data file into a data frame
  data <- epiCleanr::import(paste0(temp_dir, "/", iso_code, ".txt"))

  on.exit(unlink(temp_file)) # Remove the temporary file when done

  # Select required columns
  data <- data |>
    dplyr::select(
      country_code = .data$V9,
      asciiname = .data$V3, alternatenames = .data$V4,
      latitude = .data$V5, longitude = .data$V6,
      feature_code = .data$V8, name = .data$V2,
      admin1_code = .data$V11, admin2_code = .data$V12,
      admin3_code = .data$V13, admin4_code = .data$V14,
      last_updated = .data$V19
    )

  # Manipulate datasets
  state_region_names <- c("ADM1", "ADM1H", "ADM2", "ADM2H", "ADM3",
                          "ADM3H", "ADM4", "ADM4H")

  data <- data |>
    dplyr::filter(.data$feature_code %in% state_region_names) |>
    tidyr::pivot_wider(values_from = "name", names_from = "feature_code") |>
    dplyr::mutate(
      alternatenames = stringr::str_replace_all(
        .data$alternatenames, ",", ", "),
      alternatenames = stringr::str_replace_all(
        .data$alternatenames, " , ", ","),
      alternatenames = stringr::str_replace_all(
        .data$alternatenames, '"', "")
    )

  # Create empty list
  admin_names <- list()

  # Select admin data
  for (i in 1:4) {
    adm_column <- paste0("ADM", i)
    if (adm_column %in% colnames(data)) {
      admin_names[[paste0("adm", i)]] <- data |>
        dplyr::select(
          .data$country_code, .data$asciiname, .data$alternatenames,
          dplyr::all_of(adm_column),
          .data$latitude, .data$longitude, .data$last_updated) |>
        dplyr::filter(!is.na(.data[[adm_column]])) |>
        as.data.frame()
    }
  }

  return(admin_names)

}

