#' Export Data to Various File Formats
#'
#' This function provides a unified interface for exporting data to various
#' file formats supported by the 'rio' package. The format is automatically
#' detected from the file extension to simplify the exporting process.
#'
#' @param data The dataset to be exported.
#' @param file_path Character string specifying the path to the output file.
#' @param ... Additional arguments to be passed to the underlying write
#'   functions. These arguments are specific to the file format being exported.
#'   Please refer to the documentation of each package used for more
#'   information.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#' # Export a CSV file
#' export(mtcars, "path/to/your/file.csv")
#'
#' # Export an Excel file
#' export(mtcars, "path/to/your/file.xlsx")
#'
#' # Export a Stata DTA file
#' export(mtcars, "path/to/your/file.dta")
#'
#' # Export an RDS file
#' export(mtcars, "path/to/your/file.rds")
#'
#' # Export an RData file
#' export(list(mtcars = mtcars, iris = iris), "path/to/your/file.RData")
#'}
#'
#' @importFrom rio import
#' @importFrom rio install_formats
#' @importFrom tools file_ext
#'
#' @export
export <- function(data, file_path, ...) {

  # Install formats
  withr::with_options(list(repos = "https://cran.rstudio.com"), {
    rio::install_formats()
  })

  # Extract the file extension from the input file path
  file_ext <- tools::file_ext(file_path)

  # List of supported formats
  supported_formats <- c(
    "csv", "tsv", "xlsx", "rds", "RData", "dta"
  )

  if (file_ext %in% supported_formats) {
    rio::export(data, file_path, ...)
  } else {
    stop(
      paste(
        "File format '", file_ext, "' not supported by 'rio'.",
        "Please refer to the package documentation for a full list",
        "of supported formats."
      )
    )
  }
}
