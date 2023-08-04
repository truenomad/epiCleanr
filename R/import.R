#' Import Data from Various File Formats
#'
#' This function provides a unified interface for importing data from various
#' file formats supported by the 'rio' package. The format is automatically
#' detected from the file extension to simplify the importing process.
#'
#' @param file_path Character string specifying the path to the input file or
#'   a URL pointing to the dataset.
#' @param ... Additional arguments to be passed to the underlying read
#'   functions. These arguments are specific to the file format being imported.
#'   Please refer to the documentation of each package used for more
#'   information.
#'
#' @return A data frame or appropriate R object containing the imported data.
#'
#' @examples
#' \dontrun{
#' # Import a CSV file
#' data_csv <- import("path/to/your/file.csv")
#'
#' # Import an Excel file
#' data_excel <- import("path/to/your/file.xlsx")
#'
#' # Import a Stata DTA file
#' data_dta <- import("path/to/your/file.dta")
#'
#' # Import an RDS file
#' data_rds <- import("path/to/your/file.rds")
#'
#' # Import an RData file
#' data_rdata <- import("path/to/your/file.RData")
#'
#' # Import a JSON file
#' data_json <- import("path/to/your/file.json")
#'
#' # Import an SPSS file
#' data_spss <- import("path/to/your/file.sav")
#'
#' # Import a data.table file
#' data_dt <- import("path/to/your/file.dt")
#'
#' # Import a dataset from a website (e.g., GitHub)
#' data_web <- import("https://github.com/your_username/your_dataset.xlsx")
#'}
#'
#' @importFrom rio import
#' @importFrom rio install_formats
#' @importFrom tools file_ext
#'
#' @export
import <- function(file_path, ...) {
  # Check if the input file path is a URL and if so, read from URL directly
  if (grepl("^http[s]?://", file_path, ignore.case = TRUE)) {
    return(rio::import(file_path, ...))
  }

  # Extract the file extension from the input file path
  file_ext <- tools::file_ext(file_path)

  # List of supported formats
  supported_formats <- c(
    "csv", "tsv", "txt", "csvy", "sas7bdat", "sav",
    "dta", "xpt", "xlsx", "RData", "rds", "tsv"
  )

  if (file_ext %in% supported_formats) {
    return(rio::import(file_path, ...))
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


#' @importFrom rio install_formats
.onLoad <- function(libname, pkgname) {
  rio::install_formats()
}
