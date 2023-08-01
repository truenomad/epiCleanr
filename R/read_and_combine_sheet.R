#' Read an Excel sheet, clean and reshape its data
#'
#' This function reads an Excel sheet, applies cleaning to the data, combines
#' column names, and reshapes the data into a more manageable format. This
#' function makes use of two other functions  in the epiCleanr package:
#' clean_names() and `combine_column_names()`.
#'
#' @param excel_path Character string representing the file path to the Excel
#' file.
#' @param id_cols Character vector representing the column names to be treated
#' as identifiers in the reshaping process.
#' @param sheet Character string representing the name of the Excel sheet to
#' be read.
#' @param rows Integer, the number of rows where the column names are broken.
#'
#' @return A tidied data frame with combined column names and reshaped data.
#'
#' @examples
#' # assuming that you have an Excel file 'data.xlsx' and the sheet 'Sheet1'
#' has column names broken across the first 3 rows
#' read_and_combine_sheet("data.xlsx", c("id", "year"), "Sheet1", 3)
#'
#' @seealso \code{\link[epiCleanr]{clean_names}},
#' \code{\link[epiCleanr]{combine_column_names}}
#' for functions used within this one.
#'
#' @export
read_and_combine_sheet <- function(excel_path, id_cols, sheet, rows) {
  # Import the specified sheet from the Excel file
  sheet_data <- rio::import(excel_path, sheet = sheet)

  # Apply the clean_names function to the column names of the specified rows
  sheet_data[1:rows, ] <- purrr::map(sheet_data[1:rows, ], clean_names)

  # Remove empty rows and columns from the data
  cleaned_data <- janitor::remove_empty(sheet_data, which = c("rows", "cols"))

  # Combine column names using the combine_column_names function
  cleaned_data <- combine_column_names(
    cleaned_data,
    name_row_count = rows,
    retain_original_names = FALSE,
    slide_headers = TRUE,
    separator = "@"
  )

  # Reshape the cleaned data into a longer format, separate combined column
  # headers, and then pivot it back to a wider format
  cleaned_data_longer <- cleaned_data |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "columns_headers",
      values_to = "values"
    ) |>
    tidyr::separate(columns_headers,
      into = c("header1", "header2", "header3"), sep = "@"
    ) |>
    tidyr::pivot_wider(
      id_cols = c(s, province, name_of_district, population, header1, header2),
      names_from = header3,
      values_from = values
    )

  # Return the reshaped data
  return(cleaned_data_longer)
}
