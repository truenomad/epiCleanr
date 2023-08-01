library(readxl)
library(janitor)
library(purrr)
library(dplyr)


# Main function
read_and_combine_sheets <- function(excel_path, rows) {

  clean_names_top_rows <- function(data, rows) {

    clean_names <- function(var) {
      var <- tolower(var)
      var <- gsub("[[:space:]|[:punct:]]+", "_", var)
      var <- gsub("([a-z])([A-Z])", "\\1_\\2", var)
      var <- gsub("^_|_$", "", var)
      return(var)
    }

    # Apply clean_names function to specified rows' column names
    data[1:rows, ] <- map(data[1:rows, ], clean_names)
    return(data)
  }

  # Get the sheet names
  sheet_names <- readxl::excel_sheets(path)

  # Load and process each sheet separately
  processed_sheets <- map(sheet_names, ~ {
    sheet_data <- readxl::read_excel(excel_path, sheet = .x)
    cleaned_data <- sheet_data |>
      janitor::clean_names() |>
      # clean_names_top_rows(rows = rows) |>
      janitor::remove_empty(c("rows", "cols")) |>
      combine_column_names(name_row_count = rows,
                           retain_original_names = F, slide_headers = T, separator = "@")

    # Add sheet name as a new column
    cleaned_data$sheet_name <- .x
    cleaned_data
  })

  # Check if all sheets have the same columns (excluding the sheet_name column)
  all_same_cols <- all(map_lgl(processed_sheets[-1], ~ janitor::compare_df_cols_same(processed_sheets[[1]][, -ncol(processed_sheets[[1]])], .x[, -ncol(.x)])))

  if (all_same_cols) {
    # Combine all sheets into a single data frame
    combined_data <- bind_rows(processed_sheets)
  } else {
    stop("The sheets have different columns.")
  }

  return(combined_data)
}


path = "/Users/mohamedyusuf/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/Consaltuncies/WHO/GMP_SNT/PAK_SNT/SNT_PAK/2023_SNT/Raw_data/Data 2022 for WHO.xlsx"
# Usage
combined_data <- read_and_combine_sheets(path)
