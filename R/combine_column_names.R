#' Combine column names in a data frame
#'
#' This function combines column names in a data frame where the names are
#' broken across multiple rows. It reshapes the data and joins the names,
#' which allows for better organization and cleaner data. This function was
#' developed to tackle the challenges encountered when working with large Excel
#' files that contain many spanning headers. It is designed to be used in
#' conjunction with other functions in the epiCleanr package, like
#' `read_and_combine_sheet()`, but can also be used on it's own.
#'
#' @param df A data frame whose column names need to be combined.
#' @param name_row_count Integer, the number of rows in which column names
#' are broken.
#' @param retain_original_names Logical, if TRUE, retains the original names in
#' the combined name.
#' @param slide_headers Logical, if TRUE, slides the headers down to fill NAs.
#' @param separator Character, the character(s) to use when joining names.
#'
#' @return A data frame with combined column names.
#'
#' @examples
#' combine_column_names(df,
#'   name_row_count = 2, retain_original_names = TRUE,
#'   slide_headers = TRUE, separator = "_"
#' )
#'
#' @seealso \code{\link[epiCleanr]{read_and_combine_sheet}} for a function
#' that uses this one.
#'
#' @export
combine_column_names <- function(df, name_row_count,
                                 retain_original_names = TRUE,
                                 slide_headers = FALSE, separator = "_") {
  # Stop execution if name_row_count is 0, which means column names are not
  # split across rows.
  if (name_row_count == 0) {
    stop("If name_row_count is 0, column names aren't split")
  }

  # Reshape the data frame to long format to create a single row of combined
  # column names.
  reshaped_names_df <- df |>
    dplyr::slice(seq_len(name_row_count)) |>
    dplyr::mutate(
      row_id = 1:name_row_count,
      across(everything(), as.character)
    ) |>
    tidyr::pivot_longer(
      cols = -row_id, names_to = "original_columns",
      values_to = "values"
    ) |>
    dplyr::mutate(
      values = dplyr::na_if(values, ""),
      column_position = rep(
        seq_len(dplyr::n_distinct(original_columns)),
        dplyr::n() / dplyr::n_distinct(original_columns)
      )
    )

  # Slide headers downwards if the slide_headers option is TRUE.
  if (slide_headers) {
    reshaped_names_df <- reshaped_names_df |>
      dplyr::group_by(row_id) |>
      tidyr::fill(values, .direction = "down") |>
      dplyr::ungroup()
  }

  # Combine the column names and clean them up.
  combined_names_df <- reshaped_names_df |>
    dplyr::group_by(column_position) |>
    dplyr::summarize(values = paste0(values, collapse = separator),
                     .groups = "keep") |>
    dplyr::mutate(
      values = ifelse(retain_original_names,
                      paste(base::names(df), values, sep = separator), values
      ),
      values = base::gsub(paste0(NA, separator, "|", separator, NA), "",
                          values)
    )

  # Pivot the combined names to wide format.
  wide_combined_names_df <- combined_names_df |>
    tidyr::pivot_wider(names_from = column_position, values_from = values)

  # Replace the original column names in the data frame with the combined names.
  df <- df |>
    dplyr::slice(-(1:name_row_count)) |>
    setNames(as.character(wide_combined_names_df[1, ]))

  # Warn the user if any of the new column names are NA.
  if (any(base::grepl("^NA$", base::names(df)))) {
    warning("Potential NA in column names; verify the name_row_count argument.")
  }

  # Return the data frame with the new column names.
  return(df)
}
