#' Plot Missing data over time
#'
#' This function visualizes the proportion of missing data or reporting rate for
#'  specified variables in a dataset. It creates a tile plot using ggplot2 where
#'  the x-axis represents time (e.g., year, month), and the y-axis represents
#'  either variables or groupings (e.g., state). The output can further be
#'  manipulated to one's needs.
#'
#' @param data A data frame containing the data to be visualized. Must include
#' columns specified in 'time_var', 'group_var', and 'vars'.
#' @param time_var A character string specifying the time variable in 'data'
#' (e.g., "year", "month"). Must be provided.
#' @param group_var An optional character string specifying the grouping
#' variable in 'data' (e.g., "state"). If provided, only one variable can be
#' specified in 'vars'.
#' @param vars An optional character vector specifying the variables to be
#' visualized in 'data'. If NULL, all variables except 'time_var' and
#' 'group_var' will be used.
#' @param use_rep_rate A logical value. If TRUE, the reporting rate is
#' visualized; otherwise, the proportion of missing data is visualized.
#' Defaults to TRUE.
#' @return A ggplot2 object representing the tile plot.
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs theme_bw
#' theme scale_x_discrete scale_y_discrete guides guide_legend unit
#' @importFrom dplyr select mutate across group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with all_of
#' @importFrom stringr str_remove
#' @importFrom tools toTitleCase
#' @importFrom rlang sym
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a data frame with all combinations of states and years
#' fake_data <- expand_grid(
#'   state = state.name,
#'   month = 1:12, year = 2000:2023
#' ) |>
#'   mutate(
#'     measles = sample(0:1000, size = n(), replace = TRUE),
#'     polio = sample(0:500, size = n(), replace = TRUE),
#'     malaria = sample(0:300, size = n(), replace = TRUE),
#'     cholera = sample(0:700, size = n(), replace = TRUE)
#'   ) |>
#'   # Randomly set 10% of the data in each disease column to NA
#'   mutate(across(
#'     c(measles, polio, malaria, cholera),
#'     ~ replace(., sample(1:n(), size = n() * 0.4), NA)
#'   ))
#'
#' # Example function usage
#' plot_missing_data(fake_data,
#'   time_var = "year",
#'   vars = "polio",
#'   group_var = "state",
#'   use_rep_rate = T
#' )
#' }
plot_missing_rates <- function(data, time_var, group_var = NULL,
                               vars = NULL, use_rep_rate = TRUE) {

  # Check if 'time_var' is provided and exists in the data
  if (is.null(time_var) || !time_var %in% names(data)) {
    stop("A valid 'time_var' must be provided and must exist in the data.")
  }

  # If 'group_var' is provided, ensure only one variable is specified in 'vars'
  if (!is.null(group_var) && length(vars) != 1) {
    stop(paste(
      "When 'group_var' is provided",
      "only one variable can be specified in 'vars'."
    ))
  }

  # Determine the fill variable and label based on 'use_rep_rate'
  fill_var <- ifelse(use_rep_rate, "rep_rate", "propmiss")
  fill_label <- ifelse(use_rep_rate, "Reporting rate (%)", "Missing rate (%)")

  # Determine y-axis label based on 'group_var'
  y_axis_label <- if (!is.null(group_var)) {
    tools::toTitleCase(group_var)
  } else {
    "Variable"
  }

  # Construct the title based on the provided parameters
  title_prefix <- if (use_rep_rate) {
    "Reporting rate of"
  } else {
    "The proportion of missing data for"
  }

  max_vars_in_title <- 5 # Set a threshold

  title_vars <- if (!is.null(vars)) {
    paste(paste(vars, collapse = ", "), "by", time_var)
  } else {
    remaining_vars <- setdiff(names(data), c(time_var, group_var))
    if (length(remaining_vars) <= max_vars_in_title) {
      paste(
        paste(remaining_vars[-length(remaining_vars)], collapse = ", "),
        "and",
        remaining_vars[length(remaining_vars)],
        "by",
        time_var
      )
    } else {
      paste("various variables by", time_var)
    }
  }

  # If 'vars' is not provided, use all variables
  # except 'time_var' and 'group_var'
  if (is.null(vars)) {
    vars <- setdiff(names(data), c(time_var, group_var))
  }

  title_suffix <- if (!is.null(group_var)) paste("and", group_var) else ""

  # Select relevant columns and mutate missing values
  plot_data <- data |>
    dplyr::select(all_of(c(time_var, group_var, vars))) |>
    dplyr::mutate(dplyr::across(
      .cols = -c(
        !!sym(time_var),
        if (!is.null(group_var)) all_of(group_var) else NULL
      ),
      .fns = ~ ifelse(is.na(.), 1, 0),
      .names = "miss_{.col}"
    ))

  # Pivot the missing value columns to long format
  plot_data <- plot_data |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("miss_"),
      names_to = "variable",
      values_to = "miss_value"
    ) |>
    dplyr::mutate(variable = stringr::str_remove(variable, "miss_"))

  # If 'group_var' is not NULL, group by 'time_var', 'group_var', and 'variable'
  if (!is.null(group_var)) {
    plot_data <- plot_data |>
      dplyr::group_by(!!sym(time_var), !!sym(group_var), variable)
  } else {
    plot_data <- plot_data |>
      dplyr::group_by(!!sym(time_var), variable)
  }

  # Summarize missing data
  plot_data <- plot_data |>
    dplyr::summarise(
      miss = sum(miss_value, na.rm = TRUE),
      tot = dplyr::n()
    ) |>
    dplyr::mutate(
      propmiss = miss / tot * 100,
      rep_rate = 100 - propmiss
    )

  # Determine the y-axis variable based on the presence of 'group_var'
  if (!is.null(group_var)) {
    y_axis_var <- as.name(group_var)
  } else {
    y_axis_var <- "variable"
  }

  # Plot the data using ggplot2
  ggplot2::ggplot(plot_data,
                  aes(x = as.factor(!!sym(time_var)),
                      y = !!as.name(y_axis_var), fill = !!sym(fill_var))) +
    ggplot2::geom_tile(colour = "white", linewidth = .2) +
    ggplot2::scale_fill_viridis_c(option = "E") +
    ggplot2::labs(
      title = paste(title_prefix, title_vars, title_suffix),
      x = "", y = y_axis_label, fill = fill_label
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        size = 12, face = "bold",
        family = "Arial"
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 5, unit = "pt")
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10, unit = "pt")
      ),
      axis.text.x = ggplot2::element_text(angle = 75, hjust = 1),
      legend.margin = ggplot2::margin(t = 0, unit = "cm"),
      legend.text = ggplot2::element_text(size = 8, family = "Arial"),
      plot.title = ggplot2::element_text(
        size = 12, face = "bold",
        family = "Arial",
        margin = ggplot2::margin(t = 20)
      ),
      axis.text = ggplot2::element_text(family = "Arial"),
      axis.title = ggplot2::element_text(family = "Arial"),
      strip.text = ggplot2::element_text(family = "Arial"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "top", nrow = 1,
      label.position = "bottom", direction = "horizontal",
      key.height = ggplot2::unit(1, "lines"),
      key.width = ggplot2::unit(1, "lines")
    ))
}

