#' Togo Administrative Regions and Districts Dataset
#'
#' This dataset contains administrative level 1 and level 2 information for
#' Togo. It is intended for testing the functionalities of the epiCleanr package,
#' particularly those that relate to geographical subdivisions.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{region}{The administrative level 1 division in Togo, also known as
#'   regions.}
#'   \item{district}{The administrative level 2 division in Togo, also known
#'   as districts.}
#' }
#'
#' @usage data("togo_admin_df")
#'
#' @examples
#' data("togo_admin_df")
#'
#' head(togo_admin_df)
#'
#'
#' @name togo_admin_df
#' @docType data
#' @keywords datasets
#' @export togo_admin_df

togo_admin_df <-
  data.frame(
    district = c("Lom\u00E9", "Kpalim\u00E9", "Zio", "Vo", "Yoto", "K\u00E9ran", "Dankpen",
                 "Bas-Mono", "Tchamba", "Sotouboua", "Centrale", "Cinkass\u00E9",
                 "Blitta", "Ogou", "Est-Mono")
  ) |>
  dplyr::mutate(
    region = dplyr::case_when(
      district %in% c("Lom\u00E9", "Zio", "Vo", "Yoto", "Bas-Mono") ~ "Maritime",
      district %in% c("Kpalim\u00E9", "Blitta", "Ogou", "Est-Mono") ~ "Plateaux",
      district %in% c("K\u00E9ran") ~ "Kara",
      district %in% c("Tchamba", "Sotouboua", "Centrale") ~ "Centrale",
      district %in% c("Dankpen", "Cinkass\u00E9") ~ "Savanes"
    )
  )

# save data
save(togo_admin_df, file = "data/togo_admin_df.rda")
