#' Clean variable names
#'
#' This function transforms a variable name into a standard, cleaned format.
#' The transformation includes converting to lower case, replacing spaces and
#' punctuation with underscores, replacing uppercase characters following
#' lowercase characters (camel case) with lowercase characters separated by an
#' underscore, and removing leading or trailing underscores.  The function is
#' inspired by the \code{janitor::clean_names()} function from the janitor
#' package.
#'
#' @param var Character vector representing the variable name(s) to be cleaned.
#'
#' @return The cleaned variable name(s) as a character vector.
#'
#' @examples
#' \donttest{
#' clean_names("My variable NAME")
#' clean_names(c("var1", "Another Variable", "SOME_VAR"))
#'}
#' @seealso \code{\link[https://shorturl.at/mCFQV]{janitor::clean_names()}}
#'
#' @export

clean_names <- function(var) {
  var <- tolower(var)
  var <- gsub("[[:space:]|[:punct:]]+", "_", var)
  var <- gsub("([a-z])([A-Z])", "\\1_\\2", var)
  var <- gsub("^_|_$", "", var)
  return(var)
}
