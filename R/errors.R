### Functions for generating error messages

#' Report an error with a http response
#'
#' @param response The text of the server response
#' @keywords internal

request_error <- function(response) {
    stringr::str_glue(
        "The server responded with the following message: {response}")
}

#' Report an error handling dataframes with missing columns
#'
#' @param colname The name of the column that could not be found.
#' @keywords internal

missing_column_error <- function(colname) {
    stringr::str_glue("Could not find a column called {colname}")
}

#' Report a missing division_id argument
#'
#' @param argument The name of the missing argument.
#' @keywords internal

missing_argument <- function(argument) {
    stringr::str_glue("A {argument} has not been supplied.")
}

#' Report invalid mnis id
#'
#' @keywords internal

invalid_mnis_id <- function(member_mnis_id) {
    stringr::str_glue("The member_mnis_id provided is not associated with ",
    "any Members.")
}

#' Report an error parsing a date string
#'
#' @param date_str The date string that could not be parsed.
#' @keywords internal

date_format_error <- function(date_str) {
    stringr::str_glue(stringr::str_c(
        "{date_str} is not a valid Date or ",
        "date string: use format \"YYYY-MM-DD\""))
}
