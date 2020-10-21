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
