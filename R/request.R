### Functions for requesting API data and processing pagination

# Request ---------------------------------------------------------------------

#' Send a request to API and return the response as a named list
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @keywords internal

request <- function(url, tibble = FALSE) {

    # Get the raw data from the given URL endpoint
    response <- httr::GET(url)

    # Parse response text
    response_text <- httr::content(response, as = "text", encoding = "utf-8")

    # If server returned an error raise it with the response text
    if (response$status_code != 200) stop(request_error(response_text))

    # Return data as tibble or not
    if (tibble == TRUE) {
        response_text %>%
            jsonlite::fromJSON(flatten = TRUE) %>%
            tibble::as_tibble()
    } else {
        response_text %>%
            jsonlite::fromJSON(flatten = TRUE)
    }
}

# Process: Commons ------------------------------------------------------------

#' Processes hidden pagination of results retrieved from the API
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @keywords internal

process_cds_pagination <- function(url) {

    # Get number of divisions
    total_divisions <- request(stringr::str_glue("{API_COMMONS_TOTAL}"))

    # Skip amount
    skip_amount <- seq(0, total_divisions, by = 25)

    # Map pagination
    purrr::map_df(skip_amount, function(amount) {
        request(
            stringr::str_glue("{url}?queryParameters.skip={amount}"),
            tibble = TRUE)
    })
}

#' Process hidden pagination of Member results from the API
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @keyword internal

process_cds_pagination_member <- function(url, member_mnis_id) {

    # Get number of divisions
    total_divisions <- request(stringr::str_glue("{API_COMMONS_TOTAL}"))

    # Skip amount
    skip_amount <- seq(0, total_divisions, by = 25)

    # Map pagination
    purrr::map_df(skip_amount, function(amount) {
        request(
            stringr::str_glue("{url}?queryParameters.memberId={member_mnis_id}&queryParameters.skip={amount}"),
            tibble = TRUE)
    })
}

# Process: Lords --------------------------------------------------------------

#' Processes hidden pagination of results retrieved from the API
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @keywords internal

process_lds_pagination <- function(url) {

    # Get number of divisions
    total_divisions <- request(stringr::str_glue("{API_LORDS_TOTAL}"))

    # Skip amount
    skip_amount <- seq(0, total_divisions, by = 500)

    # Map pagination
    purrr::map_df(skip_amount, function(amount) {
        request(
            stringr::str_glue("{url}?skip={amount}&take=500"),
            tibble = TRUE)
    })
}

#' Process hidden pagination of Member results from the API
#'
#' @param url The full API URL specifying the endpoint and request parameters.
#' @keyword internal

process_lds_pagination_member <- function(url, member_mnis_id) {

    # Get number of divisions
    total_divisions <- request(stringr::str_glue("{API_LORDS_TOTAL}"))

    # Skip amount
    skip_amount <- seq(0, total_divisions, by = 500)

    # Map pagination
    purrr::map_df(skip_amount, function(amount) {
        request(
            stringr::str_glue("{url}?MemberId={member_mnis_id}&skip={amount}&take=500"),
            tibble = TRUE)
    })
}
