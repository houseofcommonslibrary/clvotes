### Request functions for fetching and processing data

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




