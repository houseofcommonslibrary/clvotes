### Functions for formatting data retrieved from the API

#' Format raw Commons divisions tibble
#'
#' @param df The divisions tibble retrieved from the API
#' @keywords internal

format_commons_all_divisions_raw <- function(df) {

    # Column names
    df <- df %>% janitor::clean_names()
    df <- df %>% dplyr::rename(
        division_date = date,
        division_title = title)

    # As character
    df <- df %>% dplyr::mutate(
        dplyr::across(
            c(division_id,
            number),
        as.character))

    # As date
    df <- df %>% dplyr::mutate(
        dplyr::across(
            c(division_date,
              publication_updated,
              remote_voting_start,
              remote_voting_end),
            as.Date))

    # Tidy and return
    df %>%
        dplyr::arrange(.data$division_date) %>%
        dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
        dplyr::mutate_if(is.character, stringr::str_trim) %>%
        dplyr::mutate_if(is.character, stringr::str_to_lower)

}
