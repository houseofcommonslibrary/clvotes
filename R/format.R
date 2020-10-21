### Functions for formatting data retrieved from the API

# Commons ---------------------------------------------------------------------

#' Format raw Commons divisions tibble
#'
#' @param df The divisions tibble retrieved from the API
#' @keywords internal

format_cds_raw <- function(df) {

    # Column names
    df <- df %>% janitor::clean_names()
    df <- df %>% dplyr::rename(
        division_date = date,
        division_title = title)

    # As character
    df <- df %>% dplyr::mutate(
        dplyr::across(
            division_id,
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
        dplyr::arrange(
            .data$division_date,
            .data$division_id) %>%
        dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
        dplyr::mutate_if(is.character, stringr::str_trim) %>%
        dplyr::mutate_if(is.character, stringr::str_squish)

}

#' Format raw Commons divisions votes tibble
#'
#' @param df The divisions vote tibble retrieved from the API
#' @keywords internal

format_cds_votes_raw <- function(df) {

    # Column names
    df <- df %>% janitor::clean_names()

    # As character
    df <- df %>% dplyr::mutate(
        dplyr::across(
            c(division_id,
              member_id),
            as.character))

    # As date
    df <- df %>% dplyr::mutate(
        dplyr::across(
            division_date,
            as.Date))

    # Tidy and return
    df %>%
        dplyr::arrange(
            .data$division_date,
            .data$division_id) %>%
        dplyr::rename(
            mnis_id = .data$member_id) %>%
        dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
        dplyr::mutate_if(is.character, stringr::str_trim) %>%
        dplyr::mutate_if(is.character, stringr::str_squish)
}

#' Format raw Commons divisions members tibble
#'
#' @param df The divisions member tibble retrieved from the API
#' @keywords internal

format_cds_members_raw <- function(df) {

    # Column names
    df <- df %>% janitor::clean_names()

    # As character
    df <- df %>% dplyr::mutate(
        dplyr::across(
            c(published_division_division_id,
              member_id),
            as.character))

    # As date
    df <- df %>% dplyr::mutate(
        dplyr::across(
            published_division_date,
            as.Date))

    # Tidy and return
    df %>%
        dplyr::arrange(
            .data$member_id,
            .data$published_division_date,
            .data$published_division_division_id) %>%
        dplyr::rename(
            mnis_id = .data$member_id,
            division_id = .data$published_division_division_id,
            division_date = .data$published_division_date,
            division_title = .data$published_division_title) %>%
        dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
        dplyr::mutate_if(is.character, stringr::str_trim) %>%
        dplyr::mutate_if(is.character, stringr::str_squish)

}

#  Lords ----------------------------------------------------------------------

#' Format raw Lords divisions tibble
#'
#' @param df The divisions tibble retrieved from the API
#' @keywords internal

format_lds_raw <- function(df) {

    # Column names
    df <- df %>% janitor::clean_names()
    df <- df %>% dplyr::rename(
        division_date = date,
        division_title = title)

    # As character
    df <- df %>% dplyr::mutate(
        dplyr::across(
            division_id,
            as.character))

    # As date
    df <- df %>% dplyr::mutate(
        dplyr::across(
            c(division_date,
              remote_voting_start,
              remote_voting_end),
            as.Date))

    # Tidy and return
    df %>%
        dplyr::arrange(
            .data$division_date,
            .data$division_id) %>%
        dplyr::rename(
            content_count = .data$teller_content_count,
            not_content_count = .data$teller_not_content_count) %>%
        dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
        dplyr::mutate_if(is.character, stringr::str_trim) %>%
        dplyr::mutate_if(is.character, stringr::str_squish)

}

#' Format raw Lords divisions votes tibble
#'
#' @param df The divisions vote tibble retrieved from the API
#' @keywords internal

format_lds_votes_raw <- function(df) {

    # Column names
    df <- df %>% janitor::clean_names()

    # As character
    df <- df %>% dplyr::mutate(
        dplyr::across(
            c(division_id,
              member_id),
            as.character))

    # As date
    df <- df %>% dplyr::mutate(
        dplyr::across(
            division_date,
            as.Date))

    # Tidy and return
    df %>%
        dplyr::arrange(
            .data$division_date,
            .data$division_id) %>%
        dplyr::rename(
            mnis_id = .data$member_id) %>%
        dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
        dplyr::mutate_if(is.character, stringr::str_trim) %>%
        dplyr::mutate_if(is.character, stringr::str_squish)
}

#' Format raw Lords divisions members tibble
#'
#' @param df The divisions member tibble retrieved from the API
#' @keywords internal

format_lds_members_raw <- function(df) {

    # Column names
    df <- df %>% janitor::clean_names()

    # As character
    df <- df %>% dplyr::mutate(
        dplyr::across(
            c(published_division_division_id,
              member_id),
            as.character))

    # As date
    df <- df %>% dplyr::mutate(
        dplyr::across(
            published_division_date,
            as.Date))

    # Tidy and return
    df %>%
        dplyr::arrange(
            .data$member_id,
            .data$published_division_date,
            .data$published_division_division_id) %>%
        dplyr::rename(
            mnis_id = .data$member_id,
            division_id = .data$published_division_division_id,
            division_date = .data$published_division_date,
            division_title = .data$published_division_title) %>%
        dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
        dplyr::mutate_if(is.character, stringr::str_trim) %>%
        dplyr::mutate_if(is.character, stringr::str_squish)

}
