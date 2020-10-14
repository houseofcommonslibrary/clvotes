### Functions for downloading divisions data

# Raw Commons divisions queries -----------------------------------------------

#' Fetch data on all commons divisions available from the API
#'
#' @keywords internal

fetch_commons_all_divisions_raw <- function() {
    data <- process_pagination(stringr::str_glue("{API_GENERAL}search"))
    data <- format_commons_all_divisions_raw(data)
    assign(CACHE_COMMONS_ALL_DIVISIONS_RAW, data, envir = cache)
    data
}

#' Fetch and filter data on all Commons divisions
#'
#' @keywords internal

filter_commons_divisions <- function(
    from_date = NA,
    to_date = NA,
    on_date = NA) {

    # Set from_date and to_date to on_date if set
    if (!is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Check cache
    if (!exists(CACHE_COMMONS_ALL_DIVISIONS_RAW, envir = cache)) {
        divisions <- fetch_commons_all_divisions_raw()
    } else {
        divisions <- get(CACHE_COMMONS_ALL_DIVISIONS_RAW, envir = cache)
    }

    # Filter on dates if requested
    if (!is.na(from_date) || !is.na(to_date)) {
        divisions <- filter_dates(
            divisions,
            start_col = "division_date",
            end_col = "division_date",
            from_date = from_date,
            to_date = to_date)
    }

    # Return
    divisions

}



# Main Commons divisions queries ----------------------------------------------

#' Fetch key details on all Commons divisions
#'
#' \code{get_commons_divisions} fetches data from the Commons Votes API
#' showing key details about each division, with one row per division.
#'
#' The from_date and to_date arguments can be used to filter divisions based
#' on the dates they occurred. The on_date argument is a convenience that sets
#' the from_date and to_date to the same given date. The on_date has priority:
#' if the on_date is set, the from_date and to_date are ignored.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the on_date.
#' @return A tibble of key details for each Commons division, with one row
#' per division.
#' @export

get_commons_divisions <- function(
    to_date = NA,
    from_date = NA,
    on_date = NA) {

    # Fetch / filter raw data
    divisions <- filter_commons_divisions(to_date, from_date, on_date)

    # Return
    divisions %>%
        dplyr::mutate(
            division_evel = ifelse(
                is.na(evel_type),
                FALSE,
                TRUE)) %>%
        dplyr::select(
            .data$division_id,
            .data$division_date,
            .data$division_title,
            .data$division_evel,
            .data$aye_count,
            .data$no_count)
}

#' Fetch key details on all Commons EVEL divisions
#'
#' \code{fetch_commons_divisions_evel} fetches data from the Commons Votes API
#' showing key details about each EVEL division, with one row per EVEL division.
#'
#' The from_date and to_date arguments can be used to filter divisions based
#' on the dates they occurred. The on_date argument is a convenience that sets
#' the from_date and to_date to the same given date. The on_date has priority:
#' if the on_date is set, the from_date and to_date are ignored.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the on_date.
#' @return A tibble of key details for each Commons EVEL division, with one row
#' per EVEL division.
#' @export

get_commons_divisions_evel <- function(
    to_date = NA,
    from_date = NA,
    on_date = NA) {

    # Fetch / filter raw data
    divisions <- filter_commons_divisions(to_date, from_date, on_date)

    # Return
    divisions %>%
        dplyr::filter(
            !is.na(.data$evel_type)) %>%
        dplyr::select(
            .data$division_id,
            .data$division_date,
            .data$division_title,
            .data$evel_type,
            .data$evel_country,
            .data$aye_count,
            .data$no_count,
            .data$double_majority_aye_count,
            .data$double_majority_no_count)
}

#' Fetch key details on all Commons deferred divisions
#'
#' \code{fetch_commons_divisions_deferred} fetches data from the Commons Votes API
#' showing key details about each deferred division, with one row per deferred division.
#'
#' The from_date and to_date arguments can be used to filter divisions based
#' on the dates they occurred. The on_date argument is a convenience that sets
#' the from_date and to_date to the same given date. The on_date has priority:
#' if the on_date is set, the from_date and to_date are ignored.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the on_date.
#' @return A tibble of key details for each Commons deferred division, with one row
#' per deferred division.
#' @export

get_commons_divisions_deferred <- function(
    to_date = NA,
    from_date = NA,
    on_date = NA) {

    # Fetch / filter raw data
    divisions <- filter_commons_divisions(to_date, from_date, on_date)

    # Return
    divisions %>%
        dplyr::filter(
            !is.na(.data$is_deferred)) %>%
        dplyr::filter(
            .data$division_id,
            .data$division_date,
            .data$division_title,
            .data$division_evel,
            .data$aye_count,
            .data$no_count)

}

#' Fetch key details on all Commons divisions tellers
#'
#' \code{fetch_commons_divisions_tellers} fetches data from the Commons Votes API
#' showing key details about each division teller, with one row per division
#' teller.
#'
#' The from_date and to_date arguments can be used to filter divisions based
#' on the dates they occurred. The on_date argument is a convenience that sets
#' the from_date and to_date to the same given date. The on_date has priority:
#' if the on_date is set, the from_date and to_date are ignored.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the on_date.
#' @return A tibble of key details for each Commons deferred division, with one row
#' per deferred division.
#' @export

get_commons_divisions_tellers <- function(
    from_date = NA,
    to_date = NA,
    on_date = NA) {

    # Fetch / filter raw data
    divisions <- filter_commons_divisions(to_date, from_date, on_date)

    # Extract tellers and bind rows
    aye_tellers <- divisions %>%
        tidyr::unnest(.data$aye_tellers) %>%
        janitor::clean_names() %>%
        dplyr::mutate(division_lobby = "Aye") %>%
        dplyr::select(
            .data$division_id,
            .data$division_date,
            .data$division_lobby,
            .data$member_id,
            .data$name,
            .data$party,
            .data$member_from)

    no_tellers <- divisions %>%
        tidyr::unnest(.data$no_tellers) %>%
        janitor::clean_names() %>%
        dplyr::mutate(division_lobby = "No") %>%
        dplyr::select(
            .data$division_id,
            .data$division_date,
            .data$division_lobby,
            .data$member_id,
            .data$name,
            .data$party,
            .data$member_from)

   divisions <- dplyr::bind_rows(aye_tellers, no_tellers)

   # Return
   divisions %>%
       dplyr::rename(
           member_mnis_id = member_id,
           member_name = name,
           member_party = party,
           member_constituency = member_from)
}


