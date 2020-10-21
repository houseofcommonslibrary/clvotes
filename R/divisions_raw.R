### Functions for downloading Commons and Lords raw divisions data

#' Fetch key details on all Commons divisions in raw format
#'
#' \code{fetch_commons_divisions_all_raw} fetches a dataframe from the Commons Votes API
#' showing key details about each division, with one row per division in raw
#' format.
#'
#' JSON returned in the response from the API is converted into a Tibble with
#' column names cleaned. No sub-selecting of columns and variable names occurs.
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
#' per division in raw format.
#' @export

fetch_commons_divisions_all_raw <- function(
    from_date = NA,
    to_date = NA,
    on_date = NA) {

    # Fetch / filter raw data
    divisions <- fetch_cds_raw_filter(
        from_date,
        to_date,
        on_date)

    # Return
    divisions
}

#' Fetch key details on all Lords divisions in raw format
#'
#' \code{fetch_lords_divisions_all_raw} fetches a dataframe from the Lords Votes API
#' showing key details about each division, with one row per division in raw
#' format.
#'
#' JSON returned in the response from the API is converted into a Tibble with
#' column names cleaned. No sub-selecting of columns and variable names occurs.
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
#' @return A tibble of key details for each Lords division, with one row
#' per division in raw format.
#' @export

fetch_lords_divisions_all_raw <- function(
    from_date = NA,
    to_date = NA,
    on_date = NA) {

    # Fetch / filter raw data
    divisions <- fetch_lds_raw_filter(
        from_date,
        to_date,
        on_date)

    # Return
    divisions
}
