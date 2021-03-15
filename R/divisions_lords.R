### Functions for downloading Lords divisions data

# Raw Lords divisions queries -----------------------------------------------

#' Fetch data on all Lords divisions available from the API
#'
#' @keywords internal

fetch_lds_raw <- function() {
    divisions <- process_lds_pagination(stringr::str_glue("{API_LORDS_GENERAL}search"))
    divisions <- format_lds_raw(divisions)
    assign(CACHE_LDS_RAW, divisions, envir = cache)
    divisions
}

#' Fetch data on individual Lords divisions available from the API
#'
#' @keywords internal

fetch_lds_votes_raw <- function(division_id) {

    divisions <- purrr::map_df(division_id, function(id) {
        division <- request(stringr::str_glue("{API_LORDS_SINGLE}{id}"))
        tibble::tibble(
            division_id = division$divisionId,
            division_date = division$date,
            division_title = division$title,
            content_votes = list(division$contents),
            not_content_votes = list(division$notContents))
    })

    # Return
    divisions
}

#' Fetch data on Lords voting for divisions available from the API
#'
#' @keywords internal

fetch_lds_members_raw <- function(
    member_mnis_id,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL) {

    member_ids <- member_mnis_id
    divisions <- purrr::map_df(member_ids, function(id) {
        divisions <- process_lds_pagination_member(
            stringr::str_glue("{API_LORDS_GENERAL}membervoting"),
            id)
    })

    if (nrow(divisions) == 0 & ncol(divisions) == 0) {
        return(divisions)
    }

    divisions <- format_lds_members_raw(divisions)

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {
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

#' Filter data on all Lords divisions
#'
#' @keywords internal

fetch_lds_raw_filter <- function(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL) {

    # Set from_date and to_date to on_date if set
    if (!is.null(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Check cache
    if (!exists(CACHE_LDS_RAW, envir = cache)) {
        divisions <- fetch_lds_raw()
    } else {
        divisions <- get(CACHE_LDS_RAW, envir = cache)
    }

    # Filter on dates if requested
    if (!is.null(from_date) || !is.null(to_date)) {
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

# Main Lords divisions queries ----------------------------------------------

#' Fetch key details on all Lords divisions
#'
#' \code{fetch_lords_divisions_all} fetches a dataframe from the Lords Votes
#' API showing key details about each division, with one row per division.
#'
#' The from_date and to_date arguments can be used to filter divisions based
#' on the dates they occurred. The on_date argument is a convenience that sets
#' the from_date and to_date to the same given date. The on_date has priority:
#' if the on_date is set, the from_date and to_date are ignored.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the on_date.
#' @return A tibble of key details for each Lords division, with one row
#' per division.
#' @export

fetch_lords_divisions_all <- function(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL) {

    # Fetch / filter raw data
    divisions <- fetch_lds_raw_filter(
        from_date,
        to_date,
        on_date)

    # Return
    divisions %>%
        dplyr::select(
            .data$division_id,
            .data$division_date,
            .data$division_title,
            .data$content_count,
            .data$not_content_count)
}

#' Fetch key details on all Lords divisions tellers
#'
#' \code{fetch_lords_divisions_tellers} fetches a dataframe from the Lords
#' Votes API showing key details about each division teller, with one row per
#' division teller.
#'
#' The from_date and to_date arguments can be used to filter divisions based
#' on the dates they occurred. The on_date argument is a convenience that sets
#' the from_date and to_date to the same given date. The on_date has priority:
#' if the on_date is set, the from_date and to_date are ignored.
#'
#' Divisions with zero tellers are ignored.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the on_date.
#' @return A tibble of key details of tellers for each Lords division, with one row
#' per teller.
#' @export

fetch_lords_divisions_tellers <- function(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL) {

    # Fetch / filter raw data
    divisions <- fetch_lds_raw_filter(
        from_date,
        to_date,
        on_date)

    divisions <- tryCatch({

        # Extract tellers and bind rows
        content_tellers <- divisions %>%
            tidyr::unnest(.data$content_tellers) %>%
            dplyr::mutate(division_lobby = "Content") %>%
            janitor::clean_names() %>%
            dplyr::select(
                .data$division_id,
                .data$division_date,
                .data$division_title,
                .data$division_lobby,
                .data$member_id,
                .data$name,
                .data$party)

        not_content_tellers <- divisions %>%
            tidyr::unnest(.data$not_content_tellers) %>%
            dplyr::mutate(
                division_lobby = "Not content") %>%
            janitor::clean_names() %>%
            dplyr::select(
                .data$division_id,
                .data$division_date,
                .data$division_title,
                .data$division_lobby,
                .data$member_id,
                .data$name,
                .data$party)

        # Bind
        divisions <- dplyr::bind_rows(content_tellers, not_content_tellers)
        divisions <- divisions %>%
            dplyr::rename(
                mnis_id = member_id,
                member_name = name,
                member_party = party)
        divisions$mnis_id <- as.character(divisions$mnis_id)

        # Return
        divisions %>%
            dplyr::select(
                .data$division_id,
                .data$division_date,
                .data$division_title,
                .data$division_lobby,
                .data$mnis_id,
                .data$member_name,
                .data$member_party)
        },
        error = function(c) {
            tibble::tibble(
                division_id = as.character(NA),
                division_date = as.Date(NA),
                division_title = as.character(NA),
                division_lobby = as.character(NA),
                mnis_id = as.character(NA),
                member_name = as.character(NA),
                member_party = as.character(NA))
        }
    )
    # Return
    divisions
}

#' Fetch key details on all Lords remote divisions
#'
#' \code{fetch_lords_divisions_remote} fetches a dataframe from the Lords
#' Votes API showing key details about each remote division, with one row per
#' remote division.
#'
#' The from_date and to_date arguments can be used to filter divisions based
#' on the dates they occurred. The on_date argument is a convenience that sets
#' the from_date and to_date to the same given date. The on_date has priority:
#' if the on_date is set, the from_date and to_date are ignored.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the on_date.
#' @return A tibble of key details of remote Lords divisions, with one row
#' per remote division.
#' @export

fetch_lords_divisions_remote <- function(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL) {

    # Fetch / filter raw data
    divisions <- fetch_lds_raw_filter(
        from_date,
        to_date,
        on_date)

    # Return
    divisions %>%
        dplyr::filter(
            !is.na(.data$remote_voting_start)) %>%
        dplyr::select(
            .data$division_id,
            .data$division_date,
            .data$division_title,
            .data$content_count,
            .data$not_content_count)
}

#' Fetch key details on Lords divisions votes
#'
#' \code{fetch_lords_divisions_votes} fetches data from the Lords Votes API
#' showing key details about each division vote, with one row per division
#' vote. All votes or abstentions are returned.
#'
#' @param division_id An integer or vector of integers representing a division
#' ID.
#' @return A tibble of key details of voting for a Lords division , with one row
#' per vote.
#' @export

fetch_lords_divisions_votes <- function(division_id) {

    # Check division ID provided
    if (is.null(division_id)) stop(missing_argument("division_id"))

    # Fetch
    divisions <- fetch_lds_votes_raw(division_id)

    divisions <- tryCatch({

        # Extract content votes
        content_votes <- divisions %>%
            tidyr::unnest(.data$content_votes) %>%
            format_lds_votes_raw() %>%
            dplyr::mutate(vote_direction = "Content") %>%
            dplyr::select(
                .data$division_id,
                .data$division_date,
                .data$division_title,
                .data$vote_direction,
                .data$mnis_id,
                .data$name,
                .data$party)

        # Extract not content votes
        not_content_votes <- divisions %>%
            tidyr::unnest(.data$not_content_votes) %>%
            format_lds_votes_raw() %>%
            dplyr::mutate(vote_direction = "Not content") %>%
            dplyr::select(
                .data$division_id,
                .data$division_date,
                .data$division_title,
                .data$vote_direction,
                .data$mnis_id,
                .data$name,
                .data$party)
        # Bind
        divisions <- dplyr::bind_rows(content_votes, not_content_votes)

        # Return
        divisions %>%
            dplyr::rename(
                mnis_id = mnis_id,
                member_name = name,
                member_party = party) %>%
            dplyr::select(
                .data$division_id,
                .data$division_date,
                .data$division_title,
                .data$vote_direction,
                .data$mnis_id,
                .data$member_name,
                .data$member_party)
        },
        error = function(c) {
            tibble::tibble(
                division_id = as.character(NA),
                division_date = as.Date(NA),
                division_title = as.character(NA),
                vote_direction = as.character(NA),
                mnis_id = as.character(NA),
                member_name = as.character(NA),
                member_party = as.character(NA))
        }
    )

    # Return
    divisions
}

#' Fetch key details on Lords divisions votes grouped by party
#'
#' \code{fetch_lords_divisions_party} fetches data from the Lords Votes API
#' showing key details about each division vote grouped by party, with one row
#' per division grouped vote.
#'
#' @param division_id An integer or vector of integers representing a division
#' ID.
#' @return A tibble of key details of voting grouped by party for a Lords
#' division, with one row per grouped vote.
#' @export

fetch_lords_divisions_party <- function(division_id) {

    # Check division ID provided
    if (is.null(division_id)) stop(missing_argument("division_id"))

    # Fetch
    divisions <- fetch_lords_divisions_votes(division_id)

    # Return
    divisions %>%
        dplyr::group_by(
            division_id,
            division_date,
            division_title,
            member_party) %>%
        dplyr::count(vote_direction) %>%
        dplyr::rename(vote_count = n) %>%
        dplyr::arrange(
            division_id,
            vote_direction,
            member_party)
}

#' Fetch key details Lords voting record for divisions
#'
#' \code{fetch_lords_divisions_members} fetches data from the Lords Votes API
#' showing key details about the voting record for a Lord for each division,
#' with one row per division. Only whether a Lord voted "Content" in a division
#' is returned.
#'
#' The from_date and to_date arguments can be used to filter divisions based
#' on the dates they occurred. The on_date argument is a convenience that sets
#' the from_date and to_date to the same given date. The on_date has priority:
#' if the on_date is set, the from_date and to_date are ignored.
#'
#' @param member_mnis_id An integer or vector of integers representing a member
#'   mnis ID.
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NULL, which means no records are excluded on the basis of
#'   the on_date.
#' @return A tibble of key details for a Lords voting record, with one row
#' per division.
#' @export

fetch_lords_divisions_members <- function(
    member_mnis_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL) {

    # Check member mnis ID provided
    if (is.null(member_mnis_id)) stop(missing_argument("member_mnis_id"))

    # Fetch
    divisions <- fetch_lds_members_raw(
        member_mnis_id,
        from_date,
        to_date,
        on_date)

    if (nrow(divisions) == 0 & ncol(divisions) == 0) stop(invalid_mnis_id(member_mnis_id))

    divisions <- tryCatch({

        # Return
        divisions %>%
            dplyr::select(
                .data$division_id,
                .data$division_date,
                .data$division_title,
                .data$mnis_id,
                .data$member_was_content,
                .data$member_was_teller)
        },
        error = function(c) {
            tibble::tibble(
                division_id = as.character(NA),
                division_date = as.Date(NA),
                division_title = as.character(NA),
                mnis_id = as.character(NA),
                member_was_content = as.character(NA),
                member_was_teller = as.character(NA))
        }
    )

    # Return
    divisions
}
