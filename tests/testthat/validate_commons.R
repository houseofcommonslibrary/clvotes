### Download data for unit testing Commons

# Imports ---------------------------------------------------------------------

source("tests/testthat/validate.R")

# Functions -------------------------------------------------------------------

#' Fetch mocks data for unit tests of Commons
#'
#' @keywords internal

fetch_commons_mocks_data <- function() {

    # Download Commons divisions
    cds <- process_cds_pagination(stringr::str_glue("{API_COMMONS_GENERAL}search"))
    write(cds, "commons_divisions")
    Sys.sleep(API_PAUSE_TIME)

    # Download Commons votes
    cdv <- request(stringr::str_glue("{API_COMMONS_SINGLE}{COMMONS_DIVISION_ID}.json"))
    write(cdv, "commons_votes")
    Sys.sleep(API_PAUSE_TIME)

    # Download Commons divisions member vote
    cdm <- process_cds_pagination_member(
        stringr::str_glue("{API_COMMONS_GENERAL}membervoting"),
        COMMONS_MEMBER_ID)
    write(cdm, "commons_vote_member")
    Sys.sleep(API_PAUSE_TIME)

}

#' Fetch validation data for unit tests of Commons
#'
#' @keywords internal

fetch_commons_validation_data <- function() {

    # Fetch all commons divisions
    cda <- fetch_commons_divisions_all()
    write(cda, "fetch_commons_divisions_all")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch all commons divisions with from and to dates
    cda <- fetch_commons_divisions_all(
        from = COMMONS_TEST_DATE, to = COMMONS_TEST_DATE)
    write(cda, "fetch_commons_divisions_all_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch EVEL commons divisions
    cde <- fetch_commons_divisions_evel()
    write(cde, "fetch_commons_divisions_evel")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch EVEL commons divisions with from and to dates
    cde <- fetch_commons_divisions_evel(
        from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
    write(cde, "fetch_commons_divisions_evel_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch deferred commons divisions
    cdd <- fetch_commons_divisions_deferred()
    write(cdd, "fetch_commons_divisions_deferred")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch deferred commons divisions with from and to dates
    cde <- fetch_commons_divisions_deferred(
        from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
    write(cde, "fetch_commons_divisions_deferred_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch commons divisions tellers
    cdt <- fetch_commons_divisions_tellers()
    write(cdt, "fetch_commons_divisions_tellers")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch commons divisions tellers with from and to dates
    cdt <- fetch_commons_divisions_tellers(
        from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
    write(cdt, "fetch_commons_divisions_tellers_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch remote commons divisions
    cdr <- fetch_commons_divisions_remote()
    write(cdr, "fetch_commons_divisions_remote")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch remote commons divisions with from and to dates
    cdr <- fetch_commons_divisions_remote(
        from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
    write(cdr, "fetch_commons_divisions_remote_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch commons divisions votes
    cdv <- fetch_commons_divisions_votes(COMMONS_DIVISION_ID)
    write(cdv, "fetch_commons_divisions_votes")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch commons divisions votes by party
    cdp <- fetch_commons_divisions_party(COMMONS_DIVISION_ID)
    write(cdp, "fetch_commons_divisions_votes_party")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch commons divisions members
    cdm <- fetch_commons_divisions_members(COMMONS_MEMBER_ID)
    write(cdm, "fetch_commons_divisions_members")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch commons divisions members with from and to dates
    cdm <- fetch_commons_divisions_members(
        COMMONS_MEMBER_ID, from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
    write(cdm, "fetch_commons_divisions_members_from_to")
    Sys.sleep(API_PAUSE_TIME)

}


# Fetch all data --------------------------------------------------------------

#' Fetch mocks and validation data for unit tests of MPs
#'
#' @keywords internal

fetch_commons_test_data <- function() {
    fetch_commons_mocks_data()
    fetch_commons_validation_data()
}



