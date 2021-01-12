### Download data for unit testing Lords

# Imports ---------------------------------------------------------------------

source("tests/testthat/validate.R")

# Functions -------------------------------------------------------------------

#' Fetch mocks data for unit tests of Lords
#'
#' @keywords internal

fetch_lords_mocks_data <- function() {

    # Download Lords divisions
    lds <- process_lds_pagination(stringr::str_glue("{API_LORDS_GENERAL}search"))
    write(lds, "lords_divisions")
    Sys.sleep(API_PAUSE_TIME)

    # Download Lords votes
    ldv <- request(stringr::str_glue("{API_LORDS_SINGLE}{LORDS_DIVISION_ID}"))
    write(ldv, "lords_votes")
    Sys.sleep(API_PAUSE_TIME)

    # Download Lords divisions member vote
    ldm <- process_lds_pagination_member(
        stringr::str_glue("{API_LORDS_GENERAL}membervoting"),
        LORDS_MEMBER_ID)
    write(ldm, "lords_vote_member")
    Sys.sleep(API_PAUSE_TIME)

}

#' Fetch validation data for unit tests of Lords
#'
#' @keywords internal

fetch_lords_validation_data <- function() {

    # Fetch all Lords divisions
    lda <- fetch_lords_divisions_all()
    write(lda, "fetch_lords_divisions_all")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch all Lords divisions with from and to dates
    lda <- fetch_lords_divisions_all(
        from = LORDS_TEST_DATE, to = LORDS_TEST_DATE)
    write(lda, "fetch_lords_divisions_all_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords divisions tellers
    ldt <- fetch_lords_divisions_tellers()
    write(ldt, "fetch_lords_divisions_tellers")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords divisions tellers with from and to dates
    ldt <- fetch_lords_divisions_tellers(
        from_date = LORDS_TEST_DATE, to_date = LORDS_TEST_DATE)
    write(ldt, "fetch_lords_divisions_tellers_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch remote Lords divisions
    ldr <- fetch_lords_divisions_remote()
    write(ldr, "fetch_lords_divisions_remote")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch remote lords divisions with from and to dates
    ldr <- fetch_lords_divisions_remote(
        from_date = LORDS_TEST_DATE, to_date = LORDS_TEST_DATE)
    write(ldr, "fetch_lords_divisions_remote_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords divisions votes
    ldv <- fetch_lords_divisions_votes(LORDS_DIVISION_ID)
    write(ldv, "fetch_lords_divisions_votes")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords divisions votes by party
    ldp <- fetch_lords_divisions_party(LORDS_DIVISION_ID)
    write(ldp, "fetch_lords_divisions_votes_party")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords divisions votes by lord type
    ldlt <- fetch_lords_divisions_lord_type(LORDS_DIVISION_ID)
    write(ldlt, "fetch_lords_divisions_lord_type")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords divisions members
    ldm <- fetch_lords_divisions_members(LORDS_MEMBER_ID)
    write(ldm, "fetch_lords_divisions_members")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch lords divisions members with from and to dates
    ldm <- fetch_lords_divisions_members(
        LORDS_MEMBER_ID, from_date = LORDS_TEST_DATE, to_date = LORDS_TEST_DATE)
    write(ldm, "fetch_lords_divisions_members_from_to")
    Sys.sleep(API_PAUSE_TIME)
}

# Fetch all data --------------------------------------------------------------

#' Fetch mocks and validation data for unit tests of Lords
#'
#' @keywords internal

fetch_lords_test_data <- function() {
    fetch_lords_mocks_data()
    fetch_lords_validation_data()
}



