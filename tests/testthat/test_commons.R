### Test Commons functions
context("Commons functions")

# Imports ---------------------------------------------------------------------

source("validate.R")

# Setup -----------------------------------------------------------------------

# Raw mock data
mock_fetch_commons_divisions <- read("commons_divisions")
mock_fetch_commons_votes <- read("commons_votes")
mock_fetch_commons_vote_member <- read("commons_vote_member")

# Mock replacement functions
mock_process_cds_pagination <- function(response) {
    function(url) {response}
}
mock_request <- function(response) {
    function(url) {response}
}
mock_process_cds_pagination_member <- function(response) {
    function(url, id) {response}
}



# Tests -----------------------------------------------------------------------

test_that("fetch_commons_divisions_all processes results correctly.", {
    with_mock(
        "clvotes::process_cds_pagination" = mock_process_cds_pagination(
            mock_fetch_commons_divisions), {

        cols <- c(
            "division_id",
            "division_date",
            "division_title",
            "aye_count",
            "no_count")

        obs <- fetch_commons_divisions_all()
        exp <- read("fetch_commons_divisions_all")
        compare_obs_exp(obs, exp, cols, "division_id")

        obs <- fetch_commons_divisions_all(from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
        exp <- read("fetch_commons_divisions_all_from_to")
        compare_obs_exp(obs, exp, cols, "division_id")
    })
})

test_that("fetch_commons_divisions_evel processes results correctly.", {
    with_mock(
        "clvotes::process_cds_pagination" = mock_process_cds_pagination(
            mock_fetch_commons_divisions), {

        cols <- c(
            "division_id",
            "division_date",
            "division_title",
            "evel_type",
            "evel_country",
            "aye_count",
            "no_count",
            "double_majority_aye_count",
            "double_majority_no_count")

        obs <- fetch_commons_divisions_evel()
        exp <- read("fetch_commons_divisions_evel")
        compare_obs_exp(obs, exp, cols, "division_id")

        obs <- fetch_commons_divisions_evel(from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
        exp <- read("fetch_commons_divisions_evel_from_to")
        compare_obs_exp(obs, exp, cols, "division_id")
    })
})

test_that("fetch_commons_divisions_deferred processes results correctly.", {
    with_mock(
        "clvotes::process_cds_pagination" = mock_process_cds_pagination(
            mock_fetch_commons_divisions), {

        cols <- c(
            "division_id",
            "division_date",
            "division_title",
            "aye_count",
            "no_count")

        obs <- fetch_commons_divisions_deferred()
        exp <- read("fetch_commons_divisions_deferred")
        compare_obs_exp(obs, exp, cols, "division_id")

        obs <- fetch_commons_divisions_deferred(from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
        exp <- read("fetch_commons_divisions_deferred_from_to")
        compare_obs_exp(obs, exp, cols, "division_id")
    })
})

test_that("fetch_commons_divisions_tellers processes results correctly.", {
    with_mock(
        "clvotes::process_cds_pagination" = mock_process_cds_pagination(
            mock_fetch_commons_divisions), {

        cols <- c(
            "division_id",
            "division_date",
            "division_title",
            "division_lobby",
            "mnis_id",
            "member_name",
            "member_party")

        obs <- fetch_commons_divisions_tellers()
        exp <- read("fetch_commons_divisions_tellers")
        compare_obs_exp(obs, exp, cols, "division_id")

        obs <- fetch_commons_divisions_tellers(from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
        exp <- read("fetch_commons_divisions_tellers_from_to")
        compare_obs_exp(obs, exp, cols, "division_id")
    })
})

test_that("fetch_commons_divisions_remote processes results correctly.", {
    with_mock(
        "clvotes::process_cds_pagination" = mock_process_cds_pagination(
            mock_fetch_commons_divisions), {

        cols <- c(
            "division_id",
            "division_date",
            "division_title",
            "aye_count",
            "no_count")

        obs <- fetch_commons_divisions_remote()
        exp <- read("fetch_commons_divisions_remote")
        compare_obs_exp(obs, exp, cols, "division_id")

        obs <- fetch_commons_divisions_remote(from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
        exp <- read("fetch_commons_divisions_remote_from_to")
        compare_obs_exp(obs, exp, cols, "division_id")
    })
})

test_that("fetch_commons_divisions_votes processes results correctly.", {
    with_mock(
        "clvotes::request" = mock_request(
            mock_fetch_commons_votes), {

        cols <- c(
            "division_id",
            "division_date",
            "division_title",
            "vote_direction",
            "mnis_id",
            "member_name",
            "member_party",
            "member_proxy_name")

        obs <- fetch_commons_divisions_votes(COMMONS_DIVISION_ID)
        exp <- read("fetch_commons_divisions_votes")
        compare_obs_exp(obs, exp, cols, "division_id")
    })
})

test_that("fetch_commons_divisions_party processes results correctly.", {
    with_mock(
        "clvotes::request" = mock_request(
            mock_fetch_commons_votes), {

        cols <- c(
            "division_id",
            "division_date",
            "division_title",
            "member_party",
            "vote_direction",
            "vote_count")

        obs <- fetch_commons_divisions_party(COMMONS_DIVISION_ID)
        exp <- read("fetch_commons_divisions_votes_party")
        compare_obs_exp(obs, exp, cols, "division_id")
    })
})

test_that("fetch_commons_divisions_members processes results correctly.", {
    with_mock(
        "clvotes::process_cds_pagination_member" = mock_process_cds_pagination_member(
            mock_fetch_commons_vote_member), {

        cols <- c(
            "division_id",
            "division_date",
            "division_title",
            "mnis_id",
            "member_voted_aye",
            "member_was_teller")

        obs <- fetch_commons_divisions_members(COMMONS_MEMBER_ID)
        exp <- read("fetch_commons_divisions_members")
        compare_obs_exp(obs, exp, cols, "division_id")

        obs <- fetch_commons_divisions_members(COMMONS_MEMBER_ID, from_date = COMMONS_TEST_DATE, to_date = COMMONS_TEST_DATE)
        exp <- read("fetch_commons_divisions_members_from_to")
        compare_obs_exp(obs, exp, cols, "division_id")
    })
})
