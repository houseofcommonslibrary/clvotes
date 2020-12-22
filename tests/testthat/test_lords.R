### Test Lords functions
context("Lords functions")

# Imports ---------------------------------------------------------------------

source("validate.R")

# Setup -----------------------------------------------------------------------

# Raw mock data
mock_fetch_lords_divisions <- read("lords_divisions")
mock_fetch_lords_votes <- read("lords_votes")
mock_fetch_lords_vote_member <- read("lords_vote_member")

# Mock replacement functions
mock_process_lds_pagination <- function(response) {
    function(url) {response}
}
mock_request <- function(response) {
    function(url) {response}
}
mock_process_lds_pagination_member <- function(response) {
    function(url, id) {response}
}



# Tests -----------------------------------------------------------------------

test_that("fetch_lords_divisions_all processes results correctly.", {
    with_mock(
        "clvotes::process_lds_pagination" = mock_process_lds_pagination(
            mock_fetch_lords_divisions), {

                cols <- c(
                    "division_id",
                    "division_date",
                    "division_title",
                    "content_count",
                    "not_content_count")

                obs <- fetch_lords_divisions_all()
                exp <- read("fetch_lords_divisions_all")
                compare_obs_exp(obs, exp, cols, "division_id")

                obs <- fetch_lords_divisions_all(
                    from_date = LORDS_TEST_DATE, to_date = LORDS_TEST_DATE)
                exp <- read("fetch_lords_divisions_all_from_to")
                compare_obs_exp(obs, exp, cols, "division_id")
            })
})

test_that("fetch_lords_divisions_tellers processes results correctly.", {
    with_mock(
        "clvotes::process_lds_pagination" = mock_process_lds_pagination(
            mock_fetch_lords_divisions), {

                cols <- c(
                    "division_id",
                    "division_date",
                    "division_title",
                    "division_lobby",
                    "mnis_id",
                    "given_name",
                    "family_name",
                    "display_name",
                    "gender")

                obs <- fetch_lords_divisions_tellers()
                exp <- read("fetch_lords_divisions_tellers")
                compare_obs_exp(obs, exp, cols, "division_id")

                obs <- fetch_lords_divisions_tellers(
                    from_date = LORDS_TEST_DATE, to_date = LORDS_TEST_DATE)
                exp <- read("fetch_lords_divisions_tellers_from_to")
                compare_obs_exp(obs, exp, cols, "division_id")
            })
})

test_that("fetch_lords_divisions_remote processes results correctly.", {
    with_mock(
        "clvotes::process_lds_pagination" = mock_process_lds_pagination(
            mock_fetch_lords_divisions), {

                cols <- c(
                    "division_id",
                    "division_date",
                    "division_title",
                    "content_count",
                    "not_content_count")

                obs <- fetch_lords_divisions_remote()
                exp <- read("fetch_lords_divisions_remote")
                compare_obs_exp(obs, exp, cols, "division_id")

                obs <- fetch_lords_divisions_remote(
                    from_date = LORDS_TEST_DATE, to_date = LORDS_TEST_DATE)
                exp <- read("fetch_lords_divisions_remote_from_to")
                compare_obs_exp(obs, exp, cols, "division_id")
            })
})

test_that("fetch_lords_divisions_votes processes results correctly.", {
    with_mock(
        "clvotes::request" = mock_request(
            mock_fetch_lords_votes), {

                cols <- c(
                    "division_id",
                    "division_date",
                    "division_title",
                    "vote_direction",
                    "mnis_id",
                    "given_name",
                    "family_name",
                    "display_name",
                    "gender",
                    "member_party")

                obs <- fetch_lords_divisions_votes(LORDS_DIVISION_ID)
                exp <- read("fetch_lords_divisions_votes")
                compare_obs_exp(obs, exp, cols, "division_id")
            })
})

test_that("fetch_lords_divisions_party processes results correctly.", {
    with_mock(
        "clvotes::request" = mock_request(
            mock_fetch_lords_votes), {

                cols <- c(
                    "division_id",
                    "division_date",
                    "division_title",
                    "member_party",
                    "vote_direction",
                    "vote_count")

                obs <- fetch_lords_divisions_party(LORDS_DIVISION_ID)
                exp <- read("fetch_lords_divisions_votes_party")
                compare_obs_exp(obs, exp, cols, "division_id")
            })
})

test_that("fetch_lords_divisions_members processes results correctly.", {
    with_mock(
        "clvotes::process_cds_pagination_member" = mock_process_lds_pagination_member(
            mock_fetch_lords_vote_member), {

                cols <- c(
                    "division_id",
                    "division_date",
                    "division_title",
                    "mnis_id",
                    "given_name",
                    "family_name",
                    "display_name",
                    "gender",
                    "member_was_content",
                    "member_was_teller")

                obs <- fetch_lords_divisions_members(LORDS_MEMBER_ID)
                exp <- read("fetch_lords_divisions_members")
                compare_obs_exp(obs, exp, cols, "division_id")

                obs <- fetch_lords_divisions_members(
                    LORDS_MEMBER_ID, from_date = LORDS_TEST_DATE, to_date = LORDS_TEST_DATE)
                exp <- read("fetch_lords_divisions_members_from_to")
                compare_obs_exp(obs, exp, cols, "division_id")
            })
})
