### Test API query functions
context("Query API function")

# Commons division query
cds_query_cols <- c(
    "division_id",
    "division_date",
    "publication_updated",
    "number",
    "is_deferred",
    "evel_type",
    "evel_country",
    "division_title",
    "aye_count",
    "no_count",
    "double_majority_aye_count",
    "double_majority_no_count",
    "aye_tellers",
    "no_tellers",
    "ayes",
    "noes",
    "friendly_description",
    "friendly_title",
    "no_vote_recorded",
    "remote_voting_start",
    "remote_voting_end")

lds_query_cols <- c(
    "division_id",
    "division_date",
    "number",
    "notes",
    "division_title",
    "is_whipped",
    "is_government_content",
    "content_count",
    "not_content_count",
    "sponsoring_member_id",
    "is_house",
    "amendment_motion_notes",
    "is_government_win",
    "remote_voting_start",
    "remote_voting_end",
    "content_tellers",
    "not_content_tellers",
    "contents",
    "not_contents")

# Tests -----------------------------------------------------------------------

test_that("request sends and recieves basic Commons division query", {

    # Query
    query <- stringr::str_glue("{API_COMMONS_GENERAL}search")

    # Fetch data
    response <- httr::GET(query)
    response_text <- httr::content(response, as = "text", encoding = "utf-8")
    response_text <- response_text %>% jsonlite::fromJSON(flatten = TRUE)

    expect_equal(response$status_code, 200)
    expect_equal(ncol(response_text), 21)
    expect_equal(colnames(response_text), cds_query_cols)
})

test_that("request sends and recieves basic Lords division query", {

    # Query
    query <- stringr::str_glue("{API_LORDS_GENERAL}search")

    # Fetch data
    response <- httr::GET(query)
    response_text <- httr::content(response, as = "text", encoding = "utf-8")
    response_text <- response_text %>% jsonlite::fromJSON(flatten = TRUE)

    expect_equal(response$status_code, 200)
    expect_equal(ncol(response_text), 21)
    expect_equal(colnames(response_text), lds_query_cols)
})
