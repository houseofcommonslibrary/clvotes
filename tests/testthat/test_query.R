### Test API query functions
context("Query API function")

# Commons division query
cds_query_cols <- c(
    "DivisionId",
    "Date",
    "PublicationUpdated",
    "Number",
    "IsDeferred",
    "EVELType",
    "EVELCountry",
    "Title",
    "AyeCount",
    "NoCount",
    "DoubleMajorityAyeCount",
    "DoubleMajorityNoCount",
    "AyeTellers",
    "NoTellers",
    "Ayes",
    "Noes",
    "FriendlyDescription",
    "FriendlyTitle",
    "NoVoteRecorded",
    "RemoteVotingStart",
    "RemoteVotingEnd")

lds_query_cols <- c(
    "divisionId",
    "date",
    "number",
    "notes",
    "title",
    "isWhipped",
    "isGovernmentContent",
    "tellerContentCount",
    "tellerNotContentCount",
    "sponsoringMemberId",
    "isHouse",
    "amendmentMotionNotes" ,
    "isGovernmentWin",
    "remoteVotingStart",
    "remoteVotingEnd",
    "contentTellers",
    "notContentTellers",
    "contents",
    "notContents")

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
    expect_equal(ncol(response_text), 19)
    expect_equal(colnames(response_text), lds_query_cols)
})
