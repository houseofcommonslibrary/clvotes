### Manage test data for validation

# Constants -------------------------------------------------------------------

READ_TEST_DATA_DIR <- file.path("data")
WRITE_TEST_DATA_DIR <- file.path("tests", "testthat", "data")
API_PAUSE_TIME <- 1

# Commons
COMMONS_TEST_DATE <- "2017-02-08"
COMMONS_DIVISION_ID <- 219
COMMONS_MEMBER_ID <- 172
# COMMONS_DIVISIONS <- stringr::str_glue("{API_COMMONS_GENERAL}search")

# Lords
LORDS_TEST_DATE <- "2017-03-20"
LORDS_DIVISION_ID <- 2085
LORDS_MEMBER_ID <- 3743


# Read and write data ---------------------------------------------------------

#' Read a file from the data directory
#'
#' @keywords internal

read <- function(filename) {
    readRDS(file.path(READ_TEST_DATA_DIR,
                      stringr::str_glue("{filename}.RData")))
}

#' Write a tibble to the data directory
#'
#' @keywords internal

write <- function(df, filename) {
    saveRDS(df, file.path(WRITE_TEST_DATA_DIR,
                          stringr::str_glue("{filename}.RData")))
}

# Comparison function ---------------------------------------------------------

#' Compare two dataframes on structure and contents of selected columns
#'
#' @keywords internal

compare_obs_exp <- function(obs, exp, cols, sort_col) {

    obs <- obs %>% dplyr::arrange(.data[[sort_col]])
    exp <- exp %>% dplyr::arrange(.data[[sort_col]])

    expect_true(all(cols %in% colnames(exp)))
    expect_equal(nrow(obs), nrow(exp))
    expect_equal(ncol(obs), ncol(exp))
    expect_equal(colnames(obs), colnames(exp))

    for (col in cols) {
        expect_equal(obs[[col]], exp[[col]])
    }
}
