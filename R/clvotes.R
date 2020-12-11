#' clvotes: Tools for retrieving data from the Commons and Lords
#' Votes API
#'
#' The clvotes package provides a suit of tools for downloading and
#' processing data from the UK Parliament's Commons and Lords Votes API.
#'
#' @docType package
#' @name clvotes
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom data.table :=
NULL

# Tell R CMD check about new operators
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ":="))
}
