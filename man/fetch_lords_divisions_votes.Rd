% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/divisions_lords.R
\name{fetch_lords_divisions_votes}
\alias{fetch_lords_divisions_votes}
\title{Fetch key details on Lords divisions votes}
\usage{
fetch_lords_divisions_votes(division_id)
}
\arguments{
\item{division_id}{An integer or vector of integers representing a division
ID.}
}
\value{
A tibble of key details of voting for a Lords division , with one row
per vote.
}
\description{
\code{fetch_lords_divisions_votes} fetches data from the Lords Votes API
showing key details about each division vote, with one row per division
vote. All votes or abstentions are returned.
}
