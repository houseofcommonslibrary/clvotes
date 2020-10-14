### Package constants

# Import
library(magrittr)

# API URL ---------------------------------------------------------------------

API_GENERAL <- "https://commonsvotes-api.parliament.uk/data/divisions.json/"
API_SINGLE  <- "https://commonsvotes-api.parliament.uk/data/division/"
API_TOTAL   <- "https://commonsvotes-api.parliament.uk/data/divisions.json/searchTotalResults"

# Cache -----------------------------------------------------------------------

cache <- new.env(parent = emptyenv())
CACHE_COMMONS_ALL_DIVISIONS_RAW <- "commons_all_divisions_raw"
