### Package constants and cache

# API URL ---------------------------------------------------------------------

API_COMMONS_GENERAL <- "https://commonsvotes-api.parliament.uk/data/divisions.json/"
API_COMMONS_SINGLE  <- "https://commonsvotes-api.parliament.uk/data/division/"
API_COMMONS_TOTAL   <- "https://commonsvotes-api.parliament.uk/data/divisions.json/searchTotalResults"
API_LORDS_GENERAL <- "https://lordsvotes-api.parliament.uk/data/Divisions/"
API_LORDS_SINGLE <- "https://lordsvotes-api.parliament.uk/data/Divisions/"
API_LORDS_TOTAL <- "https://lordsvotes-api.parliament.uk/data/Divisions/searchTotalResults"

# Cache -----------------------------------------------------------------------

cache <- new.env(parent = emptyenv())
CACHE_CDS_RAW <- "cds_raw"
CACHE_LDS_RAW <- "lds_raw"
