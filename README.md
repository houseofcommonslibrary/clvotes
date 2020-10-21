# clvotes
clvotes is an R package for downloading data from the UK Parliament's [Commons Votes API](https://commonsvotes-api.parliament.uk/swagger/ui/index) and [Lords Votes API](https://lordsvotes-api.parliament.uk/index.html). This package has been developed principally to support work on Parliamentary data in the House of Commons Library but it may be useful to other researchers working with this data. This package is still in active development and the API may evolve over time.

## Overview
The package provides sets of functions for retrieving data from different endpoints of the respective votes API and returning the data as a tibble. The package currently provides functions to download data from the main Commons and Lords Votes endpoints, but new functions may be added to extract data from other endpoints in future. The package does not aim to exhaustively expose every possible API parameter for a given endpoint, but is focussed on downloading key datasets than can be further explored, transformed and combined with other data in R. To help with using parts of the API that are not explicitly covered, the package also provides some lower level functions that allow you to retrieve data from any API endpoint URL as native R data structures.

## Installation
Install from Github with using remotes.

```r
install.packages("remotes")
remotes::install_github("houseofcommonslibrary/clvotes")
```

---

## Commons divisions
Some Commons functions have an optional arguments called ```from_date```, ```to_date``` and ```on_date``` which can be used to filter the rows returned based on the date of a division. The ```on_date``` argument is a convenience that sets the ```from_date``` and ```to_date``` to the same given date. The ```on_date``` has priority: if the ```on_date``` is set, the ```from_date``` and ```to_date``` are ignored. The values for these arguments can be either a Date or a string specifying a date in ISO 8601 format ("YYYY-MM-DD").

---

```r
clvotes::fetch_commons_divisions_all(from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about each Commons division, with one row per division.

This dataframe contains summary details for each division, such as the division date, title, the number of MPs who voted "Aye" and the number of MPs who voted "No".

---

```r
clvotes::fetch_commons_divisions_evel(from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about each EVEL Commons division, with one row per EVEL division.

This dataframe contains summary details for each EVEL division, such as the division date, title, EVEL type, the number of MPs who voted "Aye" and the number of MPs who voted "No".

--- 

```r
clvotes::fetch_commons_divisions_deferred(from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about each deferred Commons division, with one row per deferred division.

This dataframe contains summary details for each deferred division, such as the division date, title, the number of MPs who voted "Aye" and the number of MPs who voted "No".

--- 

```r
clvotes::fetch_commons_divisions_tellers(from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about each Commons division teller, with one row per division teller.

This dataframe contains summary details for each division teller, such as the division date, title, the division lobby of the teller, the teller name and teller gender. Divisions with zero tellers are ignored.

---

```r
clvotes::fetch_commons_divisions_remote(from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about each remote Commons division, with one row per remote division.

This dataframe contains summary details for each remote division, such as the division date, title, the number of MPs who voted "Aye" and the number of MPs who voted "No".

---

```r
clvotes::fetch_commons_divisions_votes(division_id)
```

Fetch a dataframe of key details about how each MP voted in a Commons division, with one row per MP vote.

This dataframe contains summary details for a division, such as the division date, title, the vote direction of an MP, the MP name, and gender.

---

```r
clvotes::fetch_commons_divisions_party(division_id)
```

Fetch a dataframe of key details about how each party voted in a Commons division, with one row per party vote direction.

This dataframe contains summary details for a division, such as the division date, title, party and each vote direction.

--- 

```r
clvotes::fetch_commons_divisions_members(division_id, from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about the division voting record of an MP, with one row per division.

This dataframe contains summary details on the voting record of an MP, such as the division date, title, whether the MP voted "Aye" and whether the MP was a teller.

---

## Lords divisions
Some Lords functions have an optional arguments called ```from_date```, ```to_date``` and ```on_date``` which can be used to filter the rows returned based on the date of a division. The ```on_date``` argument is a convenience that sets the ```from_date``` and ```to_date``` to the same given date. The ```on_date``` has priority: if the ```on_date``` is set, the ```from_date``` and ```to_date``` are ignored. The values for these arguments can be either a Date or a string specifying a date in ISO 8601 format ("YYYY-MM-DD").

--- 

```r
clvotes::fetch_lords_divisions_all(from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about each Lords division, with one row per division.

This dataframe contains summary details for each division, such as the division date, title, the number of Lords who voted "Content" and the number of MPs who voted "Not content".

--- 

```r
clvotes::fetch_lords_divisions_tellers(from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about each Lords division teller, with one row per division teller.

This dataframe contains summary details for each division teller, such as the division date, title, the division lobby of the teller, the teller name and teller gender. Divisions with zero tellers are ignored.

---

```r
clvotes::fetch_lords_divisions_remote(from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about each remote Lords division, with one row per remote division.

This dataframe contains summary details for each remote division, such as the division date, title, the number of Lords who voted "Content" and the number of MPs who voted "Not content".

---

```r
clvotes::fetch_lords_divisions_votes(division_id)
```

Fetch a dataframe of key details about how each Lord voted in a Lords division, with one row per Lord vote.

This dataframe contains summary details for a division, such as the division date, title, the vote direction of a Lord, the Lord name, and gender.

---

```r
clvotes::fetch_lords_divisions_party(division_id)
```

Fetch a dataframe of key details about how each party voted in a Lords division, with one row per party vote direction.

This dataframe contains summary details for a division, such as the division date, title, party and each vote direction.

--- 

```r
clvotes::fetch_lords_divisions_members(division_id, from_date = NA, to_date = NA, on_date = NA)
```

Fetch a dataframe of key details about the division voting record of aLord, with one row per division.

This dataframe contains summary details on the voting record of a Lord, such as the division date, title, whether the Lord voted "Content" and whether the Lord was a teller.
