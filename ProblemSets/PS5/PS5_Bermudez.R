library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(tibble)

# Webscraping w/o API

## URL of the page
url <- 'https://en.wikipedia.org/wiki/Accounting_scandals'

## Read the HTML from the URL
html <- read_html(url)

## Parse the HTML and extract the table
scandal <- html %>%
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table.wikitable") %>%
  html_table()

## Print the table
print(scandal)

# Webscraping w/API - SEC EDGAR

## Company Submissions API URL
### Define SEC EDGAR API URL for Wells Fargo
cik <- "0000740906"
url <- paste0("https://data.sec.gov/submissions/CIK", cik, ".json")

### Make API request with User-Agent header
response <- GET(url, add_headers(`User-Agent` = "hleebermudez@gmail.com"))

### Parse JSON
data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
### Extract recent filings
filings_df <- as_tibble(data$filings$recent)
  
### View first few rows
print(head(filings_df))

## Company Facts API URL
### Define SEC EDGAR API URL for Apple **Wells Fargo does not have structured data via the API**
cik <- "0000320193"
url <- paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik, ".json")

### Make API request with User-Agent header
response <- GET(url, add_headers(`User-Agent` = "hleebermudez@gmail.com"))

### Parse JSON
data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

# Extract financial facts
facts <- data$facts$`us-gaap`

# Convert nested JSON into a structured DataFrame
extract_values <- function(fact_name, fact_data) {
  if (!is.null(fact_data$units$USD)) {
    df <- as_tibble(fact_data$units$USD) %>%
      mutate(Metric = fact_name)
    return(df)
  } else {
    return(NULL)
  }
}

# Extract financial data for key concepts
facts_list <- lapply(names(facts), function(name) extract_values(name, facts[[name]]))

facts_df <- bind_rows(facts_list)

### View first few rows
print(head(facts_df))
