library(httr)
library(stringr)
source("configs.R")

get_period_string <- function(dataset_name) {
  base_string <- stringr::str_extract(dataset_name, "\\d{6}Q\\d")
  year_start <- as.numeric(substr(base_string, 1, 4))
  year <- paste0(year_start, "/", year_start-2000)
  quarter <- substr(base_string, 7,8)
  outstring <- paste(year, quarter)
  return(outstring)
}

get_json <- function(url) {
  # Send API call to get list of data-sets
  h <- curl::new_handle(
    proxy = bcc_proxy_url
  )
  
  res <- curl::curl_fetch_memory(
    url,
    handle = h
  )
  
  jsonlite::fromJSON(rawToChar(res$content))
}

get_open_data_list <- function() {
  #Define the url for the API call
  base_endpoint <- "https://opendata.nhsbsa.net/api/3/action/"
  package_list_method <- "package_list"     # List of data-sets in the portal
  package_show_method <- "package_show?id=" # List all resources of a data-set
  action_method <- "datastore_search_sql?"  # SQL action method
  
  datasets_response <-get_json(
    paste0(
      base_endpoint, 
      package_list_method
    )
  )
  # Now lets have a look at the data-sets currently available
  datasets_response$result
}

get_pharmacy_data <- function(
    period
) {
  dataset_id <- get_dataset_name(period)
  
  url <- paste0(
    "https://opendata.nhsbsa.net/api/3/action/datastore_search?resource_id=",
    dataset_id,
    "&limit=15000"
    )
  
  get_json(url)$result$records %>%
    mutate(
      period = get_period_string(dataset_id)
    )
}

# Calculates string one year before one given
# e.g. "2020-2021 Quarter 3" -> "2019-2020 Quarter 3"
last_year_str <- function(
    period
) {
  str_replace(
    period, 
    "^(\\d{4})-(\\d{2,4})( Quarter \\d+)$",
    \(m) sprintf("%04d-%0*d%s",
                 as.integer(str_extract(m, "^\\d{4}")) - 1,
                 ifelse(nchar(str_extract(m, "(?<=-)\\d+")) == 2, 2, 4),
                 as.integer(str_extract(m, "(?<=-)\\d+")) - 1,
                 str_extract(m, " Quarter \\d+$")
                 )
    )
}
  
get_dataset_name <- function(
    period = "Most recent"
) {
  # add assert for periods
  
  resources <- get_json(
    "https://opendata.nhsbsa.net/api/3/action/package_show?id=consolidated-pharmaceutical-list"
  )$result$resources %>%
    mutate(period = str_extract(title, "(?<= - ).*"))
  
  if (period == "Most recent") {
    name <- resources %>%
      filter(
        period == max(period)
      ) %>%
      pull(name)
  } else if (period == "Last year") {
    name <- resources %>%
      filter(
        period == last_year_str(max(period))
      ) %>%
      pull(name)
  }
  return(name)
}