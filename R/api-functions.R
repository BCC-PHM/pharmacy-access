library(httr)
library(jsonlite)
library(stringr)

get_open_data_list <- function() {
  #Define the url for the API call
  base_endpoint <- "https://opendata.nhsbsa.net/api/3/action/"
  package_list_method <- "package_list"     # List of data-sets in the portal
  package_show_method <- "package_show?id=" # List all resources of a data-set
  action_method <- "datastore_search_sql?"  # SQL action method
  
  # Send API call to get list of data-sets
  datasets_response <- fromJSON(paste0(
    base_endpoint, 
    package_list_method
  ))
  
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
  
  fromJSON(url)$result$records
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
  
  resources <- fromJSON(
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