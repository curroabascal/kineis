#' Function to download data for a platform between a range of dates.
#' @export
#' @examples
#' kineisdata=download_kineis_data(device_refs='267094',from_date="2026-02-01",to_date="2026-02-05")
#' head(kineisdata)
download_kineis_data <- function(device_refs, from_date, to_date) {
  # Get token
  myloadpackage('dplyr')
  access_token<-check_credentials()
  USERNAME<-Sys.getenv('kineis_user')
  PASSWORD<-Sys.getenv('kineis_pass')
  AUTH_URL <- "https://account.groupcls.com/auth/realms/cls/protocol/openid-connect/token"
  API_TELEMETRY_URL <- "https://api.groupcls.com/telemetry/api/v1"
  request_url <- paste0(API_TELEMETRY_URL, "/retrieve-bulk")
  all_data <- list()
  after_cursor <- NULL
  total_records <- 0
  
  repeat {
    # Build request with pagination cursor
    request_payload <- list(
      pagination = list(
        first = 2500),
      retrieveMetadata = TRUE, retrieveRawData = TRUE, retrieveDoppler = TRUE,
      retrieveGpsLoc = TRUE, retrieveSensors = TRUE, retrieveAdditionnalProperties = TRUE,
      deviceRefs = as.list(device_refs),
      fromDatetime = paste0(from_date, "T00:00:00.001Z"),
      toDatetime = paste0(to_date, "T23:59:59.999Z"),
      datetimeFormat = "DATETIME"
    )
    
    if (!is.null(after_cursor)) {
      request_payload$pagination$after <- after_cursor
    }
    
    data_resp <- POST(request_url,
                      add_headers(Authorization = paste("Bearer", access_token),
                                  Accept = "application/json", 
                                  "Content-Type" = "application/json"),
                      body = request_payload, encode = "json")
    
    if (status_code(data_resp) != 200) {
      stop("API error: ", content(data_resp, "text"))
    }
    
    data <- fromJSON(content(data_resp, "text"))
    new_records <- nrow(data$contents)
    total_records <- total_records + new_records
    
    cat("Page: Downloaded", new_records, "records (Total:", total_records, ")\n")
    
    # Add new records
    # all_data=list()
    # length(all_data)
    all_data <- c(all_data, list(data$contents))
    # length(all_data)
    # Check if more pages
    page_info <- data$pageInfo
    if (!page_info$hasNextPage || is.null(page_info$endCursor)) {
      cat("✓ Complete! Total records:", total_records, "\n")
      break
    }
    
    # Get next cursor
    after_cursor <- page_info$endCursor
    cat("Next cursor:", substr(after_cursor, 1, 20), "...\n")
    Sys.sleep(0.5)  # Rate limiting
  }
  all_data <- bind_rows(all_data)
  return(all_data)
}
