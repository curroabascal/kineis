#' Function to download data for a platform between a range of dates.
#' @export
#' @examples
#' kineisdata=download_kineis_data(device_refs='267094',from_date="2025-10-15",to_date="2025-10-15")
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
    
    #Huge issue: R sees large numbers as numeric and rounds, and I lose the message ID...
    # content(data_resp, "text")
    # fromJSON(content(data_resp, "text"))$contents$deviceMsgUid
    # fromJSON(content(data_resp, "text", encoding = "UTF-8"))$contents$deviceMsgUid
    
    # #FIX
    # # Step 1: Get raw JSON text
    # raw_json <- content(data_resp, "text", encoding = "UTF-8")
    # 
    # # Step 2: Convert all numeric IDs to quoted strings in JSON
    # # This keeps large numbers as character
    # raw_json_char <- gsub(
    #   '(\"deviceMsgUid\":)([0-9]+)',
    #   '\\1"\\2"',
    #   raw_json
    # )
    # 
    # # Step 3: Parse JSON normally
    # raw_page <- fromJSON(raw_json_char, simplifyVector = FALSE)
    # 
    # # Step 4: Convert each record to data frame, all character
    # page_df <- bind_rows(lapply(raw_page$contents, function(x) {
    #   x <- lapply(x, as.character)  # convert all fields to character
    #   as.data.frame(x, stringsAsFactors = FALSE)
    # }))
    
    #FIX2
    # library(jsonlite)
    # library(dplyr)
    
    raw_json <- content(data_resp, "text", encoding = "UTF-8")
    
    # Convert deviceMsgUid and providerMsgId to strings in JSON
    raw_json_char <- gsub(
      '(\"deviceMsgUid\":)([0-9]+)',
      '\\1"\\2"',
      raw_json
    )
    raw_json_char <- gsub(
      '(\"providerMsgId\":)([0-9]+)',
      '\\1"\\2"',
      raw_json_char
    )
    
    # Parse JSON normally
    raw_page <- fromJSON(raw_json_char, simplifyVector = FALSE)
    
    # Flatten and convert all fields to character recursively
    flatten_message <- function(x, parent = NULL) {
      out <- list()
      for (nm in names(x)) {
        el <- x[[nm]]
        colname <- if (is.null(parent)) nm else paste(parent, nm, sep = ".")
        
        if (is.list(el)) {
          out <- c(out, flatten_message(el, parent = colname))
        } else {
          out[[colname]] <- as.character(el)
        }
      }
      out
    }
    
    page_df <- bind_rows(lapply(raw_page$contents, function(x) {
      as.data.frame(flatten_message(x), stringsAsFactors = FALSE)
    }))
    
  
    new_records <- nrow(page_df)
    total_records <- total_records + new_records
    
    cat("Page: Downloaded", new_records, "records (Total:", total_records, ")\n")
    
    # Add new records
    # all_data=list()
    # length(all_data)
    all_data <- rbind(all_data, page_df)
    # length(all_data)
    # Check if more pages
    page_info <- raw_page$pageInfo
    if (!page_info$hasNextPage || is.null(page_info$endCursor)) {
      cat("✓ Complete! Total records:", total_records, "\n")
      break
    }
    
    # Get next cursor
    after_cursor <- page_info$endCursor
    cat("Next cursor:", substr(after_cursor, 1, 20), "...\n")
    Sys.sleep(0.5)  # Rate limiting
  }
  # all_data <- bind_rows(all_data)
  return(all_data)
}
