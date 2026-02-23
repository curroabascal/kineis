#' Function to get a list of devices.
#' @export
#' @examples
#' devices=get_device_list()
#' devices
get_device_list <- function() {
  cat("=== Getting Device List ===\n")
  access_token <- check_credentials()
  API_TELEMETRY_URL <- "https://api.groupcls.com/telemetry/api/v1"
  # POST to retrieve-device-list (empty body like bash script)
  device_url <- paste0(API_TELEMETRY_URL, "/retrieve-device-list")
  device_resp <- POST(device_url,
                      add_headers(Authorization = paste("Bearer", access_token),
                                  Accept = "application/json", "Content-Type" = "application/json"),
                      body = list(),  # Empty body like their script
                      encode = "json"
  )
  
  cat("Device list status:", status_code(device_resp), "\n")
  devices <- fromJSON(content(device_resp, "text"))
  # print("YOUR DEVICES:")
  # print(devices)
  return(devices[[1]])
}
