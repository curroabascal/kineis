#' Function to make the format readable to WC portal.
#' @export
#' @examples
#' kineisdata=download_kineis_data(device_refs='267094',from_date="2026-02-01",to_date="2026-02-05")
#' head(kineisdata)
#' kineisdata_formatted=change_format(kineisdata)
change_format<-function (data){
  newdata = data.frame(Device_ID = data$deviceRef, Program_Ref = data$programRef, 
                       `Message_date_(UTC)` = data$msgDatetime, Raw_data = data$rawData, 
                       `Size_(bits)` = data$bitLength, `GPS_Date_of_position_(UTC)` = "", 
                       GPS_Longitude = "", GPS_Latitude = "", GPS_Altitude = "", 
                       GPS_Speed = "", GPS_Heading = "", Sensors = data$sensors, 
                       Doppler_Position_ID = data$dopplerLocId, Doppler_revision = data$dopplerRevision, 
                       `Doppler_Date_(UTC)` = data$dopplerDatetime, Doppler_Longitude = data$dopplerLocLon, 
                       Doppler_Latitude = data$dopplerLocLat, Doppler_Altitude = data$dopplerLocAlt, 
                       Doppler_Error_radius = data$dopplerLocErrorRadius, Doppler_device_Frequency = data$dopplerDeviceFrequency, 
                       Doppler_Class = data$dopplerLocClass, Doppler_Nb._Msg = data$dopplerNbMsg, 
                       Satellite = data$kineisMetadata.sat, Modulation = data$kineisMetadata.mod, 
                       Signal_Level = data$kineisMetadata.level, SNR = data$kineisMetadata.snr, 
                       Frequency = data$kineisMetadata.freq, Message_UID = data$deviceMsgUid, 
                       check.names = F)
  names(newdata) = gsub("_", " ", names(newdata))
  newdata$`Message date (UTC)` = as.POSIXct(newdata$`Message date (UTC)`, 
                                            tz = "", format = "%Y-%m-%dT%H:%M:%S")
  newdata$`Doppler Date (UTC)` = as.POSIXct(newdata$`Doppler Date (UTC)`, 
                                            tz = "", format = "%Y-%m-%dT%H:%M:%S")
  newdata = newdata[order(newdata$`Message date (UTC)`, decreasing = T), 
  ]
  
  #Problema cuando la hora es 00:00:00, ya que lo corta y después no se lee.
  newdata$`Message date (UTC)`=as.character(format(newdata$`Message date (UTC)`,'%Y-%m-%d %H:%M:%S'))
  newdata$`Doppler Date (UTC)`=as.character(format(newdata$`Doppler Date (UTC)`,'%Y-%m-%d %H:%M:%S'))
  
  convert_sensor_format <- function(input) {
    cleaned <- gsub("\": \"", "=", input)
    cleaned <- gsub("\"", "", cleaned)
    cleaned <- gsub("\\s*=", "=", cleaned)
    cleaned <- gsub("=\\s*", "=", cleaned)
    cleaned <- paste0("\"", cleaned, "\"")
    return(cleaned)
  }
  toEnotation <- function(x) {
    x0 <- sprintf("%.11E", as.numeric(x))
    x1 <- gsub("E[+]0*([0-9]+)", "E\\1", x0)
    x2 <- gsub("(\\.[0-9]+?)0+(?=E|$)", "\\1", x1, perl = TRUE)
    return(x2)
  }
  newdata$Sensors = convert_sensor_format(newdata$Sensors)
  newdata$`Doppler device Frequency` = toEnotation(newdata$`Doppler device Frequency`)
  newdata$Frequency = toEnotation(newdata$Frequency)
  newdata$`Doppler Altitude` = sprintf("%.1f", as.numeric(newdata$`Doppler Altitude`))
  newdata$`Doppler Error radius` = sprintf("%.1f", as.numeric(newdata$`Doppler Error radius`))
  newdata$SNR = sprintf("%.1f", as.numeric(newdata$SNR))
  for (col in names(newdata)) {
    if (is.character(newdata[[col]])) {
      newdata[[col]][newdata[[col]] == "NA"] <- ""
    }
  }
  newdata = newdata[!duplicated(newdata$`Message UID`), ]
  return(newdata)
}

