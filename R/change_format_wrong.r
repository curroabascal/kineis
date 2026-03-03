#' Function to make the format readable to WC portal.
#' @export
#' @examples
#' kineisdata=download_kineis_data(device_refs='267094',from_date="2026-02-01",to_date="2026-02-05")
#' head(kineisdata)
#' kineisdata_formatted=change_format(kineisdata)
change_format<-function(data){
  newdata=data.frame(Device_ID=data$deviceRef,
                     Program_Ref=data$programRef,
                     'Message_date_(UTC)'=data$msgDatetime,
                     Raw_data=data$rawData,
                     'Size_(bits)'=data$bitLength,
                     'GPS_Date_of_position_(UTC)'="",
                     GPS_Longitude="",
                     GPS_Latitude="",
                     Sensors=data$sensors,
                     'Doppler_Date_(UTC)'=data$dopplerDatetime,
                     Doppler_Longitude=data$dopplerLocLon,
                     Doppler_Latitude=data$dopplerLocLat,
                     Doppler_Error_radius=data$dopplerLocErrorRadius,
                     Doppler_Class=data$dopplerLocClass,
                     Doppler_Nb._Msg=data$dopplerNbMsg,
                     GPS_Altitude="",
                     GPS_Speed="",
                     GPS_Heading="",
                     Doppler_Position_ID=data$dopplerLocId,
                     Doppler_revision=data$dopplerRevision, 
                     Doppler_Altitude=data$dopplerLocAlt,
                     Doppler_device_Frequency=data$dopplerDeviceFrequency,
                     Satellite=data$kineisMetadata$sat,
                     Modulation=data$kineisMetadata$mod,
                     Signal_Level=data$kineisMetadata$level,
                     SNR=data$kineisMetadata$snr ,
                     Frequency=data$kineisMetadata$freq,
                     Message_UID=data$deviceMsgUid,check.names = F
  )
  names(newdata)=gsub('_',' ',names(newdata))
  newdata$`Message date (UTC)`=as.POSIXct(newdata$`Message date (UTC)`,tz='',format='%Y-%m-%dT%H:%M:%S')
  newdata$`Doppler Date (UTC)`=as.POSIXct(newdata$`Doppler Date (UTC)`,tz='',format='%Y-%m-%dT%H:%M:%S')
  newdata=newdata[order(newdata$`Message date (UTC)`,decreasing = T),]
  
  library(dplyr)
  #Conversions to numeric whenever possible
  force_numeric <- function(df) {
    df[] <- lapply(df, function(x) {
      num_attempt <- suppressWarnings(as.numeric(as.character(x)))
      if(all(!is.na(num_attempt))) return(num_attempt)
      else return(x)  # Keep original if conversion fails
    })
    return(df)
  }
  
  newdata <- force_numeric(newdata)
  #Corregir formato de sensores:
  convert_sensor_format <- function(input) {
    # Paso 1: Reemplazar patrón completo para limpiar
    cleaned <- gsub('": "', '=', input)
    
    # Paso 2: Eliminar TODAS las comillas dobles
    cleaned <- gsub('"', '', cleaned)
    
    # Paso 3: Limpiar espacios extra alrededor del =
    cleaned <- gsub('\\s*=', '=', cleaned)
    cleaned <- gsub('=\\s*', '=', cleaned)
    cleaned <- paste0('"', cleaned, '"')
    return(cleaned)
  }
  
  newdata$Sensors=convert_sensor_format(newdata$Sensors)
  newdata$Frequency=gsub("E[+]0*","E", sprintf("%.11E", newdata$Frequency))
  newdata$`Doppler device Frequency`=gsub("E[+]0*","E", sprintf("%.11E", newdata$`Doppler device Frequency`))
  
  return(newdata)
}
