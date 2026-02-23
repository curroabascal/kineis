#' Function to check is credentials are stored in a file.
#' It also provides the access token
#' If not, it will require credentials and ask if you want to save them.
#' @export
#' @examples
#' myaccesstoken=.check_credentials()
#' myaccesstoken
get_home <- function() {
  if (.Platform$OS.type == "windows") {
    Sys.getenv("USERPROFILE")
  } else {
    Sys.getenv("HOME")
  }
}

check_credentials <-function() {
  credexist<-file.exists(file.path(normalizePath(get_home()),'kineiscredentials.txt')->kineiscredentialsfile)
  
  if (credexist){
    print('Reading stored credentials')
    jnk=readLines(kineiscredentialsfile)
    Sys.setenv(kineis_user=jnk[1],kineis_pass=jnk[2])
  } else {
    print('No credentials stored to access CLS environmental services. Do you want to insert them?')
    cat("1. Yes\n")
    cat("2. No")
    opcion <- readline("Choose an option (1-2): ")
    
    if (opcion==1){
      kineis_user <- readline("CLS user: ")
      kineis_pass <- readline("CLS password: ")
      Sys.setenv(kineis_user=kineis_user,kineis_pass=kineis_pass)
      
      opcion2 <- readline("Do you want to store the credentials? (y/n):")
      if (opcion2=='y'){
        writeLines(paste(kineis_user,kineis_pass,sep='\n'),con=kineiscredentialsfile)
      }
    } else stop('No credentials to access CLS environmental service',call.=F)
    }
  
  myloadpackage(c('httr','jsonlite'))
  AUTH_URL <- "https://account.groupcls.com/auth/realms/cls/protocol/openid-connect/token"
  API_TELEMETRY_URL <- "https://api.groupcls.com/telemetry/api/v1"
  
  # Get token
  token_response <- POST(AUTH_URL,
                         body = list(grant_type = "password", client_id = "api-telemetry",
                                     username = Sys.getenv('kineis_user'), password = Sys.getenv('kineis_pass')),
                         encode = "form")
  
  if (status_code(token_response) != 200) {
  print('It was not possible to connect to CLS server. Do you want to insert new credentials?')
  cat("1. Yes\n")
  cat("2. No")
  opcion <- readline("Choose an option (1-2): ")
  
    if (opcion==1){
      kineis_user <- readline("CLS user: ")
      kineis_pass <- readline("CLS password: ")
      Sys.setenv(kineis_user=kineis_user,kineis_pass=kineis_pass)
      
      opcion2 <- readline("Do you want to store the credentials? (y/n):")
      if (opcion2=='y'){
        writeLines(paste(kineis_user,kineis_pass,sep='\n'),con=kineiscredentialsfile)
      } else stop('No credentials to access the service',call.=F)
    }
  }
  access_token <- content(token_response)$access_token
  access_token
}

  
  
