#' Get data from _NatureDataCube_/_AgroDataCube_
#'
#' Wrapper to get data from _NatureDataCube_/_AgroDataCube_ through their REST API.
#'
#' @import httr
#' @param url character. Request URL.
#' @param option character. Determines the type of request.
#' @param params vector or list. List of named parameters.
#' @param token character. API token.
#' @returns A request response list.
#' @export

library(httr)

ndc_get <- function(url, option, params, token) {

  # Compose request URL
  if (missing(url) && !missing(option) && !missing(params)) {
    request_url <- ndc_url(option = option, params = params)
  } else {
    request_url <- url
  }
    
  # Compose request headers
  request_headers <- c("Accept" = "application/json;charset=utf-8",
                       "token"  = token)

  # Submit request
  response <- content(VERB("GET", url = request_url, add_headers(request_headers)))
  
  return(response)
}
