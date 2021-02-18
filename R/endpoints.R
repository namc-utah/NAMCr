#' Retrieve API endpoints
#'
#' Retrieves the API endpoint names from the schema
#'
#' @return vector A character vector.
#'
#' @examples
#'
#' endpoints = NAMCr::get_endpoints()
#'
get_endpoints = function(){
    return( pkg.globals$api$get_endpoints() )
}



#' Retrieve fields
#'
#' Retrieves the fields associated with an API endpoint from the schema
#'
#' @param api_endpoint The name of an API endpoint.
#'
#' @return vector A character vector.
#'
#' @examples
#'
#' endpoints = NAMCr::get_endpoints( api_endpoint = "sites" )
#'
get_endpoint_fields = function(api_endpoint){
    return( pkg.globals$api$get_endpoints(api_endpoint) )
}