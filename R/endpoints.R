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
    return( .pkgenv$api$get_endpoints() )
}



#' Retrieve fields
#'
#' Retrieves the fields associated with an API endpoint from the schema
#'
#' @param api_endpoint The name of an API endpoint.
#'
#' @return vector A character vector of fields.
#'
#' @examples
#'
#' fields = NAMCr::get_endpoint_fields( api_endpoint = "sites" )
#'
get_endpoint_fields = function(api_endpoint){
    return( .pkgenv$api$get_endpoint_fields(api_endpoint) )
}



#' Retrieve fields
#'
#' Retrieves the fields associated with an API endpoint from the schema
#'
#' @param api_endpoint The name of an API endpoint.
#'
#' @return vector A character vector of arguments.
#'
#' @examples
#'
#' args = NAMCr::get_endpoint_args( api_endpoint = "sites" )
#'
get_endpoint_args = function(api_endpoint){
    return( .pkgenv$api$get_endpoint_args(api_endpoint) )
}