#' Describe the NAMC API.
#'
#' @description `info` prints text to the console describing the NAMC API
#'   options.
#'
#' @details This function presents a text block describing the available API
#'   endpoints and their corresponding input parameters. Inputs are marked as
#'   required where applicable. Endpoint descriptions can be subset by providing
#'   an endpoint name as input.
#' `r info(format='markdown', to_console=FALSE)`
#'
#' @param endpoint api endpoint name to get information about.
#' @param no_paging Logical that omits the paging arguments by default
#' @param format One of: 'text (default), 'markdown'
#' @param to_console Logical TRUE(default), FALSE to print output to console
#'
#' @return If `format` argument is console nothing is returned otherwise text is
#'   returned in the format defined.
#'
#' @seealso `browseVignettes("NAMC-API-Overview")`
#' @export
#'
#' @examples
#'
#' info()
#'
info = function(endpoint = NA, no_paging = TRUE, format = "text", to_console = TRUE){
    api = .pkgenv$api$configure()
    schema = api$schema

    tpl = 'NAMC API:\n'
    if(is.na(endpoint)){
        for(e in endpoints()){
            tpl = paste0(tpl, schema$info(e, no_paging, format))
        }
    } else {
        tpl = paste0(tpl, schema$info(endpoint, no_paging, format))

    }
    if( to_console ){
        cat(tpl)
        return()

    } else if( format == "text" ){
        return(tpl)

    } else if( format == "markdown" ){
        return(tpl)

    }
}



#' View API vignett
#'
#' @export
#'
#' @examples
#'
#' NAMCr::docs()
#'
docs = function(){
    browseVignettes("NAMCr")
}


#' List all api endpoints
#'
#' @return A character vector of endpoint names
#' @export
#'
#' @examples
#'
#' endpoints()
#'
endpoints = function(){
    return( .pkgenv$api$get_endpoints() )
}



#' List all endpoint fields
#'
#' @description
#' `endpoint_fields` retrieves the fields associated with an API endpoint from the schema#'
#'
#' @param api_endpoint The name of an API endpoint.
#'
#' @return A character vector of field names.
#'
#' @examples
#'
#' endpoint_fields( "sites" )
#'
endpoint_fields = function(api_endpoint){
    return( .pkgenv$api$get_endpoint_fields(api_endpoint) )
}



#' List all endpoint parameters
#'
#' @description
#' `endpoint_parameters` returns a vector of parameters associated with an API endpoint from the schema
#'
#' @param api_endpoint The name of an API endpoint.
#' @param no_paging Logical that omits the paging arguments by default
#'
#' @return A character vector of parameters.
#'
#' @examples
#'
#' endpoint_parameters( "sites" )
#'
endpoint_parameters = function(api_endpoint, no_paging = FALSE){
    return( .pkgenv$api$get_endpoint_args(api_endpoint, no_paging) )
}


