#' Reinitiate the package state
#'
#' @return namc_api Package configured API object.
#' @export
#'
reinitiate = function(clear_credentials = FALSE){
    if(clear_credentials) pkg.globals$api$get_auth_provider()$reset_token()

    return( pkg.globals$api$configure() )
}

#' Execute public API class method
#'
#' @param fn_name Name of namc_api public method
#'
#' @return variable Depends on function executed. See public methods of namc_api
#' @export
#'
execute_api_fn = function(fn_name){
    return( pkg.globals$api[[fn_name]]() )
}



#' Get API class variable
#'
#' @param var_name Name of namc_api variable name
#'
#' @return variable Depends on variable requested. See variables of namc_api
#' @export
#'
get_api_var = function(var_name){
    return( pkg.globals$api$get_var(var_name) )
}



#' Diagnose connection issues
#'
#' @return none No return
#' @export
#'
#' @examples
#'
#' NAMCr::diagnose_connection()
#'
diagnose_connection = function(){
    if(!r_is_online){
        message("R has no internet connection.")
    } else if(!api_is_online){
        message("API connection is down.")
    } else if(!oauth_is_online){
        message("oAuth2 provider is not accessible.")
    } else {
        message("Everything is connected and accessible.")
    }
}



#' oAuth2 provider check
#'
#' @return boolean
#' @export
#'
oauth_is_online = function(){

    user_info = pkg.globals$api$get_auth_provider()$get_user_info()

    return( user_info$status_code < 300 )
}



#' API connectivity check
#'
#' @return boolean
#' @export
#'
api_is_online = function(){
    # Verify that the api address returns a forbidden status code
    return( httr::GET( config$api$URL )$status_code == 403 )
}



#' R internet check
#'
#' @return boolean
#' @export
#'
r_is_online = function(){
    tryCatch({
        is_online = curl::has_internet()
    }, error = function(cond){
        is_online = FALSE
    })
    return( is_online )
}