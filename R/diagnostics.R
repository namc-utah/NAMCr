#' Diagnose connection issues
#'
#' @description
#' `diagnose_connection` checks for connectivity of the internet, API, and authentication service
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' NAMCr::diagnose_connection()
#'
diagnose_connection = function(){
    if(!r_is_online()){
        message("R has no internet connection.")
    } else if(!api_is_online()){
        message("API connection is down.")
    } else if(!oauth_is_online()){
        message("oAuth2 provider is not accessible.")
    } else {
        message("Internet, API and oAuth2 provider are all available and online.")
    }
    return()
}



#' oAuth2 provider check
#'
#' @return boolean
#'
oauth_is_online = function(){

    auth = .pkgenv$api$get_auth_provider()
    test_url = paste0( auth$get_var("base_URL"), '/', auth$get_var("userpool_path") )

    return( site_is_online( test_url ) )
}



#' API connectivity check
#'
#' @return logical
#'
api_is_online = function(){
    # Verify that the api address returns a forbidden status code
    return( site_is_online( .pkgenv$api$URL ) )
}



#' API connectivity check
#'
#' @param site_url is a url to check if it is accessible
#'
#' @return logical
#'
site_is_online = function(site_url){
    tryCatch({
        isOnline = is.numeric(httr::GET(site_url)$status_code)
    }, error = function(e){
        isOnline <<- FALSE
    })
    return( isOnline )
}



#' R internet check
#'
#' @return locical
#'
r_is_online = function(){
    tryCatch({
        is_online = curl::has_internet()
    }, error = function(cond){
        is_online = FALSE
    })
    return( is_online )
}