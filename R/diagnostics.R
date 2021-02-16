#' Title
#'
#' @return
#' @export
#'
#' @examples
init = function(){
    return( pkg.globals$api$configure() )
}

#' Title
#'
#' @param fn_name
#'
#' @return
#' @export
#'
#' @examples
execute_public_fn = function(fn_name){
    return( pkg.globals$api[[fn_name]]() )
}



#' Title
#'
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
get_public_var = function(var_name){
    return( pkg.globals$api[[var_name]] )
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
oauth_is_online = function(){

    user_info = pkg.globals$api$get_auth_provider()$get_user_info()

    return( user_info$status_code < 300 )
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
api_is_online = function(){
    # Verify that the api address returns a forbidden status code
    return( httr::GET( config$api$URL )$status_code == 403 )
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
r_is_online = function(){
    tryCatch({
        is_online = curl::has_internet()
    }, error = function(cond){
        is_online = FALSE
    })
    return( is_online )
}