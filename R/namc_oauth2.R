#' @title namc_oauth2
#' @description R6 class for handling NAMC oAuth2
#' @return a `namc_oauth2` class (R6 class)
#' @export
#' @examples
#'
#' auth_config = list(...) # namc_oauth2 public or private variables
#' auth = namc_oauth2$new(argList = auth_config)
#'
namc_oauth2 = R6::R6Class(
    "namc_oauth2",
    inherit = base_class,
    portable = TRUE,

    private = list(

        oAuth2_appname = NULL,
        protocol = NULL,
        redirect_URL = NULL,
        authorize_path = NULL,
        access_path = NULL,
        userpool_path = NULL,
        base_path = NULL,
        base_URL = NULL,
        domain = NULL,
        clientId = NULL,
        scope = NULL,
        auto_token_refresh = NULL,
        cache_dir = NULL,
        cache_time = NULL,
        cache_timeout = NULL

    ),

    public = list(

        #' @field is_configured shows the configured state of the namc_oauth2 object
        is_configured = FALSE,


        #' Set connection details
        #'
        #' Sets the clientID and URL for the authentication provider
        #'
        #' @param clientId string The auth clientID.
        #' @param domain string The auth domain name.
        #'
        #' @return namc_oauth2 An R6 class.
        #'
        #' @examples
        #'
        #' auth_config = list(...) # namc_oauth2 public or private variables
        #' auth = namc_oauth2$new(argList = auth_config)
        #' auth$set_connection_details("XXXXXXXXXX", "namc.XXXXXXX.edu")
        #'
        set_connection_details = function(clientId, domain){

            private$domain   = domain
            private$clientId = clientId
            private$base_URL = paste0( private$protocol, domain, '/', private$base_path )

            cache = gsub('.httr-oauth','',private$cache_dir)
            if(cache != '' && !dir.exists(cache)){
                dir.create(cache, recursive = TRUE)
            }

            self$is_configured = TRUE

            private$cache_time = Sys.time()
            private$cache_timeout = 0

            invisible(self)
        },



        #' Get user info
        #'
        #' Gets the user information from the auth provider once authenticated.
        #'
        #' @return list A list containing the available user data.
        #'
        #' @examples
        #'
        #' auth_config = list(...) # namc_oauth2 public or private variables
        #' auth = namc_oauth2$new(argList = auth_config)
        #' user_info = auth$get_user_info()
        #'
        get_user_info = function(){

            userpool_URL = paste0( private$base_URL, '/', private$userpool_path )

            req = httr::GET(
                url    = userpool_URL,
                httr::add_headers(
                    Authorization = paste("Bearer", self$get_access_token())
                )

                #config = httr::config(
                #    token = self$get_token()
                #)
            )

            return( jsonlite::fromJSON( httr::content(req, "text") ) )
        },



        #' Get oAuth2 token
        #'
        #' Gets and oAuth2 token object which can be used for subsequent connections.
        #' A stop error occurs if this authentication object has not been configured via
        #' set_connection_details().
        #'
        #' @param auto_refresh Logical TRUE/FALSE to override the objects default behavior
        #'
        #' @return list An oAuth2 token structure from httr::oauth2.0_token
        #'
        #' @examples
        #'
        #' auth_config = list(...) # namc_oauth2 public or private variables
        #' auth = namc_oauth2$new(argList = auth_config)
        #' oauth_token = auth$get_token()
        #'
        get_token = function(auto_refresh = TRUE){
            if( !self$is_configured ) {
                stop("Authentication address and parameters have not been configured.")
            }

            if( auto_refresh && private$auto_token_refresh && self$requires_refresh() ){
                self$refresh_token()
            }

            return(
                httr::oauth2.0_token(
                    endpoint = httr::oauth_endpoint(
                        authorize = private$authorize_path,
                        access    = private$access_path,
                        base_url  = private$base_URL
                    ),
                    app = httr::oauth_app(
                        appname      = private$oAuth2_appname,
                        key          = private$clientId,
                        secret       = NULL,
                        redirect_uri = private$redirect_URL
                    ),
                    scope = private$scope,
                    cache = private$cache_dir
                )
            )

        },



        #' Get access token
        #'
        #' Extracts the access token from the cached oAuth2 credential. This is
        #' used for the Bearer Authentication header.
        #'
        #' @return string The oAuth2 access token
        #'
        #' @examples
        #'
        #' auth_config = list(...) # namc_oauth2 public or private variables
        #' auth = namc_oauth2$new(argList = auth_config)
        #' oauth_access_token = auth$get_access_token()
        #'
        get_access_token = function(){
            return( ( self$get_token() )$credentials$access_token )
        },



        #' Check refresh token
        #'
        #' Check if the oAuth2 token needs refreshing
        #'
        #' @return namc_oauth2 An R6 class.
        #'
        #' @examples
        #'
        #' auth_config = list(...) # namc_oauth2 public or private variables
        #' auth = namc_oauth2$new(argList = auth_config)
        #' auth$requires_refresh()
        #'
        requires_refresh = function(){
            time_since_refresh = as.numeric( difftime( Sys.time(), private$cache_time, units="sec" ) )
            return( time_since_refresh >= private$cache_timeout )
        },



        #' Refresh token
        #'
        #' Refreshes the oAuth2 token
        #'
        #' @return namc_oauth2 An R6 class.
        #'
        #' @examples
        #'
        #' auth_config = list(...) # namc_oauth2 public or private variables
        #' auth = namc_oauth2$new(argList = auth_config)
        #' auth$refresh_token()
        #'
        refresh_token = function(){
            token = self$get_token(auto_refresh = FALSE)
            token$refresh()
            private$cache_time = Sys.time()
            private$cache_timeout = token$credentials$expires_in

            invisible(self)
        },



        #' Reset token
        #'
        #' Deletes cached token file .httr-oauth
        #'
        #' @return namc_oauth2 An R6 class.
        #'
        #' @examples
        #'
        #' auth_config = list(...) # namc_oauth2 public or private variables
        #' auth = namc_oauth2$new(argList = auth_config)
        #' auth$reset_token()
        #'
        reset_token = function(){
            file.remove( private$cache_dir )

            invisible(self)
        }

    )
)
