#' @title namc_oauth2
#' @description R6 class for handling NAMC oAuth2
#' @return a `namc_oauth2` class (R6 class)
#' @export
#' @examples
namc_oauth2 = R6::R6Class(
    "namc_oauth2",
    inherit = base_class,
    portable = TRUE,

    private = list(

        oAuth2_appname = NULL,
        IAM_protocol = NULL,
        IAM_redirect_URL = NULL,
        IAM_authorize_path = NULL,
        IAM_access_path = NULL,
        IAM_userpool_path = NULL,
        IAM_base_path = NULL,
        IAM_base_URL = NULL,
        IAM_domain = NULL,
        IAM_clientId = NULL,
        IAM_scope = NULL,
        auto_token_refresh = NULL,
        cache_dir = NULL

    ),

    public = list(

        is_configured = FALSE,


        #' Set connection details
        #'
        #' Sets the clientID and URL for the authentication provider
        #'
        #' @param IAM_clientId string The auth clientID.
        #' @param IAM_domain string The auth domain name.
        #'
        #' @return namc_oauth2 An R6 class.
        #'
        #' @examples
        set_connection_details = function(IAM_clientId, IAM_domain){

            private$IAM_domain   = IAM_domain
            private$IAM_clientId = IAM_clientId
            private$IAM_base_URL = paste0( private$IAM_protocol, IAM_domain, '/', private$IAM_base_path )

            self$is_configured = TRUE

            invisible(self)
        },



        #' Get user info
        #'
        #' Gets the user information from the auth provider once authenticated.
        #'
        #' @return list A list containing the available user data.
        #'
        #' @examples
        get_user_info = function(){

            userpool_URL = paste0( private$IAM_base_URL, '/', private$IAM_userpool_path )

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
        #' @return httr::oauth2.0_token An oAuth2 token object
        #'
        #' @examples
        get_token = function(){
            if( !self$is_configured ) {
                stop("Authentication address and parameters have not been configured.")
            }

            return(
                httr::oauth2.0_token(
                    endpoint = httr::oauth_endpoint(
                        authorize = private$IAM_authorize_path,
                        access    = private$IAM_access_path,
                        base_url  = private$IAM_base_URL
                    ),
                    app = httr::oauth_app(
                        appname      = private$oAuth2_appname,
                        key          = private$IAM_clientId,
                        secret       = NULL,
                        redirect_uri = private$IAM_redirect_URL
                    ),
                    scope = private$IAM_scope
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
        get_access_token = function(){
            return( ( self$get_token() )$credentials$access_token )
        },



        #' Reset token
        #'
        #' Deletes cached tokens.
        #'
        #' @return namc_oauth2 An R6 class.
        #'
        #' @examples
        reset_token = function(){
            file.remove('./.httr-oauth')

            invisible(self)
        }

    )
)
