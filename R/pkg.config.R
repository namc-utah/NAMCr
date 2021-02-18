#' Get API parameters
#'
#' Returns a nested list of high-level parameters required for making the API and authentication calls.
#'
#' @return list A list of API and authentication parameters
#'
pkg.config = function(){

    return(

        list(
            user = list(
                display_messages          = TRUE,
                is.deployed               = FALSE,
                is.mode_test              = FALSE,
                manage_credentials        = TRUE,
                return_type               = 'R',
                credential_store          = 'vault',
                path.credential_store     = '~/R/.secrets/',
                path.credential_store_key = '~/R/.secrets/'
            ),
            api = list(
                URL                   = 'https://54w15tgigd.execute-api.us-west-2.amazonaws.com/production/api',
                pagination_limit      = 100,
                top_level_key         = 'data',
                top_level_key_error   = 'errors',
                tpl_pagination_first  = 'limit',
                tpl_pagination_offset = 'nextToken'
            ),
            auth = list(
                oAuth2_appname     = 'NAMCr',
                IAM_protocol       = 'https://',
                IAM_redirect_URL   = 'http://localhost:1410/',
                IAM_authorize_path = 'authorize',
                IAM_access_path    = 'token',
                IAM_userpool_path  = 'userInfo',
                IAM_base_path      = 'oauth2',
                IAM_base_URL       = '',
                IAM_clientId       = '',
                IAM_scope          = c('openid','email'),
                auto_token_refresh = TRUE,
                cache_dir          = '~/.secrets/R/NAMCr/.httr-oauth'
            )
        )
    )
}