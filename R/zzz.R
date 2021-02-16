#' Title
#'
#' @param libnam
#' @param pkgname
#'
#' @return
#' @export
#'
#' @examples
.onLoad = function(libnam, pkgname) {

    #c = yaml::yaml.load_file("./data-raw/config.yml")

    c = list(
        user = list(
            display_messages = TRUE,
            is.deployed = FALSE,
            is.mode_test = FALSE,
            manage_credentials = TRUE,
            return_type = 'R',
            credential_store = 'vault',
            path.credential_store = '~/R/.secrets/',
            path.credential_store_key = '~/R/.secrets/'
        ),
        api =list(
            is_initialized = FALSE,
            URL = 'https://54w15tgigd.execute-api.us-west-2.amazonaws.com/production/api',
            pagination_limit = 100,
            top_level_key = 'data',
            top_level_key_error = 'errors',
            tpl_pagination_first = 'limit:|val|',
            tpl_pagination_offset = ' nextToken:|val|'
        ),
        auth =list(
            oAuth2_appname = 'NAMCr',
            IAM_protocol = 'https://',
            IAM_redirect_URL = 'http://localhost:1410/',
            IAM_authorize_path = 'authorize',
            IAM_access_path = 'token',
            IAM_userpool_path = 'userInfo',
            IAM_base_path = 'oauth2',
            IAM_base_URL = '',
            IAM_clientId = '',
            IAM_scope =c('openid','email'),
            auto_token_refresh = TRUE,
            cache_dir = './'
        )

    )
    c$api$.auth = namc_oauth2$new( argList = c$auth )

    pkg.globals$api <<- namc_api$new( argList = c$api )

}