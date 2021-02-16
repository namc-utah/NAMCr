# # Get oAuth2 Token
# test_token = httr::oauth2.0_token(
#     endpoint = httr::oauth_endpoint(
#         authorize = 'authorize',
#         access = 'token',
#         base_url = 'https://namc-production.auth.us-west-2.amazoncognito.com/oauth2'
#     ),
#     app = httr::oauth_app(
#         appname = 'namcAPIr',
#         key = '5s0ugmce2cl3g38j0uerkgts1k',
#         secret = NULL,
#         redirect_uri = 'http://localhost:1410/'
#     ),
#     scope = c('openid','email')
# )
#
# # Define graphQL connection properties
# con = ghql::GraphqlClient$new(
#     url = 'https://54w15tgigd.execute-api.us-west-2.amazonaws.com/production/api',
#     headers = list(Authorization = paste0("Bearer "))
# )
#
# # Get schema definition
# # schema = con$load_schema()
#
# # Basic graphQL query
# qry = ghql::Query$new()
# qry$query('getInfo','{
#   auth {
#     loggedIn
#     clientId
#     domain
#     userPool
#   }
# }
# ')
#
# res = con$exec(qry$queries$getInfo)
#
# rData = jsonlite::fromJSON(res)