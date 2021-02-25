#' @title namc_api
#' @description R6 class for handling NAMC GraphQL queries
#' @return a `namc_api` class (R6 class)
#' @examples
#'
#' api_config = list(...) # namc_api public or private variables
#' api = namc_api$new(argList = api_config)
#'
namc_api = R6::R6Class(
    "namc_api",
    inherit = base_class,
    portable = TRUE,

    private = list(

        #' @field auth holds an oAuth2 object for authentication
        auth = NULL,

        #' @field retry_conn is a boolean to allow for a query reattempt given an API timeout
        retry_conn = TRUE

    ),

    public = list(

        #' @field is_configured shows the configured state of the namc_api object
        is_configured = FALSE,

        #' @field URL is the API URL endpoint
        URL = NULL,

        #' @field top_level_key is the top level key returned in the graphql JSON return for data
        top_level_key = NULL,

        #' @field top_level_key_error is the top level key returned in the graphql JSON return for errors
        top_level_key_error = NULL,

        #' @field schema is a list of endpoints and fields for the API
        schema = NULL,

        #' @field pagination_limit is the max number of records returned without pagination
        pagination_limit = NULL,

        #' @field tpl_pagination_first is the numeric first record to return
        tpl_pagination_first = NULL,

        #' @field tpl_pagination_offset is the numeric offset of the return
        tpl_pagination_offset = NULL,

        #' @field tpl_pagination_offset is the numeric offset of the return
        tpl_pagination_cursor = NULL,

        #' @field required_kind is string value of required argument kind
        required_kind = NULL,



        #' Configure parameters
        #'
        #' Retrieves authentication and schema information via unauthenticated API queries
        #'
        #' @param force is a boolean TRUE/FALSE to force a reconfiguration
        #'
        #' @return namc_api An R6 class.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' api$configure()
        #'
        configure = function(force = FALSE){

            if( !self$is_configured || force ){

                auth = self$build_schema()$get_auth_info()
                private$auth$set_connection_details( auth$clientId, auth$domain )

                self$is_configured = TRUE
            }

            invisible(self)
        },



        #' Graphql client retrieval
        #'
        #' Configures a graphql client with proper authentication and connection settings
        #'
        #' @param authenticate boolean A logical TRUE/FALSE representing the required authentication state.
        #'
        #' @return ghql::GraphqlClient A preconfigured graphql R6 class.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' api$get_connection(authenticate = TRUE)
        #'
        get_connection = function(authenticate = TRUE){
            return(
                ghql::GraphqlClient$new(
                    url     = self$URL,
                    headers = list(
                        Authorization = paste0(
                            "Bearer ",
                            ifelse(
                                test = authenticate,
                                yes  = private$auth$get_access_token(),
                                no   = ""
                            )
                        )
                    )
                )
            )
        },



        #' Introspect the API schema type
        #'
        #' Retrieves the raw schema return from introspecting the API schema type
        #'
        #' @return namc_api An R6 class.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' api$get_api_types()
        #'
        get_api_types = function(){

            # Retrieve schema via introspection
            return(
                ( self$query(
                    '{
                        __schema {
                            types {
                                name
                                kind
                                ofType {
                                    name
                                }
                                fields {
                                    name
                                    description
                                    args {
                                        name
                                        defaultValue
                                        description
                                        type {
                                            name
                                            kind
                                            ofType{
                                                name
                                                kind
                                                ofType{
                                                    name
                                                    kind
                                                    ofType{
                                                        name
                                                        kind
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    type {
                                        name
                                        kind
                                        ofType {
                                            name
                                        }
                                    }
                                }
                            }
                        }
                    }',
                    authenticate = FALSE
                ) )[['__schema']]$types
            )

        },



        #' Builds a graphql schema
        #'
        #' Builds a graphql schema structure via introspection of the API.
        #' For all Query and Mutation graphql types the built structure stores
        #' all associated fields and dependent sub-types. This is used for the
        #' automated query builder as a convenience function.
        #'
        #' @return namc_api An R6 class.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' api$build_schema()
        #' schema = api$schema
        #'
        build_schema = function(){

            # Retrieve schema via introspection
            types = self$get_api_types()
            self$schema$set_var("types", types)$configure()

            invisible(self)
        },



        #' Query the API
        #'
        #' Executes a given graphql query against the API. If errors are returned execution is stopped.
        #'
        #'
        #' @param query string Text containing a graphql query
        #' @param authenticate boolean A logical TRUE/FALSE representing the required authentication state.
        #' @param name string A name for the query.
        #'
        #' @return data.frame A dataframe contained the query result.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' data = api$query("graphql_query", ...)
        #'
        query = function(query, authenticate = TRUE, name = 'query'){

            if( !self$is_configured && authenticate ) self$configure()

            # Define graphQL connection properties
            con = self$get_connection( authenticate )

            # Define query
            qry = ghql::Query$new()
            qry$query( name, query )

            # Execute query
            tryCatch({

                res = jsonlite::fromJSON(
                    txt = con$exec( qry$queries[[ name ]] )
                )

            }, error = function(e){
                if( grepl("HTTP 504", e ) && private$retry_conn) {
                    private$retry_conn = FALSE
                    message("Connection timed out. Reattempting request...")
                    return( self$query( query, authenticate, name ) )

                } else if(!private$retry_conn) {
                    stop("Reattempt failed. Service is down. Please try again later.", call. = FALSE)

                } else {
                    stop("QUERY ERROR", call. = FALSE )

                }

            }, finally = {
                private$retry_conn = TRUE
            })

            if( self$top_level_key_error %in% names(res) ) {
                stop( paste0("Query Execution Error:\n\t", res[[ self$top_level_key_error ]]$message), call. = FALSE )
            }

            return( res[[ self$top_level_key ]] )
        },



        #' Authentication information retrieval
        #'
        #' Retrieves the clientId and domain required for authentication from the auth
        #' graphql endpoint.
        #'
        #' @return list A list containing the clientId and domain required for authentication.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' auth = api$get_auth_info()
        #' clientId = auth$clientId
        #' domain = auth$domain
        #'
        get_auth_info = function(){

            # Grab authentication data
            qry = self$query(
                '{
                    auth {
                      clientId
                      domain
                    }
                }',
                authenticate = FALSE
            )

            return( qry$auth )
        },



        #' Get the authentication object
        #'
        #' Access method for the oAuth2 authentication object stored within this namc_api object.
        #'
        #' @return namc_api An R6 authentication class.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' auth = api$get_auth_provider()
        #'
        get_auth_provider = function(){
            return( private$auth )
        },



        #' Authentication check
        #'
        #' Provides a logical depicting the state of the current authentication
        #'
        #' @return logical A logical TRUE/FALSE.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' authenticated = api$is_authenticated()
        #'
        is_authenticated = function(){

            # Grab authentication data
            qry = self$query(
                '{
                    auth {
                      loggedIn
                    }
                }'
            )

            return( qry$auth$loggedIn )
        },



        #' Retrieve API endpoints
        #'
        #' Retrieves the API endpoint names from the schema
        #'
        #' @return vector A character vector.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' endpoints = api$get_endpoints()
        #'
        get_endpoints = function(){

            if( !self$is_configured ) self$configure()

            return( self$schema$get_endpoints() )
        },



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
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' api$get_endpoint_fields(api_endpoint = 'sites')
        #'
        get_endpoint_fields = function(api_endpoint){

            if( !self$is_configured ) self$configure()

            return( self$schema$get_endpoint_fields(special_type, api_endpoint) )
        },



        #' Retrieve arguments
        #'
        #' Retrieves the arguments associated with an API endpoint from the schema
        #'
        #' @param api_endpoint The name of an API endpoint.
        #'
        #' @return vector A character vector of arguments.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' api$get_endpoint_args(api_endpoint = 'sites')
        #'
        get_endpoint_args = function(api_endpoint, no_paging = FALSE){

            if( !self$is_configured ) self$configure()

            return( self$schema$get_endpoint_args(api_endpoint, no_paging) )
        },



        #' Retrieve fields
        #'
        #' Retrieves the fields associated with an API endpoint from the schema
        #'
        #' @param api_endpoint The name of an API endpoint.
        #'
        #' @return vector A character vector.
        #'
        #' @examples
        #'
        #' api_config = list(...) # namc_api public or private variables
        #' api = namc_api$new(argList = api_config)
        #' api$get_endpoint_fields(api_endpoint = 'sites')
        #'
        is_endpoint = function(api_endpoint, stop_if_not = FALSE){

            if( !self$is_configured ) self$configure()

            is_endpoint = self$schema$is_endpoint(endpoint = api_endpoint )

            if(stop_if_not && !is_endpoint){
                endpoints = paste( self$schema$get_endpoints(), collapse = ' ' )
                msg = paste0(
                    '"',api_endpoint,'" is not a valid endpoint. The endpoints available are:\n\t',
                    endpoints
                )
                stop( msg, call. = FALSE )
            }

            return( is_endpoint )
        }

    )
)

