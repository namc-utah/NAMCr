#' @title namc_api
#' @description R6 class for handling NAMC GraphQL queries
#' @return a `namc_api` class (R6 class)
#' @export
#' @examples
namc_api = R6::R6Class(
    "namc_api",
    inherit = base_class,
    portable = TRUE,

    private = list(

        .auth = NULL

    ),

    public = list(

        is_configured = FALSE,
        URL = NULL,
        top_level_key = NULL,
        top_level_key_error = NULL,
        schema = NULL,
        pagination_limit = NULL,
        tpl_pagination_first = NULL,
        tpl_pagination_offset = NULL,



        #' Configure parameters
        #'
        #' Retrieves authentication and schema information via unauthenticated API queries
        #'
        #' @return namc_api An R6 class.
        #'
        #' @examples
        configure = function(){

            auth = self$build_schema()$get_auth_info()
            private$.auth$set_connection_details( auth$clientId, auth$domain )

            self$is_configured = TRUE

            invisible(self)
        },



        #' Graphql client retrieval
        #'
        #' Configures a graphql client with proper authentication and connection settings
        #'
        #' @return ghql::GraphqlClient A preconfigured graphql R6 class.
        #'
        #' @examples
        get_connection = function(authenticate = TRUE){
            return(
                ghql::GraphqlClient$new(
                    url     = self$URL,
                    headers = list(
                        Authorization = paste0("Bearer ",
                            ifelse(
                                test = authenticate,
                                yes  = private$.auth$get_access_token(),
                                no   = ""
                            )
                        )
                    )
                )
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
        build_schema = function(){

            # Grab authentication data
            qry = self$query(
                '{
                    __schema {
                      types {
                        name
                        kind
                        fields {
                          name
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
            )

            self$schema = list()
            iEndpoints = qry[['__schema']]$types$name == "Query"
            endpoints = qry[['__schema']]$types$fields[ iEndpoints ][[1]]$name

            for(endpoint in endpoints){
                iEndpoint = qry[['__schema']]$types$fields[ iEndpoints ][[1]]$name == endpoint
                eType = qry[['__schema']]$types$fields[ iEndpoints ][[1]]$type$ofType$name[ iEndpoint ]
                # If endpoint is of a special sub-type
                if( is.na(eType) ){
                    fType = qry[['__schema']]$types$fields[ iEndpoints ][[1]]$type$name[ iEndpoint ]
                    iType = qry[['__schema']]$types$name == fType
                    if( all(qry[['__schema']]$types$fields[ iType ][[1]]$type$kind == "SCALAR") ){
                        eType = fType
                    } else {
                        #if( any(qry[['__schema']]$types$fields[ iType ][[1]]$name == "records") ){
                        #    i2Type = qry[['__schema']]$types$fields[ iType ][[1]]$name == "records"
                        #    fType = qry[['__schema']]$types$fields[ i2Type ][[1]]$ofType$name[ i2Type ]
                        #    iType = qry[['__schema']]$types$name == fType
                        #}
                        eType = qry[['__schema']]$types$fields[ iType ][[1]]$type$ofType$name[ iType ]

                    }
                } else {
                    fType = NA
                }
                iType = qry[['__schema']]$types$name == eType
                self$schema[[endpoint]] = list(
                    grouping = fType,
                    fields = qry[['__schema']]$types$fields[ iType ][[1]]$name
                )
            }

            invisible(self)
        },



        #' Query the API
        #'
        #' Executes a given graphql query against the API. If errors are returned execution is stopped.
        #'
        #'
        #' @return data.frame A dataframe contained the query result.
        #'
        #' @examples
        query = function(query, load_all = TRUE, authenticate = TRUE, name = 'query'){

            if( !self$is_configured && authenticate ) self$configure()

            # Define graphQL connection properties
            con = self$get_connection( authenticate )

            # Define query
            qry = ghql::Query$new()
            qry$query( name, query )

            # Execute query
            res = jsonlite::fromJSON(
                txt = con$exec( qry$queries[[ name ]] )
            )

            if( self$top_level_key_error %in% names(res) ) {
                stop( paste0("Query Execution Error:\n\t", res[[ self$top_level_key_error ]]$message) )
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
        #' @return namc_oauth2 An R6 authentication class.
        #'
        #' @examples
        get_auth_provider = function(){
            return( private$.auth )
        },



        #' Authentication check
        #'
        #' Provides a logical depicting the state of the current authentication
        #'
        #' @return logical A logical TRUE/FALSE.
        #'
        #' @examples
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
        get_endpoints = function(){

            if( !self$is_configured ) self$configure()

            return( names(self$schema) )
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
        get_endpoint_fields = function(api_endpoint){

            if( !self$is_configured ) self$configure()

            return( self$schema[[ api_endpoint ]]$fields )
        }

    )
)

