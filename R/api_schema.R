#' @title api_schema
#' @description R6 class for handling the NAMC graphql schema
#' @return a `namc_oauth2` class (R6 class)
#' @examples
#'
#' types = ( namc_api$new(argList=...) )$get_api_types()
#' schema = api_schema$new(types = types)
#'
api_schema = R6::R6Class(
    "api_schema",
    inherit = base_class,
    portable = TRUE,

    private = list(

        #' @field raw_types raw list of endpoints and fields for the API
        types = NULL,
        endpoints = NULL,

        special_types = c("Query","Mutation","Subscription"),
        numeric_types = c("Int","Float"),
        string_types = c("Boolean","String"),
        fixed_types = c("Boolean","String","Int","Float"),
        base_types = NULL,

        Query = NULL,
        Mutation = NULL,
        Subscription = NULL,


        new_endpoint = function(...){
            return(
                modifyList(
                    list(
                        edge_name = NA,
                        has_edge  = FALSE,
                        fields    = NULL,
                        args      = NULL
                    ),
                    list(...)
                )
            )
        },

        new_field = function(...){
            return(
                modifyList(
                    list(
                        is_numeric    = FALSE,
                        is_expandable = FALSE
                    ),
                    list(...)
                )
            )
        },

        new_argument = function(...){
            return(
                modifyList(
                    list(
                        is_numeric    = FALSE,
                        is_array      = FALSE,
                        is_required   = FALSE,
                        is_for_paging = FALSE,
                        default_value = NA
                    ),
                    list(...)
                )
            )
        },



        discover_types = function(){
            # Identify base building graphql types
            private$base_types = private$types$name[grepl("__*",private$types$name)]

            # Determine query, mutation and subscription endpoints
            for(special_type in private$special_types){
                iTypes = tolower(private$types$name) == tolower(special_type)
                if( any(iTypes) ){
                    for(endpoint in private$types$fields[iTypes][[1]]$name){
                        private[[special_type]][[endpoint]] = private$new_endpoint()
                    }
                }
            }
            invisible(self)
        },



        #' Title
        #'
        #' @param arg_name
        #'
        #' @return
        #' @export
        #'
        #' @examples
        is_paging_arg = function(arg_name){
            return( any(arg_name == c(self$tpl_pagination_first, self$tpl_pagination_offset)) )
        }

    ),

    public = list(

        #' @field tpl_pagination_first is the numeric first record to return
        tpl_pagination_first = NULL,

        #' @field tpl_pagination_offset is the numeric offset of the return
        tpl_pagination_offset = NULL,

        #' @field tpl_pagination_offset is the numeric offset of the return
        tpl_pagination_cursor = NULL,

        #' @field tpl_page_fieldname is the name of the field holding page data
        tpl_page_fieldname = NULL,

        #' @field required_kind is string value of required argument kind
        required_kind = NULL,



        #' Parse the API types
        #'
        #' @return
        #'
        #' @examples
        configure = function(){
            private$discover_types()$parse_endpoints()
            invisible(self)
        },



        #' Parse introspected schema
        #'
        #' @return api_schema For method chaining
        #'
        #' @examples
        #'
        #' types = ( namc_api$new(argList=...) )$get_api_types()
        #' schema = api_schema$new(types = types)
        #' schema$parse_schema()
        #'
        parse_endpoints = function(){

            for(special_type in private$special_types){
                endpoints = names( private[[special_type]] )
                for(endpoint in endpoints){
                    self$parse_endpoint( endpoint, special_type )
                }
            }

            invisible(self)
        },



        #' Discover info on API endpoint
        #'
        #' @param endpoint Name of API endpoint to discover
        #' @param special_type Name of the type of the API endpoint
        #'
        #' @return list Info structure of API details
        #'
        #' @examples
        #'
        #' types = ( namc_api$new(argList=...) )$get_api_types()
        #' schema = api_schema$new(types = types)
        #' schema$parse_endpoint("siteInfo","Query")
        #'
        parse_endpoint = function(endpoint, special_type = NA){

            iSpecialType = private$types$name == special_type
            iEndpoint = private$types$fields[ iSpecialType ][[1]]$name == endpoint
            no_type   = is.na(private$types$fields[ iSpecialType ][[1]]$type$ofType[ iEndpoint ])
            data_type = ifelse(no_type, NA, private$types$fields[ iSpecialType ][[1]]$type$ofType$name[ iEndpoint ] )
            edge_name = NA
            has_edge  = FALSE

            # Endpoint data is contained in a nested sub-type
            if( is.na(data_type) ){
                data_type = private$types$fields[ iSpecialType ][[1]]$type$name[ iEndpoint ]
                iEdgeType = private$types$name == data_type

                # Is cursor/edge paginated data
                if( any(private$types$fields[ iEdgeType ][[1]]$name == self$tpl_pagination_cursor) ){
                    has_edge = TRUE
                    i2Type = private$types$fields[ iEdgeType ][[1]]$type$kind == "LIST"
                    edge_name = private$types$fields[ iEdgeType ][[1]]$name[ i2Type ]
                    data_type = private$types$fields[ iEdgeType ][[1]]$type$ofType$name[ i2Type ]

                # Is nested non-paged structured data
                } else if( any(private$types$fields[ iEdgeType ][[1]]$type$kind == "LIST") ){
                    i2Type = private$types$fields[ iEdgeType ][[1]]$type$kind == "LIST"
                    data_type = private$types$fields[ iEdgeType ][[1]]$type$ofType$name[ i2Type ]
                }
            }

            iDataType = private$types$name == data_type
            private[[ special_type ]][[ endpoint ]] = private$new_endpoint(
                edge_name = edge_name,
                has_edge  = has_edge
            )

            for(fieldname in private$types$fields[ iDataType ][[1]]$name){
                iField = private$types$fields[ iDataType ][[1]]$name == fieldname
                private[[ special_type ]][[ endpoint ]]$fields[[ fieldname ]] = private$new_field(
                    is_numeric    = any( private$types$fields[ iDataType ][[1]]$type$name[ iField ] == private$numeric_types ),
                    is_expandable = FALSE
                )
            }

            for(argname in private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$name){
                iArg = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$name == argname
                is_numeric = any( private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$name[ iArg ] == private$numeric_types)
                is_array = FALSE
                if( any("ofType" == names(private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type)) ){
                   # is_array = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$kind[ iArg ] == "LIST"
                #    if(is_array == TRUE){
                  #      is_numeric = any( private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$ofType$name[ iArg ] == private$numeric_types)
                 #   }
                }
                if(is.na(is_numeric)){
                    is_numeric = TRUE
                    try({is_numeric = private[[ special_type ]][[ endpoint ]]$fields[[ argname ]]$is_numeric},silent = TRUE)
                    if(is.null(is_numeric)) is_numeric = TRUE
                }
                private[[ special_type ]][[ endpoint ]]$args[[ argname ]] = private$new_argument(
                    is_numeric    = is_numeric,
                    is_array      = is_array,
                    is_required   = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$kind[ iArg ] == self$required_kind,
                    is_for_paging = private$is_paging_arg( argname ),
                    default_value = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$defaultValue[ iArg ]
                )
            }

            invisible(self)
        },



        #' Title
        #'
        #' @param endpoint
        #' @param argname
        #'
        #' @return
        #' @export
        #'
        #' @examples
        is_arg_numeric = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            if( any(argname == names(private[[ special_type ]][[ endpoint ]]$args)) ){
                return( private[[ special_type ]][[ endpoint ]]$args[[ argname ]]$is_numeric )

            } else {
                # Assume argument is numeric if no data available
                return( TRUE )
            }
        },



        #' Title
        #'
        #' @param endpoint
        #'
        #' @return
        #' @export
        #'
        #' @examples
        get_special_type_from_endpoint = function(endpoint){
            for(special_type in private$special_types){
                if( any(endpoint == names(private[[special_type]])) ){
                    return( special_type )
                }
            }
            return(NA)
        },



        #' Title
        #'
        #' @param endpoint
        #'
        #' @return
        #' @export
        #'
        #' @examples
        has_edge = function(endpoint){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$has_edge )
        },



        #' Title
        #'
        #' @param endpoint
        #'
        #' @return
        #' @export
        #'
        #' @examples
        get_edge_name = function(endpoint){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$edge_name )
        },



        #' Title
        #'
        #' @return
        #' @export
        #'
        #' @examples
        get_endpoints = function(){
            endpoints = c()
            for(special_type in private$special_types){
                endpoints = c(endpoints,names(private[[ special_type ]]))
            }
            return( endpoints )
        },



        #' Title
        #'
        #' @param endpoint
        #'
        #' @return
        #' @export
        #'
        #' @examples
        get_endpoint = function(endpoint){
            return( private[[ special_type ]][[ endpoint ]] )
        },



        #' Title
        #'
        #' @param endpoint
        #'
        #' @return
        #' @export
        #'
        #' @examples
        get_endpoint_fields = function(endpoint){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( names(private[[ special_type ]][[ endpoint ]]$fields) )
        },



        #' Title
        #'
        #' @param endpoint
        #' @param no_paging
        #'
        #' @return
        #' @export
        #'
        #' @examples
        get_endpoint_args = function(endpoint, no_paging = FALSE){
            special_type = self$get_special_type_from_endpoint( endpoint )
            paging_args = c()
            if( no_paging ){
                paging_args = c(self$tpl_pagination_first, self$tpl_pagination_offset)
            }
            return(
                setdiff(
                    names(private[[ special_type ]][[ endpoint ]]$args),
                    paging_args
                )
            )
        },



        #' Title
        #'
        #' @param endpoint
        #' @param argname
        #'
        #' @return
        #' @export
        #'
        #' @examples
        get_argument = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$args[[ argname ]] )
        },



        #' Title
        #'
        #' @param endpoint
        #' @param argname
        #'
        #' @return
        #' @export
        #'
        #' @examples
        is_argument_required = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$args[[ argname ]]$is_required )
        },



        #' Title
        #'
        #' @param endpoint
        #' @param argname
        #'
        #' @return
        #' @export
        #'
        #' @examples
        is_argument = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( any(argname == names(private[[ special_type ]][[ endpoint ]]$args)) )
        },



        #' Title
        #'
        #' @param endpoint
        #'
        #' @return
        #' @export
        #'
        #' @examples
        is_endpoint = function(endpoint){
            return( any( self$get_endpoints() == endpoint ) )
        }

    )
)