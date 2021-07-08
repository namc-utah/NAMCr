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
                        edge_name   = NA,
                        has_edge    = FALSE,
                        fields      = NULL,
                        args        = NULL,
                        description = NA
                    ),
                    list(...)
                )
            )
        },

        new_field = function(...){
            return(
                modifyList(
                    list(
                        data_type     = NA,
                        is_numeric    = FALSE,
                        is_expandable = FALSE,
                        description   = NA
                    ),
                    list(...)
                )
            )
        },

        new_argument = function(...){
            return(
                modifyList(
                    list(
                        data_type     = NA,
                        is_numeric    = FALSE,
                        is_array      = FALSE,
                        is_required   = FALSE,
                        is_for_paging = FALSE,
                        default_value = NA,
                        description   = NA
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



        is_paging_arg = function(arg_name){
            return( any(arg_name == c(self$tpl_pagination_first, self$tpl_pagination_offset)) )
        }

    ),

    public = list(

        #' @field tpl_pagination_first is the numeric first record to return
        tpl_pagination_first = NULL,

        #' @field tpl_pagination_offset is the numeric offset of the return
        tpl_pagination_offset = NULL,

        #' @field tpl_pagination_cursor is the cursor used to fetch data at
        tpl_pagination_cursor = NULL,

        #' @field tpl_page_fieldname is the name of the field holding page data
        tpl_page_fieldname = NULL,

        #' @field required_kind is string value of required argument kind
        required_kind = NULL,

        #' @field singular_kind is string value of singular argument kind
        singular_kind = NULL,

        #' @field array_kind is string value of array argument kind
        array_kind = NULL,


        #' Parse the API types
        #'
        #' @return api_schema For method chaining
        #'
        #' @examples
        #'
        #' types = ( namc_api$new(argList=...) )$get_api_types()
        #' schema = api_schema$new(types = types)
        #' schema$configure()
        #'
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
        #' schema$parse_endpoints()
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

            iSpecialType   = private$types$name == special_type
            iEndpoint      = private$types$fields[ iSpecialType ][[1]]$name == endpoint
            no_type        = is.na(private$types$fields[ iSpecialType ][[1]]$type$ofType[ iEndpoint ])
            no_description = is.na(private$types$fields[ iSpecialType ][[1]]$description[ iEndpoint ])
            data_type      = ifelse(no_type, NA, private$types$fields[ iSpecialType ][[1]]$type$ofType$name[ iEndpoint ] )
            edge_name      = NA
            has_edge       = FALSE

            # Endpoint data is contained in a nested sub-type
            if( is.na(data_type) ){
                data_type = private$types$fields[ iSpecialType ][[1]]$type$name[ iEndpoint ]
                iEdgeType = private$types$name == data_type

                # Is cursor/edge paginated data
                if( any(private$types$fields[ iEdgeType ][[1]]$name == self$tpl_pagination_cursor) ){
                    has_edge  = TRUE
                    i2Type    = private$types$fields[ iEdgeType ][[1]]$type$kind == "LIST"
                    edge_name = private$types$fields[ iEdgeType ][[1]]$name[ i2Type ]
                    data_type = private$types$fields[ iEdgeType ][[1]]$type$ofType$name[ i2Type ]

                # Is nested non-paged structured data
                } else if( any(private$types$fields[ iEdgeType ][[1]]$type$kind == "LIST") ){
                    i2Type    = private$types$fields[ iEdgeType ][[1]]$type$kind == "LIST"
                    data_type = private$types$fields[ iEdgeType ][[1]]$type$ofType$name[ i2Type ]
                }
            }

            iDataType = private$types$name == data_type
            private[[ special_type ]][[ endpoint ]] = private$new_endpoint(
                edge_name   = edge_name,
                has_edge    = has_edge,
                description = ifelse(no_description, NA, private$types$fields[ iSpecialType ][[1]]$description[ iEndpoint ])
            )

            for(fieldname in private$types$fields[ iDataType ][[1]]$name){
                iField         = private$types$fields[ iDataType ][[1]]$name == fieldname
                no_description = is.na(private$types$fields[ iDataType ][[1]]$description[iField])
                no_type        = is.empty(private$types$fields[ iDataType ][[1]]$type$name[ iField ])

                private[[ special_type ]][[ endpoint ]]$fields[[ fieldname ]] = private$new_field(
                    data_type     = ifelse(no_type,NA,private$types$fields[ iDataType ][[1]]$type$name[ iField ]),
                    is_numeric    = any( private$types$fields[ iDataType ][[1]]$type$name[ iField ] == private$numeric_types ),
                    is_expandable = FALSE,
                    description   = ifelse(no_description, NA, private$types$fields[ iDataType ][[1]]$description[iField])
                )
            }

            for(argname in private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$name){
                iArg           = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$name == argname
                no_description = is.na(private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$description[ iArg ])
                is_array       = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$kind[ iArg ] == self$array_kind
                is_required    = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$kind[ iArg ] == self$required_kind

                if( is_required ){
                    is_array       = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$kind[ iArg ] == self$array_kind
                    if( is_array){
                        is_numeric = any( private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$ofType$name[ iArg ] == private$numeric_types)
                        data_type  = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$ofType$name[ iArg ]

                    } else {
                        is_numeric = any( private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$name[ iArg ] == private$numeric_types)
                        data_type  = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$name[ iArg ]

                    }

                } else if( is_array){
                    is_numeric = any( private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$name[ iArg ] == private$numeric_types)
                    data_type  = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$ofType$name[ iArg ]

                } else {
                    is_numeric = any( private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$name[ iArg ] == private$numeric_types)
                    data_type  = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$name[ iArg ]
                }
                private[[ special_type ]][[ endpoint ]]$args[[ argname ]] = private$new_argument(
                    data_type     = data_type,
                    is_numeric    = is_numeric,
                    is_array      = is_array,
                    is_required   = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$type$kind[ iArg ] == self$required_kind,
                    is_for_paging = private$is_paging_arg( argname ),
                    default_value = private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$defaultValue[ iArg ],
                    description   = ifelse(no_description, NA, private$types$fields[ iSpecialType ][[1]]$args[ iEndpoint ][[1]]$description[ iArg ])
                )
            }

            invisible(self)
        },



        #' Check if argument is numeric
        #'
        #' @param endpoint String name of api endpoint
        #' @param argname String name of api endpoint argument
        #'
        #' @return logical TRUE/FALSE if argument is numeric
        #'
        # @examples
        is_arg_numeric = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            if( any(argname == names(private[[ special_type ]][[ endpoint ]]$args)) ){
                return( private[[ special_type ]][[ endpoint ]]$args[[ argname ]]$is_numeric )

            } else {
                # Assume argument is numeric if no data available
                return( TRUE )
            }
        },



        #' Check if argument is numeric
        #'
        #' @param endpoint String name of api endpoint
        #' @param argname String name of api endpoint argument
        #'
        #' @return logical TRUE/FALSE if argument is numeric
        #'
        # @examples
        is_arg_boolean = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            if( any(argname == names(private[[ special_type ]][[ endpoint ]]$args)) ){
                return( private[[ special_type ]][[ endpoint ]]$args[[ argname ]]$data_type == "Boolean" )

            } else {
                # Assume argument is not boolean if no data available
                return( FALSE )
            }
        },



        #' Get the sub-type contained within an api endpoint
        #'
        #' @param endpoint String name of api endpoint
        #'
        # @return
        #'
        # @examples
        get_special_type_from_endpoint = function(endpoint){
            for(special_type in private$special_types){
                if( any(endpoint == names(private[[special_type]])) ){
                    return( special_type )
                }
            }
            return(NA)
        },



        #' Determine if an endpoint has an edge
        #'
        #' @param endpoint String name of api endpoint
        #'
        # @return
        #'
        # @examples
        has_edge = function(endpoint){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$has_edge )
        },



        #' Get the name of the graphql edge contained in an endpoint
        #'
        #' @param endpoint String name of api endpoint
        #'
        #' @return String name of endpoint edge
        #'
        # @examples
        get_edge_name = function(endpoint){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$edge_name )
        },



        #' Get all available endpoint names
        #'
        #' @return Array of endpoint names
        #'
        # @examples
        get_endpoints = function(){
            endpoints = c()
            for(special_type in private$special_types){
                endpoints = c(endpoints,names(private[[ special_type ]]))
            }
            return( endpoints )
        },



        #' Get info about an endpoint
        #'
        #' @param endpoint String name of api endpoint
        #'
        # @return
        #'
        # @examples
        get_endpoint = function(endpoint){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]] )
        },



        #' Get all available field names for an endpoint
        #'
        #' @param endpoint String name of api endpoint
        #'
        # @return
        #'
        # @examples
        get_fields = function(endpoint){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( names(private[[ special_type ]][[ endpoint ]]$fields) )
        },



        #' Get info about an endpoint field
        #'
        #' @param endpoint String name of api endpoint
        #' @param field String name for a field of the endpoint
        #'
        # @return
        #'
        # @examples
        get_field = function(endpoint, field){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$fields[[ field ]] )
        },



        #' Get all arguments for and endpoint
        #'
        #' @param endpoint String name of api endpoint
        #' @param no_paging
        #'
        # @return
        #'
        # @examples
        get_arguments = function(endpoint, no_paging = FALSE){
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



        #' Get info about an argument
        #'
        #' @param endpoint String name of api endpoint
        #' @param argname
        #'
        # @return
        #'
        # @examples
        get_argument = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$args[[ argname ]] )
        },



        #' Checks if an argument is required
        #'
        #' @param endpoint String name of api endpoint
        #' @param argname
        #'
        # @return
        #'
        # @examples
        is_argument_required = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( private[[ special_type ]][[ endpoint ]]$args[[ argname ]]$is_required )
        },



        #' Checks if argument name is valid for an endpoint
        #'
        #' @param endpoint String name of api endpoint
        #' @param argname
        #'
        # @return
        #'
        # @examples
        is_argument = function(endpoint,argname){
            special_type = self$get_special_type_from_endpoint( endpoint )
            return( any(argname == names(private[[ special_type ]][[ endpoint ]]$args)) )
        },



        #' Check if endpoint exists
        #'
        #' @param endpoint String name of api endpoint
        #'
        # @return
        #'
        # @examples
        is_endpoint = function(endpoint){
            return( any( self$get_endpoints() == endpoint ) )
        },



        #' Describe an Endpoint.
        #'
        #' @description `info` prints text to the console describing an endpoint
        #'
        #' @details This function presents a text block describing an endpoint
        #'   and its corresponding input parameters. Inputs are marked as
        #'   required where applicable.
        #'
        #' @param endpoint api endpoint name to get information about.
        #' @param no_paging Logical that omits the paging arguments by default
        #' @param format One of: 'markdown', 'text' (default)
        #'   argument inclusion
        #'
        #' @export
        #'
        #' @examples
        #'
        #' info()
        #'
        info = function(endpoint, no_paging = FALSE, format = "text" ){

            # crude function to format text in markdown or console appropriate
            # text output styles
            wrap_text = function(txt, style, appendTo = '' ){
                styles = list(
                    h1 = list(
                        text = c('',' ------------------------------\n'),
                        markdown = c('\n## ','\n')
                    ),
                    h2 = list(
                        text = c('\n\t',':\n'),
                        markdown = c('\n### ','\n')
                    ),
                    h3 = list(
                        text = c('\t\t',':\n'),
                        markdown = c('\n### ','\n')
                    ),
                    h4 = list(
                        text = c('\t\t\t',':\n'),
                        markdown = c('\n#### ','\n')
                    ),
                    t1 = list(
                        text = c('\t','\n\n'),
                        markdown = c('','')
                    ),
                    l1 = list(
                        text = c('\t\t\t','\n'),
                        markdown = c('\n','\n')
                    ),
                    l2 = list(
                        text = c('\t\t\t','\n'),
                        markdown = c('','')
                    ),
                    k1 = list(
                        text = c('',':\t'),
                        markdown = c('','\n  :')
                    ),
                    k2 = list(
                        text = c('',':\t\t'),
                        markdown = c('* ','')
                    )
                )
                return(
                    paste0(
                        appendTo,
                        styles[[style]][[format]][1],
                        txt,
                        styles[[style]][[format]][2]
                    )
                )
            }

            e = endpoint
            e_info = self$get_endpoint(e)
            tpl = wrap_text(e,'h1')
            tpl = wrap_text(
                paste0(wrap_text('Description','k1'), ifelse(is.empty(e_info$description),'',e_info$description))
                ,'h2',tpl
            )

            tpl = wrap_text('Input Parameters','h2',tpl)
            parameters = self$get_arguments(e, no_paging)
            if(length(parameters) == 0){
                tpl = wrap_text('none','t1',tpl)
            } else {
                for(p in parameters){
                    tpl = wrap_text(p,'h3',tpl)
                    p_info = self$get_argument(e,p)
                    for(attribute_name in names(p_info)){
                        if( !is.empty(p_info[[attribute_name]]) ){
                            tpl = wrap_text( paste0( wrap_text(gsub("_"," ",attribute_name),'k1'), p_info[[attribute_name]]), 'l1', tpl)
                        }
                    }
                    tpl = paste0(tpl, '\n')
                }
            }
            tpl = wrap_text('Output Fields','h2',tpl)
            fields = self$get_fields(e)
            if(length(fields) == 0) {
                tpl = wrap_text('none','t1',tpl)
            }
            for(f in fields){
                tpl = wrap_text(f,'h3',tpl)
                f_info = self$get_field(e,f)
                for(attribute_name in names(f_info)){
                    if( !is.empty(f_info[[attribute_name]]) ){
                        tpl = wrap_text( paste0( wrap_text(gsub("_"," ",attribute_name),'k1'), f_info[[attribute_name]]), 'l1', tpl)
                    }
                }
                tpl = paste0(tpl, '\n')
            }
            tpl = paste0(tpl,'\n\n')

            return(tpl)
        }

    )
)