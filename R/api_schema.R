#' @title api_schema
#' @description R6 class for handling the NAMC graphql schema
#' @return a `namc_oauth2` class (R6 class)
#' @export
#' @examples
#'
#' types = ( namc_api$new(argList=...) )$get_raw_schema()
#' schema = api_schema$new(types = types)
#'
api_schema = R6::R6Class(
    "api_schema",
    inherit = base_class,
    portable = TRUE,

    private = list(

        types = NULL,
        tpl_pagination_offset = NULL,
        endpoints = NULL

    ),

    public = list(


        #' Parse introspected schema
        #'
        #' @return api_schema For method chaining
        #'
        #' @examples
        #'
        #' types = ( namc_api$new(argList=...) )$get_raw_schema()
        #' schema = api_schema$new(types = types)
        #' schema$parse_schema()
        #'
        parse_schema = function(){

            iEndpoints = private$types$name == "Query"
            endpoints = private$types$fields[ iEndpoints ][[1]]$name

            for(endpoint in endpoints){
                iEndpoint = private$types$fields[ iEndpoints ][[1]]$name == endpoint
                eType = private$types$fields[ iEndpoints ][[1]]$type$ofType$name[ iEndpoint ]
                # If endpoint is of a special sub-type
                if( is.na(eType) ){
                    fType = private$types$fields[ iEndpoints ][[1]]$type$name[ iEndpoint ]
                    iType = private$types$name == fType
                    if( all(private$types$fields[ iType ][[1]]$type$kind == "SCALAR") ){
                        eType = fType
                    } else {
                        #if( any(private$types$fields[ iType ][[1]]$name == "records") ){
                            i2Type = private$types$fields[ iType ][[1]]$name == "records"
                            eType = private$types$fields[ iType ][[1]]$type$ofType$name[ i2Type ]
                        #}
                        #eType = private$types$fields[ iType ][[1]]$type$ofType$name[ i2Type ]

                    }
                } else {
                    fType = NA
                }
                iType = private$types$name == eType
                private$endpoints[[endpoint]] = list(
                    subtype = fType,
                    #subfield_paginate = ,
                    fields = private$types$fields[ iType ][[1]]$name,
                    args = private$types$fields[ iEndpoints ][[1]]$args[ iEndpoint ][[1]]$name
                )
            }

            invisible(self)
        },



        #' Discover info on API type
        #'
        #' @param type_name Name of API type to discover
        #' @param recurse Recurse down field trees
        #'
        #' @return list Info structure of API details
        #'
        #' @examples
        #'
        #' types = ( namc_api$new(argList=...) )$get_raw_schema()
        #' schema = api_schema$new(types = types)
        #' schema$get_type_info()
        #'
        get_type_info = function(type_name, recurse = TRUE){
            iType = private$types$name == type_name
            label = type_name
            kind = private$types$kind[ iType ]
            fieldnames = private$types$fields[ iType ][[1]]$name
            fields = list()

            for(iField in 1:length(fieldnames)){
                fields[ fieldnames[iField] ] = list(
                    type_name = private$types$fields[ iType ][[1]]$type$name[ iField ],
                    api_type = private$types$fields[ iType ][[1]]$type$ofType$name[ iField ],
                    args = list(
                        names = private$types$fields[ iType ][[1]]$args[[ iField ]]$name,
                        type = private$types$fields[ iType ][[1]]$args[[ iField ]]$type$name
                    )
                )
            }
            is_pagination = any(private$tpl_pagination_offset == fieldnames)


            if( kind == "OBJECT" ){

            } else if( kind == "SCALAR" ){

            }
            return(
                list(
                    label = label,
                    kind = kind,
                    fields = fields,
                    is_pagination = is_pagination
                )
            )
        }#,



        # get_fields = function(api_endpoint){
        #
        # },
        #
        #
        #
        # get_args = function(api_endpoint){
        #
        # }

    )
)