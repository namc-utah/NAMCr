#' Query NAMC database.
#'
#' @description `query` retrieves data from NAMC given a specified API endpoint
#' name.
#'
#' @details Providing a desired endpoint and filtering the function retrieves
#' data from the NAMC database. If not already authenticated the will trigger
#' the authentication / account creation routines. By default all fields are
#' included. Use include and exclude arguments to limit returned fields.
#'
#' @family query functions
#' @seealso [info()],`browseVignettes("NAMC-API-Overview")`
#'
#' @param api_endpoint string A NAMC API endpoint name. Available endpoints can
#'   be found via the [endpoints()] function.
#' @param args a list of named arguments to pass to the API endpoint. Can also
#'   be passed individually in  ...
#' @param include is a vector of fields to include from the endpoint.
#' @param exclude is a vector of fields to exclude from the endpoint.
#' @param filter list A list containing filter parameters.
#' @param sort list A list containing fields / directions to sort by.
#' @param limit numeric The number of results desired.
#' @param api namc_api An instance of the namc_api class having a default of a
#'   pre-initialized package object
#' @param ... named arguments to merge with args and pass to the API endpoint.
#' @param expand_metadata logical if JSON metadata field is expanded into dataframe columns
#' @param default_json_fieldName "metadata" is the preset default
#'
#' @return data.frame A dataframe containing the query results.
#' @export
#'
#' @examples
#'
#' sites = NAMCr::query(
#'   api_endpoint = 'sites',
#'   include = c('station','lat','long')),
#'   filter = list(
#'     station = c('station1','station2'),
#'     lat = list(
#'       data = c(42,46),
#'       type = 'between'
#'     )
#'   )
#'   sort = c('station','lat'),
#'   limit = 50
#' )
#'
#' # View full API docs in console
#' NAMCr::info()
#'
query = function(
        api_endpoint,
        args    = list(),
        include = c(),
        exclude = c(),
        filter  = list(),
        sort    = c(),
        limit   = NA,
        api     = .pkgenv$api,
        json_autodetect = TRUE,
        json_fields = c(),
        ...
){

    # Verify endpoint exists
    api$is_endpoint(api_endpoint, stop_if_not = TRUE)

    # Collect all arguments and remove any pagination keys
    endpoint_args = modifyList(args, list(...))
    json_args = c()

    # Build Argument Template
    if(length(endpoint_args) != 0){
        arg_names = names( endpoint_args )
        # LET GRAPHQL ERRORS FALL THROUGH | NO ARGUMENT VALIDATION IS PERFORMED

        # Place Arguments
        tpl_args = '('
        for(arg in arg_names){
            if( json_autodetect && api$schema$is_arg_json(api_endpoint, arg) ){
                json_args = c(json_args, arg)
            }
            if( api$schema$is_arg_numeric(api_endpoint, arg) ) {
                arg_vals = paste0(endpoint_args[[arg]], collapse = ",")
            } else if( api$schema$is_arg_boolean(api_endpoint, arg) ) {
                arg_vals = paste0(gsub(TRUE,"true",gsub(1,"true",endpoint_args[[arg]])), collapse = ",")
            } else {
                arg_vals = paste0('"', paste0(endpoint_args[[arg]], collapse = '","'), '"')
            }
            is_vector = grepl(",",arg_vals)
            tpl_args = paste0(
                tpl_args,
                sprintf(
                    '%s:%s%s%s ',
                    arg,
                    ifelse(is_vector,'[',''),
                    arg_vals,
                    ifelse(is_vector,']','')
                )
            )
        }
        tpl_args = paste0(tpl_args,')')

    } else {
        tpl_args = ''
    }

    # Edge / Cursor Style Pagination
    fields = api$schema$get_fields( api_endpoint )
    if(length(include) > 0){
        fields = include
    }
    if(length(exclude) > 0){
        fields = setdiff(fields, exclude)
    }
    if( api$schema$has_edge( api_endpoint ) ){
        tpl_fields = sprintf(
            '%s{%s}%s',
            api$schema$get_edge_name( api_endpoint ),
            paste(fields, collapse = ' '),
            api$schema$tpl_pagination_cursor
        )
    } else {
        tpl_fields = paste(fields, collapse = ' ')
    }

    paged_data = NA
    is_last_page = FALSE
    page_limit = api$pagination_limit
    page_offset = NA
    is_paginated = FALSE
    page_index = 0

    cat("Retrieving data: ")

    while(!is_last_page){
        page_index = page_index + 1

        # Setup Pagination
        tpl_pagination = ''
        tpl_all_args = tpl_args

        # Check if the named "limit" argument is a part of the endpoint
        if( api$schema$is_argument( api_endpoint, api$tpl_pagination_first ) ){
            is_paginated = TRUE
            tpl_pagination = sprintf('%s:%s ', api$tpl_pagination_first, page_limit)
            endpoint_args[ api$tpl_pagination_first ] = page_limit
        }
        if(!is.na( page_offset )){
            tpl_pagination = paste0(tpl_pagination, sprintf('%s:%s ', api$tpl_pagination_offset, page_offset))
        }
        if(tpl_pagination != ''){
            if(tpl_args != ''){
                tpl_all_args = paste0( substr(tpl_args, 1, nchar(tpl_args)-1), tpl_pagination, ')')
            } else {
                tpl_all_args = paste0('(',tpl_pagination,')')
            }
        }

        tpl_query = sprintf(
            'query rQuery{
                %s%s {
                    %s
                }
            }',
            api_endpoint,
            tpl_all_args,
            tpl_fields
        )

        data = api$query( tpl_query )

        msg = ifelse( page_index%%61 == 0, "\t.\n", ".")
        cat(msg)

        if( api$schema$has_edge( api_endpoint ) ){
            page_offset = data[[api_endpoint]][[ api$schema$tpl_pagination_cursor ]]
            if(is.null(page_offset)) page_offset = NA
            data[[api_endpoint]] = data[[api_endpoint]][[ api$schema$get_edge_name( api_endpoint ) ]]

        } else if(is_paginated) {
            page_offset = ifelse(
                !is.data.frame(paged_data),
                page_limit,
                nrow(paged_data) + nrow( data[[api_endpoint]] )
            )
        }
        if(is.na(page_offset) || nrow( data[[api_endpoint]] ) !=  page_limit || (!is.na(limit) && nrow( data[[api_endpoint]] ) >= limit) ){
            is_last_page = TRUE
            if(is_paginated && any(!is.na(paged_data))){
                data[[api_endpoint]] = rbind(paged_data, as.data.frame(data[[api_endpoint]]) )
            }
        } else if(!is.data.frame(paged_data)) {
            paged_data = data[[api_endpoint]]
        } else {
            paged_data = rbind(paged_data, data[[api_endpoint]])
        }

    }
    # Handle JSON in returned data
    if( json_autodetect ){
        json_args = c(json_args, json_fields)
    } else {
        json_args = json_fields
    }

    if( any(json_args %in% names(data[[api_endpoint]])) ){
        data[[api_endpoint]] = json.expand(data[[api_endpoint]], json_args)
    }
    cat(" Complete!\n")

    return( data[[api_endpoint]] )
}



#' Save data
#'
#' Update NAMC database data via the API. Upsert routines are used.
#'
#' @family query functions
#' @seealso [info()],`browseVignettes("NAMC-API-Overview")`
#'
#' @param api_endpoint string A NAMC API endpoint name. Available endpoints can be found via the
#' @param args a list of named arguments to pass to the API endpoint. Can also be passed individually in  ...
#' namc_api get_endpoints method.
#' @param api namc_api An instance of the namc_api class having a default of a pre-initialized package object
#' @param ... named arguments to merge with data and pass to the API endpoint.
#'
#' @return data.frame An identifier of an inserted record OR a data.frame containing the modified record
#' @export
#'
#' @examples
#'
#' site = list(
#'   station = 'myStation',
#'   lat = 45.555,
#'   long = -123.555,
#'   ...
#' )
#'
#' sites = NAMCr::save(
#'   api_endpoint = 'updateSite',
#'   args = site
#' )
#'
#' # Alternative
#'
#' sites = NAMCr::save(
#'   api_endpoint = 'updateSite',
#'   station = 'myStation',
#'   lat = 45.555,
#'   long = -123.555
#' )
#'
save = function(
        api_endpoint,
        args = list(),
        api = .pkgenv$api,
        json_autodetect = TRUE,
        json_fields = list(),
        ...
){

    tpl_variables = ''
    tpl_fields = ''

    # Verify endpoint exists
    api$is_endpoint(api_endpoint, stop_if_not = TRUE)

    # Collect all arguments and remove any pagination keys
    endpoint_args = modifyList(args, list(...))

    if( json_autodetect ){
        arg_names = names(endpoint_args)
        collapsed_field_names = unique( gsub("\\..*$", "", arg_names[ grepl("\\.", arg_names) ]) )
        for(cField_name in collapsed_field_names){
            json_fields[[cField_name]] = arg_names[ grepl(c(cField_name, "\\."), arg_names) ]
        }
    }

    for( field_name in json_fields ){
        endpoint_args = json.collapse(endpoint_args, field_name, json_fields[[field_name]])
    }

    tpl_args = format_arguments(api_endpoint, endpoint_args, api)

    #tmp_endpoint_args = gsub("\\\"", "\\\\\\\"", endpoint_args[[arg]])
    #arg_vals = paste0('"', paste0(tmp_endpoint_args, collapse = '","'), '"')


    tpl_query = sprintf(
        'mutation rMutation{
            %s%s
        }',
        api_endpoint,
        tpl_args
    )

    data = api$query( tpl_query )


    return( data )
}



#' Execute raw graphql query
#'
#' @family query functions
#' @seealso [info()],`browseVignettes("NAMC-API-Overview")`
#'
#' @return data.frame A dataframe contained the query result.
#' @export
#'
#' @examples
#'
#' sites = NAMCr::raw_query(
#'   'query rQuery{
#'     sites {
#'       station lat long ...
#'     }
#'   }'
#' )
#'
raw_query = function( query_string ){
    return( .pkgenv$api$query( query_string ) )
}



#' format user passed arguments for graphql queries
#'
#' @param api_endpoint string A NAMC API endpoint name. Available endpoints can
#'   be found via the [endpoints()] function.
#' @param endpoint_args a list of named arguments to pass to the API endpoint. Can also
#'   be passed individually in  ...
#' @param api instance of a namc_api class
#'
#' @return graphql formatted query string
#'
format_arguments = function(api_endpoint, endpoint_args, api){
    # Build Argument Template
    if(length(endpoint_args) != 0){
        arg_names = names( endpoint_args )

        # LET GRAPHQL ERRORS FALL THROUGH | NO ARGUMENT VALIDATION IS PERFORMED

        # Place Arguments
        arg_count = 0
        tpl_args = '('
        for(arg in arg_names){
            if( api$schema$is_arg_numeric(api_endpoint, arg) ) {
                arg_vals = paste0(endpoint_args[[arg]], collapse = ",")
            } else if( api$schema$is_arg_boolean(api_endpoint, arg) ) {
                arg_vals = paste0(gsub(TRUE,"true", gsub(FALSE,"false", as.logical(endpoint_args[[arg]]))), collapse = ",")
            } else {
                tmp_endpoint_args = gsub("\\\"", "\\\\\\\"", endpoint_args[[arg]])
                arg_vals = paste0('"', paste0(tmp_endpoint_args, collapse = '","'), '"')
            }
            is_vector = grepl(",",arg_vals) & !grepl("\\{",arg_vals)
            if(arg_count > 0) {
                tpl_args = paste0(
                    tpl_args,
                    ","
                )
            }
            tpl_args = paste0(
                tpl_args,
                sprintf(
                    '%s:%s%s%s ',
                    arg,
                    ifelse(is_vector,'[',''),
                    arg_vals,
                    ifelse(is_vector,']','')
                )
            )
            arg_count = arg_count + 1
        }
        tpl_args = paste0(tpl_args,')')

    } else {
        tpl_args = ''
    }
    return(tpl_args)
}