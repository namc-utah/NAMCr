#' Query NAMC database
#'
#' Providing a desired endpoint and filtering the function retrieves data from the NAMC database. If not already
#' authenticated the will trigger the authentication / account creation routines. By default all fields are included.
#' Use include and exclude arguments to limit returned fields.
#'
#' @param api_endpoint string A NAMC API endpoint name. Available endpoints can be found via the namc_api get_endpoints method.
#' @param args a list of named arguments to pass to the API endpoint. Can also be passed individually in  ...
#' @param include is a vector of fields to include from the endpoint.
#' @param exclude is a vector of fields to exclude from the endpoint.
#' @param filter list A list containing filter parameters.
#' @param sort list A list containing fields / directions to sort by.
#' @param limit numeric The number of results to desired.
#' @param api namc_api An instance of the namc_api class having a default of a pre-initialized package object
#' @param ... named arguments to merge with args and pass to the API endpoint.
#'
#' @return data.frame A dataframe containing the query results
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
query = function(api_endpoint, args = list(), include = c(), exclude = c(), filter = list(), sort = c(), limit = NA, api = .pkgenv$api, ...){

    # Verify endpoint exists
    api$is_endpoint(api_endpoint, stop_if_not = TRUE)

    # Collect all arguments and remove any pagination keys
    endpoint_args = modifyList(args, list(...))

    # Build Argument Template
    if(length(endpoint_args) != 0){
        arg_names = names( endpoint_args )

        # LET GRAPHQL ERRORS FALL THROUGH | NO ARGUMENT VALIDATION IS PERFORMED

        # Place Arguments
        tpl_args = '('
        for(arg in arg_names){
            if( api$schema$is_arg_numeric(api_endpoint, arg) ) {
                arg_vals = paste0(endpoint_args[[arg]], collapse = ",")
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
    fields = api$schema$get_endpoint_fields( api_endpoint )
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
            if(is_paginated && !is.na(paged_data)){
                data[[api_endpoint]] = paged_data
            }
        } else if(!is.data.frame(paged_data)) {
            paged_data = data[[api_endpoint]]
        } else {
            paged_data = rbind(paged_data, data[[api_endpoint]])
        }

    }
    cat(" Complete!\n")

    return( data[[api_endpoint]] )
}



#' Save data
#'
#' Update NAMC database data via the API. Upsert routines are used.
#'
#' @param data data.frame or list The data to upsert into the database.
#' @param api_endpoint string A NAMC API endpoint name. Available endpoints can be found via the
#' namc_api get_endpoints method.
#'
#' @return data.frame A dataframe containing any fields added to the Query
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
#'   api_endpoint = 'sites',
#'   data = site
#' )
#'
save = function(api_endpoint, data, api = .pkgenv$api){

    tpl_variables = ''
    tpl_fields = ''

    data = .pkgenv$api$query( sprintf(
        'mutation rMutation{
            %s {
                %s
            }
        }',
        api_endpoint,
        jsonlite::serializeJSON( data )
    ) )

    return( data )
}



#' Execute raw graphql query
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