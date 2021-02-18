#' Query NAMC database
#'
#' Providing a desired endpoint and filtering the function retrieves data from the NAMC database. If not already
#' authenticated the will trigger the authentication / account creation routines.
#'
#' @param api_endpoint string A NAMC API endpoint name. Available endpoints can be found via the
#' namc_api get_endpoints method.
#' @param fields vector A vector of strings matching the desired api_endpoint fields to return.
#' @param filter list A list containing filter parameters.
#' @param sort list A list containing fields / directions to sort by.
#' @param limit numeric The number of results to desired.
#' @param return_format string One of 'dataframe', 'json'
#'
#' @return data.frame A dataframe containing the query results
#' @export
#'
#' @examples
#'
#' sites = NAMCr::query(
#'   api_endpoint = 'sites',
#'   fields = c('station','lat','long')),
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
query = function(api_endpoint, args = list(), fields = c(), filter = list(), sort = c(), limit = NA, return_format = 'dataframe', ...){

    endpoint_args = modifyList(args, list(...))

    if(length(endpoint_args) != 0){
        arg_names = names( endpoint_args )
        tpl_args = '('
        for(arg in arg_names){
            tpl_args = paste0(tpl_args, sprintf('%s:%s ', arg, endpoint_args[arg]))
        }
        tpl_args = paste0(tpl_args,')')
    } else {
        tpl_args = ''
    }

    if( !pkg.globals$api$is_configured ) pkg.globals$api$configure()

    if( pkg.globals$api$schema[[ api_endpoint ]]$is_paginated ){
        tpl_fields = sprintf(
            '%s{%s}',
            pkg.globals$api$schema[[ api_endpoint ]]$subtype,
            paste(pkg.globals$api$schema[[ api_endpoint ]]$fields, collapse = ' ')
        )
    } else {
        tpl_fields = paste(pkg.globals$api$schema[[ api_endpoint ]]$fields, collapse = ' ')
    }

    tpl_pagination_first  = 'limit:%s'
    tpl_pagination_offset = ' nextToken:%s'

    tpl_filter = ''

    tpl_query = sprintf(
        'query rQuery{
            %s%s {
                %s
            }
        }',
        api_endpoint,
        tpl_args,
        tpl_fields
    )

    data = pkg.globals$api$query( tpl_query )

    if( pkg.globals$api$schema[[ api_endpoint ]]$is_paginated ){
        data[[api_endpoint]] = data[[api_endpoint]][[ pkg.globals$api$schema[[ api_endpoint ]]$subtype ]]
    }

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
save = function(api_endpoint, data){

    tpl_variables = ''
    tpl_fields = ''

    data = pkg.globals$api$query( sprintf(
        'mutation rMutation{
            %s {
                %s
            }
        }',
        api_endpoint,
        paste(pkg.globals$api$schema[[ api_endpoint ]]$fields, collapse = ' ')
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
    return( pkg.globals$api$query( query_string ) )
}