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
#' @param return_format
#'
#' @return data.frame A dataframe containing the query results
#' @export
#'
#' @examples
query = function(api_endpoint, fields = c(), filter = list(), sort = c(), limit = NA, return_format = 'dataframe'){

    tpl_pagination_first  = 'limit:%s'
    tpl_pagination_offset = ' nextToken:%s'

    tpl_filter = ''
    tpl_fields = ''

    data = pkg.globals$api$query( sprintf(
        '{
            %s {
                %s
            }
        }',
        api_endpoint,
        paste(pkg.globals$api$schema[[ api_endpoint ]]$fields, collapse = ' ')
    ) )

    return( data )
}



#' Update data
#'
#' Update NAMC database data via the API. Upsert routines are used.
#'
#' @param data data.frame The data to upsert into the database.
#' @param api_endpoint string A NAMC API endpoint name. Available endpoints can be found via the
#' namc_api get_endpoints method.
#'
#' @return data.frame A dataframe containing any fields added to the Query
#' @export
#'
#' @examples
update = function(api_endpoint, data){

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