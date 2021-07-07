#' initialize cached API state.
#'
#' @description
#' `initialize` analyzes the API schema and authentication information.
#'
#' @export
#'
initialize = function(){

    #c = yaml::yaml.load_file("./data-raw/config.yml")
    c = pkg.config()

    c$api$auth = namc_oauth2$new( argList = c$auth )
    c$api$schema = api_schema$new( argList = c$schema )

    .pkgenv$api <<- namc_api$new( argList = c$api )
    return()
}



#' Reinitialize cached API state.
#'
#' @description
#' `reinitialize` reanalyzes the API schema and authentication information.
#'
#' @param clear_credentials logical TRUE/FALSE indicating if cached API credentials should be deleted
#' @export
#'
reinitialize = function(clear_credentials = FALSE){

    initialize()

    if(clear_credentials) .pkgenv$api$get_auth_provider()$reset_token()
    .pkgenv$api$configure( force = TRUE )
    return()
}



#' Execute public API class method
#'
#' @param fn_name Name of namc_api public method
#'
#' @return variable Depends on function executed. See public methods of namc_api
#'
execute_api_fn = function(fn_name){
    return( .pkgenv$api[[fn_name]]() )
}



#' Get API class variable
#'
#' @param var_name Name of namc_api variable name
#'
#' @return variable Depends on variable requested. See variables of namc_api
#'
get_api_var = function(var_name){
    return( .pkgenv$api$get_var(var_name) )
}



#' Checks for empty values
#'
#' @description
#' `is.empty` Null, NA and empty string values are all considered empty
#'
#' @param val value or list of values to check for emptiness
#'
#' @return logical array
#' @export
#'
#' @examples
#'
#' is.empty( list(NA,NULL,'','test',c(),list()) )
#'
is.empty = function(val){
    # Handle the singular null argument edge case
    if(is.null(val)){
        return( TRUE )
    } else {
        return( is.na(val) | val == '' | lengths(val) == 0 )
    }
}



columnize = function(items, numberItems = FALSE){
    console_width = options("width")
    col_width = 45
    nCols = floor(console_width$width / col_width)
    nItems = length(items)
    nItemsPerColumn = ceiling(nItems / nCols)
    iLastRows = which( (seq(nItems) / nItemsPerColumn) %% 1 == 0)
    if(numberItems){
        numberedItems = paste0(
            tab(),
            c(1:nItems),
            ") ",
            items
        )
    } else {
        numberedItems = items
    }
    columnizedItems = numberedItems[1:iLastRows[1]]
    for(iCol in seq(nCols)[2:nCols]){
        for(iRow in seq(nItemsPerColumn)){
            columnizedItems[iRow] = paste0(
                columnizedItems[iRow],
                space(col_width*(iCol-1) - nchar(columnizedItems[iRow]))
            )
        }
        iLastColRow = ifelse(iCol==nCols,nItems,iLastRows[iCol])
        columnizedItems = paste0(
            columnizedItems,
            numberedItems[seq(from=iLastRows[iCol-1]+1, to=iLastColRow)]
        )
    }

    columnizedItems = paste0(columnizedItems, collapse = "\n")

    return( columnizedItems )
}


tab = function(nTabs=1, tabWidth=8){
    return( paste0(replicate(nTabs,space(tabWidth)), collapse = "") )
}

space = function(nSpaces=1){
    return( paste0(replicate(nSpaces," "), collapse = "") )
}