#' CLI for the NAMC API
#'
#' @return
#' @export
#'
#' @examples
#'
#' NAMCr::cli()
#'
cli = function(api = .pkgenv$api, ...){

    api$configure()
    cAPI = api_cli$new()
    return( cAPI$start(api = api, ...) )
}



#' @title api_cli
#' @description R6 class for handling NAMC cli driven api calls
#' @return a `api_cli` class (R6 class)
#' @examples
#'
#' auth_config = list(...) # api_cli public or private variables
#' auth = api_cli$new(api,args)
#'
api_cli = R6::R6Class(
    "api_cli",
    portable = TRUE,

    public = list(

        #' #Starts an api cli instance
        #'
        #' @param api_endpoint String name of endpoint
        #' @param args List of named arguments to pass to the endpoint
        #' @param include List of fields to include
        #' @param exclude List of fields to exclude
        #' @param filter List of fieldname / filter combinations
        #' @param sort Array of fields to sort by
        #' @param limit Number of results to limit the return by
        #' @param api NAMC api instance
        #' @param ...
        #'
        #' @return nothing is returned
        #'
        # @examples
        start = function(api_endpoint = NA, args = list(), include = c(), exclude = c(), filter = list(), sort = c(), limit = NA, api = NULL, ...){
            private$restart()

            private$api           = api
            private$api_endpoint  = api_endpoint
            private$args          = utils::modifyList(args, list(...))
            private$include       = include
            private$exclude       = exclude
            private$filter        = filter
            private$sort          = sort
            private$limit         = limit

            while(private$cli_position >= 1 && private$cli_position){
                data = private[[ paste0("fn_",private$cli_positions[private$cli_position]) ]]()
            }
            if( private$cli_position == 0 ) return( data )
            if( private$cli_position == -1 ) invisible()
            invisible()
        }
    ),

    private = list(

        api           = NULL,
        api_endpoint  = NA,
        args          = list(),
        include       = c(),
        exclude       = c(),
        filter        = list(),
        sort          = c(),
        limit         = NA,

        endpoints     = c(),

        cli_position  = 1,
        cli_positions = c("greeting","endpoint","args","advanced","limit","sort","include","exclude","output","exec"),


        restart = function(){
            private$api           = NULL
            private$api_endpoint  = NA
            private$args          = list()
            private$include       = c()
            private$exclude       = c()
            private$filter        = list()
            private$sort          = c()
            private$limit         = NA

            private$endpoints     = c()
        },



        increment = function(){
            private$cli_position = private$cli_position + 1
        },



        complete = function(){
            private$cli_position = 0
        },



        exit = function(){
            private$cli_position = -1
        },



        goto = function(cli_position){
            private$cli_position = which(cli_position == private$cli_positions)
        },



        validate = function(user_input, range=c(), type="numeric", allow_blank = FALSE){
            # Check for a list of values
            if(grepl(",",user_input)){
                user_input = strsplit(user_input,split = ",")[[1]]
            }
            user_input = trimws(user_input)
            val = list(
                data = user_input,
                is_valid = FALSE,
                exit_required = FALSE,
                skip = FALSE
            )
            if(user_input == "e"){
                private$exit()
                val$exit_required = TRUE
                return( val )
            }
            if(user_input == ""){
                val$skip = TRUE
            }
            if(type == "numeric"){
                if(!is.na(suppressWarnings(as.numeric(user_input))) &&
                   (length(range) == 0 || length(setdiff(as.numeric(user_input), range)) == 0)
                ){
                    val$data = as.numeric(user_input)
                    val$is_valid = TRUE
                }
            } else {
                if( length(range) == 0 || length(setdiff(user_input, range)) == 0 ){
                    val$is_valid = TRUE
                }
            }
            if(!val$is_valid  && !allow_blank) cat("Invalid input. Try again.\n")
            return( val )
        },



        fn_greeting = function(){
            cat(
                "Welcome to the NAMC API CLI\n",
                "---------------------------\n",
                'Type "e" at any prompt to exit\n'
            )
            private$increment()
            invisible(self)
        },



        fn_endpoint = function(){
            private$endpoints = private$api$get_endpoints()

            if( is.na(private$api_endpoint) || !any(private$api_endpoint == private$endpoints) ){
                cat(
                    paste0(
                        "\nDatasources:\n",
                        columnize(private$endpoints, numberItems = TRUE)
                    )
                    # paste0("\t",
                    #     c(1:length(private$endpoints)),
                    #     ") ",
                    #     private$endpoints,
                    #     collapse = "\n"
                    # )
                )
                is_valid = FALSE
                while( !is_valid ){
                    user_input = readline(prompt = "Enter a data source number: ")
                    res = private$validate( user_input, c(1:length(private$endpoints)), "numeric", FALSE)
                    is_valid = res$is_valid
                    if( res$exit_required) return()
                }
                private$api_endpoint = private$endpoints[res$data]
                cat("Preparing to query ( ",private$endpoints[res$data]," )...\n")
            }
            private$increment()
            invisible(self)
        },



        get_arg = function(arg, arg_info){
                is_valid = FALSE
                while( !is_valid ){
                    user_input = readline(
                        prompt = paste0(
                            ifelse(
                                arg_info$is_array,
                                "Enter value(s) for ",
                                "Enter a single value for "),
                            arg,": ")
                    )
                    res = private$validate( user_input = user_input, type = ifelse(arg_info$is_numeric, "numeric", "") )
                    is_valid = res$is_valid
                    if( res$exit_required ) break
                }
                private$args[[arg]] = res$data
            return( res )
        },



        fn_args = function(){
            all_args = private$api$get_endpoint_args( private$api_endpoint, no_paging = TRUE )

            if( length( private$args ) == 0 && length(all_args) != 0 ){
                cat("\nSeparate multiple values with a (,) where allowed\n")
                required_args = c()
                for(arg in all_args){
                    arg_info = private$api$schema$get_argument( private$api_endpoint, arg)
                    required_args = c(required_args,arg_info$is_required)
                    if( arg_info$is_required ){
                        res = private$get_arg(arg, arg_info)
                        if( res$exit_required ) return()
                    }
                }

                other_args = all_args[!required_args]

                if( length(other_args) > 0){
                    while( TRUE ){
                        cat(
                            "\nOther available parameters (press enter to skip):\n",
                            paste0("\t",
                                c( 1:length(other_args) ),") ",
                                other_args,
                                collapse = "\n"
                            )
                        )
                        user_input = readline(prompt = "Enter a parameter number: ")
                        res = private$validate( user_input, c(1:length(other_args)), "numeric", TRUE)
                        if( res$skip ) break
                        if( res$exit_required) return()
                        if( res$is_valid ){
                            arg = other_args[ res$data ]
                            arg_info = private$api$schema$get_argument( private$api_endpoint, arg)
                            res = private$get_arg(arg, arg_info)
                            if( res$exit_required ) return()
                        }
                    }
                }
            }
            #private$increment()
            private$goto( "exec" )
            invisible(self)
        },



        fn_advanced = function(){
            private$increment()
            invisible(self)
        },



        fn_limit = function(){

            private$increment()
            invisible(self)
        },



        fn_sort = function(){

            private$increment()
            invisible(self)
        },



        fn_include = function(){

            private$increment()
            invisible(self)
        },



        fn_exclude = function(){

            private$increment()
            invisible(self)
        },



        fn_print_command = function(queryType){

            if(tolower(queryType) == "query"){
                argument_names = formalArgs("query")
            } else {
                argument_names = formalArgs("save")
            }
            past_first_arg = FALSE

            tpl = 'Use the following command to obtain the same results in a script:\n\n'
            tpl = paste0(tpl, "\tresult = ", tolower(queryType), "(")

            for(arg in argument_names){
                if( arg != "api" && !length(private[[arg]])==0 && !is.na(private[[arg]]) ) {
                    tpl = paste0( tpl, ifelse(past_first_arg,",",""),"\n\t\t", arg, " = ", deparse(private[[arg]]) )
                    past_first_arg = TRUE
                }
            }
            tpl = paste0(tpl,"\n\t)\n\n\n")


            cat(tpl)
        },



        fn_exec = function(){

            private$complete()
            message('***Press (ESC) to exit before data is received***')

            queryType = private$api$schema$get_special_type_from_endpoint(private$api_endpoint)
            private$fn_print_command(queryType)

            return(
                query(
                    api_endpoint = private$api_endpoint,
                    args         = private$args,
                    include      = private$include,
                    exclude      = private$exclude,
                    filter       = private$filter,
                    sort         = private$sort,
                    limit        = private$limit,
                    api          = private$api
                )
            )
        }

    )
)
