#' CLI for the NAMC API
#'
#' @return
#' @export
#'
#' @examples
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

        start = function(api_endpoint = NA, args = list(), include = c(), exclude = c(), filter = list(), sort = c(), limit = NA, api = NULL, ...){
            private$restart()

            private$api           = api
            private$endpoint_name = api_endpoint
            private$args          = modifyList(args, list(...))
            private$include       = include
            private$exclude       = exclude
            private$filter        = filter
            private$sort          = sort
            private$limit         = limit

            while(private$cli_position >= 1 && private$cli_position){
                data = private[[ paste0("fn_",private$cli_positions[private$cli_position]) ]]()
            }
            if( private$cli_position == 0 ) return( data )
            if( private$cli_position == -1 ) return()
            return()
        }
    ),

    private = list(

        api           = NULL,
        endpoint_name = NA,
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
            private$endpoint_name = NA
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
            if(!val$is_valid) cat("Invalid input. Try again.\n")
            return( val )
        },



        fn_greeting = function(){
            cat(
                "Welcome to the NAMC API CLI\n",
                "---------------------------\n",
                'Type "e" at any time to exit\n'
            )
            private$increment()
            invisible(self)
        },



        fn_endpoint = function(){
            private$endpoints = private$api$get_endpoints()

            if( is.na(private$endpoint_name) || !any(private$endpoint_name == private$endpoints) ){
                cat(
                    "\nDatasources:\n",
                    paste0("\t",
                        c(1:length(private$endpoints)),
                        ") ",
                        private$endpoints,
                        collapse = "\n"
                    )
                )
                is_valid = FALSE
                while( !is_valid ){
                    user_input = readline(prompt = "Enter a data source number: ")
                    res = private$validate( user_input, c(1:length(private$endpoints)), "numeric")
                    is_valid = res$is_valid
                    if( res$exit_required) return()
                }
                private$endpoint_name = private$endpoints[res$data]
                cat("Preparing to query (",res$data,")\n\n")
            }
            private$increment()
            invisible(self)
        },



        get_arg = function(arg, arg_info){
                is_valid = FALSE
                while( !is_valid ){
                    user_input = readline(prompt = paste0("Enter value(s) for ",arg,": "))
                    res = private$validate( user_input = user_input, type = ifelse(arg_info$is_numeric, "numeric", "") )
                    is_valid = res$is_valid
                    if( res$exit_required ) break
                }
                private$args[[arg]] = res$data
            return( res )
        },



        fn_args = function(){
            all_args = private$api$get_endpoint_args( private$endpoint_name, no_paging = TRUE )

            if( length( private$args ) == 0 && length(all_args) != 0 ){
                cat("\nSeparate multiple values with commas(,).\n")
                required_args = c()
                for(arg in all_args){
                    arg_info = private$api$schema$get_argument( private$endpoint_name, arg)
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

                        arg = pther_args[ res$data ]
                        arg_info = private$api$schema$get_argument( private$endpoint_name, arg)
                        res = private$get_arg(arg, arg_info)
                        if( res$exit_required ) return()
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



        fn_exec = function(){

            private$complete()

            return(
                query(
                  api_endpoint = private$endpoint_name,
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
