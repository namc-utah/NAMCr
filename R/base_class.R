#' @title base_class
#' @description R6 class for providing additional default R6 class features
#' @return a `base_class` class (R6 class)
#' @examples
base_class = R6::R6Class(
  "base_class",
  portable = TRUE,

  public = list(



    #' initialize - Allow list based function initialization (easier to integrate with config files)
    #'
    #' @param argList
    #' @param ...
    #'
    #' @return
    #' @export
    #'
    #' @examples
    initialize = function(argList=NULL,...){
      if(is.null(argList)) argList = list()
      private$.set_from_list( modifyList(argList, list(...)) )
    },



    #' dump - for debugging internal class values (puts var named tmpVar in global environment)
    #'
    #' @param val
    #'
    #' @return
    #' @export
    #'
    #' @examples
    dump = function(val){
      assign("tmpVar", val, envir = .GlobalEnv)
      return(val)
    },



    #' Title
    #'
    #' @param var_name
    #'
    #' @return
    #' @export
    #'
    #' @examples
    get_var = function(var_name){
      return( private[[var_name]] )
    },



    #' Title
    #'
    #' @param var_name
    #' @param var_value
    #'
    #' @return
    #' @export
    #'
    #' @examples
    set_var = function(var_name, var_value){
      private[[var_name]] = var_value
      invisible(self)
    }
  ),



  private = list(
    .silent = FALSE,


    #' .set_from_list - sets all public and private fields from a name,value list
    #'
    #' @param argList
    #'
    #' @return
    #' @export
    #'
    #' @examples
    .set_from_list = function(argList){
      if(length(argList) > 0){
        for(arg in names(argList)){
          if(exists(arg,self)){
            self[[arg]] = argList[[arg]]

          } else if (exists(arg,private)){
            private[[arg]] = argList[[arg]]

          }
        }
      }
    }
  )
)
