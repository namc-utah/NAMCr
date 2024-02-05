#' @title base_class
#' @description R6 class for providing additional default R6 class features
#' @return a `base_class` class (R6 class)
#' @export
#' @examples
#'
#' new_class = R6::R6Class(
#'   "new_class",
#'   inherit = base_class,
#'   portable = TRUE,
#'
#'   public = list(
#'     ...
#'   ),
#'   private = list(
#'     ...
#'   )
#' )
#'
base_class = R6::R6Class(
  "base_class",
  portable = TRUE,

  public = list(



    #' #Initialize object
    #'
    #' #Allow list based function initialization (easier to integrate with config files)
    #'
    #' @param argList A 'List' of name/value pairs to be passed in as arguments.
    #' @param ... Name/Value pairs as arguments.
    #'
    #' @return none Nothing is returned
    #'
    initialize = function(argList=NULL,...){
      if(is.null(argList)) argList = list()
      private$.set_from_list( utils::modifyList(argList, list(...)) )
    },



    #' #Dump object values
    #'
    #' #Convenient for debugging private class values (puts var named tmpVar in global environment)
    #'
    #' @return private The private environment of the object
    #'
    #' @examples
    #'
    #' obj = some_R6_class$new()
    #' obj$dump()
    #' data = tmpVar$some_private_var_name
    #' data = tmpVar[["some_private_var_name"]]
    #'
    dump = function(){
      assign("tmpVar", private, envir = .GlobalEnv)
      return( private )
    },



    #' #Get variable
    #'
    #' #Gets a private variables value.
    #'
    #' @param var_name The name of the private variable to return.
    #'
    #' @return var The value of the requested variable.
    #'
    #' @examples
    #'
    #' obj = some_R6_class$new()
    #' var_value = obj$get_var("var_name")
    #'
    get_var = function(var_name){
      return( private[[var_name]] )
    },



    #' #Set variable
    #'
    #' #Sets a private variables value.
    #' #This method is chainable.
    #'
    #' @param var_name The name of the private variable to set.
    #' @param var_value The value to assign to the desired private variable.
    #'
    #' @return object_ref The reference to the original object.
    #'
    #' @examples
    #'
    #' obj = some_R6_class$new()
    #' obj$set_var("var_name", var_value)
    #'
    set_var = function(var_name, var_value){
      private[[var_name]] = var_value
      invisible(self)
    }
  ),



  private = list(
    .silent = FALSE,


    # ---------------------------------------------------------------------------
    # .set_from_list - sets all public and private fields from a name,value list
    # ---------------------------------------------------------------------------
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
