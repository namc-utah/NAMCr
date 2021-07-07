#' @title messages
#' @description R6 class for handling package messages
#' @return a `messages` class (R6 class)
#' @examples
#'
#' msg = messages$new(level = "INFO")
#'
messages = R6::R6Class(
    "messages",
    portable = TRUE,

    private = list(

        # @field levels is an array of the 4 level types.
        levels = c("NONE", "INFO", "WARNING", "DEBUG")

    ),

    public = list(

        #' @field iLevel is the index of the level contained in levels.
        iLevel = 2,

        #' Initialize object
        #'
        #' @param level The text message type to initialize messaging to
        #'
        #' @return none Nothing is returned
        #'
        initialize = function(level = 'INFO'){
            self$iLevel = which(level == private$levels)
        },



        #' Title
        #'
        #' @param msg string A message to display
        #' @param type string The type of the message
        #'
        #' @return
        #' @export
        #'
        #' @examples
        #'
        #' msg = messages::new(level = 'DEBUG')
        #' msg$display("this is a message")
        #'
        display = function(msg, type = "INFO"){

            iLevel = which(type == private$levels)

            if( self$iLevel >= iLevel ){
                if( iLevel <= 2 ){
                    cat(msg)
                } else {
                    message(msg)
                }

            }
            invisible(self)
        }

    )
)