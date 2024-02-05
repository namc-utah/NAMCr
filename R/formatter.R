#' @title formatter
#' @description R6 class for formatting strings for console or markup output
#' @return a `formatter` class (R6 class)
#' @examples
#'
#' f = formatter$new()
#' formatted_text = f$wrap_text("text","h1","example ")
#'
formatter = R6::R6Class(
    "formatter",
    portable = TRUE,

    private = list(

        styles = list(
            h1 = list(
                text = c('',':\n'),
                markdown = c('##','')
            ),
            h2 = list(
                text = c('\n\t',':\n'),
                markdown = c('###','')
            ),
            h3 = list(
                text = c('\t\t',':\n'),
                markdown = c('###','')
            ),
            h4 = list(
                text = c('\t\t\t',':\n'),
                markdown = c('####','')
            ),
            t1 = list(
                text = c('\t','\n\n'),
                markdown = c('','')
            ),
            l1 = list(
                text = c('\t\t\t',':\n'),
                markdown = c('* ','')
            ),
            k1 = list(
                text = c('',':\t'),
                markdown = c('* ','')
            ),
            k2 = list(
                text = c('',':\t\t'),
                markdown = c('* ','')
            )
        )

    ),

    public = list(

        #' #Initialize object
        #'
        #' @param level The text message type to initialize messaging to
        #'
        #' @return none Nothing is returned
        #'
        initialize = function(level = 'INFO'){
            self$iLevel = which(level == private$levels)
        },




        #' #format a string for console or markup output
        #'
        #' @param txt string to wrap in a style
        #' @param style string name of a styles key
        #' @param appendTo sting to appended the styled text to
        #'
        #' @return
        #' @export
        #'
        #' @examples
        #'
        #' txt = formatter::wrap_text("My Title","h1","")
        #'
        wrap_text = function(txt, style, appendTo = ''){

            return(
                paste0(
                    appendTo,
                    styles[[style]][[format]][1],
                    txt,
                    styles[[style]][[format]][2]
                )
            )
        }

    )
)