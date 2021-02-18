#' Initialize package
#'
#' @param libnam Name of the library the package is installed in.
#' @param pkgname The name of the package (NAMCr)
#'
#' @return none No return
#'
.onLoad = function(libnam, pkgname) {

    #c = yaml::yaml.load_file("./data-raw/config.yml")
    c = pkg.config()

    c$api$.auth = namc_oauth2$new( argList = c$auth )

    pkg.globals$api <<- namc_api$new( argList = c$api )

}