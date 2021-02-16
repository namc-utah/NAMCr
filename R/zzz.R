#' Title
#'
#' @param libnam
#' @param pkgname
#'
#' @return
#' @export
#'
#' @examples
.onLoad = function(libnam, pkgname) {

    c = yaml::yaml.load_file("./data-raw/config.yml")
    c$api$.auth = namc_oauth2$new( argList = c$auth )

    pkg.globals$api <<- namc_api$new( argList = c$api )

}