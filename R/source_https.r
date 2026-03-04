#' Source R Scripts from HTTPS URLs
#'
#' Sources R scripts hosted on GitHub or other HTTPS URLs directly into the R environment.
#'
#' @param url The HTTPS URL of the R script to source
#' @param ... Additional URLs to source
#'
#' @details
#' Uses RCurl package to download and parse R scripts from HTTPS sources.
#' Useful for sourcing functions from GitHub repositories.
#'
#' @examples
#' \dontrun{
#' source_https("https://raw.github.com/user/repo/branch/file.R")
#' }
#'
#' @export
# from Tony Breyal
# http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
# to  source R scripts hosted on a github repository
source_https <- function(url, ...) {
        # load package
        require(RCurl)
        
        # parse and evaluate each .R script
        sapply(c(url, ...), function(u) {
                eval(
                        parse(
                                text = getURL(
                                        u, 
                                        followlocation = TRUE, 
                                        cainfo = system.file(
                                                "CurlSSL", 
                                                "cacert.pem",
                                                package = "RCurl")
                                        )
                                )
                        , envir = .GlobalEnv)
        })
}

# Example
# source_https("https://raw.github.com/rnj0nes/RNJmisc/master/RNJmisc.r")