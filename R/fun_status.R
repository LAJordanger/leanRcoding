################################################################################
#'
#' Function status, exported or internal.
#'
#' A little helper to investigate if a given function from a given
#' package belongs to the part that has been exported or imported.
#' Note that this function assumes that the namespace of the package
#' has been loaded, and that the function is known to exist inside of
#' it.  The reason for this simplifying assumptions is due to the way
#' this function is used at the moment of writing.
#'
#' @param .fun The name of the function to investigate, given as a
#'     character-string.
#'
#' @param .package The name of the package \code{.fun} originates
#'     from.
#'
#' @return Eiter "exported" or "internal", to reveal the status of the
#'     package.
#'
##  #' @export

fun_status <- function(.fun, .package) {
#####  TASK: Include some code in order to add sanity-checks later on.
    ## ##  Find the loaded namespaces.
    ## .the_Namespaces <- sort(loadedNamespaces())
    ## ##  Find the content of the package-environment.
    ## .env_package <- asNamespace(.package)
    ## content <- eval(epackagepr = .ls_call,
    ##                 envir = .env_package)
###-------------------------------------------------------------------

    ##  Assuming no problems, the test is simply
    ifelse(test = .fun %in% getNamespaceExports(.package),
           yes  = "exported",
           no   = "internal")
}


#####  Reminder: See the code of `the_Namespaces` for a more extensive
#####  code.
