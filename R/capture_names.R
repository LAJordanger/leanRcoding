################################################################################
#'
#' Get the names of environments, lists and vectors.
#'
#' A helper function created to simplify the code in other functions,
#' primarily where an argument either can be given as an environment
#' or as a list/vector -- and where it's required to use the names in
#' some way or another.
#'
#' @param obj The environment, list or vector under investigation.
#'
#' @return A vector containing the names from \code{obj}.  Note that
#'     this can have length zero, i.e. being \code{character(0)} --
#'     which happens when used upon an empty environment or on named,
#'     but empty, lists and vectors.  \code{NULL} will be returned
#'     when used upon a list or vector without a names attributes.
#'
#' @export

capture_names <- function(obj) {
    ##  Sanity-check the argument 'obj'.
    if (! any(is.vector(obj),  # Tests both 'vector' and 'list',
              is.environment(obj)))
        error(.argument = "obj",
              c(sQuote("obj"),
                "must be an environment, a list or a vector."))
    ##  Extract and return the names 
    if (is.environment(obj)) {
        ls(obj, all.names = TRUE)
    } else {
        names(obj)
    }
}


## test_list <- list(a = 3, "b")
## capture_names(test_list)

## test_env <- new.env()
## capture_names(test_env)

## test_list <- list(a = 3)
## test_list$a <- NULL
## capture_names(test_list)

## test_vec <- c(a = 3)
## test_vec <- test_vec[-1]
## capture_names(test_vec)
