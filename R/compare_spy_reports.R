################################################################################
#####  2014-10-13

#' Compare two spy-reports.
#'
#' This function compares two different spy-reports (created by
#' \code{spy}).
#'
#' @param first_report The first of the spy-reports.
#'
#' @param second_report The second of the spy-reports.
#'
#' @param ignore_these Some arguments of the targeted function might
#'     be irrelevant for the final result, e.g. the number of cores
#'     used in a parallel computation.  When the actual result of the
#'     computation are the focal point of interest for our comparison,
#'     such arguments should be included in the vector
#'     \code{ignore_these}.
#'
#' @return Either \code{TRUE} or \code{FALSE} will be returned,
#'     depending on whether or not the reports where equal or not.
#'     Note that equality in this context focus upon the results of
#'     the functions, and not upon whether or not it should also
#'     happen that all named arguments are identical. To be precise if
#'     we have a function \code{f(arg)} and in addition have
#'     \code{name1 = name2 = 1}, then the spy-reports from \code{f(arg
#'     = name1)} and \code{f(arg = name2)} will be considered to be
#'     equal.
#'
#' @export


compare_spy_reports <- function(
    first_report,
    second_report,
    ignore_these = character(0)) {
    ###-------------------------------------------------------------------
    ##  Do we have the same function in both reports?
    same_fun <- (first_report$fun == second_report$fun)
    ###-------------------------------------------------------------------
    ##  Check the environment-content of the two reports.
    first_content <- ls(envir = first_report$envir,
                        all.names = TRUE)
    second_content <- ls(envir = second_report$envir,
                         all.names = TRUE)
    ## If the content differs, then return 'FALSE'
    if (! identical(x = first_content,
                    y = second_content))
        return(FALSE)
    ###-------------------------------------------------------------------
    ##  Create the check-list, and compare the stored values.
    equal_values <- all(vapply(
        X = setdiff(x = first_content,
                    y = ignore_these),
        FUN = function(.arg) {
            identical(x = first_report$envir[[.arg]],
                      y = second_report$envir[[.arg]])
       },
        FUN.VALUE = logical(1)))
###-------------------------------------------------------------------
    ##  Return the answer
    return(all(same_fun, equal_values))
}

#####  TASK:(?)  I suppose it could be added some sanity-checks here,
#####  e.g. to ensure that 'ignore_these' should contain the full
#####  names of the arguments present in the formals of the function,
#####  but that might only be reasonable to do if called from the
#####  workspace.  Not to be prioritised at the moment.
