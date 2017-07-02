################################################################################
#####  2015-05-14

#' Temporary local version of 'aaply'
#'
#' The 'aaply' function did for some reason not want to cooperate for
#' one of the cases I needed to investigate, so an ad-hoc solution had
#' to be created in order to get a work-around.  The result of this
#' function has been sanity-checked with the result from 'aaply' for
#' one of the cases where 'plyr::aaply' did work.  Note that even
#' though the order of the dimensions are ensured to be the same, the
#' internal order in some of the dimensions might differ, so use
#' \code{restrict_array} to permute them into the same order.
#'
#' Note that the result from functions that returns matrices/arrays
#' probably will look like shit if used in this first coarse
#' approximation.  It might be possible to create a more advanced
#' version of this function by using 'abind' instead of 'rbind' in the
#' construction, but as I at the moment don't need such funcitonality,
#' I will simply ignore it for the time being.
#'
#' It might perhaps be possible to adjust this function in order to do
#' bigger chunks when sending stuff to the available cores, which
#' might reduce the overhead a bit.  But this will require another
#' design of the function, which I can't use time on.
#'
#' @param .arg_grid A data-frame, typically created by
#'     \code{expand.grid} used on a list containing references to the
#'     dimension names of some array containing values which we want
#'     \code{.fun} to work upon.
#'
#' @param .fun The function that we want to use. The first argument of
#'     \code{.fun} must be able to digest a row from the data-frame
#'     \code{.arg_grid}.
#'
#' @param .new_dnn The name to be used on the dimension of the result
#'     that originates from \code{.fun}.
#'
#' @param .new_dn A vector to be used upon the result that originates
#'     from \code{.fun}.  The default value \code{NULL} will trigger
#'     the use of the names returned by \code{.fun}.  The main
#'     intention of this function is to add a name when the result has
#'     length one and no names.
#' 
#' @param ...  Arguments to be given to \code{.fun}.
#'
#' @return For functions that return a vector of values, the returned
#'     object will be similar to the result obtained by
#'     \code{plyr::aaply}, with the difference that the new dimension
#'     will be named "values".
#' 
#' @export

my_aaply <- function(.arg_grid,
                     .fun,
                     .new_dnn = "values",
                     .new_dn = NULL,
                     ...) {
    ##  Investigate if '.arg_grid' contains only one row.
    .only_one_row <- dim(.arg_grid)[1] == 1
    ##  Create an internal help-function.
    .help <- function(i) {
        .row <- .arg_grid[i, ]
        .fun(.row, ...)
    }
    ##  Use '.fun' to create a matrix with one column for each result.
    .result <-
        if (.only_one_row) {
            .fun(as.vector(.arg_grid), ...)
        } else 
            foreach(
                i = 1:dim(.arg_grid)[1], 
                .combine = rbind,
                .inorder = TRUE) %dopar% .help(i)
    ##  Inspect '.arg_grid' to extract old dimnames and dimensions.
    .old_dimnames <- lapply(
        X = .arg_grid,
        FUN = unique)
    .old_dim <- vapply(
        X = .old_dimnames,
        FUN = length,
        FUN.VALUE = integer(1),
        USE.NAMES = FALSE)
    ##  Create the new named dimension.
    new_dimnames <- structure(
        .Data =
            if (! is.null(.new_dn)) {
                list(.new_dn)
            } else
                list(
                    if (.only_one_row) {
                        names(.result)
                    } else 
                        colnames(.result)),
        names = .new_dnn)
    ##  Adjust '.result' and return it to the work-flow.
    structure(
        .Data = as.vector(.result),
        dim = c(
            .old_dim,
            if (.only_one_row) {
                length(.result)
            } else
                dim(.result)[2]),
        dimnames = c(
            .old_dimnames,
            new_dimnames))
}
