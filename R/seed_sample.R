################################################################################

#' A helper when creating seed-values
#'
#' This function creates a vector with one or more seeds, which often
#' will be needed in order to ensure that simulations done in parallel
#' are reproducible.  In addition some attributes will be added to
#' document the random number generator used to obtain the values.
#' See the help-page of \code{Random} for details about the three
#' arguments \code{kind}, \code{normal.kind} and \code{vstr}.
#' 
#'
#' @param n An integer that specifies how many seeds that should
#'     be generated.  The default value is \code{1}.
#'
#' @param .seed An initial seed to be used to ensure reproducibility
#'     of the result from this function, and this value will be an
#'     attribute of the resulting vector.  The default value for this
#'     argument is \code{NULL}, and when left unchanged a value will
#'     be sampled and used for this purpose.
#'
#' @param kind Character or \code{NULL}.  If \code{kind} is a
#'     character string, set R's RNG to the kind desired.
#' 
#' @param normal.kind Character string or \code{NULL}.  If it is a
#'     character string, set the method of Normal generation.
#' 
#' @param vstr A character string containing a version number of
#'     R. The default value \code{NULL} will use the present version
#'     number.
#'
#' @return A vector of seeds between the highest and lowest available
#'     integer, i.e. between \code{-2147483647} and \code{2147483647}
#'     (the value used is the result of \eqn{2^31-1}.)  Attributes
#'     will be added in order to recreate the vector.  NB: If \code{n}
#'     is given as \code{1}, and \code{.seed} has been specified, then
#'     the returned value will be that value with the collection of
#'     attributes added to it.
#'
#' @export


seed_sample <- function(n  = 1,
                        .seed = NULL,
                        kind = NULL,
                        normal.kind = NULL,
                        vstr = NULL) {
###-------------------------------------------------------------------
    ##  Find present version of R and active kinds.
    .R_vstr <- paste(R.version$major, R.version$minor, sep = ".")
    .active_kinds <- structure(
        .Data = RNGkind(),
        .Names = c("kind", "normal.kind"))
###-------------------------------------------------------------------
    ##  Update 'kind', 'normal.kind' and 'vstr' if unspecified.
    if (is.null(kind))
        kind <- unname(.active_kinds["kind"])
    if (is.null(normal.kind))
        normal.kind <- unname(.active_kinds["normal.kind"])
    if (is.null(vstr)) 
        vstr <- .R_vstr
###-------------------------------------------------------------------
    ##  Adjust the setting based on the values, locally in this
    ##  function only.
    RNGversion(vstr)
    on.exit(RNGversion(.R_vstr))
    ##
    RNGkind(kind = kind,
            normal.kind = normal.kind)
    on.exit(RNGkind(kind = .active_kinds["kind"],
                    normal.kind = .active_kinds["normal.kind"]))
###-------------------------------------------------------------------
    ##  Compute the highest available integer
    max_integer <- 2^31 - 1
    ##  Extract the digits.
    .digits <- as.integer(unlist(
        strsplit(x = as.character(max_integer),
                 split = "")))
###-------------------------------------------------------------------
    ##  Create a help-function to find an integer in the range from
    ##  the lowest to the highest possible.
    help_fun <- function() {
        .first <- sample(0:.digits[1], size = 1)
        .result <- .first
        .continue <- TRUE
        i <- 2
        while (.continue) {
            if (identical(x = .result[i - 1],
                          y = .digits[i - 1])) {
                .result <- c(.result,
                             sample(0:.digits[i],
                                    size = 1))
                if (i == length(.digits)) {
                    .continue <- FALSE
                } else
                    i <- i + 1
            } else {
                .result <- c(.result,
                             sample(0:9,
                                    size = length(.digits) - i + 1,
                                    replace = TRUE))
                .continue <- FALSE
            }
        }
        ##  Create and return the result
        as.integer(paste(
            c(sample(c("+", "-"), size = 1),
              .result),
            collapse = ""))
    }
###-------------------------------------------------------------------
    ##  Compute '.seed' if necessary, then apply it.
    if (is.null(.seed)) 
        .seed <- help_fun()
    ##  
    set.seed(seed = .seed)
###-------------------------------------------------------------------
    ##  Compute and return the desired vector of seed-values, with
    ##  attributes required to recreate this later on.
    structure(
        .Data =
            if (n == 1) {
                .seed
            } else
                replicate(n = n,
                          expr = help_fun(),
                          simplify = "vector"),
        n = n,
        .seed = .seed,
        kind = kind,
        normal.kind = normal.kind,
        vstr = vstr)
}
