################################################################################
#####  2014-11-17

#' Intersect the dimension-names of two arrays.
#'
#' When a part of one array should be extracted and included into
#' another array, one might need to use an intersection of the
#' dimension-names approach to figure out the correct argument to be
#' used by \code{restrict_array}.  Note that the order of the
#' arguments does matter, since it's \code{dimnames1} that decides the
#' format of the resulting intersection.  Warning: The result of this
#' function can be an empty list, so keep in mind that your code can
#' deal with that if you want to use this in a function.
#'
#' @param dimnames1 The dimension names of the first array, the order
#' of the names of the dimension-names of the result will be decided
#' by this.
#'
#' @param dimnames2 The dimension names of the second array.  
#'
#' @return A list with the common parts from \code{dimnames1} and
#' \code{dimnames2} is returned.  The result will an empty list when
#' no dimension-names are common, but it will also be empty if a common
#' dimension-name has no common values stored.
#'
#' @export


dimnames_intersect <- function(dimnames1, dimnames2) {
###-------------------------------------------------------------------
    ##  Create a new list based on the intersection of names.
    intersect_dn <-
        dimnames1[intersect(
            x = names(dimnames1),
            y = names(dimnames2))]
###-------------------------------------------------------------------
    ##  Intersect the common dimensions.
    for (name in names(intersect_dn)) 
        intersect_dn[[name]] <- intersect(
            x = dimnames1[[name]],
            y = dimnames2[[name]])
###-------------------------------------------------------------------
    ##  Set the result to an empty list if any of the intersected
    ##  dimensions have length zero.
    if (any(vapply(X = intersect_dn,
                   FUN = length,
                   FUN.VALUE = numeric(1)) == 0))
        intersect_dn <- list()
###-------------------------------------------------------------------
    ##  Return the result
    return(intersect_dn)
}

