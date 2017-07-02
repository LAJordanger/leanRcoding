################################################################################
#####  2014-11-30

#' Add arrays based on dimension-names, using recycling
#'
#' Given two arrays with a full set of named dimension-names that
#' match in a proper fashion, this function will create the sum of
#' them based on matching dimension-names.  If necessary, an
#' intermediate step will be performed to properly align the arrays.
#' If one array has fewer dimensions than the other, then the product
#' will be performed by recycling of vectors.
#'
#' @param .arr1 The first array in the sum, if \code{.arr} has the
#' same size (but not necessarily the same shape), the result will be
#' given in the shape of \code{.arr1}
#'
#' @param .arr2 The second array in the sum.
#'
#' @param keep_shape A logig argument, default value \code{FALSE} that
#' only is used when recycling is needed in order to perform the sum.
#' If the shape of the resulting array doesn't matter, then the
#' default will avoid an extra permutation at the end of the
#' computation.
#'
#' @return When \code{.arr1} and \code{.arr2} have the same size, the
#' result will be the sum of them in the shape of \code{.arr1}.  If
#' one of them has fewer dimensions than the other, then recycling
#' will be performed and the result will be given in the shape of the
#' largest one.
#'
#' @export


add_arrays <- function(.arr1, .arr2, keep_shape = FALSE) {
###-------------------------------------------------------------------
    ##  Initial sanity check of dimension-names, are they there?
    dn.arr1 <- dimnames(.arr1)
    dn.arr2 <- dimnames(.arr2)
    ##---
    if (any(identical(dn.arr1, NULL),
            identical(dn.arr2, NULL)))
        error(.argument = c(".arr1", ".arr2"),
              "The arrays must have dimension-names.")
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Check that the dimensions are complete, i.e. the dimensions that
###  are common should have the same length.  Use 'dimnames_intersect'
###  for this, remember that the order of the arguments matter.  The
###  strategy is to compute both of the intersection, and then compare
###  them with the first argument.  If both are TRUE it's an indicator
###  that the two arrays have the same size (although not necessarily
###  the shame shape).  If only one of them is TRUE, we should use
###  recycling of the shorter one, and if none of them is TRUE then an
###  error must be returned.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Compute the intersect-booleans.
    intersect1 <- identical(
        x = dimnames_intersect(
            dimnames1 = dn.arr1,
            dimnames2 = dn.arr2),
        y = dn.arr1)
    ##---
    intersect2 <- identical(
        x = dimnames_intersect(
            dimnames1 = dn.arr2,
            dimnames2 = dn.arr1),
        y = dn.arr2)
###-------------------------------------------------------------------
    ##  Sanity-check, if both is false, something is wrong.
    if (! any(intersect1, intersect2) )
        error(.argument = c(".arr1", ".arr2"),
              "The arrays are not compatible,")
###-------------------------------------------------------------------
    ##  Record the class-attributes in order to use them on the
    ##  resulting array.
    class.arr <- unique(c(class(.arr1), class(.arr2)))
###-------------------------------------------------------------------
    ##  If both the intersect-booleans are TRUE, adjust the second
    ##  argument to match the first, i.e. we do not only need the
    ##  dimensions to be in the same order, but we must also guarantee
    ##  that the internal ordering of the dimensions coincide.  The
    ##  tweaking is taken care of by 'restrict_array'.
    if (all(intersect1, intersect2))
        return({
            .arr1 + 
                restrict_array(.arr = .arr2,
                               .restrict = dn.arr1,
                               .permute = TRUE)
        })
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The recycling-case remains.  A sanity-check must be performed in
###  order to see that the dimensions of the array whose indicator
###  returned TRUE match those from the larger array.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Rename '.arr1' and '.arr2' to 'smaller' and 'larger', dependig
    ##  on the indicators 'intersect1' and 'intersect2'
    if (intersect1) {
        smaller <- .arr1
        larger <- .arr2
    } else {
        smaller <- .arr2
        larger <- .arr1
    }
    ##---  KIT
    rm(.arr1, .arr2, dn.arr1, dn.arr2, intersect1, intersect2)
###-------------------------------------------------------------------
    ##  Sanity check the dimension-names of 'smaller' with regard to
    ##  completeness of dimensions compared with 'larger', using
    ##  'dimnames_intersect' once more as our tool on a restricted
    ##  version of the dimension-names of 'larger'.
    complete <- identical(
        x = dimnames(larger)[names(dimnames(smaller))],
        y = dimnames_intersect(
            dimnames1 = dimnames(larger)[names(dimnames(smaller))],
            dimnames2 = dimnames(smaller)))
    ##---
    if (! complete)
        error(c(
            "Recycling of a smaller array is only performed if the length of the common",
            "dimension-names coincide.  Use",
            sQuote("restrict_array"),
            "on the larger array before",
            "calling this function if this is what you want to do."))
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  In order to perform the sum by using recycling, the two arrays
###  must (if necessary) be adjusted so the dimensions of 'smaller'
###  match the first part of the dimensions of 'larger', then we need
###  to perform the product as vectors, and then we need to convert
###  back to an array again - with the posibility that we need to
###  retransform back to the size of the original larger array
###  (depending on the value of 'keep_shape'.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  If 'keep_shape' is 'TRUE', record the dimnames of 'larger' in
    ##  order to use them for the final adjustment.
    if (keep_shape)
        final_dimnames <- dimnames(larger)
###-------------------------------------------------------------------
    ##  If 'smaller' already "match" the beginning of 'larger', but a
    ##  permutation is needed, then permute 'smaller', otherwise
    ##  permute 'larger' (and remember we want the match to be not
    ##  only on names but on content too.)
    smaller_in_larger <- match(
        x = names(dimnames(smaller)),
        table = names(dimnames(larger)))
    ##---
    if (identical(x = max(smaller_in_larger),
                  y = length(smaller_in_larger))) {
        smaller <- restrict_array(
            .arr = smaller,
            .restrict = dimnames(larger)[names(dimnames(smaller))[smaller_in_larger]],
            .permute = TRUE)
    } else {
        larger <- restrict_array(
            .arr = larger,
            .restrict = dimnames(smaller),
            .permute = TRUE)
    }
###-------------------------------------------------------------------
    ##  Perform the sum as vectors, and convert the result back to
    ##  the present shape of 'larger'.
    result <- array(
        data = as.vector(larger) + as.vector(smaller),
        dim = dim(larger),
        dimnames = dimnames(larger))
###-------------------------------------------------------------------
    ##  If 'keep_shape' is 'TRUE' and the present dimension-names
    ##  doesn't match the desired 'final_dimnames', use
    ##  'restrict_array' to tweak it into the desired form.
    if (keep_shape)
        if (! identical(dimnames(result), final_dimnames))
            result <- restrict_array(
                .arr = result,
                .restrict = final_dimnames,
                .permute = TRUE)
###-------------------------------------------------------------------
    ##  Resurect the class-attributes
    class(result) <- class.arr
###-------------------------------------------------------------------
    ## Return the result.
    result
}



## #####  TEST
## .arr1 <- array(data = 1:(2*3*2) * 3,
##                dim = c(2, 3, 2),
##                dimnames = list(
##                    first = letters[1:2],
##                    second = LETTERS[5:7],
##                    third = letters[12:13]))
## ##---
## .arr2 <- array(data = 1:(2*2) * 5,
##                dim = c(2, 2),
##                dimnames = list(
##                    first = letters[1:2],
##                    third = letters[12:13]))
## ##---
## add_arrays(.arr1 = .arr1, .arr2 = .arr2, keep_shape = TRUE)
