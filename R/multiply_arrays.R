#' Multiply arrays based on dimension-names, using recycling
#'
#' Given two arrays with a full set of named dimension-names that
#' match in a proper fashion, this function will create the product of
#' them based on matching dimension-names.  If necessary, an
#' intermediate step will be performed to properly align the arrays.
#' If one array has fewer dimensions than the other, then the product
#' will be performed by recycling of vectors.
#'
#' @param .arr1 The first array in the product, if \code{.arr2} has
#'     the same size (but not necessarily the same shape), the result
#'     will be given in the shape of \code{.arr1}.  The case where
#'     \code{.arr1} has length 1 (as a vector) will be treated as a
#'     corner case and that single number will then be multiplied with
#'     \code{.arr2} without taking into account any requirements
#'     related to the dimension-names used on \code{.arr1}.
#'
#' @param .arr2 The second array in the product.  The case where
#'     \code{.arr2} has length 1 (as a vector) will be treated as a
#'     corner case and that single number will then be multiplied with
#'     \code{.arr1} without taking into account any requirements
#'     related to the dimension-names used on \code{.arr2}.
#'
#' @param keep_shape A logic argument, default value \code{FALSE} that
#'     only is used when recycling is needed in order to perform the
#'     product.  If the shape of the resulting array doesn't matter,
#'     then the default will avoid an extra permutation at the end of
#'     the computation.
#'
#' @return When \code{.arr1} and \code{.arr2} have the same size, the
#'     result will be the product of them in the shape of
#'     \code{.arr1}.  If one of them has fewer dimensions than the
#'     other, then recycling will be performed and the result will be
#'     given in the shape of the largest one.  It is allowed to have
#'     as a corner case that one (or both) of the array-arguments can
#'     be a length one vector, in which case a standard product will
#'     be performed.
#'
#' @export

multiply_arrays <- function(.arr1, .arr2, keep_shape = FALSE) {
    ##  Investigate if a corner case has been encountered, i.e. where
    ##  one of the arrays is given as a vector of length 1.
    if (length(.arr1) == 1) {
        attributes(.arr1) <- NULL
        return(.arr1 * .arr2)
    }
    if (length(.arr2) == 1) {
        attributes(.arr2) <- NULL
        return(.arr1 * .arr2)
    }
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
            .arr1 *
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
###  In order to perform the product by using recycling, the two
###  arrays must (if necessary) be adjusted so the dimensions of
###  'smaller' match the first part of the dimensions of 'larger',
###  then we need to perform the product as vectors, and then we need
###  to convert back to an array again - with the posibility that we
###  need to retransform back to the size of the original larger
###  array (depending on the value of 'keep_shape'.
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
    ##  only on names but on content too!)
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
    ##  Perform the product as vectors, and convert the result back to
    ##  the present shape of 'larger'.
    result <- array(
        data = as.vector(larger) * as.vector(smaller),
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


#####   2015-12-20
##  The stuff below is from an old file 'sanity_check_code' that now
##  has been extirpated.  There's a few details here that might be of
##  interest to look upon when creating examples for the present
##  function.



## ## ################################################################################
## ## #####  2014-11-05

## ## ##  Mostly obsolete now, but I think I would like to include in some
## ## ##  documentation an explanation about the multiplication of
## ## ##  "different-sizes" arrays as vectors.  Moreover, I also need to
## ## ##  keep in mind that I want to investigate if aa_restrict should be
## ## ##  modified to update arrays stored in environments.  I think that
## ## ##  might resolve some problems, but I am not sure...  First of all I
## ## ##  should investigate that 'foreach' when used in parallel would
## ## ##  update the common environment -- if that's not the case I
## ## ##  anticipate this to be more or less a dead end.


## ## ################################################################################
## ## #####  2014-09-09
## ## ##  This file will contain a collection of "toy-examples" that has
## ## ##  been performed in order to check that the different parts of the
## ## ##  code I have writen does behave like I want it to do.

## ## ###-------------------------------------------------------------------
## ## ##  Strategy for array-multiplication, where recycling is used in
## ## ##  order to (hopefully) avoid using more memory than necessary.

## ## ##  Origin: We need to multiply all the lags with the
## ## ##  cos.omega.h-array.  The idea is to see if we by a sensible
## ## ##  structure on the larger array, i.e. the two first dimensions must
## ## ##  match the dimension of cos.omega.h, can use recycling. The
## ## ##  following toy-example below shows that this approac is feasible.

## ## ###-------------------------------------------------------------------
## ## ##  I suppose I should check out that we have the desired behaviour on
## ## ##  a simple toy example.

## ## toy.array1 <- array(1:30, dim = c(5, 3, 2),
## ##                     dimnames =
## ##                     list(first = letters[1:5],
## ##                          second = LETTERS[1:3],
## ##                          third = letters[25:26]))

## ## ## , , third = y

## ## ##      second
## ## ## first A  B  C
## ## ##     a 1  6 11
## ## ##     b 2  7 12
## ## ##     c 3  8 13
## ## ##     d 4  9 14
## ## ##     e 5 10 15

## ## ## , , third = z

## ## ##      second
## ## ## first  A  B  C
## ## ##     a 16 21 26
## ## ##     b 17 22 27
## ## ##     c 18 23 28
## ## ##     d 19 24 29
## ## ##     e 20 25 30

## ## as.vector(toy.array1)
## ## ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
## ## ## [26] 26 27 28 29 30

## ## ##  OK, this looked like I expected it to do.

## ## toy.array2 <- array(1:15, dim = c(5, 3),
## ##                     dimnames = list(first = letters[1:5], second = LETTERS[1:3]))

## ## ##      second
## ## ## first A  B  C
## ## ##     a 1  6 11
## ## ##     b 2  7 12
## ## ##     c 3  8 13
## ## ##     d 4  9 14
## ## ##     e 5 10 15

## ## as.vector(toy.array1)
## ## ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
## ## ## [26] 26 27 28 29 30


## ## ##  An attempt at multiplying the two arrays directly does of course fail:

## ## toy.array1 * toy.array2
## ## ## Error in toy.array1 * toy.array2 : non-conformable arrays

## ## ##  However: By multiplying them as vectors (using recycling), and the
## ## ##  wrapping that result back into an array of the desired format, we
## ## ##  are done.

## ## toy.result1 <- array(data = as.vector(toy.array1) * as.vector(toy.array2),
## ##                      dim = dim(toy.array1),
## ##                      dimnames = dimnames(toy.array1))
## ## ## , , third = y

## ## ##      second
## ## ## first  A   B   C
## ## ##     a  1  36 121
## ## ##     b  4  49 144
## ## ##     c  9  64 169
## ## ##     d 16  81 196
## ## ##     e 25 100 225

## ## ## , , third = z

## ## ##      second
## ## ## first   A   B   C
## ## ##     a  16 126 286
## ## ##     b  34 154 324
## ## ##     c  54 184 364
## ## ##     d  76 216 406
## ## ##     e 100 250 450

## ## ##  OK, that looked exactly like I hoped it would.

## ## ###-------------------------------------------------------------------
## ## ##  Problem: Sometimes it would be nice to "stack" an existing array
## ## ##  into a larger one, in order to use the recycling of shorter
## ## ##  vectors that R does.  A function was created after some
## ## ##  experimentation, that inserts an extra dimension as a new second
## ## ##  component in an array.  Why the second component?  The reason is
## ## ##  that I would like to use recycling when multiplying different
## ## ##  arrays together, and as such (with regard to the other code that
## ## ##  is used) it seems to be the simplest solution to have this as the
## ## ##  second dimension.

## ## ##  Here follows an example of how it works.

## ## toy.array2
## ## ##      second
## ## ## first A  B  C
## ## ##     a 1  6 11
## ## ##     b 2  7 12
## ## ##     c 3  8 13
## ## ##     d 4  9 14
## ## ##     e 5 10 15

## ## new.dim2.names <- c("Tukey", "Parzen", "Bartlett")

## ## new_second_dim(orig_arr = toy.array2, new2.names = new.dim2.names)
## ## ## , , second = A

## ## ##      new.dim2.names
## ## ## first Tukey Parzen Bartlett
## ## ##     a     1      1        1
## ## ##     b     2      2        2
## ## ##     c     3      3        3
## ## ##     d     4      4        4
## ## ##     e     5      5        5

## ## ## , , second = B

## ## ##      new.dim2.names
## ## ## first Tukey Parzen Bartlett
## ## ##     a     6      6        6
## ## ##     b     7      7        7
## ## ##     c     8      8        8
## ## ##     d     9      9        9
## ## ##     e    10     10       10

## ## ## , , second = C

## ## ##      new.dim2.names
## ## ## first Tukey Parzen Bartlett
## ## ##     a    11     11       11
## ## ##     b    12     12       12
## ## ##     c    13     13       13
## ## ##     d    14     14       14
## ## ##     e    15     15       15


## ## ##  rm(toy.array1, toy.array2, toy.result1)
## ## ################################################################################

## ## ###-------------------------------------------------------------------

## ## ## What If i want to convert something to a data-table, upon which we
## ## ## can use functions from one of the packages plyr, dplyr or
## ## ## data.table.

## ## ##  The following is a function from the base-package
## ## as.data.frame.table(toy.result1)
## ## ##    first second third Freq
## ## ## 1      a      A     y    1
## ## ## 2      b      A     y    4
## ## ## 3      c      A     y    9
## ## ## 4      d      A     y   16
## ## ## 5      e      A     y   25
## ## ## 6      a      B     y   36
## ## ## 7      b      B     y   49
## ## ## 8      c      B     y   64
## ## ## 9      d      B     y   81
## ## ## 10     e      B     y  100
## ## ## 11     a      C     y  121
## ## ## 12     b      C     y  144
## ## ## 13     c      C     y  169
## ## ## 14     d      C     y  196
## ## ## 15     e      C     y  225
## ## ## 16     a      A     z   16
## ## ## 17     b      A     z   34
## ## ## 18     c      A     z   54
## ## ## 19     d      A     z   76
## ## ## 20     e      A     z  100
## ## ## 21     a      B     z  126
## ## ## 22     b      B     z  154
## ## ## 23     c      B     z  184
## ## ## 24     d      B     z  216
## ## ## 25     e      B     z  250
## ## ## 26     a      C     z  286
## ## ## 27     b      C     z  324
## ## ## 28     c      C     z  364
## ## ## 29     d      C     z  406
## ## ## 30     e      C     z  450


## ## ##  Alternatively, using reshape2::melt to do the same.

## ## melt(toy.result1)
## ## ##    first second third value
## ## ## 1      a      A     y     1
## ## ## 2      b      A     y     4
## ## ## 3      c      A     y     9
## ## ## 4      d      A     y    16
## ## ## 5      e      A     y    25
## ## ## 6      a      B     y    36
## ## ## 7      b      B     y    49
## ## ## 8      c      B     y    64
## ## ## 9      d      B     y    81
## ## ## 10     e      B     y   100
## ## ## 11     a      C     y   121
## ## ## 12     b      C     y   144
## ## ## 13     c      C     y   169
## ## ## 14     d      C     y   196
## ## ## 15     e      C     y   225
## ## ## 16     a      A     z    16
## ## ## 17     b      A     z    34
## ## ## 18     c      A     z    54
## ## ## 19     d      A     z    76
## ## ## 20     e      A     z   100
## ## ## 21     a      B     z   126
## ## ## 22     b      B     z   154
## ## ## 23     c      B     z   184
## ## ## 24     d      B     z   216
## ## ## 25     e      B     z   250
## ## ## 26     a      C     z   286
## ## ## 27     b      C     z   324
## ## ## 28     c      C     z   364
## ## ## 29     d      C     z   406
## ## ## 30     e      C     z   450

## ## ##  Well, that was more or less the same, but I think I should go for
## ## ##  the melt-variant since the package anyway already is included.

## ## ##  The mayor question would now be if we can expect it to be faster
## ## ##  to melt and then do the sum, than to do it directly using adply()
## ## ##  or whatever would be the correct version to use.


## ## ##  It might be the case that we could use reshape2::acast also, so
## ## ##  let's try that just for the heck of it.

## ## acast(data = melt(toy.result1),
## ##       formula = second ~ third,
## ##       fun.aggregate = sum)

## ## ##     y    z
## ## ## A  55  280
## ## ## B 330  930
## ## ## C 855 1830

## ## ##  Compared against the original structure, this does indeed look
## ## ##  like the desired result.  It might perhaps not be the most
## ## ##  efficient way to do that, but the different approaches with regard
## ## ##  to this could be compared later on.  (The reshape2 belong to the
## ## ##  Hadleyverse

## ## toy.result1
## ## ## , , third = y

## ## ##      second
## ## ## first  A   B   C
## ## ##     a  1  36 121
## ## ##     b  4  49 144
## ## ##     c  9  64 169
## ## ##     d 16  81 196
## ## ##     e 25 100 225

## ## ## , , third = z

## ## ##      second
## ## ## first   A   B   C
## ## ##     a  16 126 286
## ## ##     b  34 154 324
## ## ##     c  54 184 364
## ## ##     d  76 216 406
## ## ##     e 100 250 450

## ## ###-------------------------------------------------------------------

