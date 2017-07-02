################################################################################
#####  2014-11-18

#' Restrict (or permute) an array.
#'
#' This function will restrict an array based upon a specified subset
#' of its dimension-names, without any need for the user to worry
#' about how these dimension-names are positioned within the array.
#' However, if the user desires that the resulting array should have
#' the specified order on the dimension-names, then the argument
#' \code{.permute} can be set to \code{TRUE} in order to achieve that
#' result.
#'
#' @details Arrays are awesome, but it can alas be agonising to keep
#'     track of the dimension-names.  This function frees the user
#'     from such frustrations.
#'
#' @param .arr The array we want to restrict.  The code in its present
#'     incarnation requires an array with a full set of unique
#'     dimension-names, although I suppose it could be possible to
#'     extend to a case where the dimension-names are missing too.
#'
#' @param .restrict A named \code{list} that collects the arguments to
#'     be used in the restriction, i.e. the names should correspond to
#'     names of the dimension-names, and the values should specify a
#'     subset (or a permutation, if you are so inclined) of the
#'     dimension-names.
#'
#' @param .drop A logical argument, default value \code{FALSE}, that
#'     decides whether or not dimensions are allowed to drop.
#'
#' @param .never_drop A character vector to be used if \code{.drop} is
#'     \code{TRUE}, but some dimensions should be protected.  The
#'     default value \code{NULL} does nothing, whereas the argument
#'     will be ignored if \code{.drop} is \code{FALSE}.
#'
#' @param .keep_attributes A logical argument, default value
#'     \code{TRUE}, that decides whether or not attributes should be
#'     kept after the restriction.  Note that relevant \code{class}
#'     attributes are kept by default, and that \code{dim} and
#'     \code{dimnames} must be as specified by the restriction.
#'
#' @param .permute A logical argument, default value \code{FALSE},
#'     that will decide whether or not the dimensions of the final
#'     array should be adjusted to one used by \code{.restrict}.
#' 
#' @return The result will depend on how \code{.sub_arr} and
#' \code{.arr} is related.  If the dimension-names of \code{.sub_arr}
#' is contained in those of \code{.arr}, but its dimension is in fact
#' smaller, then the result will be that \code{.arr} will be filled
#' with copies of \code{.sub_arr} along the unspecified dimensions. If
#' the dimensions are equal, then only those components in \code{.arr}
#' that directly match the dimension-names of \code{.sub_arr} will be
#' affected.
#'
#' @export



restrict_array <- function(.arr,
                           .restrict,
                           .drop = FALSE,
                           .never_drop = NULL,
                           .keep_attributes = TRUE,
                           .permute = FALSE) {
###-------------------------------------------------------------------
#####  TASK: Create sanity-checks to be used here and in
#####  'update_array' (not created yet, will replace 'aa_restrict'
#####  in order to get more elegant and specialised code).
    ##  Sanity checks of '.arr' and '.restrict' with regard to having
    ##  the correct properties.
###-------------------------------------------------------------------
    ##  If '.arr' is a list of arrays, then only work upon those nodes
    ##  that are arrays.  Solution not optimal here
    if (is.list(.arr)) {
        ## ##  NOTE: Should first include a test to see that all the
        ## ##  array-nodes can be restricted, based on 
        ## .lad <- list_array_dims(.arr)
        help_fun <- function(x) {
            ##  Decide if an iterative step should be used.
            if (is.list(x)) 
                x <- lapply(
                    X = x,
                    FUN = help_fun)
            ##  Restric the array at the desired level, when
            ##  present, or else let the exisitng value be there.
            x <- if (is.array(x)) {
                     restrict_array(
                         .arr = x,
                         .restrict = .restrict,
                         .drop = .drop,
                         .never_drop = .never_drop,
                         .keep_attributes = .keep_attributes,
                         .permute = .permute)
                 } else
                     x
        }
        return(help_fun(.arr))
    }
###-------------------------------------------------------------------
    ##  Capture the names of the dimension-names.
    dnn.arr <- names(dimnames(.arr))
#####  TASK: Protest if '.arr' is a vector made to an array by
#####  'as.array', since the dimension only will be one in those
#####  cases?  Protest if no dimension names are given?
###-------------------------------------------------------------------
    ##  Capture the class-information of '.arr', in order to recover
    ##  additional details that the subsetting might destroy.
    class.arr <- class(.arr)
###-------------------------------------------------------------------
    ##  Check that the names of '.restrict' are among the names of the
    ##  dimension-names, do not allow for "unspecified" restrictions.
    if (! prod(names(.restrict) %in% c(dnn.arr, "")))
        error(.argument = c(".arr", ".restrict"),
              "Wrong dimension-names used!")
#####  TASK: Make this somewhat more informative?  List the names that
#####  can be used and point out the names that are missing.
###-------------------------------------------------------------------
    ##  Create a subset-quote to be evaluated later on.  The strategy
    ##  is to shrink it to the correct number of dimensions, and then
    ##  insert the restrictions at suitable places.
    restrict_quote <-
        quote(.arr[,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, drop=FALSE])
###-------------------------------------------------------------------
    ##  Adjust the size to fit the given array. Note that we need to
    ##  keep the two first elements of the quote, i.e. "[" and ".arr",
    ##  and then we need placeholders corresponding to the size of the
    ##  array we are targeting.  We do also need the last element of
    ##  the vector, that specifies the value of ".drop".
    restrict_quote[(2 + length(dim(.arr)) +1):
                       (length(restrict_quote) - 1)] <- NULL
###-------------------------------------------------------------------
    ##  Insert the specified restrictions into our array
    for (name in names(.restrict)) {
        restrict_quote[[2 + match(name, names(dimnames(.arr)))]] <-
            .restrict[[name]]
    }
    kill(name)
###-------------------------------------------------------------------
    ##  Take care of any restrctions, i.e. either update the value of
    ##  '.drop' (when '.never_drop' is 'NULL'), or create a suitably
    ##  adjusted call involving 'abind::adrop' to be used on the array
    ##  'result_array' produced by evaluation of the first quote.
    if (.drop) {
        if (is.null(.never_drop)) {
            ##  Update the value of 'drop', then evaluate.
            restrict_quote[["drop"]] <- .drop
            result_array <- eval(restrict_quote)
        } else {
            ##  Eval the 'restrict_quote', identify if there are some
            ##  dimensions that can be dropped,
            result_array <- eval(restrict_quote)
            .dropable_dims <-
                seq_along(dim(result_array))[dim(result_array) == 1]
            .protected_dims <- 
                seq_along(dim(result_array))[names(dimnames(result_array)) %in% .never_drop]
            .drop_these <- setdiff(x = .dropable_dims,
                                   y = .protected_dims)
            if (length(.drop_these) > 0) {
                ## Use 'abind:adrop',
                result_array <- adrop(x = result_array,
                                      drop = .drop_these)
            }
        }
    } else
        result_array <- eval(restrict_quote)
###-------------------------------------------------------------------
    ##  If '.permute' is 'TRUE' _and_ the order of the dimensions
    ##  needs to be adjusted, then fix it using 't' or 'aperm' --
    ##  remember that dimensions can disappear if '.drop' is 'TRUE' so
    ##  the code must be able to deal with NA-values.
    if (.permute) {
        ##  Find the reordering vector.
        reorder <- match(
            x = names(.restrict),
            table = names(dimnames(result_array)))
        ##  Take into account that we could have '.drop=TRUE'.
        reorder <- reorder[! is.na(reorder)]
        ##  Take into account that '.restrict' could have lower
        ##  dimension than '.arr'.
        reorder <- c(reorder,
                     seq_along(dimnames(result_array))[-reorder])
        ##  No need to do anything if 'reorder' turns out to be an
        ##  increasing sequence of integers, otherwise a permutation
        ##  of 'restrict_array' must be performed.
        if (! identical(reorder, seq_along(reorder))) {
            result_array <- aperm(
                a = result_array,
                perm = reorder)
        }
    }
    kill(.permute, reorder)
###-------------------------------------------------------------------
    ##  Resurect the original class-information insofar it makes
    ##  sense, i.e. check that we do have something with a dimension
    ##  (if not, remove references to "matrix" and/or "array"), and
    ##  then resurect what's left.
    if (is.null(dim(result_array)))
        class.arr <- setdiff(x = class.arr,
                             y = c("matrix", "array"))
    if (length(class.arr) > 0)
        class(result_array) <- class.arr
###-------------------------------------------------------------------
    ##  Add relvant original attributes (when required)
    if (.keep_attributes) {
        .keep_these <- ! names(attributes(.arr)) %in% c("dim", "dimnames", "class")
        if (any(.keep_these))
            attributes(result_array) <- {
                c(attributes(result_array),
                  attributes(.arr)[.keep_these])
            }
    }
    result_array
}
