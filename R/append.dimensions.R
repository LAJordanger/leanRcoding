################################################################################
#####  2014-09-23

#' Append dimensions and recycle old array
#'
#' Given an array (that must have dimension-names since the code
#' depends on \code{abind::afill}), a named list with new
#' dimension-names, and a vector of positions for these new dimensions
#' in the resulting array - then this function can create an extended
#' version of the original array by stacking the content of it into
#' the new one.
#'
#' This might come in handy e.g. when a for-loop can be avoided in
#' favour of an operation of arrays instead, but where we need to add
#' some extra stuff to our original array in order to ensure the
#' operation is well-defined.
#'
#' If the arrays are very large, it might in addition be preferable to
#' use the recycle-smallest-vector feature of R when we do our
#' operation, since that in many cases will enable us to only have one
#' humongous array instead of two.  However, for this to work it is
#' necessary to control exactly where the extra dimensions are
#' positioned - which can be a rather frustrating task to undertake.
#' This function will in those cases take care of most of the pesky
#' details for us.
#'
#' @param orig_arr The original array that we want to "stack" several
#'     copies of into an enlarged array.
#'
#' @param added_dimnames A list that specifies the dimension-names
#'     that we want to append.  Note that this list must be named in
#'     order for the code to work.
#'
#' @param positions A vector the positions that we want the new
#'     dimensions to occur at.  The default is to append the extra
#'     dimensions after those of \code{orig_arr}.
#' 
#' @return An enlarged array, where the original array is stacked (by
#'     the help of \code{abind::afill<-}).
#'
#' @export

append_dimensions <- function(orig_arr,
                            added_dimnames,
                            positions) {
###-------------------------------------------------------------------
    ##  Create default values of 'position' if necessary.
    if (missing(positions))
        positions <-
            length(dim(orig_arr)) + 1:length(added_dimnames)
###-------------------------------------------------------------------
    ##  Sanity check of 'orig_arr'
    if (! is.array(orig_arr) | is.null(dimnames(orig_arr)))
        error(.argument = "orig_arr",
              "The array must have dimension-names.")
###-------------------------------------------------------------------
    ##  Sanity check of 'added_dimnames'
    if (!is.list(added_dimnames) | is.null(names(added_dimnames)))
        error(.argument = "added_dimnames",
              "This argument must be a named list.")
###-------------------------------------------------------------------
    ##  Introduce a shorthand
    length.new.dim <- length(added_dimnames) + length(dim(orig_arr))
###-------------------------------------------------------------------
    ## Sanity check of position, valid length:
    if (! length(positions) == length(added_dimnames))
        error(.argument = c("added_dimnames", "positions"),
              c("The lengths of the arguments does not match: ",
                length(positions),
                " vs. ",
                length(added_dimnames)))
    ## Sanity check of position, valid content:
    if (! prod(positions %in% 1:length.new.dim) )
        error(.argument = "positions",
              c("This argument must contain ",
                length(added_dimnames),
                " integers from 1 to ",
                length.new.dim))
###-------------------------------------------------------------------
#############---------------------------------------------------------
###------------------------------------------------------------------- 
    ##  Create a template for the new array, i.e. get the dimensions
    ##  and dimension-names correct, but not the content yet.  Start
    ##  out by extracting data from the 'orig_arr' and
    ##  'added_dimnames'.
    added.dim <- unlist(lapply(X = added_dimnames, FUN = length))
    orig.dim <- dim(orig_arr)
    orig.dimnames <- dimnames(orig_arr)
###-------------------------------------------------------------------
    ##  Create a vector of the desired length, and use 'positions' to
    ##  get the correct format.
    new.dim <- vector(mode = "integer",
                      length = length.new.dim) 
    new.dim[-positions] <- orig.dim
    new.dim[positions] <- added.dim
###-------------------------------------------------------------------
    ##  Create the dimension-names in the same manner.
    new.dimnames <- vector(mode = "list",
                           length = length.new.dim)
    new.dimnames[positions] <- added_dimnames
    new.dimnames[-positions] <- orig.dimnames
    ##  Reminder: For some reason, I don't know why, the two lines
    ##  below must be in this order to produce the desired result.
    names(new.dimnames)[positions] <- names(added_dimnames)
    names(new.dimnames)[-positions] <- names(orig.dimnames)
###-------------------------------------------------------------------
    ##  Create array with the desired dimensions and dimension-names.
    new.arr <- array(data = 1,
                     dim = new.dim,
                     dimnames = new.dimnames)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###------------------------------------------------------------------- 
    ##  Use 'abind:afill<-' to stack 'orig_arr' into the new one in a
    ##  manner consistent with 'positions'.  To avoid all the pesky
    ##  details, we will to this by modifying a couple of quotes.
    quote1 <- quote(afill(x = new.arr,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,))
    quote2 <- quote(my.afill.quote <- orig_arr)
    ##  NOTE: The first two components of 'quote1' is respectively
    ##  'afill' and 'new.arr', whereas the rest is stuff that can be
    ##  used for slots for the different dimensions.  We want to
    ##  modify this to fit to the present arguments, i.e. to truncate
    ##  to the correct dimension and to insert 'TRUE' at the slots
    ##  specified by 'positions'.
    quote1 <- head(x = quote1, n = 2 + length.new.dim)
    quote1[2 + positions] <- TRUE
###-------------------------------------------------------------------
    ##  Replace 'my.afill.quote' in 'quote2' (the second component)
    ##  with 'quote1', and evaluate it in order to _update_ 'new.arr'.
    quote2[[2]] <- quote1
    eval(quote2)
    ##  Ensure that the class is preserved, but with the twist that
    ##  "matrix" must be replaced with "array".
    .class_old <- class(orig_arr)
    if (any(.class_old %in% "matrix"))
        .class_old[which(.class_old %in% "matrix")] <- "array"
    class(new.arr) <- .class_old
###-------------------------------------------------------------------
    ##  Return 'new.arr'
    return(new.arr)
}

