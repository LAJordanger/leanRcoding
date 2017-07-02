################################################################################
#####  2014-11-11

#' Update an array with a sub-array.
#'
#' This function will update an array with a sub-array, without any
#' need for the user to worry about the dimension-names.
#'
#' @details Arrays are awesome, but it can alas be agonising to keep
#' track of the dimension-names.  This function frees the user from
#' such frustrations.
#'
#' @param .arr The array we want to update.  The code in its present
#' incarnation requires an array with a full set of unique
#' dimension-names, although I suppose it could be possible to extend
#' to a case where the dimension-names are missing too.
#'
#' @param .sub_arr Tue sub-array we want to insert into \code{.arr}.
#' The dimension-names of \code{.sub_arr} must be contained in those
#' of \code{.arr}, but they do not need to be given in the same
#' order.
#' 
#' @param .env An environment in which \code{.arr} will be updated.
#' The default value is the environment of the calling function.
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

update_array <- function(.arr,
                         .sub_arr,
                         .env = parent.frame()) {
###-------------------------------------------------------------------
#####  TASK: Create sanity-checks to be used here and in
#####  'restrict_array' (not created yet, will replace 'aa_restrict'
#####  in order to get more elegant and specialised code).
    ##  Sanity checks of '.arr' and '.sub_arr' with regard to having
    ##  the correct properties, i.e. being arrays, having proper
    ##  dimension-names and moreover having a subset of
    ##  dimension-names that can be used for updating.
###-------------------------------------------------------------------
    ##  Capture the names of the dimension-names
    dnn.arr <- names(dimnames(.arr))
    dnn.sub_arr <- names(dimnames(.sub_arr))
###-------------------------------------------------------------------
    ##  If '.sub_arr' has fewer dimensions than '.arr', append those
    ##  needed in order to stack the data in the correct manner.
    if (length(dnn.sub_arr) < length(dnn.arr)) {
        ##  Find the names of the extra dimensions.
        extra_dim <-
            setdiff(x = dnn.arr,
                    y = dnn.sub_arr)
        ##  Find the positions of the extra dimensions.
        extra_pos <-
            match(x = extra_dim,
                  table = dnn.arr)
###-------------------------------------------------------------------
        ##  Investigate if a reordering of '.sub_arr' is required,
        ##  _before_ new dimensions are appended.
        extra_dnn.sub_arr__in__dn.arr <- 
            match(x = dnn.sub_arr,
                  table = dnn.arr[-extra_pos])
        ##---
        if (! identical(x = extra_dnn.sub_arr__in__dn.arr,
                        y = sort(extra_dnn.sub_arr__in__dn.arr)))
            .sub_arr <- aperm(a = .sub_arr,
                              perm = extra_dnn.sub_arr__in__dn.arr)
###-------------------------------------------------------------------
        ##  Append the extra dimensions to '.sub_arr'
        .sub_arr <- append_dimensions(
            orig_arr = .sub_arr,
            added_dimnames = dimnames(.arr)[extra_pos],
            positions = extra_pos)
        ##  Update the captured dimension-names
        dnn.sub_arr <- names(dimnames(.sub_arr))
        ##---  KIT
        rm(extra_dim, extra_pos, extra_dnn.sub_arr__in__dn.arr)
    }
###-------------------------------------------------------------------
    ##  Investigate if a reordering of '.sub_arr' is required.
    dnn.sub_arr__in__dn.arr <- 
        match(x = dnn.sub_arr,
              table = dnn.arr)
    ##---
    if (! identical(x = dnn.sub_arr__in__dn.arr,
                    y = sort(dnn.sub_arr__in__dn.arr)))
        .sub_arr <- aperm(a = .sub_arr,
                          perm = dnn.sub_arr__in__dn.arr)
    ##---
    rm(dnn.arr, dnn.sub_arr, dnn.sub_arr__in__dn.arr)
###-------------------------------------------------------------------
    ##  Create a quote to be used for the subsetting.
    subset_quote <-
        substitute(.arr[,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,])
###-------------------------------------------------------------------
    ##  Truncate 'subset_quote' to the correct size, i.e. keep the two
    ##  first components and then ensure that the remaining number of
    ##  slots equal the number of dimensions in '.arr'.
    subset_quote <- head(subset_quote, 2 + length(dim(.arr)))
###-------------------------------------------------------------------
    ##  Insert the specified restrictions into our array
    for (nam in names(dimnames(.sub_arr))) {
        subset_quote[[2 + match(nam, names(dimnames(.arr)))]] <-
            dimnames(.sub_arr)[[nam]]
    }
    ##---
    rm(nam)
###-------------------------------------------------------------------
    ##  Use 'bquote' and '.()' to create the replacement quote.
    replace_quote <-
        bquote(.(subset_quote) <- .(.sub_arr))
###-------------------------------------------------------------------
    ##  Evaluate 'replace_quote' in the specified environment.
    eval(expr = replace_quote, envir = .env)
###-------------------------------------------------------------------
    ##  The update of the array has been performed, return an
    ##  invisible NULL to end this function
    return(invisible(NULL))
}

#####  2014-11-27
##  A bug discovered in this function in the case that '.sub_arr' has
##  smaller dimension than '.arr'.

## update_array <- function(.arr,
##                          .sub_arr,
##                          .env = parent.frame()) {
## ###-------------------------------------------------------------------
## #####  TASK: Create sanity-checks to be used here and in
## #####  'restrict_array' (not created yet, will replace 'aa_restrict'
## #####  in order to get more elegant and specialised code).
##     ##  Sanity checks of '.arr' and '.sub_arr' with regard to having
##     ##  the correct properties, i.e. being arrays, having proper
##     ##  dimension-names and moreover having a subset of
##     ##  dimension-names that can be used for updating.
## ###-------------------------------------------------------------------
##     ##  Capture the names of the dimension-names, and investigate if a
##     ##  reordering of '.sub_arr' is required.
##     dnn.arr <- names(dimnames(.arr))
##     dnn.sub_arr <- names(dimnames(.sub_arr))
##     ##---
##     dnn.sub_arr__in__dn.arr <- 
##         match(x = dnn.sub_arr,
##               table = dnn.arr)
##     ##---
##     if (! identical(x = dnn.sub_arr__in__dn.arr,
##                     y = sort(dnn.sub_arr__in__dn.arr)))
##         .sub_arr <- aperm(a = .sub_arr,
##                           perm = dnn.sub_arr__in__dn.arr)
##     ##---
##     rm(dnn.arr, dnn.sub_arr, dnn.sub_arr__in__dn.arr)
## ###-------------------------------------------------------------------
##     ##  Create a quote to be used for the subsetting.
##     subset_quote <-
##         substitute(.arr[,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,])
## ###-------------------------------------------------------------------
##     ##  Truncate 'subset_quote' to the correct size, i.e. keep the two
##     ##  first components and then ensure that the remaining number of
##     ##  slots equal the number of dimensions in '.arr'.
##     subset_quote <- head(subset_quote, 2 + length(dim(.arr)))
## ###-------------------------------------------------------------------
##     ##  Insert the specified restrictions into our array
##     for (nam in names(dimnames(.sub_arr))) {
##         subset_quote[[2 + match(nam, names(dimnames(.arr)))]] <-
##             dimnames(.sub_arr)[[nam]]
##     }
##     ##---
##     rm(nam)
## ###-------------------------------------------------------------------
##     ##  Use 'bquote' and '.()' to create the replacement quote.
##     replace_quote <-
##         bquote(.(subset_quote) <- .(.sub_arr))
## ###-------------------------------------------------------------------
##     ##  Evaluate 'replace_quote' in the specified environment.
##     eval(expr = replace_quote, envir = .env)
## ###-------------------------------------------------------------------
##     ##  The update of the array has been performed, return an
##     ##  invisible NULL to end this function
##     return(invisible(NULL))
## }



#####  Keep the code below as a simple example to be integrated later on.

## .arr <- array(data = 1:(2*3*2),
##               dim = c(2, 3, 2),
##               dimnames = list(
##                   first = letters[1:2],
##                   second = letters[3:5],
##                   third = letters[6:7]))
## .sub_arr <- array(data = 100:(100 + 2*2),
##                   dim = c(2, 2),
##                   dimnames = list(
##                       second = letters[4:5],
##                       first = letters[1:2]))

## update_array(.arr = .arr, .sub_arr = .sub_arr)
