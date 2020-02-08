#' Bind arrays together, based on their dimension names.
#'
#' This function is a wrapper for \code{abind} that binds a bunch of
#' arrays together after an inspection of their dimension-names (it
#' will crash if the the arrays have no named dimensions.  As this is
#' a wrapper for \code{abind}, it will be slightly slower, but the
#' effect is tiny for large arrays and it simplifies the writing of
#' the code in other functions.
#'
#' @param ...  A sequence of disjoint arrays (or \code{NULL}s, that
#'     will be ignored) that we want to put together into a larger
#'     array.  The arrays must have the same number of dimensions with
#'     properly named dimension names.  The dimension names of these
#'     arrays must pairwise have empty intersections.  It is also
#'     possible to give a list of arrays, in which case the other two
#'     arguments are needed too.
#'
#' @param .list Logical value, default \code{FALSE}, that can be used
#'     to feed a list of arrays into the function.  The list does not
#'     need names if the arrays have non-overlapping dimension-names.
#'     It is allowed to have a list where all the dimension-names are
#'     identical, but in this case the list must have unique names
#'     that can be used to adjust the dimension-names of the arrays in
#'     it.
#'
#' @param .list_new.dnn A character string, default \code{"..NEW.."},
#'     that will be used as the name of the new dimension when a list
#'     of arrays must have its dimension-names adjusted,
#'     cf. discussion related to the \code{.list}-argument.
#'
#' @return A larger array containing all of the smaller ones, put
#'     together by the help of \code{abind} along a dimension selected
#'     by the code.
#'
#' @export

my_abind <- function(..., .list = FALSE, .list_new.dnn = "..NEW..") {
    arg_list <- list(...)
###-------------------------------------------------------------------
    ##  Extract the first element when '.list' equals TRUE, and
    ##  something has been added.
    if (all(.list,
            length(arg_list) > 0) )
        arg_list <- eval(expr = arg_list[[1]],
                         envir = sys.parent(1))
###-------------------------------------------------------------------
    ##  Ignore any components that are given as 'NULL'
    arg_list <- arg_list[! vapply(
                               X = arg_list,
                               FUN = is.null,
                               FUN.VALUE = logical(1))]
###-------------------------------------------------------------------
    ##  Sanity-check, that arguments are present.
    if (length(arg_list) == 0)
        error("No arguments detected.")
###-------------------------------------------------------------------
    ##  Sanity-check to see if the stuff given to the function have
    ##  the desired properties of being arrays.
    is_arrays <- vapply(
        X = arg_list,
        FUN = is.array,
        FUN.VALUE = logical(1))
    if (! all(is_arrays))
        error("Only arrays are accepted!")
    ##  KIT
    rm(is_arrays)
###-------------------------------------------------------------------
    ##  Return the argument if only one (non-NULL) is given.
    if (length(arg_list) == 1)
        return(arg_list[[1]])
###-------------------------------------------------------------------
    ##  Find the dimension names of the arrays.
    dimnames_list <- lapply(
        X = arg_list,
        FUN = dimnames)
    ##  If '.list' is 'TRUE', then it is possible that all the arrays
    ##  do have the same dimension-names, and in this case the
    ##  dimension-names should be tweaked with the names of the list
    ##  in order to get things up and running.
    if (.list) {
        ##  Check for equality of all the dimnames.
        .all_equal <- all(vapply(X = seq_along(dimnames_list),
               FUN = function(i) {
                   identical(
                       x = dimnames_list[[1]],
                       y = dimnames_list[[i]])
               },
               FUN.VALUE = logical(1)))
        if (.all_equal) {
            ##  Find the names in the 'arg_list', check that they
            ##  exist and are unique, and then adjust the dimension
            ##  and dimension-names of the arrays.
            .new_names <- names(arg_list)
            if (any(length(.new_names) == 0,
                    length(.new_names) != length(unique(.new_names)))) {
                error("The list of arrays must contain unique names.")
            } else {
                ##  Adjust the dimensions to remove overlapping.
                .dim <- dim(arg_list[[1]])
                .dimnames <- dimnames(arg_list[[1]])
                for (i in seq_along(arg_list)) {
                    .new_dim <- c(.dim, 1)
                    .new_dn <- c(.dimnames,
                                 list(names(arg_list)[i]))
                    names(.new_dn) <- c(names(.dimnames),
                                        .list_new.dnn)
                    dim(arg_list[[i]]) <- .new_dim
                    dimnames(arg_list[[i]]) <- .new_dn
                }
                kill(i, .dim, .dimnames, .new_dim, .new_dn)
            }
            ##  Recompute the dimension names of the arrays.
            dimnames_list <- lapply(
                X = arg_list,
                FUN = dimnames)
        }
    }
    ##  Find dimnames for the result based on 'dimnames_list', this
    ##  also checks that the names of the dimensions match up.
    new_dimnames <- dimnames_union(
        dimnames_list = dimnames_list)
###-------------------------------------------------------------------
    ##  Check that only one dimension contains different stuff, and
    ##  that the other dimensions all contain the same setup (but not
    ##  necessarily) in the same order.  This is done by a comparison
    ##  of the length of dimensions in the original arrays with those
    ##  we expect in the final one (based on the naive function
    ##  'dimnames_union'), i.e. only one of the dimensions is allowed
    ##  to grow.
    help_fun_dim <- function(.dimnames) {
        sort(vapply(
            X = .dimnames,
            FUN = length,
            FUN.VALUE = integer(1),
            USE.NAMES = TRUE))
    }
    ##  Note: We don't care about the position of the internal
    ##  position of the dimensions, only their lengths,
    out_dim <- help_fun_dim(new_dimnames)
    in_dims <- vapply(
        X = dimnames_list,
        FUN = function(x) help_fun_dim(x)[names(out_dim)],
        FUN.VALUE = integer(length(out_dim)))
###-------------------------------------------------------------------
    ##  Inspect the rows in the array 'in_dims', to see if stuff makes
    ##  sense.  That is, all but one of the dimensions should have the
    ##  same length as the one in 'out_dim'.  Reminder: The test below
    ##  recycles the vector 'out_dim'
    .dim_status <- in_dims == out_dim
    ##  Identify the dimension with different values, return an error
    ##  if all are equal or more than one are different.
    .dim_status_compact <- apply(
        X = .dim_status,
        MARGIN = 1,
        FUN = all)
    if (all(.dim_status_compact))
        error(c("All the arrays given to 'my_abind' have the exact same",
                " dimension-names. That's preposterous and not allowed!"))
    if (sum(.dim_status_compact == FALSE) > 1)
        error(c("The arrays given to 'my_abind' are different along",
                "more than one dimension.  Only one dimension can",
                "have different content."))
    ##  Code still running?  Then find the name of the one dimension
    ##  that differs, and check that there's no common component in
    ##  any of the arrays.
    .dim_that_grows <- names(which(.dim_status_compact == FALSE))
    if (sum(in_dims[.dim_that_grows, ]) > out_dim[.dim_that_grows])
        error(c("The arrays only differs along the dimension",
                sQuote(deparse(.dim_that_grows)),
                "but uniqueness fails, i.e. some name are repeated.",
                "That's preposterous and not allowed!"))
    ##  KIT
    rm(out_dim, in_dims, help_fun_dim, .dim_status, .dim_status_compact)
###-------------------------------------------------------------------
    ##  Ensure that all the arrays are structured such that abind can
    ##  work upon them properly, i.e. adjust the dimensional structure
    ##  so they all match with the first one (that is, for the
    ##  dimensions we are not going to bind along).
    .template_dims <- dimnames_list[[1]]
    for (i in 2:length(arg_list)) {
        ##  The setup below will adjust the dimensions when required.
        .template_dims[[.dim_that_grows]] <-
            dimnames_list[[i]][[.dim_that_grows]]
        arg_list[[i]] <- restrict_array(
            .arr = arg_list[[i]],
            .restrict = .template_dims,
            .permute = TRUE)
    }
    ##  KIT
    rm(i, .template_dims)
###-------------------------------------------------------------------
    ##  Identify the dimension we want to bind along, then feed this
    ##  to 'abind' to do the rest of the work.
    .along <- which(names(dimnames_list[[1]]) == .dim_that_grows)

###  Reminder: Does the dotsMethods have an environment related to it?
## .result <- abind(
##     ...,
##     along = .along)
###  It would have been nice to update the '...' instead of having to
###  create copies of the arrays all over the place...  However, I
###  can't find any easily available information about how such an
###  update can be performed.  Better discard that idea, and see if it
###  might be possible to deal with it in another fashion instead.
    
    .names_arg_list <- names(arg_list)
    .dummy_names <- paste("dummy_name__",
                          1:length(arg_list),
                          sep = "")
    ##  Add dummy names where necessary, in order to use 'attach'.
    if (is.null(.names_arg_list)) {
        names(arg_list) <- .dummy_names
    } else {
        .no_names <- .names_arg_list == ""
        if (any(.no_names)) {
            .names_arg_list[.no_names] <- .dummy_names[.no_names]
            names(arg_list) <- .names_arg_list
        }
    }
    ##  KIT
    suppressWarnings(rm(.names_arg_list, .dummy_names, .no_names))
    ##  Create the array-arguments to use in the call.
    .arguments <- setNames(
        object = lapply(
            X = names(arg_list),
            FUN = as.symbol),
        nm = names(arg_list))
###-------------------------------------------------------------------
    ##  Return the result to the work-flow.
    eval(expr = create_call( 
             .cc_fun = abind,
             c(.arguments,
               list(along = .along,
                    use.dnns = TRUE)),
             .cc_list = TRUE),
         envir = arg_list)
}

#####  2017-01-09, TASK: Add code that preserves the class(es) with
#####  the twist that "matrix" must be replaced with "array" since
#####  that otherwise might mess up things when the class is to be set
#####  om matrices stacked together.

## ## ##  Ensure that the class is preserved, but with the twist that
## ## ##  "matrix" must be replaced with "array".
## ## .class_old <- class(orig_arr)
## ## if (any(.class_old %in% "matrix"))
## ##     .class_old[which(.class_old %in% "matrix")] <- "array"
## ## class(new.arr) <- .class_old
