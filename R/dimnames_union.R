################################################################################
#####  2015-01-21

#' Create the union of a list of dimension-names.
#'
#' This function will from a list of disjoint dimension-names (for a
#' set of sub-arrays) create a single dimension-name to be used for an
#' array that can be used to store the stuff from the sub-arrays.
#'
#' @param dimnames_list The list of dimension-names, which must all
#'     contain the same names at the top level.  The "degenerate" case
#'     where the list has length one will be accepted, in order to
#'     avoid specification of exceptional cases when this function is
#'     used for programming purposes.
#'
#' @param .restrict_list A list of restrictions, default value
#'     \code{list()}, that can be used in order to shrink the
#'     resulting union to the specified range for the given
#'     dimensions.  Dimensions that are not included in
#'     \code{.restrict_list} will stay untouched, and dimensions not
#'     present in \code{dimnames_list} will be ignored.
#' 
#' @return A set of dimension-names based on those from
#'     \code{dimnames_list}.  Any dimensions that corresponds to
#'     numerical values will be sorted in numerical order instead of
#'     lexicographical order.
#'
#' @export



dimnames_union <- function(
    dimnames_list,
    .restrict_list = list()) {
###-------------------------------------------------------------------
    ##  Check first for the exceptional case.
    if (length(dimnames_list) == 1)
        return(unlist(dimnames_list, recursive = FALSE))
###-------------------------------------------------------------------
    ##  Sanity-check 'dimnames_list': All components must have the
    ##  same set of names (not necessarily in the same order) and
    ##  there should be no overlap between the different lists.
###-------------------------------------------------------------------
    ##  Record all the dimension-names.
    names_list <- lapply(
        X = dimnames_list,
        FUN = function(x) sort(names(x)))
###-------------------------------------------------------------------
    ##  Check that the dimension-names all are identical, by comparing
    ##  the first with the rest.
    names_test <- vapply(
        X = tail(names_list, -1),
        FUN = function(x) identical(names_list[[1]], x),
        FUN.VALUE = logical(1))
    ##---
    if (! all(names_test))
        stop("\t",
             "Erroneous argument to 'dimnames_union':",
             "\n\t",
             "All the components of 'dimnames_list' must have the same set of names.",
             call. = FALSE)
    ##  KIT
    rm(names_list, names_test)
###-------------------------------------------------------------------
    ##  Check that no dimension-names have any common components, by
    ##  considering all possible pairs of combinations.  It's OK when
    ##  all the intersection has length zero.
    .combinations <- combn(
        x = seq_along(dimnames_list),
        m = 2)
    ##---
    .intersection_vec <- apply(
        X = .combinations,
        MARGIN = 2,
        FUN = { function(vec)
            length(dimnames_intersect(
                dimnames1 = dimnames_list[[vec[1]]],
                dimnames2 = dimnames_list[[vec[2]]]))
            })
    ##---
    if (any(.intersection_vec != 0))
        stop("\t",
             "Erroneous argument to 'dimnames_union':",
             "\n\t",
             "Non-empty intersection between components of 'dimnames_list'.",
             "\n\t",
             "Problematic pairs: ",
             paste(apply(X = .combinations[, .intersection_vec != 0, drop = FALSE],
                         MARGIN = 2,
                         FUN = function(x)
                             paste("c(", x[1], ", ", x[2], ")", sep = "")),
                   collapse = ", "),
             call. = FALSE)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Sanity-checks successfully performed, create the union of the
###  dimension-names from those given in 'dimnames_list'.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Use the first component of the list as a template
    template <- dimnames_list[[1]]
###-------------------------------------------------------------------
    for (.dim in names(template))
        for (i in tail(seq_along(dimnames_list), -1))
            template[[.dim]] <- c(
                template[[.dim]],
                dimnames_list[[i]][[.dim]])
###-------------------------------------------------------------------
    ##  Reduce to unique components.
    for (.dim in seq_along(template))
        template[[.dim]] <- unique(template[[.dim]])
###-------------------------------------------------------------------
    ##  Find those dimensions that correspond to numerical values.
    numerical_dimensions <- vapply(
        X = template,
        FUN = function(x) 
            ! any(is.na(suppressWarnings(as.numeric(x)))),
        FUN.VALUE = logical(1))
###-------------------------------------------------------------------
    ##  Ensure that dimensions corresponding to numbers are sorted in
    ##  numerical order instead of lexicographical order.
    for (.dim in seq_along(template)[numerical_dimensions])
        template[[.dim]] <-
            template[[.dim]][order(as.numeric(template[[.dim]]))]
###-------------------------------------------------------------------
    ##  When required, restrict according to '.restrict_list'.
    for (.dim in intersect(x = names(template),
                           y = names(.restrict_list)))
        template[[.dim]] <- intersect(
            x = template[[.dim]],
            y = .restrict_list[[.dim]])
###-------------------------------------------------------------------
    ##  Return the result to the workflow.
    template
}
