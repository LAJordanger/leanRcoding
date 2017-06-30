#' Create an empty recursive skeleton list.
#'
#' Sometimes it's convenient to store stuff in lists, i.e. if the
#' stuff of interest contains attributes that might be needed in later
#' functions.  When these lists are recursive, this function can
#' simplify the remaining code by creating a skeleton for the desired
#' structure, based on the names to be used at the different levels.
#'
#' @param names_list A list-object containing vectors with the names
#'     to be used on the different levels (unique content required),
#'     where the rule is that the first component contains the names
#'     to be used at the top-level, and so on.  If \code{names_list}
#'     is a vector, then an ordinary list will be created with names
#'     in accordance with those in the vector.  Note that any
#'     \code{names}-attributes from \code{names_list} will be
#'     discarded in the construction of the new list.  The list given
#'     to \code{names_list} must be a named list if the argument
#'     \code{compact} is \code{TRUE}.
#'
#' @param default_content Use this argument to specify a desired
#'     default for the "nodes" of the recursive list.  The default
#'     selection for this argument is \code{NULL}.
#'
#' @param add_names_list_as_attribute Logic value, default
#'     \code{TRUE}, that will add the attribute
#'     \code{names_list} to the new list and assign
#'     \code{names_list} as its value.
#'
#' @param compact Logical value, default \code{FALSE}.  This argument
#'     can be used to create a version of the result where the names
#'     are suppressed in exchange for an index-function (added as an
#'     attribute) that can find the correct level based on the names
#'     that normally would have been used.  Note that
#'     \code{names_list} must have unique names when \code{compact} is
#'     given as \code{TRUE}.  If \code{names_list} is a vector, then
#'     this argument will be ignored.
#'
#' @return A (recursive) list with names in accordance with those
#'     given in \code{names_list}, or with an index-function-attribute
#'     as described for the \code{compact}-argument.
#'
#' @export


skeleton_list <- function(
    names_list,
    default_content = NULL,
    add_names_list_as_attribute = TRUE,
    compact = FALSE) {
###-------------------------------------------------------------------
    ##  Test the uniqueness of the content of 'names_list'.
    .unique <- function(vec) {
        .not_NULL <- ! is.null(vec)
        ..unique <- identical(length(vec), length(unique(vec)))
        ..proper <- ! "" %in% vec
        all(.not_NULL, ..unique, ..proper)
    }
    .content_OK <- all(vapply(
        X = names_list,
        FUN = .unique,
        FUN.VALUE = logical(1)))
    if (! .content_OK)
        error(.argument = "names_list",
              "Non-unique content detected.")
    kill(.content_OK)
###-------------------------------------------------------------------
    ##  The case when 'names_list' not is a list but only a vector, in
    ##  this case a simple list is to be returned, and the valued of
    ##  the other arguments does not matter.
    if (! is.list(names_list)) {
        new_list <- rep(
            x = list(new_list),
            times = length(names_list))
        names(new_list) <- names_list
        return(new_list)
    }
###-------------------------------------------------------------------
    ##  Check the names of 'names_list' when required.
    if (compact) 
        if (!.unique(names(names_list)))
            error(.argument = c("names_list", "compact"),
                  c("The argument",
                    quote("compact"),
                    "is given as",
                    sQuote("TRUE"),
                    "-- and then ",
                    sQuote("names_list"),
                    "must have unique names."))
    kill(.unique)
###-------------------------------------------------------------------
######################################################################
###-------------------------------------------------------------------
    ##   Initiate the kernel, and construct the list recursively.
    ##   (Reminder: This is not a good solution with regard to memory
    ##   allocation, but not a topic to look into at the moment.)
    new_list <- default_content
    for (i in rev(seq_along(names_list))) {
        new_list <- rep(
            x = list(new_list),
            times = length(names_list[[i]]))
        if (! compact)
            names(new_list) <- names_list[[i]]
    }
    ##  When required, create an additional environment and fill that
    ##  with the function and values needed for the extraction of the
    ##  indices based on a character-bookmark.
    if (compact) {
        attr(x = new_list, which = "env") <- new.env()
        attributes(new_list)$env$names_list <- names_list
        attributes(new_list)$env$array_index <- array_index
        ##  Add the index-function, as to be called.
        attributes(new_list)$index <- function(.bookmark) 
            attributes(new_list)$env$array_index(
                                         .bookmark = .bookmark,
                                         .dimnames = attributes(new_list)$env$names_list,
                                         .check_dimnames = FALSE)
    }
###-------------------------------------------------------------------
    ##  Add names_list as an attribute to 'new_list'.
    if (add_names_list_as_attribute)
        attr(x = new_list, which = "names_list") <-
            names_list
###-------------------------------------------------------------------
    ##  Return 'new_list' to the work-flow.
    new_list
}



## skeleton_list(names_list = c("a", "b", "c"))
## ##---
## skeleton_list(names_list = list(c("a", "b", "c")))
## ##---
## skeleton_list(names_list = list(
##                   first = c("a", "b", "c"),
##                   second = c("A", "B")))
## ##---
## skeleton_list(names_list = list(
##                               c("a", "b", "c"),
##                               c("A", "B")),
##               add_names_list_as_attribute = FALSE)
