################################################################################
#####  2017-01-09

#' Join similar lists along array-nodes.
#'
#' This function simplifies the task of joining list whose nodes are
#' arrays.  This can occur when some computations must be performed in
#' smaller chunks due to large intermediate objects, whereas the
#' resulting lists of arrays themselves are rather small, e.g. when
#' some statistics are to be computed during a bootstrap procedure.
#'
#' @param ...  A collection of similarly shaped lists that all have
#'     one ore more array-nodes that should be joined together.  Any
#'     arguments that are \code{NULL} will be ignored.  Note that the
#'     arrays (at the nodes) must have the same number of dimensions
#'     with properly named dimension names.  The dimension names of
#'     these arrays must pairwise have empty intersections.  In the
#'     cases where the first list have a node that only contains a
#'     single number, it will be assumed that this is a feature that
#'     is common for all the other lists, and the number from the
#'     first list will be used in the result.
#'
#' @param array_nodes A list containing the vector-bookmarks that
#'     specifies the array-nodes to be collected.
#'
#' @param .class A character-string, default \code{"array"} that
#'     specifies the class to be used for the merged arrays.
#'
#' @return A single list where the array-nodes from the lists detected
#'     in \code{...} have been joined together.  The other components
#'     of the lists are assumed to be identical, and the values from
#'     the first list will be used for these.
#'
#' @export

list_array_join <- function(...,
                    array_nodes,
                    .class = "array") {
    ..arg_list <- list(...)
    kill(...)
###-------------------------------------------------------------------
    ##  Ignore any components that are given as 'NULL'
    ..arg_list <- ..arg_list[! vapply(
                               X = ..arg_list,
                               FUN = is.null,
                               FUN.VALUE = logical(1))]
###-------------------------------------------------------------------
    ##  Sanity-check that arguments are present.
    if (length(..arg_list) == 0)
        error(.argument = "...",
              "No arguments detected.")
###-------------------------------------------------------------------
    ##  Identify any 'NULL'-values.
    is_null <- vapply(
        X = ..arg_list,
        FUN = is.null,
        FUN.VALUE = logical(1))
    ##  Return 'NULL' if only 'NULL' values.
    if (all(is_null))
        return(NULL)
    ##  Remove 'NULL'-values from the argument list.
    if (any(is_null)) 
        ..arg_list <- ..arg_list[! is_null]
    kill(is_null)
###-------------------------------------------------------------------
    ##  Sanity-check that the stuff given to the function have the
    ##  desired properties of being lists.
    is_list <- vapply(
        X = ..arg_list,
        FUN = is.list,
        FUN.VALUE = logical(1))
    if (! all(is_list))
        error(.argument = "...",
              "Only lists are accepted as arguments!")
    kill(is_list)
###-------------------------------------------------------------------
    ##  Collect the pieces if more than one list is detected.
    if (length(..arg_list) > 1) {
###-------------------------------------------------------------------
        ##  Additional sanity checks should be added here.  In
        ##  particular, it should be tested that the nodes given as
        ##  array_nodes exists and have the desired properties, and it
        ##  should be verify that the other nodes are identical.
###-------------------------------------------------------------------
        ##  Create quotes with the code that should be evaluated for
        ##  all the nodes given in 'array_nodes'.  The idea is to
        ##  update the nodes of the first component, and then simply
        ##  return that to the workflow.  Nodes in the remaining lists
        ##  will be 'NULL'ed in order to release memory.  In the quote
        ##  below, the '.bm' represents the "bookmarks" from
        ##  'array_nodes'.
        my_abind_quote <- as.call(c(
            quote(my_abind),
            lapply(
                X = seq_along(..arg_list),
                FUN = function(x)
                    bquote(..arg_list[[.(x)]][[.bm]]))
        ))
        NULL_finished_quote <- quote({
            for (i in 2:length(..arg_list))
                ..arg_list[[i]][[.bm]] <- NULL
            kill(i)})
        ##  Execute the quotes for the desired nodes.
        for (.bm in array_nodes) {
            ##  Do nothing if the stuff in the first list does not contain
            ##  an array or if it is a "degenerate" array (dim = 1L).
            if (is.array(..arg_list[[1]][[.bm]])) {
                if (! identical(x = dim(..arg_list[[1]][[.bm]]),
                                y = 1L)) {
                    ..arg_list[[1]][[.bm]] <- eval(my_abind_quote)
                    class(..arg_list[[1]][[.bm]]) <- .class
                }
            }
            eval(NULL_finished_quote)
        }
        kill(.bm, array_nodes, .class, my_abind_quote,
             NULL_finished_quote) }
###-------------------------------------------------------------------
    ##  Return the result with updated attributes for dimensions and
    ##  dimension-names, those attributes will partially be in the
    ##  same shape as '.list', i.e. only the nodes refering to arrays
    ##  will be included in the reslt.
    list_array_dims(.list = ..arg_list[[1]])
}
