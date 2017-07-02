################################################################################
#####  2017-01-09

#' Restrict a list to the nodes satisfying a given property.
#'
#' This function is mainly intended to simplify the task of working
#' with list where some of the nodes are arrays, but it could be that
#' it also could be of use in other cases.
#'
#' @param .list The list-object to work upon.
#'
#' @param .property A character, default \code{"is.character"} that
#'     gives the name of the test function that tests the property of
#'     the different nodes.  Note, the function must return a logical
#'     value.
#'
#' @param .restrict Logical value, default \code{TRUE}, that decides
#'     if \code{.list} should be restricted to those nodes satisfying
#'     \code{.property}, or if the restriction instead should be based
#'     on the complementary nodes.
#'
#' @return A new list will be returned, with only those nodes of
#'     \code{.list} that satisfied the requirements given by
#'     \code{.property} and \code{.restrict}.
#' 
#' @export

#####  2017-01-11: TASK: This function does not quite to what it
#####  should, since the sorting step becomes messed up when there are
#####  more than 9 nodes at a level.  It works good enough for the
#####  time being, and I can't waste time to get it up and running in
#####  general now.  

restrict_list <- function(.list,
                          .property = "is.array",
                          .restrict = TRUE) {
###-------------------------------------------------------------------
    ##  Check that a list is present.
    if (! is.list(.list))
        error(.argument = ".list",
              "A list must be provided to this function.")
###-------------------------------------------------------------------
    ##  Initiate to environments to store the node information.  These
    ##  will be updated by the help of the recursive helper function
    ##  defined below.
    .node_env <- new.env()  ##  Bookmarks for all the nodes.
    .drop_env <- new.env()  ##  The nodes to be dropped.
    ##  The helper-function
    .help_fun <- function(.new,
                          .old = c(),
                          ..list = .list,
                          ..property = .property,
                          ..restrict = .restrict) {
        .new_bm <- c(.old, .new)
        .node <- .list[[.new_bm]]
        if (is.list(.node) & length(.node) >0 & ! is.data.frame(.node)) {
            lapply(X = seq_along(.node),
                   FUN = .help_fun,
                   .old = .new_bm,
                   ..list = .list)
        } else {
            .name <- paste(.new_bm, collapse = "_")
            .restrict_or_drop <- local({
                .tmp <- do.call(
                    what = ..property,
                    args = list(.list[[.new_bm]]))
                if (! is.logical(.tmp))
                    error(.argument = ".property",
                          n = 2,
                          c(sQuote(".property"),
                            "must be a function that returns a logical value.",
                            "This did not happen for the node indexed by:",
                            sQuote(deparse(.new_bm))))
                if (..restrict) {
                    ! .tmp 
                } else
                    .tmp
            })
            ##  Update the enviroments
            assign(x = .name,
                   value = .new_bm,
                   envir = .node_env)
            assign(x = .name,
                   value = .restrict_or_drop,
                   envir = .drop_env)
        }
    }
###-------------------------------------------------------------------
    ##  Use '.help_fun' with 'lapply' to get the desired updates of
    ##  the two environments '.node_env' and '.drop_env'.
    lapply(X = seq_along(.list),
           FUN = .help_fun,
           ..list = .list,
           ..property = .property,
           ..restrict = .restrict)
    ##  A 'while'-construction is required to get rid of the
    ##  'list()'-values that can show up on intermediate nodes, and a
    ##  helper for that case is thus also required.
    .empty_list_cleanup <- function(x) {
        is.list(x) & length(x) == 0
    }
###-------------------------------------------------------------------
    ##  The while construction.
    .continue <- TRUE
    while (.continue) {
        ##  Find the "names" of the nodes to remove.
        .death_row <- names(as.list(.node_env))[unlist(as.list(.drop_env))]
        ##  Ensure that the order of execution is correct, with regard to
        ##  the way the indexing of '.list' are modified when the targeted
        ##  nodes are removed from it.
        .death_row <- rev(sort(.death_row))
        ##  Exterminate the nodes specified by '.death_row' and return the
        ##  revised result to the workflow.
        for (.node in .death_row) {
            .bm <- .node_env[[.node]]
            .list[[.bm]] <- NULL
        }
        ##  Investigate if there is a need for a cleaning of
        ##  intermediate nodes, by reseting the storage environments
        ##  and using '.help_fun' with '.empty_list_cleanup'.
        .node_env <- new.env()  ##  Bookmarks for all the nodes.
        .drop_env <- new.env()  ##  The nodes to be dropped.
        ##
        lapply(X = seq_along(.list),
               FUN = .help_fun,
               ..list = .list,
               ..property = ".empty_list_cleanup",
               ..restrict = FALSE) ##  Keep complementary nodes.
        ##  Check if another loop is required.
        .continue <- any(unlist(as.list(.drop_env)))
    }
    ##  Return the result to the work-flow.
    .list
}
