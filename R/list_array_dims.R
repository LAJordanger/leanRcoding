################################################################################
#####  2017-01-09

#' Join similar lists along array-nodes.
#'
#' This function simplifies the task of working with list where some
#' of the nodes are arrays.  
#'
#' @param .list A list (presumably with some nodes containing arrays)
#'     where it is of interest to extract the dimension and dimension
#'     names of these arrays.
#'
#' @return The list given to \code{.list} will be returned with
#'     additional attributes that specifies the specifyeis A single
#'     list where the dimension names of the array-nodes replaces the
#'     arrays, and where all other stuff has been dropped.
#'
#' @export

list_array_dims <- function(.list) {
###-------------------------------------------------------------------
    ##  Check that a list is present.
    if (! is.list(.list))
        error(.argument = ".list",
              "A list must be provided to this function.")
###-------------------------------------------------------------------
    ##  Restric the attention to those nodes that are arrays.
    .list_arr <- restrict_list(.list = .list,
                  .property = "is.array",
                  .restrict = TRUE)
###-------------------------------------------------------------------
    ##  Create a recursive helper that together with 'rapply' can
    ##  return the desired properties in a list shaped as 'list'.
    .recursive_helper <- function(x, type = c("dim", "dimnames")) {
        ##  Restrict type to length one, if unspecified.
        type <- type[1]
        if (is.array(x)) {
            do.call(what = type, args = list(x))
        } else
            if (is.list(x)) {
                .recursive_helper(x)
            } else
                NA
    }
###-------------------------------------------------------------------
    ##  Add the desired attributes to '.list', and return the revised
    ##  version to the workflow.
    attr(x = .list, which = "list_array_dim") <-
        rapply(object = .list_arr,
               f = .recursive_helper,
               how = "replace",
               type = "dim")
    attr(x = .list, which = "list_array_dimnames") <-
        rapply(object = .list_arr,
               f = .recursive_helper,
               how = "replace",
               type = "dimnames")
    ##
    .list
}
