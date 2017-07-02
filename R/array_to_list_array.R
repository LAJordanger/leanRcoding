################################################################################
#####  2017-01-09

#' Convert a large array into a list of arrays.
#'
#' The purpose of this function is to take an array with several
#' dimension and adjust it into a list of arrays instead.  (It will
#' also work when a list of arrays are encountered).  This can e.g. be
#' use full if an interactive investigation requires some particular
#' subsets of the original array to be visualised, since an initial
#' conversion to list of suitable sub-arrays might speed up the
#' extraction of relevant slices later on.
#'
#' @param .arr The array to be converted into a list array.  Note that
#'     this array must have dimension names, and that uniqueness of
#'     the names (at the different levels) must be satisfied.  Note
#'     that \code{.arr} itself can be a list of arrays, in which case
#'     the function will be used on those nodes that are arrays (with
#'     exception of place-holder nodes having a one-dimensional
#'     array), and it will then be assumed that all the array-nodes
#'     satisfies the requirements needed for this function to work
#'     upon them.
#'
#' @param .array_nodes Either \code{NULL} (default value) or a vector
#'     that gives a subset of the names of the dimension-names list.
#'     These names will be used to create a skeleton-list for the
#'     sub-arrays to be stored, and it will be used to define the
#'     argument \code{.sub_dim} when that is given as \code{NULL}.  If
#'     this argument is \code{NULL} and \code{.sub_dim} is different
#'     from \code{NULL}, then a value for \code{.array_nodes} will be
#'     computed based on \code{.sub_dim}.  (At least one of
#'     \code{.array_nodes} or \code{.sub_dim} must be different from
#'     \code{NULL}.)  NB: If the value \code{character(0)} is given
#'     (or computed), then the original array will be returned.
#'
#' @param .sub_dim Either \code{NULL} (default value) or a vector that
#'     gives a subset of the names of the dimension-names list.  These
#'     names will be to specify the dimensions (and the order of them)
#'     to be used for the stored sub-arrays.  It will also be used to
#'     define the argument \code{.array_nodes} when that is given as
#'     \code{NULL}.  If this argument is \code{NULL} and
#'     \code{.array_nodes} is different from \code{NULL}, then a value
#'     for \code{.sub_dim} will be computed based on \code{.sub_dim}.
#'     (At least one of \code{.array_nodes} or \code{.sub_dim} must be
#'     different from \code{NULL}.)
#'
#' @param .add_dims_attribute Logical value, default \code{FALSE},
#'     that can be used to return a list array with attributes that
#'     specifies the dimension and dimension-names found at the
#'     different nodes.
#'
#' @return An object of class \code{list_array}, constructed according
#'     to the specifications in the given arguments.  Note that the
#'     dimension names of the array-nodes all are identical, and to
#'     save memory those dimension-names are not stored for each
#'     individual array but rather stored as an attribute of the main
#'     result.
#'
#' @export

array_to_list_array <- function(.arr,
                                .array_nodes = NULL,
                                .sub_dim = NULL,
                                .add_dims_attribute = FALSE) {
###-------------------------------------------------------------------
    ##  If '.arr' is a list (presumably of arrays), then use a
    ##  recursive solution to work on those nodes that are arrays (and
    ##  have more than one dimension, since that can be used as
    ##  placeholders to indicate a constant value).
    if (is.list(.arr)) {
        ##  NOTE: Should first include a test to verify that all the
        ##  relevant array-nodes can be worked upon.
        help_fun <- function(x) {
            ##  Decide if an iterative step should be used.
            if (is.list(x))  {
                x <- lapply(
                    X = x,
                    FUN = help_fun)
            } else 
                ##  Restrict the array at the desired level (when more
                ##  than one dimension is present) or return the
                ##  original value.
                if (is.array(x))
                    if (length(dim(x)) > 1) {
                        array_to_list_array(
                            .arr = x,
                            .array_nodes = .array_nodes,
                            .sub_dim = .sub_dim,
                            .add_dims_attribute = FALSE)
                    } else
                        x
        }
        ##  Compute the result.
        .result <- help_fun(.arr)
        ##  Return the result to the work-flow, with or without added
        ##  attributes.
        return({if (.add_dims_attribute) {
                    list_array_dims(.result)
                }  else
                    .result})
    }
######################################################################
###-------------------------------------------------------------------
    ##  Sanity check the '.arr'-argument.
    if (! is.array(.arr))
        error(.argument = ".arr",
              "This argument must be an array.")
    ##  Check that the names and dimension-names are valid.
    .dn <- dimnames(.arr)  
    .dnn <- names(.dn)
    .dimnames_OK <- local({
        .unique <- function(vec)
            identical(length(vec), length(unique(vec)))
        .unique_dnn <- .unique(.dnn)
        .unique_dn <- all(vapply(
            X = .dn,
            FUN = .unique,
            FUN.VALUE = logical(1)))
        all(! is.null(.dn),
            ! is.null(.dnn),
            .unique_dnn,
            .unique_dn)
    })
    if (! .dimnames_OK)
        error(.argument = ".arr",
              "The array must have named dimension names with unique labels.")
    kill (.dimnames_OK)
###-------------------------------------------------------------------
    ##  Sanity check the arguments '.array_nodes' and '.sub_dim'.
    if (all(is.null(.array_nodes), is.null(.sub_dim)))
        error(.argument = c(".array_nodes", ".sub_dim"),
              c("Both of the arguments are NULL.",
                "One of them must be a subset of the names of the dimension-names."))
    ##  Check that the arguments makes sense, if both of them are
    ##  given.
    if (all(! is.null(.array_nodes), ! is.null(.sub_dim))) 
        local({
            .empty_intersection <-
                length(intersect(x = .array_nodes,
                                 y = .sub_dim)) == 0
            .all_of_dn <- all(
                union(x = .array_nodes,
                      y = .sub_dim) %in% .dnn)
            if (any(! .empty_intersection,
                    ! .all_of_dn))
                error(.argument = c(".array_nodes", ".sub_dim"),
                      n = 2,
                      c("Both of the arguments are given,",
                        "but they either overlap or their Union is not",
                        "all of the names of the dimension-names."))
        })
    ##  Check that the arguments makes sense when only one of them is
    ##  given, and perform the required update of the other.
    if (is.null(.sub_dim)) {
        if (! all(.array_nodes %in% .dnn))
            error(.argument = ".array_nodes",
                  "This must be a subset of the names of the dimension-names.")
        .sub_dim <- setdiff(x = .dnn,
                            y = .array_nodes)
    }
    if (is.null(.array_nodes)) {
        if (! all(.sub_dim %in% .dnn))
            error(.argument = ".sub_dim",
                  "This must be a subset of the names of the dimension-names.")
        .array_nodes <- setdiff(x = .dnn,
                                y = .sub_dim)
    }
###-------------------------------------------------------------------
    ##  Return the original array if nothing should be done.
    if (length(.array_nodes) == 0)
        return(.arr)
######################################################################
###-------------------------------------------------------------------
    ##  Still running, then create the list-array.  Start by (if
    ##  necessary) a permutation of '.arr', so the subsetting later on
    ##  selects contiguous chunks from the vector that corresponds to
    ##  the array.
    .permutation <- ! identical(x = .dn,
                                y = c(.array_nodes, .sub_dim))
    if (.permutation)
        .arr <- restrict_array(
            .arr = .arr,
            .restrict = .dn[c(.array_nodes, .sub_dim)],
            .keep_attributes = FALSE,
            .permute = TRUE)
    ##  Create the storage for the list-array.
    .result <- skeleton_list(
        names_list = .dn[.array_nodes],
        default_content = NULL,
        add_names_list_as_attribute = FALSE,
        compact = FALSE)
    ##  Create the grid needed for the subsetting.
    .argument_grid <- expand.grid(
        .dn[.array_nodes],
        stringsAsFactors = FALSE)
    ##  Update result with the desired sub-arrays.
    for (i in 1:nrow(.argument_grid)) {
        ##  Ensure that the restrict argument is a named list in the
        ##  corner case when '.array_nodes' has length one.
        .restrict <- .argument_grid[i, ]
        if (! is.list(.restrict)) {
            names(.restrict) <- .array_nodes
            .restrict <- as.list(.restrict)
        }
        .index <- unlist(.argument_grid[i, ])
        .result[[.index]] <- local({
            .tmp <- restrict_array(
                .arr = .arr,
                .restrict = .restrict,
                .drop = TRUE,
                .never_drop = .sub_dim)
            dimnames(.tmp) <- NULL
            .tmp
        })
    }
###-------------------------------------------------------------------
    ##  Add the class 'list_array', and the attributes that is needed
    ##  in order for the print-method to work properly.
    class(.result) <- c("list_array",
                        class(.result))
    attributes(.result)$original_dimnames <- .dn
    attributes(.result)$.array_nodes <- .array_nodes
    attributes(.result)$.sub_dim <- .sub_dim
###-------------------------------------------------------------------
    ##  Return the result to the work-flow, with or without added
    ##  attributes.
    if (.add_dims_attribute) {
        list_array_dims(.result)
    }  else
        .result
}
