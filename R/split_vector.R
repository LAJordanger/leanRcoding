################################################################################
#####  2014-09-25

#' Split a vector into smaller pieces
#'
#' This function chops a vector into a given number of pieces, and it
#' tries to do this by making them as similar in length as possible.
#' This function is used when splitting an array into chunks in order
#' for \code{foreach} to work upon them in parallel, and it is also
#' used in \code{splitting} that aims to avoid out-of-memory errors
#' (due to large intermediate objects) by creating sizeable chunks
#' that stays below a memory-limit. It does also allow additional stuff
#' to be stored in the result, see details below.
#'
#' @param vec The vector we want to split.
#'
#' @param pieces The number of chunks we want in the end.
#'
#' @param allow_extra_pieces Logical argument, default value
#'     \code{FALSE}.  Use \code{TRUE} when this function is used as a
#'     tool for splitting up computations in order to get them below a
#'     critical threshold in relation to memory, in which case it's
#'     safer to allow pieces to take care of the remainder than to
#'     hope that it still should work.  ## TASK: Explain this better
#'     later on.
#'
#' @param compute_name Use this to specify the name to be used in the
#'     compute-part of the resulting list.  When the default
#'     \code{NULL} is used, a name will be created based on
#'     \code{vec}.
#' 
#' @param subset_name Use this to specify the name to be used in the
#'     subset-part of the resulting list.  When the default
#'     \code{NULL} is used, a name will be created based on
#'     \code{vec}.
#' 
#' @param add_to_compute \code{list}, default \code{NULL}.  Some
#'     functions might want more stuff in the compute-list, and this
#'     argument can then be used to add that stuff.
#'
#' @param position An integer, default value 1, that only should be
#'     used when \code{add_to_compute} is used.  Use this argument to
#'     ensure that the splitting of the vector occurs at the desired
#'     \code{position} within the resulting \code{compute}-list, which
#'     might be desirable if e.g. \code{array} should be used to
#'     create a restricted array based on the splitting of a dimension
#'     \code{vec}.
#' 
#' @return This function returns a list with three components:
#'     \code{pieces}, a vector that we want to loop over, and,
#'     \code{subset} and \code{compute} that respectively gives the
#'     arguments needed by the function 'restrict' (that also can do
#'     replacements) and the function that performs the computation of
#'     interest.
#'
#' @export


split_vector <- function(vec,
                         pieces,
                         allow_extra_pieces = FALSE,
                         compute_name = NULL,
                         subset_name = NULL,
                         add_to_compute = NULL,
                         position = 1) {
###-------------------------------------------------------------------
    ##  Extract the length of the vector, and stop if 'pieces' is
    ##  larger than this length.
    L_vec <- length(vec)
    if (pieces > L_vec) {
        message(
            "\n\t",
            "In 'split_vector': Can't divide a vector of length ",
            L_vec,
            " into ",
            pieces,
            " pieces.",
            "\n\t",
            "'pieces' redefined to ",
            L_vec,
            ".")
        pieces <- L_vec
    }
###-------------------------------------------------------------------
    ##  Use 'floor(length(vec)/pieces)' to find the minimum number of
    ##  elements in each chunk (the maximum number if
    ##  'allow_extra_piece=TRUE').
    chunk.size <- floor(L_vec/pieces)
###-------------------------------------------------------------------
    ##  Adjustment when 'allow_extra_pieces'
    if (allow_extra_pieces) {
        ##  When the maximal allowed size is one:
        if (chunk.size == 1) {
            pieces <- L_vec
        } else {
            ##  When there's a  non-zero remainder.
            if ((L_vec %% chunk.size) != 0) {  
                chunk.size <- chunk.size - 1
                pieces <- pieces + 1
#####  TASK: This does not give a result that is as even as possible,
#####  'chunk.size' should be recomputed in order to get a more
#####  equal-sized result.  But I suppose that's of no real importance
#####  at the moment.  Can be fixed later on when I check the details
#####  in the computation a bit more thoroughly.
            }
        }
    }
###-------------------------------------------------------------------
    ##  Create a vector based on 'chunk.size' and 'pieces'.
    length.vec.piece <- rep(chunk.size, pieces)
###-------------------------------------------------------------------
    ##  Distribute the remainder as evenly as possible.
    remainder <- L_vec - chunk.size * pieces
    distributed_remainder <-
        cumsum(rep(1, pieces)) <= remainder
###-------------------------------------------------------------------
    ##  Update 'length.vec.piece' with the 'distributed_remainder'.
    length.vec.piece <-
        length.vec.piece + distributed_remainder
###-------------------------------------------------------------------
    ##  Create storages for 'compute' and 'subset' 
    compute <- vector("list", length = pieces)
    subset <- vector("list", length = pieces)
###-------------------------------------------------------------------
    ##  Use a loop to chop up the vector and place it in the storages.
    end.of.vec <- vec
    for (p in 1:pieces) {
        compute[[p]] <-
            list(name_vec = head(end.of.vec, length.vec.piece[p]))
        subset[[p]] <-
            list(name = as.character(head(end.of.vec, length.vec.piece[p])))
        end.of.vec <- end.of.vec[-c(1:length.vec.piece[p])]
    }
###-------------------------------------------------------------------
    ##  Find the names to be used.
    if (is.null(compute_name))
        compute_name <- deparse(substitute(vec))
    ##---
    if (is.null(subset_name))
        subset_name <- deparse(substitute(vec))
###-------------------------------------------------------------------
    ##  Use the name of the 'vec'-argument in the resulting storages.
    for (p in 1:pieces) {
        names(subset[[p]]) <-
            gsub(pattern = "name",
                 replacement = subset_name,
                 names(subset[[p]]))
        names(compute[[p]]) <-
            gsub(pattern = "name_vec",
                 replacement = compute_name,
                 names(compute[[p]]))
    }
#####  TASK: Could I make a minor check that drops replacement if
#####  'vec' is not given by a symbol?  That would be nice to do in
#####  order to avoid hopeless names like "1:17" and so on...
###-------------------------------------------------------------------
    ##  If 'add_to_compute' has been specified, add the stuff, with
    ##  the original vector at position 'position' in the result.
    if (! is.null(add_to_compute))
        for (p in 1:pieces) 
            compute[[p]] <- c(
                add_to_compute[0:(position -1)],
                compute[[p]], 
                if (position < 1 + length(add_to_compute)) {
                    add_to_compute[position:length(add_to_compute)]
                } else {
                    list()
                })
###-------------------------------------------------------------------
    ##  Return the list with information about the pieces.
    return(
        list(pieces = 1:pieces,
             compute = compute,
             subset = subset))
}
