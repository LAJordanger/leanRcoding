################################################################################
#'
#' Iterative partial string matching with updates.
#'
#' This function builds on \code{charmatch}, but with the twist that
#' it updates the first vector \code{x} based on the information found
#' in the second vector \code{table}.  A \code{while}-construction is
#' used in the updating procedure, in order to successively reduce the
#' length of the the two vectors by ignoring parts that already was
#' matched.  Note that this function only accept character-vectors as
#' input, and that it will get rid of duplicated entries from the
#' \code{table}-argument.  This function was originally created as a
#' helper for the functions \code{update_call} and
#' \code{update_formals}, but it might be of interest for other too
#' since the result contains some attributes that can be handy to use
#' in tests that compare the content of \code{x} against \code{table}.
#'
#' @param x The character string that we would like to update.
#'
#' @param table The character string containing the information to be
#'     used when updating \code{x}.
#'
#' @return A revised version of \code{x} will be returned, in which
#'     the components might have been attempted updated against the
#'     content of \code{table}.  The result will contain any
#'     attributes \code{x} already had, and it will moreover receive a
#'     new attribute \code{charmatch_loop}, which will be a list
#'     containing the following items:
#' 
#' \describe{
#'
#' \item{x}{The original \code{x}-argument as given to the function.}
#'
#' \item{table}{The original \code{table}-argument as given to the
#'     function.}
#'
#' \item{status}{A vector that encodes the different components of the
#'     result. \code{-1} is used to mark a component of \code{x} that
#'     did not occur in \code{table}; \code{0} is used to mark
#'     ambiguous components where several possible matches still are
#'     present in \code{table}; \code{1} represents components where
#'     unique match were obtained}
#' 
#' \item{problems}{A logical value that is \code{TRUE} if any
#'     ambiguous components where detected}
#'
#' \item{ambiguous}{A list that specifies the whose names are the
#'     ambiguous components from the result, and whose content will be
#'     the components from \code{table} that makes it ambiguous.
#'     Note: This part is only included if ambiguous components are
#'     detected.}
#'
#'
#' \item{ambiguous_info}{The same information as in \code{ambiguous},
#'     but presented as a vector with one character-string for each
#'     ambiguous component.  This simplifies the creation of an
#'     error-message in functions that requires that no ambiguous
#'     components remains, like e.g. \code{update_call} and
#'     \code{update_formals}.  Note: This part is only included if
#'     ambiguous components are detected.}
#' }
#' 
#' @export



## x <- vec1 <- c("m", "med")

## x <- vec1 <- c("m", "med", "mode")


## x <- vec1 <- c("m", "med", "mode", "not_there")
## table <- vec2 <- c("median", "mean", "mode")



charmatch_loop <- function(x, table) {
    ##  Test that the arguments are vectors
    if (! all(is.character(x),
              is.character(table)))
        stop("\t",
             "Erroneous argument(s) in 'charmatch_loop':\n\t",
             "The arguments 'x' and 'table' must be characters.\n",
             call. = FALSE)
    ##  Rename arguments, and ensure unique content in 'vec2'
    vec1 <- x
    vec2 <- unique(table)
    ##  Return 'vec1' if 'vec1' or 'vec2' have length 0.
    if (any(length(vec1) == 0,
            length(vec2) == 0)) {
        attributes(vec1) <-
            c(attributes(vec1),
              list(charmatch_loop =
                   list(x = x,
                        table = table,
                        status = rep(-1, length(x)),
                        problems = FALSE,
                        ambiguous = NULL)))
        return(vec1)
    }
    ##  Initiate logical vectors to keep track of progress.
    .vec1_done <- rep(FALSE, length(vec1))
    .vec2_done <- rep(FALSE, length(vec2))
    ##  Initiate integer vector to keep track of status.
    status <- rep(NA_integer_, length(vec1))
    ##  Use the 'while' + 'break' construction.
    while(TRUE) {
        pos1 <- seq_along(vec1)[! .vec1_done]
        pos2 <- seq_along(vec2)[! .vec2_done]
        V1 <- list(words  = vec1[pos1], pos = pos1)
        V2 <- list(words  = vec2[pos2], pos = pos2)
        .matches12 <- charmatch(x = V1$words,
                                table = V2$words,
                                nomatch = -1)
        ##  Update status (need update after loop too)
        status[V1$pos] <- .matches12
        ##  Update '.vec1_done'.
        .vec1_done[V1$pos[.matches12 != 0]] <- TRUE
        ##  Investigate hits.
        hits_in1 <- .matches12 > 0 
        hits_in2 <- .matches12[hits_in1]
        ##  If no matches break out of the while-loop.
        if (! any(hits_in1) > 0) {
            break
        } else  {
            ##  Update 'vec1' and '.vec2_done'.
            vec1[V1$pos[hits_in1]] <- V2$words[hits_in2]
            ## .vec1_done[V1$pos[hits_in1]] <- TRUE
            .vec2_done[V2$pos[hits_in2]] <- TRUE
        }
    }
    ##  Update status, to ensure that matches are given as 1.
    ##  (Reminder: The solution above inserted position for matches,
    ##  and these makes no sense since the vectors are modified during
    ##  the while-loop.)
    status[status > 0] <- 1
    ##  Inspect the last step from the update, and add attributes to
    ##  'vec1' that can be used to trigger warnings if something
    ##  ambiguous was detected.
    .problems <- ! all(.vec1_done)
    .ambiguous <- if (.problems) {
        ##  Many matches?
        .problem <- .matches12 == 0
        if (any(.problem)) {
            .problem_words <- vec1[V1$pos[.problem]]
            .possible_words <- V2$words
            structure(
                .Data = lapply(X = .problem_words,
                               FUN = function(x) {
                                   .pos <- grep(paste("^",
                                                      x,
                                                      sep = ""),
                                                .possible_words)
                                   .possible_words[.pos]
                               }),
                .Names = .problem_words) 
        }
    }
    ##  Create additional information to be used
    .ambiguous_info <- vapply(
            X = names(.ambiguous),
            FUN = function(x) {
                paste("'",
                      x,
                      "' can be matched with the formals: '",
                      paste(.ambiguous[[x]],
                            collapse = "', '"),
                      "'.\n",
                      sep = "")
            },
            FUN.VALUE = character(1))
    attributes(vec1) <-
        c(attributes(vec1),
          list(charmatch_loop =
                   c(list(x = x,
                          table = table,
                          status = status,
                    problems = .problems),
                    if (.problems)
                        list(ambiguous = .ambiguous,
                             ambiguous_info = .ambiguous_info))))
    ##  Return 'vec1' to the workflow.
    vec1
}
