################################################################################
#'
#' Test an object for several properties.
#'
#' This function can be used if you would like to test if an object
#' satisfies a bunch of criteria.
#'
#' @details WARNING: This function, in it's present incarnation, might
#'     need some of its arguments quoted if it's going to be used
#'     together with e.g. `vapply`.  It might thus be preferable to
#'     adjust the setup so it can investigate vectors and lists
#'     itself.  I think that task should be feasible, but it might
#'     require an additional argument to do this (that is to say, an
#'     argument that can be used when the function calls itself
#'     iteratively).
#####   Reminder: Something along the following lines, code from
#####   `error` indicates how this can be solved.  Add a new argument
#####   ``.loop` to alow iterative calling. 
## for (i in seq_along(.the_dots)) {
##     or(e1 = is.symbol(.the_dots[[i]]),
##        e2 = is.character(.the_dots[[i]]))
## }
#####
#'
#' @param obj The object to be tested
#'
#' @param .is A vector with the specification of what to test.  Note
#'     that this should be the part after the \code{.} in the tests
#'     you would like to try, e.g.\ \code{c("character", "vector"))}
#'     will trigger the tests \code{is.character} and
#'     \code{is.vector}.  The default value \code{NULL} will trigger
#'     that no tests are performed.
#'
#' @param .is_criteria One of the values \code{c("any", "all")} that
#'     will decide whether or not \code{any} or \code{all} will be used
#'     when testing the criteria given in \code{.is}.  The default for
#'     this argument is \code{"any"}.
#'
#' @param .is_not Similar to \code{.is}, but used to specify
#'     properties we don't want \code{obj} to have.  The default value
#'     \code{NULL} will trigger that no such negative tests are
#'     performed.  Note that all the negative tests must be passed in
#'     order for this function to return \code{TRUE}.
#'
#' @param .length An integer that can be used to specify the length
#'     \code{obj} should have.  The default value \code{NULL} will
#'     allow \code{obj} to have any length.  Use \code{"!0"} if it's a
#'     requirement that \code{obj} should have positive length.
#'
#' @param .env The environment in which the testing should be done.
#'     The default value is \code{parent.frame()}, but other arguments
#'     might be needed during the investigation.
#'
#' @param .loop A logical argument, default \code{FALSE} that can be
#'     used if it's of interest to investigate all the elements of a
#'     given vector/list.  Note that it's quite likely that if symbols
#'     are included in a list/vector, then it's hard to use any of the
#'     apply-functions to loop over it.
#'
#' @return The result will be \code{TRUE} or \code{FALSE} depending on
#'     whether or not the specified criteria turned out to be
#'     satisfied or not.  Diagnostic attributes will be added to the
#'     result in order to produce error-messages if that should be of
#'     interest.
#'
#' @export 

is_testing <- function(obj,
                       .is = NULL,
                       .is_criteria = c("any", "all"),
                       .is_not = NULL,
                       .length = NULL,
                       .env = parent.frame(),
                       .loop = FALSE) {
###-------------------------------------------------------------------
    ##  If `loop` is given, perform an interactive investigation, the
    ##  result will be a logical vector with one component for each
    ##  element of `obj`, and then there will be list with all the
    ##  results too, such that all the results from the individual
    ##  attributes can be inspected too.
    ## ## ## ## if (loop) {
    ## ## ## ##     ##  Use an iterative solution to loop over the setup, 
    ## ## ## ##     .iteration <- vapply(
            
    ## ## ## ##     )
    ## ## ## ##     ##  Create a storage matching the lenght of `obj`
    ## ## ## ##     .storage <- vector(mode = "list", length = length(obj))
        
        
    ## ## ## ##     .length_obj <- length(obj)
    ## ## ## ##     ##  
        
    ## ## ## ## }
###-------------------------------------------------------------------
    ##  If the argument is a symbol, update it in order to perform the
    ##  correct tests.
    if (is.symbol(obj))
        obj <- as.name(deparse(substitute(obj)))
###-------------------------------------------------------------------
    ##  Restrict '.is_criteria'.
    .is_criteria <- .is_criteria[1]
    ##  Sanity check '.is_criteria'
    if (! .is_criteria %in% c("any", "all"))
        error(.argument = ".is_criteria",
              c("This argument must either be",
              sQuote("any"),
              "or",
              sQuote("all")))
    ##  Creat copies of the original '.is' and '.is_not'
    .is___original <- .is
    .is_not___original <- .is_not
    ##  Update and sanity test '.is'.
    if (! is.null(.is)) {
        .is <- paste("is.", .is, sep = "")
        .valid_is <- vapply(
            X = .is,
            FUN = exists,
            FUN.VALUE = logical(1))
        if (! all(.valid_is))
            stop("\t",
                 "Erroneous argument in 'is_testing', ",
                 "could not find the test",
                 ifelse(
                     test = sum(.valid_is) > 1,
                     yes  = "s",
                     no   = ""),
                 ":\n\t\t'",
                 paste(.is[! .valid_is],
                       collapse = "'\n\t\t'"),
                 "'",
                 call. = FALSE)
        ##  KIT
        rm(.valid_is)
    }
    ##  Update and sanity test '.is'.
    if (! is.null(.is_not)) {
        .is_not <- paste("is.", .is_not, sep = "")
        .valid_is_not <- vapply(
            X = .is_not,
            FUN = exists,
            FUN.VALUE = logical(1))
        if (! all(.valid_is_not))
            stop("\t",
                 "Erroneous argument to 'is_testing', ",
                 "could not find the test",
                 ifelse(
                     test = sum(.valid_is_not) > 1,
                     yes  = "s",
                     no   = ""),
                 ":\n\t\t'",
                 paste(.is_not[! .valid_is_not],
                       collapse = "'\n\t\t'"),
                 "'",
                 call. = FALSE)
        ##  KIT
        rm(.valid_is_not)
    }
###-------------------------------------------------------------------
    ##  Find the length of 'obj' in the relevant environment.  This
    ##  might be needed in the test later on, and it will also be
    ##  returned as an attribute.
    .obj_length <- eval(
        expr = bquote(length(.(obj))),
        envir = .env)
###-------------------------------------------------------------------
    ##  Test the different criteria to be used, do it in several steps
    ##  such that information is available for diagnostic messages.
    .types <- c("is", "is_not", "length")
    OK_status <- structure(
        .Data = vector(mode = "list",
                       length = length(.types)),
        .Names = .types)
    .intermediate_status <- OK_status
    ##
    OK_status$is <- if (! is.null(.is)) {
        structure(
            .Data = lapply(
                X = .is,
                FUN = function(x) {
                    eval(call(name = x,
                              obj),
                         envir = .env)
                }),
            .Names = .is___original)
    } else
        TRUE
    ##
    OK_status$is_not <- if (! is.null(.is_not)) {
        structure(
            .Data = lapply(X = .is_not,
                           FUN = function(x) {
                               ! eval(call(name = x,
                                           obj),
                                      envir = .env)
                           }),
            .Names = .is_not___original)
    } else
        TRUE
    ##
    OK_status$length <- if (! is.null(.length)) {
        .obj_length == .length
        ## eval(length(obj) == .length,
        ##      envir = .env)
    } else TRUE
###-------------------------------------------------------------------
    ##  Compute the intermediate step.
    .intermediate_status$length <- OK_status$length
    .intermediate_status$is_not <- all(unlist(OK_status$is_not))
    .intermediate_status$is <- do.call(what = .is_criteria,
                                       args = OK_status$is)
###-------------------------------------------------------------------
    ##  Return the result to the workflow, with attributes that can be
    ##  used for diagnostic messages.
    structure(
        .Data = all(unlist(.intermediate_status)),
        args = list(.is = .is___original,
                    .is_criteria = .is_criteria,
                    .is_not = .is_not___original,
                    .length = .length),
        length = .obj_length,
        status = list(
            details = OK_status,
            major = .intermediate_status),
        class = "is_testing")
}


