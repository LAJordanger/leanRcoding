################################################################################
#'
#' Kill objects from environments.
#'
#' This function has the same effect as the combination
#' \code{suppressWarnings} and \code{rm}, but it's not just a wrapper
#' for these functions.  The problem is that for such a wrapper to
#' work, all the arguments to this functions would have to be given as
#' characters - in conflict with the desire for a solution where we
#' simply can replace \code{rm} with \code{kill}.  This function thus
#' inspects it's call first, and tweaks it a bit in order to figure
#' out what to do in order to perform the extirpation.  Note that this
#' function mainly is intended for the internal cleanup inside of
#' functions - and that the arguments thus have been made less general
#' than those of \code{rm}, i.e. the arguments \code{pos} and
#' \code{inherits} are not present here.
#'
#' @details The "keep it tidy"-rule is useful when writing longer
#'     functions, as it later on might be easier to update the
#'     function at some point if only the objects needed for the
#'     remaining part of the computation is presented - and the
#'     removal of objects as soon as they no longer are needed also
#'     have the beneficial effect of freeing up memory.  \code{rm} is
#'     of course the natural option to use here, but since some
#'     objects only exist under conditional statements (e.g. stuff
#'     defined inside of a \code{if}-\code{else} construction), it
#'     might once in a while happen that warnings about objects not
#'     found will be triggered from \code{rm}.  This can be suppressed
#'     by using \code{suppressWarnings}, but that
#'     "clutters up the code", so that's the reason for the existence
#'     of this function.
#'
#' @param ...  The objects to be removed, as names (unquoted) or
#'     character strings (quoted).  Other stuff can be added, but will
#'     be ignored.
#'
#' @param list A character vector naming objects to be removed, or a
#'     list of symbols.  The default value for this argument is
#'     \code{character()}.  As for the \code{...}-argument, no
#'     warnings will be triggered if the content specified in this
#'     list doesn't exist in the environment under investigation.
#'     However, the list will be sanity-checked to see if it satisfies
#'     the formal requirements.  If those requirements fails, an error
#'     will be returned.  (The rationale for this is that if someone
#'     actually use the \code{list}- argument instead of \code{...},
#'     then they might like to have this feedback with regard to the
#'     structure of that argument.)
#'
#' @param env The environment to use, no default value is given -
#'     but it will select the environment from which \code{kill} was
#'     called if left unspecified.
#'
#' @return This function will remove objects from the specified
#'     environment, without triggering any warnings for those objects
#'     that did not exist in the environment.  Errors will be returned
#'     if \code{envir} fails to be an environment, or if \code{list}
#'     doesn't 
#' 
#' @export

kill <- function(...,
                 list = character(),
                 env = sys.parent()) {
###-------------------------------------------------------------------
    ##  Select default for 'env' if not specified.
    if (missing(env))
        env <- sys.frame(which = -1)
    ##  Stop if not an environment
    if (! is.environment(env))
        error(.argument = "env",
              c(sQuote("env"),
                "is not an environment!"))
###-------------------------------------------------------------------
    ##  Identify the call, and check if any arguments was given at
    ##  all, if not, simply return invisible `NULL`.
    .this_call <- sys.call(which = 0)
    if (length(.this_call)  == 1)
        return(invisible(NULL))
###-------------------------------------------------------------------
    ##  Identify the arguments given to `dotsMethods` by inspecting
    ##  `.this_call`, i.e. find everything that hasn't been given as
    ##  arguments to `list` or `env`.  Do this by inspecting the
    ##  names, compare them against the formals of `kill`, and pick
    ##  out the relevant parts.  Reminder: This method is necessary in
    ##  order to avoid the problems triggered when undefined stuff has
    ##  been added to `...`.  The use of e.g. `list(...)` to inspect
    ##  `...` will trigger an evaluation of the promises in `...` and
    ##  that has to be avoided in the present setup.
    .names_call <- names(.this_call)
###-------------------------------------------------------------------
    ##  Figure out which parts to extract: If `.names_call` is `NULL`,
    ##  then only the `...`-argument has been used; in this case all
    ##  but the first component of the call contains information to
    ##  investigate.  Otherwise the names must be compared against the
    ##  formals of `kill`, and then the relevant part extracted.
    ##  Reminder: The arguments after `...` must match completely, so
    ##  it's thus possible to figure this out without to much fuzz.
    if (! is.null(.names_call)) {
        ##  Identify the parts that doesn't belong to the
        ##  `dotsMethods`, restrict `.this_call`, ignoring the part of
        ##  the call that correspond to the function
        .dots_extract <-
            seq_along(.this_call)[! .names_call %in%
                                     setdiff(
                                         x = names(formals(kill)),
                                         y = "...")][-1]
    } else
        .dots_extract <- seq_along(.this_call)[-1]
    ##  KIT
    rm(.names_call)
###-------------------------------------------------------------------
    ##  Use this to extract the dots when something was found.
    .the_dots <-
        if (length(.dots_extract) > 0) {
            as.list(.this_call[.dots_extract])
        } else
            list()
    ##  Remove parts that doesn't fit the bill of being `characters`
    ##  or `names`.  Due to the present status of not having
    ##  implemented in `is_testing` how to work upon lists, I think
    ##  the solution below will have to do.
    .valid_dots <- vector(mode = "logical",
                          length = length(.the_dots))
    for (i in seq_along(.the_dots))
        .valid_dots[i] <- any(is.symbol(.the_dots[[i]]),
                              is.character(.the_dots[[i]]))
    ##  Restrict to valid arguments.
    .the_dots <- .the_dots[seq_along(.the_dots)[.valid_dots]]
    ##  KIT
    rm(.this_call, .dots_extract, i, .valid_dots)
###-------------------------------------------------------------------
    ##  Check if the `list`-argument has been used to give something
    ##  of positive length.  If so check it satisfies the requirements
    ##  and then join it with `.the_dots`.
    if (length(list) > 0 ) {
        .error_text <-
            c("The argument must either be a `vector` or a `list`",
              "and the content must be characters or names")
        ##  Figure out if an error should be returned, and if not
        ##  adjust the `list` to make a simplified test.
        .return_error <- 
            if (is.vector(list)) {
                if (! is.list(list))
                    list <- as.list(list)
                .valid_ <- vector(mode = "logical",
                                  length = length(list))
                for (i in seq_along(list))


                    
                    .valid_[i] <- eval(bquote(is_testing(
                        obj = .(list[[i]]),
                        .is = c("character", "name"),
                        .length = 1)))

                ## .valid_[i] <- is_testing(
                ##     obj = list[[i]],
                ##     .is = c("character", "name"),
                ##     .length = 1)
                
                
                ## .valid_[i] <- or(e1 = is.symbol(list[[i]]),
                ##  e2 = is.character(list[[i]]))

                is_testing(obj = list,
                           .is = c("character", "name"),
                           .length = 1,
                           .loop = TRUE)

                
                ! all(.valid_)
            } else
                TRUE

        if (.return_error) {
            error(.error_text,
                  .argument = "list")
        } else {
            ##  Collect everything into `.the_dots`.
            .the_dots <- c(.the_dots, list)
            kill(.error_text, .return_error, list, i, .valid_)
            ##  Reminder: No infinite recursion while using `kill`
            ##  here, since it doesn't contain a `list`-argument.
        }
    }
    ##  If `.the_dots` has length zero, then there's nothing to do.
    if (length(.the_dots) == 0)
        return(invisible(NULL))
###-------------------------------------------------------------------
    ##  Still running? Then it's time for some extirpation!  Use the
    ##  following strategy: Convert everything in '.the_dots' to
    ##  character, investigate if there's something in 'env' that
    ##  matches the content, and exterminate it.
    .the_dots <- vapply(
        X = .the_dots,
        FUN = as.character,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE)
    ##  Restrict to those that's actually exists.
    .doomed <- .the_dots[vapply(
        X = .the_dots,
        FUN = exists,
        FUN.VALUE = logical(1),
        where = env)]
    ##  Remove those that was detected.
    suppressWarnings(do.call(
        what = "rm",
        args = list(list = .doomed),
        envir = env))
    ##  Return an invisible `NULL`.}
    return(invisible(NULL))
}
