################################################################################
#####  2016-02-05

#' Create a call for a function
#'
#' Based on arguments delivered by the \code{dotsMethods}, i.e.
#' \code{...}, or by a list or by an environment, this function will
#' create a call for the specified function.
#' 
#' @details A call can simplify code at one point of a function at the
#'     cost of including some "ugly" code somewhere else (in
#'     particular if it is constructed from scratch).  The function
#'     \code{do.call} will add everything from the list given to
#'     argument \code{args} to the function, and the evaluate it.
#'     Unless the targeted function use the \code{dotsMethods},
#'     i.e. \code{...}, this requires that the list of arguments must
#'     be fine-tuned to the function (or else an error pointing out
#'     some unused arguments will pop-up).  An approach using
#'     \code{as.call} might be used, but once again everything will be
#'     added as arguments, which implies that the resulting call must
#'     be adjusted before evaluation.  The present function doesn't
#'     add any superfluous arguments to the targeted function, and
#'     thus simplifies the task of creating and using calls when
#'     programming, which often can make the code much leaner.
#' 
#' @param .cc_fun The targeted function, either given directly or as a
#'     name or character.
#'
#' @param ... \code{dotsMethods}, the arguments to be used in the
#'     creation of our call of \code{.cc_fun}.  If the arguments of
#'     interest already are stored in a list (or an environment), set
#'     the argument \code{.list} to \code{TRUE}.  Note that the
#'     arguments given by this procedure must have names matching
#'     those of the function, in order for them to be properly
#'     included.  It's not required that all the arguments of the
#'     function should be specified.  Any unnamed arguments will be
#'     given a dummy name if \code{.cc_fun} use 'dotsMethods'.
#'
#' @param .cc_list Logic value , default \code{FALSE}.  This can be used
#'     when the arguments already are contained in a list or an
#'     environment. NOTE: Only the first argument given to \code{...}
#'     will in this case be recorded!
#'
#' @param .cc_ignore Character vector, default value \code{NULL}.  When
#'     the arguments we want to create our call are delivered in a
#'     list or an environment, this can be used to perform a partially
#'     update, i.e. give the names of the arguments we don't want to
#'     update (yet).
#'
#' @return The result of this function is a call based on \code{.cc_fun}
#'     and relevant arguments.  Warning: The present incarnation of
#'     the code does not test that all mandatory arguments are
#'     included, so it's no guarantee that a call created by a nitwit
#'     will execute.
#'
#' @export
#' 
################################################################################
#####  Reminder: An example outlining why 'do.call' is a bit bad.

## test_fun <- function(a) {print(a)}
## do.call(what = test_fun, args = list(a = 5, b = 3))
## as.call(x = list(as.symbol("test_fun"), a = 5, b= 3))


create_call <- function(
    .cc_fun,
    ...,
    .cc_list = FALSE,
    .cc_ignore = NULL) {
###-------------------------------------------------------------------
    if (! any(is.function(.cc_fun),
              is.character(.cc_fun),
              is.name(.cc_fun)) )
        error(.argument = ".cc_fun",
              c("The function must either be given directly, or specified",
                " by a name or a character string."))
###-------------------------------------------------------------------
    ##  Identify the content of '...' (can be 'list()').
    .dots <- pryr::dots(...)
###-------------------------------------------------------------------
    ##  Extract the first element when '.cc_list' equals TRUE, and
    ##  something added.
    if (all(.cc_list,
            length(.dots) > 0) )
        .dots <- eval(expr = .dots[[1]],
                      envir = sys.parent(1))
###-------------------------------------------------------------------
    ##  If necessary, replace an environment with a list.
    if (is.environment(.dots))
        .dots <- as.list(.dots, all.names = TRUE)
###-------------------------------------------------------------------
    ##  Remove stuff specified in '.cc_ignore', when necessary.
    if (! identical(.cc_ignore, NULL))
        for (ignore in .cc_ignore)
            .dots[[ignore]] <- NULL
    kill(ignore, .cc_ignore)
###-------------------------------------------------------------------
    ##  Identify unspecified names, (can be 'logical(0)').
    dots_unnamed <- names(.dots) == ""
###-------------------------------------------------------------------
    ##  Add dummy names, when missing.
    if (all(length(.dots) > 0,
            sum(dots_unnamed) > 0)) {
        names(.dots)[dots_unnamed] <- paste(
            "....nameless_arg",
            1:sum(dots_unnamed),
            sep = "_")
    }
    kill(dots_unnamed)
###-------------------------------------------------------------------
    ##  If the function is identified by a name or a character,
    ##  replace it with the corresponding function.
    if (! is.function(.cc_fun)) {
        FUN <- as.name(.cc_fun)
        if (is.name(.cc_fun))
            .cc_fun <- as.character(.cc_fun)
        .cc_fun <- get(x = .cc_fun, envir = parent.frame())
    } else
        ##  Identify the given function.
        FUN <- deparse(substitute(.cc_fun))
###-------------------------------------------------------------------
    ##  Update the formals of the local copy '.cc_fun' (when required)
    if (length(.dots) > 0)
        update_formals(.fun = .cc_fun,
                       .dots,
                       .list = TRUE)
    kill(.dots)
###-------------------------------------------------------------------
    ##  Create a quote based on '.cc_fun' / 'FUN'.
    fun_quote <- quote(.cc_fun())
    fun_quote[[1]] <- as.call(parse(text = FUN))[[1]]
###-------------------------------------------------------------------
    if (length(formals(.cc_fun)) > 0) {
        ##  Add values from the formals of '.cc_fun' to the quote.
        fun_quote[1 + seq_along(formals(.cc_fun))] <-
            formals(.cc_fun)
        ##  Add the names of the formals.
        names(fun_quote)[1 + seq_along(formals(.cc_fun))] <-
            names(formals(.cc_fun))
        ##  Clean the quote to get rid of "...= " (if present).
        fun_quote <- fun_quote[names(fun_quote) != "..."]
    }
###-------------------------------------------------------------------
    ##  Return the quote to the work-flow.
    fun_quote
}
