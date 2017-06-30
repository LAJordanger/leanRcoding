################################################################################
#'
#' Create error-messages.
#'
#' This function wraps around \code{stop} and \code{str_wrap} from the
#' \code{stringr}-package, and is a minor helper with regard to
#' including detailed error-messages in the code without to much fuzz.
#' It's also possible to get a quoted result.
#'
#' @param ... A collection of vectors or lists containing the stuff
#'     that first will be inserted into \code{code_str} and then given
#'     to \code{stop}.  Any lists will (after a sanity-check) be split
#'     into separate vectors; and each vector will be transformed to
#'     its separate paragraph, which will be separated by a blank
#'     line.  Note that any formatting (like special characters for
#'     line breaks or tabulators) will be ignored.
#' 
#'@param .argument The name(s) of the argument(s) that triggered the
#'     error.  The default value for this argument is \code{NULL},
#'     which suppress this part of the message.
#' 
#' @param n A non-negative integer that select which function to
#'     report the error for.  The default value \code{1} selects the
#'     function from which \code{error} was called, whereas higher
#'     values goes further up the call-stack (useful when
#'     sanity-checking of arguments is taken care of by a separate
#'     helper-function).  Use \code{0} to suppress information about
#'     the functions. generations to report.
#' 
#' @param fun_stack Logical value, default \code{TRUE}, that decides
#'     if a presentation of the full call-stack from the top-level to
#'     the function specified by \code{n}.  Note that this argument
#'     will be reset to \code{FALSE} if \code{n} is \code{0}.
#'
#' @param quoted Logical value, default \code{FALSE}. This can be used
#'     to return a quoted version of the error.
#' 
#' @param width The value to be given to the \code{width} argument of
#'     \code{str_wrap}.  Default value \code{80}.
#'
#' @param indent The value to be given to the \code{indent} argument
#'     of \code{str_wrap}.  Default value \code{4}.
#'
#' @param exdent The value to be given to the \code{exdent} argument
#'     of \code{str_wrap}.  Default value \code{4}.
#' 
#' @param call. The value to be given to the \code{call.}-argument of
#'     \code{stop}: logical, indicating if the call should become part
#'     of the error message.  The default value for this function is
#'     \code{FALSE}.
#' 
#' @param domain The value to be given to the \code{domain}-argument
#'     of \code{stop}: see \code{gettext}.  If \code{NA}, messages
#'     will not be translated.  The default value of this argument is
#'     \code{NULL}.
#'
#' @return The effect of this function will be that the information
#'     given to the \code{dotsMethods}-argument will be presented by
#'     means of \code{str_wrap} and \code{stop}, with variations
#'     depending on the values given to the other arguments.  Note
#'     that nothing will be done if no arguments are given to
#'     \code{...}.  If the function is used in the global workspace
#'     (happens under the development phase when writing a new
#'     function), then an adjusted message will be given with regard
#'     to the effect of the arguments \code{fun_stack} and \code{n}.
#'     The argument \code{quoted} can be used to return a quoted
#'     expression of the error, which might be useful if one would
#'     like to figure out if several errors was triggered by faulty
#'     arguments.
#'
#' @export


error <- function(...,
                  .argument = NULL,
                  n = 1,
                  fun_stack = TRUE,
                  quoted = FALSE,
                  width = 80,
                  indent = 4,
                  exdent = 4,
                  call. = FALSE,
                  domain = NULL) {
###-------------------------------------------------------------------
    ##  Capture the content of the dots (as calls).
    the_dots <- dots(...)
    ##  If no 'dots', then return invisible NULL.
    if (length(the_dots) == 0)
        return(invisible(NULL))
    kill(...)
###-------------------------------------------------------------------
    ##  Evaluate the calls in `the_dots` to get to the stored content.
    ##  Reminder: Must be done in the parent-frame to ensure that any
    ##  symbolic references can be found.
    .env <- parent.frame(n=1)
    the_dots <- lapply(
        X = the_dots,
        FUN = eval,
        envir = .env)
    kill(.env)
###-------------------------------------------------------------------
    ##  Reset `fun_stack` if `n` is `0`.
    if (n == 0)
        fun_stack <- FALSE
###-------------------------------------------------------------------
    ##  Create information based on `.argument`.
    .argument_text <- paste(
        "Erroneous argument",
        ifelse(test = length(.argument) > 1,
               yes  = "s ",
               no   = " "),
        paste(sQuote(.argument),
              collapse = ", "),
        sep = "")
    kill(.argument)
###-------------------------------------------------------------------
    ##  Figure out if the function is called from the global
    ##  environment (might happen during testing).
    .testing <- identical(sys.frame(which = -1), .GlobalEnv)
    ##  Create information to be returned based on `.testing`
    if (.testing) {
        ##  Information to return.
        .arg_fun_information <- paste(
            "Testing mode: ",
            .argument_text,
            if (all(fun_stack,
                    n > 0))
                paste(" will be reported in the function ",
                      n,
                      " step",
                      ifelse(test = n > 1,
                             yes  = "s",
                             no   = ""),
                      " up the call-stack.",
                      sep = ""),
            sep = "")
        ##
        .fun_stack_info <-
            paste("Testing mode: The call-stack will ",
                  if (! fun_stack) 
                      "not ",
                  "be reported",
                  sep = "")
    } else {
        ##  Create information to be reported, note that the argument
        ##  `n` must be increased with one before calling
        ##  `this_function` (unless `n`is `0`, in which case no
        ##  additional information will be added.)
        if (n > 0) {
            .fun_info <- this_function(n = n + 1,
                                       details = 2)
#####  TASK: Add code to ignore the occurrence of e.g. `try`, then
#####  test to see if the arguments that has been selected does exist
#####  in the targeted function, so a warning can be returned if
#####  something is amiss.
            .arg_fun_information <- paste(
                .argument_text,
                " in the function ",
                sQuote(.fun_info),
                ".",
                sep  = "")
#####  TASK: Use attributes(.fun_info)$info to give more details about
#####  the function, like package, exported/internal or global/local.
#####  Perhaps add a global option that can govern how much details
#####  that should be returned.
            ##
            .fun_stack_info <-
                if (fun_stack)
                    paste(
                        "The full call-stack is: ",
                        local({
                            .fun_names <- sQuote(names(
                                attributes(.fun_info)$lineage))
                            .indicator <- " > "
                            paste(.fun_names,
                                  collapse = .indicator)
                        }),
                        ".",
                        sep = "")
        } else {
            .arg_fun_information <- paste(
                .argument_text,
                ".",
                sep  = "")
            ##
            .fun_stack_info <- NULL
        }
    }
    kill(.argument_text, .testing, n, fun_stack, .fun_info)
###-------------------------------------------------------------------
    ##  Use `stringr::str_wrap` upon the different components
    if (! is.null(.arg_fun_information))
        .arg_fun_information <- str_wrap(
            string = .arg_fun_information,
            width = width,
            indent = indent,
            exdent = exdent)
    if (! is.null(.fun_stack_info)) 
        .fun_stack_info <- str_wrap(
            string = .fun_stack_info,
            width = width,
            indent = indent,
            exdent = exdent)
###-------------------------------------------------------------------
    ##  Check that 'the_dots' contains vectors or lists of the
    ##  appropriate form.  That is to say, it either has to be all
    ##  vectors or if it is a list it should not contain any sublists.
    .OK <- vapply(
        X = the_dots,
        FUN = is.vector,
        FUN.VALUE = logical(1))
    ##  Update needed in case there's any lists to investigate too.
    .any_lists <- vapply(
        X = the_dots,
        FUN = is.list,
        FUN.VALUE = logical(1))
    for (.i in seq_along(the_dots)[.any_lists]) {
        .OK[.i] <-  all(vapply(
            X = the_dots[[.i]],
            FUN = is_testing,
            FUN.VALUE = logical(1),
            .is = "vector",
            .is_not = "list"))
    }
    ##  Stop the execution if something failed.
    if (! all(.OK)) {
        stop("PROBLEM IN 'error'")
#####  TASK: Use `error` to report the problem...?  Create a more
#####  instructive error-message later on...
    } else {
        ##  Split any lists in `the_dots`, such that the internal
        ##  order of vectors and list-components are preserved.
        if (any(.any_lists)) {
            .index_info <- vapply(
                X = the_dots,
                FUN = function(x) {
                    if (is.list(x)) {
                        length(x)
                    } else
                        1L
                },
                FUN.VALUE = integer(1))
            ##  Temporary storage, grown by catenation since it's
            ##  assumed that the size will be fairly small
            .tmp <- list()
            for (i in seq_along(the_dots))
                .tmp <- c(
                    .tmp,
                    if (.any_lists[i]) {
                        the_dots[[i]]
                    } else 
                        list(the_dots[[i]]))
            ##  Reset the value of `the_dots`
            the_dots <- .tmp
            kill(i, .tmp)
        }
        kill(.OK, .any_lists, .i)
    }
###-------------------------------------------------------------------
    ##  Create calls to `str_wrap` based on the content of 'the_dots',
    ##  do this in a manner that ensures line-breaks between the
    ##  paragraphs.  Use two-step strategy.
    .calls_str_wrap <- as.list(
        rep("\n\n", times = length(the_dots) * 2))
    .calls_str_wrap[2 * (1:length(the_dots)) - 1] <- lapply(
        X = the_dots,
        FUN = function(x) {
            bquote(stringr::str_wrap(
                string = paste(.(x),
                               collapse = " "),
                width = .(width),
                indent = .(indent),
                exdent = .(exdent)))
        })
    kill(the_dots, width, indent, exdent)
    ##  Collect all the pieces needed as arguments of `stop`.
    .args_for_stop <- c(
        "\n",
        if (! is.null(.arg_fun_information))
            c(.arg_fun_information,
              "\n"),
        if (! is.null(.fun_stack_info))
            c(.fun_stack_info,
              "\n"),
        "\n",
        .calls_str_wrap,
        call. = call.,
        domain = domain)
    ##  Return an error, or a quoted expression, based on `quoted`.
    if (quoted) {
        bquote({
            do.call(what = "stop",
                    args =  .(.args_for_stop))
        })
    } else
        do.call(what = "stop",
                args =  .args_for_stop)
}
