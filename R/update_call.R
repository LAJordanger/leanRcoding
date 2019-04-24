#' Update a call
#'
#' The function \code{modify_call} in the \code{pryr}-package does not
#' always do as I expect when used inside of a function, so this
#' modified version of \code{update_formals} has been used in order to
#' get stuff to work in a way I better understand.  This approach
#' seems to be a bit saffer in those cases where it's desired that the
#' value \code{NULL} should be used as an argument.
#'
#' @param .call The call we want to update.
#'
#' @param ... \code{dotsMethods}, the arguments to be used for an
#' update of the formals of \code{.fun}.  If the arguments of interest
#' already are stored in a list (or an environment), set the argument
#' \code{.list} to \code{TRUE}. 
#'
#' @param .list Logic value, default \code{FALSE}.  This can be used
#' when the stuff that we want to update with already is contained in
#' a list or an environment. NOTE: Only the first argument given to
#' \code{...} will in this case be recorded!
#'
#' @param .ignore Character vector, default value \code{NULL}.  When
#' the arguments we want to create our call are delivered in a list or
#' an environment, this can be used to perform a partially update,
#' i.e. give the names of the arguments we don't want to update (yet).
#'
#' @return The result of this function is that the formals of
#' \code{.fun} is updated at the calling environment.
#'
#' @export


update_call <- function(
    .call,
    ...,
    .list=FALSE,
    .ignore = NULL){
###-------------------------------------------------------------------
    ##  Capture the content of '...', 
    .dots <- pryr::dots(...)
###-------------------------------------------------------------------
    ##  Extract the first element when '.list' equals TRUE
    if (.list) 
        .dots <- eval(expr = .dots[[1]],
                      envir = sys.parent(1))
###-------------------------------------------------------------------
    ##  If necessary, replace an environment with a list.
    if (is.environment(.dots))
        .dots <- as.list(.dots, all.names = TRUE)
###-------------------------------------------------------------------
    ##  Remove stuff specified in '.ignore', when necessary.
    if (! identical(.ignore, NULL))
        for (ignore in .ignore)
            .dots[[ignore]] <- NULL
    ##  KIT
    suppressWarnings(rm(ignore, .ignore))
###-------------------------------------------------------------------
    ##  Capture the names of the content
    if (is.environment(.dots)) {
        dot_content <- ls(.dots, all.names = TRUE)
    } else {
        dot_content <- names(.dots)
    }
###-------------------------------------------------------------------
    ##  In the case the information is not given as a list or an
    ##  environment, replace symbols with values.
    if (! .list)
        for (.content in dot_content) 
            .dots[[.content]] <- eval(
                expr = .dots[[.content]], 
                envir = sys.parent(1))
###-------------------------------------------------------------------
    ##  Fint the name of the function (whose call we want to modify),
    ##  i.e. the first element of '.call'.
    FUN <- .call[[1]]
###-------------------------------------------------------------------
    ##  If the original call '.call' contains stuff that will not be
    ##  overwritten, then add those to '.dots', take care to make sure
    ##  that any values of 'NULL' is properly copied.
    for (keep_this in setdiff(x = names(.call[-1]),
                              y = dot_content) )
        if (identical(.call[[keep_this]], NULL)) {
            .dots[keep_this] <- list(NULL)
        } else
            .dots[[keep_this]] <- .call[[keep_this]]
###-------------------------------------------------------------------
    ##  Use 'FUN' and '.dots' to create an updated call.
    create_call(
        .cc_fun = FUN,
        .dots,
        .cc_list = TRUE)
}
        

## ##  TEST

## test_fun <- function(a = 3, b = NULL){
##     ## This doesn't do anything at all...
## }

## test_env <- new.env()

## test_env <- list()

## test_env$a <- 245
## test_env$b <- 76352

## test_call <- create_call(.fun = test_fun, test_env, .cc_list = TRUE, .cc_ignore = NULL)


## test_call <- create_call(.fun = test_fun, a = 5, b = 77, .cc_ignore = "b")

## ls.str(captured_result, all.names = TRUE)

## attach(captured_result)

## search()

## detach()

## update_call(.call = test_call, a = 666, b = NULL)


## #####  TASK: In this casem something goes wrong when using
## #####  'under_construction', as the '.call'-argument are captured as
## #####  NULL in this case...  Figure out why.
## under_construction(.fun = update_call,
##                    .call = test_call,
##                    test_fun = test_fun,
##                    a = 666,
##                    clean_workspace = TRUE)

## ls(search()[2], all.names = TRUE)

## ##  Crap, something's wrong here, doesn't properly record the value of
## ##  '.call' when using under construction, whereas it's captured as it
## ##  ought to be when 'capture_env' is used as is.

## ## [1] ".call"       "dot_content" ".dots"       "FUN"         ".list"      
