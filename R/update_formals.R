################################################################################
#' Update the formals of a function
#'
#' @details The \code{dotsMethods}, i.e.  \code{...}, allows
#'     inheritance of arguments from a parent function to its
#'     children.  But if several children require different arguments
#'     passed to them, some minor problems can occur (unless all of
#'     the children have \code{...} as an argument in order to work as
#'     a sink for superfluous arguments).  This function attempts to
#'     avoid those problems (for functions without \code{dotsMethods}
#'     by comparing the formals of the targeted child-function with
#'     the content captured in the \code{dotsMethods}, whereupon it
#'     updates any common components and ignores the part of
#'     \code{...} that has no relevance.
#' 
#' @param .fun The targeted function whose formals should be updated.
#'     Warning: If the function belongs to the same environment, then
#'     the update will be permanent, whereas a local copy (with
#'     adjusted formals) will be created otherwise.
#'
#' @param ... The arguments to be used when updating the formals of
#'     \code{.fun}.
#'
#' @param .list A logical value, default \code{FALSE}.  This can be
#'     used when the stuff that you want to update with already is
#'     contained in a list or an environment. NOTE: Only the first
#'     argument given to \code{...} will in this case be recorded!
#'
#' @return The result of this function is that the formals of
#'     \code{.fun} is updated in the calling environment.  If the
#'     targeted function is defined outside of this environment, a
#'     local copy will be created.  This might trigger some "bad"
#'     behaviour, and it might be safer to use a construction based on
#'     \code{crate_call} instead.
#'
#' @export

#####  2017-01-09: Problematic case detected when this function was
#####  used inside another package in an attempt at feeding internal
#####  objects of that package into a function from some other
#####  package.  That did not work to good.  It would have been nice
#####  to include some test that detected this phenomenon and properly
#####  adjusted the reference to start with "package:::", but that is
#####  not a task to ponder at the moment.


update_formals <- function(.fun, ..., .list=FALSE) {
###-------------------------------------------------------------------
    ##  Capture the content of '...', 
    .dots <- pryr::dots(...)
###-------------------------------------------------------------------
    ##  Extract the first element when '.list' equals TRUE
    if (.list) 
        .dots <- eval(expr = .dots[[1]],
                      envir = sys.parent(1))
###--------------------------------------------------------------------
    ##  Stop if '.dots' is empty.
    if (length(.dots) == 0) {
        return()
    }
###-------------------------------------------------------------------
    ##  Capture the names of the content
    dot_content <- capture_names(.dots)

    ## if (is.environment(.dots)) {
    ##     dot_content <- ls(.dots, all.names = TRUE)
    ## } else {
    ##     dot_content <- names(.dots)
    ## }

    
###-------------------------------------------------------------------
    ##  We need the name of the function we want to modify
    FUN <- as.name(deparse(substitute(.fun)))
###-------------------------------------------------------------------
    ##  And we need its formals.
    FUN_formals <- formals(.fun)
###-------------------------------------------------------------------
    ##  Update 'dot_content', to allow for partial matching of the
    ##  names used on the arguments.

    ## capture_env()
    
    dot_content <- charmatch_loop(x = dot_content,
                                  table = names(FUN_formals))
    ##  Stop if something is ambiguous.
    if (attributes(dot_content)$charmatch_loop$problems) {
        .ambiguous_info <-
            paste("\t",
                  attributes(dot_content)$charmatch_loop$ambiguous_info,
                  sep = "")
        stop("\t",
             "Ambiguous attempt at updating formals of '",
             FUN,
             "'.\n",
             .ambiguous_info,
             "\n",
             call. = FALSE)
    } else {
        ##  Update the original names when necessary.
#####  TASK?  Create a function 'update_names' to take care of this,
#####  or perhaps use something like 'get_names' and 'set_names'.

        if (is.environment(.dots)) {
            original_names <- ls(.dots, all.names = TRUE)
            for (.i in seq_along(original_names)) {
                if (original_names[.i] != dot_content[.i]) {
                    ##  Reminder: No data copied by this procedure.
                    .dots[[dot_content[.i]]] <-
                        .dots[[original_names[.i]]]
                    eval(bquote(
                        rm(.(original_names[.i]),
                           envir = .dots)))
                }
            }
            ##  KIT
            rm(original_names, .i)
        } else
            names(.dots) <- dot_content
    }
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  When '...' is an argument of 'FUN', every argument should be
###  added to the formals, whereas we otherwise will restrict to only
###  the ordinary arguments.  We thus need to split 'dot_content' into
###  two pieces, 'ordinary_args' and 'dots_args'.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    

    

#####  Problem: This solution didn't to the trick with regard to
#####  partial matching.

    
################################################################################
    ##  Alternative idea.
    
    
################################################################################

    

    ##  Divide 'dot_content' into 'dots_args' and 'ordinary_args'.
    dots_args <-
        setdiff(
            x = dot_content,
            y = names(FUN_formals))
    ordinary_args <-
        setdiff(
            x = intersect(
                x = names(FUN_formals),
                y = dot_content),
            y = "...")
###-------------------------------------------------------------------
    ##  If '...' is an argument of 'FUN', update the formals of 'FUN'.
    ##  by adding a 'dots_list' to it.
    if (any(names(FUN_formals) == "...")) {
        ##  Get the list of new arguments.
        dots_list <- as.list(.dots)[dots_args]
        ##  Record the position of '...' in the formals of 'FUN'.
        dots_pos <- which(names(FUN_formals) == "...")
        ##  Record the length of the formals of 'FUN'.
        L <- length(FUN_formals)
        ##  Add the extra stuff after '...' in the formals of 'FUN'.
        FUN_formals <-
            c(FUN_formals[0:dots_pos],
              dots_list,
              if (dots_pos < L) {
                  FUN_formals[(dots_pos + 1):L]
              } else {
                  list()
              })
    }
###-------------------------------------------------------------------
    ##  Use 'ordinary_args' to update the remaining values.  NOTE: The
    ##  value 'NULL' must be treated in a separate manner, since we
    ##  otherwise might _remove_ some of the arguments of the function
    ##  and thus wreak havoc later on.
    if (length(.dots) != 0)
        for (.name in ordinary_args)
            if (is.null(.dots[[.name]])) {
                FUN_formals[.name] <- list(NULL)
            } else {
                FUN_formals[[.name]] <- .dots[[.name]]
            }
###-------------------------------------------------------------------
    ##  Use 'bquote' and '.()' to create a quote that can evaluate at
    ##  the level of the calling function.
    update_quote <- 
        bquote({
            formals(.(FUN)) <- .(FUN_formals)
        })
###-------------------------------------------------------------------
    ## Evaluate the quote.
    return(
        eval(expr = update_quote, envir = sys.parent(1)))
}


## ###-------------------------------------------------------------------
## ## Playing around, creating a new function 'update_formals'

## test.fun <- function(a, b=2, c=3) {a*b + c}
## update_formals(.fun = test.fun, b=6, c = 5)
## formals(test.fun)

## ##  OK

## ###-------------------------------------------------------------------
## ##  Will this work when  given a list or an environment?

## test.fun <- function(a, b=2, c=3) {a*b + c}
## arg.list <- list(a=2, b= 1)
## update_formals(.fun = test.fun, arg.list, .list=TRUE)
## formals(test.fun)

## ##  OK

## test.fun <- function(a, b=2, c=3) {a*b + c}
## arg.list <- list2env(arg.list)
## ## ls(arg.list)
## update_formals(.fun = test.fun, arg.list, .list=TRUE)
## formals(test.fun)

## ## OK
