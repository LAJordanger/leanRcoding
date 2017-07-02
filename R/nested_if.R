################################################################################
#####  2014-11-21

#' Simplify the treatment of recursive/nested \code{if}-statements.
#'
#' Recursive/nested \code{if}-statements can pop up when the latter
#' statements need the former statements to be true in order to be
#' well-defined, in which case it might be rather messy to formulate
#' an "all-must-be-TRUE" statement.  Such nested \code{if}-statements
#' can e.g. occur when inspecting objects stored in recursive lists
#' where different branches can have quite different structures, in
#' which case any statement regarding a given \code{level} of a
#' \code{list} must be preceeded by a test of the kind
#' \code{!identical(list$level, NULL)}.
#'
#' @param if_list A list of the statements we want to test, default
#'     value \code{list()}.
#'
#' @param expr_all_TRUE The result to return if the nested structure
#'     turns out to contain well defined stuff that passes the tests.
#'     The default value for this is \code{TRUE}.
#'
#' @param expr_not_all_TRUE The result to return if it turns out that
#'     something goes wrong (not defined, or false statements).  The
#'     default value for this is \code{NULL}.
#'
#' @param env The environment at which the statements should be
#'     evaluated, the default value is \code{parent.frame()}
#'
#' 
#' @return The result will be \code{TRUE} if all the statements given
#'     in \code{if_list} is \code{TRUE}, otherwise the result will be
#'     \code{FALSE}.  The result will be \code{FALSE} if
#'     \code{if_list} has length zero.
#'
#' @export

#####  TASK: This solution doesn't seem to work to good in general, so
#####  the name might be a misnomer.  I suppose it could be possible
#####  to investigate this a bit further, but at the moment I think I
#####  will only adjust to the limitations of this function, i.e. I
#####  might not succeed in using more complicated statements in the
#####  'if_list' for the time being.  UPDATE: It might perhaps not be
#####  that severe after all, the problem I encountered disappeared
#####  when I added parentheses around the expressions.  Investigate
#####  this more in detail later on and inform about it in the
#####  documentation and give an example. The somewhat undesired
#####  effect that I observed when I used a quote might also be an
#####  importeant caveat to point out.




nested_if <- function(if_list = list(),
                      expr_all_TRUE = TRUE,
                      expr_not_all_TRUE = invisible(NULL),
                      env = parent.frame()) {
###-------------------------------------------------------------------
    ##  Sanity check of 'if_list'
    if (! is.list(if_list) | length(if_list) == 0)
        stop("\t",
             "Erroneous argument in 'nested_if'.",
             "\n\t",
             "The argument 'if_list' must be a nonempty list of logic expressions.",
             call. = FALSE)
###-------------------------------------------------------------------
    ##  Use 'bquote' and '.()' to modify 'if_list' into a list of
    ##  simple 'if'-statements.
    for (i in seq_along(if_list))
        if (is.quoted(if_list[[i]])) {
            if_list[[i]] <- bquote(
                if (eval(expr = .(if_list[[i]]), envir = env))
                    expr_all_TRUE
                else
                    expr_not_all_TRUE)
        } else 
            if_list[[i]] <- bquote(
                if (.(if_list[[i]]))
                    expr_all_TRUE
                else
                    expr_not_all_TRUE)
###-------------------------------------------------------------------
    ##  Recursively nest these together.
    for (i in head(sort(x = seq_along(if_list), decreasing = TRUE), -1))
        if_list[[i - 1]][[3]] <- if_list[[i]]
###-------------------------------------------------------------------
    ##  Return the evaluation of the first component.
    return(eval(if_list[[1]]))
}






## if_list <- list(TRUE)
## if_list <- list(TRUE, TRUE, FALSE, TRUE)
## if_list <- list(TRUE, TRUE, TRUE, TRUE)



## nested_if(if_list,
##           expr_all_TRUE = {
##               print("hi")
##               print(3 + 663)},
##           expr_not_all_TRUE = print("Oh no!"))


## if_list <- list(TRUE, TRUE, quote(FALSE), TRUE)
## nested_if(if_list)  ## Behaved as I thought here, but...

################################################################################

#####  2014-11-23
##  Solution that works, but not in general when arguments are quoted.
##  Would like to investigate if a minor modification might give a
##  solution for that defect.

## nested_if <- function(if_list = list(),
##                       expr_all_TRUE = TRUE,
##                       expr_not_all_TRUE = invisible(NULL)) {
## ###-------------------------------------------------------------------
##     ##  Sanity check of 'if_list'
##     if (! is.list(if_list) | length(if_list) == 0)
##         stop("\t",
##              "Erroneous argument in 'nested_if'.",
##              "\n\t",
##              "The argument 'if_list' must be a nonempty list of logic expressions.",
##              call. = FALSE)
## ###-------------------------------------------------------------------
##     ##  Use 'bquote' and '.()' to modify 'if_list' into a list of
##     ##  simple 'if'-statements.
##     for (i in seq_along(if_list))
##         if_list[[i]] <- bquote(
##             if (.(if_list[[i]]))
##                 expr_all_TRUE
##             else
##                 expr_not_all_TRUE)
## ###-------------------------------------------------------------------
##     ##  Recursively nest these together.
##     for (i in head(sort(x = seq_along(if_list), decreasing = TRUE), -1))
##         if_list[[i - 1]][[3]] <- if_list[[i]]
## ###-------------------------------------------------------------------
##     ##  Return the evaluation of the first component.
##     return(eval(if_list[[1]]))
## }



#####  2014-11-21
##  Original version, that only returns 'TRUE' or 'FALSE', but I do
##  now think I should allow stuff to be a bit more general...


## nested_if <- function(if_list = list()) {
## ###-------------------------------------------------------------------
##     ##  Sanity check of 'if_list'
##     if (! is.list(if_list))
##         stop("\t",
##              "Erroneous argument in 'nested_if'.",
##              "\n\t",
##              "The argument 'if_list' must be a list.",
##              call. = FALSE)
## ###-------------------------------------------------------------------
##     ##  Return false when 'if_list' is empty.
##     if (length(if_list) == 0)
##         return(FALSE)
## ###-------------------------------------------------------------------
##     ##  Use 'bquote' and '.()' to modify 'if_list' into a list of
##     ##  simple 'if'-statements.
##     for (i in seq_along(if_list))
##         if_list[[i]] <- bquote(
##             if (.(if_list[[i]]))
##                 TRUE
##             else
##                 FALSE)
## ###-------------------------------------------------------------------
##     ##  Recursively nest these together, from the last and up to the
##     ##  second one,
##     for (i in head(sort(x = seq_along(if_list), decreasing = TRUE), -1))
##         if_list[[i - 1]][[3]] <- if_list[[i]]
## ###-------------------------------------------------------------------
##     ##  Return the evaluation of the first component.
##     return(eval(if_list[[1]]))
## }



## Some tests
## nested_if("a")

## if_list <- list(TRUE)
## if_list <- list(TRUE, TRUE, FALSE, TRUE)
## if_list <- list(TRUE, TRUE, TRUE, TRUE)

## nested_if(if_list)
