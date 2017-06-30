################################################################################
#####  2014-11-11
##  It seems to be the case that I have a much better solution for the
##  'spy' now, which is due to an earlier brainfart making me attempt
##  a rather stupid solution when it came to dealing with stuff
##  captured by the dotsMethods.

################################################################################
#####  2014-08-28
##  I think it might be a good idea to consider having a couple of
##  help-functions tucked away in a separate file.  Partly because it
##  might be that I would like to use them several places, but also
##  because the main-functions can be written in a more condensed form
##  when these functions are kept separate.


#####  2014-09-03
##  It seems now that this file mostly will be dealing with the
##  spy-function, and since I anticipate that even more crap might be
##  needed later on, I have renamed it to `spy.R'.


#' Spy on the arguments used
#'
#' This function is to be used inside of a target function, and it
#' will then record the full set of formals for that function and in
#' addition will it store the corresponding list of all the arguments
#' that were used when the function was called, both defaulted values
#' and user specified values.  The main reason for this function is to
#' ensure that the bootstrap-wrapper simply can replace the original
#' time series with the re-sampled alternatives, and that this should
#' suffice to rerun the analysis with the original specifications of
#' the remaining arguments.
#'
## #' @param target.call, \code{call}, in order to properly capture all
## #' the arguments, it need the result of \code{match.call()} inside of
## #' the target function.  NOTE: This must be done before you call
## #' \code{spy}, i.e. the following will not work:
## #' \code{spy(target.call=match.call())}.
#'
#' @return This function returns a list with two components, a paired
#' list \code{formals} and an environment \code{arg.env}.
#' \code{formals} contains all the arguments needed for a rerun of the
#' targeted function.  Note that \code{formals} will _only_ return the
#' \code{symbol}, i.e. the name, when an argument is given to the
#' function from an externally stored object.  The environment
#' \code{arg.env} contains all the actual values needed in order to
#' rerun this function later on.
#'
#' @export

## #' @return This function returns a list with three components,
## #' \code{call}, \code{envir} and \code{eval_me}.  The \code{call}
## #' gives the call that was used, with full names for all the arguments
## #' and whenever named objects where given as arguments, those are also
## #' given there.  The \code{envir} contains all the actual values of
## #' the arguments, i.e. the values of the named objects has been
## #' inserted in place of the names.  The \code{eval_me} is the call to
## #' actually used if you ever want to rerun the code later on.  
## #'
## #' The stuff we have stored now is sufficient to replicate the
## #' function later, which is part of the motivation for this spy.
## #' Another reason is to investigate if a particular configuration of
## #' arguments might already have been computed (and stored), and thus
## #' avoid that computational resources are hogged by an accidental
## #' restart of a program that already has been computed once.


#####  TASK, 2014-11-07, I wonder if it might be possible to adjust
#####  the capturing of the formals to be those of the calling
#####  function?  That might be feasible to do by using 'eval' in a
#####  probably not to complicated manner.  If that works, the need
#####  for the creation of calls instead of update formals might
#####  disappear, making the code slightly simpler and more in line
#####  with my original preferences.

#####  TASK: Investigate once more the procedure around the arguments
#####  delivered to the dotsMethods, I think a brainfart has made me
#####  use a rather cumbersome solution here...


#####  TASK: A warning message is created when this function is used
#####  inside a function where no arguments are explicitly given, see
#####  'SB_TS_sample.R' for an example.  Probably need a minor
#####  tweak to skip test in those cases.

spy <- function() {
###-------------------------------------------------------------------
    ##  Sanity check, stop if used in the global environment
    if (identical(parent.frame(), globalenv()))
        return(
            cat("\t",
                "'spy' must be used inside of a function.",
                "\n"))
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The function 'match.call()' used inside a function environment
###  gives a nice presentation of the arguments, where all the
###  specified arguments are given by their full names (even if the
###  user has been lazy and only used matching by position or
###  abbreviations for the names).  I guess the 'definition'-argument
###  can be used in some way to capture stuff at other levels too, but
###  only gibberish resulted from my attempts at doing that.  Since I
###  in addition want a list that contains the default arguments (that
###  the user did not bother to type) and the values of named
###  arguments - a more elaborate way of solving this has been used.
#############---------------------------------------------------------
###-------------------------------------------------------------------    
    ##  Start out by capturing the call of the targeted function
    target_call <- sys.call(which = -1)
###-------------------------------------------------------------------
    ##  Record the name of the targeted function.
    target_name <- target_call[[1]]
###-------------------------------------------------------------------
    ##  Use the name to capture the formals of the targeted function,
    target_formals <- formals(as.character(target_name))
    ##  and record the names for use later on.
    names_formals <- names(target_formals)
#####  TASK: This part is not quite satisfying, as it does not
#####  identify correctly the cases where I used 'update_formals' in
#####  the grand-parent.  I don't think it should be to hard to solve
#####  that, but it hardly matters as I worked around it using calls
#####  instead.  But it would be nice to have it done like I imagined.
###-------------------------------------------------------------------
    ##  If the 'dotsMethods' is present in the targeted function,
    ##  perform a sanity check of the arguments given to it, i.e. that
    ##  every object has a name and that those names are unique.  We
    ##  can identify the stuff that belongs to '...', by comparing the
    ##  arguments part of target_call (all except the first component)
    ##  with the formals (except the '...').
#####  TASK: I suppose it might be preferable to create a function
#####  that takes the dots as arguments and does this sanity checking.
#####  It's not an elegant solution to include such a lengthy piece of
#####  code here.  It might actually be nice to have function that for
#####  a given vector will test the validity of names as done here,
#####  e.g. also with regard to using on dimension names and so on...
    if (! any(names_formals == "...")) {
        ##  No dotsMethods
        dots_names <- character(0)
    } else {
        ##  Find the positions of the dots.
        dots_positions <- is.na(
            match(x = tail(names(target_call), -1),
                  table = names_formals))
        ##  Extract the names of the dots.
        dots_names <-
            tail(names(target_call), -1)[dots_positions]
        ##---  KIT
        rm(dots_positions)
#####  TASK: Outsource the code below to a separate function.
###-------------------------------------------------------------------
        ##  When nonempty 'dots_names', investigate if the names are
        ##  reasonable, if not terminate the program.
        if (length(dots_names) != 0) {
            ##  Do we have missing names?
            dots_missing_names <-
                any(dots_names == "")
            ##  Do we have repeated proper names? (ignore missing).
            dots_names_proper <-
                dots_names[dots_names != ""]
            ##---
            dots_repeated_names <-
                length(unique(dots_names_proper)) !=
                    length(dots_names_proper)
###-------------------------------------------------------------------
            ##  If problems detected, inform the user.
            if (dots_missing_names | dots_repeated_names) {
                ##  Create text for missing names.
                missing_names_txt <- {
                    if (dots_missing_names) {
                        nr_missing_names <- sum(dots_names == "")
                        ##---
                        c("\n\t",
                          "Missing name",
                          ifelse(test = nr_missing_names == 1,
                                 yes = " ",
                                 no = "s "),
                          "detected for ",
                          nr_missing_names,
                          " argument",
                          ifelse(test = nr_missing_names == 1,
                                 yes = " ",
                                 no = "s "),
                          "given to '...'")
                    } else 
                        c()
                }
                ##  Create text for lack of unique names.
                repeated_names_txt <- {
                    if (dots_repeated_names) {
                        ##  Tabulate to identify problematic cases.
                        dots_names_table <-
                            table(dots_names_proper)
                        ##  Restrict to the problematic cases.
                        dots_names_table <-
                            dots_names_table[dots_names_table >1]
                        ##---
                        c("\n\t",
                          ifelse(test = length(dots_names_table) == 1,
                                 "An argument is ",
                                 "Some arguments are "),
                          "used several times for '...'",
                          "\n\t",
                          paste(
                              paste("'",
                                    names(dots_names_table),
                                    "'",
                                    "\t\t",
                                    " are used ",
                                    dots_names_table,
                                    " times.",
                                    sep = ""),
                              collapse = "\n\t"))
                    } else 
                        c()
                }
###-------------------------------------------------------------------
                ## Terminate the program and inform the user.
                stop("\t",
                     "Erroneous use of '...' in function '",
                     target_name,
                     "'.",
                     missing_names_txt,
                     repeated_names_txt,
                     call. = FALSE)
            }
###-------------------------------------------------------------------
            ## No problems detected with the dots, clean up.  KIT
            rm(dots_missing_names, dots_repeated_names, dots_names_proper)
        }
    }
####  Reminder: We will return to 'dots_names' later on.
#####  TASK: Include more descriptive text here.
###-------------------------------------------------------------------
    ##  Create an impostor that only contains 'match.call' and
    ##  'sys.frame' in order to capture the environment.
    impostor <- function() {
        list(call = match.call(),
###             envir = sys.frame(sys.nframe()))
             envir = environment())
#####  TASK: There has to be a more compact way to ask for the
#####  environment of the present function...
    }
###-------------------------------------------------------------------
    ##  Make the formals of 'impostor' equal to 'target_formals'
    formals(impostor) <- target_formals
###-------------------------------------------------------------------
    ##  Replace the original function with 'impostor' in 'target_call'
    target_call[[1]] <- as.name("impostor")
#####  TASK: Attempt a strategy where we instead update the call with
#####  any arguments given to the original one instead.  It might be
#####  that would to the trick with regard to the capturing of the
#####  stuff in the dotsMethods too.
###-------------------------------------------------------------------
    ##  Evaluate 'target_call' (with the impostor) to get more details
    ##  about the arguments.
    details <- eval(target_call)
###-------------------------------------------------------------------
    ##  The result should not reveal that an impostor where present.
    details$call[[1]] <- target_name
#####   TASK: This is not the solution I actually would like to use,
#####   since I then just as well could have used a direct solution
#####   where no local environment had been captured in the impostor.
#####   What I would like is a solution that only looks for
#####   replacements when a value _is_ missing, and then preferably it
#####   should attempt to see if it can be located in the way it was
#####   used in the targeted function when it was called....
###-------------------------------------------------------------------
    ##  For targeted functions using 'dotsMethods', i.e. '...', the
    ##  captured environment will always contain the name "..." with
    ##  '...' as value, regardless of whether or not any arguments
    ##  where delivered into '...'.  The presence of '...' in the
    ##  environment makes it impossible to use 'ls.str' to inspect it
    ##  and it is thus preferable to remove it (when present).
    if (any(ls(details$envir, all.names=TRUE) == "..."))
        rm("...", envir = details$envir)
###-------------------------------------------------------------------
    ##  In nested constructs, when arguments are given as symbols, the
    ##  environment captured by 'impostor' might have missing values.
    ##  In order to adjust for this an intermediate step is needed,
    ##  updating the values with those from the parent environment.
    for (.name in ls(details$envir, all.names = TRUE))
        assign(x = .name,
               get(x = .name,
                   envir = parent.frame()),
               envir = details$envir)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The approach above doesn't capture completely the arguments from
###  'dotsMethods', i.e. '...'.  The problem is that the environment
###  doesn't record the values of the dots (at least as far as I can
###  see), but these can be extracted from the captured call.
#############---------------------------------------------------------
###-------------------------------------------------------------------    
    ##  If the target-function used the 'dotsMethods', update envir
    ##  with 'dots_names' from the code above.
    if (any(names_formals == "...")) {
        ##  Add stuff to the recorded environment, use 'eval' to
        ##  replace named objects with their values.
        for (dn in dots_names)
            details$envir[[dn]] <- eval(details$call[[dn]])
###-------------------------------------------------------------------
        ##  Modify 'names_formals' to get rid of '...' and insert new
        ##  arguments instead.
        dots_pos <- which(names_formals == "...")
        L <- length(names_formals)
        names_formals <-
            c(names_formals[0:(dots_pos - 1)],
              sort(dots_names),
              if (dots_pos < L) {
                  names_formals[(dots_pos + 1):L]
              } else {
                  character(0)
              })
        ##---  KIT
        rm(dots_pos, dots_names, L)
    }
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The present construction makes it easy to investigate if a
###  combination of arguments might have been used before (even though
###  named objects with different names has been used to feed the
###  values into the function), but the possible presence of named
###  objects in the present call does not allow us to just use 'eval'
###  on the call and the environment (since the named-objects are not
###  stored in the environment).  We thus need an additional call
###  where that is taken care of, with the syntax 'f(x=x)' where 'x'
###  is names for stuff stored in the environment.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Create a call that can be evaluated together with envir, by
    ##  copying the original call (that gives the details concerning
    ##  which function it is) and then filling it up with the names
    ##  from our (revised) 'names_formals'.  
    details$call_eval <- details$call
    ##  Remember that the first position is the name of the function!
    update_these <- 1 + seq_along(names_formals)
    details$call_eval[update_these] <- 
        lapply(X = names_formals, FUN = as.name)
    names(details$call_eval)[update_these] <- 
        names_formals
###-------------------------------------------------------------------
    ##  Add the name of the function, and when available the name and
    ##  version of the package it belongs to.  For functions defined
    ##  in the workspace (the code "breaks down" since 'package_name'
    ##  in the initial step becomes 'NULL', and 'NA' is thus returned).
    details$fun <- as.character(target_name)
    package_name <- attributes(where(details$fun, env=.GlobalEnv))$name
#####  TASK: The part below made a mess when used in a function in
#####  'under_construction', and it might be tempting to just ignore
#####  this hereafter, however, this indicates that the
#####  'under_construction' function should perform its modifications
#####  in a more advanced manner.
    ## ## ## if (! is.null(package_name)) {
    ## ## ##     details$package <- 
    ## ## ##         gsub(pattern = "package:",
    ## ## ##              replacement = "",
    ## ## ##              x = package_name)
    ## ## ##     details$version <-                  
    ## ## ##         packageDescription(pkg = details$package)$Version
    ## ## ## } else {
    ## ## ##     details$package <- NA
    ## ## ##     details$version <- NA               
    ## ## ## }
###-------------------------------------------------------------------    
    ##  Add attribute, to be used by 'eval.spy' later on.
    attr(details, "class") <- "spy"
###-------------------------------------------------------------------
    ##  Return the result.
    return(details)
}
