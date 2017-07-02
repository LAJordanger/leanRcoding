################################################################################
#####  2014-09-29

#' A revised version of \code{apply}
#'
#' The ordinary version of 'apply' have the undesirable property that
#' it always squashes the result of its function-argument into a
#' vector, which implies that the dimension and dimension-names get
#' lost in translation.  This revised version of \code{apply} does not
#' perform such despicable deeds.  UPDATE: It does however turn out to
#' be exceptionally inefficient when compared with 'apply', so this
#' attempt can't be considered a success...  I guess the better option
#' should be to create a minor tweak that ensures that we reformat the
#' matrix into the desired format instead...  UPDATE, 2015-05-14, this
#' had to be done since 'aaply' did not want to cooperate with one of
#' the cases I wanted to use, check the function 'my_aaply' for
#' details.
#' 
#'
#' I think I see the reason now...  The stuff I made below will over
#' and over again create an array with the size of the result, and
#' that does not work very well since it induces a very high overhead
#' for every single part of the loop..  Could this be avoided? Beats
#' me.  The idea of modifying the output instead now looks like the
#' most promising option.  That might indeed be done very simple I
#' think, since it could boil down to just changing the dimensions to
#' the desired format.  After doing that, I guess I could include the
#' dimension-names and so on.
#'
#' @param X, \code{array}, the array to work upon.  \code{X} can also
#' be some other object in R, in as far as it can be converted to
#' arrays.  Note that a converted vector will be considered to have
#' its first dimension equal to one, so \code{MARGIN} must thus equal
#' two in case you want to use a function on its elements.
#'
#' @param MARGIN, \code{vector}, specifies the margins that we want to
#' work upon.  This can either be a vector of integers specifying the
#' dimensions - or - it can be a vector with a subset of the
#' dimension-names.
#'
#' @param FUN, \code{character}, the name of the function to be used.
#'
#' @param ..., \code{dotsMethods}, to be used when \code{FUN} needs
#' additional arguments.
#'
#' @param .list, \code{logic}, default \code{FALSE}, use this when you
#' want to use \code{...} to feed arguments to the function \code{FUN}
#' - and - those arguments already has been packed into a list or an
#' environment.  Caution: If \code{FUN} actually expects an argument
#' that _is_ a list (or an environment), then the default setting is
#' the correct one to use.
#' 
#' @param .front, \code{logic}, default \code{TRUE}, decides if the
#' new stuff originating from the result of \code{FUN} should be added
#' as new dimensions at the "front" of the array - or - if it should
#' be appended as new dimension at the "end" of the array instead.
#'
#' @param .parallel, \code{logic}, default \code{FALSE}, use this when
#' a parallel backend is available.  The argument will be ignored if
#' no parallel backend exists.
#'
#' @param .cores, \code{integer}, default 1, use this when a specified
#' number of cores are desired for the computation, and a parallel
#' backend with that number of cores will be used to create a local
#' backend for this computation.  Any predefined backends will be
#' restored after the computation has been performed.  Remember that
#' more isn't always better when partitioning a process into parallel
#' chunks, and it might be preferable to not blindly use the maximum
#' number of available cores.
#' 
#' @return This function returns an array with dimensions specified by
#' those given in \code{MARGIN} and those originating from the result
#' of \code{FUN}.  If \code{FUN} returns a single value without any
#' name, e.g. like \code{sum} does, then only the dimensions from
#' \code{MARGIN} are used.  Otherwise new dimensions are added, either
#' in the "front" or the "end" depending on the value of \code{.front}
#' (the default is in the "front").  The resulting array will inherit
#' the names and dimension-names from \code{X} that \code{MARGIN}
#' refers to, and similarly from the result of \code{FUN}.
#' Dimension-names will be added if \code{X} and the result of
#' \code{FUN} does not have them.  The parts from \code{X} and
#' \code{FUN} will then respectively be baptised with "orig_#" and
#' "result_#", where "#" runs over the available dimensions.
#' 
#' @export


my_apply <- function(X, MARGIN, FUN, ...,
                      .list = FALSE, .front = TRUE, .parallel = FALSE,
                      .cores = 1)  {
###-------------------------------------------------------------------
    ##  Record the name of the function, to be used as a part of the
    ##  dimension-names when those are not delivered as part of the
    ##  result of 'FUN'.
    FUN_name <- deparse(substitute(FUN))
###-------------------------------------------------------------------
    ##  Find the function of interest.
    FUN <- match.fun(FUN) #  The actual function
###-------------------------------------------------------------------
    ##  Update the formals of 'FUN' with any arguments from '...'.
    update_formals(.fun = FUN, ..., .list = .list)
###-------------------------------------------------------------------
###-------------------------------------------------------------------
    ##  Ensure that X is an array and does contain a full set of
    ##  dimension-names.
    X <- aa_converter(arr_data = X)
###-------------------------------------------------------------------
###-------------------------------------------------------------------
##     ##  Find the dimension of X, and do a sanity check.
##     dl <- length(dim(X))
##     if (!dl) 
##         stop("dim(X) must have a positive length")
## #####  NOTE: Vectors are not accepted. (But will be after new code has
## #####  been added later on).
###-------------------------------------------------------------------
    ##  Extract information about X.
    d <- dim(X)
    dn <- dimnames(X)
    dnn <- names(dn)
###-------------------------------------------------------------------
    ##  Sanity check of the case when 'MARGIN' is specified by
    ##  characters - with transformation to corresponding indexes.
    if (is.character(MARGIN)) {
        if (is.null(dnn)) 
            error(.argument = "X",
                  "This array must have named dimnames.")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN)) 
            error(.argument = c("X", "MARGIN"),
                  c("Some elements of",
                    sQuote("MARGIN"),
                    "are not names of the dimensions in",
                    sQuote("X")))
    }
###-------------------------------------------------------------------
##     ##  If no dimension-names are given, add them in order to keep
##     ##  track of the different pieces.
##     if (is.null(dn)) {
##         orig.names <- vector(mode = "list", length = dl)
##         for (i in 1:dl)
##             orig.names[[i]] <- paste("or", i, "_", 1:d[i], sep = "")
##         dimnames(X) <- orig.names
##         dn <- orig.names
##         }
## ###-------------------------------------------------------------------
##     ##  If no names for the dimension-names are given, add them in
##     ##  order to make 'aa_restrict' work properly.
##     if (is.null(dnn))
##         names(dn) <- paste("orig", 1:dl, sep="")
## ###-------------------------------------------------------------------
##     ##  Update 'X' with the revised dimnames
##     dimnames(X) <- dn
## ###-------------------------------------------------------------------
    ##  The stuff that survives from 'X' into the resulting array is:
    d.ans <- d[MARGIN]
    dn.ans <- dn[MARGIN]
###-------------------------------------------------------------------
###-------------------------------------------------------------------
    ##  The part concerning the dimensions, dimension-names and
    ##  _names_ of dimension-names of 'X' are now finished.  We now
    ##  need to extract the dimensions and dimension-names of the
    ##  result from 'FUN'.  If these attributes are missing, then
    ##  defaults will be created based on the function-name.
###-------------------------------------------------------------------
###-------------------------------------------------------------------
    ##  We need an example of what 'FUN' returns, and thus need to use
    ##  it on a representative sub-array of 'X'. 'restrict' can find
    ##  this based on a the following list.
    extract_list <- as.list(rep(1, length(MARGIN)))
    names(extract_list) <- dnn[MARGIN]
###-------------------------------------------------------------------
    ##  Compute an example of what 'FUN' returns, when the result is
    ##  forced into the format of an array.
    inspect_result_FUN <- as.array(
        FUN(restrict_array(
            .arr = X,
            .restrict = extract_list),
            ...))
#####  The inclusion of specific arguments must be avoided in order to
#####  get a general function.
    ## inspect_result_FUN <- as.array(
    ##     FUN(aa_restrict(.arr = X,
    ##                     .restrict = extract_list),
    ##         TS = TS,
    ##         bw.points = bw.points,
    ##         marg = marg))
###-------------------------------------------------------------------
###------------------------------------------------------------------- 
    ##  Note: 'as.array' used on a vector does not give a "proper"
    ##   array, since the dimension will be one-dimensional.  Such
    ##   "degenerate" arrays often spell trouble in the code when used
    ##   in computations , but for our present inspection it can be
    ##   exploited to make the code simpler.
###-------------------------------------------------------------------
###-------------------------------------------------------------------
    ##  Investigate what kind of dimension, dimension-names and _names_
    ##  of dimension-names that are present in the result from 'FUN'.
    new_d <- dim(inspect_result_FUN)
    new_dl <- length(new_d)
    new_dn <- dimnames(inspect_result_FUN)
    new_dnn <- names(new_dn)
###-------------------------------------------------------------------
#####  TASK: See if 'aa_converter' might simplify this part of the
#####  code.  Might need to make 'aa_converter' more advanced...
    ##  Add dimension-names when needed.  The code is somewhat messy,
    ##  but it does make the printed result look (slightly) nicer.
    if (is.null(new_dn)) {
        ##  Use 'FUN_name' to create dimension-names, make a
        ##  distinction based on the format of the output.
        if (identical(new_dl, 1L) ) {
            if (identical(new_d, 1L)) {
                new_dn <- list(FUN_name) # The simplest case.
            } else {
                new_dn <-
                    list(paste(FUN_name,
                               1:new_d,
                               sep = "_") ) # Still a simple format.
            }
        } else {
            new_dn <-
                vector(mode = "list",
                       length = new_dl)
            for (i in 1:new_dl)
                new_dn[[i]] <-
                    paste(FUN_name,
                          "_",
                          i,
                          ".",
                          1:new_d[i],
                          sep = "")
        }
    }
###-------------------------------------------------------------------
    ## Add _names_ on the dimension-names, when needed.
    if (is.null(new_dnn)) {
        ##  Use 'FUN_name' to create _names_ on the dimension-names.
        if (identical(new_dl, 1L)) {
            if (! identical(new_d, 1L) | ! is.null(new_dn)) {
                names(new_dn) <- FUN_name # No "_1" for the name in this case.
            } # else let it be as it has been defined.
        } else {
            names(new_dn) <-
                paste(FUN_name,
                      1:new_dl,
                      sep="_")
        }
    }
###-------------------------------------------------------------------
###-------------------------------------------------------------------
#####  TASK: What if only a few of these dimension-names are missing?
#####  I think this might need a simple help-function that does a more
#####  thorough analysis of the situation.  The setup here should
#####  anyway be cleaned up, since it is awful as it is now.  I do
#####  think an internal help-function that takes care of all that
#####  crap might be the better solution to go for. The readability of
#####  this code would at least increase drastically.
###-------------------------------------------------------------------
###-------------------------------------------------------------------
    ##  We can now create the desired information for the result.
    new_dim <- c(new_d, d.ans)
    new_dimnames <- c(new_dn, dn.ans)
###-------------------------------------------------------------------
###-------------------------------------------------------------------
    ##  Call 'apply' (to compute the result in an efficient way), and
    ##  adjust the dimension and dimension-names.  (Any arguments to
    ##  'FUN' given by the dotsMethods, i.e. '...' has already been
    ##  taken into account at the beginning of this function.)  Take
    ##  into account the value of '.parallel', i.e. whether we want to
    ##  perform the computation in parallel or not.  Note that
    ##  '.parallel' equal TRUE will be ignored if no parallel backend
    ##  is defined.
###-------------------------------------------------------------------
###-------------------------------------------------------------------
#####  TASK: Include stuff related to the .cores argument, i.e. see if
#####  the present backend got the desired number of cores, record the
#####  setting if needed, and specify that it should be restored on
#####  exit, inform the user if the number of cores turns out to be
#####  higher than the available number - and use that in those cases.
###-------------------------------------------------------------------
###-------------------------------------------------------------------
#####  TASK: It might be prudent to investigate if the behaviour in
#####  the degenerate cases must be considered with other options than
#####  'abind'. 
    if (.parallel & getDoParWorkers() != 1) {
        ## Get the number of available cores, that the process can be
        ## split upon.
#####  TASK: I suppose it would be preferable to have some defaults to
#####  compare against, in order to prevent the use of to many cores,
#####  since the overhead might be large in those cases. I anticipate
#####  that one solution could be to run a separate program based on
#####  'parallel::detectCores()' and test the performance for
#####  different numbers of cores, and to reset the decision about
#####  doing stuff in parallel based on that.  I think this might
#####  perhaps be nice to do once at the beginning of a bootstrap
#####  sequence?
        cores <- getDoParWorkers()
        ##  Identify (one of) the longest available margins.
        long_margin_name <-
            names(sort(x = unlist(lapply(X = dn.ans, FUN = length)),
                       decreasing = TRUE)[1])
        ##  Use 'split_vector' on the specified dimension, to enable
        ##  'aa_restrict' to split 'X' into smaller chunks.
        split <-
            split_vector(vec = dn.ans[[long_margin_name]],
                         pieces = cores,
                         compute_name = long_margin_name,
                         subset_name = long_margin_name)
#####  TASK: Check what's needed here.
###-------------------------------------------------------------------
        ## We will use 'abind' to patch the pieces together again, and
        ##  need to find the 'along' argument in order to specify the
        ##  correct way of doing that.  Note that we need to take into
        ##  account what kind of result 'FUN' gives, i.e. will there
        ##  be added an extra dimension at the beginning or not.
        along <-
            match(long_margin_name, names(dn.ans)) +
                ifelse(test = (new_dl != 1),
                       yes = 1,
                       no = 0)
###-------------------------------------------------------------------
        ##  Create a local copy of abind to use with 'foreach', which
        ##  binds along the correct dimension and make sure that we
        ##  get the correct dimension-names inherited.
        local_abind <- function(...) 
            abind(..., along = along, hier.names = TRUE)
###-------------------------------------------------------------------
        ##  Use 'foreach' to do the computation in parallel.
        result <-  foreach(
            piece = split$pieces,
            .combine = local_abind) %dopar% {
                apply(
                    X = restrict_array(.arr = X,
                        .restrict = split$subset[[piece]]),
                    MARGIN = MARGIN,
                    FUN = FUN,
                    ...)
            }
    } else {  
        result <- apply(X = X, MARGIN = MARGIN, FUN = FUN, ...)
    }
###-------------------------------------------------------------------
    ##  Create an array from result, with the desired properties
    if (is.array(result)) {
        dim(result) <- new_dim
        dimnames(result) <- new_dimnames
    } else {
        result <- array(data = result,
                        dim = new_dim,
                        dimnames = new_dimnames)
    }
##### TASK: Do a test to see if this way of doing the modification
##### actually does reduce the overhead in those cases where 'result'
##### already is an array.
###-------------------------------------------------------------------
    ##  If '.front' equals FALSE, do a permutation such that the new
    ##  dimensions occur at the "back" of the array.
    if (! .front)
        result <- aperm(a = result,
                        perm = names(c(dn.ans, new_dn)))
    ##  Return the answer.
    return(result)
}




#####  Reminder from 'LG_approx', with regard to testing the sanity of
#####  stuff later on.
#############---------------------------------------------------------
###  The original plan of using 'plyr::aaply' had to be adjusted as it
###  turned out that it simply did not want to cooperate when used on
###  'arg_grid_h' (even though it worked fine with 'arg_grid_0').  To
###  get around this, an had-hoc solution based on 'foreach' had to be
###  made, resulting in the function 'my_aaply'.  This has been tested
###  in order to ensure that it return an array that coincided with
###  the one that 'aaply' returned when used on 'arg_grid_0', and I
###  hope that is sufficient to ensure that the results are sound.
###  More testing of the sanity of the code should be included later
###  on to make sure that nothing has gone awry.  Reminder for later
###  on: In order to compare the result from 'my_aaply' and 'aaply',
###  it might be required to use 'restrict_array' to permute the
###  dimensions, as they internally are returned in a slightly
###  different manner.
#############---------------------------------------------------------




#####  2014-10-01, a working condition, that I now would like to
#####  enhance in order to allow for the code to run in parallel.

## my_apply <- function (X, MARGIN, FUN, ...,
##                       .list = FALSE, .front = TRUE)  {
## ###-------------------------------------------------------------------
##     ##  Record the name of the function, to be used as a part of the
##     ##  dimension-names when those are not delivered as part of the
##     ##  result of 'FUN'.
##     FUN_name <- deparse(substitute(FUN))
## ###-------------------------------------------------------------------
##     ##  Find the function of interest.
##     FUN <- match.fun(FUN) #  The actual function
## ###-------------------------------------------------------------------
##     ##  Update the formals of 'FUN' with any arguments from '...'.
##     update_formals(.fun = FUN, ..., .list = .list)
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
## #####  TASK: New code here, test if X is an array, and if not,
## #####  transform it to one.
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  Find the dimension of X, and to a sanity check.
##     dl <- length(dim(X))
##     if (!dl) 
##         stop("dim(X) must have a positive length")
## #####  NOTE: Vectors are not accepted. (But will be after new code has
## #####  been added later on).
## ###-------------------------------------------------------------------
##     ##  Extract information about X.
##     d <- dim(X)
##     dn <- dimnames(X)
##     dnn <- names(dn)
## ###-------------------------------------------------------------------
##     ##  Sanity check of the case when 'MARGIN' is specified by
##     ##  characters - with transformation to corresponding indexes.
##     if (is.character(MARGIN)) {
##         if (is.null(dnn)) 
##             stop("'X' must have named dimnames")
##         MARGIN <- match(MARGIN, dnn)
##         if (anyNA(MARGIN)) 
##             stop("not all elements of 'MARGIN' are names of dimensions")
##     }
## ###-------------------------------------------------------------------
##     ##  If no dimension-names are given, add them in order to keep
##     ##  track of the different pieces.
##     if (is.null(dn)) {
##         orig.names <- vector(mode = "list", length = dl)
##         for (i in 1:dl)
##             orig.names[[i]] <- paste("or", i, "_", 1:d[i], sep = "")
##         dimnames(X) <- orig.names
##         dn <- orig.names
##         }
## ###-------------------------------------------------------------------
##     ##  If no names for the dimension-names are given, add them in
##     ##  order to make 'aa_restrict' work properly.
##     if (is.null(dnn))
##         names(dn) <- paste("orig", 1:dl, sep="")
## ###-------------------------------------------------------------------
##     ##  The stuff that survives from 'X' into the resulting array is:
##     d.ans <- d[MARGIN]
##     dn.ans <- dn[MARGIN]
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  The part concerning the dimensions, dimension-names and
##     ##  _names_ of dimension-names of 'X' are now finished.  We now
##     ##  need to extract the dimensions and dimension-names of the
##     ##  result from 'FUN'.  If these attributes are missing, then
##     ##  defaults will be created based on the function-name.
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  We need an example of what 'FUN' returns, and thus need to use
##     ##  it on a representative sub-array of 'X'. 'restrict' can find
##     ##  this based on a the following list.
##     extract_list <- as.list(rep(1, length(MARGIN)))
##     names(extract_list) <- dnn[MARGIN]
## ###-------------------------------------------------------------------
##     ##  Compute an example of what 'FUN' returns, when the result is
##     ##  forced into the format of an array.
##     inspect_result_FUN <- as.array(
##         FUN(aa_restrict(.arr = X,
##                         .restrict = extract_list)))
## ###-------------------------------------------------------------------
## ###------------------------------------------------------------------- 
##     ##  Note: 'as.array' used on a vector does not give a "proper"
##     ##   array, since the dimension will be one-dimensional.  Such
##     ##   "degenerate" arrays often spell trouble in the code when used
##     ##   in computations , but for our present inspection it can be
##     ##   exploited to make the code simpler.
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  Investigate what kind of dimension, dimension-names and _names_
##     ##  of dimension-names that are present in the result from 'FUN'.
##     new_d <- dim(inspect_result_FUN)
##     new_dl <- length(new_d)
##     new_dn <- dimnames(inspect_result_FUN)
##     new_dnn <- names(new_dn)
## ###-------------------------------------------------------------------
##     ##  Add dimension-names when needed.  The code is somewhat messy,
##     ##  but it does make the printed result look (slightly) nicer.
##     if (is.null(new_dn)) {
##         ##  Use 'FUN_name' to create dimension-names, make a
##         ##  distinction based on the format of the output.
##         if (identical(new_dl, 1L) ) {
##             if (identical(new_d, 1L)) {
##                 new_dn <- list(FUN_name) # The simplest case.
##             } else {
##                 new_dn <-
##                     list(paste(FUN_name,
##                                1:new_d,
##                                sep = "_") ) # Still a simple format.
##             }
##         } else {
##             new_dn <-
##                 vector(mode = "list",
##                        length = new_dl)
##             for (i in 1:new_dl)
##                 new_dn[[i]] <-
##                     paste(FUN_name,
##                           "_",
##                           i,
##                           ".",
##                           1:new_d[i],
##                           sep = "")
##         }
##     }
## ###-------------------------------------------------------------------
##     ## Add _names_ on the dimension-names, when needed.
##     if (is.null(new_dnn)) {
##         ##  Use 'FUN_name' to create _names_ on the dimension-names.
##         if (identical(new_dl, 1L)) {
##             if (! identical(new_d, 1L) | ! is.null(new_dn)) {
##                 names(new_dn) <- FUN_name # No "_1" for the name in this case.
##             } # else let it be as it has been defined.
##         } else {
##             names(new_dn) <-
##                 paste(FUN_name,
##                       1:new_dl,
##                       sep="_")
##         }
##     }
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
## #####  TASK: What if only a few of these dimension-names are missing?
## #####  I think this might need a simple help-function that does a more
## #####  thorough analysis of the situation.  The setup here should
## #####  anyway be cleaned up, since it is awful as it is now.  I do
## #####  think an internal help-function that takes care of all that
## #####  crap might be the better solution to go for. The readability of
## #####  this code would at least increase drastically.
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  We can now create the desired information for the result.
##     new_dim <- c(new_d, d.ans)
##     new_dimnames <- c(new_dn, dn.ans)
## ###-------------------------------------------------------------------
##     ##  Call 'apply' (to compute the result in an efficient way), and
##     ##  adjust the dimension and dimension-names.  (Any arguments to
##     ##  'FUN' given by the dotsMethods, i.e. '...' has already been
##     ##  taken into account at the beginning of this function.)
##     result <- apply(X = X, MARGIN = MARGIN, FUN = FUN)
## ###-------------------------------------------------------------------
##     ##  Create an array from result, with the desired properties
##     if (is.array(result)) {
##         dim(result) <- new_dim
##         dimnames(result) <- new_dimnames
##     } else {
##         result <- array(data = result,
##                         dim = new_dim,
##                         dimnames = new_dimnames)
##     }
## ##### TASK: Do a test to see if this way of doing the modification
## ##### actually does reduce the overhead in those cases where 'result'
## ##### already is an array.
## ###-------------------------------------------------------------------
##     ##  If '.front' equals FALSE, do a permutation such that the new
##     ##  dimensions occur at the "back" of the array.
##     if (! .front)
##         result <- aperm(a = result,
##                         perm = names(c(dn.ans, new_dn)))
##     ##  Return the answer.
##     return(result)
## }
## 

#####  2014-10-01, the overhead for the approach used here turned out
#####  to be insanely large, so although the idea of making an
#####  alternative my_apply did give a working version, it can by no
#####  means be recommended to do it this way.  However, I do believe
#####  it should be feasible to recast the content in a from enabling
#####  it to be a wrapper around the ordinary apply, and furthermore
#####  that it might be possible to do this in a manner consistent
#####  with using parallel processors.


## my_apply <- function (X, MARGIN, FUN, ..., .front = TRUE)  {
## ###-------------------------------------------------------------------
##     ##  Record the name of the function, in case it does not give out
##     ##  a result containing dimension-names, and we thus need to add
##     ##  those later on.
##     FUN_name <- deparse(substitute(FUN))
## ###-------------------------------------------------------------------
##     ##  Find the function of interest.
##     FUN <- match.fun(FUN) #  The actual function
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
## #####  TASK: New code here, test if X is an array, and if not,
## #####  transform it to one.
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  Find the dimension of X, and to a sanity check.
##     dl <- length(dim(X))
##     if (!dl) 
##         stop("dim(X) must have a positive length")
## #####  NOTE: Vectors are not accepted. (But will be after new code has
## #####  been added later on).
## ###-------------------------------------------------------------------
##     ##  Extract information about X.
##     d <- dim(X)
##     dn <- dimnames(X)
##     dnn <- names(dn)
## ###-------------------------------------------------------------------
##     ##  Sanity check of the case when 'MARGIN' is specified by
##     ##  characters - and - transformation of it to indexes.
##     if (is.character(MARGIN)) {
##         if (is.null(dnn)) 
##             stop("'X' must have named dimnames")
##         MARGIN <- match(MARGIN, dnn)
##         if (anyNA(MARGIN)) 
##             stop("not all elements of 'MARGIN' are names of dimensions")
##     }
## ###-------------------------------------------------------------------
##     ##  If no dimension-names are given, add them in order to keep
##     ##  track of the different pieces.
##     if (is.null(dn)) {
##         dummy.names <- vector(mode = "list", length = dl)
##         for (i in 1:dl)
##             dummy.names[[i]] <- paste("or", i, "_", 1:d[i], sep = "")
##         dimnames(X) <- dummy.names
##         dn <- dummy.names
##         }
## ###-------------------------------------------------------------------
##     ##  If no names for the dimension-names are given, add them in
##     ##  order to make 'aa_restrict' work properly.
##     if (is.null(dnn))
##         names(dn) <- paste("orig", 1:dl, sep="")
## ###-------------------------------------------------------------------
## #####  TASK: Clean up the descriptive code in this mess.
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  Now starts the core of the machinery.
## ###-------------------------------------------------------------------
##     ##  We want to use 'restrict' (and its replacement ability) to
##     ##  solve the task.  We need an argument grid containing the
##     ##  list-objects to be used when restricting.
##     arg.grid <- expand.grid(dn[MARGIN], stringsAsFactors = FALSE)
## ###-------------------------------------------------------------------
##     ##  Use the first row of 'arg.grid' to get an idea of what 'FUN'
##     ##  will return, and use this to create the storage area that we
##     ##  want to use. Note: Use "drop=FALSE" in the subsetting of
##     ##  'arg.grid' to ensure that we get a list also for those cases
##     ##  where only one margin has been specified.
## ##### TASK: I suspect that the 'as.array' here needs to be modified by
## ##### a more proper transformation from vector to array.
##     inspect_result_FUN_1 <- as.array(
##         FUN(aa_restrict(.arr = X,
##                         .restrict = arg.grid[1, , drop = FALSE])))
## ###-------------------------------------------------------------------
##     ##  Depending on the properties of 'inspect_result_FUN_1' make
##     ##  sure to create a storage array with suitable dimensions and
##     ##  dimension names.
## #####  TASK: Include an aspect of doing stuff in parallel here too?
## #####  Might be that I need to experiment a little bit with regard to
## #####  figure out how to bind the parts together again, since I am not
## #####  quite sure if a naive approach can be used on the splitting of
## #####  the argument array.  That can be a TASK for later on to
## #####  contemplate about.
##     ##  From the original array, we will use the dimension-names
##     ##  specified by 'dn.ans', in addition we want to see if our
##     ##  result might have some dimension-names too.
##     new_d <- dim(inspect_result_FUN_1)
##     new_dl <- length(new_d)
##     new_dn <- dimnames(inspect_result_FUN_1)
##     new_dnn <- names(new_dn)
## ###-------------------------------------------------------------------
##     ##  A couple of Boolean objects to be used below
##     new_dn_bool <- is.null(new_dn)
##     new_dnn_bool <- is.null(new_dnn)
## ###-------------------------------------------------------------------
##     ##  Based on 'new_dn_bool' and 'new_dnn_bool', check if we need to
##     ##  add dimension-names, or _names_ of dimension-names.  The code
##     ##  is somewhat messy here, but it does make the printed result
##     ##  look less messy in the simplest cases.
##     if (new_dn_bool) {
##         ##  Use 'FUN_name' to create dimension-names, make a
##         ##  distinction based on the format of the output.
##         if (identical(new_dl, 1L) ) {
##             if (identical(new_d, 1L)) {
##                 new_dn <- list(FUN_name) # The simplest case.
##             } else {
##                 new_dn <-
##                     list(paste(FUN_name,
##                                1:new_d,
##                                sep = ".") ) # Still a simple format.
##             }
##         } else {
##             new_dn <-
##                 vector(mode = "list",
##                        length = new_dl)
##             for (i in 1:new_dl)
##                 new_dn[[i]] <-
##                     paste(FUN_name,
##                           i,
##                           1:new_d[i],
##                           sep = ".")
##         }
##     }
##     ## Add names on the dimension-names, if that is needed.
##     if (new_dnn_bool) {
##         ##  Use 'FUN_name' to create names for the dimension-names.
##         if (identical(new_dl, 1L)) {
##             if (! identical(new_d, 1L) | ! new_dn_bool) {
##                 names(new_dn) <- FUN_name # No "_1" for the name in this case.
##             } # else let it be as it has been defined.
##         } else {
##             names(new_dn) <-
##                 paste(FUN_name,
##                       1:new_dl,
##                       sep="_")
##         }
##     }
## #####  TASK: What if only a few of these dimension-names are missing?
## #####  I think this might need a simple help-function that does a more
## #####  thorough analysis of the situation.  The setup here should
## #####  anyway be cleaned up, since it is awful as it is now.
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  The dimension of the result should in addition to the
##     ##  surviving dimensions and dimension-names from 'X' also reflect
##     ##  whether or not 'FUN' only returns one unnamed single value.
## ###-------------------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  The stuff that survives from 'X'.
##     d.ans <- d[MARGIN]
##     dn.ans <- dn[MARGIN]
## ###-------------------------------------------------------------------
##     ##  The construction of the dimensions and dimension-names to be
##     ##  used on the final result.
## #####  Modifications made in order to get the stuff working properly.
##     if (.front) {
##         new_dim <- c(new_d, d.ans)
##         new_dimnames <- c(new_dn, dn.ans)
##     } else {
##         new_dim <- c(d.ans, new_d)
##         new_dimnames <- c(dn.ans, new_dn)
##     }
## ###-------------------------------------------------------------------
##     ##  Create the array to store the result in
##     result <-
##         array(data = 1,
##               dim = new_dim,
##               dimnames = new_dimnames)
## ###-------------------------------------------------------------------
##     ##  Add the piece we already computed (in order to find the format
##     ##  for the resulting array).  Note that 'aa_restrict' makes
##     ##  changes directly upon the '.arr'-argument when '.replace' is
##     ##  specified.
##     aa_restrict(.arr = result,
##                 .restrict = arg.grid[1, , drop = FALSE],
##                 .replace = inspect_result_FUN_1)
## ###-------------------------------------------------------------------
##     ##   Add the remaining pieces.
##     for (i in 2:dim(arg.grid)[1]) {
##         inspect_result_FUN_i  <- as.array(
##             FUN(restrict(Arr = X,
##                          .restrict = arg.grid[i, , drop = FALSE])))
##         aa_restrict(.arr = result,
##                     .restrict = arg.grid[i, , drop = FALSE],
##                     .replace = inspect_result_FUN_i)
##     }
## ###-------------------------------------------------------------------
##     ##  Return the answer.
##     return(result)
## }

