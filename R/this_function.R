################################################################################
#'
#' Capture information about the function above.
#'
#' This function collects information about the function that it's
#' called from, based on the result from \code{sys.status}.  This
#' function can extract a variety of information, that other functions
#' can piggyback on instead of having to include large chunks of
#' similar code.
#'
#' @param n A positive integer that select which generations to
#'     report.  The default value \code{1} selects the parent of
#'     \code{this_function}, i.e. the function from which it was
#'     called.  \code{2} gives the grandparent (needed when
#'     \code{this_function} is called from within the
#'     \code{error}-function), and so on.  An error will be returned
#'     if \code{n} is out of range.
#'
#' @param details A value that is compared against the values
#'     \code{c(0, 1, 2, 3, 4)}, which decides the amount of additional
#'     information to include as attributes.  The default will be to
#'     use \code{0}.
#' 
#' @return This function will return a result that depends on the
#'     values of \code{n} and \code{details}, where the first
#'     specifies which function to return whereas the latter one
#'     decides the amount of information included as attributes.  No
#'     attributes will be added when \code{details} is given as
#'     \code{0}.  Higher values of \code{details} adds attributes as
#'     described below.
#'
#' \describe{
#'
#' \item{info}{Attribute added when \code{details} is greater than or
#'     equal to \code{1}, which is a list that specifies the following
#'     details about the function that has been returned:
#'     \code{origin}, \code{package}, \code{status} and
#'     \code{arguments}.  The value in \code{origin} presents the
#'     place where the function was defined, which can be a namespace,
#'     the global environment or the name of another function.
#'     \code{package} is a logical value that is true when
#'     \code{origin} refers to a package.  \code{status} will be
#'     either \code{exported} or \code{internal} when the function
#'     originates from some package, it will be \code{global} if the
#'     function was defined in the global environment, it will be
#'     \code{local} if it was defined in some other function, or it
#'     will be \code{unknown} if the algorithm couldn't select one of
#'     the other ones (this might e.g. happen if a function has been
#'     defined in an environment that has been \code{attach}ed to the
#'     search-path.}
#'
#'
#' \item{lineage}{Attribute added when \code{details} is greater than
#'     or equal to \code{2}.  This is a list having names equal to the
#'     functions recorded from the call-stack, and each component of
#'     this list contains details as given for the attribute
#'     \code{info}.}
#'
#' \item{sys.status}{Attribute added when \code{details} is greater
#'     than or equal to \code{3}.  A named list that gives the
#'     \code{sys.status}-information that was used to create the other
#'     attributes, see the help-page of \code{sys.parent} for the
#'     details.}
#' 
#' \item{content}{Attribute added when \code{details} is greater than
#'     or equal to \code{4}.  A named list that for each occurrence of
#'     a function adds a list with a component \code{objects}, which
#'     gives the content of the functions environment; and a component
#'     \code{is_test} that holds an array where these components has
#'     been tested against the \code{is.<something>} tests exported
#'     from the namespaces.  Note that the result listed here will be
#'     the status at the time \code{this_function} was called, and
#'     that the status of the environments might have changed if it's
#'     called at different times within the same target function.}
#'
#' }
#' 
#'
#' @export

#####   TASK: Consider solution for `dotsMethods`, and if it might be
#####   an idea to add an argument where some parts might be turned of
#####   from the returned computation, in particular if details about
#####   the 

this_function <- function(n = 1, details = c(0, 1, 2, 3, 4)) {
    ##  Nothing to do when called from the global environment.
    if (identical(.GlobalEnv, sys.frame(which = -1))) {
        message("\n`this_function` doesn't do anything when ",
                "called from the global environment.\n")
        return(invisible(NULL))
    }
###-------------------------------------------------------------------
    ##  Restrict `details` to one value.
    details <- details[1]
###-------------------------------------------------------------------
    ##  Register `sys.status`.
    .sys_status <- sys.status()
###-------------------------------------------------------------------
    ##  Sanity-check the `n`, and if valid convert it to give the
    ##  value to be used for subsetting later on.  Keep in mind that
    ##  the last to parts of the stuff captured by `sys.status` will
    ##  refer to itself and `this_function`, so adjustments must be
    ##  made to cope with that.
    if (! n %in% head(seq_along(.sys_status$sys.calls), n = -2)) {
##### TASK: Add more detailed error-message.
        stop("Invalid `n` argument for `this_function`",
             call. = FALSE)
    } else
        .select <- length(.sys_status$sys.calls) - n - 1
    ##  Truncate `.sys_status` to the part of interest.
    .sys_status <- lapply(X = .sys_status,
                         FUN = head,
                         n = .select)
    kill(n)
###-------------------------------------------------------------------
    ##  Add names to all the components of `sys.status`, to simplify
    ##  the work later on.  Note that the two last entries
    ##  respectively corresponds to `this_function`and `sys.calls`.
    .fun_names <- vapply(
        X = .sys_status$sys.calls,
        FUN = function(x) {
            .tmp <- deparse(x[[1]])
            ##  Adjustment to avoid problems when used in a
            ##  shiny-environment, where functions can be defined
            ##  'at the spot'
            if (length(.tmp) != 1) {
                "_internal_"
            } else
                .tmp
        },
        FUN.VALUE = character(1))
###-------------------------------------------------------------------
    ##  Pick out the function to return as the main part of the answer
    ##  we're giving.
    .result <- .fun_names[.select]
    ##  Check if `details` indicate that an answer should be returned.
    if (details < 1) {
        return(.result)
    } else {
        ##  Add names to the lists.  Reminder: The names are put on
        ##  the environments that contains their content.  Keep in
        ##  mind that `sys.parents` "returns an integer vector of
        ##  indices of the system parents, that can be used when
        ##  investigating the formals of the functions.  Note that the
        ##  system parent refers to environment in which the function
        ##  was called, and that is not necessarily the parent of the
        ##  environment that occurs in `.sys_status$sys.frames`.
        names(.sys_status$sys.calls) <- .fun_names
        names(.sys_status$sys.frames) <- .fun_names
        names(.sys_status$sys.parents) <- .fun_names
        ##  An additional reference list to the functions, based on
        ##  symbols and calls, in order to avoid problems when
        ##  `.fun_names` picked up something that was given as
        ##  "<package>::<fun>" or "<package>:::<fun>".
        .fun_ref <- structure(
        .Data = lapply(
            X = .sys_status$sys.calls,
            FUN = function(x) {
                x[[1]]
            }))
    }
###-------------------------------------------------------------------
    ##  Use the information in `sys.parents` to look for the formals
    ##  of the different functions.  IMPORTANT: The same function can
    ##  occurs several times!  Use `seq_along` to pick out the correct
    ##  information, or else only the stuff for the first occurrence
    ##  will be captured!
    .fun_formals_env <- structure(
        .Data = lapply(
            X = seq_along(.fun_names),
            FUN = function(i) {
                ##  Create calls for the computation of the formals
                ##  and the environment of the function.
                x_ref <- .fun_ref[[i]]
                .formal_call <- call(name = "formals", x_ref)
                .environment_call <- call(name = "environment", x_ref)
                .env_key <- .sys_status$sys.parents[i]
                .fun_in_env <- if (.env_key == 0) {
                    .GlobalEnv
                } else
                    .sys_status$sys.frames[[.env_key]]
                ##  Find the formals, and give a pointer to the
                ##  environment in which the function was defined.
                ##  The result is `NULL` for the formals when no
                ##  argument is present, and if it's a locally defined
                ##  environment we should see it from the from of the
                ##  returned format.
                list(
                    formals = eval(expr = .formal_call,
                                   envir = .fun_in_env),
                    
                    def_env = eval(expr = .environment_call,
                                   envir = .fun_in_env))
#####  TASK: Add something to pick up information about the
#####  `dotsMethods`, when used?  I think that might be somewhat
#####  interesting to store.
            }),
        .Names = .fun_names)
###-------------------------------------------------------------------
    ##  Figure out where the functions was defined.  Procedure:
    ##  Capture the character-expressions for the environments from
    ##  `.fun_formals_env` and compare them against the similarly
    ##  treated environments from `.sys_status$sys.frames`.  Either
    ##  this will reveal that the function originates from some
    ##  namespace, or that it has been defined in the global
    ##  environment - or it will reveal that it has been defined in
    ##  some internal environment.  In the latter case, a comparison
    ##  will be done in order to figure out if that environment
    ##  pinpoints another function as its parent.  In addition to
    ##  this, it will also be added information that reveals if the
    ##  function originates from a package, and if `TRUE` it status
    ##  will be recorded as "exported" or "internal".  If package is
    ##  `FALSE`, the status will either be "global", "local" or
    ##  "unknown", depending on the results of the procedure.
####-------------------------------------------------------------------
    ##  Character version of `.sys_status$sys.frames`.
    .char_sys_frames <- vapply(
        X = .sys_status$sys.frames,
        FUN = capture.output,
        FUN.VALUE = character(1))
    ##   character version of environments from `.fun_formals_env`.
    ##   NOTE: This will be updated to keep track of the progress.
    .char_fun_env <- structure(
        .Data = vapply(
            X = seq_along(.fun_formals_env),
            FUN = function(x) {
                .tmp <- capture.output(.fun_formals_env[[x]]$def_env)
                ##  Adjustment to avoid problems when used in a
                ##  shiny-environment, where functions can be defined
                ##  'at the spot'
                if (length(.tmp) != 1) {
                    "_internal_"
                } else
                    .tmp
            },
            FUN.VALUE = character(1)),
        .Names = .fun_names)
    ##  Information list to be filled out with more details about the
    ##  origin of the functions.  To be used as an attribute later on.
    .fun_data <- structure(
        .Data = vector(mode = "list",
                       length = length(.fun_names)),
        .Names = .fun_names)
###-------------------------------------------------------------------
    ##  Strategy, replace the values in '.car_fun_env' so they provide
    ##  information about the package stuff belongs to.  Start out by
    ##  replacing those defined in the global environment.
    .fun_global <- .char_fun_env == "<environment: R_GlobalEnv>"
    .char_fun_env[.fun_global] <- "Global environment"
    for (.i in seq_along(.fun_names)[.fun_global]) 
        .fun_data[[.i]] <- list(origin = "Global environment",
                                  package = FALSE,
                                  status = "global")
    kill(.fun_global, .i)
    ##  Identify those that contains references to namespaces, 
    .namespace_identification <- "<environment: namespace:"
    .namespace_trigger <- seq_along(.fun_names)[str_detect(
        string = .char_fun_env,
        pattern = .namespace_identification)]
    ##  Update `.char_fun_env` and these parts.
    for (.i in .namespace_trigger) {
        .char_fun_env[.i] <- substr(
            x =.char_fun_env[.i],
            start = nchar(.namespace_identification) + 1,
            stop = nchar(.char_fun_env[.i]) - 1)
        .fun_data[[.i]] <- list(
            origin = .char_fun_env[.i],
            package = TRUE,
            status = fun_status(.fun = .fun_names[.i],
                                .package = .char_fun_env[.i]))
        }
    kill(.namespace_identification, .namespace_trigger, .i)
    ##  Investigate if there's any further matches to be made, by
    ##  comparing against `.char_sys_frames`.
    for (.i in seq_along(.char_fun_env)) {
        .match <- .char_fun_env[.i] %in% .char_sys_frames
        if (.match) {
            .char_fun_env[.i] <- unique(
                names(.char_sys_frames)[
                         .char_sys_frames == .char_fun_env[.i]])
            .fun_data[[.i]] <- list(
                origin = .char_fun_env[.i],
                package = FALSE,
                status = "local")
        }
    }
    kill(.i, .match, .char_sys_frames)
###-------------------------------------------------------------------
    ##  If there's still something left to identify, add some basic
    ##  information to reveal that we don't know where the function is
    ##  defined.
    .unknown <- seq_along(.fun_names)[vapply(
                             X = .fun_data,
                             FUN = is.null,
                             FUN.VALUE = logical(1))]
    for (.i in .unknown)
        .fun_data[[.i]] <- list(
            origin = .char_fun_env[.i],
            package = FALSE,
            status = "unknown")
    kill(.i, .unknown, .char_fun_env)
###-------------------------------------------------------------------
    ##  Add the names of the formals to `the_data`, to simplify
    ##  inspections later on.
    for (.i in seq_along(.fun_names))
        .fun_data[[.i]]$arguments  <-
            names(.fun_formals_env[[.i]]$formals)
    kill(.i, .fun_formals_env)
###-------------------------------------------------------------------
    ##  Use `details` to see what attributes to add, the previous test
    ##  of `details` implies that the "info"-attribute has to be added
    ##  now, and a few other parts must be added too.
    attributes(.result) <- c(
        attributes(.result),
        list(info = .fun_data[[.select]]),
        if (details >= 2)
            list(lineage = head(x = .fun_data, n = .select)),
        if (details >= 3)
            list(sys.status = .sys_status))
    ##  Return the result if there's no need to compute the last part.
    if (details < 4)
        return(.result)
###-------------------------------------------------------------------
    ##  Still running, then it's time to compute the last attribute,
    ##  which contains detailed information about the content of the
    ##  different environments encountered in the system frames.  For
    ##  each environment, the following will be collected: The names
    ##  of everything stored in the environments, the result of `is`
    ##  used upon these names, and a matrix giving details with regard
    ##  to all kinds of `is.<something>` tests that was detected in
    ##  the present namespaces.
###-------------------------------------------------------------------
    ##  Find all the exported "is.<something>" functions from the
    ##  available namespaces, and adjust the content so a matrix can
    ##  be created based on these.
    .is_something <- the_Namespaces(
        .details = TRUE,
        .pattern  = glob2rx("is.*"),
        .type = "exported")
    ##  Extract information about the functions only.
    .is_something <- structure(
        .Data = lapply(
            X = .is_something,
            FUN = function(x){
                x <- attributes(.is_something)[[x]]
                if (length(x$content) == 0) {
                    NULL
                } else 
                    ##  Restrict to functions
                    x$content[vapply(
                        X = x$is,
                        FUN = function(y) {
                            any(y == "function")
                        },
                        FUN.VALUE = logical(1))]
            }),
        .Names = .is_something)
    ##  Discard those having no occurrences.
    .is_something <- .is_something[vapply(
        X = .is_something,
        FUN = function(x) {
            length(x) > 0
        },
        FUN.VALUE = logical(1))]
    ##  Create helper functions to run all the tests, with the twist
    ##  that `NA` is returned if something else than `TRUE` or `FALSE`
    ##  is returned.  This is necessary in order to store the result
    ##  in the desired matrix.
    .is_helper_1 <- function(.fun, .package, .obj, .env) {
        ##  Create an expression for the desired result.
        .expr <- parse(
            text = paste(.package,
                         "::",
                         .fun,
                         "(",
                         .obj,
                         ")",
                         sep = ""))
        ##  Try to evaluate the expression in `.env`, without any
        ##  warnings created from complaining tests.
        .tmp <- suppressWarnings(try(expr = {
            eval(expr = .expr,
                 envir = .env)
        },
            silent = TRUE))
        ##  Adjust the result to `NA` if not `TRUE` or `FALSE`.
        if (any(length(.tmp) != 1,
                ! is.logical(.tmp))) {
            NA
        } else
            .tmp
    }
    ##  Another helper function, using the first one to create one
    ##  single named vector for all the test of one object in one
    ##  environment.
    .is_helper_2 <- function(.obj, .is_something, .env) {
        unlist(lapply(X = names(.is_something),
                      FUN = function(.package) {
                          structure(
                              .Data =vapply(
                                  X = .is_something[[.package]],
                                  FUN = .is_helper_1,
                                  .package = .package,
                                  .obj = .obj,
                                  .env = .env,
                                  FUN.VALUE = logical(1)),
                              .Names = paste(
                                  .package,
                                  "::",
                                  .is_something[[.package]],
                                  sep = ""))
                      }))
        }
###-------------------------------------------------------------------
    ##  Using the helper functions, the most detailed attribute to
    ##  `.result` can be created.  This will be a list which for each
    ##  occurrence of a function contains a list with three
    ##  components: `objects` giving the names of all the objects in
    ##  the functions environment (at the time `this_function` was
    ##  called) - and - `is_test` giving the result of the above
    ##  helper-functions being used upon `objects`.
    .content <- structure(
        .Data = lapply(
            X = seq_along(.fun_names),
            FUN = function(i) {
                .env <- .sys_status$sys.frames[[i]]
                .objects <- ls(envir = .env,
                               all.names = TRUE)
                ##  
                .is_test <- vapply(
                    X = .objects,
                    FUN = .is_helper_2,
                    .is_something = .is_something,
                    .env = .env,
                    FUN.VALUE = logical(length(unlist(.is_something))))
                ##  Return the answer
                list(objects = .objects,
                     is_test = .is_test)
            }),
        .Names = .fun_names
    )
###-------------------------------------------------------------------
    ##  Add the details in `.content` as an attribute to `.result`.
    attributes(.result) <- c(
        attributes(.result),
        content = .content)
    ##  Return `.result` to the workflow.
    .result
}
