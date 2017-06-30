################################################################################
#####  2016-04-03

#' Find package and formals for exported and non-exported functions.
#'
#' This function takes care of the extraction of information related
#' to a function, i.e. the name of the function, the package(s) it
#' belongs to (if it does belong to any packages), whether or not it
#' is exported from these packages, and the formals of these
#' functions.  If the function also can be found in \code{.GlobalEnv},
#' then it will be registered with package given as \code{"NULL"}.
#' 
#' @param target This can either be the function itself, or it can be
#'     the name of it (as a symbol or a character-string), or it can e
#'     a call or an expression that contains the desired function.
#'
#' @return A list containing the name of the function (as a
#'     character-string), the package it belongs to (or NULL) if no
#'     package was detected, and the arguments of the function.  If
#'     the function was given as a call or expression, any values
#'     specified there will be included, otherwise the default values
#'     will be used instead.
#'
#' @export 

fun_info <- function(target) {
###-------------------------------------------------------------------
    ##  Identify what 'target' is.
    target_type <- c(
        character = is.character(target),
        name = is.name(target),
        fun = is.function(target),
        call = is.call(target))
    ##  Stop if not implemented
    if (! any(target_type))
        stop("\t'fun_info' used with erroneous argument:\n\t",
             "'target' has mode '",
             mode(target),
             "'.",
             call. = FALSE)
###-------------------------------------------------------------------
##  Get the name of the function.
    .fun_name <- switch(
        EXPR = names(target_type)[target_type][1],
        character = target,
        name = as.character(target),
        fun = deparse(substitute(target)),
        call = capture.output(target[[1]]))
    ##  KIT
    rm(target)
    ##  Is the function given as "package::fun" or "package:::fun"?
    .package_specified <- str_detect(
        string = .fun_name,
        pattern = ":::|::")
    ##  Find the package and the function (if they exist).
    if (.package_specified) {
        ##  In this case: If 'target' was given as a call, then the
        ##  first and last character has to be trimmed away.
        if (target_type["call"]) 
            .fun_name <- substr(x = .fun_name,
                               start = 2,
                               stop = nchar(.fun_name) - 1)
        ##  Split '.fun_name' into package and function
        .tmp <- str_split(
            string = .fun_name,
            pattern = ":::|::")
        .package <- .tmp[[1]][1]
        .fun_name <- .tmp[[1]][2]
        ##  KIT
        rm(.tmp)
        ##  Sanity check that valid information has been captured,
        ##  i.e. that a NAMESPACE for '.package' has been loaded, and
        ##  that '.fun_name' can be found there.  Stop if something is
        ##  amiss.
        if (! isNamespaceLoaded(name = .package))
            stop("\t'fun_info' used with erroneous argument:\n\t",
                 "'target' included a reference to the package '",
                 .package,
                 "',\n\t",
                 "but no NAMESPACE for this has been loaded.",
                 call. = FALSE)
        if (! any(ls(asNamespace(ns = .package),
                     all.names = TRUE) == .fun_name))
            stop("\t'fun_info' used with erroneous argument:\n\t",
                 "'target' pointed to the function '",
                 .fun_name,
                 "' in the package '",
                 .package,
                 "',\n\t",
                 "but no match was found in the corresponding NAMESPACE.",
                 call. = FALSE)
        ##  Still running? Make a local copy of the function.
        .fun_copy <- list(do.call(
            what = ":::",
            args = list(.package, .fun_name)))
    } else {
        ##  Need to find the function, and the package(s) it's in.
        .fun_info <- getAnywhere(x = .fun_name)
        ##  Stop if nothing was found.
        if (length(.fun_info$where) == 0)
            stop("\t'fun_info' used with erroneous argument:\n\t",
                 capture.output(.fun_info),
                 call. = FALSE)
        ##  Still running? Figure out where stuff can be found.
        ##  Reminder: ".GlobalEnv" can occur if '.fun_name' is defined
        ##  there too, otherwise the search-path and the NAMESPACEs
        ##  are investigated'.
        .namespaces_pos <- str_detect(
            string  = .fun_info$where,
            pattern = "namespace:")
        .package <- str_replace(
            string = .fun_info$where[.namespaces_pos],
            pattern = "namespace:",
            replacement = "")
        ##  Collect copies of the functions from the NAMESPACEs.
        .fun_copy <- .fun_info$objs[.namespaces_pos]
        ##  KIT
        rm(.namespaces_pos)
        ##  If the function is defined in '.GlobalEnv', add that to
        ##  the other information.  Use "NULL" for the package.
        if (any(.fun_info$where == ".GlobalEnv")) {
            .package <- c(.package, "NULL")
            .fun_copy <- c(.fun_copy,
                           .fun_info$objs$.GlobalEnv)
        }
        ##  A sanity-check.  If the process above didn't capture
        ##  anything, then something is mighty strange, and it's
        ##  better to call an error.
        if (length(.package) == 0)
            stop("\t",
                 "Something strange occurred in 'fun_info':\n\t",
                 "Neither the NAMESPACEs nor '.GlobalEnv' contained the target '",
                 .fun_name,
                 "'.\n\t",
                 "Here's the result from 'getAnywhere(x = ",
                 .fun_name,
                 ")':\n\n\t",
                 paste(capture.output(.fun_info),
                       collapse = "\n\t"),
                 "\n\n\tDoes this make sense to you?",
                 call. = FALSE)
        ##  KIT
        rm(.fun_info)
    }
    ## KIT
    rm(.package_specified, target_type)
###-------------------------------------------------------------------
    ##  Find the formals of the function(s) that has been captured.
    .fun_formals <- structure(
        .Data = lapply(
            X = .fun_copy,
            FUN = formals),
        .Names = .package)
    ##  Check if the detected function(s) would be exported when the
    ##  package(s) is loaded.  Not relevant when defined in
    ##  '.GlobalEnv' -- that was registered with "NULL".
    .fun_exported <- vapply(
        X = setdiff(x = .package,
                    y = "NULL"),
        FUN = function(x) {
            any(ls(asNamespace(ns = x)$.__NAMESPACE__.$exports,
                   all.names = TRUE) == .fun_name)
        },
        FUN.VALUE = logical(1))
    if (any(.package == "NULL"))
        .fun_exported <- c(.fun_exported, "NULL" = FALSE)
###-------------------------------------------------------------------
    ##  Return the result to the work-flow.
    list(name = .fun_name,
         package = .package,
         functions = .fun_copy,
         formals = .fun_formals,
         exported = .fun_exported)
}


