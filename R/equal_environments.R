################################################################################
#'
#' Equal environments.
#'
#' This function will check if two environments are equal enough to be
#' considered interchangeable, i.e. with regard to having them
#' attached to the \code{search}-path.
#'
#' @details This function is mainly intended as a helper-function for
#'     \code{options_attach}, and its present incarnation does not go
#'     deep into the internal hierarchy of the environments.  The
#'     procedure starts out by testing if the environments are
#'     identical, and if not it looks into them to see if they contain
#'     the exact same content.  If the main environments contains
#'     environments themselves, then the test will only check if these
#'     are identical.  It's possible to use an iterative algorithm,
#'     but that would require the addition of a safeguard against
#'     infinite recursions.
#'
#' @param env1 The first environment to investigate
#'
#' @param env2 The second environment to investigate.
#'
#' @return The result of this function will be either \code{TRUE} or
#'     \code{FALSE}, with an attribute \code{identical} that reveals
#'     the result of the identical test.
#'
#' @export

equal_environments <- function(env1, env2) {
    ##  Sanity-check the arguments.
    .is_env1_environment <- is.environment(env1)
    .is_env2_environment <- is.environment(env2)
    if (! all(c(.is_env1_environment, .is_env1_environment))) {
        .info_text <-
            if (! all(c(.is_env1_environment,
                        .is_env1_environment))) {
                "None of them are environments"
            } else
                paste("Only ",
                      ifelse(
                          test = .is_env1_environment,
                          yes  = "env1",
                          no   = "env2"),
                      " is an environment.",
                      sep = "")
        error(.argument = c("env1", "env2"),
              c("These arguments must both be environments!",
                .info_text))
    }
###-------------------------------------------------------------------
    ##  Are they identical?
    .identical <- identical(env1, env2)
    if (! .identical) {
        ##  Find the names of the content in the two environments.
        .env1_names <- ls(envir = env1, all.names = TRUE)
        .env2_names <- ls(envir = env2, all.names = TRUE)
        ##  Compare the content in order to conclude.
        .result <-
            if (identical(.env1_names, .env2_names)) {
                ##  Same content referred to by the names? Use an
                ##  `if`+`break` construction to terminate immediately
                ##  if it turns out to be any difference.
                .tmp <- FALSE
                for (i in .env1_names) {
                    .tmp <- identical(
                        x = env1[[i]],
                        y = env2[[i]])
                    if (! .tmp)
                        break
                }
                .result <- .tmp
            } else
                FALSE
    } else
        .result <- .identical
    ##  Return the result
    structure(.Data = .result,
              identical = .identical)
}
