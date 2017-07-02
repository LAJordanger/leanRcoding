################################################################################

#' A \code{set.seed}-wrapper to simplify reproducible coding.
#'
#' @details This function wraps around the function \code{set.seed},
#'     \code{RNGversion}-function such that all the details related to
#'     \code{kind}, \code{normal.kind} and \code{vstr} are fixed by
#'     this single function.  Moreover, it's also possible to return
#'     the code for calling this function (with the default values
#'     used) in order to ensure that computational scripts properly
#'     reflect the settings under which they initially were used.
#'     Finally, it's also possible to use this function to create a
#'     list of the present settings, such that these can be integrated
#'     into call of functions that are stored in order to ensure
#'     reproducibility.  See the help-page of \code{Random} for
#'     details about the three arguments \code{kind},
#'     \code{normal.kind} and \code{vstr}.
#'
#' @param seed The integer to be used as the seed value.  This
#'     argument will be given to the ordinary
#'     \code{set.seed}-function.
#'
#' @param expr An expression, default \code{NULL}, to be evaluated
#'     after the seed has been set.  The evaluation will be within the
#'     frame given by the three arguments \code{kind},
#'     \code{normal.kind} and \code{vstr}, and thus gives an extra
#'     level of protection for those cases where the internal workings
#'     of an expression use \code{set.seed} with a fixed value for the
#'     seed.  Note that the seed and the evaluation will be performed
#'     at the level of the calling function.
#'
#' @param envir The environment in which \code{expr} is to be
#'     evaluated, default value \code{parent.frame()}.  See the
#'     documentation of \code{eval} for further details.  
#' 
#' @param kind Character or \code{NULL}.  If \code{kind} is a
#'     character string, set R's RNG to the kind desired.
#' 
#' @param normal.kind Character string or \code{NULL}.  If it is a
#'     character string, set the method of Normal generation.
#' 
#' @param vstr A character string containing a version number of
#'     R. The default value \code{NULL} will use the present version
#'     number.  Note that the function \code{RNGversion} works as a
#'     wrapper that calls \code{RNGkind} based on the version number,
#'     and that it can be conflicts between the settings specified by
#'     \code{vstr} and the setting specified by \code{kind} and
#'     \code{normal.kind}.  The settings indicated by \code{vstr} will
#'     be used if \code{kind} and \code{normal.kind} both are
#'     \code{NULL}, but if one or both of them are given, then
#'     settings will be based upon them.
#'
#' @param create_kind_vstr_list Logical value, default \code{FALSE}.
#'     This can be used to create a list with the values for
#'     \code{kind}, \code{normal.kind} and \code{vstr}.  If this is
#'     used when the before mentioned arguments are \code{NULL}, then
#'     the present values from the active R-session will be stored.
#'     This can be used by functions that need to store detailed
#'     information in an updated call, to ensure reproducibility later
#'     on.  This argument will be ignored if \code{create_code} is
#'     \code{TRUE}.
#' 
#' @param create_code Logical value, default \code{FALSE}.  This can
#'     be used to create a chunk of code to be inserted into e.g. a
#'     script.  The idea is that this should be done when the script
#'     is created, in order to ensure that the present settings of the
#'     system is properly recorded for reproducible results if the
#'     script is to be used later on by other machines, versions of R.
#' 
#' @return The result depends on the value given to
#'     \code{create_code}. It will either be a setting of the seed, or
#'     it will be the code needed to set the seed.  NOTE: The function
#'     will ensure that the settings for \code{kind} and
#'     \code{normal.kind} will be reverted to the original ones.  The
#'     value of the \code{.Random.seed}-vector will also depend upon
#'     this. If \code{kind} and \code{normal.kind} was different from
#'     the existing values, the value of \code{.Random.seed} will be
#'     reverted back to what it was when the function was called.
#'     Otherwise, the \code{Random.seed}-vector will be similar to
#'     what it would have been if \code{set.seed} and the expression
#'     \code{expr} had been evaluated in the workspace.
#'
#' @export
#'
#' @examples
#' ##  Gives the same effect as 'set.seed'.
#' set.seed(seed = 1)
#' rnorm(3)
#' .a <- .Random.seed
#' set_seed(seed = 1)
#' rnorm(3)
#' .b <- .Random.seed
#' identical(.a, .b)
#' ## The 'expr'-argument is evaluated in the global environment,
#' ## unless otherwise specified by the 'envir'-argument.
#' set_seed(seed = 1,
#'          expr = {.tmp <- rnorm(3)})
#' .tmp
#' .c <- .Random.seed
#' identical(.a, .c)
#'
#' ## No footprint on workspace when a change of RNG has been used.
#' set.seed(NULL)
#' .a <- .Random.seed
#' set_seed(seed =1,
#'          expr = {.tmp <- rnorm(3)},
#'          vstr = "1.0.0")
#' .tmp
#' .b <- .Random.seed
#' identical(.a, .b)
#'
#' ##  No footprint on workspace when a list is produced.
#' set.seed(NULL)
#' .a <- .Random.seed
#' set_seed(seed =1,
#'          expr = {.tmp <- rnorm(3)},
#'          vstr = "1.0.0",
#'          create_kind_vstr_list = TRUE)
#' .tmp
#' .b <- .Random.seed
#' identical(.a, .b)
#'
#' ## No footprint on workspace when code is produced.
#' set.seed(NULL)
#' .a <- .Random.seed
#' .seed <- 1
#' set_seed(seed = .seed,
#'          expr = {.tmp__ <- rnorm(3)},
#'          vstr = "1.0.0",
#'          create_code = TRUE)
#' .b <- .Random.seed
#' identical(.a, .b)
#' ## Note: The creation of the code does not evaluate
#' ## the 'expr'-argument.


set_seed <- function(seed,
                     expr = NULL,
                     envir = parent.frame(),
                     kind = NULL,
                     normal.kind = NULL,
                     vstr = NULL,
                     create_kind_vstr_list = FALSE,
                     create_code = FALSE) {
###-------------------------------------------------------------------
    ##  To minimise the footprint on the workspace, record the
    ##  existing '.Random.seed' (or create one if none exists yet).
    .old_.Random.seed <- .GlobalEnv$.Random.seed
    ##  The function to revert back at the end, to be used when there
    ##  has been a temporary change of RNG or when the result from the
    ##  function is a list or some code.
    .revert_.Random.seed <- function() {
        if (is.null(.old_.Random.seed)) {
            suppressWarnings(rm(.Random.seed,
                                envir = .GlobalEnv))
        } else
            .GlobalEnv$.Random.seed <- .old_.Random.seed
    }
###-------------------------------------------------------------------
    ##  Find present version of R and active kinds.
    .R_vstr <- paste(R.version$major,
                     R.version$minor,
                     sep = ".")
    .active_kinds <- structure(
        .Data = RNGkind(),
        .Names = c("kind", "normal.kind"))
###-------------------------------------------------------------------
    ##  Investigate and resolve conflicts between 'vstr', 'kind', and
    ##  'normal.kind' + ensure that the correct RNG-session is
    ##  started.
    if (! is.null(vstr)) {
        if (all(is.null(kind),
                is.null(normal.kind))) {
            ##  Change to the desired version.
            RNGversion(vstr = vstr)
            ##  Update 'kind' and 'normal.kind' from this.
            .adjusted_kinds <- structure(
                .Data = RNGkind(),
                .Names = c("kind", "normal.kind"))
            kind <- unname(.adjusted_kinds["kind"])
            normal.kind <- unname(.adjusted_kinds["normal.kind"])
        }
    } else 
        vstr <- .R_vstr
    ##  If 'adjusted_kinds' wasn't defined above, then it's time to do
    ##  it.  Reminder: Intitate RNG with values used for 'kind' and
    ##  'normal.kind', in order to allow for partial string-matching.
    if (! exists(x = "adjusted_kind", inherits = FALSE)) {
        RNGkind(kind = kind, normal.kind = normal.kind)
        .adjusted_kinds <- RNGkind()
        ##  Update 'kind' and 'normal.kind' from this.
        .adjusted_kinds <- structure(
            .Data = RNGkind(),
            .Names = c("kind", "normal.kind"))
        kind <- unname(.adjusted_kinds["kind"])
        normal.kind <- unname(.adjusted_kinds["normal.kind"])
    }
###-------------------------------------------------------------------
    ##  Investigate if there has been a change of RNG, and if it thus
    ##  is necessary to adjust back to the original RNG and
    ##  furthermore replace '.Random.seed' with '.old_.Random.seed'
    ##  (using '.revert_.Random.seed') -- so no footprint is left on
    ##  the workspace.
    if (! identical(x = .active_kinds,
                    y = .adjusted_kinds)) 
        on.exit(expr = {
            RNGkind(kind = .active_kinds["kind"],
                    normal.kind = .active_kinds["normal.kind"])
            .revert_.Random.seed()
        })
###-------------------------------------------------------------------
    ##  Return the code if 'create_code' is 'TRUE'.
    if (create_code) {
        .this_function <- this_function()
        .the_seed <- deparse(substitute(seed))
        .the_expr <- paste(deparse(substitute(expr)),
                           collapse = "\n\t")
        cat(paste("\n",
                  .this_function,
                  "(\n\t seed = ",
                  .the_seed,
                  ",\n\t kind = \"",
                  kind,
                  "\",\n\t normal.kind = \"",
                  normal.kind,
                  "\",\n\t vstr = \"",
                  vstr,
                  "\",\n\t expr = ",
                  .the_expr,
                  ")\n\n",
                  sep = ""))
        ##  Remove footprints from workspace.
        .revert_.Random.seed()
        return(invisible(NULL))
    }
###-------------------------------------------------------------------
##  Return the specifications of 'kind', 'normal.kind' and 'vstr' if
##  'create_kind_vstr_list' is 'TRUE'.
    if (create_kind_vstr_list) {
        .kind_vstr_list <- list(
            kind = kind,
            normal.kind = normal.kind,
            vstr = vstr)
        ##  Remove footprints from workspace.
        .revert_.Random.seed()
        return(.kind_vstr_list)
    }
###-------------------------------------------------------------------
    ##  Still running?  Then it's time to set a seed and perform the
    ##  calculation specified in 'expr'.
    eval(expr = {
        set.seed(seed = seed)
        expr
    },
    envir = envir)
    ##  The 'on.exit' defined above will be triggered if a change of
    ##  'RNG' has been performed.  If no change of 'RNG' happened,
    ##  then we do want the 'Random.seed' to be as if all of this had
    ##  been performed in the workspace.
    return(invisible(NULL))
}
