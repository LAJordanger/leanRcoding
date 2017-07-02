#' Find the numerical indices in an array.
#'
#' This function will identify the numerical indices that corresponds
#' to a selection based on a character vector, by comparing it to a
#' list representing the dimension-names to be used.
#'
#' @param .bookmark The caracter vector to be used for the selection.
#'     (If a list is detected, it will be converted to a vector
#'     first.)  This vector must have the same length as the list in
#'     the \code{.dimnames}-argument.  If \code{.bookmark} is a named
#'     vector, then the names must be the same as those encountered in
#'     \code{.dimnames} (but not necessarily in the same order).
#'
#' @param .dimnames The list of dimension names.  It will be assumed
#'     that this is a named list of character vectors, and it will
#'     also be assumed that the names (and the content of the
#'     individual character vectors) are unique.  A test of the
#'     validity of \code{.dimnames} will be performed when
#'     \code{.check_dimnames} is given as \code{TRUE} (default).
#'
#' @param .check_dimnames A logical value, default \code{TRUE}, that
#'     can be used to check that \code{.dimnames} satisfies the
#'     required uniqueness requirements.
#'
#' @return The result will be a vector of integers, which can be used
#'     as an alternative to \code{.bookmark} for the subsetting.
#'
#' @export

array_index <- function(.bookmark,
                        .dimnames,
                        .check_dimnames = TRUE) {
    if (.check_dimnames) {
        ##  Check that the names and dimension-names are valid.
        .dnn <- names(.dimnames)
        .dimnames_OK <- local({
            .unique <- function(vec) {
                ..unique <- identical(length(vec), length(unique(vec)))
                ..proper <- ! "" %in% vec
                    all(..unique, ..proper)
            }
            .unique_dnn <- .unique(.dnn)
            .unique_dn <- all(vapply(
                X = .dimnames,
                FUN = .unique,
                FUN.VALUE = logical(1)))
            all(! is.null(.dimnames),
                ! is.null(.dnn),
                .unique_dnn,
                .unique_dn)
        })
        if (! .dimnames_OK)
            error(.argument = ".dimnames",
                  c("This must be a named list of character vectors, where",
                    "uniqueness of content is required at all levels."))
        kill (.dimnames_OK, .dnn)
    }
    kill(.check_dimnames)
###-------------------------------------------------------------------
    ##  If necessary, convert ''bookmark' from a list to a vector.
    if (is.list(.bookmark))
        .bookmark <- unlist(.bookmark)
    ##  Check that '.bookmark' has the correct content and length.  If
    ##  it is a named vector, ensure that it matches the names of
    ##  '.dimnames' and that the order is the same.
    if (mode(.bookmark) != "character")
        error(.argument = ".bookmark",
              c("The mode of",
              sQuote(".bookmark"),
              "must be character."))
    if (length(.bookmark) != length(.dimnames))
        error(.argument = c(".bookmark", ".dimnames"),
              "These arguments must have the same length.")
    if (! is.null(names(.bookmark))) {
        if (length(unique(names(.bookmark))) != length(.bookmark))
            error(.argument = ".bookmark",
                  "Non-unique names detected.")
        if (! all(names(.bookmark) %in% names(.dimnames)))
        error(.argument = c(".bookmark", ".dimnames"),
              c("These arguments must have the same names, or",
                sQuote(".bookmark"),
                "must be without names."))
        .bookmark <- .bookmark[names(.dimnames)]
    } else
        ##  When no names detected for 'bookmark', assumes that the
        ##  same order is used as in '.dimnames'.
        names(.bookmark) <- names(.dimnames)
###-------------------------------------------------------------------
    ##  Identify the indices, or if missing ensure that an error can
    ##  be reported.
    .indices <- vapply(
        X = seq_along(.bookmark),
        FUN = function(x) {
            .i <- which(.dimnames[[x]] %in% .bookmark[x])
            ##  Code to detect problems.
            if (length(.i) == 0) {
                NA_integer_
            } else
                .i
        },
        FUN.VALUE = integer(1))
###-------------------------------------------------------------------
    ##  Return the indices if no problems, else return an error.
    if (any(is.na(.indices))) {
        error(.argument = c(".bookmark", ".dimnames"),
              c("The values specified in",
                sQuote(".bookmark"),
                "and the content of",
                sQuote(".dimnames"),
                "does not match."))
    } else
        .indices
}
