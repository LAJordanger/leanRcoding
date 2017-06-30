################################################################################
#'
#' A function that returns information about the loaded namespaces.
#'
#' This function inspects the loaded namespaces, and returns
#' information in accordance with the user-defined specifications.
#' 
#' @param .ns An optional argument that can be used if it's of
#'     interest to inspect a particular namespace.  The default is
#'     that all the loaded namespaces are investigated.
#'
#' @param .details A logical value that specifies the amount of
#'     information to return, default value \code{TRUE}.
#'
#' @param .pattern An optional regular expression. Only names matching
#'     \code{.pattern} are returned.  \code{glob2rx} can be used to
#'     convert wildcard patterns to regular expressions.
#'
#' @param .type One of the values \code{c("all", "exported",
#'     "internal")}, the default being \code{"all"}, which can be used
#'     to restrict the returned details with regard to show only
#'     exported or internal objects.
#'
#' @return If \code{.details} is \code{FALSE}, the result will simply
#'     be the names of the loaded namespaces (in essence the same
#'     result as obtained when using \code{sort} upon the result from
#'     \code{loadedNamespaces}).  If the \code{.ns}-argument has been
#'     supplied, then only the namespace specified there will be
#'     treated.  Different attributes will be added to the result when
#'     \code{.details} is given as \code{TRUE}.  One of the attributes
#'     will be \code{.pattern} (which mainly is included in order for
#'     the print-method to figure out what to write), and then there
#'     will in addition be one attribute corresponding to each
#'     namespace (having the same names), and each of these attributes
#'     will be a list containing the following components.
#'
#' \describe{
#'
#' \item{content}{A vector, that describes the content of the
#'     namespace; it's the result of \code{ls} used upon the
#'     namespace, with restrictions based on \code{.pattern}.  No
#'     restrictions will be used if \code{.pattern} is left
#'     unspecified.}
#'
#' \item{exported}{A logical vector that for each part of
#'     \code{content} tells if the object was exported or not.}
#'
#' \item{is}{A list that gives the result of \code{is} when it is used
#'     upon the different components.}
#'
#' }
#'
#' @export


the_Namespaces <- function(.ns,
                           .details = TRUE,
                          .pattern,
                          .type = c("all", "exported", "internal")) {
    if (missing(.pattern))
        .pattern <- ""
    ##  Sanity-check `.type`
    if (! any(.type %in% c("all", "exported", "internal")))
        error(.argument = ".type",
              c("Valid arguments are \"all\", \"exported\"",
                "and \"internal\""))
    ##  Restrict `.type` to one alternative.
    .type <- .type[1]
    ##  Find the loaded namespaces.
    .the_Namespaces <- sort(loadedNamespaces())
    ##  If `.ns` has been specified, check if the restriction can be
    ##  performed, or return an error.
    if (! missing(.ns)) {
        if (! .ns %in% .the_Namespaces) {
            error(.argument = ".ns",
                  "This must specify a loaded Namespace.")
        } else
            .the_Namespaces <- .ns
    }
    ##  Add attributes if `.details` is `TRUE`
    if (.details) {
        .ls_call <- call(
            name = "ls",
            pattern = .pattern)
        details <- structure(
            .Data = lapply(
                X = .the_Namespaces,
                FUN = function(x) {
                    .env_x <- asNamespace(x)
                    content <- eval(expr = .ls_call,
                                    envir = .env_x)
                    ##  Investigate if it the content belongs to the
                    ##  exported part of the packages.
                    exported <- structure(
                        .Data = content %in% getNamespaceExports(x),
                        .Names = content)
                    ##  Adjust `content` and `exported` if `.type` is
                    ##  different from `"all"`.
                    if (.type == "exported") {
                        content <-  content[exported == TRUE]
                        exported <- exported[exported == TRUE]
                    }
                    if (.type == "internal") {
                        content <-  content[exported == FALSE]
                        exported <- exported[exported == FALSE]
                    }
                    ##  Figure out what the content is.
                    is <- structure(
                        .Data = lapply(
                            X = content,
                            FUN = function(y) {
                                .is_x_call <- call(name = "is", as.name(y))
                                eval(expr = .is_x_call,
                                     envir = .env_x)
                            }),
                        .Names = content)
                    ##  Collect the pieces.
                    list(content = content,
                         exported = exported,
                         is = is)
                }),
            .Names = .the_Namespaces)
        ##  Add the details as attribute, this will give a lot of
        ##  attributes, one for each namespace detected, but it will
        ##  simplify the extraction of information later on.
        attributes(.the_Namespaces) <- details
        ##  Add `.pattern` as an attribute, in order for the print
        ##  method to inform the user of the content.
        attributes(.the_Namespaces)$.pattern <- .pattern
        ##  Add the attribute used by the print-method.
        class(.the_Namespaces) <- c(
            "the_Namespaces",
            class(.the_Namespaces))
    }
    ##  Return the result to the workflow.
    .the_Namespaces
}
