################################################################################
#####  2014-10-05

#' Arrays are awesome, but need dimnames.
#'
#' This function attempts to return an array with a full set of
#' dimension-names.
#'
#' @param arr_data The stuff we want to work upon.
#'
#' @param def_dn A character-string default value \code{"or"}, if some
#'     dimension-names are missing, this argument will be used as a
#'     prefix when generating them.
#'
#' @param def_dnn A character-string, default value \code{"orig"}, if
#'     the _names_ of the dimension-names are missing, this argument
#'     will be used as a prefix when generating them.
#'
#' @return An array with a full set of dimension-names.
#'
#' @export

aa_converter <- function(arr_data, def_dn = "or", def_dnn = "orig") {
###-------------------------------------------------------------------
    ##  If necessary, convert to an array.
    if (!is.array(arr_data)) 
        if (is.vector(arr_data) | is.data.frame(arr_data)) {
            arr_data <- as.matrix(arr_data,
                                  rownames.force= TRUE)
        } else {
            stop("\t",
                 "Sorry, 'aa_converter' is to stupid to handle ",
                 "objects of type ",
                 typeof(arr_data),
                 call. = FALSE)
        }
###-------------------------------------------------------------------
###-------------------------------------------------------------------
    ##  If necessary, adjust the dimension-names and/or the _names_ of
    ##  the dimension-names.
###-------------------------------------------------------------------
###-------------------------------------------------------------------
    ##  Collect the original values
    d <- dim(arr_data)
    dn <- dimnames(arr_data)
    dnn <- names(dn)
###-------------------------------------------------------------------
    ##  Investigate which part(s) of the dimension-names that might
    ##  need to be modified.
    if (is.null(dn)) {
        update_dn_needed <-
            1:length(d)
    } else {
        update_dn_needed <-
            (1:length(d))[unlist(lapply(X = dn,
                                      FUN = is.null))]
    }
###-------------------------------------------------------------------
    ##   Investigate which part(s) of the _names_ of the
    ##   dimension-names that might need to be modified.
    if (is.null(dnn)) {
        update_dnn_needed <-
            1:length(d)
    } else {
        update_dnn_needed <-
            (1:length(d))[unlist(lapply(X = dnn,
                                      FUN = is.null))]
    }
###-------------------------------------------------------------------
    ##  Modifcation of 'dn' (the dimension-names).
    if (length(update_dn_needed) != 0)
        for (dn.ind in update_dn_needed)
            dn[[dn.ind]] <- paste(def_dn,
                                  "_",
                                  dn.ind,
                                  ".",
                                  1:d[dn.ind],
                                  sep = "")
###-------------------------------------------------------------------
    ##  Modifcation of 'dnn' (the _names_ of the dimension-names).
    if (length(update_dnn_needed) != 0)
        for (dn.ind in update_dnn_needed)
            dnn[[dn.ind]] <- paste(def_dnn,
                                  "_",
                                  dn.ind,
                                  sep = "")
###-------------------------------------------------------------------
    ##  Update the names of the dimension-names, and use them to
    ##  update or array.
    names(dn) <- dnn
    dimnames(arr_data) <- dn
###-------------------------------------------------------------------
    ##  Return the answer.
    return(arr_data)
}



#####  WARNING: This might not work in the desired way if vectors with
#####  "stupid" naming-conventions are encountered...


## test.vec <- c(a = 2, a =3)

## test.vec <- c(a = 2, 3)

## aa_converter(arr_data = test.vec)

## ##       orig_2
## ## orig_1 or_2.1
## ##      a      2
## ##             3

## ##  Crap, should I care about this.

#####  TASK: I think I will simply ignore this for the time being.

##  An alternative might be to overwrite everything with the modified
##  names, and rather include the original parts at the end...  That
##  might be a better solution after all.
