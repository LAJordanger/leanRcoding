################################################################################
#####  2014-10-08

#' Help function: File created
#'
#' In order to see progress, I fancy seeing messages about files that
#' has been created.  This is not intended to be exported, but while
#' developing this stuff it is nice to have it available.
#'
#' @param file_name A character string giving the name of the file
#'     that was created.
#'
#' @export

file_created <- function(file_name) {
    cat(date(),
        "\n\t",
        "File created:",
        "\n",
        file_name,
        "\n")
}


#####   TASK: I think this could/should be extended to a function that
#####   does the saving and then takes care of this information.  But
#####   then I suppose the name should be changed.
