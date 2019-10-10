#' Register the dates file
#'
#' @param name use one of the default dates files for example "VU"
#' @param dates use a dates table
#' @export
register_dates <- function(name = NULL, dates = NULL) {
    ## Test: only one parameter can be used, name or dates
    if(!is.null(name) & !is.null(dates)) {
        stop("use only one parameter: name or dates")
    }
    ## If name is used: assign the option
    if (!is.null(name)) {
        ## Check if name exists
        if (name %in% names(dates_list)) {
            options("eduperiod.table" = dates_list[[name]])
        } else {
            stop(paste(name, "not found"))
        }

    } else if (!is.null(dates)) {
        ## Assign the given dates file
        options("eduperiod.table" = dates)
    } else {
        stop("Please give a name or dates object")
    }


}

get_dates <- function() {
    if (is.null(getOption("eduperiod.table"))) {
        warning("No dates table is set: 'VU' is set as default")
        register_dates(name = "VU")
        getOption("eduperiod.table")

    } else {
        getOption("eduperiod.table")
    }

}
