##
process_dates_csv <- function(file,
                              delim = ",",
                              format = "%d-%m-%Y",
                              ...) {
    ## Load the data
    dates <- readr::read_delim(file,
                               delim = delim,
                               col_types = readr::cols(
                                   year = readr::col_double(),
                                   .default = readr::col_date(format = format)),
                               ...)

    ## process the data
    dates %>%
        ## Add an extra column to be able to create end_date_prev
        dplyr::mutate(Extra_kolom = lubridate::dmy(NA_integer_)) %>%
        ## reshape wide to long format
        tidyr::gather(key = period,
                      value = end_date,
                      -year) %>%
        ## extract the numbers from the period
        dplyr::mutate(period = as.integer(stringr::str_extract(period, "[0-9]"))) %>%
        ## Arrange the periods
        dplyr::arrange(year, period) %>%
        ## create the end date of the previous row as an extra value
        dplyr::mutate(end_date_prev = dplyr::lag(end_date)) %>%
        as.data.frame()
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



