#' Process dates csv
#'
#' Process a csv file to the right format. This file is used to know the start
#' and ending dates of the educational periods. The format of the file is.
#'
#' @param file the path of the csv-file
#' @param delim the delimiter default: ","
#' @param format the format of the date field default: "\%d-\%m-\%Y"
#' @param register default: TRUE register the dates csv in the package options
#' @param ... extra parameters passed to readr read_delim
#' @export
process_dates_csv <- function(file, delim = ",", format = "%d-%m-%Y", register = TRUE, ...) {
    ## Load the data using readr
    dates <- readr::read_delim(file,
                               delim = delim,
                               col_types = readr::cols(
                                    year = readr::col_double(),
                                   .default = readr::col_date(format = format)),
                               ...)

    ## process the data
    dates <- dates %>%
        ## Add an extra column to be able to create end_date_prev
        dplyr::mutate(Extra_kolom = lubridate::dmy(NA_integer_)) %>%
        ## reshape wide to long format
        tidyr::gather(period,
                      end_date,
                      -year) %>%
        ## extract the numbers from the period
        dplyr::mutate(period = as.integer(stringr::str_extract(period, "[0-9]"))) %>%
        ## Arrange the periods
        dplyr::arrange(year, period) %>%
        ## create the end date of the previous row as an extra value
        dplyr::mutate(end_date_prev = dplyr::lag(end_date)) %>%
        as.data.frame()

    if (register) {
        register_dates(dates = dates)
    }
    dates
}
