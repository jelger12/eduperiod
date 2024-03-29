edu_period_year <- function(x, type, name, dates = NULL){

    ## Convert POSIXct and POSIXt to Dates class
    if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
        x <- lubridate::as_date(x)
    }

    ## Stop if the class is not Date
    stopifnot(class(x) == "Date")

    ## If no dates table is given, use the dates table from the options
    if (is.null(dates)) {
        ## get the dates table
        dates <- get_dates()
    }

    ## Create a table with all unique dates
    unique_df <- dplyr::tibble(x = unique(x)) %>%
        tidyr::crossing(dates) %>%
        ## filter the rows that are within the range
        dplyr::filter(x >= .data$end_date_prev,
                      x <  .data$end_date) %>%
        dplyr::select(x, .data$year, .data$period)

    ## Join the caculated period and year to a tibble with the x vector
    x_df <- dplyr::tibble(x = x) %>%
        dplyr::left_join(unique_df, by = "x")

    ## Retourneer de waarde, afhankelijk van de parameter type die is meegegeven
    if (type == "period") {
        return(as.integer(x_df$period))
    }
    if (type == "year") {
        return(as.integer(x_df$year))
    }
}

#' Educational period
#'
#' Translate date to educational period
#'
#' @param x Date or POSIXct vector or value
#' @param dates optional: use a custom dates table
#'
#' @return the translated period as an integer vector or value
#' @export
edu_period <- function(x, dates = NULL) {
    edu_period_year(x, type = "period", dates = dates)
}

#' Educational year
#'
#' Translate date to educational year
#'
#' @param x Date or POSIXct vector or value
#' @param dates optional: use a custom dates table
#'
#' @return the translated year as an integer vector or value
#' @export
edu_year <- function(x, dates = NULL) {
    edu_period_year(x, type = "year", dates = dates)
}



