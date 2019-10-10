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
        ## Voeg een extra kolom toe omdat we deze later nodig hebben
        ## om Einddatum_prev en Peildatum_prev te berekenen
        dplyr::mutate(Extra_kolom = lubridate::dmy(NA_integer_)) %>%
        ## Klap de tabel om van wide naar long format
        tidyr::gather(key = period,
                      value = end_date,
                      -year) %>%
        ## Pas variabelen aan
        dplyr::mutate(period = as.numeric(stringr::str_extract(period, "[0-9]"))) %>%
        ## Zet de variabelen op volgorde van year en period
        ## zodat we daarna een berekening kunnen maken obv de plek van de
        ## variabele in de data
        dplyr::arrange(year, period) %>%
        ## Bereken de Einddatum_prev en Peildatum_prev op basis van de
        ## einddatum en peildatum van de voorgaande periode. Elke periode
        ## heeft een begin en einddatum
        dplyr::mutate(end_date_prev = lag(end_date)) %>%
        as.data.frame()
}



get_dates <- function() {
    if (is.null(getOption("eduperiod.table"))) {
        warning("VU is being used as dates table")
        register_dates(name = "VU")
        getOption("eduperiod.table")

    } else {
        getOption("eduperiod.table")
    }

}


register_dates <- function(name, dates) {
    if (!is.null(name)) {
        options("eduperiod.table" = dates_list[[name]])
    } else if (!is.null(dates)) {
        options("eduperiod.table" = dates)
    }


}



