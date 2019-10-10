##
process_dates_csv <- function(file) {
    dates <- readr::read_delim(file,
                               delim = ";",
                               col_types = readr::cols(
                                   year = readr::col_double(),
                                   .default = readr::col_date(format = "%d-%m-%Y")))

    dates_name <-

    dates %>%
        ## Voeg een extra kolom toe omdat we deze later nodig hebben
        ## om Einddatum_prev en Peildatum_prev te berekenen
        dplyr::mutate(Extra_kolom = dmy(NA_integer_)) %>%
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
        dplyr::mutate(end_date_prev = lag(end_date),
                      name = dates_name)
}


file_list <- list.files("data_raw", pattern = "^dates", full.names = TRUE)
dates_lists <- purrr::map(file_list,
           process_dates_csv) %>%
    setNames(stringr::str_extract(basename(file_list), "(?<=dates_).*(?=\\.csv)"))

dates_lists["VU"]

library(usethis)
use_data(dates_long, internal = TRUE, overwrite = TRUE)
