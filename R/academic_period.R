#' Educational period
#'
#' This function translates the academic period to
#'
#' @param x Een datum, of vector met meerdere data. POSIXct wordt ook geaccepteerd.
#' @param type Standaard: "period": De academische periode wordt geretourneerd.
#' bij "year" wordt het academisch jaar teruggegeven.
#' @return Het academisch jaar of periode waarin de opgegeven datum valt.
#' @export
edu_period_year <- function(x, type = "period"){

    ## Converteer POSIXct en POSIXt naar de klasse datum
    if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
        x <- lubridate::as_date(x)
    }

    ## als de klasse geen datum is, dan kan deze functie niet uitgevoerd worden
    stopifnot(class(x) == "Date")

    ## Voeg de tijdelijke variabele TMP = T toe, zodat een cartetisch product
    ## gemaakt kan worden
    dates_long <- dates_long %>%
        dplyr::mutate(TMP = T)

    ## Maak vanwege de performance eerst een tibble van alle unieke datums in de vector
    unique_df <- dplyr::tibble(x = unique(x)) %>%
        ## Voeg de tijdelijke variabele TMP = T toe, zodat een cartetisch product
        ## gemaakt kan worden
        dplyr::mutate(TMP = T) %>%
        ## Maak een cartetisch product door Dates te joinen op TMP
        dplyr::left_join(dates_long, by = "TMP") %>%
        ## Filter de rijen waarbij x overeen komt met de peildata
        dplyr::filter(x  >= end_date_prev,
                      x  <  end_date) %>%
        dplyr::select(x, year, period)

    ## Join de uitgeregekende periode en inschrijvingsjaar aan een tibble met vector
    ## x
    x_df <- dplyr::tibble(x = x) %>%
        dplyr::left_join(unique_df, by = "x")

    ## Retourneer de waarde, afhankelijk van de parameter type die is meegegeven
    if (type == "period") {
        return(x_df$period)
    }
    if (type == "year") {
        return(x_df$year)
    }
}

edu_period <- function(x) {
    edu_period_year(x, type = "period")
}

edu_year <- function(x) {
    edu_period_year(x, type = "year")
}
