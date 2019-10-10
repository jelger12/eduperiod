#' Educational period
#'
#' This function translates the date vector to a vector of academic periods or
#' years
edu_period_year <- function(x, type, name){

    ## Converteer POSIXct en POSIXt naar de klasse datum
    if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
        x <- lubridate::as_date(x)
    }

    ## als de klasse geen datum is, dan kan deze functie niet uitgevoerd worden
    stopifnot(class(x) == "Date")

    ## Voeg de tijdelijke variabele TMP = T toe, zodat een cartetisch product
    ## gemaakt kan worden
    dates_long <- get_dates() %>%
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
#'
#' @return the translated period as an integer vector or value
#' @export
#'
#' @examples
#'
edu_period <- function(x) {
    edu_period_year(x, type = "period")
}

#' Educational year
#'
#' Translate date to educational year
#'
#' @param x Date or POSIXct vector or value
#'
#' @return the translated year as an integer vector or value
#' @export
#'
#' @examples
#'
edu_year <- function(x) {
    edu_period_year(x, type = "year")
}



