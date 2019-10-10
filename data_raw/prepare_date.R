
## List all dates files in the data-raw folder
file_list <- list.files("data_raw", pattern = "^dates", full.names = TRUE)
dates_list <- purrr::map(file_list,
           process_dates_csv,
           delim = ";",
           format = "%d-%m-%Y") %>%
    setNames(stringr::str_extract(basename(file_list), "(?<=dates_).*(?=\\.csv)"))

usethis::use_data(dates_list, internal = TRUE, overwrite = TRUE)
