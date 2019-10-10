
## List all dates files in the data-raw folder
file_list <- list.files("data_raw", pattern = "^dates", full.names = TRUE)
## Load all files and process them
dates_list <- purrr::map(file_list,
           process_dates_csv,
           delim = ",",
           format = "%d-%m-%Y") %>%
    setNames(stringr::str_extract(basename(file_list), "(?<=dates_).*(?=\\.csv)"))

## Save the files in a list
usethis::use_data(dates_list, internal = TRUE, overwrite = TRUE)
