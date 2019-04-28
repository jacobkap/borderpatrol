library(rvest)
library(pdftools)
library(stringr)
library(dplyr)
library(readr)
library(zoo)
library(lubridate)
library(tidyr)



wide_to_long_sectors <- function(data) {
  data <- gsub("Grande San", "Grande  San", data)
  data <- stringr::str_split_fixed(data, "\\s{2,}", 11)
  data <- data.frame(data, stringsAsFactors = FALSE)
  data <- fix_names(data)

  data <-
    data %>%
    dplyr::rename(fiscal_year = fiscal,
                  rio_grande_valley = rio_grande,
                  southwest_border_total = southwest_border) %>%
    dplyr::mutate_if(is.character, readr::parse_number)

  data <- long_to_wide(data)

  return(data)
}

make_numeric <- function(col) {
  col <- gsub(",", "", col)
  col <- suppressWarnings(as.numeric(col))
  return(col)
}

long_to_wide <- function(data, id_var = "fiscal_year", var_name = "sector") {
  data <- data.table::melt(data.table::setDT(data),
                           id.vars = id_var,
                           variable.name = var_name)
  data <-
    data %>%
    dplyr::arrange(desc(fiscal_year),
                   sector)
  data <- as.data.frame(data)
}

fix_names <- function(data) {
  names(data) <- data[1, ]
  data <- data[-1,]
  names(data) <- gsub(" ", "_", names(data))
  names(data) <- tolower(names(data))
  return(data)
}

read_pdf <- function(file_name) {
  data <- suppressMessages(pdftools::pdf_text(file_name))
  data <- unlist(strsplit(data, split = "\n"))
  data <- trimws(data)

  return(data)
}




read_mexico_table <- function(data, value_name) {
  data <- data[grep("Sectors$|\\)$|^\\*|United States", data, invert = TRUE)]
  data <- gsub("([0-9]) ([0-9])", "\\1  \\2", data)
  data <- stringr::str_split_fixed(data, "\\s{2,}", 20)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- fix_names(data)
  data <- data[!data$sector %in% c("Rio Grande Valley", "Big Bend"), ]
  data$sector <- gsub(".formerly Marfa.", "Big Bend", data$sector)
  data$sector <- gsub(".formerly McAllen.", "Rio Grande Valley", data$sector)
  data$sector <- gsub(" ", "_", data$sector)
  data <- data %>%
    dplyr::mutate(sector = tolower(sector)) %>%
    dplyr::mutate_at(vars(starts_with("fy")), make_numeric)
  data <- long_to_wide(data, "sector", "fiscal_year")
  names(data) <- gsub("value", value_name, names(data))
  data$fiscal_year <- as.character(data$fiscal_year)
  data$fiscal_year <- readr::parse_number(data$fiscal_year)
  return(data)
}

sector_by_month_scrape <- function(data, value_name) {
  data <- gsub(".*By Month - FY ([0-9]{4})", "FY\\1", data)
  data <- gsub("^(FY[0-9]{4})", "x  x  x  x  x  x  x  x  x  x  x  x  x  x   \\1", data)
  data <- data[grep("^Big Bend|^Rio Grande Valley|United States Border Patrol|^SECTOR|\\*",
                    data, invert = TRUE)]
  data <- gsub("\\(formerly McAllen\\)", "Rio Grande Valley", data)
  data <- gsub("\\(formerly Marfa\\)", "Big Bend", data)
  data <- stringr::str_split_fixed(data, "\\s{2,}", 15)
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- c("sector", fiscal_year_months, "yearly_totals", "fiscal_year")
  data$fiscal_year <- gsub("FY", "", data$fiscal_year)
  data <-
    data %>%
    dplyr::mutate(sector = tolower(sector)) %>%
    dplyr::mutate_at(2:15, make_numeric)
  data$fiscal_year <- zoo::na.locf(data$fiscal_year)
  data <- data[data$sector != "x", ]

  data <- long_to_wide(data, c("sector", "fiscal_year"), "month")
  names(data) <- gsub("value", value_name, names(data))
  data$month <- as.character(data$month)
  #data$date <- lubridate::ymd(paste(data$fiscal_year, data$month, "1"))
  data <-
    data %>%
    dplyr::arrange(desc(fiscal_year),
                   sector)
  data <- as.data.frame(data)
  return(data)
}


fiscal_year_months <- c("october",
                        "november",
                        "december",
                        "january",
                        "february",
                        "march",
                        "april",
                        "may",
                        "june",
                        "july",
                        "august",
                        "september")




# Sector profile
get_sector_profile_table <- function(file,
                                     border_patrol_strings,
                                     year) {
  data <- file[border_patrol_strings[1]:(border_patrol_strings[2]-1)]
  data <- data[grep("Miami", data):length(data)]
  data <- data[grep("^\\*", data, invert = TRUE)]
  data <- gsub("\\(.*\\) ", " ", data)
  data <- gsub("\\*+", "", data)
  data <- stringr::str_split_fixed(data, "\\s{2,}", 10)
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- c("sector",
                   "number_of_agents",
                   "total_apprehensions",
                   "other_than_mexican_apprehensions",
                   "marijuana_pounds",
                   "cocaine_pounds",
                   "accepted_prosecutions",
                   "assaults",
                   "rescues",
                   "deaths")
  data <-
    data %>%
    dplyr::mutate(sector = tolower(sector),
                  fiscal_year = year) %>%
    dplyr::mutate_at(2:11, make_numeric)
  return(data)
}





# Juvenile and adult apprehensions
get_juv_and_adult_apprehensions_table <- function(file,
                                                  border_patrol_strings,
                                                  year) {
  data <- file[border_patrol_strings[2]:(border_patrol_strings[3]-1)]
  data <- data[grep("Miami", data):length(data)]
  data <- gsub("\\(.*\\) ", " ", data)
  data <- stringr::str_split_fixed(data, "\\s{2,}", 6)
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- c("sector",
                   "accompanied_juveniles",
                   "unaccompanied_juveniles",
                   "total_juveniles",
                   "total_adults",
                   "total_apprehensions")
  data <-
    data %>%
    dplyr::mutate(sector = tolower(sector),
                  fiscal_year = year) %>%
    dplyr::mutate_at(2:7, make_numeric)
  return(data)
}

# Apprehensions by gender
get_apprehensions_by_gender_table <- function(file,
                                              border_patrol_strings,
                                              year) {
  data <- file[border_patrol_strings[3]:(border_patrol_strings[4]-1)]
  data <- data[grep("SECTOR", data):length(data)]
  data <- gsub("\\(.*\\) ", " ", data)
  data <- stringr::str_split_fixed(data, "\\s{2,}",
                                   stringr::str_count(data[1], "\\s{2,}") + 1)
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- data[1, ]
  data <- data[-1, ]
  names(data) <- gsub(" ", "_", names(data))

  data <-
    data %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(sector = tolower(sector),
                  fiscal_year = year) %>%
    dplyr::mutate_at(vars(-one_of("sector")), make_numeric)
  return(data)
}

# Apprehensions/Seizure statistics
get_seizures_stats_table <- function(file,
                                     border_patrol_strings,
                                     year) {
  data <- file[border_patrol_strings[4]:length(file)]
  data <- data[grep("Apprehension/Seizure", data)[1]:length(data)]
  data <- gsub(" \\((.*)) ", "_\\1", data)
  data <- gsub("Border Sectors", "Border Sectors  ", data)
  data <- data[1:grep("Currency", data)]


  data <- stringr::str_split_fixed(data, "\\s{2,}", 5)
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- data[1, ]
  data <- data[-1, ]
  names(data) <- gsub(" |/", "_", names(data))

  data <-
    data %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(apprehension_seizure_type = tolower(apprehension_seizure_type)) %>%
    dplyr::mutate_at(2:5, parse_number)
  data$apprehension_seizure_type <- gsub("currency_value",
                                         "currency_in_dollars",
                                         data$apprehension_seizure_type)
  data$apprehension_seizure_type <- gsub(" ",
                                         "_",
                                         data$apprehension_seizure_type)
  data$apprehension_seizure_type <- gsub("\\*",
                                         "",
                                         data$apprehension_seizure_type)
  data$apprehension_seizure_type <- gsub("aliens_from_special_interest_countries",
                                         "aliens_special_interest_county",
                                         data$apprehension_seizure_type)

  coastal <- data[, 1:2]
  coastal$sector <- "coastal border"
  names(coastal)[2] <- "value"
  coastal <-
    coastal %>%
    tidyr::spread(apprehension_seizure_type, value)

  northern <- data[, c(1, 3)]
  northern$sector <- "northern border"
  names(northern)[2] <- "value"
  northern <-
    northern %>%
    tidyr::spread(apprehension_seizure_type, value)

  southwest <- data[, c(1, 4)]
  southwest$sector <- "southwest border"
  names(southwest)[2] <- "value"
  southwest <-
    southwest %>%
    tidyr::spread(apprehension_seizure_type, value)

  nationwide_total <- data[, c(1, 5)]
  nationwide_total$sector <- "nationwide total"
  names(nationwide_total)[2] <- "value"
  nationwide_total <-
    nationwide_total %>%
    tidyr::spread(apprehension_seizure_type, value)

  data <-
    coastal %>%
    dplyr::bind_rows(northern) %>%
    dplyr::bind_rows(southwest) %>%
    dplyr::bind_rows(nationwide_total) %>%
    dplyr::mutate(fiscal_year = year)
  return(data)
}
