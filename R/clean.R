# https://www.cbp.gov/newsroom/media-resources/stats
setwd(here::here("data/raw"))
source(here::here("R/utils.R"))

# Nationwide Illegal Alien Apprehensions Fiscal Years 1925-2018 ---------
clean_national_apprehensions <- function() {
  data <- suppressMessages(pdftools::pdf_text("bp_total_apps_fy1925_fy2018.pdf"))
  data <- unlist(strsplit(data, split = "\n"))
  data <- trimws(data)
  data <- data[4:length(data)]
  data <- stringr::str_split_fixed(data, "\\s{2,}", 4)

  data_temp <- data[, 3:4]
  data <- data[, 1:2]

  data <- data.frame(data, stringsAsFactors = FALSE)
  data_temp <- data.frame(data_temp, stringsAsFactors = FALSE)
  data <- bind_rows(data, data_temp)
  names(data) <- c("fiscal_year", "total_apprehensions")
  data <-
    data %>%
    mutate(fiscal_year = readr::parse_number(fiscal_year),
           total_apprehensions = readr::parse_number(total_apprehensions)) %>%
    arrange(desc(fiscal_year))
  data <- as.data.frame(data)

  return(data)
}



# Southwest Border Sections Apprehension Fiscal Years 1960-2018 -----------
southwest_border_apprehensions <- function() {
  data <- read_pdf("bp_southwest_border_sector_apps_fy1960_fy2018.pdf")
  data <- data[c(4, 8:length(data))]
  data <- wide_to_long_sectors(data)
  data <-
    data %>%
    select(sector,
           fiscal_year,
           everything())
  data <- data.frame(data)
  names(data) <- gsub("value", "total_apprehensions", names(data))
  data$sector <- as.character(data$sector)
  data$sector <- gsub("_", " ", data$sector)
  data$sector <- gsub("southwest border total", "southwest border", data$sector)
  return(data)
}



# Southwest Border Sections Deaths Fiscal Years 1998-2018 -----------------
southwest_border_deaths <- function() {
  data <- read_pdf("bp_southwest_border_sector_deaths_fy1998_fy2018.pdf")
  data <- data[c(4, 8:(length(data)-1))]
  data <- wide_to_long_sectors(data)
  data <-
    data %>%
    select(sector,
           fiscal_year,
           everything())
  data <- data.frame(data)
  names(data) <- gsub("value", "deaths", names(data))
  data$sector <- as.character(data$sector)
  data$sector <- gsub("_", " ", data$sector)
  data$sector <- gsub("southwest border total", "southwest border", data$sector)
  return(data)
}


# Border Patrol Agent Nationwide Staffing 1992-2018 -----------------------
clean_border_patrol_staffing <- function() {
  data <- read_pdf("staffing_fy1992_fy2018.pdf")
  # By Border sector
  sector_locations <- grep("Border Patrol Agent Staffing By Fiscal Year", data)
  data <- data[sector_locations[2]:(sector_locations[3]-1)]
  data <- data[2:29]
  data[28] <- gsub("FY 2016", "FY 2018", data[28])
  data <- stringr::str_split_fixed(data, "\\s{2,}", 5)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- fix_names(data)
  data <- data %>%
    mutate_if(is.character, readr::parse_number)
  data <- long_to_wide(data)
  names(data) <- gsub("value", "number_of_agents", names(data))
  sector_staffing <- data
  sector_staffing$sector <- as.character(sector_staffing$sector)

  # By Border subsector
  data <- read_pdf("staffing_fy1992_fy2018.pdf")
  sector_locations <- grep("Border Patrol Agent Staffing By Fiscal Year", data)
  data <- data[sector_locations[3]:length(data)]
  data <- data[grep("Sectors$|\\)$|^\\*", data, invert = TRUE)]
  data <- gsub("\\(formerly", "", data)
  data <- gsub("FY 1992", "Sector  FY 1992", data)
  data <- gsub("([0-9]) ([0-9])", "\\1  \\2", data)
  data <- gsub("FY ([0-9]{4}) ", "FY\\1  ", data)
  data <- data[-1]
  data <- stringr::str_split_fixed(data, "\\s{2,}", 28)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- fix_names(data)
  data$sector <- gsub("Valley", "Rio Grande Valley", data$sector)
  data$sector <- gsub("Livermore.*", "Livermore", data$sector)
  data <- data[data$sector != "Rio Grande", ]
  data$sector <- gsub(" ", "_", data$sector)
  data <- data %>%
    mutate(sector = tolower(sector)) %>%
    mutate_at(vars(starts_with("fy")), make_numeric)
  data <- long_to_wide(data, "sector", "fiscal_year")
  names(data) <- gsub("value", "number_of_agents", names(data))
  data$fiscal_year <- as.character(data$fiscal_year)
  data$fiscal_year <- readr::parse_number(data$fiscal_year)

  data <-
    sector_staffing %>%
    bind_rows(data) %>%
    select(sector,
           fiscal_year,
           everything()) %>%
    arrange(desc(fiscal_year),
            sector)
  data <- as.data.frame(data)
  data$sector <- gsub("_", " ", data$sector)
  data$sector <- gsub(" sectors", "", data$sector)
  return(data)

}

# Total Unaccompanied Alien Children (0-17 Years Old) by month 2010 - 2018 --------
clean_family_child_total_monthly <- function() {
  unaccompanied_child <- read_pdf("bp_total_monthly_uacs_sector_fy2010_fy2018.pdf")
  unaccompanied_child <- sector_by_month_scrape(unaccompanied_child,
                                                "unaccompanied_child_apprehension")

  # Total Illegal Apprehensions by Month 2000-2018 --------------------------
  total_monthly <- read_pdf("bp_total_monthly_apps_sector_area_fy2018.pdf")
  total_monthly <- sector_by_month_scrape(total_monthly,
                                          "total_apprehensions")


  # Total Family Unit Apprehensions by Month 2000-2018 --------------------------
  family <- read_pdf("bp_total_monthly_family_units_sector_fy13_fy18.pdf")
  family <- sector_by_month_scrape(family,
                                   "family_apprehensions")

  data <-
    total_monthly %>%
    left_join(unaccompanied_child) %>%
    left_join(family)
  data$sector <- gsub("_", " ", data$sector)
  data$sector <- gsub("monthly total", "nationwide total", data$sector)
  data$month <- gsub("yearly_totals", "yearly total", data$month)

  return(data)
}


# Applications Other than Mexico 2000-2018 --------------------------------
clean_other_than_mexico <- function() {
  file <- read_pdf("bp_total_apps_other_mexico_fy2000_fy2018.pdf")

  sector_locations <- grep("SECTOR", file)
  total <- file[sector_locations[1]:(sector_locations[2]-1)]
  total <- read_mexico_table(total, "total_apprehensions")

  mexico <- file[sector_locations[2]:(sector_locations[3]-1)]
  mexico <- read_mexico_table(mexico, "mexican_apprehensions")

  not_mexico <- file[sector_locations[3]:length(file)]
  not_mexico <- read_mexico_table(not_mexico, "other_than_mexican_apprehensions")

  data <-
    total %>%
    left_join(mexico) %>%
    left_join(not_mexico)
  data <- as.data.frame(data)
  data$sector <- gsub("grand_total", "nationwide total", data$sector)
  data$sector <- gsub("_", " ", data$sector)
  return(data)
}




# United States Border Patrol Sector Profile 2011-2018 --------------------
clean_sector_profile <- function() {
  sector_table <- data.frame(stringsAsFactors = FALSE)
  juv_and_adult_table <- data.frame(stringsAsFactors = FALSE)
  app_by_gender_table <- data.frame(stringsAsFactors = FALSE)
  seizure_table <- data.frame()
  files <- list.files(pattern = "profile|Profile")
  for (file_name in files) {
    file <- read_pdf(file_name)
    year <- gsub(".*([0-9]{4}).*", "\\1", file_name)
    border_patrol_strings <- grep("United States Border Patrol", file)

    sector_table_temp <- get_sector_profile_table(file,
                                                  border_patrol_strings,
                                                  year)
    juv_and_adult_table_temp <- get_juv_and_adult_apprehensions_table(file,
                                                                      border_patrol_strings,
                                                                      year)
    app_by_gender_table_temp <- get_apprehensions_by_gender_table(file,
                                                                  border_patrol_strings,
                                                                  year)
    seizure_table_temp <- get_seizures_stats_table(file,
                                                   border_patrol_strings,
                                                   year)

    sector_table <-
      bind_rows(sector_table,
                sector_table_temp) %>%
      arrange(desc(fiscal_year))
    juv_and_adult_table <-
      bind_rows(juv_and_adult_table,
                juv_and_adult_table_temp) %>%
      arrange(desc(fiscal_year)) %>%
      select(-total_apprehensions)
    app_by_gender_table <-
      bind_rows(app_by_gender_table,
                app_by_gender_table_temp) %>%
      arrange(desc(fiscal_year)) %>%
      select(-total_apprehensions)
    seizure_table <-
      bind_rows(seizure_table,
                seizure_table_temp) %>%
      arrange(desc(fiscal_year))
  }



  sector_profile <-
    sector_table %>%
    left_join(juv_and_adult_table) %>%
    left_join(app_by_gender_table) %>%
    rename(female_apprehensions = female,
           male_apprehensions = male) %>%
    select(-unknown) %>%
    mutate(fiscal_year = as.numeric(fiscal_year)) %>%
    select(sector,
           fiscal_year,
           everything())
  sector_profile <- data.frame(sector_profile)

  sector_profile$sector <- gsub("_", " ", sector_profile$sector)
  sector_profile$sector <- gsub(" sectors total", "", sector_profile$sector)

  seizure_table$sector <- gsub("_", " ", seizure_table$sector)
  seizure_table <-
    seizure_table %>%
    select(sector,
           fiscal_year,
           everything())
  seizure_table <- data.frame(seizure_table)
  seizure_table$fiscal_year <- as.numeric(seizure_table$fiscal_year)
  return(list(sector_profile, seizure_table))
}
