# https://www.cbp.gov/newsroom/media-resources/stats
source(here::here("R/clean.R"))

family_child_total_monthly      <- clean_family_child_total_monthly()
other_than_mexico_apprehensions <- clean_other_than_mexico()
border_patrol_staffing          <- clean_border_patrol_staffing()
nationwide_total_apprehensions  <- clean_national_apprehensions()
southwest_border_sector_deaths  <- southwest_border_deaths()
southwest_border_apprehensions  <- southwest_border_apprehensions()
#
sector_profile               <- clean_sector_profile()
apprehensions_seizures_stats <- sector_profile[[2]]
sector_profile               <- sector_profile[[1]]
#
source(here::here("R/saving_utils.R"))
setwd(here::here("data/clean"))
save_files(family_child_total_monthly,
           year = "2000_2019",
           file_name = "family_child_total_monthly_",
           save_name = "family_child_total_monthly_")
save_files(other_than_mexico_apprehensions,
           year = "2000_2019",
           file_name = "other_than_mexico_apprehensions_",
           save_name = "other_than_mexico_apprehensions_")
save_files(border_patrol_staffing,
           year = "1992_2019",
           file_name = "border_patrol_staffing_",
           save_name = "border_patrol_staffing_")
save_files(nationwide_total_apprehensions,
           year = "1925_2019",
           file_name = "nationwide_total_apprehensions_",
           save_name = "nationwide_total_apprehensions_")
save_files(southwest_border_sector_deaths,
           year = "1998_2019",
           file_name = "southwest_border_deaths_",
           save_name = "southwest_border_deaths_")
save_files(southwest_border_apprehensions,
           year = "1960_2019",
           file_name = "southwest_border_apprehensions_",
           save_name = "southwest_border_apprehensions_")
save_files(sector_profile,
           year = "2011_2019",
           file_name = "sector_profile_",
           save_name = "sector_profile_")
save_files(apprehensions_seizures_stats,
           year = "2011_2019",
           file_name = "apprehensions_seizures_stats_",
           save_name = "apprehensions_seizures_stats_")
#

save_as_zip("family_child_total_monthly_2000_2019",
            pattern = "family_child_total_monthly")
save_as_zip("other_than_mexico_apprehensions_2000_2019",
            pattern = "other_than_mexico_apprehensions")
save_as_zip("border_patrol_staffing_1992_2019",
            pattern = "border_patrol_staffing")
save_as_zip("nationwide_total_apprehensions_1925_2019",
            pattern = "nationwide_total_apprehensions")
save_as_zip("southwest_border_deaths_1998_2019",
            pattern = "southwest_border_deaths")
save_as_zip("southwest_border_apprehensions_1960_2019",
            pattern = "southwest_border_apprehensions")
save_as_zip("sector_profile_2011_2019",
            pattern = "sector_profile")
save_as_zip("apprehensions_seizures_stats_2011_2019",
            pattern = "apprehensions_seizures_stats")

setwd(here::here("data/raw"))
save_as_zip("raw_files")
