###############################################################
#                           123d Data io                      #
###############################################################


# Script pulls requested data from Moab's BigQuery database
# renames and restructures data targeting the 123d report Rmd

# It also fetches daily average waterdata from USGS

# All data is saved to csv

# Author: C. Michaud
# Created on: 8/30/2021


#-------------------------------
# Setup
#-------------------------------

# ----- Attach Packages -----
{
  library(tidyverse)
  library(lubridate)
  library(DBI)
  library(bigrquery)
  library(waterData)
}

# build "exclude"
`%!in%` <- Negate(`%in%`)


# ----- Declare variables -----

# Study
STUDY <- "123d"

# Data year (should be current year)
#YEAR <- year(now())
YEAR <- 2020

# ----- Specify the configuration environment -----

# default = authenticates to PI's account
CONFIG <- "macos"
# CONFIG = "management"
# CONFIG = "development"



# ----- Fetch config based on execution env. -----
{
  Sys.setenv(R_CONFIG_ACTIVE = CONFIG)
  config <- config::get(value = paste0(STUDY, "_config"),
                        file = "/Users/christophermichaud/Documents/etc/config_proj.yml")

}

# ----- Authenticate to bigquery -----
{
  bq_auth(email = config$email)

  bq <- dbConnect(
    bigrquery::bigquery(),
    project = config$bq_project,
    dataset = config$bq_dataset,
    billing = config$bq_project
  )
}

tmp_site <- tbl(bq, "v_analysis_sitedata") %>%
  filter(Year == {{YEAR}} &
           StudyCode == "123d") %>%
  left_join(tbl(bq, "d_river"), by = "RiverCode") %>%
  select(cd_study = StudyCode,
         cd_rvr = RiverCode,
         nm_rvr = RiverName,
         id_site = SiteID,
         tm_end = EndDateTime_UTC,
         rmi_start_bel = StartLocation_BelknapMiles,
         rmi_end_bel = EndLocation_BelknapMiles,
         amt_effort_sec = Effort_Seconds) %>%
  collect()

write_csv(tmp_site, "./Reports/Rept-data/test_123d_2020_ar-site.csv")

tmp_fish <- tbl(bq, "v_analysis_fishdata") %>%
  filter(Year == {{YEAR}} &
           StudyCode == "123d") %>%
  left_join(tbl(bq, "d_species"), by = "SpeciesCode") %>%
  select(id_site = SiteID,
         tm_encounter = EncounterDateTime_UTC,
         cd_spp = SpeciesCode,
         cn_spp = CommonName,
         n_fish = FishCount,
         tot_length = TotalLength_mm,
         weight = Weight_g,
         cd_sex = Sex,
         ind_ripe = IsRipe,
         cd_disp = DispositionCode,
         loc_x = Easting_UTM,
         loc_y = Northing_UTM) %>%
  collect()

write_csv(tmp_fish, "./Reports/Rept-data/test_123d_2020_ar-fish.csv")

# ----- WaterData -----

# Green River
gr <- importDVs(staid = "09315000",
                code = "00060",
                stat = "00003",
                sdate = paste0(YEAR, "-01-01"),
                edate = paste0(YEAR, "-12-31")) %>%
  rename(discharge = val,
         date = dates) %>%
  select(staid, date, discharge) %>%
  full_join(
    importDVs(staid = "09315000",
              code = "00010",
              stat = "00011",
              sdate = paste0(YEAR, "-01-01"),
              edate = paste0(YEAR, "-12-31")) %>%
    rename(temp = val,
           date = dates) %>%
    select(-qualcode),

            by = c("staid", "date")
    ) %>%
  mutate(sta_name = "Green River, Utah",
         rvr_code = "GR",
         rch_code = "LGR")

# Colorado River
co <- importDVs(staid = "09180500",
                code = "00060",
                stat = "00003",
                sdate = paste0(YEAR, "-01-01"),
                edate = paste0(YEAR, "-12-31")) %>%
  rename(discharge = val,
         date = dates) %>%
  select(staid, date, discharge) %>%

  full_join(
    importDVs(staid = "09180500",
              code = "00010",
              stat = "00003",
              sdate = paste0(YEAR, "-01-01"),
              edate = paste0(YEAR, "-12-31")) %>%
    rename(temp = val,
           date = dates) %>%
      select(-qualcode),

            by = c("staid", "date")
    ) %>%

  mutate(sta_name = "Near Cisco, Utah (Dewey)",
         rvr_code = "CO",
         rch_code = "LCO")

waterdata <- bind_rows(co, gr)

write_csv(waterdata, paste("./Reports/Rept-data/test", config$study, YEAR, "ar-water.csv", sep = "_"))
