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
YEAR <- year(now())
#YEAR <- 2020

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

  bq_view <- dbConnect(
    bigrquery::bigquery(),
    project = config$bq_project,
    dataset = config$bq_view,
    billing = config$bq_project
  )

  bq_dimension <- dbConnect(
    bigrquery::bigquery(),
    project = config$bq_project,
    dataset = config$bq_dimension,
    billing = config$bq_project
  )
}

tmp_site <- tbl(bq_view, "electrofish_sitedata") %>%
  filter(Year == {{YEAR}} &
           StudyCode == "123d") %>%
  collect() %>%
  left_join(tbl(bq_dimension, "d_river"), by = "RiverCode", copy = TRUE) %>%
  select(cd_study = StudyCode,
         cd_rvr = RiverCode,
         nm_rvr = RiverName,
         id_site = SiteID,
         tm_end = EndDateTime_UTC,
         rmi_start_bel = StartLocation_BelknapMiles,
         rmi_end_bel = EndLocation_BelknapMiles,
         amt_effort_sec = Effort_Seconds) %>%
  collect()

write_csv(tmp_site, paste0("./Reports/Rept-data/", paste(config$study, YEAR, "ar-site.csv", sep = "_")))

# fish biometric data
tmp_fish <- tbl(bq_view, "electrofish_fishdata") %>%
  filter(Year == {{YEAR}} &
           StudyCode == "123d") %>%
  collect() %>%
  left_join(tbl(bq_dimension, "d_species"), by = "SpeciesCode", copy = TRUE) %>%
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

write_csv(tmp_fish, paste0("./Reports/Rept-data/", paste(config$study, YEAR, "ar-fish.csv", sep = "_")))

# Fish count data (aggregated to SiteID)
s_ids <- tmp_site %>%
  pull(id_site)
tmp_count <-  tbl(bq_view, "electrofish_fishcount") %>%
  filter(SiteID %in% s_ids) %>%
  collect() %>%
  left_join(tbl(bq_dimension, "d_species"), by = "SpeciesCode", copy = TRUE) %>%
  select(id_site = SiteID,
         cd_spp = SpeciesCode,
         cn_spp = CommonName,
         n_fish = FishCount) %>%
  collect()

write_csv(tmp_count, paste0("./Reports/Rept-data/", paste(config$study, YEAR, "ar-count.csv", sep = "_")))


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
              stat = "00003",
              sdate = paste0(YEAR, "-01-01"),
              edate = paste0(YEAR, "-12-31")) %>%
    rename(temp = val,
           date = dates) %>%
    select(-qualcode),

            by = c("staid", "date")
    ) %>%
  mutate(nm_station = "Green River, Utah",
         cd_rvr = "GR")

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

  mutate(nm_station = "Near Cisco, Utah (Dewey)",
         cd_rvr = "CO")

waterdata <- bind_rows(co, gr)

write_csv(waterdata, paste0("./Reports/Rept-data/", paste(config$study, YEAR, "ar-water.csv", sep = "_")))
