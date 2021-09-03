
##############################################################
#                       proj_138 ETL                         #
##############################################################


#-------------------------------
# Attach packages
#-------------------------------

library(tidyverse)
library(lubridate)
library(sf)
library(UCRBtools)
library(waterData)
library(DBI)


# build "exclude"
`%!in%` <- Negate(`%in%`)


# ----- Fetch config vars -----

config <- config::get("138ismp_config")

#------------------------
# Required variables
#------------------------

# Year of interest
yoi <- year(now())

# ----- Connect to database -----

con <-  dbConnect(RSQLite::SQLite(), "c:/Users/cmichaud/proj_mgt/database/sqlite/138ismp.sqlite")
dbListTables(con)
# dbDisconnect(con)

# ----- Query database, all tables, year of interest -----

haul_p <- tbl(con, "site") %>%
  filter(year == yoi) %>%
  inner_join(tbl(con, "haul"), by = "site_id") %>%
  select(site_id, haul_id, reach, prim_sec)

haul <- tbl(con, "haul") %>%
  left_join(haul_p, by = c("site_id", "haul_id")) %>%
  collect() %>%
  mutate(haul_area = haul_length * haul_width)


haul_sum <- haul %>%
  group_by(site_id, reach) %>%
  summarise(haul_area = sum(haul_area, na.rm = TRUE),
            .groups = "drop")

cs <- haul_p %>%
  left_join(tbl(con, "in_count"), by = "haul_id") %>%
  collect() %>%
  group_by(site_id, species) %>%
  summarise(fish_count = sum(fish_count, na.rm = TRUE),
            .groups = "drop") %>%
  filter(species == "CS")


site <- tbl(con, "site") %>%
  filter(year == yoi) %>%
  collect() %>%
  mutate(hab_area = hab_length * hab_width) %>%
  left_join(haul_sum, by = c("site_id", "reach")) %>%
  left_join(cs, by = "site_id") %>%
  select(-c(species), CS = fish_count) %>%
  mutate(CS = replace_na(CS, 0))

# site_smry <- site %>%
#   group_by(reach) %>%
#   summarise(mean_mctemp = round(mean(mc_temp, na.rm = TRUE), 1),
#             sd_mctemp = round(sd(mc_temp, na.rm = TRUE), 3),
#             min_mctemp = min(mc_temp, na.rm = TRUE),
#             max_mctemp = max(mc_temp, na.rm = TRUE),
#             mean_mcsecchi = round(mean(mc_secchi, na.rm = TRUE), 1),
#             sd_mcsecchi = round(sd(mc_secchi, na.rm = TRUE), 3),
#             min_mcsecchi = min(mc_secchi, na.rm = TRUE),
#             max_mcsecchi = max(mc_secchi, na.rm = TRUE),
#             mean_habtemp = round(mean(hab_temp, na.rm = TRUE), 1),
#             sd_habtemp = round(sd(hab_temp, na.rm = TRUE), 3),
#             min_habtemp = min(hab_temp, na.rm = TRUE),
#             max_habtemp = max(hab_temp, na.rm = TRUE),
#             mean_habsecchi = round(mean(hab_secchi, na.rm = TRUE), 1),
#             sd_habsecchi = round(sd(hab_secchi, na.rm = TRUE), 3),
#             min_habsecchi = min(hab_secchi, na.rm = TRUE),
#             max_habsecchi = max(hab_secchi, na.rm = TRUE))


count <- haul_p %>%
  left_join(tbl(con, "in_count"), by = "haul_id") %>%
  collect() %>%
  group_by(site_id, species) %>%
  summarise(fish_count = sum(fish_count, na.rm = TRUE),
            .groups = "drop")

cpue_nf <- haul %>%
  left_join(tbl(con, "in_count"), by = "haul_id", copy = TRUE) %>%
  select(site_id, haul_id, reach, haul_area, species, fish_count) %>%
  complete(nesting(site_id, haul_id, reach, haul_area),
           nesting(species),
           fill = list(fish_count = 0)) %>%
  filter(species %in% spp_nat) %>%
  group_by(site_id, reach, species) %>%
  summarise(fish_count = sum(fish_count, na.rm = TRUE),
            haul_area = sum(haul_area, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(fp10ms = round((fish_count/haul_area) * 10, 2))

cpue_nnf <- haul %>%
  filter(prim_sec == 1 &
           haul_num == 1) %>%
  left_join(tbl(con, "in_count"), by = "haul_id", copy = TRUE) %>%
  select(site_id, reach, haul_area, species, fish_count) %>%
  complete(nesting(site_id, reach, haul_area),
           nesting(species),
           fill = list(fish_count = 0)) %>%
  filter(species %!in% spp_nat) %>%

  mutate(fp10ms = round((fish_count/haul_area) * 10, 2))

fish <- tbl(con, "fish") %>%
  left_join(haul_p, by = "haul_id") %>%
  select(site_id, reach, species, tot_length) %>%
  collect()

dbDisconnect(con)

#-----------------------------------------------------------
# Scrape water data from usgs
#-----------------------------------------------------------

# ----- Jensen UT -----

# Discharge data
jen_dis <- importDVs(staid = "09261000",
                     code = "00060",
                     stat = "00003",
                     sdate = paste0(yoi, "-1-01"),
                     edate = paste0(yoi, "-12-31")) %>%
  rename(discharge = val,
         date = dates) %>%
  select(staid, date, discharge)

# Temperature data
jen_temp <- importDVs(staid = "09261000",
                      code = "00010",
                      stat = "00003",
                      sdate = paste0(yoi, "-1-01"),
                      edate = paste0(yoi, "-12-31")) %>%
  rename(temp = val,
         date = dates) %>%
  select(-qualcode)

jen_waterdata <- full_join(jen_dis, jen_temp, by = c("staid", "date")) %>%
  mutate(sta_name = "Jensen, Utah",
         rvr_code = "GR",
         rch_code = "MGR")


# -----Green River UT-----

# Green river discharge
gr_dis <- importDVs(staid = "09315000",
                    code = "00060",
                    stat = "00003",
                    sdate = paste0(yoi - 1, "-12-31"),
                    edate = paste0(yoi, "-12-31")) %>%
  rename(discharge = val,
         date = dates) %>%
  select(staid, date, discharge)

# Temperature data
gr_temp <- importDVs(staid = "09315000",
                     code = "00010",
                     stat = "00011",
                     sdate = paste0(yoi - 1, "-12-31"),
                     edate = paste0(yoi, "-12-31")) %>%
  rename(temp = val,
         date = dates) %>%
  select(-qualcode)

gr_waterdata <- full_join(gr_dis, gr_temp, by = c("staid", "date")) %>%
  mutate(sta_name = "Green River, Utah",
         rvr_code = "GR",
         rch_code = "LGR")

#----------------------
# Near Cisco, UT (Dewey)

# Discharge
co_dis <- importDVs(staid =  "09180500",
                    code = "00060",
                    stat = "00003",
                    sdate = paste0(yoi - 1, "-12-31"),
                    edate = paste0(yoi, "-12-31")) %>%
  rename(discharge = val,
         date = dates) %>%
  select(-qualcode)

# Temperature
co_temp <- importDVs(staid = "09180500",
                     code = "00010",
                     stat = "00003",
                     sdate = paste0(yoi - 1, "-12-31"),
                     edate = paste0(yoi, "-12-31")) %>%
  rename(temp = val,
         date = dates) %>%
  select(-qualcode)

co_waterdata <- full_join(co_dis, co_temp, by = c("staid", "date")) %>%
  mutate(sta_name = "Near Cisco, Utah (Dewey)",
         rvr_code = "CO",
         rch_code = "LCO")


#-----------------------------------------------------------
# Final water data set

waterdata <- bind_rows(jen_waterdata, gr_waterdata, co_waterdata) %>%
  mutate(dataset_created = now())


# ----- Save as .csv -----

write_csv(site, "./output/analysis_data/138_2020_site.csv")
write_csv(cpue_nnf, "./output/analysis_data/138_2020_cpue_nnf.csv")
write_csv(cpue_nf, "./output/analysis_data/138_2020_cpue_nf.csv")
write_csv(fish, "./output/analysis_data/138_2020_fish.csv")
write_csv(waterdata, "./output/analysis_data/138_2020_waterdata.csv")

