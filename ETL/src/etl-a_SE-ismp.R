
##############################################################
#                         EFproj ETL                         #
##############################################################


#-------------------------------
# Attach packages
#-------------------------------
{
  library(tidyverse)
  library(lubridate)
  library(bigrquery)
  library(sf)
  library(UCRBtools)
  library(DBI)
}
# source functions
source("~/Documents/etc/bq_write-safe.R")

# build "exclude"
`%!in%` <- Negate(`%in%`)

#------------------------
# User defined variables
#------------------------

# Study
STUDY <- "160se"
STUDY <- "138ismp"
# Data year (should be current year)
YEAR <- year(now())

if (grepl("^160", STUDY)) {

  TARGET <- c("RZ")

} else {

  TARGET <- c("CS")

}

# ----- Specify the configuration environment -----

# default = authenticates to PI's account

CONFIG = "macos"
# CONFIG = "development"



# ----- Fetch config based on execution env. -----
{
  Sys.setenv(R_CONFIG_ACTIVE = CONFIG)
  config <- config::get(value = paste0(STUDY, "_config"),
                        file = "/Users/christophermichaud/Documents/etc/config_proj.yml")

}




#-------------------------------
# Import field data (dbf) from local source
#-------------------------------

# This creates a large list, each dbf table is a separate list element

data <- dbf_io(file_path_in = paste0(config$data_path, paste(config$study, YEAR, sep = "_"))) %>%
  map(rename_all, tolower) %>%
  compact()


#-------------------------------
# Create row for meta table
#-------------------------------

meta <- tibble(
  project_code = config$study,
  year = YEAR,
  principal_fname = config$pi_fname,
  principal_lname = config$pi_lname,
  agency = config$agency,
  data_type = config$data_type,
  date_created_index = force_tz(now(), tzone = "UTC")
)


#------------------------------
# Extract data from list
#------------------------------

# Combine like tables and...
# Remove "Z" and complete easy qc

# Data present

# Site data
if (exists("site", where = data)) {

  tmp_site <- map_df(data[grepl("site", names(data))], bind_rows) %>%
    mutate(across(where(is.character), na_if, "Z"),
           across(where(is.numeric), na_if, 0)) %>%
    filter(key_a %!in% sheets_dat$exist_key_a)


} else {
  warning("No `site` data present")
}


# Haul data
if (exists("haul", where = data)) {

  tmp_haul <- map_df(data[grepl("haul", names(data))], bind_rows) %>%
  mutate(across(where(is.character), na_if, "Z")) %>%
    filter(key_a %!in% sheets_dat$exist_key_a)

  } else {
  message("No `water` data present")
}

# Fish data
if (exists("fish", where = data)) {

  tmp_fish <- map_df(data[grepl("fish", names(data))], bind_rows) %>%
    mutate(across(c(tot_length, weight),                                  # Converts 0's to NA
              function(x) {ifelse(x == 0, NA, x)}),
           across(where(is.character), na_if, "Z")) %>%
    filter(key_a %!in% sheets_dat$exist_key_a)

  } else {
  warning("No `fish` data present")
}

# Count
if (exists("count", where = data)) {

  tmp_ct <- map_df(data[grepl("count", names(data))], bind_rows) %>%
    filter(!is.na(species)) %>%
    mutate(across(where(is.character), na_if, "Z")) %>%
    filter(key_a %!in% sheets_dat$exist_key_a)
  } else {
  message("No `count` data present")
}

# Depth
if (exists("depth", where = data)) {

  tmp_depth <- map_df(data[grepl("depth", names(data))], bind_rows)
  } else {
  message("No `depth` data present")
}

# Vial
if (exists("vial", where = data)) {
  tmp_vial <- map_df(data[grepl("vial", names(data))], bind_rows) %>%
    filter(!is.na(vial_num)) %>%
    mutate(across(where(is.character), na_if, "Z")) %>%
    filter(key_a %!in% sheets_dat$exist_key_a)

  } else {
  message("No `vial` data present")
}


#------------------------------
# Modify data
#------------------------------

tm_stp_haul <- select(tmp_site, key_a, date) %>%
  inner_join(select(tmp_haul, key_a, key_aa, starttime), by = "key_a") %>%
  mutate(ts = mdy_hms(paste(date, starttime)),
         .keep = "unused") %>%
  arrange(ts) %>%
  mutate(h_idx = row_number())

tm_stp <- tm_stp_haul %>%
  group_by(key_a) %>%
  summarise(ts = min(ts),
            .groups = "drop") %>%
  arrange(ts) %>%
  mutate(s_idx = row_number()) %>%
  select(-ts) %>%
  left_join(tm_stp_haul, by = "key_a")

attributes(tm_stp$ts)

# Create sample_number and index,
# Create fnl table structures

# Site table
if (exists("tmp_site")) {

  site <- tmp_site %>%
    inner_join(distinct(tm_stp, key_a, s_idx), by = "key_a") %>%
    mutate(study = config$study,
           date = mdy(date),
           year = year(date)) %>%                                  # Add year varaible


    mutate(id_site = paste(study,
                           year(date),                    # Create sample number
                           str_pad(s_idx, 3, "left", "0"),
                           sep = "_")) %>%

    left_join(tbl_rch, by = c("reach" = "cd_rch"))                   # Add rvr_code variable


  samp_n <- select(site, id_site, key_a) %>%
    left_join(tm_stp, by = "key_a") %>%
    arrange(ts) %>%
    group_by(id_site) %>%
    mutate(id_haul = paste(id_site, str_pad(row_number(), width = 2, pad = "0"), sep = "."))# Create site_id df and apply to all tables.

  data_cks(site)

  } else {

  stop("No `site` data present")
}

attributes(samp_n$ts)


# Haul table
if (exists("tmp_haul")) {
  haul <- tmp_haul %>%
    left_join(samp_n, by = c("key_a", "key_aa")) %>%
    filter(haul_width > 0 &
             haul_lengt > 0)

  data_cks(haul)

  } else {

  stop ("No `haul` data present")

}

# Depth data
if (exists("tmp_depth")) {

  depth <- tmp_depth %>%
    select(-c(key_aaa, dep_notes)) %>%
    pivot_wider(names_from = location,
                values_from = depth:sub_2,
                names_glue = "{location}_{.value}")

  data_cks((depth))

} else {

  message("No depth table in data")
}

# join haul and depth tables

if (exists("haul") & exists("depth")) {

  haul <- left_join(haul, depth, by = c("key_a", "key_aa"))
}

# Count data

if (exists("tmp_ct")) {

  count <- tmp_ct %>%
    filter(!is.na(species) |
             !is.na(fish_count)) %>%
    group_by(key_a, key_aa, species) %>%
    summarise(across(fish_count, sum),
              .groups = "drop") %>%
    left_join(samp_n, by = c("key_a", "key_aa"))

  data_cks(count)

  } else {

  message("No count data present")
}

# Fish table
if (exists("tmp_fish")) {

  fish <- left_join(tmp_fish, samp_n, by = c("key_a", "key_aa")) %>%
    arrange(ts) %>%
    mutate(f_idx = row_number(),
           fish_count = 1)

  } else {

  warning("No `fish` data present")

}

# Add fish counts (count table) to the fish data table
if (exists("fish") & exists("count")) {

  fish <- fish %>%
    bind_rows(count) %>%
    arrange(id_haul)

  data_cks(fish)
  } else {

  message("No count data present")
}



# Vial table

if (exists("tmp_vial")) {

  vial <- left_join(tmp_vial, samp_n, by = c("key_a", "key_aa")) %>%
    arrange(id_haul) %>%
    select(id_haul,
           id_site,
           vial_num,
           vial_type)

} else {

  # vial <-  tibble(
  #
  # )


  message("No `vial` data present, creating filler table")

}

#---------------------------
# Final tables
#---------------------------

# Water Quality
water <- site %>%
  mutate(IsSecchiMaxDepth = ifelse(!is.na(hab_secchi), FALSE, TRUE),
         WaterQualityNotes = str_extract(site_notes, ".*[Ss]ecchi.*")) %>%
  select(SiteID = id_site,
         WaterTemperatureMain_C = mc_temp,
         SecchiDepthMain_mm = mc_secchi,
         WaterTemperatureHabitat_C = hab_temp,
         SecchiDepthHabitat_mm = hab_secchi,
         IsSecchiMaxDepth,
         WaterQualityNotes)

water$WaterTemperatureHabitat_C[water$SiteID == "138ismp_2021_016"] <- 24

nst_water <- water %>%
  nest(WaterQuality = WaterTemperatureMain_C:WaterQualityNotes)


# Fish Data
nst_fish <- fish %>%
  mutate(across(where(is.logical), as.character)) %>%
  select(HaulID = id_haul,
         SpeciesCode = species,
         FishCount = fish_count,
         TotalLength_mm = tot_length,
         Weight_g = weight,
         DispositionCode = disp,
         FishEncounterNotes = fish_notes) %>%
  nest(FishData = c(SpeciesCode:FishEncounterNotes))

# Haul Data
nst_haul <- haul %>%
  select(SiteID = id_site,
         HaulID = id_haul,
         GearCode = gear,
         HaulDateTime_Local = haul_datetime,
         HaulLength_m = haul_length,
         HaulWidth_m = haul_width,
         HaulMeanDepth_mm = haul_avg_depth,
         PrimarySubstrate = cd_sub,
         HasRedShiner = rs,
         HasSandShiner = ss,
         HasFatHead = fh,
         HaulNotes = haul_notes) %>%
  mutate(across(matches("^Has"), ~ case_when(.x == "Y" ~ TRUE,
                                             .x == "N" ~ FALSE))) %>%
  left_join(nst_fish, by = "HaulID") %>%
  nest(HaulData = HaulID:FishData)

# Final nested site data
site <- fnl_dat$site %>%
  mutate(SiteEPSGCode = 4326,
         PassIdentifier = as.character(pass),
         SiteDate_Local = as.Date(date),
         .keep = "unused") %>%
  select(SiteID = id_site,
         StudyCode = study,
         RiverCode = cd_rvr,
         PassIdentifier,
         SiteLocation_BelknapMiles = rmi,
         SiteDate_Local,
         PrimaryHabitatCode = hab_1,
         SecondaryHabitatCode = hab_2,
         SiteLength_m = hab_length,
         SiteWidth_m = hab_width,
         SiteMaxDepth_mm = hab_mx_dep,
         SiteLongitude_DD = ilon,
         SiteLatitude_DD = ilat,
         SiteEPSGCode,
         CrewNames = crew,
         SiteNotes = site_notes) %>%
  left_join(nst_water, by = "SiteID") %>%
  left_join(nst_haul, by = "SiteID")

}


# Write data to Big Query

if (exists("site")) {
  write_safe(df = site)
  #  message("place code here")
}



## End

