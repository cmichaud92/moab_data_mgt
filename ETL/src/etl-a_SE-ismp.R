
##############################################################
#                         EFproj ETL                         #
##############################################################


#-------------------------------
# Attach packages
#-------------------------------
{
  library(tidyverse)
  library(lubridate)
  library(sf)
  library(googlesheets4)
  library(googledrive)
  library(UCRBtools)
  library(DBI)
}
# source functions
#source("./etl/src/qcfx_EL.R")

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



#--------------------------------------
# Google Drive auth for googlesheets access
#--------------------------------------

# -----Authenticate to google drive-----

drive_auth(email = config$email)
gs4_auth(token = drive_token())


 # ----- Locate QAQC sheet -----
if (CONFIG == "development") {

  qc_sheet <- drive_get(paste(YEAR, "dev", config$study, "QAQC", sep = "_"))

} else {

  qc_sheet <- drive_get(paste(YEAR, config$study, "QAQC", sep = "_"))

}

# Scrape info from QAQC-sheet if it exists

if (nrow(qc_sheet) > 1) {                       # There are multiple QAQC sheets for this project-year ???

  stop("Problem with google sheets")

} else if (nrow(qc_sheet) == 0) {               # There is no existing QAQC sheet for this project-year

  sheets_dat <- list()
  sheets_dat$last_num <- 0
  sheets_dat$exists_key_a <- ""
  message("No existing QAQC sheet. Starting site number = 0")

} else if (nrow(qc_sheet) == 1) {               # There is an existing QAQC sheet for this project-year

  tmp <- read_sheet(qc_sheet, range = "site")

  sheets_dat <- list()

  sheets_dat$last_num <- tmp %>%                # Calculate the site_id start number
    pull(id_site) %>%
    max() %>%
    str_sub(start = -3) %>%
    as.integer()

  sheets_dat$exist_key_a <- tmp %>%
    pull(key_a)

  message("QAQC sheet exists, check sheets_dat for details")
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

    left_join(tbl_rch, by = c("reach" = "cd_rch")) %>%                   # Add rvr_code variable

    select(id_site, study,
           year,
           cd_rvr,
           cd_rch = reach,
           date,
           rmi,
           matches("mc_|hab_"),
#           algae,
           ilat,
           ilon,
           crew,
           site_notes, s_idx, key_a) #%>%

  samp_n <- select(site, id_site, key_a) %>%
    left_join(tm_stp, by = "key_a") %>%
    arrange(ts) %>%
    group_by(id_site) %>%
    mutate(id_haul = paste(id_site, str_pad(row_number(), width = 2, pad = "0"), sep = "."))# Create site_id df and apply to all tables.

  } else {

  stop("No `site` data present")
}

attributes(samp_n$ts)


# Haul table
if (exists("tmp_haul")) {
  haul <- tmp_haul %>%
    left_join(samp_n, by = c("key_a", "key_aa")) %>%
    filter(haul_width > 0 &
             haul_lengt > 0) %>%
    select(id_haul,
           id_site,
           cd_gear = gear,
           haul_datetime = ts,
           haul_length = haul_lengt,
           haul_width,
           haul_notes,
           s_idx,
           h_idx,
           key_a,
           key_aa,
           s_idx
           )
  } else {

  stop ("No `haul` data present")

}


# Count data

if (exists("tmp_ct")) {

  count <- tmp_ct %>%
    filter(!is.na(species) |
             !is.na(fish_count)) %>%
    group_by(key_a, key_aa, species) %>%
    summarise(across(fish_count, sum),
              .groups = "drop") %>%
    left_join(samp_n, by = c("key_a", "key_aa")) %>%
    select(id_site,
           id_haul,
           cd_spp = species,
           n_fish = fish_count,
           key_a,
           key_aa,
           s_idx)

  } else {

  message("No count data present")
}

# Fish table
if (exists("tmp_fish")) {

  fish <- left_join(tmp_fish, samp_n, by = c("key_a", "key_aa")) %>%
    arrange(ts) %>%
    mutate(f_idx = row_number(),
           n_fish = 1) %>%

    select(id_haul,
           id_site,
           cd_spp = species,
           n_fish,
           tot_length,
           weight,
           cd_disp = disp,
           photo_num,
           fish_notes,
           key_a,
           key_aa,
           key_aab,
           s_idx)

  } else {

  warning("No `fish` data present")

}

# Add fish counts (count table) to the fish data table
if (exists("fish") & exists("count")) {

  fish <- fish %>%
    bind_rows(count) %>%
    arrange(id_haul)

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
           vial_type,
           photo_num,
           vial_notes,
           key_a,
           key_aa,
           key_aac,
           s_idx)

  } else {

  message("No `vial` data present")
}



#------------------------------
# QC data.tables
#------------------------------

ck_meta <- meta

if (exists("site")) {
  ck_site <- site %>%
    arrange(id_site)
}

if (exists("haul") && exists("site")) {
  ck_haul <- haul %>%
    left_join(select(site,
                     id_site,
                     pass,
                     cd_rvr,
                     cd_rch,
                     rmi),
               by = "id_site") %>%
    arrange(id_haul)
}

if (exists("ck_haul") && exists("fish")) {

  ck_fish <- fish %>%
    left_join(select(ck_haul,
                     id_haul,
                     pass,
                     cd_rvr,
                     cd_rch,
                     rmi),
              by = "id_haul") %>%
    arrange(id_haul)
}

if (exists("ck_haul") && exists("vial")) {

  ck_vial <- vial %>%
    left_join(select(ck_haul,
                     id_haul,
                     pass,
                     cd_rvr,
                     cd_rch,
                     rmi),
              by = "id_haul") %>%
    arrange(id_haul)
}


#-----------------------------------
# Prep files for append to gsheet
#-----------------------------------
if (nrow(ck_site) == 0) {

  stop("No new data to add!!!")

} else {

ck_names<-grep("^ck_",names(.GlobalEnv),value=TRUE) %>%
  sort()

ck_dat<-do.call("list", mget(ck_names))

fnl_names <- str_remove(ck_names, "ck_")

names(ck_dat) <- fnl_names

ck_dat <- ck_dat[sapply(ck_dat, nrow) > 0] #Remove 0 nrow dataframes

#saveRDS(ck_dat, file = paste0("./projects/", config$proj, "-", data_yr, "/output/", data_id,"_QAQC-data.Rds"))



# ----- Append data to QAQC gsheet -----

if (nrow(qc_sheet) == 0) {

  gs4_create(name = paste(YEAR, config$study, "QAQC", sep = "_"),
             sheets = ck_dat)

  drive_mv(paste(YEAR, config$study, "QAQC", sep = "_"),
           path = paste0(gsub("^.*?Drive/", "", config$gsheets_path), "/"))

  } else if (nrow(qc_sheet == 1)) {

  names(ck_dat) %>%
    map(~ sheet_append(qc_sheet, data = ck_dat[[.]], sheet = .))

  } else {

  stop("issues with google sheets")

 }
}

## End

