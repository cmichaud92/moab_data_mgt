
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

# Data year (should be current year)
YEAR <- year(now())

if (grepl("^160", STUDY)) {

  TARGET <- c("RZ")

} else {

  TARGET <- c("CS")

}

# ----- Specify the configuration environment -----

# default = authenticates to PI's account

CONFIG = "management"
# CONFIG = "development"



# ----- Fetch config based on execution env. -----
{
  Sys.setenv(R_CONFIG_ACTIVE = CONFIG)
  config <- config::get(value = paste0(STUDY, "_config"),
                        file = "T:/Shared drives/DNR_MoabFieldOffice/Data_mgt/etc/config_proj.yml")

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

# ----- Determine nrows for filter -----

if (nrow(qc_sheet) > 1) {

  stop("Problem with google sheets")

  } else if (nrow(qc_sheet) == 0) {

  last_num <- 0

  } else if (nrow(qc_sheet) == 1) {
  tmp <- read_sheet(qc_sheet, range = "site")
  last_num <- tmp %>%
    pull(site_id) %>%
    max() %>%
    str_sub(start = -3) %>%
    as.integer()

  } else {

  stop("Problem with google sheets")

}


#-------------------------------
# Import field data (dbf) from local source
#-------------------------------

# This creates a large list, each dbf table is a separate list element

data <- dbf_io(file_path_in = config$data_path) %>%
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
if ("site" %in% names(data)) {

  tmp_site <- map_df(data[grepl("site", names(data))], bind_rows) %>%
    mutate(across(where(is.character), na_if, "Z"),
           across(where(is.numeric), na_if, 0))

} else {
  warning("No `site` data present")
}


# Haul data
if ("haul" %in% names(data)) {
  tmp_haul <- map_df(data[grepl("haul", names(data))], bind_rows) %>%
  mutate(across(where(is.character), na_if, "Z"))                                                # Converts "Z"s to NA
} else {
  message("No `water` data present")
}

# Fish data
if ("fish" %in% names(data)) {
  tmp_fish <- map_df(data[grepl("fish", names(data))], bind_rows) %>%
    mutate(across(c(tot_length, weight),                                  # Converts 0's to NA
              function(x) {ifelse(x == 0, NA, x)}),
           across(where(is.character), na_if, "Z"))                        # Converts "Z"s to NA
} else {
  warning("No `fish` data present")
}

# Count
if ("count" %in% names(data)) {
  tmp_ct <- map_df(data[grepl("count", names(data))], bind_rows) %>%
    filter(!is.na(species)) %>%
    mutate(across(where(is.character), na_if, "Z"))                                               # Converts "Z"s to NA
} else {
  message("No `pittag` data present")
}

# Vial
if ("vial" %in% names(data)) {
  tmp_vial <- map_df(data[grepl("vial", names(data))], bind_rows) %>%
    filter(!is.na(vial_num)) %>%
    mutate(across(where(is.character), na_if, "Z"))
} else {
  message("No `vial` data present")
}

# Vial
if ("vial" %in% names(data)) {
  vial_tmp <- map_df(data[grepl("vial", names(data))], bind_rows) %>%
    mutate_all(na_if, "Z")
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
           pass,
           cd_rvr,
           cd_rch = reach,
           date,
           rmi,
           matches("mc_|hab_"),
           algae,
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
             haul_lengt > 0 &
             avg_dep > 0) %>%
    select(id_haul,
           id_site,
           cd_gear = gear,
           haul_datetime = ts,
           haul_length = haul_lengt,
           haul_width,
           haul_avg_depth = avg_dep,
           cd_sub = substrate,
           rs, ss, fh,
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
    group_by(key_a, key_aa, species, disp) %>%
    summarise(across(fish_count, sum),
              .groups = "drop") %>%
    left_join(samp_n, by = c("key_a", "key_aa")) %>%
    select(id_site,
           id_haul,
           cd_spp = species,
           n_fish = fish_count,
           cd_disp = disp,
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
           key_aaa,
           s_idx)

  } else {

  warning("No `fish` data present")

}

if (exists("fish") & exists("count")) {

  fish <- fish %>%
    bind_rows(count) %>%
    arrange(id_haul)

  } else {

  message("No count data present")
}


# Vial table

if (exists("tmp_vial")) {

  vial <- left_join(vial_tmp, samp_n, by = c("key_a", "key_aa")) %>%
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

#------------------------------

# Include only records NOT already appended to gsheets
sbst_site <- site %>%
  filter(s_index > last_num)

sbst_fish <- fish %>%
  filter(s_index > last_num)

#------------------------------
# QC data.tables
#------------------------------
ck_meta <- meta

if (exists("site")) {
  ck_site <- site %>%
    filter(s_index > last_num) %>%
    site_qcfx() %>%
    mutate_if(is.POSIXct, force_tz, tzone = "UTC")
}

if (exists("fish") && exists("site")) {
  ck_fish <- fish_qcfx(fish_data = sbst_fish, site_data = sbst_site) %>%
    mutate_if(is.POSIXct, force_tz, tzone = "UTC")
}

if (exists("pittag")) {
  ck_pittag <- pittag %>%
    filter(s_index > last_num) %>%
    pit_qcfx(fish_data = sbst_fish)}

if (exists("floytag")) {
  ck_floytag <- floytag %>%
    filter(s_index > last_num) %>%
    floy_qcfx(fish_data = sbst_fish)
  }

if (exists("sbst_site") && exists("sbst_fish")) {ck_stats <- stats_qcfx(site_data = sbst_site, fish_data = sbst_fish, spp = TARGET)}

if (exists("water")) {ck_water_qual <- water %>%
  filter(s_index > last_num)}

if (exists("vial")) {ck_vial <- vial %>%
  filter(s_index > last_num)}


#-----------------------------------
# Prep files for append to gsheet
#-----------------------------------
if (nrow(ck_site) == 0) {

  message("No new data to add!!!")
} else {

ck_names<-grep("^ck_",names(.GlobalEnv),value=TRUE) %>%
  sort()

ck_dat<-do.call("list",mget(ck_names))

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

