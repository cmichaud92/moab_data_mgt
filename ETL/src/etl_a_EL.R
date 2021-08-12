
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
source("./etl/src/qcfx_EL.R")

# build "exclude"
`%!in%` <- Negate(`%in%`)

#------------------------
# User defined variables
#------------------------

# Study
STUDY <- "123d"
#STUDY <- "123a"

# Data year (should be current year)
YEAR <- year(now())

TARGET <- c("SM")
TARGET <- "WE"
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

if (nrow(qc_sheet) > 1) {

  stop("Problem with google sheets")

}

# Adds 1 to the last sample number currently in the gsheet
# If new sheet, start number = 1


if (nrow(qc_sheet) == 0) {
  last_num <- 0
} else if (nrow(qc_sheet) == 1) {
  tmp <- read_sheet(qc_sheet, range = "site")
  last_num <- tmp %>%
    pull(site_id) %>%
    max() %>%
    str_sub(start = -3) %>%
    as.integer()
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
  date_created_index = now()
)


#------------------------------
# Extract data from list
#------------------------------

# Combine like tables and...
# Remove "Z" and complete easy qc

# Data present

# Site data
if ("site" %in% names(data)) {
  site_tmp <- map_df(data[grepl("site", names(data))], bind_rows) %>%
    mutate_all(na_if, "Z")                                                # Converts "Z"s to NA
} else {
  warning("No `site` data present")
}


# Water data
if ("water" %in% names(data)) {
  water_tmp <- map_df(data[grepl("water", names(data))], bind_rows) %>%
    mutate_at(c("cond_amb", "cond_spec", "rvr_temp", "secchi"),
              function(x) {ifelse(x == 0, NA, x)}) %>%                    # Converts 0's to NA
    mutate_all(na_if, "Z")                                                # Converts "Z"s to NA
} else {
  message("No `water` data present")
}

# Fish data
if ("fish" %in% names(data)) {
  fish_tmp <- map_df(data[grepl("fish", names(data))], bind_rows) %>%
    mutate_at(c("ilat", "ilon", "tot_length", "st_length", "weight"),     # Converts 0's to NA
              function(x) {ifelse(x == 0, NA, x)}) %>%
    mutate_all(na_if, "Z") %>%                                            # Converts "Z"s to NA
    mutate(ray_ct = na_if(ray_ct, "N"),
           tubercles = ifelse(species %in% spp_nat, tubercles, NA),        # Cleans up additional vars
           rep_cond = toupper(rep_cond))
} else {
  warning("No `fish` data present")
}

# Pittag
if ("pittag" %in% names(data)) {
  pit_tmp <- map_df(data[grepl("pittag", names(data))], bind_rows) %>%
    filter(!is.na(pit_num)) %>%
    mutate_all(na_if, "Z")                                               # Converts "Z"s to NA
} else {
  message("No `pittag` data present")
}

# Floytag
if ("floytag" %in% names(data)) {
  floy_tmp <- map_df(data[grepl("floytag", names(data))], bind_rows) %>%
    filter(!is.na(floy_num)) %>%
    mutate_all(na_if, "Z") %>%
    select(-floy_id)
} else {
  message("No `floytag` data present")
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

# Create sample_number and index,
# Create fnl table structures

# Site table
if (exists("site_tmp")) {
  site <- site_tmp %>%
    mutate(startdatetime = as.POSIXct(paste(mdy(date), starttime)),         # Replace `date` and `time` with `datetime`
           enddatetime = as.POSIXct(paste(mdy(date), endtime)),
           across(where(is.POSIXct), force_tz, tzone = "UTC"),
           el_sec = effort_sec + (effort_min * 60),                         # Convert effort to seconds
           project = config$study,
           year = year(startdatetime)) %>%                                  # Add year varaible

    arrange(enddatetime) %>%                                              # this orders data for indexing

    mutate(s_index = row_number(),                                          # add index for qc/site_id
#           site_num_crct = s_index + (start_num - 1),
           site_id = paste(project,
                           year(startdatetime),                    # Create sample number
                           str_pad(s_index, 3, "left", "0"),
                           sep = "_")) %>%

    left_join(tbl_rch, by = c("reach" = "cd_rch")) %>%                   # Add rvr_code variable

    select(s_index, site_id, project,
           year, river = cd_rvr,
           reach, pass,
           startdatetime, enddatetime,
           start_rmi, end_rmi,
           shoreline, el_sec,
           boat, crew,
           site_notes, key_a) %>%

    mutate_at(vars(ends_with("rmi")), function(x) {ifelse(.$reach %in% c("DESO", "ECHO"),  # Simple Belknap correction
                                                          x + 120, x)})



  samp_n <- select(site, key_a, site_id, s_index, t_stamp = enddatetime, reach)       # Create site_id df and apply to all tables.
} else {
  warning("No `site` data present")
}

attributes(samp_n$t_stamp)

attributes(site$startdatetime)
attributes(site$enddatetime)
# Water_qual table
if (exists("water_tmp")) {
  water <- left_join(water_tmp, samp_n, by = "key_a") %>%
    rename(water_notes = h2o_notes) %>%
    arrange(t_stamp) %>%
    select(key_ab, s_index, site_id,
           cond_amb, cond_spec,
           rvr_temp, secchi,
           water_notes, key_a)
} else {
  message("No `water` data present")
}

# Fish table
if (exists("fish_tmp")) {
  fish_1 <- left_join(fish_tmp, samp_n, by = "key_a") %>%
    mutate(datetime = as.POSIXct(paste(as.Date(t_stamp), time)),
           across(datetime, force_tz, tzone = "UTC")) %>%
    arrange(datetime) %>%
    mutate(f_index = row_number()) %>%
    mutate_at(vars(ends_with("rmi")), function(x) {ifelse(.$reach %in% c("DESO", "ECHO"),
                                                          x + 120, x)}) %>%
    select(f_index,
           key_aa,
           s_index,
           site_id, reach,
           rmi, datetime,
           species, tot_length,
           weight, sex,
           rep_cond, tubercles,
           ray_ct, disp,
           fish_notes, key_a,
           ilon, ilat)

  fish_sf <- fish_1 %>%                                     # Convert long-lat to UTMs
    group_by(site_id, rmi) %>%
    summarise(ilon = mean(ilon, na.rm = TRUE),
              ilat = mean(ilat, na.rm = TRUE),
              .groups = "drop") %>%
    filter(!is.na(ilon)) %>%
    st_as_sf(coords = c("ilon", "ilat"), crs = 4326) %>%
    st_transform(crs = 32612) %>%
    mutate(loc_x = st_coordinates(geometry)[, 1],
           loc_y = st_coordinates(geometry)[, 2],
           epsg = 32612) %>%
    st_drop_geometry() %>%
    select(site_id, rmi, loc_x, loc_y, epsg)

  fish <- full_join(fish_1, fish_sf, by = c("site_id", "rmi")) %>%
    select(-c(ilat, ilon))
} else {
  warning("No `fish` data present")
}

attributes(fish$datetime)

# Pittag table
if (exists("pit_tmp")) {
  pittag <- left_join(pit_tmp, samp_n, by = "key_a") %>%
    left_join(select(fish, key_aa, datetime, species), by = c("key_aa")) %>%
    arrange(datetime) %>%
    mutate(p_index = row_number(),
           pit_num = toupper(pit_num)) %>%
    select(p_index, s_index, key_aaa, key_aa, site_id,
           species,pit_type, pit_num, pit_recap,
           pit_notes, key_a)
} else {
  message("No `pittag` data present")
}

# Floytag table
if (exists("floy_tmp")) {
  floytag <- left_join(floy_tmp, samp_n, by = "key_a") %>%
    left_join(select(fish, key_aa, datetime, species), by = c("key_aa")) %>%
    arrange(datetime) %>%
    mutate(fl_index = row_number()) %>%
    select(fl_index, s_index, key_aab, key_aa, site_id,
           species, floy_color, floy_num, floy_recap,
           floy_notes)
} else {
  message("No `floytag` data present")
}

# Vial table
if (exists("vial_tmp")) {
  vial <- left_join(vial_tmp, samp_n, by = "key_a") %>%
    left_join(select(fish, key_aa, datetime, species), by = c("key_aa")) %>%
    arrange(datetime) %>%
    mutate(v_index = row_number()) %>%
    select(v_index, s_index, key_aac, key_aa, site_id,
           species, vial_num, vial_type, vial_notes)
} else {
  message("No `floytag` data present")
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

