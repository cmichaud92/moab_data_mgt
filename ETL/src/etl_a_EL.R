
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
source("./src-master/EL_qcfx.R")

# build "exclude"
`%!in%` <- Negate(`%in%`)

#------------------------
# User defined variables
#------------------------

# Study
STUDY <- "123a"

# Data set location: Defaults to ./data
DATA_DIRECTORY <- "./data"

# Data year (should be current year)
# YEAR <- year(now())
YEAR <- 2020

# Specify config used default or development
CONFIG <- "development"


#----------------------------------
# Config
#----------------------------------

Sys.setenv(R_CONFIG_ACTIVE = CONFIG)

config <- config::get(value = paste0(STUDY, "_config"),
                      file = "T:/My Drive/projects/etc/config.yml")

config::is_active("development")


#--------------------------------------
# Google Drive auth for googlesheets access
#--------------------------------------

# -----Authenticate to google drive-----

drive_auth(email = config$email)
gs4_auth(token = drive_token())


 # ----- Locate QAQC sheet -----
if (CONFIG == "development") {

  qc_sheet <- drive_get(paste(YEAR, "dev", config$proj, "QAQC", sep = "_"))

} else {

  qc_sheet <- drive_get(paste(YEAR, config$proj, "QAQC", sep = "_"))

}

if (nrow(qc_sheet) == 1) {
  tmp <- read_sheet(qc_sheet, range = "site")
} else {
  warning("Problem with google sheets")
  }

# Adds 1 to the last sample number currently in the gsheet
# If new sheet, start number = 1


if (nrow(tmp) == 0) {
  start_num <- 1
} else {
  start_num <- 1 +(
  tmp %>%
    pull(site_id) %>%
    max() %>%
    str_sub(start = -3) %>%
    as.integer())
}


#-------------------------------
# Import field data (dbf) from local source
#-------------------------------

# This creates a large list, each dbf table is a separate list element

data <- dbf_io(file_path_in = paste0("./projects/", config$proj, "-", YEAR, "/data/", data_id)) %>%
  map(rename_all, tolower) %>%
  compact()


#-------------------------------
# Create row for meta table
#-------------------------------

meta <- tibble(
  project_code = config$proj,
  year = data_yr,
  principal_fname = config$pi_fname,
  principal_lname = config$pi_lname,
  agency = config$agency,
  data_type = config$data_type,
  data_set_index = data_id,
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
  warning("No `water` data present")
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
  warning("No `pittag` data present")
}

# Floytag
if ("floytag" %in% names(data)) {
  floy_tmp <- map_df(data[grepl("floytag", names(data))], bind_rows) %>%
    filter(!is.na(floy_num)) %>%
    mutate_all(na_if, "Z") %>%
    select(-floy_id)
} else {
  warning("No `floytag` data present")
}

# Vial
if ("vial" %in% names(data)) {
  vial_tmp <- map_df(data[grepl("vial", names(data))], bind_rows) %>%
    mutate_all(na_if, "Z")
} else {
  warning("No `vial` data present")
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
           el_sec = effort_sec + (effort_min * 60),                         # Convert effort to seconds
           project = config$proj,
           year = year(startdatetime)) %>%                                  # Add year varaible

    arrange(startdatetime) %>%                                              # this orders data for indexing

    mutate(s_index = row_number(),                                          # add index for qc/site_id
           site_num_crct = s_index + (start_num - 1),
           site_id = paste(project,
                           year(startdatetime),                    # Create sample number
                           str_pad(site_num_crct, 3, "left", "0"),
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



  samp_n <- select(site, key_a, site_id, t_stamp = startdatetime, reach)       # Create site_id df and apply to all tables.
} else {
  warning("No `site` data present")
}


# Water_qual table
if (exists("water_tmp")) {
  water <- left_join(water_tmp, samp_n, by = "key_a") %>%
    rename(water_id = key_ab,
           water_notes = h2o_notes) %>%
    arrange(t_stamp) %>%
    select(water_id, site_id,
           cond_amb, cond_spec,
           rvr_temp, secchi,
           water_notes, key_a)
} else {
  warning("No `water` data present")
}

# Fish table
if (exists("fish_tmp")) {
  fish_1 <- left_join(fish_tmp, samp_n, by = "key_a") %>%
    mutate(datetime = as.POSIXct(paste(as.Date(t_stamp), time))) %>%
    arrange(datetime) %>%
    mutate(f_index = row_number()) %>%
    mutate_at(vars(ends_with("rmi")), function(x) {ifelse(.$reach %in% c("DESO", "ECHO"),
                                                          x + 120, x)}) %>%
    select(f_index,
           fish_id = key_aa,
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

# Pittag table
if (exists("pit_tmp")) {
  pittag <- left_join(pit_tmp, samp_n, by = "key_a") %>%
    rename(pit_id = key_aaa,
           fish_id = key_aa) %>%
    left_join(select(fish, fish_id, datetime, species), by = c("fish_id")) %>%
    arrange(datetime) %>%
    mutate(p_index = row_number(),
           pit_num = toupper(pit_num)) %>%
    select(p_index, pit_id, fish_id, site_id,
           species,pit_type, pit_num, pit_recap,
           pit_notes, key_a)
} else {
  warning("No `pittag` data present")
}

# Floytag table
if (exists("floy_tmp")) {
  floytag <- left_join(floy_tmp, samp_n, by = "key_a") %>%
    rename(floy_id = key_aab,
           fish_id = key_aa) %>%
    left_join(select(fish, fish_id, datetime, species), by = c("fish_id")) %>%
    arrange(datetime) %>%
    mutate(fl_index = row_number()) %>%
    select(fl_index, floy_id, fish_id, site_id,
           species, floy_color, floy_num, floy_recap,
           floy_notes)
} else {
  warning("No `floytag` data present")
}

#------------------------------
# QC data.tables
#------------------------------
ck_meta <- meta

if (exists("site")) {
  ck_site <- site_qcfx(site_data = site) %>%
    mutate_if(is.POSIXct, force_tz, tzone = "UTC")
}

if (exists("fish") && exists("site")) {
  ck_fish <- fish_qcfx(fish_data = fish, site_data = site) %>%
    mutate_if(is.POSIXct, force_tz, tzone = "UTC")
}

if (exists("pittag")) {ck_pittag <- pit_qcfx(pit_data = pittag, fish_data = fish)}

if (exists("floytag")) {ck_floytag <- floy_qcfx(floy_data = floytag, fish_data = fish)}

if (exists("site") && exists("fish")) {ck_stats <- stats_qcfx(site_data = site, fish_data = fish, spp = c("SM"))}

if (exists("water")) {ck_water_qual <- water}

if (exists("vial_tmp")) {ck_vial <- vial_tmp}


#-----------------------------------
# Prep files for append to gsheet
#-----------------------------------

ck_names<-grep("^ck_",names(.GlobalEnv),value=TRUE) %>%
  sort()

ck_dat<-do.call("list",mget(ck_names))

fnl_names <- str_remove(ck_names, "ck_")

names(ck_dat) <- fnl_names

ck_dat <- ck_dat[sapply(ck_dat, nrow) > 0] #Remove 0 nrow dataframes

saveRDS(ck_dat, file = paste0("./projects/", config$proj, "-", data_yr, "/output/", data_id,"_QAQC-data.Rds"))



# ----- Append data to QAQC gsheet -----

names(ck_dat) %>%
  map(~ sheet_append(qc_sheet, data = ck_dat[[.]], sheet = .))


## End

