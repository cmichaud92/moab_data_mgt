





library(tidyverse)
library(lubridate)
library(sf)
library(UCRBtools)
library(DBI)
library(stringr)


# ----- User defined variables -----

# Study
STUDY <- "123d"

# Data set location: Defaults to ./data
# DATA_DIRECTORY <- "./data"

# Data year (should be current year, its an update...)
YEAR <- year(now())


# Specify config used default, development or management
CONFIG <- "management"

# Specify path to config (working on remote machines)
#MAC <- "/Users/christophermichaud/google drive/My Drive/projects/DWR-projects/"
MFO <- "T:/Shared drives/Data_mgt/Data/raw_data/123d"

PATH2DRIVE <- MFO


# ----- Configuration -----

Sys.setenv(R_CONFIG_ACTIVE = CONFIG)

config <- config::get(value = paste0(STUDY, "_config"),
                      file = "T:/My Drive/projects/etc/config_proj.yml")

# config <- config::get(value = paste0(STUDY, "_config"),
#                       file = paste0(PATH2DRIVE,"etc/config_proj.yml"))

config::is_active("management")



# This creates a large list, each dbf table is a separate list element

data <- dbf_io(file_path_in = config$data_path) %>%
  map(rename_all, tolower) %>%
  compact()

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


source("./ETL/src/etl_corrections_EL-123d-2021.R")


# Create sample_number and index,
# Create fnl table structures

# Site table
if (exists("site_tmp")) {
  site <- site_tmp %>%
    mutate(startdatetime = as.POSIXct(paste(mdy(date), starttime)),         # Replace `date` and `time` with `datetime`
           enddatetime = as.POSIXct(paste(mdy(date), endtime)),
           el_sec = effort_sec + (effort_min * 60),                         # Convert effort to seconds
           project = config$study,
           year = year(startdatetime),
           pass = NA) %>%                                  # Add year varaible

    arrange(startdatetime) %>%                                              # this orders data for indexing

    mutate(s_index = row_number(),
           id_site = paste(project,
                           year,
                           str_pad(s_index, width = 3, side = "left", pad = "0"),
                           sep = "_")) %>%                                         # add index for qc/site_id


    left_join(tbl_rch, by = c("reach" = "cd_rch")) %>%                   # Add rvr_code variable

    select(id_site, project,
           year, river = cd_rvr,
           reach, pass,
           startdatetime, enddatetime,
           start_rmi, end_rmi,
           shoreline, el_sec,
           boat, crew,
           site_notes, key_a, s_index) %>%

    mutate_at(vars(ends_with("rmi")), function(x) {ifelse(.$reach %in% c("DESO", "ECHO"),  # Simple Belknap correction
                                                          x + 120, x)})



} else {
  warning("No `site` data present")
}


if (exists("water_tmp")) {
  fnl_site <- site %>%
    left_join(water_tmp, by = "key_a") %>%
    rename(water_notes = h2o_notes) %>%
    arrange(startdatetime)

} else {
  warning("No `water` data present")
}
# Site correction
fnl_site$end_rmi[fnl_site$s_index == 10] <- 126.5
fnl_site$river[fnl_site$s_index == 16] <- "CO"
fnl_site$reach[fnl_site$s_index == 16] <- "LCO"
fnl_site$end_rmi[fnl_site$s_index == 16] <- 72.8
fnl_site$shoreline[fnl_site$s_index == 16] <- "B"

# site join....
samp_n <- select(fnl_site, s_index, key_a, t_stamp = startdatetime, reach, river)       # Create site_id df and apply to all tables.

# Fish table
if (exists("fish_tmp")) {
  fish1 <- left_join(fish_tmp, samp_n, by = "key_a") %>%
    mutate(datetime = as.POSIXct(paste(as.Date(t_stamp), time))) %>%
    arrange(datetime) %>%
    mutate(f_index = row_number()) %>%
    mutate_at(vars(ends_with("rmi")), function(x) {ifelse(.$reach %in% c("DESO", "ECHO"),
                                                          x + 120, x)}) %>%
    select(f_index,
           s_index,
           key_aa,
           key_a, river, reach,
           rmi, datetime,
           species, tot_length,
           weight, sex,
           rep_cond, tubercles,
           ray_ct, disp,
           fish_notes, key_a,
           ilon, ilat)

  fish_sf <- fish1 %>%                                     # Convert long-lat to UTMs
    group_by(key_a, rmi) %>%
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
    select(key_a, rmi, loc_x, loc_y, epsg)

  fish <- full_join(fish1, fish_sf, by = c("key_a", "rmi")) %>%
    select(-c(ilat, ilon)) %>%
    group_by(s_index) %>%
    fill(loc_x, loc_y)
} else {
  warning("No `fish` data present")
}


if (exists("pit_tmp")) {
  fnl_fish <- fish %>%
    left_join(pit_tmp, by = c("key_a", "key_aa")) %>%
    left_join(vial_tmp, by = c("key_a", "key_aa")) %>%
    filter(!is.na(species)) %>%
    arrange(datetime) %>%
    mutate(p_index = row_number(),
           pit_num = toupper(pit_num)) %>%
    ungroup()
} else {
  warning("No `pittag` data present")
}

fnl_fish$pit_recap[fnl_fish$f_index == 219] <- "NNF"

vial_fish <- fnl_fish %>%
  filter(!is.na(key_aac)) %>%
  mutate(vial_type = "whole-fish") %>%
  left_join(select(fnl_site, key_a, id_site), by = "key_a") %>%
  select(id_site,
         river,
         reach,
         rmi:rep_cond,
         disp,
         fish_notes,
         loc_x,
         loc_y,
         epsg,
         vial_num:vial_notes
         )
vial_site <- semi_join(fnl_site, vial_fish) %>%
  select(id_site:site_notes, cond_amb, rvr_temp, secchi)

write_csv(vial_site, "./output/123d_2021_site-vials.csv")
write_csv(vial_fish, "./output/123d_2021_fish-vials.csv")

ck <- fnl_fish %>%
  filter(species == "WE")
