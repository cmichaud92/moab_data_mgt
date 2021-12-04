
##############################################################
#                         EFproj ETL                         #
##############################################################


#-------------------------------
# Setup: packages, functions, variables and configuration
#-------------------------------

{
  library(tidyverse)
  library(lubridate)
  library(sf)
  library(googlesheets4)
  library(googledrive)
  library(UCRBtools)
  library(DBI)

# source functions
  source("./etl/src/qcfx_EL.R")
  source("~/Documents/etc/bq_write-safe.R")
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

# TARGET <- c("SM")
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
last_num <- 0
# Data present

# Site data
if ("site" %in% names(data)) {
  site_tmp <- map_df(data[grepl("site", names(data))], bind_rows) %>%
    mutate_all(na_if, "Z")

  # Impute missing site data
  missing <- tribble(
    ~key_a, ~project, ~year, ~river, ~reach, ~date, ~starttime, ~endtime, ~start_rmi, ~end_rmi, ~shoreline, ~effort_min, ~effort_sec, ~boat, ~crew, ~site_notes,
    "5af40a4m60tlahq", "123d", 2021, "GR", "LGR", "05/18/2021", "10:15:20", "11:30:00", 120, 117.7, "R", 75, 0, "Sparky", "SB CM2", NA,
    "5af40a4m60zi7or", "123d", 2021, "GR", "LGR", "05/18/2021", "13:15:00", "14:25:00", 117.7, 115.6, "R", 63, 0, "Sparky", "SB CM2", NA,
    "5af40a4m6138fnn", "123d", 2021, "GR", "LGR", "05/18/2021", "14:51:00", "16:25:00", 115.6, 114.0, "R", 70, 0, "Sparky", "SB CM2", NA
  )
  site_tmp1 <- bind_rows(site_tmp, missing)

  site <- site_tmp1 %>%
    mutate(startdatetime = as.POSIXct(paste(mdy(date), starttime)),         # Replace `date` and `time` with `datetime`
           enddatetime = as.POSIXct(paste(mdy(date), endtime)),
           across(where(is.POSIXct), force_tz, tzone = "UTC"),
           el_sec = effort_sec + (effort_min * 60),                         # Convert effort to seconds
           project = config$study,
           year = year(startdatetime)) %>%                                  # Add year varaible

    arrange(enddatetime) %>%                                              # this orders data for indexing

    mutate(s_index = last_num + row_number(),                                          # add index for qc/site_id
           #           site_num_crct = s_index + (start_num - 1),
           site_id = paste(project,
                           year(startdatetime),                    # Create sample number
                           str_pad(last_num + s_index, 3, "left", "0"),
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

    mutate(across(reach, ~ifelse(.x == "WW", "LCO", .x)),
           pass = NA)



  samp_n <- select(site, key_a, site_id, s_index, t_stamp = enddatetime)       # Create site_id df and apply to all tables.

} else {
  warning("No `site` data present")
}


# Water data
if ("water" %in% names(data)) {
  water_tmp <- map_df(data[grepl("water", names(data))], bind_rows) %>%
    mutate_at(c("cond_amb", "cond_spec", "rvr_temp", "secchi"),
              function(x) {ifelse(x == 0, NA, x)}) %>%                    # Converts 0's to NA
    mutate_all(na_if, "Z")

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

# Fish data
if ("fish" %in% names(data)) {
  fish_tmp <- map_df(data[grepl("fish", names(data))], bind_rows) %>%
    mutate_at(c("ilat", "ilon", "tot_length", "st_length", "weight"),     # Converts 0's to NA
              function(x) {ifelse(x == 0, NA, x)}) %>%
    mutate_all(na_if, "Z") %>%                                            # Converts "Z"s to NA
    mutate(ray_ct = na_if(ray_ct, "N"),
           tubercles = ifelse(species %in% spp_nat, tubercles, NA),        # Cleans up additional vars
           rep_cond = toupper(rep_cond))

  fish_1 <- left_join(fish_tmp, samp_n, by = "key_a") %>%
    mutate(datetime = as.POSIXct(paste(as.Date(t_stamp), time)),
           across(datetime, force_tz, tzone = "UTC")) %>%
    arrange(datetime) %>%
    mutate(f_index = row_number()) %>%
    group_by(site_id, rmi) %>%
    mutate(across(c(ilat, ilon), first)) %>%
    ungroup() %>%
    select(f_index,
           key_aa,
           s_index,
           site_id,
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

# Pittag
if ("pittag" %in% names(data)) {
  pit_tmp <- map_df(data[grepl("pittag", names(data))], bind_rows) %>%
    filter(!is.na(pit_num)) %>%
    mutate_all(na_if, "Z")

  pittag <- left_join(pit_tmp, samp_n, by = "key_a") %>%
    left_join(select(fish, key_aa, datetime), by = c("key_aa")) %>%
    arrange(datetime) %>%
    mutate(p_index = row_number(),
           pit_num = toupper(pit_num)) %>%
    select(p_index, s_index, key_aaa, key_aa, site_id,
           pit_type, pit_num, pit_recap,
           pit_notes, key_a)

} else {
  message("No `pittag` data present")
}

# Floytag
if ("floytag" %in% names(data)) {
  floy_tmp <- map_df(data[grepl("floytag", names(data))], bind_rows) %>%
    filter(!is.na(floy_num)) %>%
    mutate_all(na_if, "Z") %>%
    select(-floy_id)

  floytag <- left_join(floy_tmp, samp_n, by = "key_a") %>%
    left_join(select(fish, key_aa, f_index, datetime, species), by = c("key_aa")) %>%
    arrange(datetime) %>%
    mutate(fl_index = row_number()) %>%
    select(fl_index, s_index, f_index, key_aab, key_aa, site_id,
           species, floy_color, floy_num, floy_recap,
           floy_notes)

} else {
  message("No `floytag` data present")
}

# Vial
if ("vial" %in% names(data)) {
  vial_tmp <- map_df(data[grepl("vial", names(data))], bind_rows) %>%
    mutate_all(na_if, "Z")

  vial <- left_join(vial_tmp, samp_n, by = "key_a") %>%
    left_join(select(fish, key_aa, f_index, datetime, species, tot_length), by = c("key_aa")) %>%
    arrange(datetime) %>%
    mutate(v_index = row_number()) %>%
    select(v_index, s_index, f_index, key_aac, key_aa, datetime, site_id,
           species, tot_length, vial_num, vial_type, vial_notes)

} else {
  message("No `vial` data present")
}


#------------------------------
# Add corrections
#------------------------------

{
  # Site table
  site$river[site$s_index == 16] <- "CO"
  site$reach[site$s_index == 16] <- "LCO"
  site$end_rmi[site$s_index == 16] <- 72.8
  site$end_rmi[site$s_index == 10] <- 126.5
  site$start_rmi[site$s_index == 67] <- 114.8
  site$end_rmi[site$s_index == 40] <- 115.0
  site$end_rmi[site$s_index == 74] <- 114.5
  site$boat[site$boat == "Sparky"] <- "SPARKY"
  site <- site %>%
    mutate(startdatetime = if_else(enddatetime - startdatetime < el_sec, enddatetime - ((.1 * el_sec) + el_sec), startdatetime))

  # Fish table
  data_cks(fish)
  fish <- fish %>%
    filter(!is.na(species)) %>%
    filter(!grepl("BOGUS FISH", fish_notes)) %>%
    mutate(across(disp, ~case_when(species %in% spp_nat ~ "RA",
                                   species %!in% spp_nat ~ "DE")))

  fish$fish_notes[fish$f_index == 219] <- "Pit tagged prey item recovered from stomach (3DD.003BF2ECC0)"
  fish_adds <- tibble(
    key_a = "5af40a4m60tlahq",
    s_index = 25,
    key_aa = "recovered_from_prey_01",
    site_id = "123d_2021_025",
    rmi = 118,
    datetime = ymd_hms("2021-05-18 12:06:22"),
    species = "UI",
    disp = "DE",
#    loc_x =
    fish_notes = "Recovered from walleye's stomach"
  )
  fish <- bind_rows(fish, fish_adds)

  fish$species[fish$f_index == 236] <- "BT"
  fish$fish_notes[fish$f_index == 236] <- "Originally recorded as 'BN' very likely a 'BT' :)"
  fish$rmi[fish$s_index == 49] <- 95.5
  fish$rmi[fish$f_index == 960] <- 116.3

  # Pittag
  data_cks(pittag)
  pittag <- pittag %>%
    mutate(pit_type = "134",
           across(pit_num, ~ifelse(grepl('[0-9A-Z]{3}[.]{1}', .x), .x, gsub('^([0-9A-Z]{3})([0-9A-Z]+)$', '\\1.\\2', .x))))
#  pittag$pit_recap[pittag$p_index == 9] <- "NNF"
  pittag$pit_notes[pittag$p_index == 10] <- "Originally recorded as 'BN' very likely a 'BT' :)"
  pittag$key_aa[pittag$pit_num == "3DD.003BF2ECC0"] <- "recovered_from_prey_01"
  pittag$pit_notes[pittag$pit_num == "3DD.003BF2ECC0"] <- "Tag recovered from UI carcus inside walleye stomach"
  # Vial

  vial$vial_num[vial$f_index == 34] <- "4192101"
  vial$vial_num[vial$f_index == 42] <- "4192102"
  vial$vial_num[vial$f_index == 86] <- "5052101"
  vial$vial_num[vial$f_index == 87] <- "5052102"

  vial_adds <- tribble(
    ~key_aa, ~f_index, ~s_index, ~datetime, ~site_id, ~species, ~tot_length, ~vial_num, ~vial_type,
    "4m60ymffe", 230, 25, ymd_hms("2021-05-18 12:27:51"), "123d_2021_025", "WE", 613, "5182113", "whole-fish",
    "4m60yrz0f", 231, 25, ymd_hms("2021-05-18 12:31:11"), "123d_2021_025", "WE", 430, "5182114", "whole-fish"
  )

  vial <- bind_rows(vial, vial_adds) %>%
    mutate(vial_type = "WHF",
           vial_notes = "Collected for CSU walleye energetics study")
  # vial$vial_num[fnl_fish$f_index == 231] <- "05182114"
  # vial$vial_num[fnl_fish$f_index == 218] <- "05182101"

  nst_floytag <- tibble(key_aa = fish$key_aa) %>%
    mutate(FloyTagString = NA_character_,
           FloyTagColorCode = NA_character_,
           IsFloyTagRecapture = NA,
           Notes = NA_character_) %>%
    nest(FloyTag = FloyTagString:Notes)


}
#------------------------------
# QC data.tables
#------------------------------

{
  ck_meta <- meta

  if (exists("site")) {
    ck_site <- site %>%
      site_qcfx() %>%
      mutate_if(is.POSIXct, force_tz, tzone = "UTC") %>%
      filter(if_any(ends_with("flg"), ~ .x == "FLAG"))
  }

  if (exists("fish") && exists("site")) {
    ck_fish <- fish_qcfx(fish_data = fish, site_data = site) %>%
      mutate_if(is.POSIXct, force_tz, tzone = "UTC") %>%
      filter(if_any(ends_with("flg"), ~ .x == "FLAG"))
  }

  if (exists("pittag")) {
    ck_pittag <- pittag %>%
      filter(s_index > last_num) %>%
      pit_qcfx(fish_data = fish) %>%
      filter(if_any(ends_with("flg"), ~ .x == "FLAG"))
    }

  if (exists("floytag")) {
    ck_floytag <- floytag %>%
      filter(s_index > last_num) %>%
      floy_qcfx(fish_data = fish) %>%
      filter(if_any(ends_with("flg"), ~ .x == "FLAG"))
    }

  if (exists("site") && exists("fish")) {
    ck_stats <- stats_qcfx(site_data = site,
                           fish_data = fish,
                           spp = TARGET)
  }

  if (exists("water")) {
    ck_water_qual <- water %>%
    filter(s_index > last_num)
  }

  if (exists("vial")) {
    ck_vial <- vial %>%
    filter(s_index > last_num) %>%
    filter(if_any(ends_with("flg"), ~ .x == "FLAG"))
    }
}

#-----------------------------------
# Restructure data and rename fields
#-----------------------------------

# Water Quality
data_cks(water)
nst_water <- water %>%
  select(SiteID = site_id,
         AmbientConductivity_uS = cond_amb,
         SpecificConductivity_uS = cond_spec,
         WaterTemperature_C = rvr_temp,
         SecchiDepth_mm = secchi,
         Notes = water_notes) %>%
  nest(WaterQuality = AmbientConductivity_uS:Notes)


# Pit Tag
data_cks(pittag)
nst_pittag <- pittag %>%
  mutate(PitTagType = as.character(pit_type),
         IsPitTagRecapture = case_when(pit_recap == "Y" ~ TRUE,
                                       pit_recap == "N" ~ FALSE)) %>%
  select(key_aa,
         PitTagString = pit_num,
         IsPitTagRecapture,
         PitTagType,
         Notes = pit_notes) %>%
  nest(PitTag = PitTagString:Notes)

nst_vial <- vial %>%
  select(key_aa,
         VialString = vial_num,
         VialTypeCode = vial_type,
         Notes = vial_notes) %>%
  nest(Vial = VialString:Notes)
# # Floy Tag
# nst_floytag <- floytag %>%
#   mutate(IsFloyTagRecapture = case_when(floy_recap == "Y" ~ TRUE,
#                                         floy_recap == "N" ~ FALSE)) %>%
#   select(key_aa,
#          SiteID = site_id,
#          FloyTagString = floy_num,
#          FloyTagColorCode = floy_color,
#          IsFloyTagRecapture,
#          Notes = floy_notes) %>%
#   nest(FloyTag = FloyTagString:Notes)


# Fish Data
data_cks(fish)
nst_fish <- fish %>%
  mutate(FishCount = 1,
         IsRipe = case_when(grepl("^EXP", rep_cond) ~ TRUE,
                            grepl("^INT|NOT", rep_cond) ~ FALSE),
         IsTuberculate = case_when(species %in% spp_nat &
                                     tubercles == "Y" ~ TRUE,
                                   species %in% spp_nat &
                                     tubercles == "N" ~ FALSE)) %>%
  separate(ray_ct, into = c("DorsalRayCount", "AnalRayCount"), sep = "/") %>%
  select(key_aa,
         SiteID = site_id,
         EncounterDateTime_UTC = datetime,
         EncounterLocation_BelknapMiles = rmi,
         SpeciesCode = species,
         FishCount,
         TotalLength_mm = tot_length,
         Weight_g = weight,
         Sex = sex,
         IsRipe,
         IsTuberculate,
         DorsalRayCount,
         AnalRayCount,
         DispositionCode = disp,
         Easting_UTM = loc_x,
         Northing_UTM = loc_y,
         EPSGCode = epsg,
         Notes = fish_notes) %>%
  left_join(nst_pittag, by = "key_aa") %>%
  left_join(nst_floytag, by = "key_aa") %>%
  left_join(nst_vial, by = "key_aa") %>%
  select(-key_aa) %>%
  nest(FishData = c(EncounterDateTime_UTC:Vial))


# Final nested site data
data_cks(site)
fnl_site <- site %>%
  select(SiteID = site_id,
         StudyCode = project,
         RiverCode = river,
         PassIdentifier = pass,
         StartLocation_BelknapMiles = start_rmi,
         EndLocation_BelknapMiles = end_rmi,
         StartDateTime_UTC = startdatetime,
         EndDateTime_UTC = enddatetime,
         Shoreline = shoreline,
         Effort_Seconds = el_sec,
         BoatName = boat,
         CrewNames = crew,
         Notes = site_notes) %>%
  left_join(nst_water, by = "SiteID") %>%
  left_join(nst_fish, by = "SiteID")




# Write data to Big Query

if (exists("fnl_site")) {
  write_safe(df = fnl_site)
}


# write non-nested data to rds
dat_tbls <- list(site = site,
                 fish = fish,
                 pittag = pittag,
                 vial = vial,
                 water = water,
                 meta = meta)
write_rds(dat_tbls, "./ETL/output/2021_123d.Rds")
## End

