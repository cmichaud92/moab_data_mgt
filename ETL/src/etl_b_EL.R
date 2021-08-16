
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

# build "exclude"
`%!in%` <- Negate(`%in%`)

#------------------------
# User defined variables
#------------------------

# Study
#STUDY <- "123d"
STUDY <- "123a"

# Data year (should be current year)
YEAR <- year(now())

# ----- Specify the configuration environment -----

# default = authenticates to PI's account
# CONFIG <- "macos"
# CONFIG = "management"
CONFIG = "development"



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

qc_sheet <- drive_get(paste(YEAR, config$study, "QAQC", sep = "_"))

if (nrow(qc_sheet) == 0) {
  stop("No QAQC sheet to upload!")
} else if (nrow(qc_sheet) > 1) {
  stop("Multiple QAQC sheets exist! Assess and remove all but one.")
} else {
  message("move code here")
}


#---------------------------
# Final data mods
#---------------------------


# ----- Import proofed data from gsheets -----

s_names <- sheet_names(qc_sheet)

dat <- s_names %>%
  map(~ read_sheet(qc_sheet, sheet = .))

names(dat) <- s_names

# ----- Data cleanup -----
fnl_dat <- dat %>%
#  map(~ modify_if(.x, is.POSIXct, as.character)) %>%
  map(~ select(.x, -c(matches("_flg$|_index$|^key_"))))

# ----- Additional cleaning -----
site <- fnl_dat$site %>%
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
         Notes = site_notes)

nst_water <- fnl_dat$water_qual %>%
  select(SiteID = site_id,
         AmbientConductivity_uS = cond_amb,
         SpecificConductivity_uS = cond_spec,
         WaterTemperature_C = rvr_temp,
         SecchiDepth_mm = secchi,
         Notes = water_notes) %>%
  nest(WaterQuality = AmbientConductivity_uS:Notes)
fish <- fnl_dat$fish %>%
  mutate(FishCount = 1,
         IsRipe = case_when(grepl("^EXP", rep_cond) ~ TRUE,
                            grepl("^INT|NOT", rep_cond) ~ FALSE),
         IsTuberculate = case_when(species %in% spp_nat &
                                     tubercles == "Y" ~ TRUE,
                                   species %in% spp_nat &
                                     tubercles == "N" ~ FALSE)) %>%
  separate(ray_ct, into = c("DorsalRayCount", "AnalRayCount"), sep = "/") %>%
  select(SiteID = site_id,
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
         Notes = fish_notes)




nst_pittag <- fnl_dat$pittag %>%
  mutate(PitTagType = as.character(pit_type),
         IsPitTagRecapture = case_when(pit_recap == "Y" ~ TRUE,
                                       pit_recap == "N" ~ FALSE)) %>%
  select(SiteID = site_id,
         PitTagString = pit_num,
         IsPitTagRecapture,
         PitTagType,
         Notes = pit_notes) %>%
  nest(PitData = PitTagString:Notes)




fnl_dat$floytag <- fnl_dat$floytag %>%
  select(-c(species, site_id))

fnl_dat$meta <- fnl_dat$meta %>%
  distinct()

saveRDS(fnl_dat, file = paste0("./projects/", config$proj, "-", data_yr, "/output/final-data.Rds"))

