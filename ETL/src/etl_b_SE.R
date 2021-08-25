
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
  library(bigrquery)
}

# Source bq write-safe
source("~/Documents/etc/bq_write-safe.R")
# build "exclude"
`%!in%` <- Negate(`%in%`)

#------------------------
# User defined variables
#------------------------

# Study
STUDY <- "160se"

# Data year (should be current year)
YEAR <- year(now())

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

qc_sheet <- drive_get(paste(YEAR, config$study, "QAQC", sep = "_"))

if (nrow(qc_sheet) == 0) {
  stop("No QAQC sheet to upload!")
} else if (nrow(qc_sheet) > 1) {
  stop("Multiple QAQC sheets exist! Assess and remove all but one.")
} else {

  # ----- Import proofed data from gsheets -----

  s_names <- sheet_names(qc_sheet)

  dat <- s_names %>%
    map(~ read_sheet(qc_sheet, sheet = .))

  names(dat) <- s_names

  # ----- Data cleanup -----
  fnl_dat <- dat %>%
    #  map(~ modify_if(.x, is.POSIXct, as.character)) %>%
    map(~ select(.x, -c(matches("_flg$|_index$|^key_"))))

  # ----- Additional cleaning and structuring -----

  # Water Quality
  nst_water <- fnl_dat$site %>%
    select(SiteID = id_site,
           WaterTemperatureMain_C = mc_temp,
           SecchiDepthMain_mm = mc_secchi,
           WaterTemperatureHabitat_C = hab_temp,
           SecchiDepthHabitat_mm = hab_secchi) %>%
    nest(WaterQuality = WaterTemperatureMain_C:SecchiDepthHabitat_mm)



  # Fish Data
  nst_fish <- fnl_dat$fish %>%
    mutate(across(where(is.logical), as.character)) %>%
    select(HaulID = id_haul,
           SpeciesCode = cd_spp,
           FishCount = n_fish,
           TotalLength_mm = tot_length,
           Weight_g = weight,
           DispositionCode = cd_disp,
           Notes = fish_notes) %>%
    nest(FishData = c(SpeciesCode:Notes))

  # Haul Data
  nst_haul <- fnl_dat$haul %>%
    select(SiteID = id_site,
           HaulID = id_haul,
           GearCode = cd_gear,
           HaulDateTime_Local = haul_datetime,
           HaulLength_m = haul_length,
           HaulWidth_m = haul_width,
           HaulMeanDepth_mm = haul_avg_depth,
           PrimarySubstrate = cd_sub,
           )
  # Final nested site data
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
           Notes = site_notes) %>%
    left_join(nst_water, by = "SiteID") %>%
    left_join(nst_fish, by = "SiteID")

}


# Write data to Big Query

if (exists("site")) {
  message("place code here")
}

