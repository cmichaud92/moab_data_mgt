
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
    mutate(IsSecchiMaxDepth = ifelse(!is.na(hab_secchi), FALSE, TRUE),
           WaterQualityNotes = str_extract(site_notes, ".*[Ss]ecchi.*")) %>%
    select(SiteID = id_site,
           WaterTemperatureMain_C = mc_temp,
           SecchiDepthMain_mm = mc_secchi,
           WaterTemperatureHabitat_C = hab_temp,
           SecchiDepthHabitat_mm = hab_secchi,
           IsSecchiMaxDepth,
           WaterQualityNotes) %>%
    nest(WaterQuality = WaterTemperatureMain_C:WaterQualityNotes)


  # Fish Data
  nst_fish <- fnl_dat$fish %>%
    mutate(across(where(is.logical), as.character)) %>%
    select(HaulID = id_haul,
           SpeciesCode = cd_spp,
           FishCount = n_fish,
           TotalLength_mm = tot_length,
           Weight_g = weight,
           DispositionCode = cd_disp,
           FishNotes = fish_notes) %>%
    nest(FishData = c(SpeciesCode:FishNotes))

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
    mutate(SiteEPSGCode = 4326) %>%
    select(SiteID = id_site,
           StudyCode = study,
           RiverCode = cd_rvr,
           PassIdentifier = pass,
           SiteLocation_BelknapMiles = rmi,
           SiteDate_Local = date,
           PrimaryHabitatCode = hab_1,
           SecondaryHabitatCode = hab_2,
           HabitatLength_m = hab_length,
           HabitatWidth_m = hab_width,
           HabitatMaxDepth_mm = hab_mx_dep,
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
  message("place code here")
}

