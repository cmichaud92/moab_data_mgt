
#####################################################
#           Annual project directory setup          #
#####################################################


# ----- Setup -----
{
  library(fs)
  library(lubridate)
  library(purrr)
  library(googledrive)
  library(googlesheets4)
}
#------------------------
# User defined variables
#------------------------

# Study
STUDY <- "123a"

# Data year (should be current year)
YEAR <- year(now())

# Specify config used default, management or development
CONFIG <- "management"
#CONFIG <- "development"

# General type of sampling ("el", "se", "tr", "ant")
#EFFORT_TYPE <- "tr"

#----------------------------------
# Config
#----------------------------------

{
  Sys.setenv(R_CONFIG_ACTIVE = CONFIG)
  config <- config::get(value = paste0(STUDY, "_config"),
                        file = "T:/Shared drives/DNR_MoabFieldOffice/Data_mgt/etc/config_proj.yml")

}

config::is_active("management")


#--------------------------------------


# ----- Build the file system on google drive-----


# Map the requisite files in /src
files <- c(
           list.files("./ETL/src", pattern = paste0("*_", config$data_type, ".R"), full.names = FALSE),
           list.files("./ETL/src", pattern = "*_ALL.R", full.names = FALSE)
           )

# Map all raw data files in Data/
dat_files <- list.files(config$data_path)
                        # pattern = paste0(".*", YEAR, ".*.dbf"),
                        # ignore.case = TRUE, full.names = FALSE)


# Copy specific files to the project dir

if (CONFIG == "development") {

  if (dir.exists(paste0("./projects/dev", config$study, "-", YEAR))) {
    stop("Project development directory already exists!")
  } else {
  # Create data directorys
  dir_create(path = paste0("./projects/dev", config$study, "-", YEAR, "/data/raw"))
  dir_create(path = paste0("./projects/dev", config$study, "-", YEAR, "/data/clean"))
  # Create src directory
  dir_create(path = paste0("./projects/dev", config$study, "-", YEAR, "/src"))
  # Copy src files
  map(files, ~file_copy(path = paste0("./src/", .x),
                        new_path =  paste0("./projects/dev", config$study, "-", YEAR, "/src/", .x)))
  # Copy raw data files
  map(dat_files, ~file_copy(path = paste0("t:/My Drive/Data/datafiles/raw-data/",config$study, "/", .x),
                        new_path =  paste0("./projects/dev", config$study, "-", YEAR, "/data/raw", .x)))
  }

  } else {

    if (dir.exists(paste0("./projects/", config$study, "-", YEAR))) {
      stop("Project working directory already exists!")
    } else {
  # Create data directorys
  dir_create(path = paste0("./projects/", config$study, "-", YEAR, "/data/raw"))
  dir_create(path = paste0("./projects/", config$study, "-", YEAR, "/data/clean"))
  # Create src directory
  dir_create(path = paste0("./projects/", config$study, "-", YEAR, "/src"))
  # Copy src files
  map(files, ~file_copy(path = paste0("./ETL/src/", .x),
                        new_path =  paste0("./projects/", config$study, "-", YEAR, "/src/", .x)))
  # Copy raw data files
  map(dat_files, ~file_copy(path = paste0(config$data_path, "/", .x),
                            new_path =  paste0("./projects/", config$study, "-", YEAR, "/data/raw/", .x)))
    }
  }

#-------------------------------
# Google Drive auth and io
#-------------------------------

# -----Authenticate to google drive-----

drive_auth(email = config$email)

gs4_auth(token = drive_token())


# ----- Create googlesheet -----
# WARNING: be sure there is not an existing google sheet!

# Import gsheet template

templ <- readRDS(paste0("./ETL/src/", "gsheet-template_", config$data_type, ".Rds"))

if (CONFIG == "development") {

  gs4_create(
    name = paste(YEAR, "dev", config$study, "QAQC", sep = "_"),
    sheets = list(meta = templ$meta,
                  stats = templ$stats,
                  site = templ$site,
                  fish = templ$fish,
                  pittag = templ$pittag,
                  floytag = templ$floytag,
                  water_qual = templ$water_qual)
)

 drive_mv(paste(YEAR, "dev", config$study, "QAQC", sep = "_"),
          path = paste0(gsub("^.*?Drive/","",config$gsheets_path, "/")))

} else {
  gs4_create(
    name = paste(YEAR, config$study, "QAQC", sep = "_"),
    sheets = list(meta = templ$meta,
                  stats = templ$stats,
                  site = templ$site,
                  fish = templ$fish,
                  pittag = templ$pittag,
                  floytag = templ$floytag,
                  water_qual = templ$water_qual)
  )

  drive_mv(paste(YEAR, config$study, "QAQC", sep = "_"),
           path = paste0(gsub("^.*?Drive/", "", config$gsheets_path), "/"))

}

## END
