
###################################################################
#                      EF_V7  Database loading                    #
###################################################################

{
  library(tidyverse)
  library(purrr)
  library(lubridate)
  library(fs)
  library(DBI)
  library(googledrive)
  library(googlesheets4)
}


#-------------------------------
# Enter user defined variables
#-------------------------------

# Name of google sheets document containing PROOFED data to upload
# proof_data <- "Exact file name as it appears in Google drive"

# Enter the dataset year
YEAR <- 2020

# Entet the study code: This references the config
STUDY <- "123d"

#---------------------------------------------
# Fetch config for EL project of interest
#---------------------------------------------

config <- config::get(value = paste0(STUDY, "_config"),
                      file = "T:/My Drive/projects/etc/config.yml")

#-------------------------------
# Google Drive auth and io
#-------------------------------

# -----Authenticate to google drive-----

drive_auth(email = config$email)

gs4_auth(token = drive_token())


# -----Locate proofed dataset-----

# If sets returns more than 1 observation LOOK carefully
# Google drive allows multiple identical file names!!!!!

# qc_sheet <- drive_get(paste(data_yr, config$proj, "QAQC", sep = "_"))
qc_sheet <- drive_get(paste("fnl", YEAR, config$proj, "QAQC", sep = "_"))


#-----Connect to database-----


con <-  dbConnect(RSQLite::SQLite(), paste0(config$db_path, config$db_name))
dbListTables(con)

# dbDisconnect(con)
#------------------------------
# Upload ID
#------------------------------

# For initial upload
# u_id <- 1

# For all additional uploads
# Fetch last upload_id value (max()) from the database increment +1
u_id <- 1 + (
  tbl(con, "site") %>%
  pull(upload_id) %>%
  max()
  )
u_id[is.infinite(u_id)] <- 1

#dbDisconnect(con)


#---------------------------
# Final data mods
#---------------------------


# ----- Import proofed data from gsheets -----

s_names <- sheet_names(qc_sheet)

dat <- map(s_names, ~ read_sheet(qc_sheet, range = .x))
names(dat) <- s_names

# ----- Data cleanup -----
fnl_dat <- dat %>%
  map(~ modify_if(.x, is.POSIXct, as.character)) %>%
  map(~ select(.x, -c(matches("_flg$|_index$|^key_"))))

# ----- Additional cleaning -----
fnl_dat$site <- fnl_dat$site %>%
  rename(project_code = project) %>%
  mutate(upload_id = u_id)

fnl_dat$fish <- fnl_dat$fish %>%
  select(-c(reach)) %>%
  filter(!is.na(fish_id))

fnl_dat$pittag <- fnl_dat$pittag %>%
  mutate_at("pit_type", as.character) %>%
  select(-c(species, site_id))

fnl_dat$floytag <- fnl_dat$floytag %>%
  select(-c(species, site_id))

fnl_dat$meta <- fnl_dat$meta %>%
  distinct()

saveRDS(fnl_dat, file = paste0("./projects/", config$proj, "-", data_yr, "/output/final-data.Rds"))

#--------------------------------
# Upload tables
#--------------------------------

# con <- dbConnect(RSQLite::SQLite(),":memory:")


  # ----- Begin transaction -----
dbWithTransaction(con, code = {
  names(fnl_dat) %>%
    map(~ dbWriteTable(conn = con, name = ., value = fnl_dat[[.]], append = TRUE))
})

s <- tbl(con, "site") %>% collect()

ck <- fnl_dat$site

# Disconnect
dbDisconnect(con)


# Create backup database and copy to archive
# file.copy(from = paste0(config$root_path, config$db_name),
#           to = paste0(config$arch_path, "database/",format(as.Date(Sys.Date()), "%Y%m%d"),
#                       "_", "uid-", u_id, "_", config$db_name),
#           overwrite = TRUE)


## End

# # Ancillary delete me
#
# fnl_dat$site <- fnl_dat$site %>%
#   mutate(upload_id = 1)
#
# names(fnl_dat) %>%
#   map(~write_csv(fnl_dat[[.]], paste0("123d", ., ".csv")))
#
