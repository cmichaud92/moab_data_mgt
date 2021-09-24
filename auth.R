
####################################################################
#                  Non-interactive authentication                  #
####################################################################

# Set up a .secret directory to store google auth tokens in the
# project directory

# This should have to run 1 time per user (in config file).  Once tokens
# are cached they can be fetched in non-interactive sessions

# ----- Declare variables -----

# Study
STUDY <- "123d"

# Data year (should be current year)
YEAR <- lubridate::year(lubridate::now())
#YEAR <- 2020

# ----- Specify the configuration environment -----

# default = authenticates to PI's account
CONFIG <- "macos"
# CONFIG = "management"
# CONFIG = "development"



# ----- Fetch config based on Study. -----
{
  Sys.setenv(R_CONFIG_ACTIVE = CONFIG)
  config <- config::get(value = paste0(STUDY, "_config"),
                        file = "/Users/christophermichaud/Documents/etc/config_proj.yml")

}

# designate project-specific cache
options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
gargle::gargle_oauth_cache()

# trigger auth on purpose --> store a token in the specified cache
bigrquery::bq_auth(email = config$email)

# see your token file in the cache, if you like
list.files(".secrets/")

