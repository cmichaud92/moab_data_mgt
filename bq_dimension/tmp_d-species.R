

library(tidyverse)
library(readxl)
library(DBI)

d_spp <- read_xlsx("~/Downloads/Species Table.xlsx") %>%
  select(1:5)

write_csv(d_spp, "./bq_dimension/craig_spp.csv")
# Study
STUDY <- "123d"

# Data year (should be current year)
#YEAR <- year(now())
YEAR <- 2020

# ----- Specify the configuration environment -----

# default = authenticates to PI's account
CONFIG <- "macos"

{
  Sys.setenv(R_CONFIG_ACTIVE = CONFIG)
  config <- config::get(value = paste0(STUDY, "_config"),
                        file = "/Users/christophermichaud/Documents/etc/config_proj.yml")

}

{
  bigrquery::bq_auth(email = config$email)

  bq_view <- dbConnect(
    bigrquery::bigquery(),
    project = config$bq_project,
    dataset = config$bq_view,
    billing = config$bq_project
  )

  bq_dimension <- dbConnect(
    bigrquery::bigquery(),
    project = config$bq_project,
    dataset = config$bq_dimension,
    billing = config$bq_project
  )
}

d_species <- tbl(bq_dimension, "d_species") %>%
  collect()
# udwr_cds <- read_csv("./bq_dimension/d_species_udwr-codes.csv") %>%
#   rename(UCREFRP_SpeciesCode = 1)
# write_csv(udwr_cds, "./bq_dimension/d_species_udwr-codes.csv")
#
# udwr_cds <- read_csv("./bq_dimension/d_species_udwr-codes.csv") %>%
#   select(SpeciesCode = UCREFRP_SpeciesCode,
#          FishSpeciesStrainCode = FSSTRCD,
#          ScientificNameCode = SCIFNMCD)
ls <- read_csv("./bq_dimension/lifestage.csv")

cds <- unique(strsplit(ls$cd_spp, " "))
names(cds) <- unique(ls$id_lifestage)


new_d <- d_species %>%
  mutate(LifeStageID = case_when(SpeciesCode %in% cds$ls_1 ~ "ls_1",
                                 SpeciesCode %in% cds$ls_2 ~ "ls_2",
                                 SpeciesCode %in% cds$ls_3 ~ "ls_3",
                                 SpeciesCode %in% cds$ls_4 ~ "ls_4",
                                 SpeciesCode %in% cds$ls_5 ~ "ls_5")) %>%
  rename(LifeStageCode = LifeStageID)

ls_new <- ls %>%
  rename(LifeStageCode = id_lifestage,
         LifeStageAbbr = cd_lifestage,
         MinTotalLength_mm = min_totlength,
         MaxTotalLength_mm = max_totlength,
         LifeStageName = nm_lifestage,
         LifeStageDescription = dsc_lifestage,
         AssociatedSpeciesCodes = cd_spp)



map(names(cds), ~ mutate(case_when(SpeciesCode %in% . )))
dbWriteTable(bq_dimension,
             name = "d_species",
             value = new_d)

dbWriteTable(bq_dimension,
             name = "d_lifestage",
             value = ls_new)

library(UCRBtools)
