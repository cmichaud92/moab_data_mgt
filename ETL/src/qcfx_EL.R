
################################################################
#           DataPlus EF application QAQC functions             #
################################################################

library(tidyverse)
library(lubridate)

# These functions create "flag" cols based on the given criteria.  Then return only rows containing a flag

#--------------------
# Site table QC
#--------------------
site_qcfx <- function(site_data) {
  site_data %>%
  mutate(rmi_length_flg = ifelse(start_rmi - end_rmi > 5 | start_rmi < end_rmi, "FLAG", ""),
         rmi_flg = case_when(reach == "ECHO" & between(start_rmi, 319.9, 344.4) &
                               reach == "ECHO" & between(end_rmi, 319.9, 344.4) ~ "",
                             reach == "DESO" & between(start_rmi, 128.5, 215.8) &
                               reach == "DESO" & between(end_rmi, 128.5, 215.8) ~ "",
                             reach == "LGR" & between(start_rmi, 0, 128.5) &
                               reach == "LGR" & between(end_rmi, 0, 128.5) ~ "",
                             reach == "LCO" & between(start_rmi, 0, 110.5) &
                               reach == "LCO" & between(end_rmi, 0, 110.5) ~ "",
                             reach == "WW" & between(start_rmi, 110.5, 127.6) &
                               reach == "WW" & between(end_rmi, 110.5, 127.6) ~ "",
                             reach == "LSJ" & between(start_rmi, 0, 53) &
                               reach == "LSJ" & between(end_rmi, 0, 53) ~ "",
                             TRUE ~ "FLAG"),
         datetime_flg = ifelse(as.duration(enddatetime - startdatetime)  < as.duration(el_sec), "FLAG", ""),
         el_sec_flg = ifelse(el_sec > 7200 | el_sec < 1000, "FLAG", ""),
         NA_flg = ifelse(apply(select(., s_index:crew), 1, function(x){any(is.na(x))}), "FLAG", "")) #%>%
}

#site_qcfx(site_data = site)
#-------------------
# Fish table QC
#-------------------
fish_qcfx <- function(fish_data, site_data) {

  native <- c("CS", "RZ", "HB", "BT", "FM", "BH", "RT", "SD", "CH")
  com_spp <- c("CS", "RZ", "HB", "BT", "SD", "FM", "BH", "RT", "SM", "BC",
               "LG", "BG", "GS", "GC", "BB", "YB", "WE", "GZ", "NP", "WS", "CH")
  site_data %>%
    select(s_index, site_id, key_a) %>%
    full_join(fish_data) %>%
    mutate(orphan_flg = ifelse(is.na(s_index), "FLAG", ""),
           # zero_catch_flg = ifelse(is.na(fish_id), "FLAG", ""),
           rmi_flg = case_when(reach == "ECHO" & between(rmi, 319.9, 344.4) ~ "",
                               reach == "DESO" & between(rmi, 128.5, 215.8) ~ "",
                               reach == "LGR" & between(rmi, 0, 128.5) ~ "",
                               reach == "LCO" & between(rmi, 0, 110.5) ~ "",
                               reach == "WW" & between(rmi, 110.5, 127.6) ~ "",
                               reach == "LSJ" & between(rmi, 0, 53) ~ "",
                               TRUE ~ "FLAG"),
           species_flg = ifelse(species %!in% com_spp, "FLAG", ""),
           tot_length_flg = ifelse(species %!in% c("CC", "GC", "NP") & tot_length > 1000, "FLAG", ""),
           weight_flg = ifelse(species %!in% c("CC", "GC", "NP") & weight > 2800, "FLAG", ""),
           disp_flg = ifelse(disp == "DE" & species %in% native |
                               disp == "RA" & species %!in% native |
                               is.na(disp),
                             "FLAG", ""))
}

#--------------------
# Pittag table QC
#--------------------
pit_qcfx <- function(pit_data, fish_data) {
  fish_data %>%
    select(f_index, fish_id, species) %>%
    right_join(pit_data) %>%
    mutate(orphan_flg = ifelse(is.na(f_index), "FLAG", ""),
           nnf_flg = ifelse(pit_recap == "NNF", "FLAG", ""),
           pit_recap_flg = ifelse(is.na(pit_recap) & !is.na(pit_num) |
                                    is.na(pit_num) & !is.na(pit_recap), "FLAG", ""),
           invalid_pitnum_flg = case_when(pit_type == 134 & str_length(pit_num) != 14 ~ "FLAG",
                                          pit_type == 400 & str_length(pit_num) != 9 ~ "FLAG"))
}

#---------------------
# Floytag table QC
#---------------------

floy_qcfx <- function(floy_data, fish_data) {
  floy_data %>%
    mutate(NA_flg = ifelse(apply(select(., fl_index:floy_recap), 1, function(x){any(is.na(x))}), "FLAG", ""))
}

#---------------------
# Stats
#---------------------

stats_qcfx <- function(site_data, fish_data, spp) {
  f <- fish_data %>%
    filter(species %in% spp) %>%
    group_by(site_id) %>%
    summarise(ct = n(),
              .groups = "drop")
  site_data %>%
    left_join(f, by = "site_id") %>%
    group_by(pass) %>%
    summarise(n_site = n(),
              effort_hr = round(sum(el_sec) / 3600, 2),
              catch = sum(ct, na.rm = TRUE),
              cpue = round(catch / effort_hr, 2),
              species = spp,
              .groups = "drop")
}

## Code below removes all rows NOT containing  a flag
#
# ck_site <- site_qctmp %>%
#   filter_at(vars(ends_with("flg")), any_vars(. == "FLAG"))
#
# ck_fish <- fish_qctmp %>%
#   filter_at(vars(ends_with("flg")), any_vars(. == "FLAG"))
#
# ck_pit <- pit_qctmp %>%
#   filter_at(vars(ends_with("flg")), any_vars(. == "FLAG"))
