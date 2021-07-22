
# Impute missing site data
missing <- tribble(
  ~key_a, ~project, ~year, ~river, ~reach, ~date, ~starttime, ~endtime, ~start_rmi, ~end_rmi, ~shoreline, ~effort_min, ~effort_sec, ~boat, ~crew, ~site_notes,
  "5af40a4m60tlahq", "123d", 2021, "GR", "LGR", "05/18/2021", "10:15:20", "11:30:00", 120, 117.7, "R", 65, 0, "Sparky", "SB CM2", NA,
  "5af40a4m60zi7or", "123d", 2021, "GR", "LGR", "05/18/2021", "13:15:00", "14:25:00", 117.7, 115.6, "R", 63, 0, "Sparky", "SB CM2", NA,
  "5af40a4m6138fnn", "123d", 2021, "GR", "LGR", "05/18/2021", "14:51:00", "16:25:00", 115.6, 114.0, "R", 55, 0, "Sparky", "SB CM2", NA
)

site_tmp <- bind_rows(site_tmp, missing)
