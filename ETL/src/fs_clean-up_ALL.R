
############################################################
#                   File system cleanup                    #
############################################################


# Run this script after all annual data is uploaded for each project

# ----- What it does -----
# Creates a database restore point from last upload and saves to `z-archive`
# Creates an archive of all ETL data and code and moves to `z-archive`
# Deletes the original ETL directory and everything in it


# Create backup database and copy to archive
file.copy(from = paste0("./", config$db_name),
          to = paste0("./z-archive/database/",format(as.Date(Sys.Date()), "%Y%m%d"),
                      "_", "uid-", u_id, "_", config$db_name),
          overwrite = TRUE)


zip_dir <- config$root_path
zip(zipfile = paste0("./z-archive/data-etl/", config$proj),
    files = zip_dir,
    flags = " a -tzip",
    zip = "C:\\Program Files\\7-Zip\\7z")


# ----- DANGER -----
# Remove annual etl files following archive step above

file_delete(config$root_path)


