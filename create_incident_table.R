
# init --------------------------------------------------------------------

# the packages below are required to run all functions for this script.
pkgs = c("dplyr", "lubridate", "pdftools", "purrr", "readr", "readxl", "stringr", "tidyr")
lapply(pkgs, require, character.only = TRUE)

# hardcoded variables -----------------------------------------------------

# input folder: change this to a folder where you store the PDFs
folder <- "C:/Users/INSO_HQ_SIO/OneDrive - International NGO Safety Organisation/Information Management/06. INSO Palestine/PDFs/"

# output (the filename gets a prefix with the report's date)
output_filename ="_pal_incidents.csv"

# wd & folders (current working directory)
# working directory: set path to the folder containing this script (create_incident_table.R)
wd = "C:/rprojects/pal_incidents_extract/"

# data sub-folder, containing required data files
wd_data = "data/"

# R sub folder, containing all R scripts
wd_r = "R/"

# file with security area names
file_sec_areas = "report_security_areas.csv"

# sourcing functions ------------------------------------------------------

# set working directory to the directory specified above. This is required for
# running the script from a scheduled task
setwd(wd)

# source all script files in 'R' subfolder
purrr::map(paste0(wd, wd_r, list.files(paste0(wd, wd_r))), \(x) source(x))

# load data files ---------------------------------------------------------

#read file with sec. areas. This file tells the script what security area to categorize the different incidents into
sec_areas <- read.csv2(paste0(wd, wd_data,  file_sec_areas))

# read the latest PDF file
file <- get_latest_file(folder)

# extracting the date of the report, used for filename as well as conf. note column.
file_date <- get_file_date(file)
output_filename = paste0(gsub("-", "", file_date), output_filename)

# reads the different location files in the 'data' folder, these are taken as-is from CHDC.
# Ignores any duplicate names in the combined file.
locations = load_location_file(
  paste0(wd, wd_data, 
         list.files(paste0(wd, wd_data), pattern = "location")
         )
  )

# create incident files ---------------------------------------------------

# loads pdf file
dat_pdf <- read_pdf_file(paste0(folder, file), collapse = TRUE)

# generate dataframe & log. Generating the log must be done before expanding incursion incidents
dat_pre <- extract_incidents(dat_pdf, sec_areas)  

log <- dplyr::summarise(dat_pre, count = dplyr::n(), .by = c(sec_area, governorate))

# expand incursion incidents and create full dataframe. (Incursion incidents mention multiple
# locations in one bullet point. In CHDC these are counted as separate incidents, so this
# function splits them into separate rows)
dat = dat_pre |> 
  expand_incursion_locs(vec = "report")  |>
  create_incident_df(file_date, locations)

# write files -------------------------------------------------------------

# write to .csv
dat |> 
  readr::write_csv(output_filename, na = "")
log |> 
  readr::write_csv(paste0("log_", output_filename), na = "")

# write to .xlsx
dat |> 
  writexl::write_xlsx(gsub(".csv", ".xlsx", output_filename))
log |> 
  writexl::write_xlsx(paste0("log_", gsub(".csv", ".xlsx", output_filename)))
