require(dplyr)
require(googlesheets)

## Load project properties
source("project_properties.R")

## Connect with Google Sheets
googlesheets::gs_auth(token = "shiny_app_token.rds")
token_log = "1g0EAVpEgv3dprBKx6oVFOjCRazTGNG3oHRUeroUce2Q"
# token_log = "1bQW57vcpBDxRQsukeI24lHQhOaf-aIPyDLFOFxDpxAY" # testing
token_sec001 = "1HPQkc51IBvtqQDZ4Wrk_yhsAOmtlMnQ_UP2mDrqe0M0"
token_sec002 = "1mDPJgwLSZKdcQjOfx2o23ukGf7ZHFXsLP8zFs03AGdg"
log_ss = googlesheets::gs_key(token_log)
