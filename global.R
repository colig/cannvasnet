require(dplyr)
require(googlesheets)

## Load project properties
source("project_properties.R")

## Connect with Google Sheets
googlesheets::gs_auth(token = "shiny_app_token.rds")
token_log = "1g0EAVpEgv3dprBKx6oVFOjCRazTGNG3oHRUeroUce2Q"
token_sec001 = "1HPQkc51IBvtqQDZ4Wrk_yhsAOmtlMnQ_UP2mDrqe0M0"
token_sec002 = "1mDPJgwLSZKdcQjOfx2o23ukGf7ZHFXsLP8zFs03AGdg"
log_ss = googlesheets::gs_key(token_log)


CreateSNADataFrame <- function(df, from, to, linkNames) {
  # Create SNA data frame
  #
  # Args:
  #   df: data frame containing raw data
  #   from: name of "from" column
  #   to: vector of names of "to" columns
  #   linkNames: vector of link names (e.g., retweet, reply)
  #
  # Note: I start with implementing 1-1 links
  
  df.sna <- data.frame(from = df[[from]], 
                       to = df[[to]], 
                       link = linkNames)
  # remove rows with NA
  df.sna <- na.omit(df.sna)
  
  # merge rows with same metadata, and compute weight
  df.sna %>% group_by(from, to) %>% dplyr::summarise(weight = n())
}
