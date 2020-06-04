cat("\014")
gc() # garbage collector
# ****************************
library(tidyverse)
PATH_OUT <- "./00_data/in/covid/"
date_name <- Sys.Date()


file_name <- "https://opendata.arcgis.com/datasets/4cc2d9431ecf406aa1ab039fed1de668_0.geojson"
minsa_covid_tbl <- jsonlite::fromJSON(file_name) %>% as.data.frame

properties_tbl <- minsa_covid_tbl$features.properties 
position_tbl <- minsa_covid_tbl$features.geometry
position_tbl$num_id <- rownames(position_tbl)
properties_tbl$num_id <- rownames(properties_tbl)

properties_tbl <- inner_join(properties_tbl, position_tbl, by = 'num_id')
properties_tbl$coordinates <- NULL 
properties_tbl$num_id <- NULL 

print(getwd())

print(paste0("extract data from:https://opendata.arcgis for ", date_name, ", total rows:", nrow(minsa_covid_tbl)))
out_file <- print(paste0("/home/erick_gordon/rowsums_icfj_data_platform/00_data/in/covid/", "CORREGIMIENTOS_(PU)-", as.character(date_name), ".csv"))

write.csv(properties_tbl, out_file, row.names = FALSE)

