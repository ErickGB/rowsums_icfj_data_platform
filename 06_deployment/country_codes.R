cat("\014")
# ********************************************************************
# google bigquery connection ----
library(tidyverse)
library(bigrquery)
library(googledrive)
library(gargle)
library(dplyr)

PATH_IN <- "./00_data/in/imports/"
PATH_OUT <- "./00_data/out/imports/"
# ********************************************************************
# upload imports records to Google BigQuery staging table
upload_file <- function(upload_tbl, project, dataset, upload_table, disposition) {
	#drive_find(n_max = 5)
	# Load data table
	upload_data <- insert_upload_job(project, dataset, table = upload_table, 
		values = upload_tbl, write_disposition = disposition) #  WRITE_TRUNCATE
	wait_for(upload_data)
}


# ********************************************************************
country_tbl <- readr::read_csv(paste0(PATH_IN, "countries.csv"))
country_tbl <- country_tbl %>% 
	janitor::clean_names()
country_tbl <- country_tbl %>% 
	mutate(intermediate_region = NULL, intermediate_region_code = NULL)
country_tbl %>% 
	glimpse()

loaded_country_tbl <-  readr::read_csv(paste0(PATH_IN, "countries_loaded.csv"))
loaded_country_tbl <- loaded_country_tbl %>% 
	janitor::clean_names()
loaded_country_tbl <- loaded_country_tbl %>% 
	rename(alpha_2 = 'country_origin_code')

loaded_country_tbl %>% 
	glimpse()


# joined
country_tbl <- left_join(loaded_country_tbl, country_tbl, by = 'alpha_2')
country_tbl %>% 
	DataExplorer::plot_missing()

country_tbl %>% 
	filter(is.na(country_code))

country_tbl	%>% 
		glimpse()

country_tbl <- country_tbl	%>% 
	rename(latitud = lat, longitude = lon) %>% 
	select(alpha_2, alpha_3, iso_3166_2, country_code, name, region, sub_region, region_code, sub_region_code, latitud, longitude)

country_tbl


httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())
				
upload_file(country_tbl, "rowsums", "data_test", "staging_country", "WRITE_APPEND")

bq_deauth()


