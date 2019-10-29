# ********************************************************************
# google bigquery connection ----
library(bigrquery)
library(googledrive)
library(gargle)
library(dplyr)

PATH_OUT <- "./00_data/out/imports/"
# ********************************************************************

# googledrive authentication Sys.setlocale(locale="es_ES.UTF-8")
# "rowsums", "data_test", table = "staging_trades"
upload_file <- function(upload_tbl, project, dataset, upload_table, disposition) {
	drive_auth(path = "./00_scripts/rowsums-2198b8679813.json")
	# 1: Load data table
	upload_data <- insert_upload_job(project, dataset, table = upload_table, 
		values = upload_tbl, write_disposition = disposition) #  WRITE_TRUNCATE
	wait_for(upload_data)
}



# ********************************************************************
# load data ----
data_tbl <- readr::read_csv(paste0(PATH_OUT, "out_imports_2019-09-30.csv"))
data_tbl <- data_tbl %>% 
	mutate(
		date = as.Date(date,  tryFormats = c("%d-%m-%Y", "%d-%b-%Y")), 
		origin = ifelse(is.na(origin), "UN", origin),
		valor_del_flete = as.numeric(valor_del_flete),
		key = as.integer(key),
		id = as.integer(id),
		day = lubridate::day(date), 
		month = lubridate::month(date)
		) %>% 
	mutate(
		record_date = as.Date(paste0(lubridate::year(date), "-", lubridate::month(date), "-", lubridate::day(date)), tryFormats = c("%Y-%m-%d", "%d-%b-%Y")),
		year_month_date = as.Date(paste0(lubridate::year(date), "-", lubridate::month(date), "-", '01'), tryFormats = c("%Y-%m-%d", "%d-%b-%Y")),
		input_date = Sys.Date()
		)

data_tbl %>% 
	glimpse()


middle_one_tbl <- data_tbl %>% 
	filter(day >= 1 & day <= 10)
upload_file(middle_one_tbl, "rowsums", "data_test", "staging_imports", "WRITE_APPEND")


middle_two_tbl <- data_tbl %>% 
	filter(day > 10 & day <= 20)
upload_file(middle_two_tbl, "rowsums", "data_test", "staging_imports", "WRITE_APPEND")


middle_two_tbl <- data_tbl %>% 
	filter(day > 20 & day <= 31)
upload_file(middle_two_tbl, "rowsums", "data_test", "staging_imports", "WRITE_APPEND")










#countries_tbl <- readr::read_csv(paste0(PATH_OUT, "out_countries.csv"))
#countries_tbl <-  countries_tbl %>% 
#	select(name, alpha_2, un, alpha_3, country_code, iso_3166_2, region, sub_region, pop2005, lat, lon) %>% 
#	rename(origin = alpha_2)
#countries_tbl %>% 
#	glimpse()


#data_tbl <- left_join(data_tbl, countries_tbl, by  ='origin')
#data_tbl %>% 
#	filter(is.na(name) == TRUE) %>% 
#	distinct(origin)
#nrow(data_tbl)
#total - nrow(data_tbl) 
#table(data_tbl$origin, useNA = "always")
