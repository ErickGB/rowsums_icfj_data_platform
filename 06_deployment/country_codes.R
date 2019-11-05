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
# country_id:STRING,alpha_2:STRING,alpha_3:STRING,iso_3166_2:STRING,country_code:STRING,name:STRING,region:STRING,sub_region:STRING,region_code:STRING,sub_region_code:STRING,latitude:FLOAT,longitude:FLOAT,is_active:INTEGER,date_end:DATETIME

httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())
				
upload_file(country_tbl, "rowsums", "data_test", "staging_country", "WRITE_APPEND")

bq_deauth()


#INSERT INTO rowsums.trade.dim_country
#SELECT GENERATE_UUID() AS country_id, 
#alpha_2, alpha_3, iso_3166_2, country_code, name, region, sub_region, region_code, sub_region_code,
#latitud as latitude, longitude, 1, CAST('2099-12-31' AS date) 
#from rowsums.data_test.staging_country

#insert into rowsums.trade.dim_category 
#select GENERATE_UUID() , 'XXII', '98', 'Reservado para usos particulares del país', 'Menaje de casa, efectos personales, muestras sin valor comercial, envíos de socorro', 'SIECA', cast('2099-12-31' as date), 1;


#insert into rowsums.trade.fact_import
#select GENERATE_UUID() AS import_id,
#category_id, country_id, 
#CAST(
#	concat(
#		cast(EXTRACT(year FROM date) as string), 
#		case when length(cast(month as string)) = 1 THEN concat('0', cast(month as string)) ELSE cast(month as string) end,
#		'01'
#	) AS INT64) date_id,
#link, date, RUC, company, country_origin_code, description, key, original_text, quantity_text, 
#tariff_fraction, import_taxes, oil_protection_taxes, isc_taxes, itbms_taxes, gross_weight_text, 
#net_weight_text, port, total_to_pay, cif, freight_value, insurance, fob, quantity, gross_weight, 
#net_weight, day, month, EXTRACT(year FROM date) year, country_data, 
#year_month_date, cast(concat(cast(EXTRACT(year FROM date) as string), '-01-01') as date) year_date, 1 record_id
#from rowsums.data_test.staging_imports si
#inner join rowsums.trade.dim_country co on si.country_origin_code = co.alpha_2
#inner join rowsums.trade.dim_category ca on SUBSTR(tariff_fraction, 1, 2) = ca.sub_category_code;




