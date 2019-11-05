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

sieca_category_tbl <- readr::read_csv(paste0(PATH_IN, "sieca_categories.csv"))
sieca_category_tbl <- sieca_category_tbl %>% 
	mutate(category = stringr::str_to_title(category))

sieca_sub_category_tbl <- readr::read_csv(paste0(PATH_IN, "sieca_sub_products.csv"))
sieca_sub_category_tbl <- sieca_sub_category_tbl %>% 
	mutate(sub_category = stringr::str_to_title(sub_category), X4 = NULL)

sieca_product_tbl <- readr::read_csv(paste0(PATH_IN, "sieca_products.csv"))
sieca_product_tbl <- sieca_product_tbl %>% 
	mutate(product_name = stringr::str_replace(product_name, "-", "")) %>% 
	mutate(product_name = stringr::str_replace(product_name, "-", "")) %>% 
	mutate(product_name = stringr::str_trim(product_name, side = 'both' )) %>% 
	mutate(tariff_code = stringr::str_trim(tariff_code, side = 'both' )) %>% 
	mutate(product_name = stringr::str_to_title(product_name)) %>% 
	mutate(tariff_code = ifelse(nchar(tariff_code) == 9, paste0('0', tariff_code), tariff_code)) %>% 
	mutate(sub_category_code = substr(tariff_code, 1, 2))

category_tbl <- inner_join(sieca_sub_category_tbl, sieca_category_tbl, by = 'category_code')
category_tbl <- category_tbl %>% 
	select(category_code, sub_category_code, category, sub_category)

# insert category to stagin table
# category_id:STRING,category_code:STRING,sub_category_code:STRING,category:STRING,sub_category:STRING,cod_sys:STRING,date_end:DATE
upload_file(category_tbl, "rowsums", "data_test", "staging_category", "WRITE_APPEND")


product_tbl <- inner_join(category_tbl, sieca_product_tbl, by = 'sub_category_code')
product_tbl %>% 
	glimpse()

product_tbl %>% 
	DataExplorer::plot_missing()

product_tbl <- product_tbl %>% 
	mutate(
		cheapter = substr(tariff_code, 1, 2),
		heading = substr(tariff_code, 3, 4),
		sub_heading = substr(tariff_code, 5, 6),
		further = substr(tariff_code, 7, 10)
	) %>% 
	mutate(original_code = tariff_code) %>% 
	mutate(tariff_code = paste0(cheapter, heading, '.', sub_heading, '.', further))
table(nchar(product_tbl$tariff_code))	
product_tbl$tariff_code[1:10]

product_tbl

arancel_loaded_tbl <- readr::read_csv(paste0(PATH_IN, "arancel_codes_loaded.csv"))
arancel_loaded_tbl <- arancel_loaded_tbl %>% 
	rename(tariff_code = tariff_fraction) %>% 
	mutate(tariff_code = paste0(tariff_code, '00'))
table(nchar(arancel_loaded_tbl$tariff_code))
arancel_loaded_tbl$tariff_code[1:10]

product_tbl <- left_join(arancel_loaded_tbl, product_tbl, by ='tariff_code')
product_tbl %>% 
	filter(is.na(category)) %>% 
	distinct(tariff_code)

product_tbl %>% 
	filter(cheapter == '30' & heading == '04' & sub_heading == '90' ) %>% 
	select(tariff_code)

1 9803.00.0000
2 3004.90.9900
  3004.90.1100
3 8703.23.9200
4 8708.99.9000
5 8703.22.9200


#insert into rowsums.trade.dim_category
#select GENERATE_UUID() AS category_id,
#category_code, sub_category_code, category, sub_category, 'SIECA', CAST('2099-12-31' AS date), 1
#from rowsums.data_test.staging_category


