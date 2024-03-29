cat("\014")
# ********************************************************************
# google bigquery connection ----
library(bigrquery)
library(googledrive)
library(gargle)
library(dplyr)

type <- "I" # "I"
PATH_OUT <- ifelse(type == "I", "./00_data/out/imports/", "./00_data/out/exports/") 
# ********************************************************************
# spanish to english months
month_names_tbl <- tibble(name = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"), 
													value = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
													id = seq(1, 12, 1)) 
get_month_id <- function(month_name) {
	value <- month_names_tbl %>% 
		filter(name == month_name) %>% 
		select(value) %>% 
		as.character()
	
	return(value)
}

# upload imports records to Google BigQuery staging table
upload_file <- function(upload_tbl, project, dataset, upload_table, disposition) {
	#drive_find(n_max = 5)
	# Load data table
	upload_data <- insert_upload_job(project, dataset, table = upload_table, 
		values = upload_tbl, write_disposition = disposition) #  WRITE_TRUNCATE
	wait_for(upload_data)
}

set_save_log <- function(data_tbl)
{
	upload_file(data_tbl, "rowsums", "data_test", "log_event", "WRITE_APPEND")
}

# process data records
set_process_data <- function(file_name) {
	# file_name <- "out_imports_2019-12-31.csv"
	message_out <- ""
	total <- 0
	tryCatch(
		{
			print(file_name)
			data_tbl <- readr::read_csv(paste0(PATH_OUT, file_name))
			
			if(type == "E") {
				data_tbl <- data_tbl %>% 
					rename(
						impuestos_de_importacion = impuestos_de_exportacion
					)
			}

			
			#temp <- as.character(data_tbl[c(61272), c("valor_del_flete", "text_original")][2])
			data_tbl <- data_tbl %>% 
				mutate(
					mmonth = purrr::map_chr(tolower(substr(date, 4, 6)), get_month_id),
					date = paste(substr(date, 1, 2), mmonth, substr(date, 8, 11), sep='-'),
					date = as.Date(date,  tryFormats = c("%d-%m-%Y", "%d-%b-%Y")), 
					origin = ifelse(is.na(origin), "UN", origin),
					valor_del_flete = as.numeric(valor_del_flete),
					key = as.integer(key),
					id = as.integer(id),
					day = as.integer(lubridate::day(date)), 
					month = as.integer(lubridate::month(date))
				) %>% 
				mutate(
					#record_date = as.Date(paste0(lubridate::year(date), "-", lubridate::month(date), "-", lubridate::day(date)), tryFormats = c("%Y-%m-%d")),
					country_data = 'PA',
					year_month_date = as.Date(paste0(lubridate::year(date), "-", lubridate::month(date), "-", '01'), tryFormats = c("%Y-%m-%d", "%d-%b-%Y")),
					input_date = Sys.Date(),
					impuestos_de_importacion = as.numeric(impuestos_de_importacion),
					cantidad_int = as.integer(cantidad_int)
				) %>% 
				mutate(
					mmonth = NULL 
				) %>% 
				rename(
					country_origin_code = origin,
					original_text = text_original,
					quantity_text = cantidad, 
					tariff_fraction = fraccion_arancelaria,
					import_taxes = impuestos_de_importacion,
					oil_protection_taxes = impuestos_de_proteccion_de_petroleo,
					isc_taxes = impuestos_isc, 
					itbms_taxes = impuestos_itbm,
					gross_weight_text = peso_bruto,
					net_weight_text = peso_neto,
					port = puerto_de_entrada,
					total_to_pay = total_a_pagar,
					cif = valor_cif,
					freight_value = valor_del_flete,
					insurance = valor_del_seguro,
					fob = valor_fob,
					quantity = cantidad_int,
					gross_weight = peso_bruto_kg,
					net_weight = peso_neto_kg
				)
			
			data_tbl$total_pagar_txt <- NULL 
			data_tbl$impuestos_de_proteccion_de_petroleo_txt <- NULL 
			data_tbl$valor_cif_txt <- NULL 
			data_tbl$valor_fob_txt <- NULL 
			data_tbl$record_type <- type
			
			data_tbl %>% 
				glimpse()
			
			print(table(data_tbl$day))
			
			data_tbl %>% 
				DataExplorer::plot_missing()
			
			#distinct_days <- data_tbl %>% 
			#	distinct(day) 
			#print(distinct_days$day)
			
			for(day_in in 1:31) {
				first_tbl <- data_tbl %>% 
					filter(day == day_in)
				
					total <- nrow(first_tbl)
					if(total > 0) {
						upload_file(first_tbl, "rowsums", "data_test", "staging_imports", "WRITE_APPEND")
						print(paste0(as.character(day_in),  ': uploading data ->', as.character(total), " records, done!"))
					} else {
						print(paste0(day_in, ": we didn't find records..."))
					}
			}
			
			total_records <- nrow(data_tbl)
			return(total_records)
		},
		error=function(cond) {
			message_out <- paste0("error: ", as.character(cond))
			print(message_out)
			# Choose a return value in case of error
			total <- 0
			return(0)
		} #,warning=function(cond) {}
		)
	# save log record
	log_tbl <- tibble(date = Sys.Date(), file = file_name, records = total, message = message_out)
	set_save_log(log_tbl)
	
	return(out)
}


#paste(substr(date, 1, 2), get_month_id(tolower(substr(date, 4, 6))), substr(date, 8, 11), sep='-')
# ********************************************************************
# load data ----

# read file ----  "./00_data/out/imports/out_imports_2020-01-31.csv"
list_files <- base::list.files(PATH_OUT)
data_tbl <- tibble(file_name = list_files)
date_time <- as.character(Sys.Date()) # process execution day
last_update <- paste0(substr(date_time, 1, 8), "01") # execution month

data_tbl$process_date <- as.Date(last_update, tryFormats = c('%Y-%m-%d')) #Sys.Date()
View(data_tbl)
data_tbl[55,]
data_tbl <- data_tbl[55,]
nrow(data_tbl)

httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())
				
drive_auth(path = "./00_scripts/rowsums-2198b8679813.json")
project <- "rowsums"

projectid<-'rowsums'
datasetid<-'journalists'
bq_conn <-  dbConnect(bigquery(), 
											project = projectid,
											dataset = datasetid, 
											use_legacy_sql = FALSE
)


data_tbl
nrow(data_tbl)

system.time(
	# load data ----
	data_tbl <- data_tbl %>% 
		mutate(tbl = furrr::future_map(file_name, set_process_data), .progress = TRUE) %>% 
		tidyr::unnest()
)

data_tbl %>% 
	head()
sum(data_tbl$tbl) # 147,226; 137,332
# data_tbl[c(85249, 85249), ]

bq_deauth()


#row             col               expected  actual                                               file
#45524 valor_del_flete no trailing characters ,826.09 './00_data/out/imports/out_imports_2019-10-31.csv'
#61977 valor_del_flete no trailing characters ,623.66 './00_data/out/imports/out_imports_2019-10-31.csv'
#151690 valor_del_flete no trailing characters ,700.95 './00_data/out/imports/out_imports_2019-10-31.csv'
#185851 valor_del_flete no trailing characters ,368.78 './00_data/out/imports/out_imports_2019-10-31.csv'
#row             col               expected  actual                                               file
#114771 valor_del_flete no trailing characters ,624.61 './00_data/out/imports/out_imports_2019-11-30.csv'
#64521 valor_del_flete no trailing characters ,098.04 './00_data/out/imports/out_imports_2020-01-31.csv'

# git config --global user.email "gordon.erick@gmail.com"
# git config --global user.name "ErickGB"

/*
INSERT INTO trade.dim_country
select GENERATE_UUID(), 'SC', 'SYC', 'ISO 3166-2:SC', '690', 'Seychelles', 'Africa', 'Sub-Saharan Africa', '002', '202', -4.325, 55.371, 1, NULL 

INSERT INTO trade.dim_country
select GENERATE_UUID(), 'SK', 'SKV', 'ISO 3166-2:SK', '703', 'Slovakia', 'Europe', 'Eastern Europe', '150', '151', 48.72, 19.347, 1, NULL 

INSERT INTO trade.dim_country
select GENERATE_UUID(), 'SB', 'SLB', 'ISO-3166', '677', 'Solomon Islands', 'Oceania', 'Australia and New Zealand', '009', '053', -8.309090, 159.382619, 1, NULL 

select GENERATE_UUID(), 'LY', 'LYB', 'ISO-3166', '218', 'Libia', 'Africa', 'Northern Africa', '002', '015', 2.379440, 37.5088900, 1, NULL 

INSERT INTO trade.dim_country
select GENERATE_UUID(), 'SS', 'SSD', 'ISO-3166', '728', 'South Sudan', 'Africa', 'East Africa', '002', '??', 6.52340, 30.62477, 1, cast('2099-12-31' as date) 	
	

*/

	select count(*) from data_test.staging_imports im 
inner join trade.dim_category ca on ca.sub_category_code = substr(tariff_fraction, 1, 2)
inner join trade.dim_country co on co.alpha_2 = country_origin_code 
where input_date > cast('2020-09-01' as date) 	
	
insert into trade.fact_import
select GENERATE_UUID(), ca.category_id, co.country_id, 
cast(concat(substr(cast(im.date as string), 1, 4), substr(cast(im.date as string), 6, 2), substr(cast(im.date as string), 9, 2)) as INT64), 
link, date, RUC, company, country_origin_code, 
description, key, original_text, quantity_text, 
tariff_fraction, import_taxes, oil_protection_taxes, isc_taxes, itbms_taxes, 
gross_weight_text, net_weight_text, port, total_to_pay, cif, freight_value, insurance, fob, quantity, gross_weight, net_weight,
day, month, cast(extract(year from im.date) as int64), country_data, year_month_date, 
cast(concat(substr(cast(im.date as string), 1, 8),'01') as date), 7 # IMPORTANT!!
from data_test.staging_imports im 
inner join trade.dim_category ca on ca.sub_category_code = substr(tariff_fraction, 1, 2)
inner join trade.dim_country co on co.alpha_2 = country_origin_code 
where input_date > cast('2020-09-01' as date) # yyyy-mm-dd
# 126554 - 126554 - 126554 - 122906 (3 months)

insert into trade.fact_agg_product
select co. alpha_2 , co.name, region, sub_region, latitude, longitude, company, RUC, category_code, 
sub_category_code, category, sub_category, --port, 
count(*), sum(total_to_pay), sum(cif), sum(freight_value), sum(insurance), 
sum(fob), sum(quantity), sum(gross_weight), sum(net_weight), 0, 0, 
month, year, year_month_date, year_date
from trade.fact_import fi
inner join trade.dim_category ca on  fi.category_id = ca.category_id
inner join trade.dim_country co on  fi.country_id = co.country_id
where -- record_id = 8 # IMPORTANT!!
group by co. alpha_2 , co.name, region, sub_region, latitude, longitude, company, RUC, category_code, 
sub_category_code, category, sub_category,month, year, year_month_date, year_date 






# googledrive authentication Sys.setlocale(locale="es_ES.UTF-8")
# "rowsums", "data_test", table = "staging_trades"
#drive_auth(use_oob = TRUE)
#drive_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
#					 email = gargle::gargle_oauth_email(),
#					 cache = gargle::gargle_oauth_cache(),
#					 use_oob = gargle::gargle_oob_default()
#					 )
# now carry on with your work


#Sys.setlocale(locale="es_ES.UTF-8")
#OlsonNames()
# sudo locale-gen es_ES.UTF-8 .. in terminal
#as.Date('01-ago-2019',  tryFormats = c("%d-%m-%Y", "%d-%b-%Y"), tz="America/Panama") # not working


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


