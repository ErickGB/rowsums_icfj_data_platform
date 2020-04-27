cat("\014")
# ********************************************************************
# google bigquery connection ----
library(bigrquery)
library(googledrive)
library(gargle)
library(tidyverse)

library(vroom)     # 
library(anomalize)
library(tidyquant)
library(anytime) # Convert input in any one of character, integer, numeric, factor, or ordered type into 'POSIXct'
library(plotly)

PATH_OUT <- "./00_data/out/imports/"
text_search <- "SALMON"
codes_search <- c(
"0302.13.00", # 
"0302.14.00", #
"0303.12.00", #
"0303.13.00", #
"0304.41.00", #
"0304.81.00", #
"0305.44.00"
)

# ********************************************************************
httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())

bigquery_conn <- bigrquery::src_bigquery(project = "rowsums", dataset = "trade")
# List table names
src_tbls(bigquery_conn)

# 
trade_tbl <- tbl(bigquery_conn, "trade.fact_import")

# **********************************************
# companies
company_tbl <- trade_tbl %>% 
	count(RUC, company)
show_query(company_tbl)
company_tbl <- collect(company_tbl)
nrow(company_tbl) # 79,985


# get_company_name('121-261-33279')
get_company_name <- function(ruc_id) {
	text_name <- company_tbl %>% 
		ungroup() %>% 
		filter(RUC == ruc_id) %>% 
		arrange(desc(n)) %>% 
		head(1) %>% 
		dplyr::select(company) %>% 
		pull()
	
	text_name <- ifelse(is.na(text_name) == TRUE, "UNKNOW", text_name)
	return(text_name)
}
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

company_tbl <- company_tbl %>% 
	mutate(company_name = future_map(RUC, get_company_name) )

company_tbl <- company_tbl %>% 
	unnest() %>% 
	janitor::clean_names() 

company_final_tbl <- company_tbl %>% 
	ungroup() %>% 
	count(ruc, company_name) %>% 
	mutate(ruc_id = ruc) %>% 
	dplyr::select(ruc, ruc_id, company_name)

# **********************************************
# trade filtered

trade_filtered_tbl <- trade_tbl %>% 
	filter(year == 2019 ) %>% 
	group_by(RUC, country_origin_code, description, tariff_fraction, year_month_date) %>% 
	summarise(cif = round(sum(cif), 2), fob = round(sum(fob), 2) , total_to_pay = round(sum(total_to_pay), 2), 
					 freight_value = round(sum(freight_value), 2), import_taxes = round(sum(import_taxes), 2),
					 insurance = round(sum(insurance), 2), quantity = round(sum(quantity), 2), 
					 net_weight = round(sum(net_weight), 2))
show_query(trade_filtered_tbl)
nrow(trade_filtered_tbl)

trade_filtered_1_tbl <- collect(trade_filtered_tbl)

# filter
trade_serch_tbl <- trade_filtered_1_tbl %>% 
	filter(tariff_fraction %in% codes_search) # stringr::str_detect(description, text_search)  == TRUE 

trade_serch_2_tbl <- trade_filtered_1_tbl %>% 
	filter(tariff_fraction %in% codes_search) # stringr::str_detect(description, "SALMÓN")  == TRUE &

trade_serch_tbl <- rbind(trade_serch_tbl, trade_serch_2_tbl)
trade_serch_tbl$type <- "imports" 
trade_serch_tbl <- trade_serch_tbl %>% 
	janitor::clean_names()

trade_serch_tbl <- left_join(trade_serch_tbl, company_final_tbl, by = 'ruc')


trade_serch_tbl %>% 
	DataExplorer::plot_missing()

trade_serch_tbl %>% 
	glimpse()


View(trade_serch_tbl)
nrow(trade_serch_tbl)

descriptions_tbl <- trade_serch_tbl %>% 
	ungroup() %>% 
	count(tariff_fraction, description) %>% 
	arrange(desc(n))


file_type <- "salmon_descriptions_v2"
write.csv(descriptions_tbl, #fileEncoding = "UTF-8",
					paste0(PATH_OUT, "out_", file_type,".csv"), row.names = FALSE)

file_type <- "trade_salmon"
write.csv(trade_serch_tbl, #fileEncoding = "UTF-8",
					paste0(PATH_OUT, "out_", file_type,".csv"), row.names = FALSE)


# **********************************************
# exports
export_jun_tbl <- readr::read_csv(paste0(PATH_OUT, "out_exports_2019-06-30.csv"))
export_jun_tbl <- export_jun_tbl %>% 
	janitor::clean_names()


export_dic_tbl <- readr::read_csv(paste0(PATH_OUT, "out_exports_2019-12-31.csv"))
export_dic_tbl <- export_jun_tbl %>% 
	janitor::clean_names()

export_dic_tbl <- rbind(export_dic_tbl, export_jun_tbl)

export_dic_tbl %>% 
	glimpse()
#sapply(primer_nombre, function(x) get_sex_by_name(x))
#write.csv(employee_tbl, paste0(PATH_OUT, "min_gov_jan.csv"), row.names = FALSE)


exports_tbl <- 	export_dic_tbl %>% 
	mutate(
		mmonth = purrr::map_chr(tolower(substr(date, 4, 6)), get_month_id),
		date = paste(substr(date, 1, 2), mmonth, substr(date, 8, 11), sep='-'),
		date = as.Date(date,  tryFormats = c("%d-%m-%Y", "%d-%b-%Y")), 
		year_month_date	= as.Date(paste0(lubridate::year(date), "-", lubridate::month(date), "-", '01'), tryFormats = c("%Y-%m-%d", "%d-%b-%Y")),
	) %>% 
	rename(
		country_origin_code = origin,
		original_text = text_original,
		quantity_text = cantidad, 
		tariff_fraction = fraccion_arancelaria,
		import_taxes = impuestos_de_exportacion,
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
	) %>% 
	group_by(ruc, country_origin_code, description, tariff_fraction, year_month_date) %>% 
	summarise(cif = round(sum(cif), 2), fob = round(sum(fob), 2) , total_to_pay = round(sum(total_to_pay), 2), 
						freight_value = round(sum(freight_value), 2), import_taxes = round(sum(import_taxes)),
						insurance = round(sum(insurance), 2), quantity = round(sum(quantity), 2), 
						net_weight = round(sum(net_weight), 2)) 

nrow(exports_tbl)

exports_filtered_tbl <- exports_tbl %>% 
	filter(tariff_fraction %in% codes_search) %>% 
	mutate(type = 'export')

exports_filtered_tbl

exports_filtered_tbl <- left_join(exports_filtered_tbl, company_final_tbl, by = 'ruc')

exports_filtered_tbl %>% 
	DataExplorer::plot_missing()

exports_filtered_tbl %>% 
	glimpse()

exports_filtered_tbl <- exports_filtered_tbl %>% 
	ungroup()

trade_serch_tbl <- trade_serch_tbl %>% 
	ungroup()

colnames(exports_filtered_tbl)
colnames(trade_serch_tbl)



trade_serch_tbl$ruc_id <- NULL
exports_filtered_tbl$ruc_id <- NULL

final_tbl <- rbind(trade_serch_tbl, exports_filtered_tbl)

table(trade_serch_tbl$tariff_fraction)

final_tbl %>% 
	group_by(type) %>% 
	summarise(total = sum(cif))

(25786228 - 1757039)/1000000
24.02919

write.csv(final_tbl, #fileEncoding = "UTF-8",
					paste0(PATH_OUT, "out_", "final_import_export",".csv"), row.names = FALSE)


#select code, entity,  first_name, last_name, person_id, position, start_date, status, 
#salary,	expenses expense,	total_income, record_date, update_date
#from journalists.central_gov_salaries where code = '018';



#drive_auth(path = "./00_scripts/rowsums-2198b8679813.json")
#project <- "rowsums"

#projectid<-'rowsums'
#datasetid<-'journalists'
#bq_conn <-  dbConnect(bigquery(), 
#											project = projectid,
#											dataset = datasetid, 
#											use_legacy_sql = FALSE
#)
