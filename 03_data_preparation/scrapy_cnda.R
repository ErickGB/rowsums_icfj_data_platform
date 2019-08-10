cat("\014")
#install.packages("V8")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(V8)				 # call javascript functions
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(XML)
library(stringr) 
# ***********************************************
PATH_OUT <- "./00_data/out/importations/"
date_time <- as.character(Sys.Date())

#http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-02-01&calendario_hasta=2019-02-28&ruc=&importador=&tipo_oper=I&puerto=&arancel=&mercancia=&cantresxpag=99&pag=formprin&Accion_Consultar=Consultar
#http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-02-01&calendario_hasta=2019-02-28&ruc=&importador=&tipo_oper=I&puerto=&arancel=&mercancia=&cantresxpag=99&pag=formprin&Accion_Consultar=Consultar&np=9
#http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-02-01&calendario_hasta=2019-02-28&ruc=&importador=&tipo_oper=I&puerto=&arancel=&mercancia=&cantresxpag=99&pag=formprin&Accion_Consultar=Consultar&np=12

#157770 / 2000
# importación: http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-01-01&calendario_hasta=2019-05-16&ruc=&importador=&tipo_oper=I&puerto=&arancel=&mercancia=&cantresxpag=99&pag=formprin&Accion_Consultar=Consultar
# exportación: http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-01-01&calendario_hasta=2019-05-16&ruc=&importador=&tipo_oper=E&puerto=&arancel=&mercancia=&cantresxpag=99&pag=formprin&Accion_Consultar=Consultar
url <- "http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-01-01&calendario_hasta=2019-01-31&ruc=&importador=&tipo_oper=I&puerto=&arancel=&mercancia=&cantresxpag=55708&pag=formprin&Accion_Consultar=Consultar&np=2"
session <- html_session(url)


# date, master table
date_var <- session %>% 
		rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
	  rvest::html_nodes(css = 'table.TablaDato') %>% 
	  rvest::html_nodes('tr') %>% 
	  rvest::html_nodes(css = 'td.colFecha') %>% 
	  rvest::html_text()

# RUC 
ruc_var <- session %>% 
		rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
	  rvest::html_nodes(css = 'table.TablaDato') %>% 
	  rvest::html_nodes('tr') %>% 
	  rvest::html_nodes(xpath = 'td[2]') %>% 
	  rvest::html_nodes('span') %>% 
	  rvest::html_text()

# RUC name company
company_var <- session %>% 
		rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
	  rvest::html_nodes(css = 'table.TablaDato') %>% 
	  rvest::html_nodes('tr') %>% 
	  rvest::html_nodes(xpath = 'td[contains(@valign,"top")]') %>% 
	  rvest::html_text() %>% 
	  str_remove_all("\n") %>%
    str_remove_all("\r") %>%
	  str_replace_all("\t\t\t", ";") %>% 
	  str_remove_all("\t")

# procedencia *OK*
source_var <- session %>% 
		rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
	  rvest::html_nodes(css = 'table.TablaDato') %>% 
	  rvest::html_nodes('tr') %>% 
	  rvest::html_nodes(xpath = 'td[3]') %>% 
	  rvest::html_nodes('span') %>% 
	  rvest::html_text()

# descripción de la mercancia
description_var <- session %>% 
		rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
	  rvest::html_nodes(css = 'table.TablaDato') %>% 
	  rvest::html_nodes('tr') %>% 
	  rvest::html_nodes(xpath = 'td[3]') %>% 
	  rvest::html_text() %>% 
	  str_remove_all("\n") %>%
    str_remove_all("\r") %>%
	  str_replace_all("\t\t\t", ";") %>% 
	  str_remove_all("\t") %>% 
	  substr(2, nchar(.)) 



data_tbl <- tibble(
	date = date_var, 
	company = company_var,
	source = source_var, 
	description = description_var
	) %>% 
	mutate(key = as.numeric(rownames(.)) )

data_tbl$description[190]

data_tbl <- data_tbl %>% 
	mutate(
		company = substr(company, 27, nchar(company)),
		RUC = purrr::map_chr(company, function(x) {stringr::str_split(x, ";")[[1]][1]}),
		company = purrr::map_chr(company, function(x) {stringr::str_split(x, ";")[[1]][2]}),
		origin = substr(source, nchar(source) - 1, nchar(source)),
		text_original = description,
		description = stringr::str_trim(substr(description, 17, nchar(description)), side = 'both'),
		description = stringr::str_replace(description, "Mostrar información", " ;"),
		description = purrr::map_chr(description, function(x) {
			position = (as.numeric(str_locate(x, ";")[1]) - 1)
			str_dat <- ifelse(is.na(substr(x, 1, position)) == TRUE, x, substr(x, 1, position))
			return (str_dat)
			}
			),
		source = NULL
		) %>% 
	select(date, RUC, company, origin, description, key, text_original)
#View(data_tbl)

data_tbl %>% 
	glimpse()

dim(data_tbl)

data_tbl %>% 
	head()

# ***********************************************************
# detail table ----

# fields (14)
fields_var <- session %>% 
		rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
	  rvest::html_nodes(css = 'table.TablaDato') %>% 
	  rvest::html_nodes('tr') %>% 
	  rvest::html_nodes(css = 'td.dataRUC_label') %>% 
	  rvest::html_text()

# data (14)
data_var <- session %>% 
		rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
	  rvest::html_nodes(css = 'table.TablaDato') %>% 
	  rvest::html_nodes('tr') %>% 
	  rvest::html_nodes(css = 'td.dataRUC') %>% 
	  rvest::html_text()

variables_tbl <- tibble(
	fields = fields_var,
	data = data_var) %>% 
	mutate(key = floor((as.numeric(rownames(.)) -1) /14) + 1)

variables_tbl <- variables_tbl %>% 
	mutate(
		data_processed = str_replace(data, "B/. ", ""),
		data_processed = str_replace(data_processed, ",", ""),
		data_processed = str_replace(data_processed, " Kg", ""),
		data_processed = str_replace(data_processed, " Unidad", ""),
		fields = ifelse(fields == '"Peso Neto: "', 'Peso Neto:', fields)
		)

variables_tbl %>% 
	glimpse()



# ***********************************************************
# join tables ----
data_tbl$text_original <- NULL 
data_final_tbl <- inner_join(data_tbl, variables_tbl, by = 'key')

table(variables_tbl$fields)

data_final_tbl %>% 
	glimpse()

data_final_tbl %>% 
	head()

table(data_final_tbl$date)

data_final_tbl %>% 
	filter(fields == 'Total a Pagar:') %>% 
	group_by(RUC, company) %>% 
	summarize(total = sum(as.numeric(data_processed))) %>% 
	arrange(desc(total))

data_final_tbl %>% 
	filter(stringr::str_trim(RUC) == '65219-68-360495') %>%
	filter(fields == 'Total a Pagar:') %>% 
	group_by(description) %>% 
	summarize(n = n(), total = sum(as.numeric(data_processed))) %>% 
	arrange(desc(total))

data_final_tbl$ddate <- as.Date(data_final_tbl$date, tryFormats = c("%d-%M-%Y"))

write.csv(data_final_tbl, #fileEncoding = "UTF-8",
	paste0(PATH_OUT, "out_cnda_3.csv"), row.names = FALSE)

# ****************************

reg01_tbl <- readr::read_csv(paste0(PATH_OUT, "out_cnda.csv"))
reg02_tbl <- readr::read_csv(paste0(PATH_OUT, "out_cnda_2.csv"))
reg03_tbl <- readr::read_csv(paste0(PATH_OUT, "out_cnda_3.csv"))

reg01_tbl$text_original <- NULL 

data_tbl <- rbind(reg01_tbl, reg02_tbl)
data_tbl <- rbind(data_tbl, reg03_tbl)

data_tbl <- data_tbl %>% 
	mutate(
		fields = stringr::str_remove(fields, ":"),
		fields = stringr::str_replace(fields, " ", "_"),
		data = NULL 
		)


data_tbl <- data_tbl %>% 
  group_by(fields) %>% 
  mutate(grouped_id = row_number())

data_tbl <- data_tbl %>% 
  spread(key = fields, value = data_processed) %>% 
  select(-grouped_id)

data_tbl <- data_tbl %>%  
	janitor::clean_names()

data_tbl <- data_tbl %>%  
	mutate(
		impuestos_de_importacion = as.numeric(impuestos_de_importacion), 
		impuestos_de_proteccion_de_petroleo = as.numeric(impuestos_de_proteccion_de_petroleo), 
		impuestos_isc = as.numeric(impuestos_isc), 
		impuestos_itbm = as.numeric(impuestos_itbm), 
		peso_bruto = as.numeric(peso_bruto), 
		puerto_de_entrada = as.numeric(peso_bruto), 
		total_a_pagar = as.numeric(total_a_pagar),
		valor_cif  = as.numeric(valor_cif ),
		valor_del_flete = as.numeric(valor_del_flete),
		valor_del_seguro = as.numeric(valor_del_seguro),
		valor_fob = as.numeric(valor_fob)
		)



data_tbl %>% 
	head()

data_tbl %>% 
	glimpse()

data_tbl %>% 
	DataExplorer::plot_missing()



table(data_tbl$date)
table(data_tbl$fields)

data_tbl %>% 
	head()

write.csv(data_tbl, #fileEncoding = "UTF-8",
	paste0(PATH_OUT, "out_cnda_totalv2.csv"), row.names = FALSE)



#data_tbl <- readr::read_csv(paste0(PATH_OUT, "out_cnda_totalv2.csv"))
#data_tbl %>% 
#	glimpse()

#summary <- data_tbl %>% 
#	filter(stringr::str_detect(stringr::str_to_lower(description), 'cal hidratada'))
#View(summary)
		
		

		
		
		
		
		
		
		
		


