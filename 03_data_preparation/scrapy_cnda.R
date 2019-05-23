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
# ***********************************************
PATH_OUT <- "./00_data/out/importations/"
date_time <- as.character(Sys.Date())

#157770 / 99
# importación: http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-01-01&calendario_hasta=2019-05-16&ruc=&importador=&tipo_oper=I&puerto=&arancel=&mercancia=&cantresxpag=99&pag=formprin&Accion_Consultar=Consultar
# exportación: http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-01-01&calendario_hasta=2019-05-16&ruc=&importador=&tipo_oper=E&puerto=&arancel=&mercancia=&cantresxpag=99&pag=formprin&Accion_Consultar=Consultar
url <- "http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-04-01&calendario_hasta=2019-04-30&ruc=&importador=&tipo_oper=I&puerto=&arancel=&mercancia=&cantresxpag=199&pag=formprin&Accion_Consultar=Consultar&np=0"
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
	  substr(2, 100) 



data_tbl <- tibble(
	date = date_var, 
	#ruc = ruc_var,
	company = company_var,
	source = source_var, 
	description = description_var
	) %>% 
	mutate(key = as.numeric(rownames(.)) )

data_tbl %>% 
	glimpse()

dim(data_tbl)

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

variables_tbl %>% 
	glimpse()

table(variables_tbl$key)

# ***********************************************************
# join tables ----


