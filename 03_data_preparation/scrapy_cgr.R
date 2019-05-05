cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(XML)
# ***********************************************
PATH_OUT <- "./00_data/out/salaries/"

# functions ----
get_employees <- function(codigo) {
	#codigo <- '001'
	url <- 'http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp'
	session <- html_session(url)
  pgform <- html_form(session)[[1]]
  pgform <- set_values(pgform, institucion = codigo)
  pgform$fields[[1]]$value <- codigo
	result <- submit_form(session, pgform, submit = NULL, httr::add_headers('x-requested-with' = 'XMLHttpRequest'))
	rows <-  result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr')
	print(length(rows))

	first_name <- result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr') %>% 
		.[3:length(rows)] %>% 
		#rvest::html_table()
		rvest::html_nodes(xpath = 'td[1]') %>% 
		rvest::html_text()
	
	last_name <- result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr') %>% 
		.[3:length(rows)] %>% 
		#rvest::html_table()
		rvest::html_nodes(xpath = 'td[2]') %>% 
		rvest::html_text()
	
	personal_id <- result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr') %>% 
		.[3:length(rows)] %>% 
		#rvest::html_table()
		rvest::html_nodes(xpath = 'td[3]') %>% 
		rvest::html_text()
	
	job <- result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr') %>% 
		.[3:length(rows)] %>% 
		#rvest::html_table()
		rvest::html_nodes(xpath = 'td[4]') %>% 
		rvest::html_text()
	
	salary <- result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr') %>% 
		.[3:length(rows)] %>% 
		#rvest::html_table()
		rvest::html_nodes(xpath = 'td[5]') %>% 
		rvest::html_text()
	
	other <- result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr') %>% 
		.[3:length(rows)] %>% 
		#rvest::html_table()
		rvest::html_nodes(xpath = 'td[6]') %>% 
		rvest::html_text()
	
	status <- result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr') %>% 
		.[3:length(rows)] %>% 
		#rvest::html_table()
		rvest::html_nodes(xpath = 'td[7]') %>% 
		rvest::html_text()
	
	date_in <- result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr') %>% 
		.[3:length(rows)] %>% 
		rvest::html_nodes(xpath = 'td[8]') %>% 
		rvest::html_text()
		
	final_tbl <- tibble(
		nombre = first_name, 
		apellido = last_name, 
		cedula = personal_id,
		cargo = job,
		salario = salary,
		gasto = other,
		estado = status,
		fecha_inicio = date_in
		)  %>% 
	mutate(
		 nombre = gsub("\r\n", "", nombre),
		 nombre = gsub("\"", "", nombre),
		 primer_nombre = sapply(nombre, function(x) substr(x, 1, gregexpr(pattern =" ", x)[[1]][1] - 1)),
		 nombre = stringr::str_trim(nombre, side = "right"),
		nombre = stringr::str_trim(nombre, side = "left"),
		 apellido = gsub("\r\n", "", apellido),
		 apellido = gsub(" ", "", apellido),
		 cedula = gsub("\r\n", "", cedula), 
		 cedula = gsub(" ", "", cedula), 
		 salario = gsub("\r\n", "", salario), 
		 salario = gsub(" ", "", salario), 
		 salario = gsub(",", "", salario), 
		 gasto = gsub("\r\n", "", gasto), 
		 gasto = gsub(" ", "", gasto), 
		 gasto = gsub(",", "", gasto), 
		
		 cargo = gsub("\r\n", "", cargo), 
		 cargo = stringr::str_trim(cargo, side = "right"),
		 cargo = stringr::str_trim(cargo, side = "left"),
		 estado = gsub("\r\n", "", estado), 
		 estado = gsub(" ", "", estado), 
		 fecha_inicio = gsub("\r\n", "", fecha_inicio), 
		 fecha_inicio = gsub(" ", "", fecha_inicio), 
				
		 salario = as.numeric(salario),
		 gasto = as.numeric(gasto),
		 total = salario + gasto
		)	
	return (final_tbl)
}

# ***********************************************
# load data ----
url <- 'http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp'
#xopen(url)

html <- read_html(url)
#pgform #pgform$fields

options_values <- html %>% 
	rvest::html_nodes('[name="institucion"]') %>%
	rvest::html_nodes('option') %>% 
	rvest::html_attr("value") 

text_values <- html %>% 
	rvest::html_nodes('[name="institucion"]') %>%
	rvest::html_nodes('option') %>% 
	rvest::html_text() 

entities_tbl <- tibble(
	codigo = options_values, 
	entidad = text_values
	)
entities_tbl$url <- url

# PROCESSED IN PARALLEL with furrr (5 minutes)
plan("multiprocess")
employee_salaries_tbl <- entities_tbl %>%
	  filter(codigo != '000') %>% 
    mutate(features = future_map(codigo, get_employees))

#get_employees(url, '001')
final_tbl <- employee_salaries_tbl %>% 
  unnest()
	
final_tbl %>% 
	glimpse()

final_tbl %>% 
	head()
table(final_tbl$entidad)

nrow(final_tbl)
write.csv(final_tbl, paste0(PATH_OUT, "out_centralgov_salaries_may2019.csv")) 


salaries_tbl <- read.csv(paste0(PATH_OUT, "out_centralgov_salaries.csv"))
nrow(salaries_tbl)

