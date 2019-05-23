cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(purrr)     # Functional programming
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(XML)
# ***********************************************
PATH_OUT <- "./00_data/out/salaries/"
date_time <- as.character(Sys.Date())

# css: http://www.css.gob.pa/p/grid_defensoria/
# canal de panama: http://www.defensoriadelpueblo.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=26
# up: http://consulta.up.ac.pa/PortalUp/planilla.aspx
# utp: http://www.utp.ac.pa/planilla-de-la-utp

# unachi excel: http://www.unachi.ac.pa/transparencia
# ifaruh pdf: https://www.ifarhu.gob.pa/transparencia/11-3-planillas/ 
# pandeportes, pdf.
# tocumenn: http://tocumenpanama.aero/index.php/planilla?find=all


# functions ----
get_employees <- function(codigo) {
	#codigo <- '001'
	url <- 'http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp'
	session <- html_session(url)
  pgform <- html_form(session)[[1]]
  pgform <- set_values(pgform, institucion = codigo)
  pgform$fields[[1]]$value <- codigo
  
  # take last updated 
  update_data <-  session %>% 
  	rvest::html_nodes(xpath = '//p') 
	update <- update_data[9] %>% 
		html_text()
	update <- gsub("\r\n", "", update)
	update <- gsub("\"", "", update)
	update <- gsub("Fecha de Actualización de los Datos :", "", update)
	update <-stringr::str_trim(update, side = "right")
	update <-stringr::str_trim(update, side = "left")
  
  
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
		 primer_nombre = ifelse(primer_nombre == "", nombre, primer_nombre),
		 primer_nombre = stringr::str_trim(primer_nombre, side = "right"),
		 primer_nombre = stringr::str_trim(primer_nombre, side = "left"),		
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
		 fecha_inicio = substr(fecha_inicio, 1, 10),
				
		 salario = as.numeric(salario),
		 gasto = as.numeric(gasto),
		 total = salario + gasto,
		 last_update = update
		)	
	return (final_tbl)
}


# ***********************************************
# load data ----
# Starting web scraping
# ***********************************************

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

# create table 
entities_tbl <- tibble(
	codigo = options_values, 
	entidad = text_values
	)
entities_tbl$url <- url

# ********************************************************************
# PROCESSED IN PARALLEL with furrr (5 minutes) ----
plan("multiprocess")
employee_salaries_tbl <- entities_tbl %>%
	  filter(codigo != '000') %>% 
    mutate(features = future_map(codigo, get_employees))

#get_employees(url, '001')
final_tbl <- employee_salaries_tbl %>% 
  unnest()
final_tbl$record_date <- 	Sys.time()
nrow(final_tbl)

# ********************************************************************
# performs sex estimation by name ----
names_tbl <- readr::read_csv("./00_Data/in/names/namesComplete2016.csv")
names_tbl$X1 <- NULL  
names_tbl <- names_tbl %>% 
	group_by(firstname, sex) %>% 
	summarize(total = sum(count)) %>%
	ungroup() %>% 
	mutate(
			firstname = toupper(firstname), 
			sex = ifelse(sex == "F", "MUJER", "HOMBRE")
	)
	
get_sex_by_name <- function(name) 
{
	sex <- names_tbl %>% 
		filter(firstname == toupper(name)) %>% 
		arrange(desc(total)) %>% 
		dplyr::select(sex) %>% 
		head(1) %>% 
		as.character()
	
	sex <- ifelse(nchar(sex) > 6, "X", sex)
	return (sex)
}

final_tbl <- final_tbl %>% 
	mutate(
		fecha_inicio = as.Date(fecha_inicio, format = "%d/%m/%Y"),
		last_update = as.Date(last_update, format = "%d/%m/%Y"), 
		sex = sapply(primer_nombre, function(x) get_sex_by_name(x))
		)

final_tbl %>% 
	glimpse()

master_tbl <- final_tbl %>% 
	rename(
		code = codigo, complete_name = nombre, last_name = apellido, person_id = cedula, 
		position = cargo, salary = salario, expenses = gasto, total_income = total, status = estado, 
		start_date = fecha_inicio, first_name = primer_nombre, entity = entidad, update_date = last_update
		) %>%	dplyr::select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
		start_date, first_name, entity, update_date, sex) 
nrow(master_tbl)
write.csv(master_tbl, paste0(PATH_OUT, "central_gov_salaries.csv"), row.names = FALSE) 
table(master_tbl$update_date)
rm(master_tbl)

# ********************************************************************
# write to disk 3 files ----

# processing data
write.csv(final_tbl, paste0(PATH_OUT, "out_centralgov_salaries.csv"), row.names = FALSE) 

# People
people_one <- final_tbl %>% 
	dplyr::select(nombre, apellido, cedula, sex, fecha_inicio, record_date)
write.csv(people_one, paste0(PATH_OUT, "out_people.csv"), row.names = FALSE) 

# Entities
entities_tbl <- final_tbl %>% 
	count(entidad, cargo, record_date)
write.csv(entities_tbl, paste0(PATH_OUT, "out_entities.csv"), row.names = FALSE) 


