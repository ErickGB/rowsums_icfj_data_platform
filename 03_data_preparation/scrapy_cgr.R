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
actual_month <- "jul" # 
last_update <- as.Date('2019-08-01')
# ***********************************************
# functions ----
get_employees <- function(codigo) {
	print(codigo)
	url <- 'http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp'
	session <- html_session(url)
  pgform <- html_form(session)[[1]]
  pgform <- set_values(pgform, institucion = codigo)
  pgform$fields[[1]]$value <- codigo
  
	result <- submit_form(session, pgform, submit = NULL, httr::add_headers('x-requested-with' = 'XMLHttpRequest'))
	rows <-  result %>% 
		rvest::html_nodes(xpath = '//form/table[2]') %>% 
		rvest::html_nodes('tr')

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
		) 
	#final_tbl$last_update = update
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
entities_tbl

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

# ********************************************************************
# PROCESSED IN PARALLEL with furrr (5 minutes) ----
#get_employees(url, '001')

plan("multiprocess")
codes <- c('007', '018', '012', '000', '045')
codes <- c('000', '045')
employee_salaries_tbl <- entities_tbl %>%
	  filter(!(codigo %in% codes)) %>% 
    mutate(features = future_map(codigo, get_employees))

final_tbl <- employee_salaries_tbl %>% 
  unnest()


final_tbl <- final_tbl  %>% 
	mutate(
		 last_update = update,
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
		 total = salario + gasto
		)	

final_tbl$last_update <- last_update
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

# create data for Tableau month dashboard : central_gov_salaries
master_tbl <- final_tbl %>% 
	rename(
		code = codigo, complete_name = nombre, last_name = apellido, person_id = cedula, 
		position = cargo, salary = salario, expenses = gasto, total_income = total, status = estado, 
		start_date = fecha_inicio, first_name = primer_nombre, entity = entidad, update_date = last_update
		) %>%	dplyr::select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
		start_date, first_name, entity, update_date, sex) 
nrow(master_tbl)
write.csv(master_tbl, paste0(PATH_OUT, "central_gov_salaries_jul.csv"), row.names = FALSE) 
table(master_tbl$update_date)

# gov_salaries_jul_tbl <- readr::read_csv(paste0(PATH_OUT, "central_gov_salaries_jul.csv"))
gov_salaries_jun_tbl <- master_tbl %>%
	rename(
		codigo = code, entidad = entity, nombre = complete_name, 
		apellido = last_name, cedula = person_id, cargo = position, 
		salario = salary, gasto = expenses, estado = status, 
		fecha_inicio = start_date, primer_nombre = first_name, 
		total = total_income, last_update = update_date
		) %>% 
	mutate(
		url = "http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp",
		record_date = date_time
		) %>% 
	select(
		codigo, entidad, url, nombre, apellido, cedula, cargo, salario, gasto, estado, 
		fecha_inicio, primer_nombre, total, last_update, record_date, sex
		) %>% 
	mutate(
		cedula = stringr::str_trim(as.character(cedula), side = "both"),
		nombre = stringr::str_trim(as.character(nombre), side = "both"),
		apellido = stringr::str_trim(as.character(apellido), side = "both"),
		cargo = stringr::str_replace(stringr::str_trim(as.character(cargo), side = "both"), " ", "_"),
		entidad = stringr::str_trim(as.character(entidad), side = "both"),
		key = paste(cedula, as.character(fecha_inicio), cargo, sep = "_")
		)
rm(master_tbl)

gov_salaries_jun_tbl %>% 
	glimpse()

write.csv(gov_salaries_jun_tbl, paste0(PATH_OUT, "f_salary_jul.csv"), row.names = FALSE) 


# ********************************************************************
# write to disk 3 files ----

# base con cédula, nom, ape, cargo, fecha inicio
# busca la mínima fecha de inicio
# filtra la base, solo quedan aquellos con minima fecha de inicio.. no importa el cargo.
# listado de empleados únicos. Evita que se repitan dos personas con nombres diferentes pero la misma cédula
get_record <- function(id) {
	record_tbl <- final_tbl %>% 
		filter(cedula == id) %>% 
		select(cedula, nombre, apellido, fecha_inicio, sex) %>% 
		arrange(desc(fecha_inicio)) %>% head(1)
	return (record_tbl)
	}

out_tbl <- final_tbl %>% 
	distinct(cedula) 

people_tbl <- bind_rows(purrr::map_df(out_tbl$cedula, .f = function(x) {get_record(x)}))

people_tbl <- people_tbl %>% 
	mutate(
		people_id = as.integer(rownames(.))
		) %>% 
	select(people_id, cedula, nombre, apellido, fecha_inicio, sex)

write.csv(people_tbl, paste0(PATH_OUT, "out_people.csv"), row.names = FALSE) 
# **************
# Entities ----
entities_n_tbl <- final_tbl %>% 
	count(entidad, cargo, record_date) %>% 
	rename(cantidad = n) %>% 
	mutate(key = paste(entidad, cargo, sep = '_')) %>% 
	mutate(cantidad_anterior = ifelse(key == lag(key), lag(cantidad), NA)) %>% 
	mutate(key = NULL)

tipos_cantidad <- c('cantidad', 'cantidad_anterior', 'cantidad_diferencia')
entities_sum_tbl <- final_tbl %>% 
	group_by(codigo, entidad, cargo, record_date) %>% 
	summarize(total = sum(total), salario = sum(salario), gasto = sum(gasto), cantidad = n()) %>% 
	mutate(key = paste(entidad, cargo, sep = '_')) %>% 
	mutate(
		cantidad_anterior = ifelse(key == lag(key), lag(cantidad), NA),
		total_anterior = ifelse(key == lag(key), lag(total), NA),
		cantidad_diferencia = cantidad - cantidad_anterior,
		total_diferencia = round(as.numeric(total - total_anterior), digits = 0)
		) %>% 
	mutate(key = NULL) %>% 
	gather(key, value, 5:ncol(.)) %>% 
	mutate(
		tipo = ifelse(key %in% tipos_cantidad, "cantidad", "salarios")
		)
	
entities_sum_tbl <- entities_sum_tbl %>% 
	filter(is.na(value) == FALSE)

entities_sum_tbl %>% 
	filter(value < 0)

table(entities_sum_tbl$key)

write.csv(entities_tbl, paste0(PATH_OUT, "out_entities.csv"), row.names = FALSE) 
write.csv(entities_sum_tbl, paste0(PATH_OUT, "out_entities_extended.csv"), row.names = FALSE) 





# ********************************************************************
# load to google storage ----

#Sys.setenv("GCS_AUTH_FILE" = "./00_scripts/rowsums-963a7bdf28fe.json") # bigquery reader
path <- paste0(getwd(), "/00_scripts/rowsums-7a419b99de2f.json")
Sys.setenv("GCS_AUTH_FILE" = path) # storage

Sys.setenv("GCS_DEFAULT_BUCKET" = "rowsums.com")
library(googleCloudStorageR)
gcs_global_bucket('rowsums.com')
gcs_get_global_bucket()

## attempt upload
big_file <- "./00_data/out/salaries/out_entities_extended.csv"
upload_try <- gcs_upload(big_file)



library(googleAuthR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/urlshortner")
service_token <- gar_auth_service(json_file=path)
analytics_url <- function(shortUrl, 
                          timespan = c("allTime", "month", "week","day","twoHours")){
  
  timespan <- match.arg(timespan)
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "GET",
                         pars_args = list(shortUrl = "shortUrl",
                                          projection = "FULL"),
                         data_parse_function = function(x) { 
                           a <- x$analytics 
                           return(a[timespan][[1]])
                         })
  
  f(pars_arguments = list(shortUrl = shortUrl))
}
analytics_url("https://goo.gl/2FcFVQbk")




# css: http://www.css.gob.pa/p/grid_defensoria/
# canal de panama: http://www.defensoriadelpueblo.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=26
# up: http://consulta.up.ac.pa/PortalUp/planilla.aspx
# utp: http://www.utp.ac.pa/planilla-de-la-utp

# unachi excel: http://www.unachi.ac.pa/transparencia
# ifaruh pdf: https://www.ifarhu.gob.pa/transparencia/11-3-planillas/ 
# pandeportes, pdf.
# tocumenn: http://tocumenpanama.aero/index.php/planilla?find=all




