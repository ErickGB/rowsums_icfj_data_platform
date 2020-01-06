cat("\014")
gc()
# *******************************************************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(purrr)     # Functional programming
library(fs)        # Working with File System
library(janitor)   # data wrangling 
library(xopen)     # Quickly opening URLs
library(XML)
# google big query
library(bigrquery)
# *******************************************************************************
PATH_OUT <- "./00_data/out/salaries/"    
date_time <- as.character(Sys.Date())    
actual_month <- "nov"                    # 
last_update <- as.Date('2019-11-01')     #
requiere_joins_files <- TRUE             # requiere row bindws with other extraccion
# *******************************************************************************
# functions ----
get_people_id <- function(key) {
	key_02 <- str_split(key, pattern = "-")[[1]][2]
	key_03 <- str_split(key, pattern = "-")[[1]][3]
	#paste0(substr("0000", 1, 4 - nchar(key_02)), key_02)
	#paste0(substr("00000", 1, 5 - nchar(key_03)), key_03)
	people_id <- paste0(str_split(key, pattern = "-")[[1]][1], "-", 
											paste0(substr("0000", 1, 4 - nchar(key_02)), key_02), "-",
											paste0(substr("00000", 1, 5 - nchar(key_03)), key_03))
	return(people_id)		
}

is.na(str_split("ABC", pattern = " ")[[1]][2])

get_split_value <- function (value, position) {
	split_value <- str_split(value, pattern = " ")[[1]][position]
	split_value <- ifelse(is.na(split_value) == TRUE, "", as.character(split_value))
	
	return(split_value)
}

get_employees <- function(codigo, estado) {
	print(paste0("codigo: ", codigo, " status:", estado))
  #<form> 'f_institucion' (POST Index_planillagub3.asp)
  #<select> 'institucion' [0/26]
  #<input submit> 'boton_01': Buscar
  #<input button> 'boton_02': Limpiar
  #<input text> 'nombre': 
  #	<input text> 'apellido': 
  #	<input text> 'cargo': 
  #	<select> 'estado' [0/11]
	
  final_tbl <- tibble(
  	nombre = "sin_datos", 
  	apellido = "sin_datos", 
  	cedula = "sin_datos",
  	cargo = "sin_datos",
  	salario = "0",
  	gasto = "0",
  	estado = "sin_datos",
  	fecha_inicio = as.character("03/01/2019")
  ) 
  
  tryCatch(
  {
  		#codigo <- "001"
  		#estado <- "10"
  		url <- 'http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp'
  		session <- html_session(url)
  		pgform <- html_form(session)[[1]]
  		pgform <- set_values(pgform, institucion = codigo)
  		pgform <- set_values(pgform, estado = estado)
  		pgform$fields[[1]]$value <- codigo
  		pgform$fields[[7]]$value <- estado
  		
			result <- submit_form(session, pgform, submit = NULL, httr::add_headers('x-requested-with' = 'XMLHttpRequest'))
			rows <-  result %>% 
				rvest::html_nodes(xpath = '//form/table[2]') %>% 
				rvest::html_nodes('tr')
		
		  # Are data?
			print(paste0("total:", length(rows)))
			if(length(rows) > 2) {
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
				
				temp_tbl <- tibble(
					nombre = first_name, 
					apellido = last_name, 
					cedula = personal_id,
					cargo = job,
					salario = salary,
					gasto = other,
					estado = status,
					fecha_inicio = date_in
				) 
				
				final_tbl <- rbind(final_tbl, temp_tbl)
				final_tbl <- final_tbl %>% 
					filter(nombre != "sin_datos")
				
			}
  	}, # end try
			error=function(cond) {
				#message(paste("URL does not seem to exist:", url))
				#message("Here's the original error message:")
				#message(cond)
				final_tbl$nombre[1] <- as.character(cond)
				# Choose a return value in case of error
				#return(NA)
	}, 
  warning=function(cond) {
  	final_tbl$nombre[1] <- as.character(cond)
  	#message(paste("URL caused a warning:", url))
  	#message("Here's the original warning message:")
  	#message(cond)
  	# Choose a return value in case of warning
  	#return(NULL)
  })
	
	#final_tbl$last_update = update
	return (final_tbl)
}rstudio6


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

options_status_values <- html %>% 
	rvest::html_nodes('[name="estado"]') %>%
	rvest::html_nodes('option') %>% 
	rvest::html_attr("value") 


# create table - Entities
entities_tbl <- tibble(
	codigo = options_values, 
	entidad = text_values
	)
entities_tbl$url <- url
entities_tbl
# status: 1. Permanente, Eventual, Contrato, Interino Abierto, Interino Hasta Fin de Año.... otros....
status_tbl <- tibble(status = options_status_values)

# cross join: Search  Entity by entity and status by status
entities_tbl <- crossing(entities_tbl, status_tbl)
entities_tbl <- entities_tbl %>% 
	filter(status != 0) %>% 
	filter(codigo != '000')

# take last updated 
update_data <-  html %>% 
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
#get_employees('007', '1')

plan("multiprocess")
codes <- c('007', '018', '012', '000', '045')
codes <- c('000')
employee_salaries_tbl <- entities_tbl %>%
	  #filter(!(codigo %in% codes)) %>% 
    mutate(features = future_map2(codigo, status, get_employees))

final_tbl <- employee_salaries_tbl %>% 
  unnest()

final_tbl %>% 
	glimpse()


if(requiere_joins_files) {
	# meduca 
	meduca_tbl <- readr::read_csv(paste0(PATH_OUT, "meduca_nov.csv"))
	meduca_tbl[c(20300, 20861, 31126, 38777, 42841), ] 
	meduca_tbl[c(20300, 20861, 38777), c("GASTOS")] <- 1000
	meduca_tbl[c(31126), c("GASTOS")] <- 3000
	meduca_tbl[c(42841), c("GASTOS")] <- 3500
	meduca_tbl[c(20300, 20861, 31126, 38777, 42841), ] 
	meduca_tbl[meduca_tbl$CEDULA == "8-0252-00598", c("GASTOS")] <- 3000
	meduca_tbl[meduca_tbl$CEDULA == "1-0015-00887", c("GASTOS")] <- 1000
	meduca_tbl[meduca_tbl$CEDULA == "8-0163-01149", c("GASTOS")] <- 1000
	meduca_tbl[meduca_tbl$CEDULA == "8-0195-00824", c("GASTOS")] <- 3000
	meduca_tbl[is.na(meduca_tbl$`FECHA DE INICIO`) == TRUE, 8] <- "01/12/2019"
	
	meduca_tbl <- meduca_tbl %>% 
		clean_names()
	
	meduca_tbl %>% 
		glimpse()
	
	meduca_tbl <- meduca_tbl %>% 
		mutate(
			codigo = "007",
			entidad = "Ministerio de Educación",
			url = "http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp",
			status = "-1", 
			salario = as.character(salario),
			gastos = as.character(gastos)
		) %>% 
		rename(
			nombre = nombres, 
			gasto = gastos, 
			fecha_inicio = fecha_de_inicio
		) %>% 
		select(codigo, entidad, url, status, nombre, apellido, cedula, cargo, salario, gasto, estado, fecha_inicio)
	
	final_tbl <- final_tbl %>% 
		filter(!(codigo == "007")) # borra a meduca 
	final_tbl <- rbind(final_tbl, meduca_tbl) # agrega meduca 
	
	# CSS 
	css_tbl <- readr::read_csv(paste0(PATH_OUT, "css_employees_processing.csv"))
	css_tbl <- css_tbl %>% 
		clean_names()
	
	css_tbl <- css_tbl %>% 
		mutate(
			x1 = NULL,
			person_id = map_chr(person_id, get_people_id), # standarize "cedula"
			codigo = "900",
			entidad = "Caja de Seguro Social",
			nombre =  map2_chr(complete_name, 1, get_split_value), # str_split(complete_name, pattern = " ")[[1]][1],
			apellido = map2_chr(complete_name, 2, get_split_value), # str_split(complete_name, pattern = " ")[[1]][2],
			entity = as.character(entity),
			salary = as.character(salary),
			expens = as.character(expens),
			over_costs = as.character(over_costs), 
			total = as.character(total),
			start_date = as.character(start_date)
		) %>% 
		rename(
			url = site, 
			cedula = person_id,
			cargo = job_title, 
			salario = salary, 
			gasto = expens, 
			fecha_inicio = start_date,
			estado = status
		) %>% 
		mutate(status = "-1") %>% 
		select(codigo, entidad, url, status, nombre, apellido, cedula, cargo, salario, gasto, estado, fecha_inicio, departament, over_costs)
	
	css_tbl %>% 
		glimpse()
	final_tbl$departament = "unknow"
	final_tbl$over_costs = "0"
	
	# procesar formato de cédula
	#key <- "10-21-450"
	#key <- "8-0925-00290"
	
	final_tbl <- rbind(final_tbl, css_tbl)
}

final_tbl %>% 
	glimpse()

final_tbl %>% 
	DataExplorer::plot_missing()

# data wrangling.. data cleaning.
# A tibble: 195,411 x 14
final_tbl <- final_tbl  %>% 
	filter(nombre != "sin_datos") %>% # elimina los registros vacios 
	mutate(
		departament = gsub("\r\n", "", departament),
		over_costs = gsub("\r\n", "", over_costs),
		departament = stringr::str_trim(departament, side = "both"),
		over_costs = stringr::str_trim(over_costs, side = "both"), 
		over_costs = gsub(",", "", over_costs)
	) %>% 
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
		 salario = stringr::str_replace(salario, " ", ""),
		 salario = gsub(" ", "", salario), 
		 salario = gsub(",", "", salario), 
		 salario =  stringr::str_trim(salario, side = 'both'),
		 gasto = gsub("\r\n", "", gasto), 
		 gasto = gsub(" ", "", gasto), 
		 gasto = gsub(",", "", gasto), 
		 gasto =  stringr::str_trim(gasto, side = 'both'),
		
		 cargo = gsub("\r\n", "", cargo), 
		 cargo = stringr::str_trim(cargo, side = "right"),
		 cargo = stringr::str_trim(cargo, side = "left"),
		 estado = gsub("\r\n", "", estado), 
		 estado = gsub(" ", "", estado), 
		 fecha_inicio = str_trim(fecha_inicio, side = "both"),
		 fecha_inicio = gsub("\r\n", "", fecha_inicio), 
		 fecha_inicio = gsub(" ", "", fecha_inicio), 
		 fecha_inicio = substr(fecha_inicio, 1, 10),
		 
				
		 salario = as.numeric(salario),
		 gasto = as.numeric(gasto),
		 over_costs = as.numeric(over_costs),
		 total = salario + gasto + over_costs,
		 status = NULL ,
		)	

final_tbl$last_update <- last_update
final_tbl$record_date <- 	Sys.time()
nrow(final_tbl)
(nrow(final_tbl)/253000) * 100 


final_tbl %>% 
	glimpse()

# ********************************************************************
# performs sex estimation by name ----
names_tbl <- readr::read_csv("./00_data/in/names/namesComplete2016.csv")
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
		sex = sapply(primer_nombre, function(x) get_sex_by_name(x))
	)

css_tbl <- final_tbl %>% 
	filter(codigo == "900")

css_tbl <- final_tbl %>% 
	filter(codigo == "900") %>% 
	mutate(
		fecha_inicio = as.Date(fecha_inicio, tryFormat = c("%d/%m/%Y", "%Y-%m-%d")),
		last_update = as.Date(last_update, tryFormat = c("%d/%m/%Y", "%Y-%m-%d"))
	) 

final_tbl2 <- final_tbl %>% 
	filter(codigo != "900") %>% 
	mutate(
		fecha_inicio = as.Date(fecha_inicio, tryFormat = c("%d/%m/%Y", "%Y-%m-%d")),
		last_update = as.Date(last_update, tryFormat = c("%d/%m/%Y", "%Y-%m-%d"))
		) 

css_tbl %>% 
	DataExplorer::plot_missing()
final_tbl2 %>% 
	DataExplorer::plot_missing()

final_tbl <- rbind(css_tbl, final_tbl2)

date_time <- as.Date(substr(update, 1, 10), format = "%m/%d/%Y")
final_tbl <- final_tbl %>%
		mutate(
		#url = "http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp",
		record_date = date_time,
		cedula = stringr::str_trim(as.character(cedula), side = "both"),
		nombre = stringr::str_trim(as.character(nombre), side = "both"),
		apellido = stringr::str_trim(as.character(apellido), side = "both"),
		cargo = stringr::str_replace(stringr::str_trim(as.character(cargo), side = "both"), " ", "_"),
		entidad = stringr::str_trim(as.character(entidad), side = "both"),
		key = paste(cedula, as.character(fecha_inicio), cargo, sep = "_")
		)

final_tbl %>% 
	filter(cargo == 'AGENTE_DE INSTRUCCI`N DELEGADO') %>% 
	head()

final_tbl$entidad <- ifelse(final_tbl$entidad == 'Otros Gastos de la Administracion', 
														 'Otros Gastos de la Administración', final_tbl$entidad) 

final_tbl$entidad <- ifelse(final_tbl$entidad == 'Ministerio de Comercio e Industria', 
														 'Ministerio de Comercio e Industrias', final_tbl$entidad) 

final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == 'AGENTE_DE INSTRUCCI`N DELEGADO', 'AGENTE_DE INSTRUCCIÓN DELEGADO', cargo))

final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == 'EDUCADOR_B 1 (MAESTRO CON TIT. DE MAESTRO DE ENSE%ANZA PRIM.', 
												'EDUCADOR_B 1 (MAESTRO CON TIT. DE MAESTRO DE ENSEÑANZA PRIM.', cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == 'EDUCADOR_I 3 (PROFESOR EDUC.SEC.PREP.   ESP.2 A%OS UNIV.)', 
												'EDUCADOR_I 3 (PROFESOR EDUC.SEC.PREP.   ESP.2 AÑOS UNIV.)', cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == 'EDUCADOR_I 5 (PROFESOR SEC.TIT.UNIV.TEC ING.2 A%OS UNIV.)', 
												'EDUCADOR_I 5 (PROFESOR SEC.TIT.UNIV.TEC ING.2 AÑOS UNIV.)', cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == 'EDUCADOR_F 5 (MAESTRO ESC.PRIM.2 A%OS UNIV.)', 
												'EDUCADOR_F 5 (MAESTRO ESC.PRIM.2 AÑOS UNIV.)', cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == 'JEFE_DE DISE%O', 
												'JEFE_DE DISEÑO', cargo))

#CSS
# CORREDOR_DE PRIMA DE ANTIG√É¬É√Ç¬úEDAD
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == 'CONDUCTOR_DE VEHICULO I', 
												'CONDUCTOR_DE VEHICULO   I', cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == 'CONDUCTOR_DE VEHICULO II', 
												'CONDUCTOR_DE VEHICULO  II', cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == "TECNOLOGO_EN RADIOLOG E IMÃÂGENES I II", 
												"TECNOLOGO_EN RADIOLOG E IMAGENES I II", cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == "CORREDOR_DE PRIMA DE ANTIGÃÂEDAD", 
												"CORREDOR_DE PRIMA DE ANTIGUEDAD", cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == "ALBAÃÂIL_JEFE", 
												"ALBANIL_JEFE", cargo))
final_tbl <- final_tbl %>% 
	mutate(cargo = ifelse(cargo == "ALBAÃÂIL", 
												"ALBANIL", cargo))


View(as_tibble(query_results))

final_tbl %>% 
	glimpse()


# ********************************************************************
# create data for Tableau month dashboard : central_gov_salaries ----
master_tbl <- final_tbl %>% 
	rename(
		code = codigo, complete_name = nombre, last_name = apellido, person_id = cedula, 
		position = cargo, salary = salario, expenses = gasto, total_income = total, status = estado, 
		start_date = fecha_inicio, first_name = primer_nombre, entity = entidad, update_date = last_update
		)  %>%	
	select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
									start_date, first_name, entity, update_date, sex, url, record_date, key, 
				 over_costs, departament) 

master_fail_tbl <- master_tbl %>% 
	filter(is.na(start_date) == TRUE) 

master_tbl <- master_tbl %>% 
	filter(is.na(start_date) == FALSE) 

master_tbl %>% 
	glimpse()

View(master_tbl)

nrow(master_tbl)
write.csv(master_tbl, paste0(PATH_OUT, "central_gov_salaries_", actual_month,".csv"), row.names = FALSE) 
table(master_tbl$update_date)


#master_tbl <- readr::read_csv(paste0(PATH_OUT, "central_gov_salaries_", actual_month,".csv"))
master_tbl %>% 
	glimpse()

master_tbl %>% 
	distinct(entity)

entities_tbl <- master_tbl %>% 
	group_by(code, entity) %>% 
	summarize(count = n(), amount = sum(total_income)) %>% 
	arrange(desc(count))

View(entities_tbl)

# repetidos 
person_id_list <- master_tbl %>% 
	filter(code %in% c("900", "012")) %>% 
	group_by(person_id) %>% 
	summarize(count = n(), amount = sum(total_income)) %>% 
	ungroup() %>% 
	filter(count > 1) %>% 
	arrange(desc(amount)) %>% 
	select(person_id) %>% 
	pull() %>% 
	as.character()
NROW(person_id_list) # 391 profesionales de la salud con puestos en dos entidades

master_tbl %>% 
	filter(person_id %in% c(person_id_list)) %>% 
	select(first_name, last_name, person_id, entity, total_income, position, status, start_date) %>% 
	arrange(person_id, total_income)
	
master_tbl %>% 
	filter(code %in% c("900", "012")) %>% 
	group_by(person_id) %>% 
	summarize(count = n(), amount = sum(total_income)) %>% 
	ungroup() %>% 
	filter(count > 1) %>% 
	arrange(desc(amount))

master_tbl %>% 
	filter(person_id == '8-0200-02135') %>%  # RUSBEL     BATISTA   9-0098-00959 DIRECTOR_NACIONAL... 5k + 7495 = 12k
	select(first_name, last_name, person_id, position, total_income, status, start_date, entity)

# ********************************************************************
# upload file to storage

# ********************************************************************
# upload file to database staging table


# ********************************************************************
# google bigquery connection ----
# install.packages("googledrive")
# install.packages("gargle")
library(bigrquery)
library(googledrive)
library(gargle)
get_record <- function(id) {
	record_tbl <- master_tbl %>% 
		filter(person_id == id) %>% 
		select(person_id, complete_name, last_name, start_date, sex) %>% 
		arrange(desc(start_date)) %>% head(1)
	return (record_tbl)
}

# googledrive authentication
httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())

#drive_auth(path = "./00_scripts/rowsums-2198b8679813.json")
#project <- "rowsums"

#projectid<-'rowsums'
#datasetid<-'journalists'
#bq_conn <-  dbConnect(bigquery(), 
#											project = projectid,
#											dataset = datasetid, 
#											use_legacy_sql = FALSE
#)



sql <- "select max( employee_salary_id ) as max from journalists.f_employee_salary "
count_result <- query_exec(sql, project = project, useLegacySql = FALSE)
count_result$max

names <- colnames(master_tbl)
master_tbl <- master_tbl %>% 
	mutate(employee_salary_id = as.integer(rownames(.))) %>% 
	mutate(employee_salary_id = employee_salary_id + count_result$max) %>% 
	select(employee_salary_id, names)
min(master_tbl$employee_salary_id) - count_result # 1 it's ok

# 1: Load principal table: staging_central_gov_salaries
job <- insert_upload_job("rowsums", "data_test", table = "staging_central_gov_salaries", 
	values = master_tbl, write_disposition = "WRITE_TRUNCATE")
wait_for(job)

cgs_tbl <- master_tbl
cgs_tbl$employee_salary_id <- NULL
job <- insert_upload_job("rowsums", "journalists", table = "central_gov_salaries", 
	values = cgs_tbl, write_disposition = "WRITE_TRUNCATE")
wait_for(job)

# *****************
# JOBS : new jobs? ADD MANUALLY    :(
sql <- "SELECT position, count(*) as total, avg(salary) salary 
FROM data_test.staging_central_gov_salaries where code != '900' and position not in (
  SELECT job_title FROM journalists.d_jobs
) GROUP BY position"
query_results <- query_exec(sql, project = project, useLegacySql = FALSE)
as_tibble(query_results) %>% 
	arrange(desc(total))
write.csv(as_tibble(query_results), paste0(PATH_OUT, "new_jobs", actual_month,".csv"))

# JOBS : new jobs? ADD MANUALLY    :(
sql <- "SELECT position, count(*) as total, avg(salary) salary 
FROM data_test.staging_central_gov_salaries where code = '900' and position not in (
  SELECT job_title FROM journalists.d_jobs
) GROUP BY position"
query_results <- query_exec(sql, project = project, useLegacySql = FALSE)
as_tibble(query_results) %>% 
	arrange(desc(total))
write.csv(as_tibble(query_results), paste0(PATH_OUT, "new_jobs_css", actual_month,".csv"))



sql <- "SELECT max(jobs_id) max FROM journalists.d_jobs"
count_result <- query_exec(sql, project = project, useLegacySql = FALSE)
count_result$max + 1

# add jobs manually
jobs_tbl <- readr::read_csv(paste0(PATH_OUT, "out_final_jobs.csv"))
jobs_tbl$jobs_id <- as.integer(jobs_tbl$jobs_id)
jobs_tbl[1750:1753,]
nrow(jobs_tbl)

job <- insert_upload_job("rowsums", "journalists", table = "d_jobs", 
	values = jobs_tbl, write_disposition = "WRITE_TRUNCATE")
wait_for(job)

# *****************
# PEOPLE: add new people

sql <- "SELECT max(people_id) count from journalists.d_people"
query_results <- query_exec(sql, project = project, useLegacySql = FALSE)
id <- query_results$count + 1

# new people
sql <- "SELECT person_id FROM data_test.staging_central_gov_salaries where person_id not in (select  person_id from journalists.d_people)"
new_people_tbl <- query_exec(sql, project = project, useLegacySql = FALSE)
new_people_tbl

# insert new people
people_tbl <- bind_rows(purrr::map_df(new_people_tbl$person_id, .f = function(x) {get_record(x)}))
people_tbl
table(people_tbl$sex, useNA = "always")

#why??
table(lubridate::year(people_tbl$start_date))
people_tbl %>% 
	mutate(year = lubridate::year(people_tbl$start_date)) %>% 
	filter(year < 2019) %>% 
	arrange(year)


final <- id + nrow(people_tbl) - 1
people_tbl$people_id <- seq(from = id, to = final ,by = 1)
people_tbl$people_id <- as.integer(people_tbl$people_id)
people_tbl$record_date <- last_update
people_tbl <- people_tbl %>% 
	rename(fist_name = complete_name) %>% 
	select(people_id, person_id, fist_name, last_name, start_date, sex, record_date)
#people_tbl$people_id <- as.character(people_tbl$people_id)

nrow(people_tbl)
people_tbl %>% 
	glimpse()
min(people_tbl$people_id)

# add data to final tables 
job <- insert_upload_job("rowsums", "journalists", table = "d_people", 
	values = people_tbl, write_disposition = "WRITE_APPEND")
wait_for(job)

# *****************
# DATE: date update

# actual dates 
sql <- "select record_id, record_date, processed_date, is_actual from journalists.d_date_upload"
dates_records <- query_exec(sql, project = project, useLegacySql = FALSE)
dates_records <- dates_records %>% 
	arrange(record_id)
dates_records %>% 
	glimpse()
dates_records

new_record <- dates_records[1, ]
new_record$record_id <- (max(dates_records$record_id) + 1)
new_record$record_date <- as.Date(last_update)
new_record$processed_date <-  as.Date(last_update) - as.difftime(1, unit = "days")
new_record$is_actual <- 1

dates_records$is_actual <- 0
dates_records <- rbind(dates_records, new_record) # igualar con usuarios record_date
dates_records$record_id <- as.integer(dates_records$record_id)
dates_records
job <- insert_upload_job("rowsums", "journalists", table = "d_date_upload", 
	values = dates_records, write_disposition = "WRITE_TRUNCATE")
wait_for(job)


# *****************
# F_EMPLOYEE_SALARY: finally...
sql <- "INSERT INTO journalists.f_employee_salary
SELECT c.employee_salary_id,
p.people_id, c.person_id, e.entity_id, e.entity_name, 
c.url, c.first_name, c.last_name, 
j. jobs_id , j.job_title, j.job_position, 
c.salary, c.expenses, c. total_income , c.status, c.start_date, 
cast(d.record_id as INT64) record_id, d. processed_date , d.record_date, 
c.sex, c.key,concat(c.person_id, " ", j.job_title) as key1
FROM data_test.staging_central_gov_salaries c
INNER JOIN journalists.d_people p ON p.person_id = c.person_id 
INNER JOIN journalists.d_entity e ON e.entity_code = c.code  
INNER JOIN journalists.d_jobs j ON j.job_title = c.position 
INNER JOIN journalists.d_date_upload d ON d.record_date = c.record_date"
employee_records <- query_exec(sql, project = project, useLegacySql = FALSE)

#GENERATE_UUID()
#(SHA256(Bizkey)) SurrogateKey
#tidyverse::tidyverse_update()

bigquery_conn <- bigrquery::src_bigquery(project = "rowsums", dataset = "journalists")
# List table names
src_tbls(bigquery_conn)

f_employee_salary_out <- tbl(bigquery_conn, "f_employee_salary_out")
total_tbl <- f_employee_salary_out %>% 
	group_by(date_processed, record_date) %>% 
	summarize(n = n()) 
show_query(total_tbl)







# ********************************************************************
# google bigquery connection ----
# install.packages("googledrive")
# install.packages("gargle")

library(bigrquery)
library(googledrive)
library(gargle)

# googledrive
drive_auth(path = "./00_scripts/rowsums-7a419b99de2f.json")


project <- "rowsums"
sql <- "select code, complete_name, last_name, person_id from journalists.central_gov_salaries limit 10"
query_results <- query_exec(sql, project = project, useLegacySql = FALSE)
query_results




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




# css 63M: http://www.css.gob.pa/p/grid_defensoria/
# up (15nal) 14M: http://consulta.up.ac.pa/PortalUp/planilla.aspx
# metro: https://www.elmetrodepanama.com/transparencia-3/planilla-de-funcionarios/
# antai: http://www.antai.gob.pa/11489-2/


# canal de panama: http://www.defensoriadelpueblo.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=26
# canal de panama: https://apps.pancanal.com/pls/defensoria/def2.inicio

# utp: http://www.utp.ac.pa/planilla-de-la-utp

# unachi excel: http://www.unachi.ac.pa/transparencia
# ifaruh pdf: https://www.ifarhu.gob.pa/transparencia/11-3-planillas/ 
# pandeportes, pdf.
# tocumenn: http://tocumenpanama.aero/index.php/planilla?find=all




(153849 + 157304 + 158416 + 156577 + 156518 + 157753) / 6


