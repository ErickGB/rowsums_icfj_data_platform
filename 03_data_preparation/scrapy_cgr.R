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
PATH_PROCESS_OUT <- "./00_data/out/salaries/pending_process/"    

date_time <- as.character(Sys.Date()) # process execution day
last_update <- paste0(substr(date_time, 1, 8), "01") # execution month
process_date <- as.Date(last_update, tryFormats = c("%Y-%m-%d")) - as.difftime(1, unit = "days") # data of the month ...
actual_month <- tolower(month.name[as.integer(paste0(substr(process_date, 6, 7)))])

requiere_joins_files <- TRUE             # requiere row bindws with other extraccion


# *******************************************************************************
# Load supplementary data ----
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
# *******************************************************************************
# functions ----
# format key get_people_id("8-707-2100")
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
# split txt data
get_split_value <- function (value, position) {
	split_value <- str_split(value, pattern = " ")[[1]][position]
	split_value <- ifelse(is.na(split_value) == TRUE, "", as.character(split_value))
	return(split_value)
}
# realiza web scraping 
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
}
# get sex by name
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

# ***********************************************
# load data ----
# Starting web scraping
# ***********************************************

# capture the last update
url <- 'http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp'
file_name <- paste0("./00_data/images/2020/cgr/cgr_last_update_", actual_month,".png")
img_last <- render_png(url = url, wait = 5)
image_write(img_last, file_name)

# start 
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
# PROCESSED IN PARALLEL with furrr  ----
#get_employees('007', '1')

time_1 <- system.time()
#plan("multiprocess")
codes <- c('007', '018', '012', '000', '045')
codes <- c('000')
employee_salaries_tbl <- entities_tbl %>%
	  #filter(!(codigo %in% codes)) %>% 
    mutate(features = future_map2(codigo, status, get_employees))

final_tbl <- employee_salaries_tbl %>% 
  unnest()
system.time() - time_1 

# end scrapy 
final_tbl$departament = "unknow"
final_tbl$over_costs = "0"

final_tbl %>% 
	glimpse()

final_tbl %>% 
	DataExplorer::plot_missing()

# ***********************************************************************
# data wrangling.. data cleaning for All data
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

# ********************************************************************
# performs sex estimation by name ----
final_tbl <- final_tbl %>% 
	mutate(
		sex = sapply(primer_nombre, function(x) get_sex_by_name(x))
	)

final_tbl %>% 
	glimpse()

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

final_tbl %>% 
	glimpse()


final_tbl %>% 
	count(entidad) %>% 
	arrange(desc(n))

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
paste0(PATH_PROCESS_OUT,  actual_month, "/",  "central_gov_salaries_", actual_month,".csv")
write.csv(master_tbl, 
					paste0(PATH_PROCESS_OUT,  actual_month, "/",  "central_gov_salaries_", actual_month,".csv")
					, row.names = FALSE) 
table(master_tbl$update_date)
max(master_tbl$start_date)

# 2020-03-01 
#     149466 

# ********************************************************************
# END 
# ********************************************************************


