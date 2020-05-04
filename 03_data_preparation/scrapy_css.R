cat("\014")
gc() # garbage collector
# ***************************************************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(splashr)   # HTML javascript functions
library(magick)    # Simplify high-quality image processing in R
library(purrr)     # Functional programming
library(furrr)     # Parallel Processing using purrr (iteration)
# ***************************************************************************
url_css <- "http://www.css.gob.pa/p/grid_defensoria/"
url_last_update <- "http://web.css.gob.pa/transparencia/nodo-de-transparencia/" #"http://www.css.gob.pa/transparencia.html"
PATH_OUT <- "./00_data/out/salaries/pending_process/"
date_time <- as.character(Sys.Date()) # process execution day
last_update <- paste0(substr(date_time, 1, 8), "01") # execution month

process_date <- as.Date(last_update) - as.difftime(1, unit = "days") # data of the month ...
process_month <- tolower(month.name[as.integer(paste0(substr(process_date, 6, 7)))])


error_tbl <- tibble(
	error_id = 0,
	messages = 'no_errors'
)
# ***********************************************
# functions ----
source("00_scripts/etl_functions.R")
#get_css_employees(7)
get_css_employees <- function(page) {
	tryCatch({
		id <- 1
		# Data page  1...10 records
		page_tbl <- tibble(
			person_id = character(),
			complete_name = character(),
			job_title = character(),
			departament = character(),
			status = character(),
			start_date = character(),
			salary = character(),
			expens = character(),
			over_costs = character(),
			total = character()
		)
		
		print(paste0("page:", as.character(page)))
		body_html <- splash_local %>% 
			splash_go(url_css) %>% 
			splash_wait(10)
		
		body_html <- body_html %>% 
			splash_focus("#rec_f0_bot") %>% 
			splash_send_text(page) %>% 
			splash_send_keys("<Return>") %>% 
			splash_focus("#brec_bot") %>% 
			splash_send_keys("<Return>") %>% 
			splash_wait(10) %>% 
			#splash_click(x = 62, y = 760) %>% 
			splash_html() # splash_png() 
		#body_html
		
		#body_html %>% 
		#	rvest::html_nodes(css = 'input[id="rec_f0_bot"]') %>% 
		#	rvest::html_attr("value") 
		
		for(i in 1:10) {
			id <- i
			pperson_id <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_cedula_1"]', "1", as.character(i))) %>% 
				rvest::html_text() # "10-21-450"
			
			pcomplete_name <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_sc_field_0_1"]', "1", as.character(i)))  %>% 
				rvest::html_text()
			
			pjob_title <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_tcargo_1"]', "1", as.character(i)))  %>% 
				rvest::html_text()
			
			pdepartament <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_depto_1"]', "1", as.character(i)))  %>% 
				rvest::html_text()
			
			pstatus <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_estatus_1"]', "1", as.character(i)))  %>% 
				rvest::html_text()
			
			pstart_date <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_inicio_planilla_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			psalary <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_salario_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			pexpens <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_gastos_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			pover_costs <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_ssueldo_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			ptotal <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_total_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			record_tbl <- tibble(
				person_id = pperson_id,
				complete_name = pcomplete_name,
				job_title = pjob_title,
				departament = pdepartament,
				status = pstatus,
				start_date = pstart_date,
				salary = psalary,
				expens = pexpens,
				over_costs = pover_costs,
				total = ptotal
			)
			
			page_tbl <- rbind(page_tbl, record_tbl)
			
		}
	},
	error=function(error_message) {
		htlm_error <- body_html %>% 
			splash_html() # splash_png() 
		
		error_row_tbl <- tibble(
			error_id = id,
			messages = paste0("error ", id, ": ", error_message, " -->> with html result:", as.character(htlm_error))
		)
		print(paste0("error ", id, ": ", error_message))
		error_tbl <- rbind(error_tbl, error_row_tbl)
		
		page_tbl <- tibble(
			person_id = id,
			complete_name = error_message,
			job_title = "",
			departament = "",
			status = "",
			start_date = "",
			salary = "",
			expens = "",
			over_costs = "",
			total = ""
		)
		#page_tbl <- temp_tbl 
	})
	return(page_tbl)
}

# *******************************************************************************

# Table with results 1 to 3443
final_tbl <- tibble(
	id = 1:3436,
	entity = rep("CSS", 1, 3436),
	site = rep(url_css, 1, 3436)
)


# ***********************************************
# Save images

# Start, active splash ----
splash("localhost") %>% splash_active()

# 1. capture the last update
file_name <- paste0("./00_data/images/2020/css/css_last_update_", process_month,".png")
img_last <- render_png(url = url_last_update, wait = 5)
image_write(img_last, file_name)

# 1. capture principal page
file_name <- paste0("./00_data/images/2020/css/css_principal_page_", process_month,".png")
img_princial <- render_png(url = url_css, wait = 5)
image_write(img_princial, file_name)

#file_name <- paste0("./00_data/images/2020/antai/", "ANTAI-DS-DTAI-3989-2018",".png")
#img_princial <- render_png(url = "https://www.antai.gob.pa/circutar-no-antai-ds-dtai-3989-2018/", wait = 5)
#image_write(img_princial, file_name)

# ***********************************************
# scraping CSS web page 
# ***********************************************

body_html <- splash_local %>% 
	splash_go(url_css) %>% 
	splash_wait(5) %>% 
	splash_html()

time <- Sys.time()
#plan("multiprocess")
final_tbl_2 <- final_tbl %>%
	mutate(
		records = furrr::future_map(id, get_css_employees) 
	) # %>% unnest()
Sys.time() - time # Time difference of 6.748743 hours
error_tbl


final_expanded_tbl <- final_tbl_2 %>% 
	unnest()

# structure 
final_expanded_tbl %>% 
	glimpse()

final_expanded_tbl %>% 
	filter(job_title == "") %>%
	count(complete_name)

final_expanded_tbl %>% 
	DataExplorer::plot_missing()

final_expanded_tbl %>%  
	head()

write.csv(final_expanded_tbl, paste0(PATH_OUT, "css_employees_raw.csv"))
error_tbl


# clean data
final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(
		person_id = str_replace(str_trim(person_id), '"', ""),
		complete_name = str_replace(str_trim(complete_name), '"', ""),
		#first_name = str_split(complete_name, " "),
		job_title = str_replace(str_trim(job_title), '"', ""),
		departament = str_replace(str_trim(departament), '"', ""),
		status = str_replace(str_trim(status), '"', ""),
		salary = str_replace(str_trim(salary), '"', ""),
		salary = str_replace(salary, ',', ""),
		expens = str_replace(str_trim(expens), '"', ""),
		expens = str_replace(expens, ',', ""),
		over_costs = str_replace(str_trim(over_costs), '"', ""),
		over_costs = str_replace(over_costs, ',', ""),
		total = str_replace(str_trim(total), '"', ""), 
		total = str_replace(total, ',', ""),
	) %>% 
	mutate(
		start_date = as.Date(start_date, tryFormats="%Y-%m-%d"),
		salary = as.numeric(salary),
		expens = as.numeric(expens),
		over_costs = as.numeric(over_costs), 
		total = as.numeric(total), 
	)

# correct names
final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == 'CONDUCTOR_DE VEHICULO I', 
												'CONDUCTOR_DE VEHICULO   I', job_title))
final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == 'CONDUCTOR_DE VEHICULO II', 
												'CONDUCTOR_DE VEHICULO  II', job_title))
final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "TECNOLOGO_EN RADIOLOG E IMÃÂGENES I II", 
												"TECNOLOGO_EN RADIOLOG E IMAGENES I II", job_title))
final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "CORREDOR_DE PRIMA DE ANTIGÃÂEDAD", 
												"CORREDOR_DE PRIMA DE ANTIGUEDAD", job_title))
final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "ALBAÃÂIL_JEFE", 
												"ALBANIL_JEFE", job_title))
final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "ALBAÃÂIL", 
												"ALBANIL", job_title))

final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "ALBA√ëIL I", 
														"ALBANIL I", job_title))

final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "ALBA√ëIL", 
														"ALBANIL", job_title))

final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "TECNOLOGO_EN RADIOLOG E IM√É¬É√Ç¬ÅGENES I II", 
														"TECNOLOGO_EN RADIOLOG E IMAGENES I I", job_title))

final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "CORREDOR_DE PRIMA DE ANTIG√É¬É√Ç¬úEDAD", 
														"CORREDOR_DE PRIMA DE ANTIGUEDAD", job_title))

final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "CONDUCTOR_DE VEHICULO II", 
														"CONDUCTOR_DE VEHICULO  II", job_title))

final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(job_title = ifelse(job_title == "CONDUCTOR_DE VEHICULO I", 
														"CONDUCTOR_DE VEHICULO   I", job_title))

# final processing
final_expanded_tbl %>%
	glimpse()

# review
sum(final_expanded_tbl$over_costs) # 5,451,908... ene 5,565,226
sum(final_expanded_tbl$total) # 63,300,141... ene 63,943,842

# write data processing
write.csv(final_expanded_tbl, paste0(PATH_OUT, "css_employees_processing.csv"), row.names = FALSE)


# **************************
# start new data structure
employee_css_tbl <- final_expanded_tbl %>% 
	mutate(
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

employee_css_tbl <- employee_css_tbl  %>% 
	filter(nombre != "sin_datos") %>% # elimina los registros vacios 
	mutate(
		departament = gsub("\r\n", "", departament),
		over_costs = gsub("\r\n", "", over_costs),
		departament = stringr::str_trim(departament, side = "both"),
		over_costs = stringr::str_trim(over_costs, side = "both"), 
		over_costs = gsub(",", "", over_costs)
	) %>% 
	mutate(
		last_update = last_update,
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
		record_date = process_date,
		#key = paste(cedula, as.character(fecha_inicio), cargo, sep = "_")
		status = NULL ,
	)	

# ********************************************************************
# performs sex estimation by name ----
employee_css_tbl <- employee_css_tbl %>% 
	mutate(
		sex = sapply(primer_nombre, function(x) get_sex_by_name(x))
	)

employee_css_tbl %>% 
	glimpse()
sum(employee_css_tbl$over_costs) # 5,592,724

# write data processing
write.csv(employee_css_tbl, paste0(PATH_OUT, "css_employees_processing_", process_month,".csv"))

# ********************************************************************
# create data for Tableau month dashboard : central_gov_salaries ----

#employee_css_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/css_employees_processing_february.csv")
#employee_css_tbl$X1 <- NULL 
master_css_tbl <- employee_css_tbl %>% 
	mutate(
		key = paste(cedula, as.character(fecha_inicio), cargo, sep = "_")
	) %>% 
	rename(
		code = codigo, complete_name = nombre, last_name = apellido, person_id = cedula, 
		position = cargo, salary = salario, expenses = gasto, total_income = total, status = estado, 
		start_date = fecha_inicio, first_name = primer_nombre, entity = entidad, update_date = last_update
	)  %>%	
	select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
				 start_date, first_name, entity, update_date, sex, url, record_date, key, 
				 over_costs, departament) 

write.csv(master_css_tbl, paste0(PATH_OUT, "central_css_gov_salaries_", process_month,".csv"), row.names = FALSE) 
print(sum(master_css_tbl$over_costs, na.rm = TRUE))
rm(body_html, error_tbl, final_tbl)
warnings() 



# Nota: Validar caso de
# RUSBEL     BATISTA   9-0098-00959 DIRECTOR_NACIONAL... 5k + 7k = 12k
# ********************************************************************
# END 
# ********************************************************************



