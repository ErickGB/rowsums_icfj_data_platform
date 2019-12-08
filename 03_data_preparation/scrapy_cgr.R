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

library(bigrquery)
# ***********************************************
PATH_OUT <- "./00_data/out/salaries/"
date_time <- as.character(Sys.Date())
actual_month <- "nov" # 
last_update <- as.Date('2019-11-01')
# ***********************************************
# functions ----
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


final_tbl <- final_tbl  %>% 
	filter(nombre != "sin_datos") %>% # elimina los registros vacios 
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

date_time <- as.Date(substr(update, 1, 10), format = "%d/%m/%Y")
final_tbl <- final_tbl %>%
		mutate(
		url = "http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp",
		record_date = date_time,
		cedula = stringr::str_trim(as.character(cedula), side = "both"),
		nombre = stringr::str_trim(as.character(nombre), side = "both"),
		apellido = stringr::str_trim(as.character(apellido), side = "both"),
		cargo = stringr::str_replace(stringr::str_trim(as.character(cargo), side = "both"), " ", "_"),
		entidad = stringr::str_trim(as.character(entidad), side = "both"),
		key = paste(cedula, as.character(fecha_inicio), cargo, sep = "_")
		)

master_tbl$entidad <- ifelse(master_tbl$entidad == 'Otros Gastos de la Administracion', 
	'Otros Gastos de la Administración', master_tbl$entidad) 

master_tbl$entidad <- ifelse(master_tbl$entidad == 'Ministerio de Comercio e Industria', 
	'Ministerio de Comercio e Industrias', master_tbl$entidad) 

# create data for Tableau month dashboard : central_gov_salaries
master_tbl <- final_tbl %>% 
	rename(
		code = codigo, complete_name = nombre, last_name = apellido, person_id = cedula, 
		position = cargo, salary = salario, expenses = gasto, total_income = total, status = estado, 
		start_date = fecha_inicio, first_name = primer_nombre, entity = entidad, update_date = last_update
		) %>%	dplyr::select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
		start_date, first_name, entity, update_date, sex, url, record_date, key) 


master_fail_tbl <- master_tbl %>% 
	filter(is.na(start_date) == TRUE) 

master_tbl <- master_tbl %>% 
	filter(is.na(start_date) == FALSE) 

master_tbl %>% 
	glimpse()

View(master_tbl)

master_tbl %>% 
	filter(position == 'AGENTE_DE INSTRUCCI`N DELEGADO') %>% 
	head()

master_tbl <- master_tbl %>% 
	mutate(position = ifelse(position == 'AGENTE_DE INSTRUCCI`N DELEGADO', 'AGENTE_DE INSTRUCCIÓN DELEGADO', position))

nrow(master_tbl)
write.csv(master_tbl, paste0(PATH_OUT, "central_gov_salaries_", actual_month,".csv"), row.names = FALSE) 
table(master_tbl$update_date)


#master_tbl <- readr::read_csv(paste0(PATH_OUT, "central_gov_salaries_", actual_month,".csv"))
master_tbl %>% 
	glimpse()
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
drive_auth(path = "./00_scripts/rowsums-2198b8679813.json")
project <- "rowsums"

sql <- "select max( employee_salary_id ) as max from journalists.f_employee_salary "
count_result <- query_exec(sql, project = project, useLegacySql = FALSE)
count_result$max

names <- colnames(master_tbl)
master_tbl <- master_tbl %>% 
	mutate(employee_salary_id = as.integer(rownames(.))) %>% 
	mutate(employee_salary_id = employee_salary_id + count_result$max) %>% 
	select(employee_salary_id, names)

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
FROM data_test.staging_central_gov_salaries where position not in (
  SELECT job_title FROM journalists.d_jobs
) GROUP BY position"
query_results <- query_exec(sql, project = project, useLegacySql = FALSE)
query_results

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

new_record <- dates_records[1, ]
new_record$record_id <- (max(dates_records$record_id) + 1)
new_record$record_date <- last_update
new_record$processed_date <- as.Date("2019-09-30")
new_record$is_actual <- 1

dates_records$is_actual <- 0
dates_records <- rbind(dates_records, new_record)
dates_records$record_id <- as.integer(dates_records$record_id)

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




# css: http://www.css.gob.pa/p/grid_defensoria/
# canal de panama: http://www.defensoriadelpueblo.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=26
# https://apps.pancanal.com/pls/defensoria/def2.p_inicio
# up: http://consulta.up.ac.pa/PortalUp/planilla.aspx
# utp: http://www.utp.ac.pa/planilla-de-la-utp

# unachi excel: http://www.unachi.ac.pa/transparencia
# ifaruh pdf: https://www.ifarhu.gob.pa/transparencia/11-3-planillas/ 
# pandeportes, pdf.
# tocumenn: http://tocumenpanama.aero/index.php/planilla?find=all




(153849 + 157304 + 158416 + 156577 + 156518 + 157753) / 6


