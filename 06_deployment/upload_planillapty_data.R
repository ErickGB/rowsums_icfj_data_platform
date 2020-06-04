cat("\014")
gc() # garbage collector
# ********************************************************************
# google bigquery connection ----
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

get_css_position_name <- function(name) {
	#name <- "SINESPACIO"
	position <- as.integer(str_locate(name, " ") [1])
	new_name <- name
	if(is.na(position) == FALSE) {
			new_name <- paste0(str_replace(substr(name, 1, position), " ", "_"), substr(name, position+1, nchar(name)))	
	}
	
	return(new_name)
}

PATH_OUT <- "./00_data/out/salaries/pending_process/2020/march/"
date_time <- as.character(as.Date('15/04/2020', tryFormats=c('%d/%m/%Y'))) # process execution day Sys.Date()
last_update <- paste0(substr(date_time, 1, 8), "01") # execution month

process_date <- as.Date(last_update) - as.difftime(1, unit = "days") # data of the month ...
process_month <- tolower(month.name[as.integer(paste0(substr(process_date, 6, 7)))])
process_month
# ********************************************************************
# upload file 
list_files <- list.files(PATH_OUT)
master_tbl <- tibble()
for(i in 1:length(list_files))
{
	print(list_files[i])
	temp_raw_tbl <- readr::read_csv(paste0(PATH_OUT, list_files[i]))
	temp_raw_tbl$file_name <- as.character(list_files[i])
	
	source <- substr(as.character(list_files[i]), 1, 3)
	if (source == "css") {
		temp_raw_tbl$start_date = as.Date(temp_raw_tbl$start_date, tryFormats = c("%Y-%m-%d"))
		temp_raw_tbl <- temp_raw_tbl %>% 
			mutate(
				position = map_chr(position, get_css_position_name)
			)
		
		temp_raw_tbl <- temp_raw_tbl %>% 
			mutate(position = ifelse(position == "TECNOLOGO_EN RADIOLOG E IMÃ\u0083Â\u0081GENES I II", 
																"TECNOLOGO_EN RADIOLOG E IMAGENES I I", position))
		
		temp_raw_tbl <- temp_raw_tbl %>% 
			mutate(position = ifelse(position == "CONDUCTOR_DE VEHICULO II", 
																"CONDUCTOR_DE VEHICULO  II", position))
		
		temp_raw_tbl <- temp_raw_tbl %>% 
			mutate(position = ifelse(position == "CONDUCTOR_DE VEHICULO I", 
																"CONDUCTOR_DE VEHICULO   I", position))
		
		temp_raw_tbl <- temp_raw_tbl %>% 
			mutate(position = ifelse(position == "ALBA√ëIL I", 
																"ALBAQIL_I", position))
		
		temp_raw_tbl <- temp_raw_tbl %>% 
			mutate(position = ifelse(position == '"CORREDOR_DE PRIMA DE ANTIGÃ\u0083Â\u009cEDAD"', 
															 "CORREDOR_DE PRIMA DE ANTIGUEDAD", position))
		
		
	}
	temp_raw_tbl$start_date = as.Date(temp_raw_tbl$start_date, tryFormats = c("%d-%m-20%y", "%d/%m/%Y"))
	
	if (source == "cgr") {
		temp_raw_tbl$start_date = as.Date(temp_raw_tbl$start_date, tryFormats = c("%d/%m/%Y"))
	}
	if (source == "mic") {
		temp_raw_tbl$start_date = as.Date(temp_raw_tbl$start_date, tryFormats = c("%d-%m-%y"))
	}
	if (source == "mia") {
		temp_raw_tbl$start_date = as.Date(temp_raw_tbl$start_date, tryFormats = c("%d/%m/%Y"))
	}
	
	if(i == 1) {
		master_tbl <- temp_raw_tbl
	}	else {
		master_tbl <- rbind(master_tbl, temp_raw_tbl)
	}
}

master_tbl %>% 
	glimpse()

table(master_tbl$record_date, master_tbl$file_name)
table(master_tbl$record_date, master_tbl$update_date)


master_tbl <- master_tbl %>% 
	mutate(position = ifelse(substr(position, 1, 25) == "CORREDOR_DE PRIMA DE ANTI", 
													 "CORREDOR_DE PRIMA DE ANTIGUEDAD", position))

master_tbl <- master_tbl %>% 
	mutate(position = ifelse(position == "ALBAÑIL I", 
													 "ALBAQIL_I", position))

master_tbl <- master_tbl %>% 
	mutate(position = ifelse(position == "ALBAÑIL", 
													 "ALBANIL", position))

master_tbl <- master_tbl %>% 
	mutate(position = ifelse(position == "ALBAÃ\u0083Â\u0091IL_JEFE", 
													 "ALBANIL_JEFE", position))

master_tbl <- master_tbl %>% 
	mutate(position = ifelse(position == "ALBAÃÂIL_JEFE", 
													 "ALBANIL_JEFE", position))

master_tbl <- master_tbl %>% 
	mutate(position = ifelse(position == "JEFA DE COOÉRACIÓN TECNICA", 
													 "JEFA DE COORDINACIÓN TECNICA", position))

master_tbl <- master_tbl %>% 
	mutate(position = ifelse(position == "DISEÑADOR GRAFICO", 
													 "DISEÑADOR GRÁFICO", position))



# date_processed = fecha de registro del dato.. debería ser record_date
# record_date = fecha en la que se proceso... debería ser processed_date
# date_processed = css es record_date  ... en central gov es update_date.. 
# record_date = css es update_date ... en central gov es record_date
#central_css_gov_salaries_december.csv cambiar update_date a 2020-01-01

master_tbl %>% 
	glimpse()

table(master_tbl$record_date)
table(master_tbl$record_date, master_tbl$update_date)

master_tbl <- master_tbl %>% 
	#filter(is.null(start_date) == FALSE) %>% 
	mutate(
		update_date = as.Date('2020-04-15', tryFormat = '%Y-%m-%d'), # cuando lo actualice
		record_date = as.Date('2020-03-31', tryFormat = '%Y-%m-%d')  # de cuando es el dato
		#status = toupper(str_trim(status, side = "both")),
		#position = toupper(str_trim(position, side = "both")),
				 )

# master_tbl <- readr::read_csv(paste0("./00_data/out/salaries/", "miamb_employees_processing_november.csv"))
#css_tbl <- readr::read_csv(paste0(PATH_OUT, "central_css_gov_salaries_", process_month, ".csv"))
#cgr_tbl <- readr::read_csv(paste0(PATH_OUT, "central_gov_salaries_", process_month, ".csv"))
#mic_tbl <- readr::read_csv(paste0(PATH_OUT, "mic_gov_salaries_", process_month, ".csv"))
#meduca_tbl <- readr::read_csv(paste0(PATH_OUT, "meduca_gov_salaries_", process_month, ".csv"))
# 184184 - 195224.. 180720, feb = 186625
nrow(master_tbl)


master_tbl %>% 
	DataExplorer::plot_missing()

master_tbl %>% 
	mutate(month = lubridate::month(update_date)) %>% 
	group_by(month, file_name) %>% 
	summarize(total = n(), salary = round(sum(salary)/1000000, 2),  max_date = max(start_date))

entities_temp <- master_tbl %>% 
	mutate(month = lubridate::month(update_date)) %>% 
	group_by(month, code, entity) %>% 
	summarize(total = n(), salario = sum(salary), max_date = max(start_date, na.rm = TRUE)) %>% 
	arrange(desc(total))

nrow(master_tbl)
View(entities_temp)

# temporally out MEDUCA
#master_tbl <- master_tbl %>% 
#	filter(code != "007")

 
# ********************************************************************
# googledrive authentication
httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())

drive_auth(path = "./00_scripts/rowsums-2198b8679813.json")
project <- "rowsums"

projectid<-'rowsums'
datasetid<-'journalists'
bq_conn <-  dbConnect(bigquery(), 
											project = projectid,
											dataset = datasetid, 
											use_legacy_sql = FALSE
)


sql <- "select max( employee_salary_id ) as max from journalists.f_employee_salary "
count_result <- query_exec(sql, project = project, useLegacySql = FALSE)
count_result$max

cgs_tbl <- master_tbl
cgs_tbl$file_name <- NULL


names <- colnames(cgs_tbl)
cgs_tbl <- cgs_tbl %>% 
	mutate(employee_salary_id = as.integer(rownames(.))) %>% 
	mutate(employee_salary_id = employee_salary_id + count_result$max) %>% 
	select(employee_salary_id, names)
min(cgs_tbl$employee_salary_id) - count_result # 1 it's ok

#master_tbl[178780:178782, c("start_date", "record_date", "update_date", "entity", "file_name")] %>% glimpse()
#cgs_tbl[185693, c("start_date") ] <- as.Date("2020-02-01", tryFormats = c('%Y-%m-%d'))

# 1: Load principal table: staging_central_gov_salaries
tryCatch(
	{
		job <- insert_upload_job("rowsums", "data_test", table = "staging_central_gov_salaries", 
														 values = cgs_tbl, write_disposition = "WRITE_TRUNCATE")
		status <- wait_for(job)
	}, # end try
error=function(error_message) {
	print(error_message)
}) 


#cgs_tbl[185693, c("start_date") ] 

table(cgs_tbl$status)
#cgs_tbl <- master_tbl # 180,720
cgs_tbl$employee_salary_id <- NULL
cgs_tbl$file_name <- NULL
cgs_tbl$position <- str_trim(cgs_tbl$position, "both")
job <- insert_upload_job("rowsums", "journalists", table = "central_gov_salaries", 
												 values = cgs_tbl, write_disposition = "WRITE_TRUNCATE")
wait_for(job)

entidades_tbl <- readr::read_csv2("./00_data/out/salaries/entidades.csv")
entidades_tbl$entity_id <- as.integer(entidades_tbl$entity_id)
entidades_tbl %>% 
	glimpse()

# add data to final tables 
job <- insert_upload_job("rowsums", "journalists", table = "d_entity", 
												 values = entidades_tbl, write_disposition = "WRITE_TRUNCATE")
wait_for(job)


# *****************
# JOBS : new jobs? ADD MANUALLY    :(     .. code != '900' and 
sql <- "SELECT entity, upper(position) position, count(*) as total, avg(salary) salary 
FROM journalists.central_gov_salaries where (position) not in (
  SELECT (job_title) FROM journalists.d_jobs
) GROUP BY entity, position"
query_results <- query_exec(sql, project = project, useLegacySql = FALSE)
as_tibble(query_results) %>% 
	arrange(desc(salary))


entity_jbos <- as_tibble(query_results) %>% 
	group_by(entity, position) %>% 
	summarise(salary = sum(salary)) %>% 
	arrange(desc(salary)) 
table(entity_jbos$entity)
entity_jbos$position[1]
entity_jbos$position[2]
View(entity_jbos)

add_jobs <- as_tibble(query_results) %>% 
	rename(job_title = position) %>% 
	mutate(PEP = NA, PEP2 = NA, date_record = '03/18/2020', 
				 job_position = NA,
				 cluster = NA, jobs_id = rownames(.)) %>% 
	dplyr::select(jobs_id, job_position, job_title, PEP, PEP2, date_record, cluster)

write.csv(entity_jbos, 
					paste0(PATH_OUT, "entity_new_jobs_", process_month,".csv"), row.names = FALSE)
View(entity_jbos)

add_jobs <- readr::read_csv2(paste0("./00_data/out/salaries/", "new_jobs_", process_month,".csv"))


# Ajusta los cargos que hacen falta... 

#UPDATE data_test.staging_central_gov_salaries ST
#   SET ST.position = tp.position_modified
#  FROM data_test.staging_jobs_diferences tp
# WHERE ST.position = tp.position


#update data_test.staging_central_gov_salaries 
#   SET position = 'CONDUCTOR DE VEHICULO  II' 
# where position = 'CONDUCTOR_DE VEHICULO II'




sql <- "SELECT max(jobs_id) max FROM journalists.d_jobs"
count_result <- query_exec(sql, project = project, useLegacySql = FALSE)
count_result$max + 1

add_jobs$jobs_id <- add_jobs$jobs_id +  count_result$max

# add jobs manually
jobs_tbl <- readr::read_csv(paste0("./00_data/out/salaries/", "last_jobs.csv"))
jobs_tbl$jobs_id <- as.integer(jobs_tbl$jobs_id)
jobs_tbl$cluster <- as.integer(jobs_tbl$cluster)
jobs_tbl[count_result$max:count_result$max+1,]
nrow(jobs_tbl)


jobs_tbl_2 <- rbind(jobs_tbl, add_jobs)
jobs_tbl_2 <- jobs_tbl %>% 
	group_by(job_position, job_title) %>% 
	summarise(jobs_id = max(jobs_id), PEP2 = min(PEP2), date_record = min(date_record), n = n()) %>% 
	filter(n > 1)
View(jobs_tbl_2)

add_jobs <- add_jobs %>% 
	filter(!(jobs_id %in% jobs_tbl_2$jobs_id))
jobs_tbl <- rbind(jobs_tbl, add_jobs)

job <- insert_upload_job("rowsums", "journalists", table = "d_jobs", 
												 values = jobs_tbl, write_disposition = "WRITE_TRUNCATE")
wait_for(job)



# *****************
# PEOPLE: add new people

sql <- "SELECT max(people_id) count from journalists.d_people"
query_results <- query_exec(sql, project = project, useLegacySql = FALSE)
id <- query_results$count + 1

# new people
sql <- "SELECT person_id, count(*) count_persons FROM data_test.staging_central_gov_salaries where person_id not in (select person_id from journalists.d_people) group by person_id"
new_people_tbl <- query_exec(sql, project = project, useLegacySql = FALSE)
new_people_tbl$count_persons <- NULL 

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
	mutate(record_date = as.Date(record_date, tryFormats = c('%Y-%m-%d'))) %>% 
	rename(fist_name = complete_name) %>% 
	select(people_id, person_id, fist_name, last_name, start_date, sex, record_date)
#people_tbl$people_id <- as.character(people_tbl$people_id)

nrow(people_tbl)
people_tbl %>% 
	glimpse()
min(people_tbl$people_id)

# sin repetidos
people_tbl %>% 
	count(person_id) %>% 
	filter(n > 1)

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
new_record$record_date <- master_tbl$update_date[1]  #as.Date(last_update)
new_record$processed_date <-  master_tbl$record_date[1] #as.Date(last_update) - as.difftime(1, unit = "days")
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
c.sex, c.key,concat(c.person_id, " ", j.job_title) as key1, over_costs , departament
FROM data_test.staging_central_gov_salaries c 
INNER JOIN journalists.d_people p ON p.person_id = c.person_id 
INNER JOIN journalists.d_entity e ON e.entity_code = c.code 
INNER JOIN journalists.d_jobs j ON j.job_title = c.position 
INNER JOIN journalists.d_date_upload d ON d.processed_date = c.record_date;"
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
# END 
# ********************************************************************










# Upload files 
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
# upload file to storage

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



# realizados 
# css 63M / 30K empleados: http://www.css.gob.pa/p/grid_defensoria/

# http://www.utp.ac.pa/historial-de-planilla-de-empleados
# https://www.ana.gob.pa/other/transp/planilla.php?page=83
# up (15nal) 14M: http://consulta.up.ac.pa/PortalUp/planilla.aspx
# metro: https://www.elmetrodepanama.com/transparencia-3/planilla-de-funcionarios/
# antai: http://www.antai.gob.pa/11489-2/
# senacyt: http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&id=162

# ACP: http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=26
# canal de panama: http://www.defensoriadelpueblo.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=26
# canal de panama: https://apps.pancanal.com/pls/defensoria/def2.inicio

# unachi excel: http://www.unachi.ac.pa/transparencia
# ifaruh pdf: https://www.ifarhu.gob.pa/transparencia/11-3-planillas/ 
# pandeportes, pdf.
# tocumenn: http://tocumenpanama.aero/index.php/planilla?find=all


# **********************************************************************
# DEFENSORIA DEL PUEBLO
# http://ogov.defensoria.gob.pa/transparencia/

# Lotería Nacional de Beneficencia - 2020-01-10 05:00:11, 1868 Records ** cedula sin formato
# http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=82

#Instituto Nacional de Cultura - 2020-01-17 10:17:47, 1009
#http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=74


# Autoridad Nacional de los Servicios Públicos - 2020-01-08 04:13:08 - 512
# http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=33

# Autoridad de Protección al Consumidor- ACODECO, 501
# http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=22

# Autoridad Panameña de Seguridad de Alimentos - 2020-01-10 04:32:00 
# http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&id=165

#Defensoría del Pueblo - 2020-01-13 04:11:24, 200
#http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=54


# ***************
# SIN CEDULA

# ATP 2019-12-19 11:07:04 **SIN CEDULA,
# http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=24

# ACP - 2019-12-31 10:38:36 **SIN CEDULA,
# http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=26

# Autoridad Marítima de Panamá - 2020-01-16 11:30:35 **SIN CEDULA, 1500
# http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=31&p=1


# BACKUP.. Revisar si actualizan en enero.

# MEDUCA 2019-12-26 01:39:10 *
#http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=92




# **********************************************************************
# PRESENTACIÓN
# ¿Qué dice la regulación sobre las PEP?
# ¿En que consiste el servicio? 
# ¿De dónde se optienen los datos?
# ¿Cómo identificamos los PEP?
		# cargos de elección popular, ministros y vice-ministros, directores, policias, jueces
# Productos
	# servicio mensual
	# down para datos actuales. 
	# empleados nuevos * 

# presentar tabla de empleados pep






