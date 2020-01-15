

# ********************************************************************
# upload file to storage

# ********************************************************************
# upload file to database staging table


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



# *****************************
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


# http://ogov.defensoria.gob.pa/transparencia/


DEIMIR 0900721001614,
INGENIERIA NATPA, S.A.
31 October 2019 (3 months ago)

0900721001614,
9-721-1614,

0100702000843,
1  702   843

0700118000217,
7  118   217


