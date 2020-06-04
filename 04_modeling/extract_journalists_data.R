cat("\014")
# ********************************************************************
# google bigquery connection ----
library(bigrquery)
library(googledrive)
library(gargle)
library(tidyverse)

library(vroom)     # 
library(anomalize)
library(tidyquant)
library(anytime) # Convert input in any one of character, integer, numeric, factor, or ordered type into 'POSIXct'
library(plotly)

PATH_OUT <- "./00_data/out/salaries/backup"
PATH_OUT_EXPORT <- "./00_data/out/salaries/export"
# ********************************************************************
httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())

bigquery_conn <- bigrquery::src_bigquery(project = "rowsums", dataset = "journalists")
# List table names
src_tbls(bigquery_conn)


# download jobs
jobs_tbl <- tbl(bigquery_conn, "journalists.d_jobs")
jobs_tbl <- collect(jobs_tbl)
write.csv(jobs_tbl, paste0(PATH_OUT, "d_jobs.csv"), row.names = FALSE)

# dowload people
people_tbl <- tbl(bigquery_conn, "journalists.d_people")
people_tbl <- collect(people_tbl)
write.csv(people_tbl, paste0(PATH_OUT, "d_people.csv"), row.names = FALSE)

# dowload entity
entity_tbl <- tbl(bigquery_conn, "journalists.d_entity")
entity_tbl <- collect(entity_tbl)
write.csv(entity_tbl, paste0(PATH_OUT, "d_entity.csv"), row.names = FALSE)

# dowload upload dates
date_upload_tbl <- tbl(bigquery_conn, "journalists.d_date_upload")
date_upload_tbl <- collect(date_upload_tbl)
write.csv(date_upload_tbl, paste0(PATH_OUT, "d_date_upload.csv"), row.names = FALSE)


# dowload upload dates
central_gov_salaries_tbl <- tbl(bigquery_conn, "journalists.central_gov_salaries")
central_gov_salaries_tbl <- collect(central_gov_salaries_tbl)
write.csv(central_gov_salaries_tbl, paste0(PATH_OUT, "central_gov_salaries.csv"), row.names = FALSE)


f_employee_salary_tbl <- tbl(bigquery_conn, "journalists.f_employee_salary")
f_employee_salary_tbl <- collect(f_employee_salary_tbl)
write.csv(f_employee_salary_tbl, paste0(PATH_OUT, "f_employee_salary.csv"), row.names = FALSE)

judicial_tbl %>% 
	glimpse()


# dowload upload dates
central_gov_salaries_tbl <- tbl(bigquery_conn, "journalists.f_employee_salary")
central_gov_salaries_tbl <- central_gov_salaries_tbl %>% 
	filter(record_id %in% c(4, 13))
show_query(central_gov_salaries_tbl)
central_gov_salaries_tbl <- collect(central_gov_salaries_tbl)
#write.csv(central_gov_salaries_tbl, paste0(PATH_OUT, "central_gov_salaries_asamblea_4-13.csv"), row.names = FALSE)



central_gov_salaries_tbl %>% 
	glimpse()


antes_tbl <- central_gov_salaries_tbl %>% 
	filter(record_id == 4)

ahora_tbl <- central_gov_salaries_tbl %>% 
	filter(record_id == 13 & entity_id %in% antes_tbl$entity_id)


# los que están ahora y no estaban antes (nuevos)
nuevos_tbl <- ahora_tbl %>% 
	filter(!(person_id %in% antes_tbl$person_id)) %>% 
	dplyr::select(person_id, first_name, job_title, total, status, start_date) %>% 
	mutate(is_new = ifelse(start_date >= as.Date("2019-07-01", tryFormats = c("%Y-%m-%d")), "Si", "No"))

nuevos_tbl %>% 
	filter(is.na(is_new))

nrow(nuevos_tbl)
table(nuevos_tbl$is_new)
nuevos_tbl %>% 
	group_by(is_new) %>% 
	summarise(n = n(), mean_salary = mean(total), median_salary = median(total), max_salary = max(total))
(table(nuevos_tbl$is_new) / nrow(nuevos_tbl)) * 100



# Revisión
nrow(antes_tbl) # antes 
nrow(ahora_tbl) # ahora
# diferencia
nrow(ahora_tbl) - nrow(antes_tbl)


ahora_tbl$is_new <- ifelse(ahora_tbl$person_id %in% nuevos_tbl$person_id, "es_nuevo", "no_es_nuevo")
ahora_tbl$is_before <- ifelse(ahora_tbl$person_id %in% antes_tbl$person_id, "si_es_anterior", "no_es_anterior")
table(ahora_tbl$is_new, ahora_tbl$is_before, useNA = "always")






# los que estaban antes y no están ahora (desvinculados)
desvinculados_tbl <- antes_tbl %>% 
	filter(!(person_id %in% ahora_tbl$person_id)) %>% 
	dplyr::select(person_id, first_name, job_title, total, status, start_date) 

nrow(desvinculados_tbl)

# detalle diferencia
nrow(nuevos_tbl) - nrow(desvinculados_tbl)


nrow(antes_tbl) +  nrow(nuevos_tbl) - nrow(desvinculados_tbl)

157773 - 138059 = 19714 - 19494
153920 

157773 - 153920 



antes habián 157,773
que están ahora quedan 138,059. Es decir que 157,773 - 138,059 = 19,714 han salido. Pero si busco los que 
estaban antes y no están ahora (desvinculados) = 19,494 ... hay +220 de diferencia. Hay 220 que tenían dos posiciones y ahora tienen una.  


ahora_tbl$is_before <- ifelse(ahora_tbl$person_id %in% antes_tbl$person_id, "si_es_anterior", "no_es_anterior")
table(ahora_tbl$is_before, useNA = "always")

antes_tbl$in_actual <- ifelse(antes_tbl$person_id %in% ahora_tbl$person_id, "permanece", "desvinculado")
table(antes_tbl$in_actual, useNA = "always") #  19494 

permanece_tbl <-  ahora_tbl %>% 
	filter(is_before =='si_es_anterior')

antes_tbl$valida <-  ifelse(antes_tbl$person_id %in% permanece_tbl$person_id, "ambos", "")
table(antes_tbl$valida, useNA = "always")

antes_tbl %>% 
	filter(valida == "ambos") %>% 
	count(person_id) %>% 
	filter(n > 1)


# ********************************************************************
# for page 

judicial_tbl <- f_employee_salary_tbl %>% 
	filter(entity_id == 14 & record_id %in% c(11, 12, 13)) %>% 
	dplyr::select(person_id, entity_name, first_name, last_name, job_position, status, start_date,
								salary, expenses, total, date_processed) %>% 
	rename(record_date = date_processed)
write.csv(judicial_tbl, paste0(PATH_OUT_EXPORT, "/judicial_enero_marzo_2020.csv"), row.names = FALSE)


president_tbl <- f_employee_salary_tbl %>% 
	filter(entity_id == 9 & record_id %in% c(11, 12, 13)) %>% 
	dplyr::select(person_id, entity_name, first_name, last_name, job_position, status, start_date,
								salary, expenses, total, date_processed) %>% 
	rename(record_date = date_processed)
write.csv(president_tbl, paste0(PATH_OUT_EXPORT, "/presidencia_enero_marzo_2020.csv"), row.names = FALSE)


legislativo_tbl <- f_employee_salary_tbl %>% 
	filter(entity_id == 8 & record_id %in% c(11, 12, 13)) %>% 
	dplyr::select(person_id, entity_name, first_name, last_name, job_position, status, start_date,
								salary, expenses, total, date_processed) %>% 
	rename(record_date = date_processed)
write.csv(legislativo_tbl, paste0(PATH_OUT_EXPORT, "/legislativo_enero_marzo_2020.csv"), row.names = FALSE)


nrow(judicial_tbl)
nrow(president_tbl)
nrow(legislativo_tbl)

legislativo_tbl %>% 
	glimpse()

table(judicial_tbl$record_date)
table(president_tbl$record_date)
table(legislativo_tbl$record_date)

# ********************************************************************




employee_tbl <- tbl(bigquery_conn, "journalists.f_employee_salary")
employee_tbl <- employee_tbl %>% 
	filter(entity_id == 24) %>% 
	select(entity_id, entity_name,  first_name, last_name, person_id, job_title, start_date, status, 
				 salary,	expenses,	total, date_processed, record_date) 
show_query(employee_tbl)

employee_tbl <- collect(employee_tbl)
nrow(employee_tbl)

employee_tbl %>% 
	glimpse()

employee_tbl <- employee_tbl %>% 
	mutate(temp = record_date) %>% 
	mutate(record_date = date_processed) %>% 
	mutate(date_processed = temp) %>% 
	mutate(temp = NULL)

table(employee_tbl$date_processed, employee_tbl$record_date)

employee_tbl <- employee_tbl %>% 
	rename(expense = expenses)

employee_tbl <- employee_tbl %>% 
	rename(processed_date = date_processed)

employee_tbl %>% 
	group_by(entity_name, record_date) %>% 
	summarize(n = n(), total = sum(total))
	


write.csv(employee_tbl, paste0(PATH_OUT, "min_gov_jan.csv"), row.names = FALSE)


select code, entity,  first_name, last_name, person_id, position, start_date, status, 
salary,	expenses expense,	total_income, record_date, update_date
from journalists.central_gov_salaries where code = '018';



drive_auth(path = "./00_scripts/rowsums-2198b8679813.json")
project <- "rowsums"

projectid<-'rowsums'
datasetid<-'journalists'
bq_conn <-  dbConnect(bigquery(), 
											project = projectid,
											dataset = datasetid, 
											use_legacy_sql = FALSE
)
