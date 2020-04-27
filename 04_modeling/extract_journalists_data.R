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

PATH_OUT <- "./00_data/out/salaries/"
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
