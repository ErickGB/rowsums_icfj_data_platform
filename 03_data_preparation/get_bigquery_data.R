cat("\014")
# ********************************************************************
# google bigquery connection ----
library(bigrquery)
library(googledrive)
library(gargle)
library(tidyverse)
PATH_OUT <- "./00_data/out/imports/"
# ********************************************************************
httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())

bigquery_conn <- bigrquery::src_bigquery(project = "rowsums", dataset = "trade")
# List table names
src_tbls(bigquery_conn)

categories_tbl <- tbl(bigquery_conn, "trade.dim_category")
categories_tbl <- collect(categories_tbl)

imports_tbl <- tbl(bigquery_conn, "trade.fact_agg_product")
imports_tbl %>% 
	glimpse()

total_ref <- imports_tbl %>% 
	filter(year == 2019L) 
show_query(total_ref)

total_tbl <- collect(total_ref)
nrow(total_tbl) # 286,125

total_tbl %>% 
	head()
#write.csv(total_tbl, paste0(PATH_OUT, "imports_class.csv"), row.names = FALSE)

temp <- total_tbl %>% 
	group_by(company) %>% 
	summarize(cif = sum(cif)) %>% 
	arrange(desc(cif)) %>% 
	ungroup() %>% 
	mutate(percent = (cif/sum(cif)) * 100) %>% 
	mutate(percent_acum = cumsum(percent)) %>% 
	mutate(secuence_id = as.numeric(rownames(.))) %>% 
	head(2200)
temp %>% filter(secuence_id > 40 & secuence_id <= 60) %>% select(-secuence_id)
temp %>% filter(secuence_id %in% c(340, 341, 512, 513, 873, 912, 914, 945))
View(temp)

top_200_companies_tbl <- total_tbl %>% 
	group_by(company) %>% 
	summarize(cif = sum(cif)) %>% 
	arrange(desc(cif)) %>% 
	top_n(2200, wt = cif)

summary_tbl <- total_tbl %>% 
	filter(company %in% c(top_200_companies_tbl$company)) %>% 
	group_by(company, sub_category_code) %>% 
	summarize(cif = sum(cif)) %>% 
	spread(key = sub_category_code, value = cif)

summary_tbl[is.na(summary_tbl)] <- 0
train_tbl <- as_tibble(scale(summary_tbl[,-1]))

# ********************************************************************
# backup 

bigquery_conn <- bigrquery::src_bigquery(project = "rowsums", dataset = "journalists")
# List table names
src_tbls(bigquery_conn)

salary_tbl <- tbl(bigquery_conn, "journalists.f_employee_salary")
salary_tbl <- collect(salary_tbl)

salary_tbl %>% 
	glimpse()

PATH_OUT <- "./00_data/out/salaries/"
write.csv(salary_tbl, paste0(PATH_OUT, "salary_oct.csv"), row.names = FALSE)

people_tbl <- tbl(bigquery_conn, "journalists.d_people")
people_tbl <- collect(people_tbl)
write.csv(people_tbl, paste0(PATH_OUT, "people_oct.csv"), row.names = FALSE)


jobs_tbl <- tbl(bigquery_conn, "journalists.d_jobs")
jobs_tbl <- collect(jobs_tbl)
write.csv(jobs_tbl, paste0(PATH_OUT, "jobs_oct.csv"), row.names = FALSE)

entity_tbl <- tbl(bigquery_conn, "journalists.d_entity")
entity_tbl <- collect(entity_tbl)
write.csv(entity_tbl, paste0(PATH_OUT, "entity_oct.csv"), row.names = FALSE)


upload_tbl <- tbl(bigquery_conn, "journalists.d_date_upload")
upload_tbl <- collect(upload_tbl)
write.csv(upload_tbl, paste0(PATH_OUT, "entity_oct.csv"), row.names = FALSE)


travel_tbl <- tbl(bigquery_conn, "journalists.f_travel_expenses")
travel_tbl <- collect(travel_tbl)
write.csv(travel_tbl, paste0(PATH_OUT, "travel_oct.csv"), row.names = FALSE)

mapping_tbl <- tbl(bigquery_conn, "journalists.d_mapping_city")
mapping_tbl <- collect(mapping_tbl)
write.csv(mapping_tbl, paste0(PATH_OUT, "travel_oct.csv"), row.names = FALSE)

historical_tbl <- tbl(bigquery_conn, "journalists.historical_gov_employees")
historical_tbl <- collect(historical_tbl)
write.csv(historical_tbl, paste0(PATH_OUT, "hist_oct.csv"), row.names = FALSE)


bq_deauth()


