
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
