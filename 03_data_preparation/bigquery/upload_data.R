# 153909
# 157363 - 153909
# ********************************************************************

cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(furrr)     # Parallel Processing using purrr (iteration)
library(purrr) 
library(purrrlyr)
library(RPostgreSQL)
library(fs)        # Working with File System
# ***********************************************

# upload data to the cloud, postgress database. Use BULK copy ----
#username = doadmin
#password = **************** 
#host = rowsums-db-postgresql-do-user-4865488-0.db.ondigitalocean.com
#port = 25060
#database = defaultdb
#sslmode = require

# reglas
# se actualiza last_update, de personas
# si la cedula no existe, se agrega a personas
# 

# --------------------------------------------------------------------------------------
# Database credentials
dbname <- "rowsums_db" 
host_name <- "rowsums-db-postgresql-do-user-4865488-0.db.ondigitalocean.com"
user_name <- "doadmin" 
pwd <- "qcaatg6n8cxj8dvd"
sslmode = 'require'
# --------------------------------------------------------------------------------------
to_list <- function(row) 
{  #row is a tibble with one row, and the same number of columns as the original df
  l <- as.list(row)
  return(l)
}	

set_insert_salaries <- function(data_row) {
  # insert data from the PostGIS server
  db_connection = dbConnect(
  PostgreSQL(), dbname=dbname, host=host_name, port=25060, user=user_name, password=pwd #, sslmode="disable"
  )
  fields <- paste("code", "entity_name", "url_source", "first_name", "last_name", "person_id", 
  	"job_title", "salary", "expenses", "status", "start_date", "filter_name", "total_income", "sex", 
  	"last_update", "record_date", sep = ", ")
  params <- paste0("$", seq(1:16))
  sql_command = paste0("INSERT INTO staging.employee_salary_temp (", fields, ") VALUES (", params, ")")
  #df = dbGetQuery(dbConnection, sql.command)
  dbSendQuery(db_connection, sql_command, purrrlyr::by_row(data_row[1, ], to_list, .labels = FALSE)$.out)
  close <- dbDisconnect(db_connection)
}

get_test_sql <- function()
{
	# insert data from the PostGIS server
  db_connection = dbConnect(
  dbDriver("PostgreSQL"), dbname=dbname, host=host_name, port=25060, user=user_name, password=pwd
  )
  sql_command = paste0("select * from staging.employee_salary_temp")
  df <- dbGetQuery(db_connection, sql_command)
  close <- dbDisconnect(db_connection)
  return (df)
}

get_test_sql() 

set_insert_salaries(final_tbl[1,])

#install.packages("purrrlyr")
#lists <-  purrrlyr::by_row(final_tbl[1,], to_list, .labels = FALSE)$.out
