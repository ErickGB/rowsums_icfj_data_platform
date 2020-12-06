cat("\014")
# ********************************************************************
# google bigquery connection ----
library(bigrquery)
library(googledrive)
library(gargle)
library(dplyr)

# ********************************************************************

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

dbListTables(bq_conn)


job_tbl <- dbReadTable(bq_conn, 'journalists.d_jobs')

job_tbl %>%
	glimpse()

write.csv(job_tbl, "./00_data/jobs_all.csv")


people_tbl <- Bin <- dbGetQuery(bq_conn, "SELECT people_id, person_id, fist_name, last_name, start_date, sex FROM journalists.d_people")

people_tbl %>%
	glimpse()

write.csv(job_tbl, "./00_data/people_tbl")



# ****************************************************


install.packages("AzureKeyVault")

library(AzureRMR)
library(AzureKeyVault)

vault <- key_vault("https://keyvaultplanillapty.vault.azure.net/")

# can also be done from the ARM resource object
vault <- kv$get_endpoint()
vault

vault$list()

key <- vault$keys$get("newkey")


# ***************************************


Sys.getenv("HOME")
install.packages("RSQLServer")
devtools::install_github('imanuelcostigan/RSQLServer')


library(DBI)
# Connect to AW server in ~/sql.yaml
# This is an Azure hosted SQL Server database provided at someone else's 
# expense. Feel free to tip them some:
# http://sqlblog.com/blogs/jamie_thomson/archive/2012/03/27/adventureworks2012-now-available-to-all-on-sql-azure.aspx
aw <- dbConnect(RSQLServer::SQLServer(), "RS", database = 'rowsumsdb')
# RSQLServer only returns tables with type TABLE and VIEW.
# But this DB has lots of useless tables. 
dbListTables(aw)
dbReadTable(aw, 'HumanResources.Department')

# Fetch all results
res <- dbSendQuery(aw, 'SELECT TOP 10 * FROM HumanResources.Department')
dbFetch(res)
dbClearResult(res)

(dept <- tbl(aw, sql("SELECT * FROM HumanResources.Department")))
# The following is translated to SQL and executed on the server. Only
# the first ten records are retrieved and printed to the REPL.
rd <- dept %>% 
	filter(GroupName == "Research and Development") %>% 
	arrange(Name)
# Bring the full data set back to R
collect(rd)

# Disconnect from DB
dbDisconnect(aw)







library(odbc)
library(dplyr)
odbc::odbcListDrivers()
con <- dbConnect(odbc(),
								 Driver = "ODBC Driver 17 for SQL Server",
								 Server = "rowsums-dbserver.database.windows.net",
								 Database = "rowsumsdb",
								 UID = "rs-app-account",
								 #PWD = rstudioapi::askForPassword("Database password"),
								 PWD = "xgBo0st.0$_",
								 Port = 1433)

dbListTables(con)
batch_tbl <- tbl(con, dbplyr::in_schema("internal", "dbatch")) 

res <- dbSendQuery(con, 'SELECT TOP 10 * FROM internal.dbatch')
dbFetch(res)

batch_tbl <- tbl(con, "dbatch") 
system.time(dbWriteTable(odbc, "internal.dbatch", as.data.frame(flights)))








