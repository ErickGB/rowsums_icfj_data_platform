library(RSQLServer)
conn <- dbConnect(
  RSQLServer::SQLServer(),
  "MyServer.database.windows.net",
  "useNTLMv2=false;user=MyUser;Password=MyPassword")
dbGetQuery(conn, "SELECT 1+1")
dbDisconnect(conn)



# **********************************************
install.packages("DBI")
install.packages("odbc")
install.packages("dbplyr")
install.packages("keyring")

library(odbc)
library(dplyr)
sort(unique(odbcListDrivers()[[1]]))

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "rowsums-dbserver.database.windows.net",
                 Database = "rowsumsdb",
                 UID = "rs-app-account",
                 #PWD = rstudioapi::askForPassword("Database password")
                 PWD = "xgBo0st.0$_",
                 Port = 1433)

db_compay_tbl <- tbl(con, dbplyr::in_schema("trade", "dcompany"))
db_company_tbl <- db_company_tbl %>%
  select(company_id, ruc, company_name)



