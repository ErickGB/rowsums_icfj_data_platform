cat("\014")
#install.packages("DataExplorer")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(V8)				 # call javascript functions
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(magick)    # Simplify high-quality image processing in R
library(splashr)   # HTML javascript functions
library(XML)
library(stringr) 
library(DataExplorer)

# ***********************************************
date_start <- "2020-07-01"
date_end <- "2020-07-31"
year <- substr(date_end, 1, 4)
record_type <- "I" # I = Imports, E = exports 
file_type <- ifelse(record_type == "I", "imports", "exports")
PATH_OUT <- paste0("./00_data/out/", file_type, "/") 
page_record <- ifelse(record_type == "I", 5000, 1000)

date_time <- as.character(Sys.Date()) # process execution day
last_update <- paste0(substr(date_time, 1, 8), "01") # execution month
process_date <- as.Date(last_update) - as.difftime(1, unit = "days") # data of the month ...
process_month <- tolower(month.name[as.integer(paste0(substr(process_date, 6, 7)))])

source("./00_scripts/aduanas_records.R")

#Mesa de Ayuda Procesos
#Teléfonos: 504 - 2723 / 4224 / 4253
#Email: soporte.siga@ana.gob.pa / grupo_procesos@ana.gob.pa
#Mesa de Ayuda SIGA
#Teléfono: 506 - 6200

# 1. print capture the last update
url <- paste0("http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=", date_start,"&calendario_hasta=", date_start,"&ruc=&importador=&tipo_oper=", record_type,"&puerto=&arancel=&mercancia=&cantresxpag=",as.character(10),"&pag=formprin&Accion_Consultar=Consultar")
file_name <- paste0("./00_data/images/2020/aduana/aduana_last_update_", process_month,".png")
img_last <- render_png(url = url, wait = 5)
image_write(img_last, file_name)


# exportación: http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=2019-01-01&calendario_hasta=2019-05-16&ruc=&importador=&tipo_oper=E&puerto=&arancel=&mercancia=&cantresxpag=99&pag=formprin&Accion_Consultar=Consultar
url <- paste0("http://190.34.178.196/aduana/SIGA_SICE/index.php?calendario_desde=", date_start,"&calendario_hasta=", date_end,"&ruc=&importador=&tipo_oper=", record_type,"&puerto=&arancel=&mercancia=&cantresxpag=",as.character(page_record),"&pag=formprin&Accion_Consultar=Consultar")
session <- html_session(url)


# number of records
record_text <- session %>% 
	rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
	rvest::html_text() %>% 
	substr(2952, 2986) %>% 
	str_replace(pattern = "Se encontraron", "") %>% 
	str_replace(pattern = "registros", "") %>% 
	str_replace(pattern = "\r", "") %>% 
	str_replace(pattern = "\n", "") %>% 
	str_replace(pattern = "\\.", "") %>% 
	str_replace(pattern = " ", "") %>% 
	str_trim(side = "both")
rm(session)
record_text # number of records

# table for get data
count <- ceiling(as.numeric(record_text) / page_record) - 1
records_tbl <- tibble(id = c(0, seq(1, count)))
records_tbl <- records_tbl %>% 
	mutate(link = paste0(url, "&np=", id))
records_tbl$link <- ifelse(records_tbl$id == 0, url, records_tbl$link)
if(count == 0) {
	records_tbl <- records_tbl[1,]
}


# srapy data (it's slowww, large number of records have been downloaded)
date_end
time <- Sys.time()
time
#plan("multiprocess")
	out <- tryCatch(
        {
records_tbl <- records_tbl %>% 
	mutate(features = future_map(link, get_all_products))
        }, error=function(cond) {
        	print(cond)
	})
Sys.time() - time	
records_tbl <- records_tbl %>% 
  unnest()
nrow(records_tbl)
# Stop clusters
#future:::ClusterRegistry("stop")

records_tbl %>% 
	glimpse()

records_tbl %>% 
	head()

print(records_tbl$link[1])

records_tbl %>% 
	DataExplorer::plot_missing()

nrow(records_tbl)
table(records_tbl$date)
record_text


table(substr(records_tbl$peso_neto, nchar(records_tbl$peso_neto) - 1, nchar(records_tbl$peso_neto)))
paste0(PATH_OUT, "out_", file_type, "_", date_end,".csv")

# save montly file 
file_type <- ifelse(record_type == "I", "import", "export")
write.csv(records_tbl, #fileEncoding = "UTF-8",
	paste0(PATH_OUT, "out_", file_type, "_", date_end,".csv"), 
	row.names = FALSE, na = '')

# *******************************************************************************
# upload azure 
source("./00_scripts/azure_functions.R")

# upload data to azure
set_load_data_az(
	"ctrade",
	paste0("./00_data/out/", file_type,"s/"),
	paste0("/", file_type,"/", year,"/"), 
	paste0("out_", file_type, "_", date_end,".csv")
	)

# **************************
# remove all variables 
rm(list=ls()) 
gc()
#flete chavale de aguadulce 





