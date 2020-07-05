cat("\014")
gc() # garbage collector
# ****************************
library(tidyverse)
library(tidyverse) # Main Package - Loads dplyr, purrr
library(AzureStor)

PATH_OUT <- "./00_data/in/covid/"
date_name <- Sys.Date()


file_name <- "https://opendata.arcgis.com/datasets/4cc2d9431ecf406aa1ab039fed1de668_0.geojson"
minsa_covid_tbl <- jsonlite::fromJSON(file_name) %>% as.data.frame

properties_tbl <- minsa_covid_tbl$features.properties 
position_tbl <- minsa_covid_tbl$features.geometry
position_tbl$num_id <- rownames(position_tbl)
properties_tbl$num_id <- rownames(properties_tbl)

properties_tbl <- inner_join(properties_tbl, position_tbl, by = 'num_id')
properties_tbl$coordinates <- NULL 
properties_tbl$num_id <- NULL 

print(getwd())

print(paste0("extract data from:https://opendata.arcgis for ", date_name, ", total rows:", nrow(minsa_covid_tbl)))

out_file <- paste0("/home/erick_gordon/rowsums_icfj_data_platform/00_data/in/covid/", "CORREGIMIENTOS_(PU)-", as.character(date_name), ".csv")
output_file_name <- paste0("CORREGIMIENTOS_(PU)-", as.character(date_name), ".csv")
write.csv(properties_tbl, out_file, row.names = FALSE)

# *****************************
# upload data to azure ----

set.seed(777)
library(tidyr)
PATH_IN <- "./00_data/in/covid"
PATH_OUT <- "./00_data/out/covid"

# ********************************************************************
# load data ----
list_files <- list.files(PATH_IN)
#list_files <- c("CORREGIMIENTOS_(PU)-2020-06-05.csv")

master_tbl <- tibble()
for(i in 1:length(list_files)) # 
{
	file_name <- list_files[i]
	extension <- str_sub(file_name, nchar(file_name) - 2, nchar(file_name))
	if(extension == 'csv'){
		print(paste0("loading..", file_name))
		temp_tbl <- readr::read_csv(paste0(PATH_IN, "/", file_name))
		temp_tbl$date <- substr(file_name, (nchar(file_name) - 11), (nchar(file_name) - 4))
		temp_tbl$file_name <- file_name
		
		if(i == 1) {
			master_tbl <- temp_tbl
		} else {
			master_tbl <- rbind(master_tbl, temp_tbl)
		}
	} 
}

# join all data
master_tbl <- master_tbl %>% 
	janitor::clean_names() %>% 
	mutate(
		date = as.Date(date, tryFormats = c('%y-%m-%d')),
		letalidad = round(fallecido / cantidad, 4),
		mortalidad = fallecido / sum(fallecido)
	)  %>% 
	mutate(corregimiento = stringr::str_trim(corregimiento, side = "both"))

out_file_all <- paste0(PATH_OUT, "/covid_minsa_all_data.csv")
write.csv(master_tbl, out_file_all, row.names = FALSE)

# *****************************
# upload data to azure ----

endp <- storage_endpoint("https://planillaptystaging.blob.core.windows.net", key="rMEGfnIWVo9eFefDiEk8pXpt3w6edCra8RLi43X/GGb7ve8pWzRdeJP/VBYSvkmhDxct06EkZY1ZGKUfYUv0Aw==")
cont <- storage_container(endp, "covid")

upload_blob(cont, out_file_all, dest=paste0("/minsa/covid_minsa_all_data.csv"))

#map_file <-"./00_data/in/maps/poligonos_2019.geojson"
#upload_blob(cont, map_file, dest=paste0("/minsa/poligonos_2019.geojson"))
#4NvmC#E85Xym#d5





