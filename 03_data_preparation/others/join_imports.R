gc()
cat("\014") 
# **********************************************************************
# Created by: Erick Gordon
# Date: 17 Enero 2019
# Description: Google points ---- 
# https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html
# **********************************************************************
# Libraries
# AIzaSyC2IjpVxOn_VgnpUaQrL9UC5ZiFS-mQLCY
library(tidyverse)
library(tidyquant)
library(janitor)
library(lubridate) # date manipulations
library(furrr)     # Parallel Processing using purrr (iteration)
#library(purrr)    # Functional programming
library(recipes)
library(DataExplorer)
# **********************************************************************
# Constant 
PATH_IN <- "./00_data/in/imports/"
PATH_OUT <- "./00_data/out/imports/"
# **********************************************************************
get_records_all <- function(list_file, root, type) {
	total_tbl <- as_tibble()
	for(i in 1:length(list_files)){
		print(list_files[i])
		tbl <- readr::read_csv(paste0(root, list_files[i]))
		tbl %>% 
			glimpse()
		if(nrow(tbl) > 0) {
			tbl$search_type <- type	
			tbl$ddate <- NULL
			if(i == 1) {
				total_tbl <- tbl
			} else {
				total_tbl <- rbind(total_tbl, tbl)
			}
		}
	}
	return(total_tbl)
}

list_files <- list.files(paste0(PATH_IN))
raw_tbl <- get_records_all(list_files, PATH_IN, "IMP")
raw_tbl %>% 
	glimpse()

# 13,046,838
raw_tbl <- raw_tbl %>% 
	mutate(
		es = map_chr(date, .f = function(x) tolower(substr(x, 4, 6))),
		year = map_chr(date, .f = function(x) substr(x, 8, 11)),
		day = map_chr(date, .f = function(x) as.numeric(substr(x, 1, 2))),
		fields = str_replace(fields, "\\:", ""),
		fields = str_replace(fields, "\\,", ""),
		idkey = paste0(id_page, "_", key, "_", es)
		) 

months_tbl <- readr::read_csv(paste0("./00_data/in/imp/", "dim_months.csv"))
months_tbl <- months_tbl %>% 
	janitor::clean_names()

initial <- nrow(raw_tbl)
raw_tbl <- inner_join(raw_tbl, months_tbl, by = 'es')
initial - nrow(raw_tbl)

raw_tbl <- raw_tbl %>% 
	mutate(
		#ddate = paste0(day, "-", number, "-", year)
		ddate = as.Date(paste0(day, "-", number, "-", year), tryFormats = c("%d-%m-%Y"))
		)

# *******************************************************************
# countries ----
# conseguir países del mundo: país, región, codigo
countries_tbl <- readr::read_csv(paste0("./00_data/in/imp/", "country_code.csv"))
countries_tbl <- countries_tbl %>% 
	janitor::clean_names()

countries_tbl <- countries_tbl %>% 
	rename(origin = code) %>% 
	mutate(country = toupper(country))

raw_tbl <- left_join(raw_tbl, countries_tbl, by = "origin")
table(raw_tbl$fields)
View((raw_tbl))

textos <- c("Cantidad", "Peso Neto", "Peso Bruto")
raw_tbl <- raw_tbl %>% 
	mutate(
		medida = ifelse(fields %in% textos, map_chr(data, .f = 
	function(x) substr(x, (str_locate_all(x, " ")[[1]][1] + 1), nchar(x))),
	"")
		) %>% 
	mutate(
		idkey = paste0(id_page, "_", key, "_", es, "_", year),
		medida = ifelse(is.na(medida) == TRUE, "", medida)
		) %>% 
	select(
		id_page, key, idkey, 
		date, day, es, year, en, season, number, 
		RUC, company, origin, country, description, 
		fields, data, data_processed, medida, search_type)

raw_tbl %>% 
	filter(medida != "") %>% 
	count(medida) %>% 
	arrange(desc(n)) %>% 
	mutate(
		perc = round((n / sum(n)) * 100, 2),
		perc_acum = cumsum(perc) 
		) %>% 
	head(20)





199 * 12
150 * 12

