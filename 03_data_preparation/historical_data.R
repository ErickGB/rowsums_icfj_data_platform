cat("\014")
# **************************************************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(purrr)     # Functional programming
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(XML)
# **************************************************************************
PATH_OUT <- "./00_data/out/salaries/"
date_time <- as.character(Sys.Date())
# **************************************************************************
# load summarized data ---- 
#historical_tbl <- readr::read_csv(paste0(PATH_OUT, "out_historical_data.csv"))

employees_2018 <- read_csv("00_data/in/salaries/cantidad_planilla_ene_dic_2018.csv")
salaries_2018 <- read_csv("00_data/in/salaries/salarios_planilla_ene_dic_2018.csv")
salaries_2019 <- read_csv("00_data/in/salaries/salarios_planilla_ene_dic_2019.csv")
employees_2019 <- read_csv("00_data/in/salaries/cantidad_planilla_ene_dic_2019.csv")
months_tbl <- read_csv("00_data/in/salaries/months.csv")

employees_2019 %>% 
	glimpse()

employees_2018 %>% 
	glimpse()

employees_2018 <- rbind(employees_2018, employees_2019)
employees_2018 <- employees_2018 %>% 
	mutate(Variable = "cantidad empleados") 

salaries_2018 <- rbind(salaries_2018, salaries_2019)
salaries_2018 <- 	salaries_2018 %>% 
	mutate(Variable = "salarios") 

rm(salaries_2019, employees_2019)


# data bind 
historical_data_tbl <- rbind(employees_2018, salaries_2018)
table(historical_data_tbl$Year)


historical_data_tbl <- historical_data_tbl %>% 
	dplyr::select(Tipo, Entidad, Year, Variable, Enero, Febrero, Marzo, Abril, Mayo, 
		Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre)
historical_data_tbl <- historical_data_tbl %>% 
	gather(key, value, 5:ncol(historical_data_tbl))

historical_data_tbl <- historical_data_tbl %>% 
	rename(month = key)

historical_data_tbl %>% 
	glimpse()


head(historical_data_tbl)

historical_data_tbl <- historical_data_tbl %>% 
	rename(month = key)

historical_data_tbl <- inner_join(historical_data_tbl, months_tbl, by = "month")
historical_data_tbl <- historical_data_tbl %>% 
	mutate(date = paste0(text, Year)) %>% 
	mutate(date = as.Date(date, "%d/%m/%Y"), text = NULL, lang = NULL) 

historical_data_tbl %>% 
	filter(is.na(historical_data_tbl$date))

historical_data_tbl <- historical_data_tbl %>% 
	filter(is.na(value) == FALSE)

historical_data_tbl$value = as.numeric(historical_data_tbl$value)

historical_data_tbl %>% 
	filter(is.na(value) == TRUE)

historical_data_tbl %>% 
	glimpse()

table(historical_data_tbl$date)
table(historical_data_tbl$Variable)
table(historical_data_tbl$Tipo)

historical_data_tbl

write.csv(historical_data_tbl, #fileEncoding = "UTF-8",
	paste0(PATH_OUT, "out_historical_data_jul.csv"), row.names = FALSE)


