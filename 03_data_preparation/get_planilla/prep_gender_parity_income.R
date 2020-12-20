cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse)
library(tidyquant)
library(ggthemes) 
library(forecast)     #
library(stringr)      # string manipulation
library(janitor)      # basic manipulation 
library(furrr)     		# Parallel Processing using purrr (iteration)
library(purrr)     		# Functional programming
library(DataExplorer) # general data explorer
# devtools::install_github("kalimu/genderizeR")
# https://store.genderize.io/usage
library(genderizeR)

#
source("./00_scripts/etl_functions.R")
PATH_IN <- "00_data/in/salaries/parity/"
# ***********************************************
# Load data ----

salary_tbl <- read_csv(paste0(PATH_IN, "central_gov_salaries_august.csv")) %>% 
	mutate(
		start_date = as.Date(start_date, tryFormats = c("%d/%m/%Y") )
	)
salary_tbl %>% 
	glimpse()

defendoria_tbl <- read_csv(paste0(PATH_IN, "defensoria_gov_salaries_september_2020_processed_2020-09-05.csv"))
defendoria_tbl %>% 
	glimpse()

serv_publicos_tbl <- read_csv(paste0(PATH_IN, "4_aut_serv_publicos_gov_salaries_september_2020_processed_2020-09-04.csv")) %>% 
	mutate(
		start_date = as.Date(start_date, tryFormats = c("%d/%m/%Y") )
	)
serv_publicos_tbl %>% 
	glimpse()

contrataciones_publicos_tbl <- read_csv(paste0(PATH_IN, "dir_contrataciones_publicas_gov_salaries_august_2020_processed_2020-09-04.csv")) %>% 
	mutate(
		start_date = as.Date(start_date, tryFormats = c("%d/%m/%Y") )
	)

contrataciones_publicos_tbl %>% 
	glimpse()


css_tbl <-  read_csv("00_data/out/salaries/pending_process/2020/march/2_central_css_gov_salaries_march.csv") %>% 
	mutate(
		start_date = as.Date(start_date, tryFormats = c("%d/%m/%Y") )
	)

css_tbl %>% 
	group_by(sex) %>% 
	summarise(
		n = n(),
		mean_income = mean(total_income),
		median_income = median(total_income)
	)


salary_tbl <- salary_tbl %>% 
	bind_rows(
		defendoria_tbl %>% 
			mutate(code = as.character(code))
	) %>% 
	bind_rows(
		serv_publicos_tbl %>% 
			mutate(code = as.character(code))
	) %>% 
	bind_rows(
		contrataciones_publicos_tbl %>% 
			mutate(code = as.character(code))
	) %>% 
	#bind_rows(
	#	css_tbl %>% 
	#		mutate(code = as.character(code))
	#) 
	mutate(
		fname = future_map2(tolower(first_name), 1, get_split_value)
	) %>% 
	unnest()
	

salary_tbl %>% 
	glimpse()

	
nrow(salary_tbl) # 156,431
table(salary_tbl$sex, useNA = "always")
table(salary_tbl$sex, useNA = "always") / nrow(salary_tbl)

salary_tbl %>% 
	group_by(sex) %>% 
	summarise(income = median(total_income))

nrow(salary_tbl) # 156,431 156431
salary_tbl %>% 
	count(entity)

rm(contrataciones_publicos_tbl, serv_publicos_tbl, defendoria_tbl, css_tbl)
# ********************************
# names 
# devtools::install_github("kalimu/genderizeR", force = TRUE)

names_tbl <- salary_tbl %>% 
	count(fname)
nrow(names_tbl)

gender_names_tbl <- genderizeR::findGivenNames(names_tbl$fname, progress = FALSE, apikey = 'b6c2ea47281db9963d2122187572e11d') # 


gender_names_tbl <- gender_names_tbl %>% 
	count(name, gender, probability, count, country_id) 

gender_names_tbl %>% 
	count(name) %>% 
	arrange(desc(n))

nrow(gender_names_tbl)

salary_tbl <- salary_tbl %>% 
	rename(name = fname)

salary_tbl <- salary_tbl %>% 
	unnest()


nrow(gender_names_tbl)
nrow(salary_tbl)
data_tbl <- salary_tbl %>% 
	left_join(gender_names_tbl, by = 'name') %>% 
	mutate(gender = ifelse(gender == "female", "MUJER", "HOMBRE") ) %>% 
	mutate(sex = ifelse(sex == 'X', gender, sex) )
nrow(data_tbl)


data_tbl %>% 
	glimpse()

data_tbl %>% 
	DataExplorer::plot_missing()

nrow(data_tbl) # 157,133
table(data_tbl$sex, useNA = "always")
table(data_tbl$sex, useNA = "always") / nrow(salary_tbl)

(6439 / 157133) * 100


data_tbl <- data_tbl %>% 
	mutate(
		gender = ifelse(as.character(gender) == "", NA, as.character(gender)),
		decil_name = ifelse(total_income <= 600, '1. menor B./600', NA),
		decil_name = ifelse(total_income > 600 & total_income <= 800, '2.  B./601 a 800', decil_name),
		decil_name = ifelse(total_income > 800 & total_income <= 970, '3.  B./801 a 970', decil_name),
		decil_name = ifelse(total_income > 970 & total_income <= 1154, '4.  B./971 a 1,154', decil_name),
		decil_name = ifelse(total_income > 1154 & total_income <= 1442, '5.  B./1,155 a 1,442', decil_name),
		decil_name = ifelse(total_income > 1442 & total_income <= 1719, '6.  B./1,443 a 1,719', decil_name),
		decil_name = ifelse(total_income > 1719 & total_income <= 1850, '7.  B./1,720 a 1,850', decil_name),
		decil_name = ifelse(total_income > 1850 & total_income <= 1994, '8.  B./1,851 a 1,994', decil_name),
		decil_name = ifelse(total_income > 1994 & total_income <= 2263, '9.  B./1,995 a 2,263', decil_name),
		decil_name = ifelse(total_income > 2263, '9.1  mayor a B./2,264', decil_name),
		start_date = as.Date(start_date, tryFormats = c("%d/%m/%y", "%d/%m/%Y", "%Y-%m-%d")), 
		years_in_work = as.integer(difftime(
			strptime(Sys.Date(), format = "%Y-%m-%d"),
			strptime(start_date, format = "%Y-%m-%d"), units="days")
		),
		years_in_work = round(years_in_work/365, 0),
		is_new = ifelse(start_date >= as.Date("2020-07-01", tryFormats = c('%Y-%m-%d')), 1, 0)
	)

entity_tbl <- read_csv2(paste0(PATH_IN, "entity_ca.csv"))
entity_tbl <- entity_tbl %>% 
	select(entity_code, with_ca, year, legal) %>% 
	rename(legal_year = year, code = entity_code) 


data_tbl <- data_tbl %>% 
	left_join(entity_tbl, by = 'code') 

data_tbl %>% 
	glimpse()


write.csv(data_tbl, "./00_data/in/salaries/parity/parity_ajusted_v3.csv")

# ********************************
# people 
people_tbl <- read_csv(paste0(PATH_IN, "people_all.csv"))
people_tbl <- people_tbl %>%
	mutate(name = furrr::future_map2(tolower(fist_name), 1, get_split_value)) %>% 
	unnest()
nrow(people_tbl)

nrow(people_tbl) # 156,431
table(people_tbl$sex, useNA = "always")
table(people_tbl$sex, useNA = "always") / nrow(people_tbl)


rm(names_tbl)
names_tbl <- people_tbl %>% 
	count(name)

nrow(names_tbl) # 
names_tbl <- findGivenNames(names_tbl$name, progress = FALSE, apikey = 'b6c2ea47281db9963d2122187572e11d') # 
names_tbl <- names_tbl %>% 
	count(name, gender, probability, count, country_id)

# join names and people data
people_tbl <- people_tbl %>% 
	left_join(names_tbl, by = 'name') %>% 
	mutate(gender = ifelse(gender == "female", "MUJER", "HOMBRE") ) %>% 
	mutate(sex = ifelse(sex == 'X', gender, sex) )
nrow(people_tbl)

nrow(people_tbl) # 156,431
table(people_tbl$sex, useNA = "always")
table(people_tbl$sex, useNA = "always") / nrow(people_tbl)

people_tbl %>% 
	glimpse()

write.csv(data_tbl, paste0(PATH_IN, "/people_ajusted_all.csv"), row.names = FALSE)

# ****************************
