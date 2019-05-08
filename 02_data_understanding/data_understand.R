cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse)
library(tidyquant)
library(ggthemes) 
library(forecast)     #
library(stringr)      # string manipulation
library(janitor)      # basic manipulation 
library(DataExplorer) # general data explorer
library(HDoutliers)   # outlier detection
#
source("./00_Scripts/base_funtions.R")
# ***********************************************
PATH_IN <- "./00_Data/in/salaries/"
PATH_OUT <- "./00_Data/out/salaries/"
source("./03_data_preparation/gov_salaries.R")
# ***********************************************
# Load Data ---- 
train_raw <- planitlla_20190406
rm(planitlla_20190406)

names_tbl <- readr::read_csv("./00_Data/in/names/namesComplete2016.csv")
names_tbl$X1 <- NULL  
names_tbl <- names_tbl %>% 
	group_by(firstname, sex) %>% 
	summarize(total = sum(count)) %>%
	ungroup() %>% 
	mutate(
			firstname = toupper(firstname), 
			sex = ifelse(sex == "F", "MUJER", "HOMBRE")
	)
	
get_sex_by_name <- function(name) 
{
	sex <- names_tbl %>% 
		filter(firstname == toupper(name)) %>% 
		arrange(desc(total)) %>% 
		dplyr::select(sex) %>% 
		head(1) %>% 
		as.character()
	
	sex <- ifelse(nchar(sex) > 6, "X", sex)
	return (sex)
}

#train_raw <- readr::read_csv(paste0(PATH_OUT, "out_planilla_01042019v2.csv"))
train_raw <- train_raw %>% 
	mutate(
		fecha_inicio = as.Date(fecha_inicio, format = "%d/%m/%Y"),
		last_update = as.Date(last_update, format = "%d/%m/%Y"), 
		sex = sapply(primer_nombre, function(x) get_sex_by_name(x))
		)
table(train_raw$sex, useNA = "always") 
table(is.na(train_raw$salario)) / nrow(train_raw)

train_raw %>% 
	filter(sex == 'X') %>% 
	dplyr::select(primer_nombre) %>% 
	head(20)

 
train_raw <- train_raw %>% 
	clean_names()

train_raw %>% 
	glimpse()


sum(train_raw_v2$salario)
# 211,256,702

train_raw <- train_raw %>% 
	mutate(nombre = gsub("\"", "", nombre),
		nombre = stringr::str_trim(nombre, side = "right")) %>% 
	mutate(total = salario + gasto) %>% 
	dplyr::select(
		code, nombre, apellido, cedula, cargo, salario, gasto, total, 
		estado, fecha_inicio, primer_nombre, name, last_update, sex
		)
	
table(is.na(train_raw$fecha_inicio))
train_raw %>% 
	filter(is.na(fecha_inicio) == TRUE)


train_raw[9,]

train_raw$record_date <- Sys.time()
train_raw <- train_raw %>% 
	mutate(url = 'http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp') %>% 
	dplyr::select(code, name, url, nombre, apellido, cedula, cargo, salario, gasto, estado,
		fecha_inicio, primer_nombre, total, sex, last_update, record_date) 

write.csv(train_raw, paste0(PATH_OUT, "out_centralgov_salaries.csv"), row.names = FALSE)

# ***********************************************
# EDA Objectives ----
# 	1. Basic statistical measure, and distribution checking.
# 				1.1 Character
# 				1.1 Numeric
# 				1.1 Correlation

# By feature
#   >> Missing value analysis: 
#   >> Data gap analysis for each attribute
#   >> Logical attribute check
#   >> Business data check

# By feature
# Suvirval check (% chunk by month)
# Pt - Purcharsing trend

# Attribute relevance 
# Weight of Evidence  (WoE) and Information Value  (IV) in case of Binary Target Variable (pag 179)
# ***********************************************


# Basic statistical measure, and distribution checking ----
numeric_cols <- train_raw %>% 
	select_if(is.numeric) %>% 
	colnames()
length(numeric_cols) # total numeric cols : 106
numeric_cols

character_cols <- train_raw %>% 
	select_if(is.character) %>% 
	colnames()
length(character_cols) # total numeric cols : 16
character_cols

# all features with missing values (numerical and character)
withnull_names <- train_raw %>% 
	map_df(is.na) %>% 
	gather(factor_key = TRUE) %>% 
	filter(value == TRUE) %>% 
	distinct(key) %>% 
	pull(key) %>% 
	as.character()
withnull_names


# Contigency tables for all categorical predictors
summary_categorical <- train_raw %>% 
	#select_if(is.character) %>% 
	dplyr::select(name, cargo, estado) %>% 
	gather("feature", "value", 2:3) %>% 
	summary_contigency_table(name, col = value) %>% 
	mutate(
		value = ifelse(is.na(value) == TRUE, "XNA", value), 
		Colname = NULL
	) 

summary_categorical %>% 
	arrange(desc(n))

# contigency table by entity
review_name <-  train_raw %>% 
	summary_col_by_group(name, col = salario) 
View(review_name)

# contigency table by role 
review_cargo <-  train_raw %>% 
	summary_col_by_group(cargo, col = salario) 
View(review_cargo)

# 
last_empleyee <- train_raw %>% 
	mutate(
		year = year(fecha_inicio)
		) %>% 
	filter(year > 2018) %>% 
	summary_col_by_group(name, cargo, col = salario) %>% 
	arrange(desc(Count)) %>% 
	head(20)
View(last_empleyee)

top_cargos <- train_raw %>% 
	filter(cargo %in% last_empleyee$cargo)

manual <- top_cargos %>% 
		mutate(
		year = year(fecha_inicio)
		) %>% 
	filter(cargo == "TRABAJADOR MANUAL  I") %>% 
	count(cargo, year, estado) %>% 
	arrange(desc(n))

	
	


