cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(V8)				 # call javascript functions
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(XML)
library(stringr) 
library(DataExplorer)
library(janitor)
# ***********************************************
# load data ----
PATH_IN <- "./00_data/in/salaries/" 
PATH_OUT <- "./00_data/out/" 
date_time <- as.character(Sys.Date())

get_camel <- function(x){ #function for camel case get_camel('caTAR, DOHA')
		x <- tolower(x)
    capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
    sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}

judicial_tbl <- readr::read_csv(paste0(PATH_IN, "judicial/mayo_julo_2019.csv")) # "judicial/viajes_ene_abril.csv"
judicial_tbl <- judicial_tbl %>% 
	clean_names()

legislativo_tbl <- readr::read_csv(paste0(PATH_IN, "asamblea/jun_jul.csv")) # asamblea/viajes_resumen.csv
legislativo_tbl <- legislativo_tbl %>% 
	clean_names()

legislativo_tbl %>% 
	glimpse()

# ****************************************
# processing ----
legislativo_tbl %>% 
	head(10)

legislativo_tbl <- legislativo_tbl %>% 
	mutate(
		fecha_salida = as.Date(fecha_salida,  tryFormats = c("%m/%d/%y")),
		fecha_regreso = as.Date(fecha_regreso,  tryFormats = c("%m/%d/%y")),
		pais = map_chr(pais_ciudad, function(x) {return(str_split(x, pattern = "/")[[1]][1])}),
		ciudad = map_chr(pais_ciudad, function(x) {
			split <- str_split(x, pattern = "/") 
			value <- ifelse(length(split) > 1, str_split(x, pattern = "/")[[1]][2], split[[1]][2]) 
			return(value)
			}), 
		pais = get_camel(pais), ciudad = get_camel(ciudad)
		) %>% 
	rename(viatico = viaticos) %>% 
	select(nombre, cargo, pais, ciudad, descripcion, fecha_salida, fecha_regreso, pasaje, viatico, tipo, entidad, code)


judicial_tbl <- judicial_tbl %>% 
	mutate(
		fecha_salida = as.Date(inicio,  tryFormats = c("%d/%m/%Y")),
		fecha_regreso = as.Date(fin,  tryFormats = c("%d/%m/%Y")),
		ciudad = map_chr(mision, function(x) {return(str_split(x, pattern = ",")[[1]][1])}),
		pasaje = as.numeric(str_replace(pasaje_aereo, pattern = ",", "")),
		pasaje = ifelse(is.na(pasaje) == TRUE, 0, pasaje),
		pais = map_chr(mision, function(x) {
			split <- str_split(x, pattern = ",") 
			value <- ifelse(length(split) > 1, str_split(x, pattern = "/")[[1]][2], split[[1]][2]) 
			return(value)
			}), 
		pais = get_camel(pais), ciudad = get_camel(ciudad)
		) %>% 
	rename(cargo = despacho, descripcion = objetivo_de_la_mision) %>% 
	select(nombre, cargo, pais, ciudad, descripcion, fecha_salida, fecha_regreso, pasaje, viatico, tipo, entidad, code)

final_tbl <- rbind(judicial_tbl, legislativo_tbl) 

final_tbl <- final_tbl %>% 
	mutate(
		complete_name = stringr::str_replace(complete_name, "\"", ""),
		dias = difftime(strptime(fecha_regreso, format = "%Y-%m-%d"), strptime(fecha_salida, format = "%Y-%m-%d"),units="days"),
		dias = floor(as.numeric(dias)),
		viatico_diario = round(viatico / dias, digits = 2), 
		pais = ifelse(pais == 'NANA', ciudad, pais),
		
		pais = get_camel(stringr::str_trim(pais, side = "both")),
		ciudad = get_camel(stringr::str_trim(ciudad, side = "both")),
		ciudad = ifelse(pais == 'Dc', 'Washington', ciudad),
		pais = ifelse(pais == 'Dc', 'Estados unidos', pais),

		) %>% 
	arrange(desc(viatico_diario))

table(final_tbl$viatico_diario)
final_tbl$cedula = 'determinar'

final_tbl <- final_tbl %>% 
	mutate(
		nombre = stringr::str_replace(nombre, "í", "i"), 
		nombre = stringr::str_replace(nombre, "é", "e"), 
		nombre = stringr::str_replace(nombre, "á", "a"), 
		nombre = stringr::str_replace(nombre, "ó", "o"), 
		
		complete_name = toupper(nombre),
		key = paste(complete_name, entidad, sep = "_")
		) 

final_tbl %>% 
	select(complete_name, key, viatico, entidad) %>% 
	arrange(desc(viatico))

final_tbl %>% 
	glimpse()

employees_tbl <- readr::read_csv(paste0(PATH_OUT, "salaries/result_employee_salaries.csv"))
employees_tbl %>% 
	glimpse()

employees_tbl <- employees_tbl %>% 
	filter(entity_name %in% c("Asamblea Legislativa", "Presidencia de la República", "Organo Judicial")) %>% 
	mutate(
		first_name = stringr::str_replace(first_name, "í", "i"), 
		first_name = stringr::str_replace(first_name, "é", "e"), 
		first_name = stringr::str_replace(first_name, "á", "a"), 
		first_name = stringr::str_replace(first_name, "ó", "o"), 
		
		last_name = stringr::str_replace(last_name, "í", "i"), 
		last_name = stringr::str_replace(last_name, "é", "e"), 
		last_name = stringr::str_replace(last_name, "á", "a"), 
		last_name = stringr::str_replace(last_name, "ó", "o"), 
		
		first_name = purrr::map_chr(first_name, function(x) {stringr::str_trim(x, side = "both")}),
		last_name = purrr::map_chr(last_name, function(x) {stringr::str_trim(x, side = "both")}),
		complete_name = toupper(paste(first_name, last_name, sep = " ")),
		year_month = paste(lubridate::year(record_date), lubridate::month(record_date), sep = "_"),
		key = paste(complete_name, entity_name, sep = "_")
		) %>% 
	group_by(first_name, last_name, complete_name, key, entity_name, person_id) %>% 
	summarize(salary = max(salary)) %>% 
	ungroup()

final_tbl <- left_join(final_tbl, employees_tbl, by = 'key')

final_tbl <- final_tbl %>% 
	#filter(is.na(entity_name) == TRUE) %>% 
	select(nombre, cargo, pais, ciudad, descripcion, 
		fecha_salida, fecha_regreso, pasaje, viatico, tipo, 
		entidad, code, dias, viatico_diario, 
		person_id, salary, entity_name
		)
table(is.na(final_tbl$entity_name))

correccion_tbl <- readr::read_csv(paste0(PATH_IN, "asamblea/correccion.csv")) 
correccion_tbl

final_tbl <- left_join(final_tbl, correccion_tbl, by = 'nombre')
final_tbl <- final_tbl %>% 
	mutate(
		person_id = ifelse(is.na(person_id) == TRUE, cedula_cor, person_id),
		entity_name = ifelse(is.na(entity_name) == TRUE, entidad_e, entity_name),
		cedula = NULL, entidad_o = NULL 
		) %>% 
	select(person_id, nombre, cargo, code, entidad, pais, ciudad, descripcion, fecha_salida, fecha_regreso, tipo, entidad, code, dias, viatico_diario, 
		pasaje, viatico) %>% 
	gather(key, value, 14:15)


final_tbl %>% 
	glimpse()

final_tbl %>% 
	plot_missing()

final_tbl <- final_tbl %>% 
	select()


write.csv(final_tbl, paste0(PATH_OUT, "out_viajes_jul.csv"), row.names = FALSE)



