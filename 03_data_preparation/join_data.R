
cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(purrr)     # Functional programming
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(XML)
# ***********************************************
PATH_OUT <- "./00_data/out/salaries/"
date_time <- as.character(Sys.Date())


gov_salaries_abr_tbl <- readr::read_csv(paste0(PATH_OUT, "out_planilla_01042019v3.csv"))
gov_salaries_may_tbl <- readr::read_csv(paste0(PATH_OUT, "out_centralgov_salaries_at_2019-05-18.csv"))
gov_salaries_jun_tbl <- readr::read_csv(paste0(PATH_OUT, "central_gov_salaries_jun.csv"))

# performs sex estimation by name ----
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

gov_salaries_abr_tbl <- gov_salaries_abr_tbl %>% 
	mutate(
		sex = sapply(primer_nombre, function(x) get_sex_by_name(x))
		)



colnames(gov_salaries_abr_tbl)
colnames(gov_salaries_may_tbl)
colnames(gov_salaries_jun_tbl)

table(gov_salaries_abr_tbl$last_update)
table(gov_salaries_may_tbl$last_update)
table(gov_salaries_jun_tbl$update_date)

# ********
# abr 
gov_salaries_abr_tbl <- gov_salaries_abr_tbl %>% 
	rename(
		codigo = code, 
		entidad = name
		) %>% 
	mutate(
		url = "http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp", 
		record_date = last_update
		) %>% 
	select(
		codigo, entidad, url, nombre, apellido, cedula, cargo, salario, gasto, estado, 
		fecha_inicio, primer_nombre, total, last_update, record_date, sex
		)


# ********
# junio 
gov_salaries_jun_tbl <- gov_salaries_jun_tbl %>%
	rename(
		codigo = code, entidad = entity, nombre = complete_name, 
		apellido = last_name, cedula = person_id, cargo = position, 
		salario = salary, gasto = expenses, estado = status, 
		fecha_inicio = start_date, primer_nombre = first_name, 
		total = total_income, last_update = update_date
		) %>% 
	mutate(
		url = "http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp",
		record_date = date_time
		) %>% 
	select(
		codigo, entidad, url, nombre, apellido, cedula, cargo, salario, gasto, estado, 
		fecha_inicio, primer_nombre, total, last_update, record_date, sex
		)

gov_salaries_jun_tbl %>% 
	glimpse()

colnames(gov_salaries_abr_tbl)
colnames(gov_salaries_may_tbl)
colnames(gov_salaries_jun_tbl)

gov_salaries_abr_tbl$record_date[1:10]
gov_salaries_may_tbl$record_date[1:10]
gov_salaries_jun_tbl$record_date[1:10]

gov_salaries_abr_tbl$record_date <- as.Date("2019-04-03")
gov_salaries_may_tbl$record_date <- as.Date("2019-05-18")
gov_salaries_jun_tbl$record_date <- as.Date("2019-06-08")

gov_salaries_abr_tbl <- gov_salaries_abr_tbl %>% 
	mutate(
		fecha_inicio = as.Date(fecha_inicio, tryFormats = c("%d/%m/%Y")),
		last_update = as.Date(last_update, tryFormats = c("%d/%m/%Y")), 
		cedula = stringr::str_trim(as.character(cedula), side = "both"),
		nombre = stringr::str_trim(as.character(nombre), side = "both"),
		apellido = stringr::str_trim(as.character(apellido), side = "both"),
		cargo = stringr::str_replace(stringr::str_trim(as.character(cargo), side = "both"), " ", "_"),
		entidad = stringr::str_trim(as.character(entidad), side = "both"),
		key = paste(cedula, as.character(fecha_inicio), cargo, sep = "_")
		)

gov_salaries_may_tbl <- gov_salaries_may_tbl %>% 
	mutate(
		cedula = stringr::str_trim(as.character(cedula), side = "both"),
		nombre = stringr::str_trim(as.character(nombre), side = "both"),
		apellido = stringr::str_trim(as.character(apellido), side = "both"),
		cargo = stringr::str_replace(stringr::str_trim(as.character(cargo), side = "both"), " ", "_"),
		entidad = stringr::str_trim(as.character(entidad), side = "both"),
		key = paste(cedula, as.character(fecha_inicio), cargo, sep = "_")
		)

gov_salaries_jun_tbl <- gov_salaries_jun_tbl %>% 
	mutate(
		cedula = stringr::str_trim(as.character(cedula), side = "both"),
		nombre = stringr::str_trim(as.character(nombre), side = "both"),
		apellido = stringr::str_trim(as.character(apellido), side = "both"),
		cargo = stringr::str_replace(stringr::str_trim(as.character(cargo), side = "both"), " ", "_"),
		entidad = stringr::str_trim(as.character(entidad), side = "both"),
		key = paste(cedula, as.character(fecha_inicio), cargo, sep = "_")
		)


gov_salaries_abr_tbl %>% 
	glimpse()

gov_salaries_may_tbl %>% 
	glimpse()

gov_salaries_jun_tbl %>% 
	glimpse()


final_tbl <- rbind(gov_salaries_abr_tbl, gov_salaries_may_tbl)
final_tbl <- rbind(final_tbl, gov_salaries_jun_tbl)

final_null_tbl <- final_tbl %>% 
	filter(is.na(fecha_inicio) == TRUE) # 9 registros

final_tbl <- final_tbl %>% 
	filter(is.na(fecha_inicio) == FALSE)
table(final_tbl$record_date)

#write.csv(final_tbl, paste0(PATH_OUT, "out_jun-may-abr.csv"), row.names = FALSE)


gov_salaries_jun_tbl <- gov_salaries_jun_tbl %>% 
	filter(is.na(start_date) == FALSE)	
write.csv(gov_salaries_jun_tbl, paste0(PATH_OUT, "gov_salaries_jun_2.csv"), row.names = FALSE)

final_tbl$entidad <- ifelse(final_tbl$entidad == 'Otros Gastos de la Administracion', 
	'Otros Gastos de la Administración', final_tbl$entidad) 

final_tbl$entidad <- ifelse(final_tbl$entidad == 'Ministerio de Comercio e Industria', 
	'Ministerio de Comercio e Industrias', final_tbl$entidad) 
	
final_tbl %>% 
	distinct(entidad, record_date) %>% 
	count(entidad) %>% 
	arrange(n)


# return all rows from x where there are not matching values in y, keeping just columns from x.
out_people_jobs_may_tbl <- anti_join(gov_salaries_abr_tbl, gov_salaries_may_tbl,  by="key")
out_people_jobs_jun_tbl <- anti_join(gov_salaries_may_tbl, gov_salaries_jun_tbl, by="key")

out_people_jobs_may_tbl$finish_date <-  as.Date("2019-05-01")
out_people_jobs_jun_tbl$finish_date <-  as.Date("2019-06-01")

out_people_jobs_may_tbl %>% 
	count(entidad, cargo) %>% 
	arrange(desc(n))

out_people_jobs_jun_tbl %>% 
	count(entidad, cargo) %>% 
	arrange(desc(n))

finised_tbl <- rbind(out_people_jobs_may_tbl, out_people_jobs_jun_tbl)
finised_tbl %>% 
	glimpse()
table(finised_tbl$finish_date)

write.csv(finised_tbl, paste0(PATH_OUT, "out_finished_may_jun_people.csv"), row.names = FALSE)


# ********************************************************
abr_people_tbl <- gov_salaries_abr_tbl %>% 
	group_by(cedula, nombre, apellido) 
	select(cedula, nombre, apellido, fecha_inicio)

may_people_tbl <- gov_salaries_may_tbl %>% 
	select(cedula, nombre, apellido, fecha_inicio, record_date)

jun_people_tbl <- gov_salaries_jun_tbl %>% 
	select(cedula, nombre, apellido, fecha_inicio, record_date)

people_tbl <- anti_join(may_people_tbl, abr_people_tbl, by = 'cedula')
people_tbl <- anti_join(jun_people_tbl, people_tbl, by = 'cedula')

people_tbl %>% 
	count(cedula) %>% 
	filter(n > 1)

final_tbl %>% 
	filter(cedula == '1-0018-02415') %>% 
	select(cedula, nombre, apellido, cargo, fecha_inicio, record_date, total)


## set bucket via environment
Sys.setenv(
	"GCS_CLIENT_ID" = "mykey",
  "GCS_CLIENT_SECRET" = "1084727730786-stm0iqvk0r7b1r641c85pt6fjd59tmpo.apps.googleusercontent.com",
  #"GCS_WEB_CLIENT_ID" = "my-shiny-key",
  #"GCS_WEB_CLIENT_SECRET" = "my-shiny-secret-key",
  "GCS_DEFAULT_BUCKET" = "rowsums.com",
  #"GCS_AUTH_FILE" = "/fullpath/to/service-auth.json"
	)

Sys.setenv("GCS_DEFAULT_BUCKET" = "rowsums.com")
library(googleCloudStorageR)
## first time this will send you to the browser to authenticate
gcs_auth() # Sys.setenv("GCS_AUTH_FILE" = "/fullpath/to/auth.json") # Auto-authentication

gcs_upload(paste0(PATH_OUT, "out_finished_may_jun_people.csv"))

## upload an R data.frame directly - will be converted to csv via write.csv
#gcs_upload(finised_tbl)





