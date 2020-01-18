cat('\014')
# libraries ----
library(tidyverse) # base data manipulation
library(lubridate) # date processing
library(furrr)     # Parallel Processing using purrr (iteration)
library(purrr)     # Functional programming
library(stringr)   # string processing
#library(pdftools)  # pdf reader library
library(staplr)  # pdf reader library
library(tabulizer)
# ****************************************************
PATH_IN <- "./00_data/in/salaries/"
PATH_OUT <- "./00_data/out/salaries/"

sheet_start <- 2 
finish_start <- 2 
date_time <- as.character(Sys.Date()) # process execution day
last_update <- paste0(substr(date_time, 1, 8), "01") # execution month

process_date <- as.Date(last_update) - as.difftime(1, unit = "days") # data of the month ...
process_month <- tolower(month.name[as.integer(paste0(substr(process_date, 6, 7)))])

# ****************************************************
# base table record
record_tbl <- tibble(
	cedula = character(),       # CEDULA
	nombre = character(),       # NOMBRE
	status = character(),       # CONDICION_EN_PLANILLA
	salary = character(),       # SALARIO_FIJO 
	overcost = character(),     # SOBRESUELDO
	access = character(),       # DIFICIL_ACCESO
	vocational = character(),   # COMP_VOCACIONA
	others = character(),       # SUP_Y_OTRAS_COMPEN
	expenses = character()      # GTOSDEREPRESE
)
# ****************************************************
# functions ----
source("00_scripts/etl_functions.R")

# Process a line from the pdf file and return it as a tibble table
# get_employee(text_raw[[1]][3]) 
get_employee <- function(text_line) {
	# text_line <- raw_data[3]
	data_text <- str_split(text_line, pattern = " ")[[1]]
	text_tbl <- tibble(raw = data_text) %>% 
		filter(raw != "")
	text_tbl
	
	rows <- nrow(text_tbl)
	people_row <- tibble(
		cedula = as.character(text_tbl[3, 1]),         # CEDULA
		nombre = paste(unlist(text_tbl[4:(rows-7),]), collapse=' '),       # NOMBRE
		status = as.character(text_tbl[rows-6,]),       # CONDICION_EN_PLANILLA
		salary = as.character(text_tbl[rows-5,]),       # SALARIO_FIJO 
		overcost = as.character(text_tbl[rows-4,]),     # SOBRESUELDO
		access = as.character(text_tbl[rows-3,]),       # DIFICIL_ACCESO
		vocational = as.character(text_tbl[rows-2,]),   # COMP_VOCACIONA
		others = as.character(text_tbl[rows-1,]),       # SUP_Y_OTRAS_COMPEN
		expenses = as.character(text_tbl[rows,])      # GTOSDEREPRESE
	)
	return (people_row)
}

# Process a sheet from the pdf file and return it as a tibble table with many rows
# get_data(2)
get_data <- function(id) {
	raw_sheet <- text_raw[[id]]
	employee_tbl <- tibble(id_row = 1:length(raw_sheet))
	employee_tbl$sheet <- as.character(paste0("Sheet ", as.character(id)))
	
	employee_tbl <- employee_tbl %>% 
		mutate(features = future_map(raw_sheet[id_row], get_employee)) %>% 
		# unnest(cols = c("id_row", "sheet", "cedula", "nombre", "status", "salary", "overcost", "access", "vocational", "others", "expenses")) %>% 
		unnest()

	return(employee_tbl)
}



# ****************************************************
file_name <- "Planilla_Enero_2019.pdf"
#pdf_raw_data <- staplr::get_fields(input_filepath = paste0(PATH_IN, file_name))
pdf_raw_data <- pdftools::pdf_text(paste0(PATH_IN, file_name))
text_raw <- strsplit(pdf_raw_data, "\n")

length(text_raw)
length(text_raw[[1]])
length(text_raw[[1023]]) # sheet number


for(i in 1:length(text_raw)) {
	print(paste0("Sheet :", as.character(i), " in process..."))
	meduca_tbl <- get_data(i)
	record_tbl <- rbind(record_tbl, meduca_tbl)
}
nrow(record_tbl)
record_tbl %>% 
	filter(cedula != "CEDULA") %>% 
	head()


record_tbl <- record_tbl %>% 
	filter(cedula != "CEDULA") %>% 
	mutate(
		cedula = str_replace(str_trim(cedula), '"', ""),
		salary = str_replace(str_trim(salary), '"', ""),
		salary = str_replace(salary, ',', ""),

		overcost = str_replace(str_trim(overcost), '"', ""),
		overcost = str_replace(overcost, ',', ""),
		
		access = str_replace(str_trim(access), '"', ""),
		access = str_replace(access, ',', ""),
				
		vocational = str_replace(str_trim(vocational), '"', ""),
		vocational = str_replace(vocational, ',', ""),
		
		others = str_replace(str_trim(others), '"', ""),
		others = str_replace(others, ',', ""),
		
		expenses = str_replace(str_trim(expenses), '"', ""),
		expenses = str_replace(expenses, ',', ""),
	) %>% 
	mutate(
		cedula = map_chr(cedula, get_people_id), # standarize "cedula"
		codigo = "007",
		entidad = "Ministerio de Educación", 
		departament = "unknow",
		url = paste0("http://www.meduca.gob.pa/transparencia/articulo11/planilla/", file_name)
	) %>% 
	mutate(
		salary = as.numeric(salary),
		overcost = as.numeric(overcost),
		access = as.numeric(access),
		vocational = as.numeric(vocational),
		others = as.numeric(others),
		expenses = as.numeric(expenses), 
		total_income = salary + expenses + overcost + access + vocational + others
	)

record_tbl %>% 
	glimpse()

record_tbl %>% 
	DataExplorer::plot_missing()

record_tbl %>% 
	filter(is.na(expenses) | is.na(salary)) %>% 
	select(sheet, id_row, nombre, status, salary, overcost, expenses)



record_tbl <- record_tbl %>% 
	filter(is.na(expenses) == FALSE & is.na(salary) == FALSE) %>% 
	rename(
	code = codigo, person_id = cedula, 
	complete_name = nombre, last_name = apellido, position = cargo, start_date = fecha_inicio, first_name = primer_nombre, 
	entity = entidad, update_date = last_update
)  %>%	
	select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
				 start_date, first_name, entity, update_date, sex, url, record_date, key, 
				 over_costs, departament) 

write.csv(master_css_tbl, paste0(PATH_OUT, "central_meduca_gov_salaries_", process_month,".csv"), row.names = FALSE) 






cedula = as.character(text_tbl[3, 1]),         # CEDULA
nombre = paste(unlist(text_tbl[4:(rows-7),]), collapse=' '),       # NOMBRE
status = as.character(text_tbl[rows-6,]),       # CONDICION_EN_PLANILLA
salary = as.character(text_tbl[rows-5,]),       # SALARIO_FIJO 
overcost = as.character(text_tbl[rows-4,]),     # SOBRESUELDO
access = as.character(text_tbl[rows-3,]),       # DIFICIL_ACCESO
vocational = as.character(text_tbl[rows-2,]),   # COMP_VOCACIONA
others = as.character(text_tbl[rows-1,]),       # SUP_Y_OTRAS_COMPEN
expenses = as.character(text_tbl[rows,])      # GTOSDEREPRESE




# END
# ****************************************************
