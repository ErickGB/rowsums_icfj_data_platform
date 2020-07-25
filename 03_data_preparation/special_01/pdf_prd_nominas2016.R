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
PATH_IN <- "./00_data/in/salaries/prd/"
PATH_OUT <- "./00_data/out/"

# get_people_id("8-707-2100")
get_people_id <- function(key) {
	key_02 <- str_split(key, pattern = "-")[[1]][2]
	key_03 <- str_split(key, pattern = "-")[[1]][3]
	#paste0(substr("0000", 1, 4 - nchar(key_02)), key_02)
	#paste0(substr("00000", 1, 5 - nchar(key_03)), key_03)
	people_id <- paste0(str_split(key, pattern = "-")[[1]][1], "-", 
											paste0(substr("0000", 1, 4 - nchar(key_02)), key_02), "-",
											paste0(substr("00000", 1, 5 - nchar(key_03)), key_03))
	return(people_id)		
}

# split txt data
get_split_value <- function (value, position) {
	split_value <- str_split(value, pattern = " ")[[1]][position]
	split_value <- ifelse(is.na(split_value) == TRUE, "", as.character(split_value))
	return(split_value)
}
# ****************************************************
#pdf_raw_data <- staplr::get_fields(input_filepath = paste0(PATH_IN, "BOLETIN_3956-A.pdf"))
metadata_tbl <- tibble(
file_name = c("06-20_Directivas_de_area.pdf", "21-32_Directores 1.pdf", "33-46_Directores 2.pdf", 
							"47-59_Directores 3.pdf", "60-60_FF nacional.pdf", "61-81_FF area.pdf", 
							"94-94_Juventud Nacional.pdf", "95-98_Juventud Nacional.pdf", 
							"04-04_CEN.pdf", "05-05_Defensor.pdf"),
columns = c(10, 10, 11, 12, 5, 9, 5, 5, 5, 5)
) 


get_pdf_tbl <- function(id) {
	print(paste0("Loading... ", metadata_tbl$file_name[id]))
	pdf_raw <- extract_tables(paste0(PATH_IN, metadata_tbl$file_name[id]), 
														method = "stream")
	str(pdf_raw)
	length(pdf_raw)
	
	record_tbl <- tibble()
	for(sheet in 1:length(pdf_raw)) {
		sheet_pdf <- as.data.frame(pdf_raw[[sheet]])
		print(paste0("Rows:", nrow(sheet_pdf), " Columns:", ncol(sheet_pdf) ))
		if(ncol(sheet_pdf) == metadata_tbl$columns[id])
		{
			if(sheet == 1) {
				record_tbl <- sheet_pdf
			} else {
				record_tbl <- rbind(record_tbl, sheet_pdf)
			}
		}
	}
	return(record_tbl)
}


# *******************************
# Directiva

directiva_tbl <- get_pdf_tbl(1)
colnames(directiva_tbl) <- c("area", "circuito", "provincia", "orden_nomina", "cargo", "x1", 
													"cedula", "nombres", "paterno", "materno")

directiva_tbl <- directiva_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		cargo_ajustado = ifelse(nchar(cargo) == 1, x1, cargo),
		orden_nomina = str_trim(orden_nomina, side = 'both'),
		orden_nomina = ifelse(nchar(cargo) == 1, paste0(orden_nomina, " ", cargo), orden_nomina), 
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		orden =  map2_chr(orden_nomina, 1, get_split_value),
		nomina =  map2_chr(orden_nomina, 2, get_split_value),
		rol = "Directores de área",
		x1 = NULL
				 ) %>% 
	filter(record_id > 1) %>% 
	dplyr::select(area, circuito, provincia, cedula, person_id, cargo_ajustado, nombres, paterno, materno, orden, nomina, rol)


directiva_tbl %>% 
	glimpse()

View(directiva_tbl)
write.csv(directiva_tbl, paste0(PATH_OUT, "directiva_area.csv"), row.names = FALSE)

# *******************************
# Directores 1
metadata_tbl[2,]
directores1_tbl <- get_pdf_tbl(2)
colnames(directores1_tbl) <- c("area", "provincia", "nomina", "orden", "posición", "cargo", 
														 "cedula", "nombres", "paterno", "materno")

directores1_tbl <- directores1_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		rol = "Directores 1"
	)

View(directores1_tbl)
directores1_tbl %>% 
	glimpse()

write.csv(directores1_tbl, paste0(PATH_OUT, "directores_1.csv"), row.names = FALSE)



# *******************************
# Directores 2

metadata_tbl[3,]
directores2_tbl <- get_pdf_tbl(3)
colnames(directores2_tbl) <- c("area", "provincia", "distrito", "nomina", "orden", "posición", "cargo", 
															 "cedula", "nombres", "paterno", "materno")

directores2_tbl <- directores2_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		rol = "Directores 2"
	)

View(directores2_tbl)
directores2_tbl %>% 
	glimpse()

write.csv(directores2_tbl, paste0(PATH_OUT, "directores_2.csv"), row.names = FALSE)


# *******************************
# Directores 3

metadata_tbl[4,]
directores3_tbl <- get_pdf_tbl(4)
colnames(directores3_tbl) <- c("area", "provincia", "distrito", "corregimiento",  "nomina", "orden", "posición", "cargo", 
															 "cedula", "nombres", "paterno", "materno")

directores3_tbl <- directores3_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		rol = "Directores 3"
	)

View(directores3_tbl)
directores3_tbl %>% 
	glimpse()

write.csv(directores3_tbl, paste0(PATH_OUT, "directores_3.csv"), row.names = FALSE)

# *******************************
# FF nacional

metadata_tbl[5,]
ffnacional_tbl <- get_pdf_tbl(5)
colnames(ffnacional_tbl) <- c("cargo", "cedula", "nombres", "paterno", "materno")

ffnacional_tbl <- ffnacional_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.))
	) %>% 
	filter(record_id > 4) 	%>% 
	mutate( 
		cargo = str_trim(cargo, side = 'both'),
		cedula_ajustada = ifelse(record_id >= 19, cargo, cedula), 
		cargo_ajustada = ifelse(record_id >= 19, materno, cargo),
		nombre_ajustada = ifelse(record_id >= 19, cedula, nombres),
		paterno_ajustada = ifelse(record_id >= 19, nombres, paterno),
		materno_ajustada = ifelse(record_id >= 19, paterno, materno),
		person_id = map_chr(cedula_ajustada, get_people_id), # standarize "cedula"
		rol = "FF Nacional"
	) 

View(ffnacional_tbl)
ffnacional_tbl %>% 
	glimpse()

write.csv(ffnacional_tbl, paste0(PATH_OUT, "ff_nacional.csv"), row.names = FALSE)


# *******************************
# FF nacional

metadata_tbl[6,]
ffarea <- get_pdf_tbl(6)
colnames(ffarea) <- c("area", "provincia", "orden", "nomina", "cargo", "cedula", "nombres", "paterno", "materno")

ffarea <- ffarea %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		cargo = ifelse(nchar(nomina) > 2, nomina, cargo),
		provincia_ajustada = ifelse(nchar(nomina) > 2, map2_chr(provincia, 1, get_split_value), provincia),
		nomina_ajustada = ifelse(nchar(nomina) > 2, map2_chr(provincia, 2, get_split_value), nomina),
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		rol = "FF Area"
	) 

View(ffarea)
ffarea %>% 
	glimpse()

write.csv(ffarea, paste0(PATH_OUT, "ff_area.csv"), row.names = FALSE)


# *******************************
# FF nacional

metadata_tbl[7,]
juventud_nac_tbl <- get_pdf_tbl(7)
colnames(juventud_nac_tbl) <- c("cedula", "nombres", "paterno", "materno", "cargo")

juventud_nac_tbl <- juventud_nac_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		rol = "Juventud Nacional"
	) %>% 
	filter(record_id != 2)

View(juventud_nac_tbl)
juventud_nac_tbl %>% 
	glimpse()

write.csv(juventud_nac_tbl, paste0(PATH_OUT, "juventud_nac.csv"), row.names = FALSE)

# *******************************
# FF nacional

metadata_tbl[8,]
juventud_nac_2_tbl <- get_pdf_tbl(8)
colnames(juventud_nac_2_tbl) <- c( "cargo", "cedula", "nombres", "paterno", "materno")

juventud_nac_2_tbl <- juventud_nac_2_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		rol = "Juventud Nacional 2"
	) %>% 
	filter(record_id != 2)

View(juventud_nac_2_tbl)
juventud_nac_2_tbl %>% 
	glimpse()

write.csv(juventud_nac_2_tbl, paste0(PATH_OUT, "juventud_2_nac.csv"), row.names = FALSE)


# *******************************
# CEN
metadata_tbl[9,]
cen_tbl <- get_pdf_tbl(9)
colnames(cen_tbl) <- c( "cedula", "nombres", "paterno", "materno", "cargo")

cen_tbl <- cen_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		rol = "CEN"
	)

write.csv(cen_tbl, paste0(PATH_OUT, "CEN.csv"), row.names = FALSE)

# *******************************
# CEN
metadata_tbl[10,]
defensor_tbl <- get_pdf_tbl(10)
colnames(defensor_tbl) <- c("cargo", "cedula", "nombres", "paterno", "materno")

defensor_tbl <- defensor_tbl %>% 
	mutate_if(is.factor, as.character) %>% 
	mutate(
		record_id = as.numeric(rownames(.)),
		cargo = str_trim(cargo, side = 'both'),
		person_id = map_chr(cedula, get_people_id), # standarize "cedula"
		rol = "Defensor"
	)

write.csv(defensor_tbl, paste0(PATH_OUT, "Defensor.csv"), row.names = FALSE)




# *******************************
# test 
rm(total_tbl)
total_tbl <-  directores1_tbl %>% 
	select(person_id, rol, nomina) %>% # nomina  
	bind_rows(
		directores2_tbl %>% 
			select(person_id, rol, nomina) # nomina  
	) %>% 
	bind_rows(
		directores3_tbl %>% 
			select(person_id, rol, nomina) # nomina  
	) %>%
	bind_rows(
		ffarea %>% 
			mutate(nomina = nomina_ajustada) %>% 
			select(person_id, rol, nomina) # nomina_ajustada 
	) %>%
	bind_rows(
		ffnacional_tbl %>% 
			mutate(nomina = "NoAplica") %>% 
			select(person_id, rol, nomina)
	) %>%
	bind_rows(
		juventud_nac_tbl %>% 
			mutate(nomina = "NoAplica") %>% 
			select(person_id, rol, nomina)
	) %>%
	bind_rows(
		juventud_nac_tbl %>% 
			mutate(nomina = "NoAplica") %>% 
			select(person_id, rol, nomina)
	) %>% 
	count(person_id, rol, nomina)

View(total_tbl)
total_tbl %>% 
	glimpse()




# ***********
# planilla 
planilla_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/1_central_gov_salaries_march.csv")
css_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/2_central_css_gov_salaries_march.csv")
contrata_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/3_dir_contrataciones_publicas_gov_salaries_march_2020_processed_2020-05-17.csv")
consumidor_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/5_aut_consumidor_gov_salaries_march_2020_processed_2020-05-17.csv")


planilla_tbl <- planilla_tbl %>% 
	mutate(start_date = as.Date(start_date, tryFormats = c("%d/%m/%Y"))) %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)
css_tbl<- css_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)
contrata_tbl<- css_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)
consumidor_tbl<- css_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)


contrata_tbl %>% 
	glimpse()

planilla_tbl %>% 
	glimpse()

planilla_tbl <- planilla_tbl %>% 
	bind_rows(css_tbl) %>% 
	bind_rows(contrata_tbl) %>% 
	bind_rows(consumidor_tbl) 


total_tbl <- left_join(total_tbl, planilla_tbl, by = 'person_id')
total_tbl %>% 
	DataExplorer::plot_missing()

prd_gov <- total_tbl %>% 
	filter(is.na(first_name) == FALSE) %>% 
	mutate(is_new = ifelse(start_date >= as.Date("01-07-2019", "%d-%m-%Y"), "yes", "no"))

prd_gov %>% 
	glimpse()

table(prd_gov$is_new)
table(prd_gov$nomina)
table(prd_gov$rol)


table(prd_gov$is_new) / nrow(prd_gov)
table(prd_gov$entity, prd_gov$is_new)
table(prd_gov$rol, prd_gov$nomina)
table(prd_gov$rol, prd_gov$is_new)

table(total_tbl$rol)
178 / 551
193 / 633
148 /461
279 /907
9 /20 
5 /10



# 952 / 2245 

# *************************
# CEN
cen_raw <- text_raw[[4]]
cen_lines <- length(text_raw[[4]])

cen_tbl <- tibble(
	person_id = stringr::str_trim(substr(cen_raw[3:cen_lines], 1, 12), side = "both"), # cedula
	nomber = stringr::str_trim(substr(cen_raw[3:cen_lines], 14, 34), side = "both"), # nombre
	apellido_paterno = stringr::str_trim(substr(cen_raw[3:cen_lines], 35, 53), side = "both"), # apellido paterno
	apellido_materno = stringr::str_trim(substr(cen_raw[3:cen_lines], 54, 72), side = "both"), # apellido materno
	cargo = stringr::str_trim(substr(cen_raw[3:cen_lines], 73, 93), side = "both") # apellido materno
)

cen_tbl <- cen_tbl %>%
	filter(person_id != "")

cen_tbl %>% 
	glimpse()

rm(cen_raw, cen_lines)
# *************************
# DEFENSOR
defensor_raw <- text_raw[[5]]
defendor_lines <- length(text_raw[[5]]) - 2

defensor_tbl <- tibble(
	cargo = stringr::str_trim(substr(defensor_raw[4:defendor_lines], 1, 12), side = "both"), # cedula
	person_id = stringr::str_trim(substr(defensor_raw[4:defendor_lines], 13, 24), side = "both"), # nombre
	nombres = stringr::str_trim(substr(defensor_raw[4:defendor_lines], 25, 43), side = "both"), # apellido paterno
	apellido_paterno = stringr::str_trim(substr(defensor_raw[4:defendor_lines], 44, 56), side = "both"), # apellido materno
	apellido_materno = stringr::str_trim(substr(defensor_raw[4:defendor_lines], 57, 93), side = "both") # apellido materno
)
defensor_tbl
#"Principal   8-302-573    Edgar               Zachrisson   Mitre"
# 123456789012345678901234567890123456789012345678901234567890123
 
rm(defensor_raw, defendor_lines)

# *************************
# Paul Radu 
# DIRECTIVAS
#"BOCAS DEL TORO   1-1        BOCAS DEL TORO        0      1 Presidente                                          8-724-855    Benisio Enisio         Robinson      Gonzalez"  
# 1234567890123456789012345678901234567890123456789012345678901231234567890123456789012345678901234567890123456789012345678901231234567890123456789012345678901234567890123
#"COLON 2   3-2   COLON    3   2   Sec. de Organizacion                                3-724-2135   Yamileth Yariza      Molinar        De Leon"  

# directiva 1 : 6 a 32... 6,3
get_directiva_tbl(7, 1)
get_directiva_tbl <- function(page, start_line, rol_text) {
	print(paste0("page: ", page, " line: ", start_line))
	directivas_raw <- text_raw[[page]] # página 6
	directivas_lines <- length(text_raw[[page]]) - 2
	start_line <- 3
	directivas_tbl <- tibble(
		provincia = stringr::str_trim(substr(directivas_raw[start_line:directivas_lines], 1, 16), side = "both"), # cedula
		circuito = stringr::str_trim(substr(directivas_raw[start_line:directivas_lines], 17, 23), side = "both"), # cedula
		cargo = stringr::str_trim(substr(directivas_raw[start_line:directivas_lines], 59, 75), side = "both"), # cedula
		person_id = stringr::str_trim(substr(directivas_raw[start_line:directivas_lines], 106, 120), side = "both"), # cedula
		nombre = stringr::str_trim(substr(directivas_raw[start_line:directivas_lines], 121, 135), side = "both"), # cedula
		apellido_paterno = stringr::str_trim(substr(directivas_raw[start_line:directivas_lines], 140, 156), side = "both"), # cedula
		apellido_materno = stringr::str_trim(substr(directivas_raw[start_line:directivas_lines], 158, 170), side = "both"), # cedula
	) 
	# limpia los registros de cabecera
	directivas_tbl <- directivas_tbl %>% 
		filter(provincia != "") %>% 
		mutate(rol = rol_text, page = page)
	
	return (directivas_tbl)
}


text_raw[[7]]
# 6 a 32  32 - 6
# 6 a 46   
pages_tbl <- tibble( 
	page = c(6, seq(from = 7, to = 59, by = 1)),
	start_page = c(3, rep(1, 53)), 
	rol = c(rep("directores 1", 1, 27), rep("directores 2", 1, 14), rep("directores 3", 1, 13))
)

pages_tbl <- pages_tbl %>% 
	mutate(data = pmap(list(page, start_page, rol), get_directiva_tbl)) %>% 
	unnest()

pages_tbl %>% 
	glimpse()

pages_tbl %>% 
	count(rol1)

pages_tbl %>% 
	count(provincia) %>% 
	arrange(desc(n))

