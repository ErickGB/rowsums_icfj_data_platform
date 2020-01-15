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

sheet_start <- 2 
finish_start <- 2 
# ****************************************************
pdf_raw_data <- staplr::get_fields(input_filepath = paste0(PATH_IN, "BOLETIN_3956-A.pdf"))
pdf_raw_data <- pdftools::pdf_text(paste0(PATH_IN, "BOLETIN_3956-A.pdf"))
text_raw <- strsplit(pdf_raw_data, "\n")
  head(text_raw[[64]], 20) # sheet number

pdf_raw_2 <- extract_tables(paste0(PATH_IN, "BOLETIN_3956-A.pdf"))
cen_tbl <- as.data.frame(pdf_raw_2[[1]])
cen_tbl <- rbind(cen_tbl, as.data.frame(pdf_raw_2[[2]]))
cen_tbl$type <- "CEN"

defendor_tbl <- as.data.frame(pdf_raw_2[[3]])
defendor_tbl <- rbind(defendor_tbl, as.data.frame(pdf_raw_2[[4]]))
defendor_tbl$type <- "DEFENSOR"

final_tbl <- as.data.frame(pdf_raw_2[[5]])
cols <- c(10)
for(i in 6:154)
{
	temp_tbl <- as.data.frame(pdf_raw_2[[i]])
	cols <- c(cols, ncol(temp_tbl))
	print(paste0("page: ", as.character(i)," cols: ", ncol(temp_tbl), " rows: ", nrow(temp_tbl)))
	if(ncol(temp_tbl) == 10) {
		final_tbl <- rbind(final_tbl, temp_tbl)
	}
}
table(cols)
colnames(final_tbl) <- c("area_org", "circuito",  "provincia", "orden", "posición", "cargo", "cedula", "nombre", "apellido_paterno", "apellido_materno")
final_tbl %>% 
	glimpse()


pdf_raw_2[[77]]
pdf_raw_2[[78]]







# 10.. no
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
	head()

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

