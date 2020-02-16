cat("\014")
gc()
# *******************************************************************************
# Load libraries ----
library(readxl)

# *******************************************************************************
PATH_IN <- "./00_data/in/salaries/"
get_meduca_employees("3")
get_meduca_employees <- function(id) {
	text_raw[[id]]
}

pdf_raw_data <- pdftools::pdf_text(paste0(PATH_IN, "Planilla_Enero_2019.pdf"))
text_raw <- strsplit(pdf_raw_data, "\n")

nchar(text_raw[[2]][1])
nchar(text_raw[[70]][1])
nchar(text_raw[[500]][1])
nchar(text_raw[[900]][1])

text_raw[[1]][2]
text_raw[[2]][1]
text_raw[[70]][1]
text_raw[[500]][1]
text_raw[[900]][1]


head(text_raw[[1]], 10) # sheet number
NROW(text_raw)
NROW(text_raw[[70]])


"254 29445 1-14-291   CLEMENTE HOOKER JOLE                ACTIVO                 1,275.90 488.32 100.00  0.00 0.00 0.00"
 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678
 
text <- "254 29445 1-14-291   CLEMENTE HOOKER JOLE                ACTIVO                 1,275.90 488.32 100.00  0.00 0.00 0.00" 
strsplit(text, " ")  
list_text <- strsplit(text, " ")
list_text[[1]][1] # PLANILLA
list_text[[1]][2] # POSICION
list_text[[1]][3] # CEDULA
# NOMBRE
# CONDICION_EN_PLANILLA - status
# SALARIO_FIJO
# SOBRESUELDO
# DIFICIL_ACCESO
# COMP_VOCACIONAL
# SUP_Y_OTRAS_COMPEN
# GTOSDEREPRESE


 
text <- substr(as.character(text_raw[[900]][2]), 1, 58)
text_raw[[2]][2]

 28  61 8-286-832  ITALINA OSILIS ESPINOSA RIQUELME    
557  62 4-722-1115 ZETH YODIER MARTINEZ SANCHEZ     
606 4024 9-202-553  ROSA DEL CARMEN ÁLVAREZ GÓMEZ     
162 77221 9-189-503   BIENEL QUINTERO QUINTERO    

strsplit(text, " ") 

# *******************************************************************************

# Table with results 1 to 3443
meduca_raw_tbl <- tibble(
	id = 2:29,
	entity = rep("Ministerio de Educación", 1, 28),
	site = rep("Planilla_Enero_2019.pdf", 1, 28)
)

meduca_raw_tbl <- meduca_raw_tbl %>%
	mutate(
		records = furrr::future_map(id, get_meduca_employees) 
	) 

meduca_raw_tbl <- meduca_raw_tbl %>% 
	unnest()

meduca_raw_tbl %>% 
	glimpse()
