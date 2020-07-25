cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(furrr)     # Parallel Processing using purrr (iteration)
library(purrr)     # Functional programming
# ***********************************************
PATH_OUT <- "./00_data/out/importations/"
PATH_IN <- "./00_data/in/imp/cnda_2019/"

get_file <- function (file_name) {
	print(paste("Loading file ", file_name, "..."))  
	file <- readr::read_csv(paste0(PATH_IN, file_name))
	file <- file %>% 
		janitor::clean_names()
	return(file)
}


# read file ----
list_files <- list.files(paste0(PATH_IN))
data_tbl <- tibble(file_name = list_files)
data_tbl 

# load data ----
data_tbl <- data_tbl %>% 
	mutate(tbl = future_map(file_name, get_file)) %>% 
	unnest()

data_tbl %>% 
	glimpse()

data_tbl %>% 
	head()

table(data_tbl$fields)

data_tbl %>% 
	filter(fields == 'Valor CIF:') %>% 
	group_by(company) %>% 
	summarize(total = sum(as.numeric(data_processed)), n = n() ) %>% 
	arrange(desc(total)) %>% 
	head(20)

numeric_fields <- c("Cantidad", "Impuestos ISC", "Impuestos ITBM", "Valor del Flete", "Valor CIF:", "Valor FOB:", "Total a Pagar:", 
	"Valor del Seguro:", "Valor del Seguro:")


grou_tbl <- data_tbl %>% 
	filter(fields %in% numeric_fields) %>% 
	select(file_name, id_page, date, ruc, company, origin, description, fields, data_processed) %>% 
	group_by(file_name, id_page, date, ruc, company, origin, description, fields) %>% 
	summarize(total = sum(as.numeric(data_processed)), n = n() ) %>% 
	spread(key = fields, value = total)
	
write.csv(grou_tbl, paste0(PATH_OUT, "out_imports.csv"))
