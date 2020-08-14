cat("\014")
gc() # garbage collector
# ***************************************************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(splashr)   # HTML javascript functions
library(magick)    # Simplify high-quality image processing in R
library(purrr)     # Functional programming
library(furrr)     # Parallel Processing using purrr (iteration)
# ***************************************************************************
url <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=31"
PATH_OUT <- "./00_data/out/salaries/"
entity_name <- "Autoridad Marítima de Panamá"
	
# ***********************************************
# functions ----
source("00_scripts/etl_functions.R")

get_amp_employee <- function(url) {
	body_html <- splash_local %>% 
		splash_go(url) %>% 
		splash_wait(5) %>% 
		splash_html()
	
	# we return all the tables on the page
	table_html <- body_html %>% 
		rvest::html_nodes(xpath = '//table')
	
	rows <- table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr')
	rows <- (length(rows) - 3)
	
	pcomplete_name <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[2]') %>% 
		rvest::html_text()
	
	pjob_position <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[3]') %>% 
		rvest::html_text()
	
	pstatus <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[4]') %>% 
		rvest::html_text()
	
	pdirection <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[5]') %>% 
		rvest::html_text()
	
	psalary <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[6]') %>% 
		rvest::html_text()
	
	pexpenses <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[7]') %>% 
		rvest::html_text()
	
	ptotal <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[8]') %>% 
		rvest::html_text()
	
	row_tbl <- tibble(
		complete_name = pcomplete_name,
		job_position = pjob_position,
		status = pstatus,
		direction = pdirection,
		salary = psalary,
		expenses = pexpenses,
		total = ptotal
	)
	return(row_tbl)
}

# ***********************************************
# Save images

# Start, active splash ----
splash("localhost") %>% splash_active()


# ***********************************************
# scraping CSS web page 
# ***********************************************

body_html <- splash_local %>% 
	splash_go(url) %>% 
	splash_wait(5) %>% 
	splash_html()


# ************************************     
# dates ---

# we return date 
table_span <- body_html %>% 
	rvest::html_nodes(xpath = '//span') 

page_date <- table_span[4] %>% 
	rvest::html_text()
page_date <- as.POSIXlt(page_date, tz = "UTC-5")
page_date

execution_date <- as.character(Sys.Date()) # process execution day
execution_month <- paste0(substr(execution_date, 1, 8), "01") # execution month

update_date <- as.Date(page_date)  # data of the month ...
update_month <- tolower(month.name[as.integer(paste0(substr(update_date, 6, 7)))])
update_year <- as.integer(substr(page_date, 1, 4))

data_date <- as.Date(paste0(substr(page_date, 1, 4), "/", substr(page_date, 6, 7), "/01"), 
										 tryFormats = c("%Y/%m/%d")) - as.difftime(1, unit = "days")
data_month <- tolower(month.name[as.integer(paste0(substr(data_date, 6, 7)))])

# dates 
page_date       # page -> last update
update_date     # page -> last update in date format

data_date       # What month of payment correspond .. last_date - 1 month
execution_date  # when I run the data extraction

# ************************************     
# 1. capture the last update
file_name <- paste0("./00_data/images/2020/amp/amp_last_update_", update_date,".png")
img_last <- render_png(url = url, wait = 5)
image_write(img_last, file_name)


# ************************************     




# we return all the tables on the page
table_html <- body_html %>% 
	rvest::html_nodes(xpath = '//table')

# how many data sheets to go?
count_page <- table_html[5]  %>% 
	rvest::html_nodes('tbody') %>% 
	rvest::html_nodes('tr') %>% 
	rvest::html_nodes(xpath = 'td[1]') %>% 
	rvest::html_text()
count <- str_split(count_page[1], pattern = " ")[[1]][4] %>% as.numeric()

update_text <- table_html[2] %>% 
	rvest::html_text()

records_tbl <- tibble(
	id = 1:count,
	code = rep("901", 1, count)
) %>% 
	mutate(
		url = paste0(url, paste0("p=", as.character(id)))
	)
records_tbl$url[1] <- url

time <- Sys.time()
amp_tbl <- records_tbl %>% 
	mutate(features = furrr::future_map(url, get_amp_employee)) %>% 
	unnest()
Sys.time() - time # Time difference of 6.748743 hours

amp_tbl %>% 
	head(10)


# ***********************************************
# cleaning data

final_tbl <- amp_tbl %>% 
	mutate(
		#person_id = str_replace(str_trim(person_id), '"', ""),
		complete_name = str_replace(str_trim(complete_name), '"', ""),
		#first_name = str_split(complete_name, " "),
		job_position = str_replace(str_trim(job_position), '"', ""),
		direction = str_replace(str_trim(direction), '"', ""),
		status = str_replace(str_trim(status), '"', ""),
		salary = str_replace(str_trim(salary), '"', ""),
		salary = str_replace(salary, ',', ""),
		expenses = str_replace(str_trim(expenses), '"', ""),
		expenses = str_replace(expenses, ',', ""),
		total = str_replace(str_trim(total), '"', ""), 
		total = str_replace(total, ',', ""),
	) %>% 
	mutate(
		#start_date = as.Date(start_date, tryFormats="%Y-%m-%d"),
		salary = as.numeric(salary),
		expenses = as.numeric(expenses),
		total = as.numeric(total)
	)

final_tbl %>% 
	summarize(total = sum(total), count = n())

final_tbl %>% 
	glimpse()


master_tbl <- final_tbl %>% 
	mutate(
		#person_id = map_chr(person_id, get_people_id), # standarize "cedula" 
		#start_date = map_chr(start_date, get_date_esp),
		count_words = map_chr(complete_name, get_count_words),
		first_name = map_chr(complete_name, get_first_name),
		last_name = map_chr(complete_name, get_last_name),
		#code = code_id,
		entity = entity_name,
		departament = "unknow",
		#key = paste(person_id, as.character(start_date), position, sep = "_"),
		update_date = update_date, # cuándo se actualizó la página?
		record_date = data_date  # de cuándo es el dato? 
		# execution_date = execution_date # cuando procese el dato?
	) %>% 
	mutate(
		count_words = as.integer(count_words), 
		sex = sapply(first_name, function(x) get_sex_by_name(x))
	) 

master_tbl %>% 
	DataExplorer::plot_missing()

master_tbl %>% 
	glimpse()


# write data processing
file_name <- paste0(PATH_OUT, "central_amp_gov_salaries_", update_date,".csv")
file_name
write.csv(master_tbl, file_name, row.names = FALSE)
