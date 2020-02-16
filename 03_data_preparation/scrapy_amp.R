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
date_time <- as.character(Sys.Date()) # process execution day
last_update <- paste0(substr(date_time, 1, 8), "01") # execution month

process_date <- as.Date(last_update) - as.difftime(1, unit = "days") # data of the month ...
process_month <- tolower(month.name[as.integer(paste0(substr(process_date, 6, 7)))])
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
		rvest::html_nodes(xpath = 'td[6]') %>% 
		rvest::html_text()
	
	psalary <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[8]') %>% 
		rvest::html_text()
	
	pexpenses <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[9]') %>% 
		rvest::html_text()
	
	ptotal <-  table_html[4] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[10]') %>% 
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

# 1. capture the last update
file_name <- paste0("./00_data/images/2020/amp/amp_last_update_", process_month,".png")
img_last <- render_png(url = url, wait = 5)
image_write(img_last, file_name)


# ***********************************************
# scraping CSS web page 
# ***********************************************

body_html <- splash_local %>% 
	splash_go(url) %>% 
	splash_wait(5) %>% 
	splash_html()

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

# write data processing
write.csv(final_tbl, paste0(PATH_OUT, "amp_employees_processing_", process_month,".csv"), row.names = FALSE)


