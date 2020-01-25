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
url <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=35"
url_dinamic <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_grid&amp;gid=5_ql_1&amp;o_b=id&amp;o_d=ASC&amp;p=x_url&amp;rpp=125&id=35"
PATH_OUT <- "./00_data/out/salaries/"
date_time <- as.character(Sys.Date()) # process execution day
last_update <- paste0(substr(date_time, 1, 8), "01") # execution month

process_date <- as.Date(last_update) - as.difftime(1, unit = "days") # data of the month ...
process_month <- tolower(month.name[as.integer(paste0(substr(process_date, 6, 7)))])
# ***********************************************
# functions ----
source("00_scripts/etl_functions.R")

get_mc_employee <- function(tableid, url) {
	#tableid <- 2
	#url <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_grid&amp;gid=26_ed_1&amp;o_b=id&amp;o_d=ASC&amp;p=7&amp;rpp=125&id=74"
	discount <- ifelse(tableid == 4, 3, 2)
	
	body_html <- splash_local %>% 
		splash_go(url) %>% 
		splash_wait(10) %>% 
		splash_html()
	
	# we return all the tables on the page
	table_html <- body_html %>% 
		rvest::html_nodes(xpath = '//table')
	
	rows <- table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr')
	rows <- (length(rows) - discount)
	
	pperson_id <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[2]') %>% 
		rvest::html_text()
	
	pcomplete_name <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[3]') %>% 
		rvest::html_text()
	
	pjob_position <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[4]') %>% 
		rvest::html_text()
	
	pdepartament <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[5]') %>% 
		rvest::html_text()
	
	pstart_date <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[6]') %>% 
		rvest::html_text()
	
	
	pstatus <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[7]') %>% 
		rvest::html_text()
	
	psalary <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[8]') %>% 
		rvest::html_text()
	
	pexpenses <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[9]') %>% 
		rvest::html_text()
	
	ptotal <-  table_html[tableid] %>% 
		rvest::html_nodes('tbody') %>% 
		rvest::html_nodes('tr') %>% 
		.[2:rows] %>% 
		rvest::html_nodes(xpath = 'td[10]') %>% 
		rvest::html_text()
	
	row_tbl <- tibble(
		person_id = pperson_id,
		complete_name = pcomplete_name,
		position = pjob_position,
		department = pdepartament,
		status = pstatus,
		start_date = pstart_date,
		salary = psalary,
		expenses = pexpenses,
		over_costs = 0,
		total_income = ptotal
	)
	return(row_tbl)
}


# ***********************************************
# Save images

# Start, active splash ----
splash("localhost") %>% splash_active()

# 1. capture the last update
file_name <- paste0("./00_data/images/2020/miamb/miamb_last_update_", process_month,".png")
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
	id = 0: (count -1), 
	table_id = 2
) %>% 
	mutate(
		url = str_replace(url_dinamic, "x_url", as.character(id)),
		shett = paste0("sheet ", as.character(id))
	)
records_tbl$url[1] <- url
records_tbl$table_id[1] <- 4
records_tbl

time <- Sys.time()
scrapy_tbl <- records_tbl %>% 
	mutate(features = furrr::future_map2(table_id, url, get_mc_employee)) %>%  # future_map2 => with many parameters
	unnest()
Sys.time() - time # Time difference of  54.01361 secs

scrapy_tbl %>% 
	head(10)

# ***********************************************
# cleaning data

final_tbl <- scrapy_tbl %>% 
	mutate(
		person_id = str_replace(str_trim(person_id), '"', ""),
		complete_name = str_replace(str_trim(complete_name), '"', ""),
		#first_name = str_split(complete_name, " "),
		start_date = str_replace(str_trim(start_date), '"', ""),
		start_date = str_replace(start_date, "-", "/"),
		start_date = str_replace(start_date, "-", "/"),
		start_date = str_replace(start_date, " ", ""),
		start_date = str_replace(start_date, "//", "/"),
		position = str_replace(str_trim(position), '"', ""),
		#direction = str_replace(str_trim(direction), '"', ""),
		status = str_replace(str_trim(status), '"', ""),
		salary = str_replace(str_trim(salary), '"', ""),
		salary = str_replace(salary, ',', ""),
		expenses = str_replace(str_trim(expenses), '"', ""),
		expenses = str_replace(expenses, ',', ""),
		over_costs = str_replace(str_trim(over_costs), '"', ""),
		over_costs = str_replace(over_costs, ',', ""),
		total_income = str_replace(str_trim(total_income), '"', ""), 
		total_income = str_replace(total_income, ',', ""),
	) %>% 
	mutate(
		salary = as.numeric(salary),
		expenses = as.numeric(expenses),
		over_costs = as.numeric(over_costs),
		total_income = as.numeric(total_income)
	)

final_tbl %>% 
	summarize(total = sum(total_income), count = n())

#final_tbl <- final_tbl %>% 
#	select(person_id, complete_name, position, department, status, start_date, salary, expenses, over_costs, total_income)
View(final_tbl)

# write data processing
write.csv(final_tbl, paste0(PATH_OUT, "miamb_employees_processing_", "november",".csv"), row.names = FALSE)

# ********************************************************************
# create data for Tableau month dashboard : central_gov_salaries ----
table(final_tbl$start_date)


master_tbl <- final_tbl %>% 
	filter(start_date != "POR ENTRAR") %>% 
	mutate(
		person_id = map_chr(person_id, get_people_id), # standarize "cedula" 
		start_date = map_chr(start_date, get_date_esp),
		count_words = map_chr(complete_name, get_count_words),
		first_name = map_chr(complete_name, get_first_name),
		last_name = map_chr(complete_name, get_last_name),
		code = "902",
		entity = "Ministerio de Ambiente",
		departament = "unknow",
		key = paste(person_id, as.character(start_date), position, sep = "_"),
		update_date = last_update, 
		record_date = process_date
	) %>% 
	mutate(
		count_words = as.integer(count_words), 
		sex = sapply(first_name, function(x) get_sex_by_name(x))
	) 


master_tbl %>% 
	glimpse()

master_tbl %>% 
	DataExplorer::plot_missing()


master_tbl <- master_tbl %>%	
	select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
				 start_date, first_name, entity, update_date, sex, url, record_date, key, 
				 over_costs, departament) 

write.csv(master_tbl, paste0(PATH_OUT, "miamb_gov_salaries_", "november",".csv"), row.names = FALSE) 
rm(body_html, final_tbl, scrapy_tbl, records_tbl, table_html)
