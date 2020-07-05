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
# Autoridad Nacional de los Servicios Públicos
#<a href="javascript:;" onclick="searchjx('?option=com_grid&amp;gid=16_bq_1&amp;o_b=id&amp;o_d=ASC&amp;p=1&amp;rpp=200', '16_bq_1');" class="pageNum">2</a>
url <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=33"
url_dinamic <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_grid&amp;gid=16_bq_1&amp;o_b=id&amp;o_d=ASC&amp;p=x_url&amp;rpp=125&id=33"
entity_name <- "Autoridad Nacional de los Servicios Públicos"
code_id <- "905"
output_file_name <- "4_aut_serv_publicos_gov_salaries_"
image_file_name <- "4_aut_serv_publicos_last_update_"


PATH_OUT <- "./00_data/out/salaries/pending_process/"

# ***********************************************
# functions ----
source("00_scripts/etl_functions.R")

get_mc_employee <- function(tableid, url) {
	#tableid <- 2
	#url <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_grid&amp;gid=22_fg_1&amp;o_b=id&amp;o_d=ASC&amp;p=0&amp;rpp=125&id=22"
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
	
	pstatus <-  table_html[tableid] %>% 
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
	
	pdepartament <-  table_html[tableid] %>% 
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
# scraping - Dates 
# ***********************************************

body_html <- splash_local %>% 
	splash_go(url) %>% 
	splash_wait(5) %>% 
	splash_html()

# we return date 
table_span <- body_html %>% 
	rvest::html_nodes(xpath = '//span') 

page_date <- table_span[4] %>% 
	rvest::html_text()
page_date <- as.POSIXlt(page_date, tz = "UTC-5")
page_date

unclass(page_date)

# cuando lo ejecuto 
# que fecha de actualización tiene
# de que mes es el dato (fecha actualización - 1 mes)

execution_date <- as.character(Sys.Date()-1) # process execution day
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

# ***********************************************
# Save images

# Start, active splash ----
splash("localhost") %>% splash_active()


# 1. capture the last update
file_name <- paste0("./00_data/images/",update_year, "/",  update_month, image_file_name, paste0(update_month, update_year)  ,"_process_", execution_date,".png")
file_name
img_last <- render_png(url = url, wait = 5)
image_write(img_last, file_name)

# ***********************************************

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
		sheet = paste0("sheet ", as.character(id))
	)
#records_tbl$url[1] <- url
#records_tbl$table_id[1] <- 4

time <- Sys.time()
scrapy_tbl <- records_tbl %>% 
	mutate(features = furrr::future_map2(table_id, url, get_mc_employee)) %>%  # future_map2 => with many parameters
	unnest()
Sys.time() - time # Time difference of  54.01361 secs

scrapy_tbl %>% 
	head(10)

scrapy_tbl %>% 
	filter(person_id == '6-707-2025') %>% 
	dplyr::select(person_id, start_date, salary)
scrapy_tbl$start_date <- ifelse(scrapy_tbl$start_date == "0000-00-00", NA, scrapy_tbl$start_date)

View(scrapy_tbl)
table(scrapy_tbl$start_date)


# ***********************************************
# cleaning data

final_tbl <- scrapy_tbl %>% 
	mutate(
		person_id = str_replace(str_trim(person_id), '"', ""),
		complete_name = str_replace(str_trim(complete_name), "[\r\n]", ""),
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
#View(final_tbl)

# write data processing
# write.csv(final_tbl, paste0(PATH_OUT, "miamb_employees_processing_", process_month,".csv"), row.names = FALSE)

# ********************************************************************
# create data for Tableau month dashboard : central_gov_salaries ----
table(final_tbl$start_date)


master_tbl <- final_tbl %>% 
	#filter(start_date != "POR ENTRAR") %>% 
	mutate(
		person_id = map_chr(person_id, get_people_id), # standarize "cedula" 
		start_date = map_chr(start_date, get_date_esp),
		count_words = map_chr(complete_name, get_count_words),
		first_name = map_chr(complete_name, get_first_name),
		last_name = map_chr(complete_name, get_last_name),
		code = code_id,
		entity = entity_name,
		departament = "unknow",
		key = paste(person_id, as.character(start_date), position, sep = "_"),
		update_date = update_date, # cuándo se actualizó la página?
		record_date = data_date  # de cuándo es el dato? 
		# execution_date = execution_date # cuando procese el dato?
	) %>% 
	mutate(
		count_words = as.integer(count_words), 
		sex = sapply(first_name, function(x) get_sex_by_name(x))
	) 

master_tbl %>% 
	glimpse()

master_tbl %>% 
	DataExplorer::plot_missing()

master_tbl %>% 
	filter(is.na(first_name)) %>% 
	dplyr::select(complete_name, count_words, first_name, last_name, salary)


master_tbl <- master_tbl %>%	
	select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
				 start_date, first_name, entity, update_date, sex, url, record_date, key, 
				 over_costs, departament) 

master_tbl %>% 
	glimpse()


ouput_path <- paste0(PATH_OUT, update_year, "/",  update_month, "/", output_file_name, paste0(update_month, "_",update_year)  ,"_processed_", execution_date,".csv")
ouput_path
write.csv(master_tbl, ouput_path, row.names = FALSE) 
# total time


rm(body_html, final_tbl, scrapy_tbl, records_tbl, table_html)





