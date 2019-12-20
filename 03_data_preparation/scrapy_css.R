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
url_css <- "http://www.css.gob.pa/p/grid_defensoria/"
PATH_OUT <- "./00_data/out/salaries/"
date_time <- as.character(Sys.Date())
actual_month <- "nov" # 
last_update <- as.Date('2019-12-01')

# Table with results 1 to 3443
final_tbl <- tibble(
	id = 1:3443,
	entity = rep("CSS", 1, 3443),
	site = rep(url_css, 1, 3443)
)

error_tbl <- tibble(
	error_id = character(),
	messages = character()
)
# ***********************************************
# Functions ----
# ***********************************************
#get_css_employees(1)
#get_css_employees(575)
get_css_employees <- function(page) {
	tryCatch({
		id <- 1
		print(paste0("page:", as.character(page)))
		body_html <- splash_local %>% 
			splash_go(url_css) %>% 
			splash_focus("#rec_f0_bot") %>% 
			splash_send_text(page) %>% 
			splash_send_keys("<Return>") %>% 
			splash_focus("#brec_bot") %>% 
			splash_send_keys("<Return>") %>% 
			#splash_click(x = 62, y = 760) %>% 
			splash_wait(5) %>% 
			splash_html() # splash_png() 
		#body_html
		
		#body_html %>% 
		#	rvest::html_nodes(css = 'input[id="rec_f0_bot"]') %>% 
		#	rvest::html_attr("value") 
		
		# Data page  1...10 records
		page_tbl <- tibble(
			person_id = character(),
			complete_name = character(),
			job_title = character(),
			departament = character(),
			status = character(),
			start_date = character(),
			salary = character(),
			expens = character(),
			over_costs = character(),
			total = character()
		)
		for(i in 1:10) {
			id <- i
			pperson_id <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_cedula_1"]', "1", as.character(i))) %>% 
				rvest::html_text() # "10-21-450"
			
			pcomplete_name <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_sc_field_0_1"]', "1", as.character(i)))  %>% 
				rvest::html_text()
			
			pjob_title <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_tcargo_1"]', "1", as.character(i)))  %>% 
				rvest::html_text()
			
			pdepartament <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_depto_1"]', "1", as.character(i)))  %>% 
				rvest::html_text()
			
			pstatus <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_estatus_1"]', "1", as.character(i)))  %>% 
				rvest::html_text()
			
			pstart_date <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_inicio_planilla_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			psalary <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_salario_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			pexpens <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_gastos_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			pover_costs <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_ssueldo_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			ptotal <- body_html %>% 
				rvest::html_nodes(css = str_replace('span[id="id_sc_field_total_1"]', "1", as.character(i))) %>% 
				rvest::html_text()
			
			record_tbl <- tibble(
				person_id = pperson_id,
				complete_name = pcomplete_name,
				job_title = pjob_title,
				departament = pdepartament,
				status = pstatus,
				start_date = pstart_date,
				salary = psalary,
				expens = pexpens,
				over_costs = pover_costs,
				total = ptotal
			)
			
			page_tbl <- rbind(page_tbl, record_tbl)
			
		}
	},
	error=function(error_message) {
		error_row_tbl <- tibble(
			error_id = id,
			message = error_message
		)
		print(paste0("error message:", error_message))
		error_tbl <- rbind(error_tbl, error_row_tbl)
		
		temp_tbl <- tibble(
			person_id = id,
			complete_name = error_message,
			job_title = "",
			departament = "",
			status = "",
			start_date = "",
			salary = "",
			expens = "",
			over_costs = "",
			total = ""
		)
		page_tbl <- temp_tbl 
	})
	return(page_tbl)
}

# ***********************************************

# ***********************************************
# Start, active splash ----
splash("localhost") %>% splash_active()
render_png(url = url_css, wait = 5)

# *********************
# scraping CSS web page 
body_html <- splash_local %>% 
	splash_go(url_css) %>% 
	splash_wait(5) %>% 
	splash_html()

time <- Sys.time()
final_tbl <- final_tbl %>%
	mutate(
		records = furrr::future_map(id, get_css_employees) 
	) # %>% unnest()
Sys.time() - time

final_expanded_tbl <- final_tbl %>% 
	unnest()

# structure 
final_expanded_tbl %>% 
	glimpse()

final_expanded_tbl %>% 
	filter(job_title == "") %>%
	count(complete_name)

final_expanded_tbl %>% 
	DataExplorer::plot_missing()

final_expanded_tbl %>%  
	head()

write.csv(final_expanded_tbl, paste0(PATH_OUT, "css_employees_raw.csv"))
error_tbl


# clean data
final_expanded_tbl <- final_expanded_tbl %>% 
	mutate(
		person_id = str_replace(str_trim(person_id), '"', ""),
		complete_name = str_replace(str_trim(complete_name), '"', ""),
		#first_name = str_split(complete_name, " "),
		job_title = str_replace(str_trim(job_title), '"', ""),
		departament = str_replace(str_trim(departament), '"', ""),
		status = str_replace(str_trim(status), '"', ""),
		salary = str_replace(str_trim(salary), '"', ""),
		salary = str_replace(salary, ',', ""),
		expens = str_replace(str_trim(expens), '"', ""),
		expens = str_replace(expens, ',', ""),
		over_costs = str_replace(str_trim(over_costs), '"', ""),
		over_costs = str_replace(over_costs, ',', ""),
		total = str_replace(str_trim(total), '"', ""), 
		total = str_replace(total, ',', ""),
	) %>% 
	mutate(
		start_date = as.Date(start_date, tryFormats="%Y-%m-%d"),
		salary = as.numeric(salary),
		expens = as.numeric(expens),
		over_costs = as.numeric(over_costs), 
		total = as.numeric(total), 
	)
# write data processing
write.csv(final_expanded_tbl, paste0(PATH_OUT, "css_employees_processing.csv"))

warnings() 
# ***********************************************
# data review
# ***********************************************

sum(final_expanded_tbl$over_costs) # 5,451,908
sum(final_expanded_tbl$total) # 63,300,141

(sum(final_expanded_tbl$over_costs) / sum(final_expanded_tbl$total)) * 100

median(final_expanded_tbl$total) 
max(final_expanded_tbl$total) 

final_expanded_tbl %>% 
	group_by(person_id) %>% 
	summarize(count = n(), total = sum(total)) %>% 
	ungroup() %>% 
	filter(count > 1)

summary(final_expanded_tbl$total)
