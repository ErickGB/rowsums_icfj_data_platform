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

# *******************************************************************************

# *******************
# aduanas



# *******************
# Contraloría General de la República
# capture the last update
url <- 'http://www.contraloria.gob.pa/archivos_planillagub/Index_planillagub3.asp'
file_name <- paste0("./00_data/images/2020/cgr/cgr_last_update_", actual_month,".png")
img_last <- render_png(url = url, wait = 5)
image_write(img_last, file_name)

# start 
html <- read_html(url)

# take last updated 
update_data <-  html %>% 
	rvest::html_nodes(xpath = '//p') 
update <- update_data[9] %>% 
	html_text()

update <- gsub("\r\n", "", update)
update <- gsub("\"", "", update)
update <- gsub("Fecha de Actualización de los Datos :", "", update)
update <-stringr::str_trim(update, side = "right")
update <-stringr::str_trim(update, side = "left")
update # record_date m/d/Y  "6/16/2020 2:12:50 PM"



# *******************
# CSS




# *******************
# defensoría 
url <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=54"
body_html <- splash_local %>% 
	splash_go(url) %>% 
	splash_wait(5) %>% 
	splash_html()

table_span <- body_html %>% 
	rvest::html_nodes(xpath = '//span') 

page_date <- table_span[4] %>% 
	rvest::html_text()
page_date
page_date <- as.POSIXlt(page_date, tz = "UTC-5")
page_date # "2020-06-09 11:38:41 UTC"


# *******************
# protección al consumidor 
url <- "http://ogov.defensoria.gob.pa/transparencia/index.php?option=com_k2&view=item&layout=item&id=22"
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





# *******************
# amp 


# *******************
# mi ambiente


# *******************
# servicios públicos


# *******************
# contrataciones 

# UTP
# UP


