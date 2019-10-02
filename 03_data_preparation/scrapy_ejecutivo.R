cat("\014")
#install.packages("V8")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(V8)				 # call javascript functions
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(XML)
library(stringr) 
# ***********************************************
PATH_OUT <- "./00_data/out/"
date_time <- as.character(Sys.Date())

url <- "https://www.presidencia.gob.pa/Detalle-de-Viajes-y-Viaticos/2"
#url <- "www.prensa.com"
session <- read_html(url)
html_text(session) 

session %>% 
		rvest::html_nodes('table')


# RUC 
name_var <- session %>% 
		rvest::html_nodes(css = 'div[id="fb-root"]') %>% #rvest::html_text()
		rvest::html_nodes(css = 'table[css="table-striped"]') %>% 
	  rvest::html_nodes('tr') %>% 
	  rvest::html_nodes(xpath = 'td[1]') %>% 
	  rvest::html_text()

