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
# get_features(data_tbl$json[4])
get_features <- function(info_json) {
	data_json <- rjson::fromJSON(info_json)
	categories <- ""
	for (idx in 1:length(data_json$topCategories)) {
		categories <- paste(categories, data_json$topCategories[[idx]]$name, "|")
	}
	payments <- ""
	for (idx in 1:length(data_json$paymentMethodsList)) {
		if(!(data_json$paymentMethodsList[[idx]]$descriptionES == "Ingenico"))
			payments <- paste(payments, data_json$paymentMethodsList[[idx]]$descriptionES, "|")
	}
	return_tbl <- tibble(
		  id = data_json$id %>% as.character(),
			name = data_json$name,
			city_name = data_json$cityName,
			#shipping_amount = json$shippingAmount,
			delivery_time = data_json$deliveryTime,
			delivery_type = data_json$deliveryType,
			count = data_json$validReviewsCount,
			category = categories,
		  payment = payments
		)
	return(return_tbl)
}

get_restaurants <- function(lat, lon, page) {
	url <- paste0("https://www.appetito24.com.pa/restaurantes/ciudad-de-panama?lat=",lat,"&lng=-",lon,"&&bc=none", "bt=RESTAURANT") 
	if(page > 1) {
		url <- paste0(url, "&page=", page)
	}
	session <- html_session(url)
	session %>% 
			rvest::html_nodes(css = 'div[id="listContent"]')
	
	items <- session %>% 
			rvest::html_nodes(css = 'div[id="listContent"]') %>% #rvest::html_text()
			rvest::html_nodes(css = 'ul[id="resultList"]') %>% 
		  rvest::html_nodes('li.restaurant-wrapper.peyaCard.show') 
	
	
	info_json <-  items %>% 
		 rvest::html_attr("data-info") 
	
	ids <- items %>% 
		 rvest::html_attr("data-id")
	
	lat <- items %>% 
		 rvest::html_attr("data-lat") 
	
	lng <- items %>% 
		 rvest::html_attr("data-lng")
	mscd <- items %>% 
		 rvest::html_attr("data-mastercard")
	clave <- items %>% 
		 rvest::html_attr("data-tarjetaclavepos")
	visa <- items %>% 
		 rvest::html_attr("data-visa")
	url_business <- items %>% 
		 rvest::html_attr("data-url")
	title <- items %>% 
		 rvest::html_attr("title")
	
	data_tbl <- tibble(
		id = ids,
		json = info_json %>% as.character(),
		title = title,
		latitude = lat,
		longitude = lng,
		url_base = url,
		url = url_business,
		visa = visa,
		master_card = mscd
		)
	final_tbl <- map_df(info_json, get_features)
	final_tbl <- inner_join(data_tbl, final_tbl, by = 'id')
	return(final_tbl)
} 

# ***********************************************

PATH_OUT <- "./00_data/out/"
date_time <- as.character(Sys.Date())

lat <- 8.986138921303459
lon <- 79.52019978580472
page <- 1
sleep_time <- 25

summary_tbl <- get_restaurants(lat, lon, page = 1)
for (i in 1:12) {
	Sys.sleep(sleep_time) # The time interval to suspend execution for, in seconds.
	temp_tbl <- get_restaurants(lat, lon, i)
	summary_tbl <- rbind(summary_tbl, temp_tbl)
}
summary_tbl$record_date <- NULL
summary_tbl$record_date <- date_time

# processing
latitude <- NULL 
longitude <- NULL 
summary_tbl <- summary_tbl %>% 
	mutate(
		latitude = as.numeric(latitude),
		longitude = as.numeric(longitude),
		id = as.integer(id),
		category = stringr::str_trim(category),
		payment = stringr::str_trim(payment)
		)

summary_tbl %>% 
	count(id) %>% 
	arrange(desc(id))

summary_tbl %>% 
	glimpse()

write.csv(summary_tbl, paste0(PATH_OUT, "app24_points.csv"))

