gc()
cat("\014") 
# **********************************************************************
# Created by: Erick Gordon
# Date: 17 Enero 2019
# Description: Google points ---- 
# https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html
# **********************************************************************
# Libraries
# AIzaSyC2IjpVxOn_VgnpUaQrL9UC5ZiFS-mQLCY
library(tidyverse)
library(tidyquant)
library(janitor)
library(furrr)     # Parallel Processing using purrr (iteration)
#library(purrr)     # Functional programming

# spatial 
library(googleway) # Google API keys
library(spdplyr)
library(sp)
library(rgdal)
library(recipes)
library(DataExplorer)
# **********************************************************************
# Constant 
PATH_IN <- "./00_Data/in/"
PATH_OUT <- "./00_Data/out/"
source("./00_scripts/geo_functions.R") 

# **********************************************************************

## not specifying the api will add the key as your 'default'
clear_keys() ## clear any previously set keys

key <- "AIzaSyCk75htzL6iQeMfxi8_Lfpn1K0P-d0XROo"
set_key(key = key)
google_keys()

# google places 
#res <- google_places(search_string = "Restaurants in Melbourne, Australia", key = key)

get_type <- function(data_list) {
	return(data_list[[1]][1])
}

get_near_points <- function(key_word, radius_mts, lat, lon, name = NA)
{
	res <- google_places(location = c(lat, lon), # numeric vector of latitude/longitude
											 #keyword = key_word, #language = "es",
											 radius = radius_mts, # numeric Defines the distance (in meters) within which to return place results. 
											 key = key)
	# transform in tibbble table
	final_tbl <- res$results %>% 
		as_tibble() %>% 
		mutate(
			lat = .$geometry$location$lat, 
			lng = .$geometry$location$lng,
			type = sapply(.$types, function(x) get_type(x))
		) #%>% dplyr::select(place_id, name, vicinity, user_ratings_total, scope, type, lat, lng)
	
	return(final_tbl)
}

temp_tbl <- get_near_points("Rest", 5000, 9.02, -79.5)
View(temp_tbl)

# **********************************************************************
# Nearest points 

buildings_tbl <- readr::read_csv(paste0(PATH_OUT, "final_processing/db_master_points_v2.csv"))
buildings_tbl %>% 
	glimpse()

buildings_tbl <- buildings_tbl %>% 
	filter(latitutde > 0) %>% 
	mutate(
		type_search = "beauty_salon", 
		distance = 5000
	) 

test <- get_near_points(buildings_tbl$point_name[1], 5000, buildings_tbl$latitutde[1], buildings_tbl$longitude[1])
test$icon

type <- buildings_tbl$type_search
dist <- buildings_tbl$distance
lat <- buildings_tbl$latitutde
lon <- buildings_tbl$longitude
data <- list(t = type, d = dist, l = lat, n = lon)

temp_df <- pmap_df(list(type, dist, lat, lon), get_near_points) 
final_tbl <- temp_df %>% 
	group_by(place_id, name, vicinity, rating, user_ratings_total, scope, type, lat, lng) %>% 
	summarize(total = n()) 

final_tbl %>% 
	arrange(desc(total))
write.csv(final_tbl, paste0(PATH_OUT, "out_restaurants.csv"))