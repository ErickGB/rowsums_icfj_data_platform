cat("\014")
gc()
# *******************************************************************************
# Erick Gordón B.
# Creted: 09-jan-2020
# Description: main functions used in scrapy processes
# *******************************************************************************
# Load supplementary data ----
names_tbl <- readr::read_csv("./00_data/in/names/namesComplete2016.csv")
names_tbl$X1 <- NULL  
names_tbl <- names_tbl %>% 
	group_by(firstname, sex) %>% 
	summarize(total = sum(count)) %>%
	ungroup() %>% 
	mutate(
		firstname = toupper(firstname), 
		sex = ifelse(sex == "F", "MUJER", "HOMBRE")
	)
# *******************************************************************************
# functions ----
# format key get_people_id("8-707-2100")
get_people_id <- function(key) {
	key_02 <- str_split(key, pattern = "-")[[1]][2]
	key_03 <- str_split(key, pattern = "-")[[1]][3]
	#paste0(substr("0000", 1, 4 - nchar(key_02)), key_02)
	#paste0(substr("00000", 1, 5 - nchar(key_03)), key_03)
	people_id <- paste0(str_split(key, pattern = "-")[[1]][1], "-", 
											paste0(substr("0000", 1, 4 - nchar(key_02)), key_02), "-",
											paste0(substr("00000", 1, 5 - nchar(key_03)), key_03))
	return(people_id)		
}
# split txt data: get_split_value("PRIMER SEGUNDO APP.PAT APP.MAT", 3)
get_split_value <- function (value, position) {
	split_value <- str_split(value, pattern = " ")[[1]][position]
	split_value <- ifelse(is.na(split_value) == TRUE, "", as.character(split_value))
	return(split_value)
}
# get sex by name: 
get_sex_by_name <- function(name) 
{
	sex <- names_tbl %>% 
		filter(firstname == toupper(name)) %>% 
		arrange(desc(total)) %>% 
		dplyr::select(sex) %>% 
		head(1) %>% 
		as.character()
	
	sex <- ifelse(nchar(sex) > 6, "X", sex)
	return (sex)
}

# get_count_words("GERMANIA MURILLO VILLARREAL DE LAKE")
# get_count_words("Eric Cárdenas")
get_count_words <- function(sentence)
{
	data_text <- str_split(sentence, pattern = " ")[[1]]
	text_tbl <- tibble(raw = data_text) %>% 
		filter(raw != "")
	return(nrow(text_tbl))
}

# get_first_name("BERNADETH LOURDES  SERRANO LEE ")
# get_first_name("Eric Cárdenas")
get_first_name <- function(sentence) 
{
	data_text <- str_split(sentence, pattern = " ")[[1]]
	text_tbl <- tibble(raw = data_text) %>% 
		filter(raw != "") %>% 
		mutate(raw =  stringr::str_trim(raw, side = "both"))
	
	first_name <- case_when(
		nrow(text_tbl) >= 4 ~ paste0(text_tbl$raw[1], " ", text_tbl$raw[2]), 
		nrow(text_tbl) == 3 ~ text_tbl$raw[1],
		nrow(text_tbl) == 2 ~ text_tbl$raw[1]
	)
	return(first_name)
}
# get_last_name("BERNADETH LOURDES  SERRANO LEE ")
# get_last_name("Eric Cárdenas")
get_last_name <- function(sentence)
{
	first <- nchar(get_first_name(sentence))
	last_name <- stringr::str_trim(substr(sentence, (first + 1), nchar(sentence)), side = "both")
	return(last_name)
}


# get_month_num_esp("nov")
get_month_num_esp <- function(month_name_esp) {
	month <- case_when(
		month_name_esp == "ene" ~ "01",
		month_name_esp == "feb" ~ "02",
		month_name_esp == "mar" ~ "03",
		month_name_esp == "abr" ~ "04",
		month_name_esp == "may" ~ "05",
		month_name_esp == "jun" ~ "06",
		month_name_esp == "jul" ~ "07",
		month_name_esp == "ago" ~ "08",
		month_name_esp == "sep" ~ "09",
		month_name_esp == "oct" ~ "10",
		month_name_esp == "nov" ~ "11",
		month_name_esp == "dic" ~ "12",
		TRUE ~ as.character(month_name_esp)
	)
	return(month)
}

# geth_month_name(12)
geth_month_name <- function(month_number){
	month <- case_when(
		month_number == 1 ~  "ene",
		month_number == 2 ~  "feb",
		month_number == 3 ~  "mar",
		month_number == 4 ~  "abr",
		month_number == 5 ~  "may",
		month_number == 6 ~  "jun",
		month_number == 7 ~  "jul",
		month_number == 8 ~  "ago",
		month_number == 9 ~  "sep",
		month_number == 10 ~ "oct",
		month_number == 11 ~ "nov",
		month_number == 12 ~ "dic",
		TRUE ~ as.character("error, not is a number")
	)
	return(month)
}

# get_date_esp("00/01/1900")
# get_date_esp("01/10/2020")
# get_date_esp("1999/12/22")
# get_date_esp("0000-00-00") 

get_date_esp <- function(date_esp) {
	#date_esp <- "02-ene-19"
	date_str <- "01/01/1900"
	tryCatch(
		{
			temp_date <- stringr::str_replace(date_esp, substr(date_esp, 4, 6), get_month_num_esp(substr(date_esp, 4, 6))) 
			date <- as.Date(temp_date, tryFormat = c("%d-%m-%Y", "%d-%m-%y", "%d/%m/%Y", "%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d", "%Y/%d/%m"))
			date_str <- temp_date
		}, error=function(cond) {
			print(paste0("Error in function 'get_date_esp' with data: ", date_esp, ". Message: ", cond))
		} )
	
	return(date_str)
}


# *******************************************************************************
