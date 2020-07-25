cat("\014")
# ********************************************************************
# Erick Gordón B.
# erick.gordon@rowsums.com
# Panama City, Panama 
# ********************************************************************
library(dplyr)
library(tidymodels)
library(tidyr)
library(h2o)

#library(dlookr)
library(ggcharts)

source("./00_scripts/base_functions.R")
PATH_IN <- "./00_data/in/covid"
PATH_OUT <- "./00_data/out/salaries/pending_process/2020/"

# ********************************************************************
# load data ----
set.seed(777)
records_tbl <- tibble(
	id = seq(1, 5, 1),
	file_name = c(
		"january/cgr_gov_salaries_ene.csv" 
		,"febrary/central_gov_salaries_february.csv"
		,"march/1_central_gov_salaries_march.csv"
		,"april/central_gov_salaries_april.csv"
		,"may/central_gov_salaries_may.csv"
	),
	data_date = c(
		"31/01/2020", "29/02/2020", "31/03/2020", "30/04/2020", "31/05/2020" 
	)
) %>% 
	mutate(data_date = as.Date(data_date, tryFormats = c("%d/%m/%Y")))


total_tbl <- tibble()
new_tbl <- tibble()
out_tbl <- tibble()
for (i in 1:(nrow(records_tbl) - 1)) {
	firt_tbl <- readr::read_csv(paste0(PATH_OUT, records_tbl$file_name[i]))
	firt_tbl$data_date <- records_tbl$data_date[i]
	second_tbl <- readr::read_csv(paste0(PATH_OUT, records_tbl$file_name[i+1]))
	second_tbl$data_date <- records_tbl$data_date[(i+1)]
	
	temp_new_tbl <- anti_join(second_tbl, firt_tbl[, c("person_id")]) # may and not in apr = news
	temp_out_tbl <- anti_join(firt_tbl, second_tbl[, c("person_id")]) # apr and not in may = out
	
	if(i == 1) {
		total_tbl <- bind_rows(firt_tbl, second_tbl)
		new_tbl <- temp_new_tbl
		out_tbl <- temp_out_tbl
	} else {
		total_tbl <- bind_rows(total_tbl, second_tbl)
		new_tbl <- bind_rows(new_tbl, temp_new_tbl)
		out_tbl <- bind_rows(out_tbl, temp_out_tbl)		
	}
}

table(total_tbl$data_date, total_tbl$record_date)

new_tbl$type <- 'new_employee'
out_tbl$type <- 'ex_employee'
out_tbl <- out_tbl %>% 
	bind_rows(new_tbl)


review_tbl <- total_tbl %>% 
	group_by(code, entity, data_date) %>% 
	summarise(count = n(), total_salary = sum(total_income), 
						max_salary = max(total_income)) %>% 
	ungroup()
	
write.csv(out_tbl, paste0("./00_data/out/salaries/out_people_2020.csv"))
write.csv(review_tbl, paste0("./00_data/out/salaries/summary_people_2020.csv"))


rm(temp_new_tbl, temp_out_tbl, second_tbl, records_tbl, 
	  i, firt_tbl, new_tbl)

# final table
total_tbl %>% 
	glimpse()

# ***************************
# EDA
#bin <- binning(carseats$Income, nbins = 4,labels = c("LQ1", "UQ1", "LQ3", "UQ3"))

# 1. Bar - total expense
review_tbl %>% 
	group_by(data_date) %>% 
	ungroup() %>% 
	mutate(month_name = as.integer(format(data_date,"%m"))) %>% 
	mutate(month_name = geth_month_name(month_name)) %>% 
	summarise(people_count = round(sum(count)/1000000), 
						total_expense = round(sum(total_salary)/1000000)) %>% 
	bar_chart(x = month_name, y = total_expense) + 
	labs(x = "Mes", y = "Gasto (Millones balboas)")


lubridate::months(as.Date("01/01/2020"), "day")
as.integer(format(as.Date("01/01/2020"),"%m"))
