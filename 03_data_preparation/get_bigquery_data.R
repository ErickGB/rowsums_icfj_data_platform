cat("\014")
# ********************************************************************
# google bigquery connection ----
library(bigrquery)
library(googledrive)
library(gargle)
library(tidyverse)

library(vroom)     # 
library(anomalize)
library(tidyquant)
library(anytime) # Convert input in any one of character, integer, numeric, factor, or ordered type into 'POSIXct'
library(plotly)

PATH_OUT <- "./00_data/out/imports/"
# ********************************************************************
# functions
plot_category <- function(code) {
	g <- total_tbl %>%
		filter(sub_category_code == code) %>% 
		ggplot(aes(date, cif_total)) +
		geom_line() +
		expand_limits(y = 0) +
		scale_y_continuous(labels = scales::number_format(big.mark = ","))
	
	ggplotly(g)
}
# ********************************************************************
httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())

bigquery_conn <- bigrquery::src_bigquery(project = "rowsums", dataset = "trade")
# List table names
src_tbls(bigquery_conn)

categories_tbl <- tbl(bigquery_conn, "trade.dim_category")
categories_tbl <- collect(categories_tbl)

imports_tbl <- tbl(bigquery_conn, "trade.fact_import")
imports_tbl %>% 
	glimpse()

# prepare query ----
total_ref <- imports_tbl %>% 
	filter(year == 2919)
	group_by(company, RUC, date) %>% 
	summarize(cif_total = sum(cif), count_total = n(), gross_weight = sum(gross_weight))
show_query(total_ref)
# collect data ----
total_tbl <- collect(total_ref) # 436.21 MB, Downloading 94,350 rows in 10 pages.
nrow(total_tbl) # 94350

# join
total_tbl <- left_join(total_tbl, categories_tbl, by = 'category_id')
write.csv(total_tbl, paste0(PATH_OUT, "oub_imports.csv"), row.names = FALSE)

# head
total_tbl %>% 
	group_by(company, RUC) %>%
	summarize(total_cif = sum(cif_total)) %>% 
	arrange(desc(total_cif)) %>% 
	head(20)








plot_category("84") 

date_tbl <-  total_tbl %>% 
	filter(sub_category_code == "84") %>% 
	mutate(date = anytime::anydate(date)) %>% 
	arrange(date) %>% 
	as_tibble()

date_tbl %>% 
	time_decompose(cif_total, merge = TRUE) %>%
	anomalize(remainder) %>%
	time_recompose() %>% 
	select(date, cif_total, sub_category, observed, season, trend, remainder, remainder_l1, remainder_l2, anomaly, recomposed_l1, recomposed_l2) %>% 
	head()


date_tbl %>% 
	time_decompose(cif_total, merge = TRUE) %>%
	anomalize(remainder) %>%
	time_recompose() %>% 
	plot_anomalies(time_recomposed = TRUE) +
	expand_limits(y = 0) +
	scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
	labs(x = "Date", y = "cif_total")


# descomposition SLT - Seasonal-Trend-Loess Method
date_tbl %>% 
	# Step 1 - Decomposition
	time_decompose(
		target  = cif_total, 
		method  = "stl", # stl or twitter 
		merge   = TRUE,
		frequency = "7 days",
		trend     = "3 months"
	) %>%
	# Step 2 - Detect Anomalies in Remainder (Residual Analysis)
	anomalize(
		target = remainder, 
		method = "iqr", # iqr or gesd
		alpha  = 0.05
	) %>%
	# Step 3 - Add Boundaries separating the anomaly lower and upper limits
	time_recompose() %>%
	
	plot_anomaly_decomposition(alpha_dots = 0.5) + 
	scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
	labs(title = "Anomaly Decomposition", subtitle = "Using Seasonal-Trend-Loess Method")

# Trend & Frequency ----
time_scale_template()




# **************************************************
# Multi-Time Series - Scaled to Top 12 categories ----

top_20_categories <- total_tbl %>% 
	group_by(sub_category_code) %>% 
	summarize(cif = sum(cif_total)) %>% 
	arrange(desc(cif)) %>% 
	head(20) %>% 
	pull(sub_category_code)

total_tbl %>% 
	count(date, sub_category_code) %>% 
	filter(n > 1)


temp <- total_tbl %>% 
	filter(sub_category_code %in% c("87")) %>% 
	mutate(date = anytime::anydate(date)) %>% 
	arrange(date) %>% 
	as_tibble() %>%
	select(sub_category, date, cif_total)

categories_tbl %>% 
	filter(sub_category_code %in% top_20_categories[1:3]) %>% 
	select(sub_category)


time_categories_tbl <- total_tbl %>% 
	filter(sub_category_code %in% top_20_categories) %>% 
	mutate(date = anytime::anydate(date)) %>% 
	arrange(date) %>% 
	as_tibble() %>%
	group_by(sub_category) %>% 
	select(sub_category, date, cif_total) %>% 
	time_decompose(cif_total, merge = TRUE) %>%
	anomalize(remainder) %>%
	time_recompose() %>%
	mutate(visits_cleaned = ifelse(anomaly == "Yes", season + trend, observed))

time_categories_tbl %>% 
	plot_anomalies(ncol = 3, alpha_dots = 0.5)


bq_deauth()


# ***********************************************************
# Top companies



temp <- total_tbl %>% 
	group_by(company) %>% 
	summarize(cif = sum(cif)) %>% 
	arrange(desc(cif)) %>% 
	ungroup() %>% 
	mutate(percent = (cif/sum(cif)) * 100) %>% 
	mutate(percent_acum = cumsum(percent)) %>% 
	mutate(secuence_id = as.numeric(rownames(.))) %>% 
	head(2200)
temp %>% filter(secuence_id > 40 & secuence_id <= 60) %>% select(-secuence_id)
temp %>% filter(secuence_id %in% c(340, 341, 512, 513, 873, 912, 914, 945))
View(temp)

top_200_companies_tbl <- total_tbl %>% 
	group_by(company) %>% 
	summarize(cif = sum(cif)) %>% 
	arrange(desc(cif)) %>% 
	top_n(2200, wt = cif)

summary_tbl <- total_tbl %>% 
	filter(company %in% c(top_200_companies_tbl$company)) %>% 
	group_by(company, sub_category_code) %>% 
	summarize(cif = sum(cif)) %>% 
	spread(key = sub_category_code, value = cif)

summary_tbl[is.na(summary_tbl)] <- 0
train_tbl <- as_tibble(scale(summary_tbl[,-1]))

