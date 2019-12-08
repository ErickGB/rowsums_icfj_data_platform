gc()
cat("\014") 
# **********************************************************************
# Created by: Erick Gordon
# Date: 14 Jun 2017
# Description: Business Science Problem Framework ---- 
# **********************************************************************
# Libraries
library(tidyverse)
library(tidyquant)
library(forcats)
library(stringr)
library(corrplot)   # correlation vizualization
library(caTools)    # split data
library(factoextra) # fitting variables
library(FactoMineR) 
library(caret)      # improve model
library(DataExplorer)
library(recipes)
library(h2o)

library(googledrive)
library(gargle)
library(bigrquery) # R Interface to Google BigQuery API  
library(dplyr) # Grammar for data manipulation  
library(DBI) # Interface definition to connect to databases 
library(ggplot2) # Data Viz package

#install.packages("DBI")
# *******************************************************************************
# Constant 
PATH_IN <- "./00_Data/in/salaries/"
PATH_OUT <- "./00_data/out/salaries/"
actual_month <- "sep" # 
source("./00_scripts/base_functions.R")
# *******************************************************************************
# load data ----
# gcp authentication
drive_auth(path = "./00_scripts/rowsums-2198b8679813.json")
project <- "rowsums"

projectid<-'rowsums'
datasetid<-'trade'
bq_conn <-  dbConnect(bigquery(), 
                            project = projectid,
                            dataset = datasetid, 
                            use_legacy_sql = FALSE
                      )

bigrquery::dbListTables(bq_conn) # List all the tables in BigQuery data set
data_raw_tbl <- dplyr::tbl(bq_conn, "fact_agg_product") # connects to a table but no load data in memory
class(data_raw_tbl)

#data_raw_tbl <- data_raw_tbl %>% 
#	group_by() %>% 
#	summarize()


data_raw_tbl <- collect(data_raw_tbl)
data_raw_tbl %>% 
	glimpse()




# variable skewness
skewness_names <- data_raw_tbl %>% 
	select_if(is.numeric) %>% 
	map_df(skewness) %>% 
	gather(factor_key = 1) %>% 
	arrange(desc(value)) %>% 
	filter(value >= 0.75) %>% 
	pull(key) %>% as.character()
skewness_names

# variable null 
withnull_names <- data_raw_tbl %>% 
	map_df(is.na) %>% 
	gather(factor_key = TRUE) %>% 
	filter(value == TRUE) %>% 
	distinct(key) %>% 
	pull(key) %>% 
	as.character()








rec_obj <- recipe(~ ., data = train_tbl[, 2:ncol]) %>%
		#step_YeoJohnson(skewness_names) %>% 
	  #step_meanimpute(withnull_names) %>% 
		#step_rm(remove_col) %>% 
	  step_center(all_numeric()) %>%  
	  step_scale(all_numeric()) %>% 
	  step_zv(all_predictors()) %>% 
	  #step_dummy("years") %>%
    prep()
rec_obj

train_prepared_tbl <- bake(rec_obj, train_tbl[, 2:ncol]) 
train_prepared_tbl %>% head()


# anomaly detection with Isolation Forest
outliers <- get_outliers(jobs_summary_tbl)















