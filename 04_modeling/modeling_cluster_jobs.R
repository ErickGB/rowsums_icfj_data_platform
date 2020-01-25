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
datasetid<-'journalists'
bq_conn <-  dbConnect(bigquery(), 
                            project = projectid,
                            dataset = datasetid, 
                            use_legacy_sql = FALSE
                      )

bigrquery::dbListTables(bq_conn) # List all the tables in BigQuery data set
data_raw_tbl <- dplyr::tbl(bq_conn, "f_employee_salary") # connects to a table but no load data in memory
class(data_raw_tbl)

# get jobs summary 
jobs_tbl <- data_raw_tbl %>% 
	#filter(record_id == 9) %>% 
	mutate(years = lubridate::year(Sys.Date()) - year(start_date)) %>% 
	group_by(job_id, job_title, job_position) %>% 
	summarize(count = n(), salary = mean(total, na.rm = TRUE), years = round(mean(years, na.rm = TRUE), digits=0)) %>% 
	arrange(desc(count))

jobs_tbl %>% 
	head(20) %>% 
ggplot(aes(x = job_title, y = count)) + geom_bar(stat = "identity") + coord_flip() + labs(y =
       "No of employees ", x = "Employee Position") + geom_text(aes(label = salary),size = 3)

jobs_tbl

jobs_summary_tbl <- jobs_tbl %>% 
	collect() 

hist(jobs_summary_tbl$years)
class(jobs_summary_tbl)

#sql_query <-
#  "SELECT Offence_category,sum(Dec_2018) Dec_2018,sum(Nov_2018) Nov_2018,sum(Oct_2018) Oct_2018 FROM `uts-mdsi.stds_assignment.crime_by_postcode`
#  group by Offence_category order by Dec_2018 desc limit 10;"
#  offence_qtr <- bq_project_query(projectid, sql_query)


# anomaly detection with Isolation Forest
outliers <- get_outliers(jobs_summary_tbl)
jobs_summary_tbl$outlier <- outliers
(table(jobs_summary_tbl$outlier)/nrow(jobs_summary_tbl)) * 100

jobs_summary_tbl %>% 
	filter(outlier == 1) %>% 
	arrange(desc(salary))

jobs_summary_tbl$years <- ifelse(jobs_summary_tbl$years >= 3 & jobs_summary_tbl$years  <= 5, 5, jobs_summary_tbl$years)
jobs_summary_tbl$years <- ifelse(jobs_summary_tbl$years > 5 & jobs_summary_tbl$years  <= 10, 10, jobs_summary_tbl$years)
jobs_summary_tbl$years <- ifelse(jobs_summary_tbl$years > 10 & jobs_summary_tbl$years  <= 20, 20, jobs_summary_tbl$years)
jobs_summary_tbl$years <- ifelse(jobs_summary_tbl$years > 20, 20, jobs_summary_tbl$years)
jobs_summary_tbl$years <- paste0('x_', jobs_summary_tbl$years)

# outliers: 115
outliers_tbl <- jobs_summary_tbl %>% 
	filter(outlier == 1) %>% 
	select(job_id, job_title, count, salary) %>% 
	mutate(outlier = 99)

# no outliers
train_tbl <- jobs_summary_tbl %>% 
	filter(outlier != 1) %>% 
	select(job_id, job_title, count, salary) 
job_id <- train_tbl$job_id
train_tbl$job_id <- NULL 
ncol <- ncol(train_tbl)


rec_obj <- recipe(~ ., data = train_tbl[, 2:ncol]) %>%
		#step_YeoJohnson(col_skew_names) %>% 
	  #step_meanimpute(impute_cols) %>% 
		#step_rm(remove_col) %>% 
	  step_center(all_numeric()) %>%  
	  step_scale(all_numeric()) %>% 
	  #step_zv(all_predictors()) %>% 
	  #step_dummy("years") %>%
    prep()
rec_obj

train_prepared_tbl <- bake(rec_obj, train_tbl[, 2:ncol]) 
#train_tbl$Count <- NULL 
train_prepared_tbl %>% head()

#*****************************************
## Initial Cluster analysis (k selection) ----
#*****************************************
set.seed(7777)

#Exploratory for find the best numbers of groups (k) 
# Setup for k-means loop 
km_out <- list()
sil_out <- list()
x <- vector()
y <- vector()
min_clust <- 2      # Hypothesized minimum number of segments
max_clust <- 400    # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
num_cols <- ncol(train_tbl) # don't include codigo 
for (centr in min_clust:max_clust) {
        i <- centr-(min_clust-1) # relevels start as 1, and increases with centr
        print(i)
        set.seed(777) # For reproducibility
        km_out[i] <- list(kmeans(train_prepared_tbl, centers = centr, iter.max=1000000, nstart=1))
        sil_out[i] <- list(cluster::silhouette(km_out[[i]][[1]], dist(train_prepared_tbl)))
        # Used for plotting silhouette average widths
        x[i] = centr  # value of k
        y[i] = summary(sil_out[[i]])[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

d <- data.frame(x, y) %>% 
	filter(y > 0.25) %>% 
	arrange(desc(y)) %>% 
	head(10)
d
#     x         y
#1    2 0.5532108   2 0.8852680		 3 0.4344967
#2    3 0.4553970   3 0.6160007
#3    5 0.4209295   6 0.5321709
#4    4 0.4120493


# After, create cluster with k selected
base_centers <- 3 #  0.4715790
clusters <- list()
fit <- NA
# Because k-means is sensitive to starting conditions, the algorithm was randomly initialized 100,000 times
# Each run of k-means was allowed a maximum of 1,000,000 steps.**
for (i in 1:10000){ # 100000 
	#set.seed(i) # For reproducibility
  class_250 <- kmeans(x=train_prepared_tbl, centers= base_centers, iter.max=1000000, nstart=1)
  fit[i] <- class_250$tot.withinss
  if(i == 1) { 
  	fit[i] <- class_250$tot.withinss 
  	clusters <- class_250
  	} else {
	  if (fit[i] < min(fit[1:(i-1)])){
	    clusters <- class_250}
  }
  print(paste("finish run", i, ", tot.withinss: ", fit[i], sep=" "))
}
print(paste("finish run", i, ", tot.withinss: ", clusters$tot.withinss, sep=" "))

train_tbl$cluster
kms_res <- eclust(train_prepared_tbl, "kmeans", k = base_centers, nstart = 20, graph = FALSE)
fviz_silhouette(kms_res, palette = "jco", ggtheme = theme_classic()) 

# Visualize k-means clusters
#fviz_cluster(kms_res, geom = "point", ellipse.type = "norm",
#             palette = "jco", ggtheme = theme_minimal())

 
train_tbl$cluster <- as.factor(clusters$cluster)     
train_tbl$job_id <- job_id
table(train_tbl$cluster)
train_tbl %>% head()

outliers_tbl <- outliers_tbl %>% 
	rename(cluster = outlier)

train_complete_tbl <- rbind(outliers_tbl[, c("job_id", "cluster")], train_tbl[, c("job_id", "cluster")])

jobs_summary_tbl <- left_join(jobs_summary_tbl, train_complete_tbl, by = "job_id")
table(jobs_summary_tbl$cluster, useNA = "always")
write.csv(jobs_summary_tbl, paste0(PATH_OUT, "job_summary_outlier.csv"), row.names = FALSE)
write.csv(jobs_summary_tbl[, c("job_id", "cluster")], paste0(PATH_OUT, "out_cluster_outlier.csv"), row.names = FALSE)


jobs_raw_tbl_2 <- dplyr::tbl(bq_conn, "d_jobs") # connects to a table but no load data in memory
class(jobs_raw_tbl)

jobs_raw_tbl_2 <- jobs_raw_tbl_2 %>% 
	collect()

jobs_summary_tbl <- jobs_summary_tbl %>% 
	rename(jobs_id = job_id)


jobs_raw_tbl <- left_join(jobs_raw_tbl, jobs_summary_tbl[, c("jobs_id", "cluster")], by = 'jobs_id')
jobs_raw_tbl %>% 
	plot_missing()

jobs_raw_tbl <- jobs_raw_tbl %>% 
	mutate(PEP2 = ifelse(is.na(PEP2) == TRUE, 0, PEP2), 
				 cluster = as.integer(cluster))


job <- insert_upload_job("rowsums", "journalists", table = "d_jobs", 
												 values = jobs_raw_tbl, write_disposition = "WRITE_TRUNCATE")


# *******************************************************************************
# 5. Heatmap of Cluster (diss.ctr <- dist(final$centers)) ----
# *******************************************************************************
# create a distance matric describing the disimilarity among the 250 clusters
diss_ctr <- dist(final$centers)
#heatmap(as.matrix(diss_ctr), hclustfun= function(d) hclust(d, method="ward.D2"))
barplot(height=summary(jobs_summary_tbl$cluster, base_centers), names.arg=1:base_centers, main="cluster sizes")
write.csv(data_raw_tbl, paste0(PATH_OUT, "out_cluster_5_1.csv"), row.names = FALSE)

data_raw_outlier_tbl <- readr::read_csv(paste0(PATH_OUT, "out_job_title_summary_outliers.csv"))
data_raw_outlier_tbl[is.na(data_raw_tbl)] <- 0
data_raw_outlier_tbl$cluster <- 99
data_raw_outlier_tbl <- data_raw_outlier_tbl %>% 
	select(job_title, Count, Q1, Median, Q3, Max, cluster) 
write.csv(data_raw_outlier_tbl, paste0(PATH_OUT, "out_cluster_5_outlier.csv"), row.names = FALSE)


