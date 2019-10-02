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
# *******************************************************************************
# Constant 
PATH_IN <- "./00_Data/in/salaries/"
PATH_OUT <- "./00_data/out/salaries/"
# *******************************************************************************
# load data ----
data_raw_tbl <- readr::read_csv(paste0(PATH_OUT, "out_job_title_summary.csv"))
data_raw_tbl[is.na(data_raw_tbl)] <- 0

outliers_tbl <- readr::read_csv(paste0(PATH_OUT, "out_cluster_5_outlier.csv"))
outliers_tbl <- outliers_tbl %>% 
	filter(cluster == 99)

data_raw_tbl <- data_raw_tbl %>% 
	filter(!(job_title %in% outliers_tbl$job_title))


data_raw_tbl %>% 
	glimpse()


# Skew data check ----
col_skew_names <- data_raw_tbl %>% 
	filter(Skewed > 8) %>% 
	dplyr::select(job_title) %>% 
	pull()

data_raw_tbl <- data_raw_tbl %>% 
	select(job_title, Count, Q1, Median) 
ncol <- ncol(data_raw_tbl)

rec_obj <- recipe(~ ., data = data_raw_tbl[, 2:ncol]) %>%
		#step_YeoJohnson(col_skew_names) %>% 
	  #step_meanimpute(impute_cols) %>% 
		#step_rm(remove_col) %>% 
	  step_center(all_numeric()) %>%  
	  step_scale(all_numeric()) %>% 
	  #step_zv(all_predictors()) %>% 
	  #step_dummy(character_cols) %>%
    prep()
rec_obj

train_tbl <- bake(rec_obj, data_raw_tbl[, 2:ncol]) 
#train_tbl$Count <- NULL 
train_tbl %>% head()

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
num_cols <- ncol(train_tbl) - 1 # don't include codigo 
for (centr in min_clust:max_clust) {
        i <- centr-(min_clust-1) # relevels start as 1, and increases with centr
        print(i)
        set.seed(777) # For reproducibility
        km_out[i] <- list(kmeans(train_tbl, centers = centr, iter.max=1000000, nstart=1))
        sil_out[i] <- list(cluster::silhouette(km_out[[i]][[1]], dist(train_tbl)))
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
#1    2 0.5532108
#2    3 0.4553970
#3    5 0.4209295
#4    4 0.4120493


# After, create cluster with k selected
base_centers <- 5 #  0.4715790
clusters <- list()
fit <- NA
# Because k-means is sensitive to starting conditions, the algorithm was randomly initialized 100,000 times
# Each run of k-means was allowed a maximum of 1,000,000 steps.**
for (i in 1:10000){ # 100000 
	#set.seed(i) # For reproducibility
  class_250 <- kmeans(x=data_raw_tbl[, 2:num_cols], centers= base_centers, iter.max=1000000, nstart=1)
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


data_raw_tbl$cluster <- as.factor(clusters$cluster)     
table(data_raw_tbl$cluster)
data_raw_tbl %>% head()

kms_res <- eclust(data_raw_tbl[, 2:num_cols], "kmeans", k = 5, nstart = 20, graph = FALSE)
fviz_silhouette(kms_res, palette = "jco", ggtheme = theme_classic()) 
# *******************************************************************************
# 5. Heatmap of Cluster (diss.ctr <- dist(final$centers)) ----
# *******************************************************************************
# create a distance matric describing the disimilarity among the 250 clusters
diss_ctr <- dist(final$centers)
#heatmap(as.matrix(diss_ctr), hclustfun= function(d) hclust(d, method="ward.D2"))
barplot(height=summary(data_raw_tbl$cluster, base_centers), names.arg=1:base_centers, main="cluster sizes")
write.csv(data_raw_tbl, paste0(PATH_OUT, "out_cluster_5_1.csv"), row.names = FALSE)

data_raw_outlier_tbl <- readr::read_csv(paste0(PATH_OUT, "out_job_title_summary_outliers.csv"))
data_raw_outlier_tbl[is.na(data_raw_tbl)] <- 0
data_raw_outlier_tbl$cluster <- 99
data_raw_outlier_tbl <- data_raw_outlier_tbl %>% 
	select(job_title, Count, Q1, Median, Q3, Max, cluster) 
write.csv(data_raw_outlier_tbl, paste0(PATH_OUT, "out_cluster_5_outlier.csv"), row.names = FALSE)


