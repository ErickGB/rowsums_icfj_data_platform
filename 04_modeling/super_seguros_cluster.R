gc()
cat("\014") 
# **********************************************************************
# Created by: Erick Gordon
# Date: 11 may 2020
# Description:  Cluster analysis ---- 
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
# **********************************************************************
# Constant 
PATH_IN <- "./00_Data/in/insurance/"
PATH_OUT <- "./00_data/out/insurance/"
actual_month <- "sep" # 
source("./00_scripts/base_functions.R")
# *******************************************************************************

data_tbl <- readr::read_csv(paste0(PATH_IN, "out_insurance_process.csv"))
data_tbl$anio <- paste0("01-", substr(data_tbl$file_name, 8, 14))
data_tbl$anio <- as.Date(data_tbl$anio, tryFormats = c("%d-%m-%Y"))

train_tbl <- data_tbl %>% 
	group_by(entidad, producto) %>% 
	summarise(avg_month = mean(porcentaje_sieniestro)) %>% 
	ungroup() %>% 
	spread(key = producto, value = avg_month) %>% 
	janitor::clean_names()

summary_tbl <-  data_tbl %>% 
	group_by(entidad, producto) %>% 
	summarise(
		prima = mean(prima),
		siniestro = mean(porcentaje_sieniestro),
		porcentaje_sieniestro = mean(porcentaje_sieniestro)
	) %>% 
	ungroup() 

train_tbl %>% 
	glimpse()

train_tbl %>% 
	DataExplorer::plot_missing()

entidad <- train_tbl$entidad
train_tbl$entidad <- NULL 

# *******************************************************************************
col_skew_names <- train_tbl %>%
	select_if(is.numeric) %>%
	map_df(skewness) %>%
	gather(factor_key = T) %>%
	arrange(desc(value)) %>%
	filter(value >= 0.8) %>%
	pull(key) %>%
	as.character()
ncol <- ncol(train_tbl)

colnames(train_tbl)

rec_obj <- recipe(~ ., data = train_tbl) %>%
	step_YeoJohnson(col_skew_names) %>% 
	#step_meanimpute(impute_cols) %>% 
	#step_rm(remove_col) %>% 
	#step_center(all_numeric()) %>%  
	#step_scale(all_numeric()) %>% 
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
max_clust <- 15    # Hypothesized maximum number of segments

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


# After, create cluster with k selected
base_centers <- 2 #  0.4715790
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

train_tbl$cluster <- as.factor(clusters$cluster)     
kms_res <- eclust(train_prepared_tbl, "kmeans", k = base_centers, nstart = 6, graph = FALSE)
fviz_silhouette(kms_res, palette = "jco", ggtheme = theme_classic()) 

# Visualize k-means clusters
fviz_cluster(kms_res, geom = "point", ellipse.type = "norm",
						 palette = "jco", ggtheme = theme_minimal())


table(train_tbl$cluster)
train_tbl$entidad <- entidad
cluster_1 <- train_tbl %>% 
	filter(cluster == 1) %>% 
	dplyr::select(entidad) %>% 
	pull()


t <- train_tbl %>% 
	filter(entidad %in% c(cluster_1))
View(t)

data_tbl <- data_tbl %>% 
	mutate(
		cluster = ifelse(entidad %in% cluster_1, "cluster 1", "cluster 2")
	)
table(data_tbl$entidad, data_tbl$cluster)

write.csv(data_tbl, paste0(PATH_OUT, "cluster_insurance.csv"), row.names = FALSE)
