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
library(bbplot)
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
httr::set_config(httr::config(http_version = 0))
# autentication - only one time
bq_auth(path = "./00_scripts/rowsums-2198b8679813.json", 
				email = "gordon.erick@gmail.com", #gargle::gargle_oauth_email(),
				cache = gargle::gargle_oauth_cache(),
				use_oob = gargle::gargle_oob_default())

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

data_raw_tbl %>% 
	glimpse()

# by subcategories
sub_category_tbl <- data_raw_tbl %>% 
	filter(year >= 2019) %>% 
	group_by(category_code, sub_category_code, sub_category) %>% 
	summarize(total_cif = sum(cif)) %>% 
	ungroup() %>% 
	arrange(desc(total_cif)) %>% 
	mutate(acum_percent = round(cumsum(total_cif)/sum(total_cif), digits = 2)) %>% 
	head(30)
sub_category_tbl <- collect(sub_category_tbl)
sub_category_tbl

# create scritp 
company_tbl <- data_raw_tbl %>% 
	filter(year >= 2019) %>% 
	group_by(company, category_code, sub_category_code, sub_category) %>% 
	summarize(total_cif = sum(cif))

company_tbl %>% 
	show_query()

# extract data from bigquery
company_tbl <- collect(company_tbl)

company_tbl <- company_tbl %>% 
	filter(sub_category_code %in% sub_category_tbl$sub_category_code)


company_tbl <- company_tbl %>% 
	ungroup() %>% 
	select(company, sub_category, total_cif) %>% 
	spread(key = sub_category, value = total_cif) %>% 
	janitor::clean_names()

company_tbl %>% 
	glimpse()

# data scale
company_tbl[is.na(company_tbl)] <- 0
ncol <- ncol(company_tbl)
train_prepared_tbl <- as_tibble(scale(company_tbl[, 2:ncol]))

# anomaly detection with Isolation Forest
outliers <- get_outliers(train_prepared_tbl)
table(outliers)

# assing outliers field
company_tbl$outlier <- as.integer(outliers)
table(company_tbl$outlier)

company_tbl$outlier <- as.integer(outliers)
train_prepared_tbl$outlier <- as.integer(outliers)
train_prepared_tbl <- train_prepared_tbl %>% 
	filter(outlier == 0) %>% 
	mutate(outlier = NULL)

data_raw_outlier_tbl <- company_tbl %>% 
	filter(outlier == 1)

data_raw_without_outlier_tbl <-  company_tbl %>% 
	filter(outlier == 0)

#*****************************************
## Initial Cluster analysis (k selection) ----
#*****************************************
set.seed(7777)

#Exploratory for find the best numbers of groups (k) 
# Setup for k-means loop 
#km_out <- list()
#sil_out <- list()
x <- vector()
y <- vector()
min_clust <- 2      # Hypothesized minimum number of segments
max_clust <- 20     # Hypothesized maximum number of segments

small_data <- train_prepared_tbl[sample(1:16000),,drop=FALSE]

# Compute k-means clustering over various clusters, k, from minClust to maxClust
num_cols <- ncol(small_data)  # don't include codigo 
for (centr in min_clust:max_clust) {
	i <- centr-(min_clust-1) # relevels start as 1, and increases with centr
	#set.seed(777) # For reproducibility
	km_out <- kmeans(small_data, centers = centr, iter.max=1000, nstart=1)
	sil_out <- cluster::silhouette(km_out[[1]], dist(small_data))
	# Used for plotting silhouette average widths
	x[i] = centr  # value of k
	y[i] = summary(sil_out)[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
	geom_point(size=3) + 
	geom_line() +
	bbc_style() + 
	xlab("Number of Cluster Centers") +
	ylab("Silhouette Average Width") +
	ggtitle("Silhouette Average Width as Cluster Center Varies")

data.frame(x, y) %>% 
	filter(y > 0.25) %>% 
	arrange(desc(y)) %>% 
	head(20)

# ***************************
# Genera el cluster fiinal, 
base_centers <- 8 # ¿Cuántos clústers deseas crear?
k_model <- eclust(train_prepared_tbl, "kmeans", k = base_centers,
									nstart = 2, graph = FALSE)
table(k_model$cluster)

# visualiza el silhouette de cada clúster
fviz_silhouette(k_model)

# otra visualización
fviz_cluster(k_model, geom = "point",  ellipse = FALSE)


# ************************
# visualización en 3D usando Plotly (otra librería)
train_prepared_tbl$cluster <- k_model$cluster
p <- plot_ly(train_prepared_tbl, x = ~combustibles_minerales_aceites_minerales_y_productos_de_su_destilacion_materias_bituminosas_ceras_minerales, y = ~reactores_nucleares_calderas_maquinas_aparatos_y_artefactos_mecanicos_partes_de_estas_maquinas_o, z = ~vehiculos_automoviles_tractores_velocipedos_y_demas_vehiculos_terrestres_sus_partes_y_accesorios,
						 marker = list(color = ~cluster, colorscale = c('#FFE1A1', '#683531', 'FFE1AA'), showscale = TRUE)) %>%
	add_markers() %>%
	layout(scene = list(xaxis = list(title = 'combustibles_minerales_aceites_minerales_y_productos_de_su_destilacion_materias_bituminosas_ceras_minerales'),
											yaxis = list(title = 'reactores_nucleares_calderas_maquinas_aparatos_y_artefactos_mecanicos_partes_de_estas_maquinas_o'),
											zaxis = list(title = 'vehiculos_automoviles_tractores_velocipedos_y_demas_vehiculos_terrestres_sus_partes_y_accesorios')),
				 annotations = list(
				 	x = 1.13,
				 	y = 1.05,
				 	text = 'Miles/(US) gallon',
				 	xref = 'paper',
				 	yref = 'paper',
				 	showarrow = FALSE
				 ))
p 

# assign cluster
data_raw_outlier_tbl$cluster <- 99
data_raw_without_outlier_tbl$cluster <- k_model$cluster
total_tbl <- rbind(data_raw_without_outlier_tbl, data_raw_outlier_tbl)

total_tbl <- total_tbl %>% 
	#filter(cluster == 99) %>% 
	select(company, outlier, cluster)

write.csv(total_tbl, paste0(PATH_OUT, "cluster_tbl.csv"))















