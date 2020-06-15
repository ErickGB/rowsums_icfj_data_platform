cat("\014")
# ********************************************************************
# Erick Gordón B.
# erick.gordon@rowsums.com
# Panama City, Panama 

library(tidyverse)
library(tidyquant)
library(DataExplorer)
library(magrittr)
library(lubridate)
library(tibble)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(rvest)
library(bbplot) 


# PCA Analisys 
library(corrplot) # correlation vizualization
library(caTools) # split data
library(factoextra) # fitting variables
library(FactoMineR)
library(caret) # improve model
library(h20)

source("./00_scripts/base_functions.R")
PATH_IN <- "./00_data/in/covid"
# ********************************************************************
list_files <- list.files(PATH_IN)
list_files <- c("CORREGIMIENTOS_(PU)-2020-06-02.csv")

master_tbl <- tibble()
for(i in 1:length(list_files)) # 
{
	file_name <- list_files[i]
	print(paste0("loading..", file_name))
	temp_tbl <- readr::read_csv(paste0(PATH_IN, "/", file_name))
	temp_tbl$date <- substr(file_name, (nchar(file_name) - 11), (nchar(file_name) - 4))
	
	if(file_name =="CORREGIMIENTOS_(PU)-9_14052020.csv") {
		temp_tbl <- temp_tbl %>% mutate(X = NULL, Y = NULL)
	}
	
	if(i == 1) {
		master_tbl <- temp_tbl
	} else {
		master_tbl <- rbind(master_tbl, temp_tbl)
	}
}

nrow(master_tbl)
table(master_tbl$date)

master_tbl %>% 
	glimpse()



# ***************************************************
# sum
sum(master_tbl$CANTIDAD, na.rm = TRUE) # 13841
sum(master_tbl$HOSPITALIZADO, na.rm = TRUE) # 305
sum(master_tbl$UCI, na.rm = TRUE) # 78
sum(master_tbl$FALLECIDO, na.rm = TRUE) # 343

#*****************************************
## Initial Cluster analysis (k selection) ----
#*****************************************
set.seed(7777)

master_tbl <- master_tbl %>% 
	janitor::clean_names() %>% 
	dplyr::select(provincia, corregimiento, global_id, cantidad, hospitalizado, aislamiento_domiciliario, fallecido, uci, recuperado)
master_tbl[is.na(master_tbl)] <- 0

master_tbl <- master_tbl %>% 
	filter(cantidad > 0)

training_cluster_tbl <- master_tbl


# **********************************************************************
# Normalization (Min-Max scaling) it's best for k-means and pca. But outliers can be deleted before to use. ----
set_range_standarize_process <- function(x){(x-min(x))/(max(x)-min(x))}
training_cluster_tbl<- apply(training_cluster_tbl[complete.cases(training_cluster_tbl),4:ncol(training_cluster_tbl)], MARGIN=2, 
																	 FUN=set_range_standarize_process) %>% 
	as_tibble() 


# **********************************************************************
# clean outliers 
outliers_tbl <- get_outliers(training_cluster_tbl)
table(outliers_tbl)

training_cluster_tbl$outlier <- outliers_tbl 
master_tbl$outlier <- outliers_tbl 

master_tbl %>% 
	filter(outlier == 1)

# discart outliers
training_cluster_tbl <- training_cluster_tbl %>% 
	filter(outlier == 0) %>% 
	mutate(outlier = NULL)



#Exploratory for find the best numbers of groups (k) 
# Setup for k-means loop 
km_out <- list()
sil_out <- list()
x <- vector()
y <- vector()
min_clust <- 2      # Hypothesized minimum number of segments
max_clust <- 130    # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
num_cols <- ncol(training_cluster_tbl)  # don't include codigo 
for (centr in min_clust:max_clust) {
	i <- centr-(min_clust-1) # relevels start as 1, and increases with centr
	print(paste0("cluster:", centr))
	#set.seed(777) # For reproducibility
	km_out[i] <- list(kmeans(training_cluster_tbl[, 1:num_cols], centers = centr, iter.max=100000, nstart=1))
	sil_out[i] <- list(cluster::silhouette(km_out[[i]][[1]], dist(training_cluster_tbl[, 1:num_cols])))
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
	arrange(desc(y))
d %>% 
	head(14)

km_out[3]



kms_res <- eclust(training_cluster_tbl, "kmeans", k = 5, nstart = 25, graph = FALSE)
fviz_silhouette(kms_res, palette = "jco", ggtheme = theme_classic()) 


# Visualize k-means clusters
fviz_cluster(kms_res, geom = "point", ellipse.type = "norm",
						 palette = "jco", ggtheme = theme_minimal())


# ************************
# visualización en 3D usando Plotly (otra librería)
library(plotly)
master_tbl$cluster <- kms_res$cluster
plot_ly(master_tbl, x = ~hospitalizado, y = ~uci, z = ~fallecido, 
						 marker = list(color = ~cluster, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE, labels = ~corregimiento)) %>%
	add_markers() %>%
	layout(scene = list(xaxis = list(title = 'hospitalizado'),
											yaxis = list(title = 'uci'),
											zaxis = list(title = 'fallecidos')),
				 annotations = list(
				 	x = 1.13,
				 	y = 1.05,
				 	text = 'pacientes',
				 	xref = 'pacientes',
				 	yref = 'pacientes',
				 	showarrow = FALSE
				 ))

# ***************************************************
# pre process for PCA

processing_tbl <- master_tbl %>% 
	mutate(id = paste0("x_", rownames(.))) %>% 
	mutate(CANTIDAD = ifelse(is.na(CANTIDAD), 0, CANTIDAD)) %>% 
	filter(CANTIDAD > 0) %>%  
	mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%y-%m-%d",  "%d%m%Y"))) %>% 
	dplyr::select(PROVINCIA, CORREGIMIENTO, GlobalID, id, CANTIDAD, date) %>% 
	janitor::clean_names() %>% 
	pivot_wider(names_from = id, values_from = cantidad) 
processing_tbl[is.na(processing_tbl)] <- 0

processing_tbl %>% 
	glimpse()

train_tbl <- processing_tbl
train_tbl$global_id <- NULL 
train_tbl$date <- NULL 

#train_tbl %>% 
#	plot_correlation()


# ***************************************************
# PCA Analysis 
library(tidymodels)

#  we update the role for global_id, date, since these are variables we want to keep around for convenience as identifiers for rows but are not a predictor or outcome
pca_rec <- recipe(~., data = processing_tbl) %>%
	update_role(provincia, corregimiento, global_id, date, new_role = "id") %>%
	step_normalize(all_predictors()) %>% # center and scale the numeric predictors
	step_pca(all_predictors())           # actual principal component analysis

pca_prep <- prep(pca_rec) # gets evaluate
pca_prep


# Visualize 
tidied_pca <- tidy(pca_prep, 2) # Turn an object into a tidy tibble

tidied_pca %>%
	filter(component %in% paste0("PC", 1:5)) %>%
	mutate(component = fct_inorder(component)) %>%
	ggplot(aes(value, terms, fill = terms)) +
	geom_col(show.legend = FALSE) +
	facet_wrap(~component, nrow = 1) +
	labs(y = NULL)


juice(pca_prep) %>%
	ggplot(aes(PC1, PC2, label = corregimiento)) +
	geom_point(aes(color = provincia), alpha = 0.7, size = 2) +
	geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
	labs(color = NULL)


# **********************************************************************
# Principal Components lap 2 - selected variables 
# **********************************************************************
# PCA 
colnames(train_tbl)
response_pca <- FactoMineR::PCA(train_tbl %>% dplyr::select_if(is.numeric), ncp=11, scale.unit = TRUE, graph = FALSE)
print(response_pca)
summary(response_pca) 

# pre-conditions
# scale data 
#plot(sapply(data.frame(cor(train_tbl)), var))


# *******************************
# - Total Intertia - 
#  Eigenvalues correspond to the amount of the variation explained by each principal component 
#  Elbow method : Visualize eigenvalues/variances
fviz_screeplot(response_pca, addlabels = TRUE, ncp=33)
# trace is the total sum of eigenvalues 
eigenvalues <- response_pca$eig
head(eigenvalues[1:9, 1:3], 20)


# *******************************
# -  The correlation circle - 
# Variables factor map 
# Coordinates of variables on the principal components

# Control variable colors using their contribution
# Possible values for the argument col.var are :
# "cos2", "contrib", "coord", "x", "y"
fviz_pca_var(response_pca, col.var="contrib",
						 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
						 repel = FALSE # Avoid text overlapping
)

