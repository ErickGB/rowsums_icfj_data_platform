cat("\014")
# ********************************************************************
# Erick Gordón B.
# erick.gordon@rowsums.com
# Panama City, Panama 
# ********************************************************************
set.seed(777)
library(tidymodels)
library(tidyr)
library(h2o)
library(mlflow)
require(knitr)

source("./00_scripts/base_functions.R")
PATH_IN <- "./00_data/in/covid"
PATH_OUT <- "./00_data/out/covid"
# 24 junio - destituyen a turner


# ********************************************************************
# load data ----
list_files <- list.files(PATH_IN)
#list_files <- c("CORREGIMIENTOS_(PU)-2020-06-05.csv")

master_tbl <- tibble()
for(i in 1:length(list_files)) # 
{
	file_name <- list_files[i]
	extension <- str_sub(file_name, nchar(file_name) - 2, nchar(file_name))
	if(extension == 'csv'){
		print(paste0("loading..", file_name))
		temp_tbl <- readr::read_csv(paste0(PATH_IN, "/", file_name))
		temp_tbl$date <- substr(file_name, (nchar(file_name) - 11), (nchar(file_name) - 4))
		temp_tbl$file_name <- file_name
		
		if(i == 1) {
			master_tbl <- temp_tbl
		} else {
			master_tbl <- rbind(master_tbl, temp_tbl)
		}
	} 
}


master_tbl <- master_tbl %>% 
	janitor::clean_names() %>% 
	mutate(
		date = as.Date(date, tryFormats = c('%y-%m-%d')),
		letalidad = round(fallecido / cantidad, 4),
		mortalidad = fallecido / sum(fallecido)
	)  %>% 
	mutate(
		provincia = stringr::str_trim(provincia, side = "both"), 
		corregimiento = stringr::str_trim(corregimiento, side = "both") 
				 ) %>% 
	mutate(objectid = paste0(provincia, "_", distrito ,"_", corregimiento)) #%>% 
	filter(date <= as.Date("2020-07-08", tryFormats = c("%Y-%m-%d")) )


master_tbl %>% 
	glimpse()

summary_tbl <- master_tbl  %>% 
	group_by(date) %>% 
	summarise(
		across(cantidad:recuperado, sum, na.rm = TRUE),
		mean_letalidad = mean(letalidad, na.rm = TRUE), .groups = 'drop'
		) %>% 
	mutate(
		perc_uci = (uci / cantidad) ,
		perc_hosp = (hospitalizado / cantidad),
		per_domiciliario = (aislamiento_domiciliario / cantidad)
	)
View(summary_tbl)

summary_tbl <- master_tbl  %>% 
	mutate(key = paste0(provincia, "_", distrito, "_", corregimiento)) %>% 
	mutate(key = iconv(key, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
	group_by(key, date) %>% 
	summarise(
		across(cantidad:recuperado, sum, na.rm = TRUE),
		mean_letalidad = mean(letalidad, na.rm = TRUE), .groups = 'drop'
	) 
View(summary_tbl)
summary_tbl %>% 
	glimpse()
write.csv(summary_tbl, paste0(PATH_OUT, "/covid_daily_cluster_key.csv"), row.names = FALSE)


# benford law
master_tbl %>% 
	filter(cantidad > 0) %>% 
	filter(date == "2020-08-16") %>% 
	mutate(first_digit = substr(as.character(cantidad), 1, 1)) %>%
	count(first_digit) %>%
	mutate(percent = (n / sum(n))*100  ) %>%
	arrange(first_digit)





# start mlFlow 
with(mlflow_start_run(), { 
	# mlFlow parameters
	first_date <- min(master_tbl$date)
	last_date <- max(master_tbl$date)
	mlflow_param("first_date", first_date, "string")
	mlflow_param("last_date", last_date, "string")
	message(paste("First date:", first_date))
	message(paste("Last date:", last_date))
	
	# clear
	start_tbl <- master_tbl %>% 
		filter(date == first_date) %>% 
		mutate(cantidad = ifelse(is.na(cantidad) == TRUE, 0, cantidad)) %>% 
		rename(cantidad_original = cantidad, corregimiento_original = corregimiento) %>% 
		dplyr::select(objectid, corregimiento_original, cantidad_original)
	
	# sin casos
	sin_casos_tbl <-  master_tbl %>% 
		filter(date == last_date) %>% # as.Date("14-06-2020", tryFormats = '%d-%m-%Y')
		filter(cantidad <= 0 | is.na(cantidad)) %>% 
		mutate(cluster = 100)
	
	# join start with final
	master_tbl <- left_join(master_tbl, start_tbl, by = 'objectid') %>% 
		filter(date == last_date) %>% 
		mutate(
		diferencia = cantidad - cantidad_original,
		porcentaje_diferencia = round( ((cantidad - cantidad_original)/cantidad_original) * 100, 2)
		)  %>% 
		mutate(corregimiento_original = corregimiento) %>% 
		janitor::clean_names() 

	# data clear
	master_tbl[is.na(master_tbl)] <- 0
	original_tbl <- master_tbl
	original_tbl <- original_tbl 
	
	master_tbl <- master_tbl %>% 
		filter(cantidad > 0) 
	
	nrow(master_tbl)
	nrow(sin_casos_tbl)	
	
	
	master_tbl %>% 
		filter(corregimiento != corregimiento_original | is.na(corregimiento_original) ) %>% 
		dplyr::select(objectid, corregimiento_original, corregimiento)
	
	#write.csv(master_tbl, paste0(PATH_OUT, "out_reference_covid.csv"), row.names = FALSE)
	master_tbl %>% 
		glimpse()
	
	master_tbl %>% 
		filter(cantidad_original == 0 | is.na(cantidad_original))
	
	message(paste0(" With cases:", as.character(last_date), " Records:", as.character(nrow(master_tbl))))
	message(paste0(" Without cases:", as.character(last_date), " Records:", as.character(nrow(sin_casos_tbl))))
	
	
	# ***************************************************
	# sum
	sum(master_tbl$cantidad, na.rm = TRUE) # 13841
	sum(master_tbl$hospitalizado, na.rm = TRUE) # 305
	sum(master_tbl$uci, na.rm = TRUE) # 78
	sum(master_tbl$fallecido, na.rm = TRUE) # 343
	mean(master_tbl$letalidad, na.rm = TRUE)
	
	master_tbl %>% 
		mutate(cantidad = aislamiento_domiciliario) %>% 
		dplyr::select(objectid, cantidad, hospitalizado, fallecido, uci, diferencia, porcentaje_diferencia) %>% 
		DataExplorer::plot_missing()
	
	
	# create train data 
	train_tbl <- master_tbl %>% 
		dplyr::select(objectid, cantidad, hospitalizado, fallecido, uci, letalidad) # 
	train_tbl[is.na(train_tbl)] <- 0
	
	#train_tbl$porcentaje_diferencia <- ifelse(is.infinite(train_tbl$porcentaje_diferencia)
	#																					, train_tbl$diferencia
	#																					, train_tbl$porcentaje_diferencia)
	#summary(train_tbl$porcentaje_diferencia)
	
	# **********************************************************************
	# Normalization (Min-Max scaling) it's best for k-means and pca. But outliers can be deleted before to use. ----
	set_range_standarize_process <- function(x){(x-min(x))/(max(x)-min(x))}
	train_normalized_tbl<- apply(train_tbl[complete.cases(train_tbl),2:ncol(train_tbl)], MARGIN=2, 
															 FUN=set_range_standarize_process) %>% 
		as_tibble() 
	
	print(train_normalized_tbl %>% 
		head())
	
	# **********************************************************************
	# clean outliers 
	outliers_lst <- get_outliers(train_normalized_tbl)
	out <- as_tibble(table(outliers_lst))
	# mlfow message
	mlflow_log_param("Outliers_0", as.character(out$n[1]))
	mlflow_log_param("Outliesr_1", as.character(out$n[2]))
	message(paste("Outlier 0", as.character(out$n[1])))
	message(paste("Outlier 1", as.character(out$n[2])))
	train_normalized_tbl <- train_tbl[, 2:ncol(train_tbl)]
	
	train_tbl$outlier <- 0#outliers_lst 
	master_tbl$outlier <- 0#outliers_lst 
	train_normalized_tbl$outlier <- 0#outliers_lst 
	
	outliers_tbl <- train_tbl %>% 
		filter(outlier == 1)
	
	# discart outliers
	train_tbl <- train_tbl %>% 
		filter(outlier == 0) %>% 
		mutate(outlier = NULL)
	object_id <-train_tbl$objectid
	train_tbl$objectid <- NULL
	
	train_normalized_tbl <- train_normalized_tbl %>% 
		filter(outlier == 0) %>% 
		mutate(outlier = NULL)
	train_normalized_tbl$global_id <- NULL
	
	#*****************************************
	## Initial Cluster analysis (k selection) ----
	#*****************************************
	
	# Let’s say we want to explore the effect of different choices of k, from 1 to 9, on this clustering.
	kclusts <- 
		tibble(k = 1:50) %>%
		mutate(
			kclust = map(k, ~kmeans(train_normalized_tbl, .x, iter.max=10000)),
			tidied = map(kclust, tidy),
			glanced = map(kclust, glance),
			augmented = map(kclust, augment, train_tbl)
		) 
	
	kclusts %>% 
		glimpse()
	
	
	# Each of these goes into a separate data set as they represent different types of data.
	clusters <- 
		kclusts %>%
		unnest(cols = c(tidied))
	
	assignments <- 
		kclusts %>% 
		unnest(cols = c(augmented))
	
	clusterings <- 
		kclusts %>%
		unnest(cols = c(glanced))
	
	# ***********************************
	# plots
	
	# Of particular interest is the total within sum of squares, saved in the tot.withinss column.
	ggplot(clusterings, aes(k, tot.withinss)) +
		geom_line() +
		geom_point()
	
	cluster_tbl <- clusterings %>% 
		arrange(desc(tot.withinss))
	View(cluster_tbl)
	
	# Now we can plot the original points
	p1 <- assignments %>% 
		filter(k <= 16) %>% 
		ggplot(aes(x = hospitalizado, y = cantidad)) +
		geom_point(aes(color = .cluster), alpha = 0.8) + 
		facet_wrap(~ k)
	p1
	
	# We can then add the centers of the cluster using the data from tidy():
	#p1 + geom_point(data = clusters, size = 10, shape = "x")
	
	
	
	# ************************
	library(factoextra) # fitting variables
	library(FactoMineR)
	library(caret) # improve model
	set.seed(777)
	# set colors
	#RColorBrewer::display.brewer.all()
	nb.cols <- 19
	my_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "RdYlBu"))(nb.cols)
	
	# 
	total_clusters <- 5
	mlflow_log_param("k", as.character(total_clusters))
	
	kms_res <- eclust(train_normalized_tbl, "kmeans", k = total_clusters, nstart = 25, graph = FALSE)
	viz_result <- fviz_silhouette(kms_res, palette = my_colors, ggtheme = theme_classic()) 
	viz_result
	
	# Visualize k-means clusters
	fviz_cluster(kms_res, geom = "point", ellipse.type = "norm",
							 palette = my_colors, ggtheme = theme_minimal())
	
	
	cluster_review_tbl <- viz_result$data %>% 
		group_by(cluster) %>% 
		summarise(avg_sil_width = mean(sil_width)) %>% 
		ungroup() %>% 
		mutate(cluster = as.integer(cluster)) %>% 
		arrange(desc(avg_sil_width))
	message(paste0(" Avg. sillloute width:", mean(viz_result$data$sil_width)))
	mlflow_log_metric("sillloute", mean(viz_result$data$sil_width))
	mean(cluster_review_tbl$avg_sil_width)
	cluster_review_tbl
	
	#cluster avg_sil_width
	#<int>         <dbl>
  #1       5         0.880
	#2       4         0.751
	#3       3         0.582
	#4       1         0.566
	#5       2         0.461
	
	# ************************
	# 
	outliers_join_tbl <- outliers_tbl %>% 
		mutate(
			cluster = 99
		) %>% 
		dplyr::select(objectid, cluster)
	
	final_tbl <- train_tbl %>% 
		mutate(
			objectid = object_id, 
			cluster = as.integer(kms_res$cluster)
		) %>% 
		dplyr::select(objectid, cluster) %>% 
		#bind_rows(outliers_join_tbl) %>% 
		bind_rows(sin_casos_tbl[, c("objectid", "cluster")]) %>% 
		left_join(., y = cluster_review_tbl, by = 'cluster' ) 

	
	original_tbl %>% 
		glimpse()
	
	final_tbl %>% 
		glimpse()
	
	final_tbl <- left_join(original_tbl, final_tbl, by = 'objectid') %>% 
		mutate(letalidad = round(letalidad * 100, 2))
	final_tbl[is.na(final_tbl)] <- 0 
	
	final_tbl <- final_tbl %>% 
		mutate(diferencia = cantidad - cantidad_original) %>% 
		dplyr::select(objectid, provincia, distrito, corregimiento, corregimiento_original, date,
									cantidad, cantidad_original, diferencia, porcentaje_diferencia, 
									aislamiento_domiciliario, hospitalizado, uci, fallecido, recuperado, 
									letalidad, mortalidad, 
									long, lat, cluster, avg_sil_width)
	
	
	# summary
})	

viz_result
cluster_review_tbl
table(final_tbl$cluster)
final_tbl %>% 
	glimpse()

sum(final_tbl$cantidad)


#mlflow::mlflow_end_run()
mlflow::mlflow_ui()	
	
PATH_OUT
write.csv(final_tbl, paste0(PATH_OUT, "/cluster_covid.csv"), row.names = FALSE)
#h2o.shutdown()


#final_tbl <- readr::read_csv(paste0(PATH_OUT, "/cluster_covid.csv"))
# ************************
# Tree analysis 
library(C50)
model_tree <- C50::C5.0(final_tbl[, c("aislamiento_domiciliario", "hospitalizado", 
																			"fallecido", "uci", "letalidad")], as.factor(final_tbl$cluster))
summary(model_tree)
plot(model_tree)

#Decision tree:  9.9% error
#	aislamiento_domiciliario <= 0:
#	:...fallecido <= 0: 100 (345/53)
#:   fallecido > 0: 4 (9)
#aislamiento_domiciliario > 0:
#	:...aislamiento_domiciliario <= 79: 4 (264/8)
#aislamiento_domiciliario > 79:
#	:...aislamiento_domiciliario > 261:
#	:...aislamiento_domiciliario <= 563: 2 (17/3)
#:   aislamiento_domiciliario > 563: 1 (6)
#aislamiento_domiciliario <= 261:
#	:...fallecido > 7: 5 (13/1)
#fallecido <= 7:
#	:...aislamiento_domiciliario <= 180: 3 (20/1)
#aislamiento_domiciliario > 180: 5 (5/1)


# Load rpart and rpart.plot
library(rpart)
library(rpart.plot)
# Create a decision tree model
tree_data_tbl <- final_tbl %>% 
	mutate(cluster = as.factor(cluster)) %>% 
	dplyr::select(cluster, aislamiento_domiciliario, hospitalizado, fallecido, uci, letalidad)

tree <- rpart(cluster~., data=tree_data_tbl, cp=.02)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, type = 2, yesno=2, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#1) root 728 352 100 (0.019 0.0014 0.049 0.36 0.022 0.0069 0.023 0.52)  
#2) cantidad>=0.5 348  89 4 (0.04 0.0029 0.092 0.74 0.046 0.014 0.049 0.011)  
#4) cantidad>=71.5 75  43 3 (0.19 0.013 0.43 0 0.21 0.067 0.067 0.027)  
#8) cantidad< 238 34   2 3 (0 0 0.94 0 0 0 0 0.059) *
#	9) cantidad>=238 41  25 5 (0.34 0.024 0 0 0.39 0.12 0.12 0)  
#18) cantidad>=428 24  10 1 (0.58 0.042 0 0 0 0.21 0.17 0) *
#	19) cantidad< 428 17   1 5 (0 0 0 0 0.94 0 0.059 0) *
#	5) cantidad< 71.5 273  14 4 (0 0 0 0.95 0 0 0.044 0.0073)  
#10) letalidad< 13.81 261   2 4 (0 0 0 0.99 0 0 0 0.0077) *
#	11) letalidad>=13.81 12   0 99 (0 0 0 0 0 0 1 0) *
#	3) cantidad< 0.5 380   8 100 (0 0 0.011 0.011 0 0 0 0.98) *


# ************************
# EDA 
# ************************

# summary 
summary_tbl <- final_tbl %>% 
	group_by(cluster) %>% 
	summarise(
		n = n(), 
		mean_withinss = mean(avg_sil_width),
		across(c("cantidad", "cantidad_original", 
						 "diferencia", "porcentaje_diferencia", "hospitalizado", 
						 "fallecido", "uci"), list(mean = mean, median = median, max = max))
						)
	#summarise(n = n(), mean_withinss = mean(avg_sil_width),  
	#					cantidad = mean(cantidad), hosp = mean(hospitalizado), 
	#					mean_uci = mean(uci), mean_fallec = mean(fallecido), 
	#					mean_aislamiento = mean(aislamiento_domiciliario),
	#					mean_letalidad = mean(letalidad), max_letalidad = max(letalidad),
	#					max_fallecido = max(fallecido), ) %>% 
	#arrange(desc(cluster)) %>% 
	#head(20)
View(summary_tbl)

#print(kable(summary_tbl))

#scatter plot cluster por hospitalizado vs fallecido 
pscatter <- final_tbl %>% 
	ggplot(aes(x = hospitalizado, y = fallecido)) +
	geom_point(aes(color = as.factor(cluster), size = cantidad), alpha = 0.8) + 
	facet_wrap(~ cluster)

plotly::ggplotly(pscatter)

final_tbl %>% 
	ggplot(aes(x = letalidad, y = uci)) +
	geom_point(aes(color = cluster, size = cantidad), alpha = 0.8) + 
	facet_wrap(~ cluster)

final_tbl %>% 
	ggplot(aes(x = hospitalizado, y = letalidad)) +
	geom_point(aes(color = cluster, size = cantidad), alpha = 0.8) + 
	facet_wrap(~ cluster)


# maxima de casos 
final_tbl %>% 
	filter(cluster %in% c(4, 2)) %>% 
	dplyr::select(distrito, corregimiento, cluster, letalidad, cantidad, fallecido, uci)

# maxima de letalidad 
final_tbl %>% 
	filter(cluster %in% c(3)) %>% 
	dplyr::select(distrito, corregimiento, cluster, letalidad, cantidad, fallecido, uci) %>% 
	arrange(desc(letalidad))



# visualización en 3D usando Plotly (otra librería)
c('#4575b4', '#74add1', '#abd9e9', '#e0f3f8', 
	'#ffffbf', '#fee090', '#fdae61', '#f46d43', 
	'#d73027', '#4d4d4d')

library(plotly)
final_tbl$cluster <- as.factor(final_tbl$cluster)
fig <- plot_ly(final_tbl, ~hospitalizado, ~fallecido,  ~letalidad,  opacity = 0.8, # size = ~cantidad,
							 text = ~paste('Correg:', corregimiento,    "(cluster: ", as.character(cluster), ") ",  
							 							"<br>Hospit. ", as.character(hospitalizado), " Fallec. ", as.character(fallecido), 
							 							" UCI ", as.character(uci), " Letalidad ", as.character(letalidad)),
							 color = ~cluster, colors = c('#a50026', '#f46d43', '#f46d43', '#fee090', 
							 														 '#fee090', '#e0f3f8', '#74add1', '#4575b4', 
							 														 '#4d4d4d', '#4d4d4d')) 
fig <- fig %>%
	#add_markers() %>% 
	layout(
		title = "Clúster casos de covid en Panamá",
		scene = list(xaxis = list(title = 'x = Hospitalizados'),
								 yaxis = list(title = 'y = Fallecidos'),
								 zaxis = list(title = 'z = % Letalidad'))
	)
fig

'#74add1', 

# fallecidos y uci por cluster 
final_tbl %>%
	pivot_longer(cantidad:recuperado, names_to = "stat", values_to = "value") %>%
	filter(stat %in% c("fallecido", "uci")) %>% 
	ggplot(aes(stat, value, fill = stat, color = stat)) +
	geom_boxplot(alpha = 0.4) +
	facet_wrap(~cluster, scales = "free_y", ncol = 4) +
	labs(y = NULL, color = NULL, fill = NULL)


final_tbl %>%
	filter(cluster %in% c(99, 9, 4)) %>% 
	ggplot(aes(x = hospitalizado, y = fallecido, colour = as.factor(cluster))) + 
	geom_point()

