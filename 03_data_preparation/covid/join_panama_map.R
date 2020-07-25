cat("\014")
# ********************************************************************
# Erick Gordón B.
# erick.gordon@rowsums.com
# Panama City, Panama 
# ********************************************************************
set.seed(777)
library(tidymodels)
library(tidyr)
library(recipes)
library(rgdal)

PATH_IN <- "./00_data/in/maps/"
PATH_OUT <- "./00_data/out/covid" 


# ********************************************************************

# Load geojson polygon layer----
#mapping_tbl <- readr::read_csv(paste0(PATH_IN, "mapeo_inec_2010_2019.csv"))
mapping_tbl <- readr::read_csv2(paste0(PATH_IN, "mapping.csv")) %>% 
	mutate(
		cod10 = stringr::str_trim(cod10, side = "both"),
		cod19 = stringr::str_trim(cod19, side = "both")
		) %>% 
	mutate(
		lenght19 = nchar(cod19),
		lenght10 = nchar(cod10)
	) %>% 
	mutate(
		cod10 = ifelse(lenght10 == 5, paste0('0', cod10), cod10), 
		cod19 = ifelse(lenght19 == 5, paste0('0', cod19), cod19)
	) %>% 
	mutate(
		lenght19 = nchar(cod19),
		lenght10 = nchar(cod10)
	) 

mapping_tbl %>% 
	filter(lenght10 != 6) %>% 
	dplyr::select(cod10, cod19, lenght10, lenght19)


panama_map_geojson <- readOGR(paste0(PATH_IN, "poligonos_2019.geojson"), verbose = FALSE)
panama_map_geojson <- spTransform(panama_map_geojson, CRS(paste("+proj=utm +zone=",17," ellps=WGS84",sep='')))
# test_tbl <- readr::read_csv("./00_data/in/covid/CORREGIMIENTOS_(PU)-2020-06-26.csv")
# 679
test_tbl <- readr::read_csv(paste0(PATH_OUT, "/cluster_covid.csv")) %>% 
	janitor::clean_names() %>% 
	mutate(key = paste0(provincia, "_", distrito, "_", corregimiento)) %>% 
	rename(corregimiento_minsa = corregimiento) %>% 
	dplyr::select(cluster, objectid, cantidad, aislamiento_domiciliario, hospitalizado, 
								uci, fallecido, recuperado, 
								letalidad, mortalidad, diferencia, porcentaje_diferencia, 
								long, lat,
								key, corregimiento_minsa, avg_sil_width) %>% 
	mutate(
		casos_activos = aislamiento_domiciliario + hospitalizado + uci, 
		relacion_uci = uci / casos_activos
	)

panama_map_geojson@data <- panama_map_geojson@data %>% 
	mutate(key = paste0(PROV_NOMB, "_", DIST_NOMB, "_", CORR_NOMB)) %>% 
	janitor::clean_names()

panama_map_geojson@data %>% 
	glimpse()

repetidos_list <-  panama_map_geojson@data %>% 
	count(codigo) %>% 
	filter(n > 1) %>% 
	arrange(desc(n)) %>% 
	dplyr::select(codigo)

#cod n
#1 040513 2
#2 120604 2
#3 120606 2

test_tbl %>% 
	count(key) %>% 
	filter(n > 1) %>% 
	arrange(desc(n)) 

# repetido
mapping_tbl %>% 
	count(cod10, cod19) %>% 
	filter(n > 1) %>% 
	arrange(desc(n)) 

# separado en dos corregimientos = 679 + 4 = 683  max
mapping_tbl %>% 
count(cod19) %>% 
	filter(n > 1) %>% 
	arrange(desc(n)) 


# validate files 18,288 - 29040
#sum(minsa_geojson@data$CANTIDAD, na.rm = TRUE) 
sum(test_tbl$cantidad, na.rm = TRUE)

test_tbl %>% 
	glimpse()
sum(test_tbl$cantidad)

panama_map_geojson@data %>% 
	glimpse()

mapping_tbl %>% 
	glimpse()


# **************************
# "conform" data structure 
mapping_tbl <- mapping_tbl %>%
	mutate(provincia = ifelse(provincia == 'DARIÉN', 'DARIEN', provincia)) %>% 
	janitor::clean_names() %>%
	#rename(corregimiento = corregimie) %>%
	mutate(key = paste0(provincia, "_", distrito, "_", corregimiento)) #%>% 
	#mutate(key = ifelse(prov_id %in% key_prov, corregimiento, key)) #%>% 
	#dplyr::select(key, provincia, distrito, corregimiento, 
	#							cod10, cod19, dist_nomb, dist10, dist19, prov_id, prov_nomb, tipo)

mapping_tbl$dist10 <- NULL 
mapping_tbl$dist19 <- NULL 
mapping_tbl$prov_id <- NULL 
mapping_tbl$dist_nomb <- NULL 

panama_map_geojson@data <- panama_map_geojson@data %>%
	janitor::clean_names() %>%
	rename(cod19 = codigo) %>%
	mutate(cod19 = as.character(cod19), corr_map = corr_nomb) %>%
	dplyr::select(key, cod19, corr_map, corr_ley, corr_feac)


# final structure
mapping_tbl %>%
	glimpse()

panama_map_geojson@data  %>%
	glimpse()


rep_lit <- mapping_tbl %>% 
	count(key) %>% 
	filter(n > 1) %>%
	dplyr::select(key) %>% 
	pull()

mapping_tbl %>% 
	filter(key %in% rep_lit) %>% 
	dplyr::select(key, cod10, distrito) %>% # dist_nomb
	arrange(desc(key))


# codigos repetidos
rep_lit <- panama_map_geojson@data %>% 
	count(cod19) %>% 
	filter(n > 1) %>%
	dplyr::select(cod19) %>% 
	pull()

panama_map_geojson@data %>% 
	filter(cod19 %in% rep_lit) %>% 
	arrange(desc(cod19)) %>% 
	dplyr::select(cod19, corr_map)



test_tbl %>%
	glimpse()

rep_lit <- test_tbl %>% 
	count(key) %>% 
	filter(n > 1) %>%
	dplyr::select(key) %>% 
	pull()

test_tbl %>% 
	filter(key %in% rep_lit) %>% 
	dplyr::select(cluster, key, cantidad, fallecido) %>% 
	arrange(desc(cluster))


mapping_tbl %>%
	DataExplorer::plot_missing()

nrow(test_tbl)                 # minsa 
nrow(mapping_tbl)              # maping 2010 vs  2019
nrow(panama_map_geojson@data)  # map


# ********************************************************************
# join records ----
test_tbl <- test_tbl %>% 
	mutate(
		key = iconv(key, from = 'UTF-8', to = 'ASCII//TRANSLIT')
				 )

mapping_tbl <- mapping_tbl %>% 
	mutate(
		key = iconv(key, from = 'UTF-8', to = 'ASCII//TRANSLIT')
	)


join_mapping_tbl <- left_join(test_tbl, mapping_tbl, by = 'key')
join_mapping_tbl %>% 
	DataExplorer::plot_missing()

temp_tbl <-  join_mapping_tbl %>% 
	filter(is.na(cod10)) #%>% 
	#dplyr::select(objectid) %>% 
	#rename(key = objectid) 
View(temp_tbl)
View(mapping_tbl)


join_mapping_tbl %>% 
	glimpse()

# repetido
rep_tbl <- join_mapping_tbl %>% 
	group_by(cod19) %>%
	summarise(n = n(), total = sum(cantidad) ) %>% 
	filter(n > 1)
rep_tbl

join_mapping_tbl <- join_mapping_tbl %>% 
	mutate(
		rep_with_data = ifelse(cantidad > 0 & objectid %in%rep_tbl$cod19, 1, 0),
		tipo = stringr::str_trim(tipo, side = 'both')
				 )

View(join_mapping_tbl)
join_mapping_tbl %>% 
	filter(substr(cod19, 1, 2) == '09')


join_mapping_tbl %>% 
	filter(is.na(tipo)) %>% 
	mutate(flag_with_cases = ifelse(is.na(cantidad) == FALSE & cantidad > 0, 1, 0)) %>% 
	summarise(n = n(), with_cases = sum(flag_with_cases),  cases = sum(cantidad, na.rm = TRUE))

write.csv(join_mapping_tbl, paste0(PATH_OUT, "/mapping_covid_19.csv"), row.names = FALSE)


# ********************************************************
# load census data ---- 
get_data_wider <- function(data, column_name, column_value, replace_null_value) {
	col_name <- enquo(column_name)
	col_value <- enquo(column_value)
	
	print("first")
	# data
	temp_data_tbl <- data %>%
		mutate(
			n = row_number(),
			value_name = (!!col_value),
			pivot_name = ifelse(is.na(!!col_name), replace_null_value, (!!col_name) )) %>%
		dplyr::select(codigo, pivot_name, value_name)

	
	# sum by code
	sum_tbl <- data %>%
		mutate(
			n = row_number(),
			pivot_name = ifelse(is.na(!!col_name), replace_null_value, (!!col_name) )) %>%
		dplyr::select(codigo, pivot_name, (!!col_value)) %>% 
		group_by(codigo) %>% 
		summarise(sum_total = sum( (!!col_value)) )
	
	data <- left_join(temp_data_tbl, sum_tbl, by = 'codigo') %>% 
		mutate(percent_value = round( (value_name/sum_total) * 100, 2)) 	%>% 
		mutate(sum_total = NULL, value_name = NULL) %>% 
		pivot_wider(names_from = pivot_name, values_from = percent_value, 
								values_fill = list(pivot_name = 0)) %>%
		janitor::clean_names() %>%
		rename(cod10 = codigo)
	
	#data <- data %>%
	#	mutate(
	#		n = row_number(),
	#		pivot_name = ifelse(is.na(!!col_name), replace_null_value, (!!col_name) )) %>%
	#	dplyr::select(codigo, pivot_name, (!!col_value)) %>%
	#	pivot_wider(names_from = pivot_name, values_from = (!!col_value), 
	#							values_fill = list(pivot_name = 0)) %>%
	#	janitor::clean_names() %>%
	#	rename(cod10 = codigo)
	
	data[is.na(data)] <- 0
	return(data)
}


home_salary_tbl <- readr::read_csv("./00_data/in/maps/home_salary.csv") %>%
	get_data_wider(rango_salarial, hogares, "SalaryNE")
home_salary_tbl <- home_salary_tbl %>% 
	mutate(
	x100_599 = (x100_124 + x125_174 + x175_249 + x250_399 + x400_599),
	x600_1499 = (x600_799 + x800_999 + x1000_1499),
	x1500_2999 = (x1500_1999 + x2000_2499 + x2500_2999),
	x3000_4999 = (x3000_3499 + x3500_3999 + x4000_4499 + x4500_4999)
) %>% 
	dplyr::select(cod10, menos_100, x100_599, x600_1499, x1500_2999, x3000_4999, x5000_y_mas)

home_salary_tbl %>% 
	glimpse()


house_date_tbl <- readr::read_csv("./00_data/in/maps/house_date.csv") %>%
	get_data_wider(building_date, houses_date, "DateNE")

house_ceiling_tbl <- readr::read_csv("./00_data/in/maps/house_ceiling.csv") %>%
	get_data_wider(ceiling, hogares, "CeilingNE")

house_floor_tbl <- readr::read_csv("./00_data/in/maps/house_floor.csv") %>%
	get_data_wider(floor, hogares, "FloorNE")

house_water_tbl <- readr::read_csv("./00_data/in/maps/house_water.csv") %>%
	get_data_wider(water, hogares, "WaterNE")

home_persona_hogar_tbl <- readr::read_csv("./00_data/in/maps/home_persona_hogar.csv") %>%
	get_data_wider(personas, total_personas_hogar, "persona_hogar_NE")

people_age_tbl <- readr::read_csv("./00_data/in/maps/people_age.csv") 
people_tbl <- people_age_tbl %>% 
	rename(cod10 = codigo) %>% 
	group_by(cod10) %>% 
	summarise(poblacion = sum(total, na.rm = TRUE))

table(people_age_tbl$age_group_1, useNA = "always")
menos_25 <- c('0-4', '5-9', '10-14', '15-19', '20-24')
x25_45 <-   c('25-29', '30-34', '35-39', '40-44')
x45_65 <-   c('45-49', '50-54', '55-59', '60-64')

people_age_tbl <- people_age_tbl %>% 
	mutate(
		age_group = 'mas_65',
		age_group = ifelse(age_group_1 %in% c(menos_25), 'menos_25',  age_group),
		age_group = ifelse(age_group_1 %in% c(x25_45), 'x25_45',  age_group),
		age_group = ifelse(age_group_1 %in% c(x45_65), 'x45_65',  age_group)
	) %>%
	group_by(codigo, age_group) %>% 
	summarise(total_people = sum(total), .groups = "drop") %>% 
	get_data_wider(age_group, total_people, "AgeNE") 


people_grade_tbl <- readr::read_csv("./00_data/in/maps/people_grade.csv") %>%
	get_data_wider(grade, total, "GradeNE")

census_tbl <- left_join(people_grade_tbl, house_floor_tbl, by = 'cod10')
census_tbl <- left_join(census_tbl, house_water_tbl, by = 'cod10')
census_tbl <- left_join(census_tbl, house_ceiling_tbl, by = 'cod10')
census_tbl <- left_join(census_tbl, people_age_tbl, by = 'cod10')
census_tbl <- left_join(census_tbl, home_salary_tbl, by = 'cod10')
census_tbl <- left_join(census_tbl, house_date_tbl, by = 'cod10')
census_tbl <- left_join(census_tbl, home_persona_hogar_tbl, by = 'cod10')



census_tbl %>% 
	DataExplorer::plot_missing()

census_longer_tbl <- census_tbl %>% 
	pivot_longer(-cod10, names_to = "indicator", values_to = "value", values_drop_na = TRUE)
	
join_mapping_tbl <- left_join(join_mapping_tbl, people_tbl,  by = 'cod10')
write.csv(join_mapping_tbl, paste0(PATH_OUT, "/mapping_covid_19.csv"), row.names = FALSE)


write.csv(census_longer_tbl , paste0(PATH_OUT, "/census_data_longer_2010.csv"))
write.csv(census_tbl , paste0(PATH_OUT, "/census_data_2010.csv"))





census_tbl%>% 
	head()

census_longer_tbl%>% 
	glimpse()


join_mapping_tbl %>% 
	glimpse()

join_mapping_tbl %>% 
	DataExplorer::plot_missing()


join_mapping_tbl %>% 
	mutate(relacion_uci = (uci / cantidad)*100) %>% 
	dplyr::select(poblacion, cantidad, uci, relacion_uci) %>% 
	arrange(desc(cantidad))



join_mapping_tbl %>% 
	filter(cantidad > 0) %>% 
	mutate(relacion_uci = (uci / cantidad)*100) %>% 
	dplyr::select(poblacion, cantidad, uci, relacion_uci) %>% 
	DataExplorer::plot_correlation()


join_mapping_tbl %>% 
	filter(is.na(people)) %>%
	group_by(objectid) %>% 
	summarise(cantidad_total = sum(cantidad))








rm(distrito_tbl, home_salary_tbl, house_ceiling_tbl, house_date_tbl, house_floor_tbl, house_water_tbl, 
	 mapping_tbl, people_age_tbl, people_grade_tbl, records_tbl, rep_tbl, repetidos_list, 
	 test_tbl)
rm(menos_25, rep_lit, x25_45, x45_65, get_data_wider)

# ********************************************************
# analisys ----
# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1) {
	
	model_name <- h2o_leaderboard %>%
		as_tibble() %>%
		slice(n) %>%
		pull(model_id) 
	
	#print(model_name)
	
	return(model_name)
	
}

# join data 
training_cluster_tbl <- left_join(join_mapping_tbl, census_tbl, by = 'cod10')
training_cluster_tbl %>%  
	filter(is.na(doctorado)) %>% 
	select(objectid)

# selected variables
training_cluster_tbl <- training_cluster_tbl %>% 
	mutate(cluster = as.factor(cluster)) %>% 
	#mutate(
	#	x100_599 = sum(x100_124 + x125_174 + x175_249 + x250_399 + x400_599, na.rm = TRUE),
	#	x600_1499 = sum(x600_799 + x800_999 + x1000_1499, na.rm = TRUE),
	#	x1500_3000 = sum(x1500_1999 + x2000_2499 + x2500_2999, na.rm = TRUE),
	#	x3000_5000 = sum(x3000_3499 + x3500_3999 + x4000_4499 + x4500_4999, na.rm = TRUE)
	#) %>% 
	dplyr::select(
		cluster, ningun_grado, primaria, secundaria, universitaria, pavimentado_concreto, ladrillo, acueducto_publico_del_idaan,   
		people, menos_25, x25_45, x45_65, mas_65, 
		antes_de_2000, entre_2000_y_2005, entre_2006_y_2010,
		menos_100, x100_599, x600_1499, x1500_2999, x3000_4999, x5000_y_mas, 
		agua_lluvia, metal_zinc_aluminio_entre_otros, pozo_brocal_no_protegido, 
		x1:x12, 
		tipo
	) 


training_cluster_tbl %>% 
	filter(
		is.na(x1) == TRUE	
	) %>% 
	glimpse()

training_cluster_tbl %>% 
	glimpse()

training_cluster_tbl %>% 
	DataExplorer::plot_missing()



# ***************
# Prediction Tree

library(C50)
temp_tbl <- training_cluster_tbl %>% 
	filter(
		is.na(x1) == FALSE	
	)
#temp_tbl <- temp_tbl %>% 
#	filter(as.character(cluster) != '100')

cluster <- temp_tbl$cluster
temp_tbl$cluster <- NULL 
table(cluster)
model_tree <- C50::C5.0(temp_tbl, as.factor(cluster))
summary(model_tree)

#100.00%	tipo
#88.56%	secundaria
#74.15%	x2
#57.06%	agua_lluvia
#44.73%	ladrillo
#33.73%	x7
#31.80%	primaria
#24.52%	x5000_y_mas
#21.25%	x600_1499
#20.36%	entre_2006_y_2010
#16.34%	people
#13.97%	x3
#13.67%	x8
#12.18%	x12
#10.55%	x100_599
#9.51%	pavimentado_concreto
#9.51%	x25_45
#8.77%	ningun_grado
#8.62%	x1
#8.32%	metal_zinc_aluminio_entre_otros
#7.73%	x6
#7.58%	x5
#3.12%	mas_65
#2.67%	x11
#2.53%	acueducto_publico_del_idaan
#1.78%	x10
#1.49%	pozo_brocal_no_protegido
#0.74%	x3000_4999




library(rpart)
library(rpart.plot)
# Create a decision tree model
tree <- rpart(cluster~., data=training_cluster_tbl, cp=.02)
tree
# Visualize the decision tree with rpart.plot
rpart.plot(tree, type = 2, yesno=2, box.palette="RdBu", shadow.col="gray", nn=TRUE)


# ***************
# 


# skewed columns
skewed_feature_names <- training_cluster_tbl %>%
	select_if(is.numeric) %>%
	map_df(skewness) %>%
	gather(factor_key = T) %>%
	arrange(desc(value)) %>%
	filter(value >= 0.8) %>%
	pull(key) %>%
	as.character()

recipe_obj <- recipe(cluster ~ ., data = training_cluster_tbl) %>%
  step_dummy(all_nominal(), - cluster, one_hot = TRUE) %>%             # Create dummy variables
	step_zv(all_predictors()) %>%                # Zero variance remove feature
	step_YeoJohnson(skewed_feature_names) %>%    # Individual transformations for skewness and other issues 
	step_center(all_numeric()) %>%        # Normalization steps (center, scale, range, etc)
	step_scale(all_numeric())   %>%       # 
  prep(stringsAsFactors = FALSE)
	
	#step_num2factor(target) %>%                  # Discretize - act of making continuos varible discrete.
	#step_discretize(discretize_variables) %>% 
	#step_modeimpute(all_nominal()) %>%           # impute character features 
	#step_knnimpute(all_numeric()) %>%            # Imput (replace null with a value)
	#step_pca(all_numeric(), - outcome_variables, num = 15) %>% 
	#prep(stringsAsFactors = FALSE)

recipe_obj

training_tbl <- bake(recipe_obj, new_data = training_cluster_tbl)

training_tbl %>% 
	glimpse()

# Modeling ----
h2o.init() # nthreads = -1 
## optional: connect to a running H2O cluster
#h2o.init(ip="mycluster", port=55555)
#split_h2o <- h2o.splitFrame(as.h2o(training_tbl), ratios =c(0.75, 0.15) , #ratios =c(0.80, 0.10)
#														seed = 777) # split data in two data frames 

#train_h2o <- split_h2o[[1]] # using to develop the model 
#valid_h2o <- split_h2o[[2]] # use to tune hyperparameters via grid search  
#test_h2o  <- split_h2o[[3]] # Import R object to the H2O cloud

train_h2o <- as.h2o(training_tbl)


y <- "cluster"
predictors <- setdiff(names(train_h2o), y) # produce the difference between two vectors

auto_ml_model <- h2o.automl(
	x = predictors,
	y = y, 
	training_frame = train_h2o # using to develop the model 
	# validation_frame = valid_h2o, # using for tuning model 
	#leaderboard_frame = test_h2o, # complete separation of model and tuning 
	, max_runtime_secs = 600 # default 1h
	, nfolds = 10 # cross validation folds 
)

auto_ml_model@leaderboard # resumen by model (model_id) auc 0.7126641

# Please note that the models will still be validated using cross-validation only, 
# the validation frame will be used to provide purely informative validation metrics 
# on the trained models.

#auto_ml_model@leaderboard %>%
#    plot_h2o_performance(newdata = test_h2o, order_by = "auc", 
#                         size = 1, max_models = 5)

best_model <- auto_ml_model@leaderboard %>% extract_h2o_model_name_by_position(n = 1)  %>% # r2  0.8847284
	h2o.getModel()

#best_model %>% 
#		h2o.saveModel(path = "04_Modeling/h2o_models") # StackedEnsemble_BestOfFamily_0_AutoML_20180609_122026
#h2o.download_pojo(best_model, path = "04_Modeling/h2o_models")
#h2o.download_mojo(best_model, path = "04_Modeling/h2o_models")

# glm_model_test <- best_model

performance_h2o <- h2o.performance(best_model, newdata = train_h2o)# test_h2o)
performance_h2o

performance_h2o %>%
	h2o.confusionMatrix()
1 - (27 / 75)
1 - (24 / 75)
1 - (36/102)
1 - (33/102)
1 - (21/64)
1 - (76/652)

var_importance <- h2o.varimp(best_model) %>% 
	as.tibble() 
View(var_importance)






# ********************************************************
# join with map ----
census_tbl %>% 
	count(cod19) %>% 
	filter(n > 1) %>% 
	arrange(desc(n))

census_tbl %>% 
	filter(cod19 == '041303') %>% 
	select(cod19, objectid, cluster, people)

panama_map_geojson@data %>% 
	glimpse()

temp_tbl <- left_join(panama_map_geojson@data, census_tbl, by = 'cod19')
temp_tbl  %>% 
	glimpse()

panama_map_geojson@data <- left_join(panama_map_geojson@data, census_tbl, by = 'cod19')
panama_map_geojson@data %>%
	DataExplorer::plot_missing()

panama_map_geojson@data %>% 
	glimpse()

# missing values
panama_map_geojson@data %>%
	filter(is.na(tipo)) %>%
	dplyr::select(objectid, cod19, cod10, cantidad, corregimiento_minsa, corregimiento, corr_map)

panama_map_geojson@data %>% 
	glimpse()





