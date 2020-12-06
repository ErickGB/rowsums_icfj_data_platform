cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse)
library(tidyquant)
library(ggthemes) 
library(forecast)     #
library(stringr)      # string manipulation
library(janitor)      # basic manipulation 
library(DataExplorer) # general data explorer
library(kableExtra)   # tables in markdown document (html)
# devtools::install_github("kalimu/genderizeR")
# install.packages("kableExtra")
library(genderizeR)

#
source("./00_scripts/base_functions.R")
PATH_IN <- "00_data/in/salaries/parity/"
# ***********************************************
# Load data ----
#write.csv(data_tbl, "./00_data/in/salaries/paridad/parity_ajusted.csv", 
#					row.names = FALSE, na = "")


data_tbl <- read.csv(paste0(PATH_IN, "/parity_ajusted_v3.csv"), 
														dec = ".") 

data_tbl %>% 
	glimpse()


decil_tbl <- data_filtered_tbl %>% 
	summary_col_by_group(gender, decil_name, col = total_income) %>% 
	ungroup() 

# ***********************************************
# Hipótesis:
# 1. El hombre gana más que la mujer.
# 2. La diferencia es mayor en el grupo de salario más alto y más bajo.
# 3. Existén ciertas profesiones dónde la mujer es mejor pagada que el hombre.
# 4. Existen entidades dónde la diferencia es marcadamente mayor.
# 

# Preguntas
# ¿cantidad total y porcentaje por sexo? 
# ¿salrio medio  y porcentaje por sexo? 

# ¿cantidad/monto y porcentaje por entidad?
# ¿cuáles son las entidades con mayor y menor paridad?
# ¿cuáles entidades tiene mayor cantidad de mujeres pero mejor ingreso?
# ¿cuánto gana la mujer por cada dólar que gana el hombre?
# ¿quiénes son las 20 mujeres mejor pagadas? comparar con los 20 hombres mejor pagados.
# en el decil más alto, como se distribuye $

# validar las ocupaciones, las de mando y juristicción vs las admin.
# comparar antiguedad y salario por sexo

# ***********************************************
# Business understand


# ***********************************************
# Plots ---- 
# b6c2ea47281db9963d2122187572e11d

# ¿cantidad total y porcentaje por sexo? 
nrow(data_tbl) # 156,431
table(data_tbl$gender, useNA = "always")
table(data_tbl$gender, useNA = "always") / nrow(data_tbl)

data_tbl %>%  
	filter(total_income == 0)

# todos.. nota: corregir el de 15k
data_tbl %>% 
	filter(total_income > 0) %>% 
	mutate(gender = ifelse(is.na(gender), "No identificado", gender)) %>% 
	summary_col_by_group(gender, col = total_income) %>% 
	select(gender, Count, Min, Max, Mean, Median)

review_by_entity_tbl <- data_tbl %>% 
	group_by(gender, entity) %>% 
	summarise(
		mean_income = mean(total_income),
		median_income = median(total_income),
		median_years_in_work = median(years_in_work),
		total_new = sum(is_new)
	) %>% 
	arrange(entity)
View(review_by_entity_tbl)


# deciles
quantile(data_tbl$total_income, prob = seq(0, 1, length = 11), type = 5)
#quantile(data_tbl$total_income, prob = seq(0, 1, 0.01), type = 5)

# deciles
decil_tbl <- data_tbl %>% 
	filter(gender != 'NA') %>% 
	summary_col_by_group(gender, decil_name, col = total_income) %>% 
	ungroup() 

decil_tbl %>% 
	select(gender, decil_name, Median) %>% 
	filter(is.na(gender) == FALSE) %>% 
	pivot_wider(
		names_from = "gender",
		values_from = "Median"
	) %>% 
	mutate(
		DIFERENCE = MUJER - HOMBRE,
		PERCENTAGE = (DIFERENCE/MUJER) * 100
	) %>% 
	janitor::clean_names()




# ******************
# by entity

parity_entity_tbl <- data_tbl %>% 
	filter(decil_name == "9.1  mayor a B./2,264") %>% 
	summary_col_by_group(gender, entity, col = salary) %>% 
	arrange(entity, gender) %>% 
	select(gender, entity, Mean) %>% 
	pivot_wider(names_from = gender, values_from = Mean) %>% 
	janitor::clean_names() %>% 
	mutate(
		diference =  mujer - hombre,
		percent = (( mujer -  hombre)/(hombre+mujer)) * 100,
		equivalent = mujer / hombre
	) %>% 
	arrange(diference)

View(parity_entity_tbl)


parity_entity_count_tbl <- data_tbl %>% 
	#filter(decil_name == "9.1  mayor a B./2,264") %>% 
	summary_col_by_group(gender, entity, col = salary) %>% 
	arrange(entity, gender) %>% 
	select(gender, entity, Count) %>% 
	pivot_wider(names_from = gender, values_from = Count) %>% 
	janitor::clean_names() %>% 
	mutate(
		diference = hombre - mujer,
		percent = (hombre - mujer)/(hombre+mujer),
		equivalent = mujer / hombre
	)
View(parity_entity_count_tbl)


# ******************
# by status

# decil más bajo
status_tbl <- data_tbl %>% 
	summary_col_by_group(gender, status, col = total_income) %>% 
	ungroup()

status_cantidad_tbl <- status_tbl %>% 
	filter(gender != "NA") %>% 
	select(gender, status, Count) %>% 
	pivot_wider(
		names_from = "gender",
		values_from = "Count"
	) %>% 
	janitor::clean_names() %>% 
	rename(cant_hombre = hombre, cant_mujer = mujer)

status_tbl %>% 
	filter(gender != "NA") %>% 
	select(gender, status, Median) %>% 
	pivot_wider(
		names_from = "gender",
		values_from = "Median"
	) %>% 
	janitor::clean_names() %>% 
	left_join(status_cantidad_tbl, by = 'status')


# ******************
# Top 10 by sex
data_tbl %>% 
	filter(gender == 'MUJER') %>% 
	select(complete_name, last_name, total_income, entity, position, status, start_date, years_in_work) %>% 
	arrange(desc(total_income)) %>% 
	head(10)

data_tbl %>% 
	filter(gender == 'HOMBRE') %>% 
	select(complete_name, last_name, total_income, entity, position, status, start_date, years_in_work) %>% 
	arrange(desc(total_income)) %>% 
	head(10)

# ******************
# Top decil superior vs inferior. Años vs salario

# por cantidad 
data_tbl %>% 
	filter(is.na(gender) == FALSE) %>% 
	filter(total_income > 2264) %>% 
	count(entity) %>% 
	arrange(desc(n))


# por cantidad 
data_tbl %>% 
	filter(is.na(gender) == FALSE) %>% 
	filter(total_income > 2264) %>% 
	summary_col_by_group(gender, entity, position, col = total_income) %>% 
	ggplot(aes(x = Count, y = Median, colour = gender, fill = gender)) + 
	geom_point(shape= 21, size = 2.5)



# por antiguedad
data_tbl %>% 
	filter(gender != "NA") %>% 
	filter(total_income > 2264) %>% 
	group_by(decil_name, gender, entity, position) %>% 
	summarise(income = median(total_income), years_in_work = median(years_in_work),  n_count = n()) %>% 
	filter(years_in_work < 750) %>% 
	ggplot(aes(x = years_in_work, y = income, colour = gender, fill = gender, size = n_count)) + 
	geom_point(shape= 21, alpha = 0.5) + 
	facet_grid(decil_name ~ gender)



# por antiguedad
data_tbl %>% 
	filter(is.na(gender) == FALSE) %>% 
	filter(total_income < 800) %>% 
	group_by(gender, entity, position) %>% 
	summarise(income = median(total_income), dwork = median(years_in_work),  n_count = n()) %>% 
	ungroup() %>% 
	ggplot(aes(x = dwork, y = income, colour = gender, fill = gender, size = n_count)) + 
	geom_point(shape= 21, alpha = 0.5) + 
	facet_grid(. ~ gender)





data_filtered_tbl %>% 
	filter(entity == 'Fiscalía General Electoral') %>% 
	summary_col_by_group(gender, position, col = total_income) %>%
	ungroup() %>% 
	select(gender, position, Median) %>% 
	pivot_wider(
		names_from = gender,
		values_from = Median
	) %>%  #left_join(count_tbl, by = 'entity') %>% 
	mutate(MUJER = ifelse(is.na(MUJER), 0, MUJER)) %>% 
	mutate(HOMBRE = ifelse(is.na(HOMBRE), 0, HOMBRE)) %>% 
	janitor::clean_names() %>% 
	arrange(desc(hombre))


g <-  data_tbl %>% 
	#filter(decil_name == "9.1  mayor a B./2,264") %>% 
	summary_col_by_group(code, entity, gender, position, col = total_income) %>% 
	summary_col_by_group(code, entity, gender, col = Median) %>% 
	arrange(code, entity, gender) %>% 
	select(code, entity, gender, Median) %>% 
	pivot_wider(names_from = gender, values_from = Median) %>% 
	janitor::clean_names() %>% 
	mutate(
		salary_diference = mujer - hombre,
		salary_percent = (mujer - hombre)/(hombre+mujer), 
		equivalent = mujer / hombre
	) %>% 
	rename(
		salary_h = hombre,
		salary_m = mujer, 
		salary_no_identificado = na
	)

View(g)




if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
							 'ggplot2',  'ggalt',
							 'forcats', 'R.utils', 'png', 
							 'grid', 'ggpubr', 'scales',
							 'bbplot')





# plot 
income_gender_tbl <- data_position_tbl %>% 
	summary_col_by_group(gender, col = Mean) %>% 
	mutate(Leyenda = gender) %>% 
	filter(gender != "NA") 

background_colors_1 <- c(
	"#ffeda0", "#fed976", "#feb24c", 
	"#fd8d3c", "#fc4e2a", "#fc4e2a", 
	"#fcbba1", "#fc9272", "#fb6a4a", 
	#San Sal   Panama,   San Pedro
	"#cb181d", "#67000d", "#252525")

letter_color <- c("#000000", "#000000", "#000000", "#000000", 
									"#000000", "#000000", "#000000", "#ffffff",
									"#ffffff", "#ffffff", "#ffffff", "#ffffff")

g <- ggplot(income_gender_tbl, aes(x = gender, y = Mean, fill = gender)) +
	geom_bar(stat="identity", position="dodge", fill=background_colors_1[1:nrow(income_gender_tbl)]) +
	scale_x_discrete(labels = income_gender_tbl$gender) +
	#geom_bar(position="dodge", stat="identity") + 
	geom_hline(yintercept = 0, size = 1, colour="#333333") +
	coord_flip() +
	scale_colour_manual(values = background_colors_1[1:nrow(income_gender_tbl)]) +
	bbc_style() +
	rs_style() +
	facet_grid(gender ~ ., scales="free") + 
	labs(
		x = "", y = "% de variación", 
		title="Porcentaje de variación en la congestión de tráfico\nen las principales ciudades de América Latina",
		#subtitle = "Con un 98% en la disminución del tráfico, las ciudades Bridgetown(Barbados) y \nSan Pedro Sula (Honduras) lideran el ranking por el Caribe y Centroamérica", 
		caption = "Fuente:Inter-American Development Bank and IDB Invest. IDB And IDB Invest Coronavirus Impact Dashboard. 2020.\nCreado Por: Erick Gordón. www.rowsums.com")


parity_entity_salary_tbl <- data_filtered_tbl %>% 
	filter(entity == 'Presidencia de la República') %>% 
	summary_col_by_group(code, entity, gender, position, col = total_income) %>% 
	select(code, entity, gender, position, Median) %>% 
	pivot_wider(names_from = gender, values_from = Median) %>% 	
	janitor::clean_names() %>% 
	mutate(
		salary_diference = mujer - hombre,
		salary_percent = (mujer - hombre)/(hombre+mujer), 
		equivalent = mujer / hombre
	)

View(parity_entity_salary_tbl)

parity_entity_salary_tbl %>% 
	summarise(
		mean_hombre = median(hombre, na.rm = TRUE),
		mean_mujer = median(mujer, na.rm = TRUE),
		mean_diff = median(salary_diference, na.rm = TRUE)
	)



View(data_position_tbl %>% head())

write.csv(data_position_tbl, "./00_data/in/salaries/parity/gov_by_position.csv", row.names = FALSE, 
					 na = "")

#*******************************
parity_entity_count_tbl <- data_tbl %>% 
	summary_col_by_group(code, entity, gender, col = total_income) %>% 
	arrange(entity, gender) %>% 
	select(code, gender, entity, Count) %>% 
	pivot_wider(names_from = gender, values_from = Count) %>% 
	janitor::clean_names() %>% 
	mutate(
		diference = mujer - hombre,
		percent = (mujer - hombre)/(hombre+mujer)
	) %>% 
	rename(
		no_identificado = na
	)

parity_entity_salary_tbl <- data_tbl %>% 
	#filter(decil_name == "9.1  mayor a B./2,264") %>% 
	summary_col_by_group(code, entity, gender, position, col = total_income) %>% 
	summary_col_by_group(code, entity, gender, col = Mean) %>% 
	arrange(code, entity, gender) %>% 
	select(code, entity, gender, Median) %>% 
	pivot_wider(names_from = gender, values_from = Median) %>% 
	janitor::clean_names() %>% 
	mutate(
		salary_diference = mujer - hombre,
		salary_percent = (mujer - hombre)/(hombre+mujer), 
		equivalent = mujer / hombre
	) %>% 
	rename(
		salary_h = hombre,
		salary_m = mujer, 
		salary_no_identificado = na
	)

# join 
join_entity_tbl <- parity_entity_count_tbl %>% 
	#select(code, entity, diference) %>% 
	left_join(parity_entity_salary_tbl %>% select(-entity), by = 'code') %>% 
	rename(cantidad = diference, salarios = salary_diference)


join_entity_tbl %>% 
	filter(entity == 'Fiscalía General Electoral') %>% 
	glimpse()

View(join_entity_tbl)



background_colors_1 <- c(
	"#ffeda0", "#fed976", "#feb24c", 
	"#fd8d3c", "#fc4e2a", "#fc4e2a", 
	"#fcbba1", "#fc9272", "#fb6a4a", 
	#San Sal   Panama,   San Pedro
	"#cb181d", "#67000d", "#252525")

final_tbl <- join_entity_tbl %>% 
	filter(is.na(hombre) == FALSE & is.na(mujer) == FALSE) %>% 
	select(code, entity, cantidad, salarios) %>% 
	pivot_longer(cantidad:salarios, names_to = "type", values_to = "value") %>% 
	mutate(flag = ifelse(value < 0, "- mujeres", "+ mujeres") )
	
	final_tbl %>% 
	ggplot(aes(x = value, y = entity, fill=flag)) +
	geom_bar(stat = "identity") + 
	facet_grid(. ~ type,  scale = "free") + 
	scale_colour_manual(values = background_colors_1[1:nrow(join_entity_tbl)]) +
	bbc_style() +
	rs_style() +
	#geom_line(size = 1.2) +
	#geom_hline(xintercept = 0, size = 1, colour="#333333") +
	labs(
		x = "", y = "% de variación", 
		title="Desigualdad en la cantidad de funcionarios e ingreso según el sexo",
		#subtitle = "Con un 98% en la disminución del tráfico, las ciudades Bridgetown(Barbados) y \nSan Pedro Sula (Honduras) lideran el ranking por el Caribe y Centroamérica", 
		caption = "Elaboración propia a partir de datos de la Contraloría General de la Rep.\nCreado Por: Erick Gordón. www.rowsums.com")


final_tbl %>% 
	kbl(caption = "Diferencias por entidad por cargo y sexo") %>%
	kable_classic(full_width = F, html_font = "Cambria") %>% 
	#add_header_above(c(" " = 1, "Cantidad" = 5, "Salario" = 6)) %>% 
	footnote(general = "Elaboración propia a partir de datos de la Contraloría General de la Rep.")

# ********************************
salary_tbl  <- data_tbl %>% 
	#filter(entity == 'Presidencia de la República') %>% 
	summary_col_by_group(code, entity, gender, position, col = total_income) %>% 
	select(entity, gender, position, Median) %>% 
	pivot_wider(names_from = gender, values_from = Median) %>% 
	janitor::clean_names() %>% 
	filter(is.na(hombre) == FALSE & is.na(mujer) == FALSE) %>% 
	group_by(entity) %>% 
	summarise(hombre = mean(hombre), mujer = mean(mujer)) %>% 
	mutate(diference = mujer - hombre)

salary_tbl %>% 
	summarise(hombre = mean(hombre), mujer = mean(mujer))

1536 - 1516
1511 - 1494


count_tbl  <- data_tbl %>% 
	#filter(entity == 'Presidencia de la República') %>% 
	summary_col_by_group(code, entity, gender, position, col = total_income) %>% 
	select(entity, gender, position, Count) %>% 
	pivot_wider(names_from = gender, values_from = Count) %>% 
	janitor::clean_names() %>% 
	mutate(hombre = ifelse(is.na(hombre), 0, hombre), mujer = ifelse(is.na(mujer), 0, mujer)) %>% 
	group_by(entity) %>% 
	summarise(cantidad_h = sum(hombre), cantidad_m = sum(mujer)) %>% 
	ungroup() %>% 
	mutate(cantidad_diference = cantidad_m - cantidad_h)
View(count_tbl)

# ***********************
# cantidad 
plot_data_tbl <- count_tbl %>% 
	rename(valor = cantidad_diference) %>% 
	mutate(flag = ifelse(valor < 0, "+Hombres que mujeres", "+Mujeres que hombres"))
	
# reorder(entity, (valor))
plot_data_tbl %>% 
	select(entity, valor, flag) %>% 
	ggplot(aes(x = entity, y = valor, fill= flag)) +
	geom_bar(stat = "identity", position="dodge") + 
	coord_flip() +
	#scale_x_discrete(labels = salary_tbl$entity) +
	geom_hline(yintercept = 0, size = 1, colour="#333333") +
	#facet_grid(. ~ type,  scale = "free") + 
	scale_fill_manual(values = c("#fc8d59", "#91bfdb")) +
	bbc_style() +
	rs_style() +
	labs(
		x = "", y = "% de variación", 
		title="Brecha de género en la cantidad de funcionarios\npor entidad, agosto 2020.",
		#subtitle = "Se concideran aquellos cargos que cuentan con representación de hombres y mujeres", 
		caption = "Elaboración propia a partir de datos de la Contraloría General de la Rep.\nCreado Por: Erick Gordón. www.rowsums.com") + 
	theme(legend.position = "top") 

# ***********************
# salarios
plot_data_tbl <- salary_tbl %>% 
	rename(valor = diference) %>% 
	mutate(flag = ifelse(valor < 0, "Hombre con mayor ingreso", "Mujeres con mayor ingreso") )

salary_tbl %>% 
	summarise(
		mean_h = mean(hombre), mean_m = mean(mujer),
		median_h = median(hombre), median_m = median(mujer)
		)
mean(data_tbl$probability, na.rm = TRUE)


plot_data_tbl %>% 
	select(entity, valor, flag) %>% 
	ggplot(aes(x = entity, y = valor, fill= flag)) +
	geom_bar(stat = "identity", position="dodge") + 
	coord_flip() +
	#scale_x_discrete(labels = salary_tbl$entity) +
	geom_hline(yintercept = 0, size = 1, colour="#333333") +
	#facet_grid(. ~ type,  scale = "free") + 
	scale_fill_manual(values = c("#fc8d59", "#91bfdb")) +
	bbc_style() +
	rs_style() +
	labs(
		x = "", y = "", 
		title="Brecha de género en el ingreso salarial\npor entidad, agosto 2020.",
		subtitle = "Se concideran aquellos cargos que cuentan con representación\nde hombres y mujeres en la posición.", 
		caption = "Elaboración propia a partir de datos de la Contraloría General de la Rep.\nCreado Por: Erick Gordón. www.rowsums.com") + 
	theme(legend.position = "top") 

# ***********************
# salarios por entidad, todos los cargos 
salary_all_tbl  <- data_tbl %>% 
	summary_col_by_group(code, entity, gender, position, col = total_income) %>% 
	select(entity, gender, position, Mean) %>% 
	pivot_wider(names_from = gender, values_from = Mean) %>% 
	janitor::clean_names() %>% 
	#filter(is.na(hombre) == FALSE & is.na(mujer) == FALSE)  salary_all_tbl %>% 
	group_by(entity) %>% 
	summarise(hombre = mean(hombre, na.rm = TRUE), mujer = mean(mujer, na.rm = TRUE)) %>% 
	mutate(diference = mujer - hombre)

plot_data_tbl <- salary_all_tbl %>% 
	rename(valor = diference) %>% 
	mutate(flag = ifelse(valor < 0, "Hombre mejor pagado", "Mujeres mejor pagada") )

salary_all_tbl %>% 
	summarise(
		mean_h = mean(hombre), mean_m = mean(mujer),
		median_h = median(hombre), median_m = median(mujer)
	)

plot_data_tbl %>% 
	select(entity, valor, flag) %>% 
	ggplot(aes(x = entity, y = valor, fill= flag)) +
	geom_bar(stat = "identity", position="dodge") + 
	coord_flip() +
	#scale_x_discrete(labels = salary_tbl$entity) +
	geom_hline(yintercept = 0, size = 1, colour="#333333") +
	#facet_grid(. ~ type,  scale = "free") + 
	scale_fill_manual(values = c("#fc8d59", "#91bfdb")) +
	bbc_style() +
	rs_style() +
	labs(
		x = "", y = "", 
		title="Brecha de género en el ingreso salarial\npor entidad, agosto 2020.",
		subtitle = "Se concideran aquellos cargos que cuentan con representación\nde hombres y mujeres en la posición.", 
		caption = "Elaboración propia a partir de datos de la Contraloría General de la Rep.\nCreado Por: Erick Gordón. www.rowsums.com") + 
	theme(legend.position = "top") 
View(plot_data_tbl)

# ************************************
# Techo de la mujer 

position_tbl <- data_tbl %>% 
	summary_col_by_group(code, entity, gender, position, col = total_income) 

position_tbl <- position_tbl %>% 
	pivot_wider(
		names_from = "gender",
		values_from = "Mean"
	) %>% 
	mutate(
		diference = MUJER - HOMBRE
	) %>% 
	filter(is.na(HOMBRE) == FALSE & is.na(MUJER) == FALSE) %>% 
	select(position, HOMBRE, MUJER, diference)
View(position_tbl)
	
summary(position_tbl$diference)

position_tbl %>% 
	filter(gender != "NA") %>% 
	ggplot(aes(x = gender, y = Mean, fill = gender, size = Count)) + 
	geom_violin() + 
	coord_flip()

data_tbl %>% 
	filter(gender != "NA") %>% 
	filter(total_income > 3000) %>% 
	ggplot(aes(colour = gender, x = total_income, fill = gender)) + 
	#geom_density(alpha=0.6) + 
	geom_histogram(position = "identity", alpha=0.7) + 
	scale_fill_manual(values = c("#fc8d59", "#91bfdb")) +
	scale_color_manual(values = c("#fc8d59", "#91bfdb")) +
	bbc_style() +
	rs_style() +
	labs(
		x = "Ingreso", y = "Cantidad", 
		title="Histograma de ingreso mayores a 3 mil balboas por género",
		subtitle = "13% mayor cantidad de hombres y mejor pagados que la mujer en un 5%", 
		caption = "Elaboración propia a partir de datos de la Contraloría General de la Rep.\nCreado Por: Erick Gordón. www.rowsums.com") + 
	theme(legend.position = "top")

decil_tbl <- data_tbl %>% 
	filter(gender != "NA") %>% 
	summary_col_by_group(code, entity, decil_name, gender, col = total_income) %>% 
	summary_col_by_group(decil_name, gender, col = Mean) %>% 
	select(gender, decil_name, Mean) %>% 
	pivot_wider(
		names_from = "gender",
		values_from = "Mean"
	) %>% 
	mutate(
		DIFERENCE = MUJER - HOMBRE,
		PERCENTAGE = (DIFERENCE/MUJER) * 100
	)
View(decil_tbl)

data_tbl %>% 
	filter(gender != "NA") %>% 
	summary_col_by_group(decil_name, gender, col = total_income) %>% 
	select(decil_name, gender, Count) %>% 
	pivot_wider(
		names_from = "gender",
		values_from = "Count"
		) 
	
(5654 + 4660 + 6138) / (9104 + 10065 + 9046)

58 / 100
29 / 50

7543/7681
98/100
49/50

data_tbl %>% 
	filter(gender != "NA") %>% 
	filter(total_income > 3000) %>% 
	group_by(gender) %>% 
	summarise(
		n = n(),
		salary = mean(total_income),
		msalaary = median(total_income)
	)

(4594 - 4356 )*12
(4594 - 4356)/4594 * 100

(3659 - 3153)/3659 * 100

4464 - 4294

3659 - 3153 

# ************************************
# boxplot
data_tbl %>% 
	filter(gender != "NA") %>% 
	ggplot(aes(x = factor(gender), y = total_income, fill=factor(gender))) + 
	geom_point() +
	coord_flip() + 
	scale_fill_manual(values = c("#fc8d59", "#91bfdb")) +
	scale_color_manual(values = c("#fc8d59", "#91bfdb")) + 
	bbc_style() +
	rs_style() +
	labs(
		x = "Ingreso", y = "", 
		title="Ingreso por sexo",
		subtitle = "La mujer presenta un techo en el ingreso que se estanca en los 10 mil balboas.", 
		caption = "Elaboración propia a partir de datos de la Contraloría General de la Rep.\nCreado Por: Erick Gordón. www.rowsums.com") + 
	guides(fill=FALSE)

	


# ************************************
plot_data_tbl %>% 
	select(entity, diference) %>% 
	rename(valor = diference) %>% 
	ggplot(aes(x = entity, y = valor, fill=flag)) +
	geom_bar(stat = "identity", position="dodge") + 
	scale_x_discrete(labels = salary_tbl$entity) +
	geom_hline(yintercept = 0, size = 1, colour="#333333") +
	coord_flip() +
	#facet_grid(. ~ type,  scale = "free") + 
	scale_colour_manual(values = background_colors_1[1:nrow(join_entity_tbl)]) +
	bbc_style() +
	rs_style() +
	labs(
		x = "", y = "% de variación", 
		title="Brecha de género en el ingreso salarial por entidad",
		subtitle = "Se concideran aquellos cargos que cuentan con representación de hombres y mujeres", 
		caption = "Elaboración propia a partir de datos de la Contraloría General de la Rep.\nCreado Por: Erick Gordón. www.rowsums.com")









salary_tbl %>% 
	left_join(count_tbl, by = 'entity') %>% 
	select(entity, diference, cantidad_diference) %>% 
	pivot_longer(diference:cantidad_diference, names_to = "type", values_to = "value") %>% 
	mutate(flag = ifelse(value < 0, "- mujeres", "+ mujeres") ) %>% 
	ggplot(aes(x = value, y = entity, fill=flag)) +
	geom_bar(stat = "identity") + 
	facet_grid(. ~ type,  scale = "free") + 
	scale_colour_manual(values = background_colors_1[1:nrow(join_entity_tbl)]) +
	bbc_style() +
	rs_style() +
	labs(
		x = "", y = "% de variación", 
		title="Desigualdad en la cantidad de funcionarios e ingreso según el sexo",
		#subtitle = "Con un 98% en la disminución del tráfico, las ciudades Bridgetown(Barbados) y \nSan Pedro Sula (Honduras) lideran el ranking por el Caribe y Centroamérica", 
		caption = "Elaboración propia a partir de datos de la Contraloría General de la Rep.\nCreado Por: Erick Gordón. www.rowsums.com")


salary_tbl %>% 
	left_join(count_tbl, by = 'entity') %>% 
	mutate(flag = ifelse(diference < 0, "+Hombres que mujeres", "+Mujeres que hombres") ) %>% 
	kbl(caption = "Diferencias por entidad por cargo y sexo") %>%
	kable_classic(full_width = F, html_font = "Cambria") %>% 
	#add_header_above(c(" " = 1, "Cantidad" = 5, "Salario" = 6)) %>% 
	footnote(general = "Elaboración propia a partir de datos de la Contraloría General de la Rep.")



data_tbl %>% 
	summary_col_by_group(code, entity, gender, position, with_ca, col = total_income) %>% 
	group_by(entity, gender, with_ca) %>% 
	summarise(n = n(), total = sum(Count), income = mean(Median)) %>% 
	select(entity, with_ca, gender, income) %>% 
	pivot_wider(
		names_from = "gender",
		values_from = "income"
	)



