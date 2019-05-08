# load libraries ----
library(readr)
library(tidyverse)
# constants ----
PATH_IN <- "./00_Data/in/"
PATH_OUT <- "./00_Data/out/"

# **************************************************************************
# load data detail --- 
planitlla_20190406 <- read_csv("00_data/in/salaries/planitlla_20190406.csv")
planitlla_20190406[2:5,]

table(is.na(planitlla_20190406$salario)) / nrow(planitlla_20190406)

planitlla_20190406 <- planitlla_20190406 %>% 
	mutate(
		 nombre = gsub("\r\n", "", nombre),
		 nombre = gsub("\"", "", nombre),
		 primer_nombre = sapply(nombre, function(x) substr(x, 1, gregexpr(pattern =" ", x)[[1]][1] - 1)),
		 nombre = stringr::str_trim(nombre, side = "right"),
		 apellido = gsub("\r\n", "", apellido),
		 apellido = gsub(" ", "", apellido),
		 cedula = gsub("\r\n", "", cedula), 
		 cedula = gsub(" ", "", cedula), 
		 salario = gsub("\r\n", "", salario), 
		 salario = gsub(" ", "", salario), 
		 salario = gsub(",", "", salario), 
		 gasto = gsub("\r\n", "", gasto), 
		 gasto = gsub(" ", "", gasto), 
		 gasto = gsub(",", "", gasto), 
		
		 cargo = gsub("\r\n", "", cargo), 
		 cargo = stringr::str_trim(cargo, side = "right"),
		 estado = gsub("\r\n", "", estado), 
		 estado = gsub(" ", "", estado), 
		 fecha_inicio = gsub("\r\n", "", fecha_inicio), 
		 fecha_inicio = gsub(" ", "", fecha_inicio), 
		 fecha_inicio = substr(fecha_inicio, 1, 10),
				
		 salario = as.numeric(salario),
		 gasto = as.numeric(gasto),
		 total = salario + gasto
		)
# 01/04/2019 3:19:18 PM
# 1234567890
head(planitlla_20190406)
View(planitlla_20190406)

table(is.na(planitlla_20190406$salario)) / nrow(planitlla_20190406)


#write.csv(codes, paste0(PATH_OUT, "salaries/out_codes.csv"), row.names = FALSE)
codes <- readr::read_csv(paste0(PATH_OUT, "salaries/out_codes.csv"))

planitlla_20190406 <- inner_join(planitlla_20190406,  codes, by = "code")
planitlla_20190406$last_update <- "01/04/2019"

table(is.na(planitlla_20190406$salario)) / nrow(planitlla_20190406)

planitlla_20190406 %>% 
	glimpse()

double_record_tbl <- planitlla_20190406 %>% 
	filter(cargo != 'REPRESENTANTE DE CORREGIMIENTO') %>% 
	group_by(cedula) %>% 
	summarize(n = n(), total =sum(salario + gasto)) %>% 
	filter(n > 1) %>% 
	arrange(desc(total))

nrow(double_record_tbl) # 127

planitlla_20190406 %>% 
	filter(cedula == '8-0105-00709') %>% 
	dplyr::select(nombre, apellido, cargo, name, salario)



write.csv(planitlla_20190406, #fileEncoding = "UTF-8",
	paste0(PATH_OUT, "out_planilla_01042019v3.csv"), row.names = FALSE)

rm(codes)

# **************************************************************************
# load summarized data ---- 

employees_2018 <- read_csv("00_data/in/salaries/cantidad_planilla_ene_dic_2018.csv")
salaries_2018 <- read_csv("00_data/in/salaries/salarios_planilla_ene_dic_2018.csv")
salaries_2019 <- read_csv("00_data/in/salaries/salarios_cantidad_planilla_ene_2019.csv")
months_tbl <- read_csv("00_data/in/salaries/months.csv")

employees_2018 <- employees_2018 %>% 
	mutate(Variable = "cantidad empleados") 

employees_2018 %>% 
	glimpse()

salaries_2018 <- 	salaries_2018 %>% 
	mutate(Variable = "salarios") 
	
salaries_2018 %>% 
	glimpse()

# data bind 
historical_data_tbl <- rbind(employees_2018, salaries_2018)
historical_data_tbl <- historical_data_tbl %>% 
	dplyr::select(Tipo, Entidad, Year, Variable, Enero, Febrero, Marzo, Abril, Mayo, 
		Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre)
historical_data_tbl <- historical_data_tbl %>% 
	gather(key, value, 5:ncol(historical_data_tbl))


employees_2019 <- salaries_2019 %>% 
	mutate(Variable = "cantidad empleados", key = "Enero", value = Funcionarios) %>% 
	dplyr::select(Tipo, Entidad, Year, Variable, key, value)

salaries_2019 <- salaries_2019 %>% 
	mutate(Variable = "salarios", key = "Enero", value = Suedos) %>% 
	dplyr::select(Tipo, Entidad, Year, Variable, key, value)

historical_2019_tbl <- rbind(employees_2019, salaries_2019)
historical_data_tbl <- rbind(historical_data_tbl, historical_2019_tbl)
historical_data_tbl <- historical_data_tbl %>% 
	rename(month = key)

table(is.na(historical_data_tbl$Year))


historical_data_tbl <- inner_join(historical_data_tbl, months_tbl, by = "month")
historical_data_tbl <- historical_data_tbl %>% 
	mutate(date = paste0(text, Year)) %>% 
	mutate(date = as.Date(date, "%d/%m/%Y"), text = NULL, lang = NULL) 

historical_data_tbl %>% 
	filter(is.na(historical_data_tbl$date))

historical_data_tbl %>% 
	glimpse()

write.csv(historical_data_tbl, #fileEncoding = "UTF-8",
	paste0(PATH_OUT, "out_historical_data.csv"), row.names = FALSE)

table(historical_data_tbl$date)
table(historical_data_tbl$Variable)

