library(tidyverse)
PATH_OUT <- "./00_data/out/salaries/"

last_data_tbl <- readr::read_csv(paste0(PATH_OUT, "out_centralgov_salaries_at_2019-05-18.csv"))
temp_tbl <- readr::read_csv(paste0(PATH_OUT,"out_centralgov_salaries.csv"))


temp_tbl <- temp_tbl %>% 
	rename(
		codigo = code, entidad = name
		) %>% 
	dplyr::select(codigo, entidad, url, nombre, apellido, cedula, cargo, 
		salario, gasto, estado, fecha_inicio, primer_nombre, total, 
		last_update, record_date, sex)

last_data_tbl <- rbind(last_data_tbl, temp_tbl)

for_review <- last_data_tbl %>% 
	filter(is.na(fecha_inicio) == TRUE)

last_data_tbl <- last_data_tbl %>% 
	filter(is.na(fecha_inicio) == FALSE)

last_data_tbl %>% 
	glimpse()


table(last_data_tbl$last_update)
write.csv(last_data_tbl, paste0(PATH_OUT, "out_04-05-2019_binded.csv"), row.names = FALSE, col.names = FALSE)


top_20_tbl <- last_data_tbl %>% 
	filter(last_update == '2019-05-16') %>%
	arrange(desc(total)) %>% 
	rename(
		code = codigo, complete_name = nombre, last_name = apellido, person_id = cedula, 
		position = cargo, salary = salario, expenses = gasto, total_income = total, status = estado, 
		start_date = fecha_inicio, first_name = primer_nombre, entity = entidad, update_date = last_update
		) %>% 
	dplyr::select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
		start_date, first_name, entity, update_date, sex) %>% 
	head(20)

last_20_tbl <- last_data_tbl %>% 
	filter(last_update == '2019-05-16') %>%
	arrange(total) %>% 
	rename(
		code = codigo, complete_name = nombre, last_name = apellido, person_id = cedula, 
		position = cargo, salary = salario, expenses = gasto, total_income = total, status = estado, 
		start_date = fecha_inicio, first_name = primer_nombre, entity = entidad, update_date = last_update
		) %>% 
	dplyr::select(code, complete_name, last_name, person_id, position, salary, expenses, total_income, status, 
		start_date, first_name, entity, update_date, sex) %>% 
	head(20)


write.csv(top_20_tbl, paste0(PATH_OUT, "top_20.csv"), row.names = FALSE) 
write.csv(last_20_tbl, paste0(PATH_OUT, "last_20.csv"), row.names = FALSE) 


last_data_tbl %>% 
	filter(is.na(salario) == TRUE) %>% dplyr::select(cedula, entidad, apellido, nombre)

nrow(last_data_tbl)
write.csv(last_data_tbl, paste0(PATH_OUT, "central_gov_salaries.csv"), row.names = FALSE) 

