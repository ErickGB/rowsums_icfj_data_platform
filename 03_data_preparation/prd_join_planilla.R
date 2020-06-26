# ***********
# planilla ----
planilla_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/1_central_gov_salaries_march.csv")
css_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/2_central_css_gov_salaries_march.csv")
contrata_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/3_dir_contrataciones_publicas_gov_salaries_march_2020_processed_2020-05-17.csv")
consumidor_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/5_aut_consumidor_gov_salaries_march_2020_processed_2020-05-17.csv")

planilla_marzo_2019 <- readr::read_csv("./00_data/out/salaries/prd/planilla_marzo.csv")


css_tbl <- css_tbl %>% 
	count(code, complete_name, last_name, person_id, position, 
				salary, expenses, total_income, status, start_date, first_name, 
				entity, update_date, sex, url, record_date, key, 
				over_costs, departament) %>%
	mutate(n = NULL)


planilla_tbl <- planilla_tbl %>% 
	mutate(start_date = as.Date(start_date, tryFormats = c("%d/%m/%Y"))) %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)

css_tbl<- css_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)

contrata_tbl<- contrata_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)

consumidor_tbl<- consumidor_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)


# union all 
planilla_tbl <- planilla_tbl %>% 
	bind_rows(css_tbl) %>% 
	bind_rows(contrata_tbl) %>% 
	bind_rows(consumidor_tbl) 

nrow(planilla_tbl)

rm(css_tbl, contrata_tbl, consumidor_tbl)


# *****************************************************************************
# load data --- 
get_data_format <- function(data) {
	return_tbl <- data %>%
		mutate(nomina = as.character(nomina)) %>%
		dplyr::select(provincia, nomina, cargo, cedula, person_id, nombres, paterno, materno, rol)
	
	return(return_tbl)
} 

ff_arrea_tbl <- readr::read_csv2("./00_data/out/salaries/prd/ff_area.csv") %>%
	get_data_format() 
cen_tbl <- readr::read_csv2("./00_data/out/salaries/prd/CEN.csv") %>%
	mutate(provincia = "No aplica", nomina = "No aplica") %>%
	get_data_format() 
defensor_tbl <- readr::read_csv2("./00_data/out/salaries/prd/defensor.csv") %>%
	mutate(provincia = "No aplica", nomina = "No aplica") %>%
	get_data_format() 
directores1_tbl <- readr::read_csv2("./00_data/out/salaries/prd/directores_1.csv") %>%
	get_data_format() 
directores2_tbl <- readr::read_csv2("./00_data/out/salaries/prd/directores_2.csv") %>%
	get_data_format() 
directores3_tbl <- readr::read_csv2("./00_data/out/salaries/prd/directores_3.csv") %>%
	get_data_format() 
ff_nacional_tbl <- readr::read_csv2("./00_data/out/salaries/prd/ff_nacional.csv") %>%
	rename(cargo = cargo_ajustada, nombres = nombre_ajustada, paterno = paterno_ajustada,
				 materno = materno_ajustada, cedula = cedula_ajustada) %>%
	mutate(provincia = "No aplica", nomina = "No aplica") %>%
	get_data_format() 
juventud2_tbl <- readr::read_csv2("./00_data/out/salaries/prd/juventud_2_nac.csv") %>%
	mutate(provincia = "No aplica", nomina = "No aplica") %>%
	get_data_format() 
juventud_tbl <- readr::read_csv2("./00_data/out/salaries/prd/juventud_nac.csv")  %>%
	mutate(provincia = "No aplica", nomina = "No aplica") %>%
	get_data_format() 
directivaarea_tbl <- readr::read_csv2("./00_data/out/salaries/prd/directiva_area.csv") %>%
	rename(cargo = cargo_ajustado) %>%
	get_data_format() 




prd_people_by_rol_tbl <- cen_tbl %>%
	bind_rows(defensor_tbl) %>%
	bind_rows(directores1_tbl) %>%
	bind_rows(directores2_tbl) %>%
	bind_rows(directores3_tbl) %>%
	bind_rows(ff_nacional_tbl) %>%
	bind_rows(ff_arrea_tbl) %>%
	bind_rows(juventud2_tbl) %>%
	bind_rows(juventud_tbl) %>%
	bind_rows(directivaarea_tbl) 

prd_people <- prd_people_by_rol_tbl %>%
	count(person_id)

prd_people_by_rol_tbl %>%
	glimpse()

# 3118 - 3095 = 23 
nrow(prd_people)
nrow(prd_people_by_rol_tbl)

rm(juventud2_tbl, juventud_tbl, directivaarea_tbl, ff_arrea_tbl, ff_nacional_tbl,
	 directores1_tbl, directores2_tbl, directores3_tbl, defensor_tbl, cen_tbl)

# doble salario 
planilla_tbl %>%
	count(person_id) %>%
	filter(n > 1)

planilla_tbl %>%
	filter(person_id == '1-0022-01559')



# *******************
# final joins 
planilla_marzo_2019$is_new <- "no"

prd_people <- left_join(prd_people, planilla_tbl, by = 'person_id')
prd_all <- left_join(prd_people_by_rol_tbl, planilla_tbl, by = 'person_id')
prd_all <- left_join(prd_all, planilla_marzo_2019[, c("person_id", "is_new")], by = 'person_id')
#prd_all$is_new <- ifelse(is.na(prd_all$is_new), "yes", prd_all$is_new)

prd_all %>%
	glimpse()

prd_all <- prd_all %>%
	mutate(is_in = ifelse(is.na(salary), "fuera de planilla", "en planilla")) %>% # si no encuentra el salario, no lo ubico en la planilla
	mutate(is_new = ifelse(start_date >= as.Date("01-07-2019", "%d-%m-%Y"), "yes", is_new)) %>% # si la fecha de inicio es mayor al 1 jul 19
	mutate(is_new = ifelse(start_date < as.Date("01-07-2019", "%d-%m-%Y") & is.na(is_new) # si la fecha es menor, pero no estaba en planilla en juni 19
												 , "yes", is_new)) 
	





# ******************************************
# prd 
nrow(prd_all) # cargos
nrow(prd_people) # personas

# en planilla...
prd_all %>%
	count(is_in) %>%
	mutate(percent = round((n / sum(n)) * 100, 2))

# son nuevos
prd_all %>%
	filter(is_in == 'en planilla') %>%
	count(is_in, is_new) %>%
	mutate(percent = (n / sum(n)))

prd_all %>%
	filter(is_in == 'en planilla' & is_new == 'no') %>%
	select(cargo, cedula, nombres, salary, entity, start_date) %>%
	arrange(desc(start_date))



prd_all %>%
	filter(is_in == 'en planilla' & is_new == 'no') %>%
	group_by(entity) %>%
	summarise(n = n(), total_salary = sum(total_income), 
						mean_salary = mean(total_income), max_salary = max(total_income) ) %>%
	arrange(desc(n))



# por rol
prd_all %>%
	count(is_in, rol) %>%
	pivot_wider(
		names_from = is_in,
		values_from = n
	) %>%
	janitor::clean_names() %>%
	mutate(
		total = en_planilla + fuera_de_planilla,
		percent_in = round(en_planilla/total * 100, 2),
		percent_out = round(fuera_de_planilla/total * 100, 2),
	)

prd_all %>%
	filter(rol == 'CEN' & is_in == 'fuera de planilla') %>%
	dplyr::select(person_id, nombres, paterno, materno, rol) %>%
	View()


# comparacion de salarios 
summary(planilla_tbl$total_income)
summary(prd_all$total_income)


prd_all %>%
	group_by(entity, is_new) %>% 
	summarise(n = n(), total_salary = sum(total_income)) %>%
	mutate(percent = total_salary / n) %>%
	filter(is_new == 'yes' | is.na(is_new)) %>%
	arrange(desc(n))
	

prd_all %>%
	filter(is.na(is_new) & entity == 'Caja de Seguro Social') %>%
	select(person_id, nombres, paterno, materno, position, start_date, total_income, is_new, is_in)



prd_all %>%
	mutate(is_in = ifelse(is.na(salary), "no en planilla", "en planilla")) %>%
	mutate(is_new = ifelse(start_date >= as.Date("01-07-2019", "%d-%m-%Y"), "yes", "no")) %>%
	count(is_in, is_new, nomina, rol) %>%
	pivot_wider(
		names_from = rol,
		values_from = n
	) %>%
	janitor::clean_names()




prd_gov <- prd_people %>% 
	filter(is.na(first_name) == FALSE) %>% 
	mutate()

nrow(prd_gov) / nrow(prd_people)

prd_gov %>% 
	glimpse()

table(prd_gov$is_new)
table(prd_gov$nomina)
table(prd_gov$rol)


table(prd_gov$is_new) / nrow(prd_gov)
table(prd_gov$entity, prd_gov$is_new)
table(prd_gov$rol, prd_gov$nomina)
table(prd_gov$rol, prd_gov$is_new)




