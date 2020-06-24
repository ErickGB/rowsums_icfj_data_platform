# ***********
# planilla 
planilla_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/1_central_gov_salaries_march.csv")
css_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/2_central_css_gov_salaries_march.csv")
contrata_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/3_dir_contrataciones_publicas_gov_salaries_march_2020_processed_2020-05-17.csv")
consumidor_tbl <- readr::read_csv("./00_data/out/salaries/pending_process/2020/march/5_aut_consumidor_gov_salaries_march_2020_processed_2020-05-17.csv")

planilla_tbl <- planilla_tbl %>% 
	mutate(start_date = as.Date(start_date, tryFormats = c("%d/%m/%Y"))) %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)

css_tbl<- css_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)

contrata_tbl<- css_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)

consumidor_tbl<- css_tbl %>% 
	dplyr::select(person_id, first_name, last_name, entity, position, start_date, salary, expenses, total_income)


# union all 
planilla_tbl <- planilla_tbl %>% 
	bind_rows(css_tbl) %>% 
	bind_rows(contrata_tbl) %>% 
	bind_rows(consumidor_tbl) 



rm(css_tbl, contrata_tbl, consumidor_tbl)


ff_arrea_tbl <- readr::read_csv2("./00_data/out/salaries/prd/ff_area.csv")
ff_arrea_tbl %>% 
	glimpse()



ff_arrea_tbl <- left_join(ff_arrea_tbl, planilla_tbl, by = 'person_id')
ff_arrea_tbl %>% 
	DataExplorer::plot_missing()


prd_gov <- ff_arrea_tbl %>% 
	filter(is.na(first_name) == FALSE) %>% 
	mutate(is_new = ifelse(start_date >= as.Date("01-07-2019", "%d-%m-%Y"), "yes", "no"))

prd_gov %>% 
	glimpse()

table(prd_gov$is_new)
table(prd_gov$nomina)
table(prd_gov$rol)


table(prd_gov$is_new) / nrow(prd_gov)
table(prd_gov$entity, prd_gov$is_new)
table(prd_gov$rol, prd_gov$nomina)
table(prd_gov$rol, prd_gov$is_new)



