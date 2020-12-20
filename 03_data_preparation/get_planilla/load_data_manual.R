

PATH_IN <- "./00_data/out/salaries/pending_process/2020/"

march_tbl <- read_csv(paste0(PATH_IN, "march/1_central_gov_salaries_march.csv")) %>% mutate(value_date = as.Date("2020-03-31", "%Y-%m-%d"))
abril_tbl <- read_csv(paste0(PATH_IN, "april/central_gov_salaries_april.csv")) %>% mutate(value_date = as.Date("2020-04-30", "%Y-%m-%d"))
mayo_tbl  <- read_csv(paste0(PATH_IN, "may/central_gov_salaries_may.csv")) %>% mutate(value_date = as.Date("2020-05-31", "%Y-%m-%d"))
junio_tbl <- read_csv(paste0(PATH_IN, "june/central_gov_salaries_june.csv")) %>% mutate(value_date = as.Date("2020-06-30", "%Y-%m-%d"))
julio_tbl <- read_csv(paste0(PATH_IN, "july/central_gov_salaries_july.csv")) %>% mutate(value_date = as.Date("2020-07-31", "%Y-%m-%d"))


salary_tbl <- march_tbl %>% 
	bind_rows(abril_tbl) %>% 
	bind_rows(mayo_tbl) %>% 
	bind_rows(junio_tbl) %>% 
	bind_rows(julio_tbl) 

table(salary_tbl$entity)


p <- salary_tbl %>% 
	group_by(code, entity, status, value_date) %>% 
	summarise(
		funcionarios = n(),
		salario = sum(salary), 
		gastos = sum(expenses),
		total_income = sum(total_income),
		mean = mean(total_income),
		median = median(total_income)
	) 

p %>% 
	count(code, entity)

View(p %>% 
	filter(code == '003')
)

write.csv(p, paste0(PATH_IN, "planilla_julio.csv"), row.names = FALSE, fileEncoding = "UTF-8")



