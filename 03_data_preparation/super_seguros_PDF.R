library(tabulizer)
library(dplyr)

# Location of WARN notice pdf file .. ./00_data/in/insurance/DES-PA-11-2019.pdf y 10 t 9
location <- './00_data/in/insurance/'
list_files <- list.files(location)

master_tbl <- tibble()
total_tbl <- tibble()
col_names <- c("id", "entidad", "prima", "porc_prima", "no", "siniestro", "porcentaje_sieniestro", "prom_siniestro")
for(i in 1:length(list_files))
{
	print(paste0(location, list_files[i]))
	# Extract the table
	out <- extract_tables(paste0(location, list_files[i]))
	pdf_tables <- do.call(rbind, out[-length(out)])
	print(paste0("tables: ", length(out), " cols:", ncol(pdf_tables)))
	
	# table headers get extracted as rows with bad formatting. Dump them.
	pdf_tables <- as.data.frame(pdf_tables[3:nrow(pdf_tables), ])
	colnames(pdf_tables) <- col_names
	pdf_tables$file_name <- as.character(list_files[i])
	pdf_tables$file_seq <- rownames(pdf_tables)
	pdf_tables$count <- (length(out) - 1)
	
	print(paste0("uploaded: ", nrow(pdf_tables)))
	if(i == 1) {
	  total_tbl <- pdf_tables[1:(length(out) - 1), ]
		master_tbl <- pdf_tables[(length(out)):nrow(pdf_tables), ]
	} else {
		total_tbl <- rbind(total_tbl, pdf_tables[1:(length(out) - 1), ])
		master_tbl <- rbind(master_tbl, pdf_tables[(length(out)):nrow(pdf_tables), ])
	}
	print(paste0("total at this point: ", nrow(master_tbl)))
}


master_tbl %>% 
	glimpse()
View(total_tbl)
View(master_tbl)
table(master_tbl$id)

master_tbl  %>% 
	head(23)

fitered_tbl <- master_tbl %>% 
	filter(id != "TOTAL") %>% 
	filter(id != "#") %>% 
	mutate(
		# clean text
		entidad = stringr::str_trim(entidad),
		prima = stringr::str_trim(stringr::str_replace_all(prima, ",", ""), "both"),
		porc_prima = stringr::str_trim(stringr::str_replace(porc_prima, "%", ""), "both"),
		siniestro = stringr::str_trim(stringr::str_replace_all(siniestro, ",", ""), "both"),
		porcentaje_sieniestro = stringr::str_trim(stringr::str_replace(porcentaje_sieniestro, "%", ""), "both"),
		prom_siniestro = stringr::str_trim(stringr::str_replace(prom_siniestro, "%", ""), "both")
	) %>% 
	mutate(
		# change to number
		id = as.integer(id),
		prima = as.numeric(prima), 
		porc_prima = as.numeric(porc_prima)/100, 
		no = as.integer(no), 
		siniestro = as.numeric(porc_prima), 
		porcentaje_sieniestro = as.numeric(porc_prima)/100, 
		prom_siniestro = as.numeric(porc_prima), 
		file_seq = as.integer(file_seq)
	)

fitered_tbl  %>% 
	head(13)

nrow(fitered_tbl)/14
View(fitered_tbl)
table(fitered_tbl$id)

(fitered_tbl[1:14, ])
sum_entity <- sum(fitered_tbl[1:14, c("prima")])
name <- fitered_tbl[1, c("file_name")]
sum_entity
name

total_tbl %>% 
	mutate(
		entidad = stringr::str_trim(entidad),
		prima = stringr::str_trim(stringr::str_replace_all(prima, ",", ""), "both"),
		prima = as.numeric(prima)
	) %>% 
	filter(
		prima == sum_entity & file_name == name
	) %>% 
	dplyr::select(entidad) %>% 
	pull()

sum(fitered_tbl[15:28, c("prima")])




totals_tbl <- master_tbl %>% 
	filter(id == "TOTAL") %>% 
	mutate(
		entidad = stringr::str_trim(stringr::str_replace_all(entidad, "$", ""), "both"),
		entidad = stringr::str_trim(stringr::str_replace_all(entidad, ",", ""), "both")
	)


