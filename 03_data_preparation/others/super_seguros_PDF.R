cat("\014")
gc() # garbage collector
# ***************************************************************************
library(tabulizer)
library(dplyr)
# ***************************************************************************
# funtions ----
set_process_records <- function(detail_tbl, tot_tbl, pages, bug = 300) {
	fitered_tbl <- detail_tbl %>% 
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
			siniestro = as.numeric(siniestro), 
			porcentaje_sieniestro = as.numeric(porcentaje_sieniestro)/100, 
			prom_siniestro = as.numeric(prom_siniestro)/100, 
			file_seq = as.integer(file_seq), 
			nombre <- NA
		)
	
	
	
	init_record <- 1
	final_record <-14
	for(page in 1:pages) {
		# sum 14 items
		sum_entity <- round(sum(fitered_tbl[init_record:final_record, c("prima")]), 0)
		name <- fitered_tbl[1, c("file_name")]
		
		# I'm crossing the sum with a name
		search_name <- tot_tbl %>% 
			mutate(
				entidad = stringr::str_trim(entidad),
				prima = stringr::str_trim(stringr::str_replace_all(prima, ",", ""), "both"),
				prima = round(as.numeric(prima), 0)
			) %>% 
			filter(
				prima == sum_entity & file_name == name
			) %>% 
			dplyr::select(entidad) %>% 
			pull()
		print(paste0("entidad match:", search_name, " monto total:", sum_entity, " file name:", name, " final:", final_record))
		if(init_record > bug) {
			print(fitered_tbl[init_record:final_record, c("entidad", "prima")])
			temp <- tot_tbl %>% 
				mutate(
					entidad = stringr::str_trim(entidad),
					prima = stringr::str_trim(stringr::str_replace_all(prima, ",", ""), "both"),
					prima = as.numeric(prima)
				)
			print(temp[, c("entidad", "prima")])
		} 
		# asign 
		fitered_tbl[init_record:final_record, c("nombre")] <- ifelse(is.na(search_name), "unknow", search_name)
		
		init_record <- init_record + 14
		final_record <-final_record +14
		final_record <-ifelse(final_record > nrow(fitered_tbl), nrow(fitered_tbl), final_record)
	}
	return (fitered_tbl)
}

# ***************************************************************************
# Location of WARN notice pdf file .. ./00_data/in/insurance/DES-PA-11-2019.pdf y 10 t 9
location <- './00_data/in/insurance/'
list_files <- list.files(location)


master_tbl <- tibble()
total_tbl <- tibble()
col_names <- c("id", "entidad", "prima", "porc_prima", "no", "siniestro", "porcentaje_sieniestro", "prom_siniestro")
for(i in 1:length(list_files)) # 
{
	print(paste0(location, list_files[i]))
	# Extract the table
	out <- extract_tables(paste0(location, list_files[i])) # 
	pdf_tables <- do.call(rbind, out) # [-length(out)]
	print(paste0("tables: ", length(out), " cols:", ncol(pdf_tables)))
	
	# table headers get extracted as rows with bad formatting. Dump them.
	pdf_tables <- as.data.frame(pdf_tables[2:nrow(pdf_tables), ])
	colnames(pdf_tables) <- col_names
	pdf_tables$file_name <- as.character(list_files[i])
	pdf_tables$file_seq <- rownames(pdf_tables)
	pdf_tables$count <- (length(out) - 1)
	
	detail_tbl <- set_process_records(
		pdf_tables[((length(out)):nrow(pdf_tables)+2), ], # detail
		pdf_tables[1:(length(out)), ],            # total
		pages = (length(out) - 1)
	)
	
	
	print(paste0("uploaded: ", nrow(pdf_tables)))
	if(i == 1) {
	  total_tbl <- pdf_tables[1:(length(out) - 1), ]
		master_tbl <- detail_tbl #pdf_tables[(length(out)):nrow(pdf_tables), ]
	} else {
		total_tbl <- rbind(total_tbl, pdf_tables[1:(length(out) - 1), ])
		master_tbl <- rbind(master_tbl, detail_tbl)
	}
	print(paste0("total at this point: ", nrow(master_tbl)))
}

# final clean
for(i in 1:24) {
	master_tbl <- master_tbl %>% 
		mutate(
			nombre = stringr::str_replace_all(nombre, paste0(i," "),  ""),
			nombre = stringr::str_replace_all(nombre, paste0("1 "),  ""), 
			nombre = stringr::str_replace_all(nombre, paste0("2 "),  "") 
		)
}
master_tbl <- master_tbl %>% 
	rename(producto = entidad) %>% 
	mutate(
		entidad = stringr::str_trim(nombre, "both")
	) 


# ************************
# review
master_tbl %>% 
	glimpse()
View(master_tbl)

temp <- master_tbl  %>% 
	group_by(entidad) %>% 
	summarise(mean = mean(prima), n = n()) 
View(temp)

write.csv(master_tbl, paste0("./00_data/out/insurance/out_insurance_process.csv"), 
														 row.names = FALSE)

