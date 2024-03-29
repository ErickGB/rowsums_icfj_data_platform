get_count <- function(x) {
	len <- (as.integer(regexpr(' ', x)))  
	text <- substr(x, 1, (len -1))
	text <- ifelse(is.na(text), "-1", text)
	return(as.integer(text))
}
#get_all_products('')
get_all_products <- function(url) {
	session <- html_session(url)
	operation_type <- substr(url, 133, 133)
	print(url)

	
		# date, master table
	date_var <- session %>% 
			rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
		  rvest::html_nodes(css = 'table.TablaDato') %>% 
		  rvest::html_nodes('tr') %>% 
		  rvest::html_nodes(css = 'td.colFecha') %>% 
		  rvest::html_text()
	
	# RUC 
	ruc_var <- session %>% 
			rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
		  rvest::html_nodes(css = 'table.TablaDato') %>% 
		  rvest::html_nodes('tr') %>% 
		  rvest::html_nodes(xpath = 'td[2]') %>% 
		  rvest::html_nodes('span') %>% 
		  rvest::html_text()
	
	# RUC name company
	company_var <- session %>% 
			rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
		  rvest::html_nodes(css = 'table.TablaDato') %>% 
		  rvest::html_nodes('tr') %>% 
		  rvest::html_nodes(xpath = 'td[contains(@valign,"top")]') %>% 
		  rvest::html_text() %>% 
		  str_remove_all("\n") %>%
	    str_remove_all("\r") %>%
		  str_replace_all("\t\t\t", ";") %>% 
		  str_remove_all("\t")
	
	# procedencia *OK*
	source_var <- session %>% 
			rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
		  rvest::html_nodes(css = 'table.TablaDato') %>% 
		  rvest::html_nodes('tr') %>% 
		  rvest::html_nodes(xpath = 'td[3]') %>% 
		  rvest::html_nodes('span') %>% 
		  rvest::html_text()
	
	# descripción de la mercancia
	description_var <- session %>% 
			rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
		  rvest::html_nodes(css = 'table.TablaDato') %>% 
		  rvest::html_nodes('tr') %>% 
		  rvest::html_nodes(xpath = 'td[3]') %>% 
		  rvest::html_text() %>% 
		  str_remove_all("\n") %>%
	    str_remove_all("\r") %>%
		  str_replace_all("\t\t\t", ";") %>% 
		  str_remove_all("\t") %>% 
		  substr(2, nchar(.)) 
	
	data_tbl <- tibble(
		date = date_var, 
		company = company_var,
		source = source_var, 
		description = description_var
		) %>% 
		mutate(key = as.numeric(rownames(.)) )
	
	
	data_tbl <- data_tbl %>% 
		mutate(
			company = substr(company, 27, nchar(company)),
			RUC = purrr::map_chr(company, function(x) {stringr::str_split(x, ";")[[1]][1]}),
			company = purrr::map_chr(company, function(x) {stringr::str_split(x, ";")[[1]][2]}),
			origin = substr(source, nchar(source) - 1, nchar(source)),
			text_original = description,
			description = stringr::str_trim(substr(description, 17, nchar(description)), side = 'both'),
			description = stringr::str_replace(description, "Mostrar información", " ;"),
			description = purrr::map_chr(description, function(x) {
				position = (as.numeric(str_locate(x, ";")[1]) - 1)
				str_dat <- ifelse(is.na(substr(x, 1, position)) == TRUE, x, substr(x, 1, position))
				return (str_dat)
				}
				),
			source = NULL
			) %>% 
		dplyr::select(date, RUC, company, origin, description, key, text_original)
	
	# ***********************************************************
	# detail table ----
	
	# fields (14)
	fields_var <- session %>% 
			rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
		  rvest::html_nodes(css = 'table.TablaDato') %>% 
		  rvest::html_nodes('tr') %>% 
		  rvest::html_nodes(css = 'td.dataRUC_label') %>% 
		  rvest::html_text()
	
	# data (14)
	data_var <- session %>% 
			rvest::html_nodes(css = 'div[id="areaMainCont"]') %>% 
		  rvest::html_nodes(css = 'table.TablaDato') %>% 
		  rvest::html_nodes('tr') %>% 
		  rvest::html_nodes(css = 'td.dataRUC') %>% 
		  rvest::html_text()
	
	variables_tbl <- tibble(
		fields = fields_var,
		data = data_var) %>% 
		mutate(key = floor((as.numeric(rownames(.)) -1) /14) + 1)
	
	variables_tbl <- variables_tbl %>% 
		mutate(
			data_processed = str_replace(data, "B/. ", "")
		)
	
	variables_tbl <- variables_tbl %>% 
		mutate(
			data_processed = str_replace(data, "B/. ", ""),
			data_processed = str_replace(data_processed, ";", ""),
			data_processed = str_replace(data_processed, ",", ""),
			data_processed = str_trim(data_processed, side = "both"),
			#data_processed = str_replace(data_processed, " Kg", ""),
			#data_processed = str_replace(data_processed, " Unidad", ""),
			fields = ifelse(fields == '"Peso Neto: "', 'Peso Neto:', fields)
			)
	
	variables_tbl <- variables_tbl %>% 
		dplyr::select(key, fields, data_processed) %>% 
		spread(key = fields, value = data_processed) %>% 
		janitor::clean_names()
	
	variables_tbl <- variables_tbl %>% 
		mutate(
			peso_bruto = str_replace(peso_bruto, ",", ""), 
			peso_neto = str_replace(peso_neto, ",", ""),
			total_a_pagar = str_replace(total_a_pagar, ",", ""),
			impuestos_de_proteccion_de_petroleo = str_replace(impuestos_de_proteccion_de_petroleo, ",", ""),
			valor_cif = str_replace(valor_cif, ",", ""),
			valor_fob = str_replace(valor_fob, ",", ""),
			total_pagar_txt = str_trim(total_a_pagar),
			impuestos_de_proteccion_de_petroleo_txt = str_trim(impuestos_de_proteccion_de_petroleo),
			valor_cif_txt = str_trim(valor_cif),
			valor_fob_txt = str_trim(valor_fob),
			cantidad_int = map_int(cantidad, get_count),
			peso_bruto_kg = str_replace(peso_bruto, " Kg", ""), 
			peso_neto_kg = str_replace(peso_neto, " Kg", "")		
		) 
	
	if(operation_type == "I") {
		variables_tbl <- variables_tbl %>% 
			mutate(
				impuestos_de_importacion = as.numeric(impuestos_de_importacion), # separar preguntando si es I o E.
				impuestos_de_proteccion_de_petroleo = as.numeric(impuestos_de_proteccion_de_petroleo), 
				impuestos_isc = as.numeric(impuestos_isc), 
				impuestos_itbm = as.numeric(impuestos_itbm), 
				cantidad_int = as.integer(cantidad_int), 
				peso_bruto_kg = as.numeric(peso_bruto_kg), 
				peso_neto_kg = as.numeric(peso_neto_kg), 
				total_a_pagar = as.numeric(total_a_pagar), 
				valor_cif = as.numeric(valor_cif), 
				valor_fob = as.numeric(valor_fob), 
				valor_del_seguro = as.numeric(valor_del_seguro)
			)
	} else {
		variables_tbl <- variables_tbl %>% 
			mutate(
				impuestos_de_exportacion = as.numeric(impuestos_de_exportacion), # separar preguntando si es I o E.
				impuestos_de_proteccion_de_petroleo = as.numeric(impuestos_de_proteccion_de_petroleo), 
				impuestos_isc = as.numeric(impuestos_isc), 
				impuestos_itbm = as.numeric(impuestos_itbm), 
				cantidad_int = as.integer(cantidad_int), 
				peso_bruto_kg = as.numeric(peso_bruto_kg), 
				peso_neto_kg = as.numeric(peso_neto_kg), 
				total_a_pagar = as.numeric(total_a_pagar), 
				valor_cif = as.numeric(valor_cif), 
				valor_fob = as.numeric(valor_fob), 
				valor_del_seguro = as.numeric(valor_del_seguro)
			)
	}
	
	data_final_tbl <- inner_join(data_tbl, variables_tbl, by = 'key')
	
	return(data_final_tbl)

}
