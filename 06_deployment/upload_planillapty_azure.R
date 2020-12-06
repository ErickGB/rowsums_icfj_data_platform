cat("\014")
gc()
# *******************************************************************************
# Load libraries ----
# install.packages("AzureStor", dependencies = TRUE)

library(tidyverse) # Main Package - Loads dplyr, purrr
library(AzureStor)
# *******************************************************************************
endp <- storage_endpoint("https://planillaptystaging.blob.core.windows.net", key="rMEGfnIWVo9eFefDiEk8pXpt3w6edCra8RLi43X/GGb7ve8pWzRdeJP/VBYSvkmhDxct06EkZY1ZGKUfYUv0Aw==")
cont <- storage_container(endp, "planillapty")
list_blobs(cont)
#storage_download(cont, "myblob.csv", "local_filename.csv")
src_path <- "./00_data/out/salaries/pending_process/april/central_gov_salaries_april.csv"
upload_blob(cont, src_path, dest="/2020/april/central_gov_salaries_april.csv")


src_path <- "./00_data/out/salaries/pending_process/febrary/miamb_gov_salaries_february.csv"
upload_blob(cont, src_path, dest="/2020/febrary/miamb_gov_salaries_february.csv")

file_type <- "export"
PATH_OUT <- "./00_data/out/imports/"
list_files <- base::list.files(PATH_OUT)
list_files

set_load_data_az <- function(src_path, des_path, fname) {
	cont <- storage_container(endp, "ctrade")
	# "/2020/febrary/miamb_gov_salaries_february.csv"
	print(paste0(src_path, fname))
	upload_blob(cont, paste0(src_path, fname), dest=paste0(des_path, fname))
}
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2018-06-30.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2017-06-30.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2019-06-30.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2020-04-30.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2020-06-30.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2020-05-31.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2019-12-31.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2018-12-31.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2017-12-31.csv")
set_load_data_az("./00_data/out/exports/", "/export/2020/", "out_exports_2016-12-31.csv")

# imports
set_load_data_az("./00_data/out/imports/", "/import/2020/", "out_imports_2020-03-31.csv")
set_load_data_az("./00_data/out/imports/", "/import/2020/", "out_imports_2020-04-30.csv")
set_load_data_az("./00_data/out/imports/", "/import/2020/", "out_imports_2020-05-31.csv")
set_load_data_az("./00_data/out/imports/", "/import/2020/", "out_imports_2020-06-30.csv")


system.time(
	# load data ----
	data_tbl <- data_tbl %>% 
		mutate(tbl = furrr::future_map_dfc(spath, dpath, file_name, set_load_data_az), .progress = TRUE) %>% 
		tidyr::unnest()
)









