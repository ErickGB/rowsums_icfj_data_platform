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
