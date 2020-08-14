
# ***********************************************
library(tidyverse) # Main Package - Loads dplyr, purrr
library(AzureStor)
# ***********************************************
endp <- storage_endpoint("https://planillaptystaging.blob.core.windows.net", key="rMEGfnIWVo9eFefDiEk8pXpt3w6edCra8RLi43X/GGb7ve8pWzRdeJP/VBYSvkmhDxct06EkZY1ZGKUfYUv0Aw==")

set_load_data_az <- function(container, src_path, des_path, fname) {
	cont <- storage_container(endp, container) # "ctrade"
	print(paste0(src_path, fname))
	upload_blob(cont, paste0(src_path, fname), dest=paste0(des_path, fname))
	
	print(paste0("uplodad file OK:", paste0(des_path, fname), "... "))
}
