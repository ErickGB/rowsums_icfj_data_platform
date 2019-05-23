install.packages("tidyverse")
install.packages("readr")
install.packages("rvest")

install.packages("bigrquery")
install.packages("googleCloudStorageR")

install.packages("xopen")
install.packages("furrr")
install.packages("XML")
install.packages("purrr")

install.packages("spdplyr")
install.packages("RPostgreSQL") 


# 1084727730786-stm0iqvk0r7b1r641c85pt6fjd59tmpo.apps.googleusercontent.com
# fv-vQgarUQPES_6STLCaIGJj

library()

library(googleCloudStorageR)
library(bigrquery)
 
 gcs_global_bucket("rowsums")
 
 ## custom upload function to ignore quotes and column headers
 f <- function(input, output) {
   write.table(input, sep = ",", col.names = FALSE, row.names = FALSE, 
               quote = FALSE, file = output, qmethod = "double")}
   
 ## upload files to Google Cloud Storage
 gcs_upload(train_raw, name = "out_cental_gov_salaries.csv", object_function = f)
 
 ## create the schema of the files you just uploaded
 user_schema <- schema_fields(train_raw)
 
 ## load files from Google Cloud Storage into BigQuery
 bqr_upload_data(projectId = "rowsums", 
                datasetId = "out_central_gov_salaries", 
                tableId = "central_gov_salaries", 
                upload_data = c("gs://rowsums/out_cental_gov_salaries.csv"),
                schema = user_schema)
 
 ## for big files, its helpful to create your schema on a small sample
 ## a quick way to do this on the command line is:
 # "head bigfile.csv > head_bigfile.csv"
