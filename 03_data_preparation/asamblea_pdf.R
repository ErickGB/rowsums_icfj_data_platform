

install.packages("tabulizer", dependencies=TRUE)
library(tabulizer)
library(rJava)

site <- "https://www.asamblea.gob.pa/sites/default/files/2019-06/GIRAS-FEBRERO-2019.pdf"
# default call with no parameters changed
matrix_results <- extract_tables(site)
NROW(matrix_results)
matrix_results[[1]][3:9]
matrix_results[[1]][,1][3:9]
matrix_results[[1]][,2][3:9]
matrix_results[[1]][,3][3:9]
matrix_results[[1]][,4][3:9]
matrix_results[[1]][,5][3:9]
matrix_results[[1]][,6][3:9]



# get back the tables as data frames, keeping their headers
df_results <- extract_tables(site, output = "data.frame", header = TRUE)

first_df <- df_results[[1]]
first_df$Number.of.Coils


text <- extract_text(site)
# print text
cat(text)


get_n_pages(site)
extract_metadata(site)






install.packages("pdftools")
library(pdftools)
pdf_file <- "https://github.com/ropensci/tabulizer/raw/master/inst/examples/data.pdf"
txt <- pdf_text(site)
cat(txt[1])
cat(txt[2])

# All textboxes on page 1
temp_text <- pdf_data(site)[[1]]


