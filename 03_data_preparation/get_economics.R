#install.packages("Quandl")
# https://fred.stlouisfed.org/categories/32784?cid=32784&t=&et=&pageID=2

library(Quandl)
library(tidyverse)

usa_10year <- Quandl("YC/USA10Y", api_key="4kULas618zFnkNm6K79Y")
max(usa_10year$Date)

# WTI
path_file <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=748&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DCOILWTICO&scale=left&cosd=2015-05-04&coed=2020-05-04&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=2020-05-08&revision_date=2020-05-08&nd=1986-01-02"
data_wti_tbl <- readr::read_csv(path_file) %>% 
	
	arrange(desc(DATE))

# BRENT
path_file <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=748&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DCOILBRENTEU&scale=left&cosd=2015-05-04&coed=2020-05-04&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=2020-05-08&revision_date=2020-05-08&nd=1987-05-20"
data_brent_tbl <-  readr::read_csv(path_file) %>% 
	arrange(desc(DATE))
data_brent_tbl
# treasure bouns 10 years
path_file <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=IRLTLT01USM156N&scale=left&cosd=1960-01-01&coed=2020-03-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=2020-05-08&revision_date=2020-05-08&nd=1960-01-01"
data_treasure_bones_10y_tbl <-  readr::read_csv(path_file)
data_treasure_bones_10y_tbl %>% 
	arrange(desc(DATE))


