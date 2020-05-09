###########################################################
# Title:"Scrape GOOGLE trends website"
# Author:"Jose A. Ferreira Queimada"
# Date:"20 de Abril de 2020"
###########################################################


###########################################################
# PACKAGES REQUIRED TO BE INSTALLED 
# as they don't come with Base R
###########################################################
# install.packages("gtrendsR")
# Beta version: devtools::install_github("PMassicotte/gtrendsR")
# install.packages("ggpubr")
# Beta version: devtools::install_github("kassambara/ggpubr")
# install.packages("jsonlite")
# install.packages("openxlsx", dependencies = TRUE)
# install.packages("patchwork")


###########################################################
# LIBRARIES
###########################################################
library(gtrendsR)	# to retrieve data from Google Trends
library(rlang)		# to use duplicate
library(dplyr)		# to use %>%
library(ggplot2)	# to plot graphs during EDA
library(ggpubr)		# to use multiple plots in one grid
library(openxlsx)	# to export datasets to Excel file
library(patchwork)	# show multiple plots in one grid


###########################################################
# FUNCTION TO RETRIEVE DATA FROM GOOGLE TRENDS
###########################################################
# Some interesting URLs to check
# https://www.ssl.com/country-codes/
# https://github.com/pat310/google-trends-api/wiki/Google-Trends-Categories -> TRAVEL = 67

gather_data <- function(search_string, geo_string = NULL, time_string = NULL, category_string = NULL) {
	if (is.null(geo_string)) {
		geo_string <- c("AR", "AW", "BS", "BB", "BZ", "BO", "BR", "CA", "CO", "CL", "CR", "CU", "CW", "DM", "DO", "EC", "SV", "GT", "GY", "HT", "HN", "JM", "MQ", "MX", "NI", "PA", "PY", "PE", "PR", "SX", "US", "UY", "VE")
	}
	
	if (is.null(time_string)) {
		time_string <- "today 3-m"
	}
	
	if (is.null(category_string)) {
		category_string <- "0"
	}
	
	counter <- 1
	for (geo_now in geo_string) {
		
		print(paste("SEARCH: ", search_string, " - LOCATION: ", geo_now, " (", toString(counter), " of ", toString(length(geo_string))," elements)", sep=""))
		
		tmp_result <- gtrends(search_string, geo = geo_now, time = time_string, category = category_string)
		Sys.sleep(1)
		
		if (exists("resultado")) {
			resultado$interest_over_time <- rbind(resultado$interest_over_time, tmp_result$interest_over_time)
			resultado$interest_by_dma <- rbind(resultado$interest_by_dma, tmp_result$interest_by_dma)
			resultado$interest_by_region <- rbind(resultado$interest_by_region, tmp_result$interest_by_region)
			resultado$interest_by_city <- rbind(resultado$interest_by_city, tmp_result$interest_by_city)
			resultado$related_queries <- rbind(resultado$related_queries, tmp_result$related_queries)
		} else {
			resultado <- duplicate(tmp_result, shallow = FALSE)
		}
		
		counter <- counter + 1
	}
	
	return(resultado)
}


###########################################################
# DATA GATHERING PROCESS
###########################################################
# Due to maximum limit of 5 topics to search, we need to create a "n-loops" process, where "n" = Number_Searches / 5. 
# Each loop will retrieve data about 5 search topics.

big_i <- 1
while (big_i <= 4) {
	
	if (big_i == 1) {
		search_elements <- c("vuelos baratos", "boletos baratos", "cheap flight", "voos baratos", "passagen aerea barata")
	}
	if (big_i == 2) {
		search_elements <- c("vuelo a Miami", "vuelo a MIA", "vuelo a Los Angeles", "vuelo a LA", "vuelo a New York", "vuelo a Nueva York", "vuelo a NY", "vuelo a San Francisco", "vuelo a Brasil", "vuelo a Mexico", "vuelo a M�xico", "vuelo a Mejico", "vuelo a M�jico", "vuelo al caribe", "vuelo a Argentina", "vuelo a Colombia")
	}
	if (big_i == 3) {
		search_elements <- c("flight to Miami", "flight to MIA", "flight to Los Angeles", "flight to LA", "flight to New York", "flight to NY", "flight to San Francisco", "flight to Brasil", "flight to Brazil", "flight to Mexico", "flight to DF", "flight to Argentina", "flight to Colombia")
	}
	if (big_i == 4) {
		search_elements <- c("coronavirus", "COVID-19", "COVID 19", "COVID", "travel", "viajar", "cenar fuera")
	}
	
	# >>>>>>>>>>>>>><<<<<<<<<<<<<<<>>>>>>>>>>>>>><<<<<<<<<<<<<>>>>>>>>>>>>>>
	# >>>>>>>>>>>>>> Don't change anything below this point <<<<<<<<<<<<<<<<
	# >>>>>>>>>>>>>><<<<<<<<<<<<<<<>>>>>>>>>>>>>><<<<<<<<<<<<<>>>>>>>>>>>>>>
	counter <- 1
	for (i in search_elements) {
		
		print(paste("SEARCHING... ", i, " (", toString(counter), " of ", toString(length(search_elements)), ")", sep = ""))
		output <- gather_data(i)
		
		if (exists("total_output")) {
			total_output$interest_over_time <- rbind(total_output$interest_over_time, output$interest_over_time)
			total_output$interest_by_dma <- rbind(total_output$interest_by_dma, output$interest_by_dma)
			total_output$interest_by_region <- rbind(total_output$interest_by_region, output$interest_by_region)
			total_output$interest_by_city <- rbind(total_output$interest_by_city, output$interest_by_city)
			total_output$related_queries <- rbind(total_output$related_queries, output$related_queries)
		} else {
			total_output <- duplicate(output, shallow = FALSE)
		}
		
		counter <- counter + 1
	}
	
	if (exists("interest_over_time")) {
		interest_over_time <- rbind(interest_over_time, total_output$interest_over_time)
	} else {
		interest_over_time <- total_output$interest_over_time
	}
	# Remove duplicates (just in case)
	interest_over_time <- interest_over_time[!duplicated(interest_over_time),]
	
	if (exists("interest_by_region")) {
		interest_by_region <- rbind(interest_by_region, total_output$interest_by_region)
	} else {
		interest_by_region <- total_output$interest_by_region
	}
	# Remove duplicates (just in case)
	interest_by_region <- interest_by_region[!duplicated(interest_by_region),]
	
	if (exists("related_topics")) {
		related_topics <- rbind(related_topics, total_output$related_topics)
	} else {
		related_topics <- total_output$related_topics
	}
	# Remove duplicates (just in case)
	related_topics <- related_topics[!duplicated(related_topics),]
	# Encoding to UTF-8
	Encoding(related_topics[["value"]]) <- "UTF-8"
	
	if (exists("related_queries")) {
		related_queries <- rbind(related_queries, total_output$related_queries)
	} else {
		related_queries <- total_output$related_queries
	}
	# Remove duplicates (just in case)
	related_queries <- related_queries[!duplicated(related_queries),]
	# Encoding to UTF-8
	Encoding(related_queries[["value"]]) <- "UTF-8"
	
	# Let's wait 1 minute until the next "5 topics" search...
	Sys.sleep(60)
	
	big_i <- big_i + 1
}

# Every hit value equals to "<1" is changed to "0"
interest_over_time$hits[interest_over_time$hits %in% c("<1")] <- "0"
interest_by_region$hits[interest_by_region$hits %in% c("<1")] <- "0"

# Change data type
interest_over_time$hits <- as.numeric(interest_over_time$hits)

# Housekeeping
rm(big_i, i, counter, search_elements, output, total_output)


###########################################################
# SIMPLIFY KEYWORDS
###########################################################
# This step is not required, but it helps to simplify the keyword's set into a smaller one
# This step requires manual intervention
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelos baratos", "boletos baratos", "cheap flight", "voos baratos", "passagen aerea barata")] <- "CHEAP FLIGHTS"
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelo a Miami", "vuelo a MIA", "flight to Miami", "flight to MIA")] <- "Flights to MIAMI"
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelo a Los Angeles", "vuelo a LA", "flight to Los Angeles", "flight to LA")] <- "Flights to LOS ANGELES"
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelo a New York", "vuelo a Nueva York", "vuelo a NY", "flight to New York", "flight to NY")] <- "Flights to NEW YORK"
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelo a San Francisco", "flight to San Francisco")] <- "Flights to SAN FRANCISCO"
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelo a Brasil", "flight to Brasil", "flight to Brazil")] <- "Flights to BRAZIL"
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelo a Mexico", "vuelo a M�xico", "vuelo a Mejico", "vuelo a M�jico", "flight to Mexico", "flight to DF")] <- "Flights to MEXICO"
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelo a Argentina", "flight to Argentina")] <- "Flights to ARGENTINA"
interest_over_time$keyword[interest_over_time$keyword %in% c("vuelo a Colombia", "flight to Colombia")] <- "Flights to COLOMBIA"
interest_over_time$keyword[interest_over_time$keyword %in% c("coronavirus", "COVID-19", "COVID 19", "COVID")] <- "COVID-19"
interest_over_time$keyword[interest_over_time$keyword %in% c("travel", "viajar", "cenar fuera")] <- "LEISURE"


###########################################################
# EXPORT RESULTS TO XLS FILE
###########################################################
# Generate XLS file name
file_name <- paste(gsub("-", "", toString(Sys.Date())), "_GOOGLE_TREND_SCRAP.xlsx", sep = "")

# Write each dataset in a different sheet
list_of_datasets <- list("INTEREST OVER TIME" = interest_over_time
												 , "INTEREST BY REGION" = interest_by_region
												 , "RELATED TOPICS" = related_topics
												 , "RELATED QUERIES" = related_queries)
write.xlsx(list_of_datasets, file = file_name)

# Housekeeping
rm(file_name, list_of_datasets)


###########################################################
# SAVE WORKSPACE DATA
###########################################################
save.image(file = 'GOOGLE_TREND.RData')


###########################################################
# THAT'S ALL FOLKS!
###########################################################
print("END OF PROCESS")


###########################################################
# EXPLORATORY DATA ANALYSIS (EDA)
###########################################################

interest_over_time %>%
	filter(hits != "<1") %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(geo, keyword) %>%
	summarise(average = mean(hits), maximun = max(hits), minimum = min(hits))

interest_over_time %>%
	filter(hits != "<1") %>%
	filter(geo == c("PA", "US", "BR", "CL")) %>%
	filter(keyword == c("COVID-19")) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, geo) %>%
	summarise(average = mean(hits), maximun = max(hits), minimum = min(hits)) %>%
	ggplot(aes(x = date, y = average, group = geo, colour = geo)) +
	geom_line() +
	stat_smooth(method = "lm")

interest_over_time %>%
	filter(hits != "<1") %>%
	filter(keyword != "COVID-19") %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(geo) %>%
	summarise(average = mean(hits), maximun = max(hits), minimum = min(hits)) %>%
	ggbarplot(x = "geo", y = "average",
						fill = "geo", # change fill color by geo
						color = "white", # Set bar border colors to white
						sort.val = "asc", # Sort the value in ascending order
						x.text.angle = 90 # Rotate vertically x axis texts
	) +
	font("x.text", size = 8)



# >>>>>>>>>>>>>><<<<<<<<<<<<<<<>>>>>>>>>>>>>><<<<<<<<<<<<<>>>>>>>>>>>>>>
# >>> COMPARE COVID-19 VS NON COVID-19 INTEREST
# >>>>>>>>>>>>>><<<<<<<<<<<<<<<>>>>>>>>>>>>>><<<<<<<<<<<<<>>>>>>>>>>>>>>

tmp_no_COVID <- interest_over_time %>%
	filter(hits != "<1") %>%
	filter(keyword != "COVID-19") %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, geo) %>%
	summarise(average = mean(hits), maximun = max(hits), minimum = min(hits))
tmp_no_COVID$type <- "not COVID-19"

tmp_COVID <- interest_over_time %>%
	filter(hits != "<1") %>%
	filter(keyword == "COVID-19") %>%
	filter(as.Date(date) >= Sys.Date() - 30) %>%
	group_by(date, geo) %>%
	summarise(average = mean(hits), maximun = max(hits), minimum = min(hits))
tmp_COVID$type <- "COVID-19"

tmp_comparison <- tmp_no_COVID
tmp_comparison <- rbind(tmp_comparison, tmp_COVID)
rm(tmp_COVID, tmp_no_COVID)

# List of countries
unique(tmp_comparison$geo)

# One plot per selected country
filter_by_country <- c("AR")
plot_AR <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

filter_by_country <- c("BR")
plot_BR <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

filter_by_country <- c("CL")
plot_CL <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

filter_by_country <- c("CO")
plot_CO <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

filter_by_country <- c("MX")
plot_MX <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

filter_by_country <- c("PA")
plot_PA <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

filter_by_country <- c("PE")
plot_PE <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

filter_by_country <- c("US")
plot_US <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

filter_by_country <- c("VE")
plot_VE <- tmp_comparison %>%
	filter(geo %in% filter_by_country) %>%
	filter(as.Date(date) >= Sys.Date() - 15) %>%
	group_by(date, type) %>%
	summarise(average = round(mean(average), digits = 0)) %>%
	filter(average > 0) %>%
	ggplot(aes(x = date, y = average, group = type, colour = type)) +
	geom_line() +
	stat_smooth(method = "lm") +
	ggtitle(label = paste(filter_by_country, " - Trend comparison between COVID-19 and non COVID related searches", sep = ""),
					subtitle = "Hits by country (last 15 days)")

# Housekeeping
rm(filter_by_country)

# Plot
(plot_AR | plot_BR) / (plot_CL | plot_CO) / (plot_MX | plot_US) / plot_PA

plot_PA + {
	plot_US + {
		plot_BR +
			plot_CO +
			plot_layout(ncol = 1)
	}
}
+ plot_layout(ncol = 1)

plot_PA + {
	plot_US + {
		plot_BR +
			plot_CO +
			plot_layout(ncol = 1)
	}
} + plot_layout(ncol = 1)


ggdotchart(tmp_comparison, x = "geo", y = "average",
					 color = "geo",									# Color by groups
					 #palette = c("#00AFBB", "#E7B800", "#FC4E07"),	# Custom color palette
					 sorting = "descending",							# Sort value in descending order
					 add = "segments",								# Add segments from y = 0 to dots
					 rotate = FALSE,									# Do not rotate vertically
					 #group = "keyword",								# Order by groups
					 dot.size = 10,									# Large dot size
					 label = round(mean(tmp_comparison$average), digits = 0), # Add average values as dot labels
					 font.label = list(color = "white",
					 									size = 9,
					 									vjust = 0.5),					# Adjust label parameters
					 ggtheme = theme_pubr(),							# ggplot2 theme
					 ylab = "Hits",
					 xlab = "Countries",
					 legend.title = "Countries") +
	ggtitle(label = "Searches not related to COVID",
					subtitle = "Hits by country (last 7 days)") +
	theme(legend.position = "none") +
	theme(legend.title = element_blank()) +
	theme(plot.title = element_text(color = "red", size = 14, face = "bold.italic")) +
	theme(plot.title = element_text(hjust = 0.5)) + # title centered
	theme(plot.subtitle = element_text(color = "blue", face = "italic")) +
	theme(axis.title.x = element_text(color = "blue", size = 14, face = "bold")) +
	theme(axis.title.y = element_text(color = "#993333", size = 14, face = "bold")) +
	scale_x_discrete()




