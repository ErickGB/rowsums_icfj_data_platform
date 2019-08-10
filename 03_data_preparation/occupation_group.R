
cat("\014")
# ***********************************************
# Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(purrr)     # Functional programming
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(XML)
# ***********************************************
PATH_OUT <- "./00_data/out/salaries/"
date_time <- as.character(Sys.Date())

data_tbl <- readr::read_csv(paste0(PATH_OUT, "out_jun-may-abr.csv"))
data_tbl %>% 
	glimpse()

occupations_tbl <- data_tbl %>% 
	group_by(cargo, codigo) %>% 
	summarize(count = n(), salary = mean(salario))

# 3k
occupations_tbl <- occupations_tbl %>% 
	mutate(
		occupation_sub_group = purrr::map_chr(cargo, .f = function(x) {
		return(substr(x, 1, (as.numeric(gregexpr(pattern =' ',x)[[1]][1]) -1)))
		}),
		occupation_sub_group = ifelse(occupation_sub_group == "", cargo, occupation_sub_group)
		) 

# 1.4k
occupations_tbl <- occupations_tbl %>% 
	mutate(
		occupation_group = purrr::map_chr(occupation_sub_group, .f = function(x) {
		return(substr(x, 1, (as.numeric(gregexpr(pattern ='_',x)[[1]][1]) -1)))
		})
		) 

# 187
summary_tbl <- occupations_tbl %>% 
	group_by(occupation_sub_group, codigo) %>% 
	summarize(mean_salary = mean(salary)) %>% 
	spread(key = codigo, value = mean_salary)
	
summary_tbl[is.na(summary_tbl)] <- 0

library(recipes)
system.time(
	recipe_obj <- recipe( ~ ., data = summary_tbl) %>%
 	    step_center(all_numeric() ) %>% 
	    step_scale(all_numeric()) %>% 
	    prep(stringsAsFactors = FALSE)
) 

recipe_obj

final_tbl <- bake(recipe_obj, new_data = summary_tbl)
 

km_out <- list()
sil_out <- list()
x <- vector()
y <- vector()
min_clust <- 2      # Hypothesized minimum number of segments
max_clust <- 200    # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in min_clust:max_clust) {
        i <- centr-(min_clust-1) # relevels start as 1, and increases with centr
        set.seed(777) # For reproducibility
        km_out[i] <- list(kmeans(final_tbl$mean_salary, centers = centr, iter.max=1000000, nstart=1))
        sil_out[i] <- list(cluster::silhouette(km_out[[i]][[1]], dist(final_tbl$mean_salary)))
        # Used for plotting silhouette average widths
        x[i] = centr  # value of k
        y[i] = summary(sil_out[[i]])[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("numero") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies") +
	theme_minimal()


sillhoute_tbl <- data.frame(x, y) %>% 
	filter(y > 0.25) %>% 
	arrange(desc(y)) %>% 
	head(10)

sillhoute_tbl

# -------------------------------------------------------------
# Decision Tree C5.0
# -------------------------------------------------------------
library(C50)
final_tbl$cluster <- km_out[187][[1]]$cluster

model_tree <- C50::C5.0(data.frame(salary = final_tbl[, c("mean_salary")]) , as.factor(final_tbl$mean_salary))
summary(model_tree)
plot(model_tree)

