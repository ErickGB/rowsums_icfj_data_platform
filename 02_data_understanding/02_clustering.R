# *********************************************************************
# carga librerias ----
library(tidyverse) 
library(factoextra)
library(C50)

library(plotly)
# *********************************************************************

# revisamos los nulos
train_tbl %>% 
  DataExplorer::plot_missing()

# remove outliers



#*****************************************
## Initial Cluster analysis (k selection) ----
#*****************************************
set.seed(7777)

# Explora para establecer el número de "k" potenciales
km_out <- list()
sil_out <- list()
x <- vector()
y <- vector()
min_clust <- 2      # Hypothesized minimum number of segments
max_clust <- 20    # Hypothesized maximum number of segments

# calcula los k means desde min_clust hasta max_clust
for (centr in min_clust:max_clust) {
        i <- centr-(min_clust-1) 
        km_out[i] <- list(kmeans(train_tbl, centers = centr, iter.max=1000, nstart=1))
        sil_out[i] <- list(cluster::silhouette(km_out[[i]][[1]], dist(train_tbl)))
        # Para generar el plot 
        x[i] = centr  # value of k
        y[i] = summary(sil_out[[i]])[[4]]  # Silhouette average width
}


# Plot silhouette 
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Número de clústers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

data.frame(x, y) %>% 
	arrange(desc(y))

#    x         y
#1   2 0.9813632
#2   6 0.9711128
#3   8 0.9704322
#4   3 0.9702369
#5   5 0.9694590
#6  11 0.9676697
#7   4 0.9674228
#8  13 0.9669128
#9  14 0.9668865

rm(km_out, sil_out, x, y)

# ***************************
# Genera el cluster fiinal, 
base_centers <- 6 # ¿Cuántos clústers deseas crear?
k_model <- eclust(train_tbl, "kmeans", k = base_centers,
                 nstart = 2, graph = FALSE)

table(k_model$cluster)

# visualiza el silhouette de cada clúster
fviz_silhouette(k_model)

# otra visualización
fviz_cluster(k_model, geom = "point",  ellipse = FALSE)

# ************************
# visualización en 3D usando Plotly (otra librería)
train_tbl$cluster <- k_model$cluster
p <- plot_ly(train_tbl, x = ~arr_delay, y = ~dep_delay, z = ~distance,
        marker = list(color = ~cluster, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'array delay'),
                     yaxis = list(title = 'dep_delay'),
                     zaxis = list(title = 'distance')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Miles/(US) gallon',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p 


# ************************************
# Árbol de decisión

# Pasamos el identificador del clúster a los datos no estandarizados
summary_tbl$cluster <- k_model$cluster
set.seed(1234) # For reproducibility
tree_model <- C50::C5.0(summary_tbl[, 2:98], 
	as.factor(summary_tbl$cluster),
	control = C50::C5.0Control(winnow = FALSE, minCases = 20))
summary(tree_model)
plot(tree_model)


# ************************************
# revisión cluster
table(summary_tbl$cluster)

summary_gather_tbl <- summary_tbl %>% 
	select(company, cluster, colnames(summary_tbl)[2:98]) %>% 
	gather(key, value, 3:99) %>% 
	filter(is.na(value) == FALSE)


categories_tbl <- categories_tbl %>% 
	select(category_code, sub_category_code, category, sub_category) %>% 
	rename(key = sub_category_code)

summary_gather_tbl <- inner_join(summary_gather_tbl, categories_tbl, by = 'key')
summary_gather_tbl %>% 
	filter(cluster == 5) %>% 
	group_by(sub_category) %>% 
	summarize(total = sum(value)/1000000) %>%
	arrange(desc(total)) %>% 
	mutate(acum = (cumsum(total)/sum(total)) * 100) %>% 
	head(20)


summary_gather_tbl %>% 
	filter(cluster == 6) %>% 
	group_by(company) %>% 
	summarize(total = sum(value)/1000000) %>%
	arrange(desc(total)) %>% 
	mutate(acum = (cumsum(total)/sum(total)) * 100) %>% 
	head(20)
