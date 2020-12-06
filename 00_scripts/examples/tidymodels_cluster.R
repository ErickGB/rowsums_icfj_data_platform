library(tidymodels)
library(tidyr)

set.seed(27)

centers <- tibble(
	cluster = factor(1:3), 
	num_points = c(100, 150, 50),  # number points in each cluster
	x1 = c(5, 0, -3),              # x1 coordinate of cluster center
	x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
)

centers %>% head()

labelled_points <- 
	centers %>%
	mutate(
		x1 = map2(num_points, x1, rnorm),
		x2 = map2(num_points, x2, rnorm)
	) %>% 
	select(-num_points) %>% 
	unnest(cols = c(x1, x2))

# graficamos los datos
ggplot(labelled_points, aes(x1, x2, color = cluster)) +
	geom_point(alpha = 0.3)

# extraemos el clúster
points <- 
	labelled_points %>% 
	select(-cluster)

# ******************
# k-means 

# hacemos el primer clúster, con 3 centroides
kclust <- kmeans(points, centers = 3)
kclust

augment(kclust, points)
tidy(kclust)
glance(kclust)


# determinar la cantidad de k
# Explorarmos diferentes acercamientos en la cantidad de k 
kclusts <- 
	tibble(k = 1:9) %>%
	mutate(
		kclust = map(k, ~kmeans(points, .x)),
		tidied = map(kclust, tidy),
		glanced = map(kclust, glance),
		augmented = map(kclust, augment, points)
	)

kclusts


# Each of these goes into a separate data set as they represent different types of data.
clusters <- 
	kclusts %>%
	unnest(cols = c(tidied))

assignments <- 
	kclusts %>% 
	unnest(cols = c(augmented))

clusterings <- 
	kclusts %>%
	unnest(cols = c(glanced))



# Now we can plot the original points
p1 <- 
	ggplot(assignments, aes(x = x1, y = x2)) +
	geom_point(aes(color = .cluster), alpha = 0.8) + 
	facet_wrap(~ k)
p1

# We can then add the centers of the cluster using the data from tidy():
p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

# Of particular interest is the total within sum of squares, saved in the tot.withinss column.
ggplot(clusterings, aes(k, tot.withinss)) +
	geom_line() +
	geom_point()

