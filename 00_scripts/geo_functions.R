gc()
cat("\014")  
## ***************************************************************************************************
## Generales
## ***************************************************************************************************
## Creado por: Erick Gordon
## Fecha: 10-Jun-2018 
## Modificado:
## Project: Demographic data exploration (gis) 
## ***************************************************************************************************
## Shape Files Exploration
## ***************************************************************************************************
library(GISTools)
#library(OpenStreetMap)
library(rgdal)
require(FNN)

library(leaflet)
library(RColorBrewer)
#library(htmltools)

# Convert to GeoJSON
library(geojsonio)
# Simplify GeoJSON file
library(rmapshaper)
# test out the coordinates values
library(lawn)
require(spdep) # for spatial plot

# to clean data and eda analysis 
library(janitor)
## ******************************************
# Functions
## ******************************************

# get shape file to disck
get_shape_file <- function(path) {
	shape_map <- readOGR(path, verbose = FALSE)
	shape_map <- spTransform(neibours, CRS(paste("+proj=utm +zone=17 ellps=WGS84",sep='')))
	shape_map@data <- shape_map@data %>% 
		clean_names()
	
	return(shape_map)
}

# transform dataframe to spatial point
transform_to_spatial_points <- function(df_points, lon_colname, lat_colname) {
	coordinates(df_points) <- c(lon_colname, lat_colname)  
	proj4string(df_points) <- CRS("+proj=longlat +datum=WGS84 +units=m +no_defs") 
	sp_points <- spTransform(df_points, CRS(paste("+proj=utm +zone=17 ellps=WGS84",sep='')))
	
	return(sp_points)
}

# Convert shape file to geojson file ----
shape_to_geojson <- function(path_file, columns_selected, neighborhood_codes_scope, col_key="codigo", new_col_names=NA) {
	columns_selected <- toupper(columns_selected) 
	if(is.na(new_col_names)==TRUE)
		new_col_names <- columns_selected
	
	col_expr <- enquo(col_key)
	
	# load city shape file  
	city_shape <- readOGR(path_file, verbose = FALSE) # original, for intervencion project
	city_shape <- spTransform(city_shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
	city_shape <- city_shape %>% 
		filter(codigo %in% c(neighborhood_codes_scope %>% pull(codigo) %>% as.character()))
	
	# selected columns
	colnames(city_shape@data) <- toupper(colnames(city_shape@data))
	city_shape@data <- city_shape@data[, columns_selected]
	colnames(city_shape@data) <- new_col_names
	# Convert to GeoJSON
	shape_json <- geojson_json(city_shape) 
	# Simplify GeoJSON file
	shape_json <- ms_simplify(shape_json, keep = 0.1) # 5 Mb
	# Export GeoJSON file
	# geojson_write(shape_json, file = newFileName)
	return(shape_json)
}

convert_to_geojson <- function(city_shape) 
{
	# Convert to GeoJSON
	shape_json <- geojson_json(city_shape) 
	# Simplify GeoJSON file
	shape_json <- ms_simplify(shape_json, keep = 0.1) # 5 Mb
}


# plot interactive map  
plot_map <- function(levelShape, labels, zoomPlot=10, withLegend=FALSE)
{
	pal <- colorNumeric("Spectral", NULL) # viridis
	MBaccessToken <- "pk.eyJ1IjoiZXJpY2tnYiIsImEiOiJjaXZ1ZGl2NDcwNzg4MnpwN3Awbzlja2VsIn0.3JECiBB3CWrNOr87PuMizg"
	MBurlTemplate <- "https://api.mapbox.com/styles/v1/erickgb/ciwcz2pa8000i2qrwqcv9rtw9/tiles/256/{z}/{x}/{y}?access_token=" # "https://a.tiles.mapbox.com/v4/ibreckhe.map-z05003mi/{z}/{x}/{y}.png?access_token="
	MBTemplate <- paste(MBurlTemplate,MBaccessToken,sep="")
	
	shape <- leaflet(levelShape) %>%
		setView(lng = -79.518934, lat = 9.0201361, zoom = zoomPlot) %>%
		addTiles(MBTemplate) 
	
	# map 1
	shape <- shape %>% 
		addPolygons(stroke = FALSE, smoothFactor = 0.4, fillOpacity = 0.45,
								fillColor = ~VALUE,
								label = labels, 
								color = "gray",
								weight = 2,
								dashArray = "3",
								highlightOptions = highlightOptions(
									weight = 5,
									color = "#666",
									dashArray = "",
									fillOpacity = 0.90,
									bringToFront = TRUE), 
								labelOptions = labelOptions(
									style = list("font-weight" = "normal", padding = "3px 8px"),
									textsize = "15px",
									direction = "auto"))  
	
	if(withLegend==TRUE)
		shape <-  shape %>% 
		addLegend(pal = pal, values = ~(as.numeric(VALUE)), opacity = 1.0,
							labFormat = labelFormat(transform = function(x) round(10^x)))
	
	return(shape)
}

# *********************************************************************************************

# the range of Moran’s I varies with the W matrix.
# maximum and minimum values of I are shown (eigenvalue)
moran_range <- function(lw) {
	wmat <- listw2mat(lw) # generates a weights matrix for a neighbours list with spatial weights
	return(range(eigen((wmat + t(wmat)) / 2)$values))
}

# Autocorrelation: plot local moran I for significance diferences 
plot_local_moran <- function(x, variable.name, local.moran, weights, sig = 0.05, plot.only.significant = TRUE, legend.location = "bottomleft", zero.policy = NULL){
	if(!inherits(local.moran, "localmoran"))
		stop("local.moran not an object of class localmoran")
	if(!inherits(weights, "listw"))
		stop("weight not a listw")
	if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
		stop("MUST be sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT")
	
	
	# Check if local.moran subsetted missing data
	#x <- na.action(local.moran)
	na.act <- na.action(local.moran)
	
	if (is.null(na.act)) {
		# Rows to drop in weight matrix (weights)
		subVec <- !(1:length(weights$neighbours) %in% na.act)
		
		# Subset weights
		weights <- subset(weights, subVec, zero.policy = zero.policy)
		
		# Subset localmoran
		local.moran <- local.moran[subVec,]
		
		# Subset Polygons
		origPoly <- x
		x <- subset(x, subVec)
	}
	
	# Get length of x
	n <- length(x)
	
	#
	vec <- c(1:n)
	vec <- ifelse(local.moran[,5] < sig, 1,0)
	
	# Create the lagged variable
	lagvar <- lag.listw(weights, x[[variable.name]])
	
	# get the mean of each
	m.myvar <- mean(x[[variable.name]])
	m.lagvar <- mean(lagvar)
	
	myvar <- x[[variable.name]]
	
	# Derive quadrants
	q <- c(1:n) 
	
	for (i in 1:n){   
		if (myvar[[i]]>=m.myvar & lagvar[[i]]>=m.lagvar)
			q[i] <- 1
		if (myvar[[i]]<m.myvar & lagvar[[i]]<m.lagvar) 
			q[i] <- 2
		if (myvar[[i]]<m.myvar & lagvar[[i]]>=m.lagvar) 
			q[i] <- 3   
		if (myvar[[i]]>=m.myvar & lagvar[[i]]<m.lagvar) 
			q[i] <- 4
	}
	
	# set coloring scheme
	q.all <- q
	colors <- c(1:n)
	for (i in 1:n) {
		if (q.all[i]==1) 
			colors[i] <- "red"
		if (q.all[i]==2) 
			colors[i] <- "blue"
		if (q.all[i]==3) 
			colors[i] <- "lightblue"
		if (q.all[i]==4) 
			colors[i] <- "pink"
		if (q.all[i]==0) 
			colors[i] <- "white"   
		if (q.all[i]>4) 
			colors[i] <- "white"
	}
	
	# Mark all non-significant regions white
	locm.dt <- q*vec
	colors1 <- colors
	for (i in 1:n){
		if ( !(is.na (locm.dt[i])) )  {
			
			if (locm.dt[i]==0) colors1[i] <- "grey78"
			
		}
	}
	
	colors2 <- colors
	colors2 <- paste(colors2,vec)
	pos = list()
	for (i in 1:n) {
		pos[[i]] <- c(which(myvar==colors2["blue 0"]))
	}
	
	blue0 <- which(colors2=="blue 0")
	red0 <- which(colors2=="red 0")
	lightblue0 <- which(colors2=="lightblue 0")
	pink0 <- which(colors2=="pink 0")
	lb <- 6
	labels=c("High-High", "High-Low", "Low-High", "Low-Low", "Not Significant", "Missing Data")
	#plot the map
	#Plot out the full set of polygons (missing data will not be overlaid)
	plot(origPoly, col = "black")
	if (plot.only.significant == TRUE){
		plot(x, col=colors1,border=T, lwd=0.2, add = TRUE) 
	}else{
		plot(x, col=colors,border=T, add = TRUE)
	}
	legend(legend.location, legend = labels, fill = c("red", "pink", "lightblue", "blue", "grey78", "black"), bty = "n")
	
	return (origPoly)
	
}


# Moran's I Monte Carlo simulation
get_local_mora_mc <- function(shape_map, is_queen = TRUE, num_sim, value, print_plot = FALSE)
{
	# Step 1: we define “neighboring” polygons.
	neighboring_nb <- poly2nb(shape_map, queen = is_queen)
	# Step 2: assign weights to each neighboring polygon ----
	neighboring_lw <- nb2listw(neighboring_nb)
	
	# Monte Carlo simulation
	mora_mc<- moran.mc(value, neighboring_lw, nsim = num_sim)
	# Plot the distribution (note that this is a density plot instead of a histogram)
	if(print_plot == TRUE) plot(MC, main=NULL)
	return(moran_mc)
}

# Get lat/lon data, as character, from google using geo-coding feature
get_geocoding <- function(direcction) 
{
	# create 
	library(ggmap) # 
	result <- geocode(direcction, output = "latlona", source = "google")
	lon <- as.numeric(result[1])
	lat <- as.numeric(result[2])
	geo_position <- paste0(lon, ", ", lat)
	return(geo_position)
}


# count points in polygon
# parameters: DataFrame:points and polygon: shape_map, CRS = default is from panama city, panama 
get_count_points <- function(points, shape_map, long_name = "longitude", lat_name = "latitude", crs_name = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {
	# convert to SpatialPointsDataframe
	coordinates(points) <- c(long_name, lat_name)
	# Assign projection  
	proj4string(points) <-  CRS(crs_name) 
	points <- spTransform(points, CRS(paste("+proj=utm +zone=17 ellps=WGS84",sep='')))
	# Count points by each polygons
	points_tbl <- poly.counts(points, shape_map)
	points_tbl <- data.frame(points_tbl)
	points_tbl$codigo <- rownames(shape_map@data)
	colnames(points_tbl) <- c("count_points", "codigo")
	# return data.frame
	return(points_tbl)
}


# Get data frame with the code (key) of neighborhood for each point
# parameters: points and polygon
#points <- my_points[20001:20010, ]
#names <- my_points$id[20001:20010]
#shape_map <-  map_geored
#rm(points, names, shape_map)
get_spatial_intersection <- function(points, shape_map, names)
{
	original_points <- points
	original_points$id <- as.numeric(rownames(points))
	original_points$key_point <- names
	coordinates(points) <- c("longitude", "latitude")
	# Assign projection  
	proj4string(points) <-  CRS("+proj=longlat +datum=WGS84 +zone=17 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
	points <- spTransform(points, CRS(paste("+proj=longlat +datum=WGS84 +zone=17 +no_defs +ellps=WGS84 +towgs84=0,0,0",sep='')))
	#shape_map <- map_geored
	#rownames(shape_map@data) <- map_geored@data$codigo
	#plot(shape_map) 
	#plot(points, add = TRUE, pch = 1, col = "#FB6A4A4C")
	area_points <- rgeos::gIntersection(shape_map, points, byid=TRUE)
	points_tbl <- area_points %>% as.data.frame() 
	keys <- rownames(points_tbl)
	keys <- strsplit(keys, " ")
	area_key <- (sapply(keys, "[[", 1)) %>% as.numeric()
	id_key <- (sapply(keys, "[[", 2)) %>% as.numeric()
	
	final <- data.frame(id = id_key, area_map = area_key)
	final <- inner_join(final, original_points, by = "id")
	final <- final %>% 
		dplyr::select(key_point, area_map) 
	return(final)
}

get_spatial_join_panama <- function(points, data_raw, path_shapefile, cols_name, projection = "+proj=longlat +datum=WGS84 +zone=17 +no_defs +ellps=WGS84 +towgs84=0,0,0")  {
	map_shape <- readOGR(path_shapefile, verbose = FALSE)
	map_shape <- spTransform(map_shape, CRS(paste(projection,sep='')))
	map_shape@data <- map_shape@data %>% 
		clean_names()
	map_shape@data$codigo <- as.character(map_shape@data$codigo)
	map_shape@data$area_map <- as.numeric(rownames(map_shape@data))
	# join spatial points into the shape  ** very slow process ** 
	geo_intersection_tbl <- get_spatial_intersection(points, map_shape)
	# add area id to each point 
	data_raw <- left_join(data_raw, geo_intersection_tbl, by = "key_point") 
	# add area (polygon) name to points 
	data_raw <- left_join(data_raw, map_shape@data[, c(cols_name)], by = "area_map")
	return(data_raw) # return data joined wiht spatial code and name!!
}


# Search: K-NN  
# parameters: points A, and points array
# Nested Neighbor Search, with k=1, is spetial knn with Voronoi partition diagram - partition space.
# The CR algorithm is the VR using distance 1-x'y assuming x and y are unit vectors
# point_coa(155), point_others(929)
get_knn_points <- function(search_points, buffer_point, 
													 lon_colname = "longitude", lat_colname = "latitude") {
	buffer_point$id <- rownames(buffer_point)
	st_points_search <- transform_to_spatial_points(search_points, lon_colname, lat_colname) # busca los que están más cerca de este... es la base
	st_points_buffer <- transform_to_spatial_points(buffer_point, lon_colname, lat_colname) # listado donde se busca el más cerca. 
	st_nestered_points <- get.knnx(coordinates(st_points_search), coordinates(st_points_buffer), k=1, algorithm = "kd_tree") 
	st_nestered_points <- data.frame(st_nestered_points) # count is nrow(st_points_buffer): 929
	colnames(st_nestered_points) <- c("Index", "Distance") # index of search_point: 155
	# use index for search to nestered point 
	values <- sapply(1:nrow(st_nestered_points), function(x) search_points[st_nestered_points[x, c("Index")], c("point_name")])
	values_tbl <- data.frame(point_nestered_name=values)
	st_nestered_points <- cbind(st_nestered_points, values_tbl) # 
	# use index for search to nestered point 
	values <- sapply(1:nrow(st_nestered_points), function(x) search_points[st_nestered_points[x, c("Index")], c("point_name")])
	values_tbl <- data.frame(point_nestered_name=values)
	st_nestered_points <- cbind(st_nestered_points, values_tbl) # 
	return(st_nestered_points)
}


# Finding nestered point using a buffer area
get_points_in_buffer <- function(buffer_point, mts, search_points, 
																 lon_colname = "longitude", lat_colname = "latitude")
{
	st_points_1 <- transform_to_spatial_points(buffer_point, lon_colname, lat_colname) 
	st_buffer <- gBuffer(st_points_1, width=mts, byid=TRUE)
	
	st_points_2 <- transform_to_spatial_points(search_points, lon_colname, lat_colname) 
	
	# interseccion points vs buffer area
	point_intersection <-  get_spatial_intersection(st_points_2, st_buffer, names = ronames(st_points_2))
	return(point_intersection)
}


# Hotspot: 
