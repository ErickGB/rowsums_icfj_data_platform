# load library
library(ggplot2)

# Create test data.
data <- data.frame(
	category=c("A", "B", "C", "D", "E"),
	count=c(10, 45, 25, 15, 5)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
	geom_rect() +
	geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
	scale_fill_brewer(palette=3) +
	scale_color_brewer(palette=3) +
	coord_polar(theta="y") +
	xlim(c(-1, 4)) +
	theme_void() +
	theme(legend.position = "none")



library(rjson)
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))
reporters %>% 
	filter(V2 %in% c("Panama"))




