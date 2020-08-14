install.packages("tidyverse")  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr, ...
install.packages("readr")      # load data
install.packages('devtools')
install.packages("DataExplorer")
install.packages("janitor")
install.packages("refinr") # Cluster and merge similar char values: an R implementation of Open Refine clustering algorithms


install.packages("EpiModel")
install.packages(c("earlyR", "incidence"))

# shiny
install.packages("shiny") 
install.packages("shinydashboard") # dashboard structure
install.packages("shinythemes") # shiny themes
install.packages("shinyjs")
install.packages("ggthemes")   # ggplot extra themes
install.packages("plumber")    # for create web service REST with R
install.packages("rvest")      # for web scrawling 
install.packages("anytime")    # Convert input in any one of character, integer, numeric, factor, or ordered type into 'POSIXct'
install.packages("vroom")      # The goal of 'vroom' is to read and write data (like 'csv', 'tsv' and 'fwf') quickly. 

# plots
install.packages("GGally")
devtools::install_github('bbc/bbplot') # plots style by BBC 
#devtools::install_github('hadley/ggplot2')
install.packages("plotly")
install.packages("networkD3")
install.packages("treemap")
install.packages("corrplot")
install.packages("RColorBrewer")


install.packages("roxygen2")
install.packages("anomalize", dependencies = TRUE) # outliers detection
install.packages("caTools")
install.packages("DMwR")      # SMOTE function for balance data 
install.packages("recipe")    # Creating ML preprocessing recipes

install.packages("lime")       # ML local interpretation
install.packages("vip")        # ML global interpretation
install.packages("pdp")        # ML global interpretation
install.packages("caret")      # ML model building
install.packages("DALEX")      # Machine Learning models are widely used and have various applications in classification or regression 
install.packages("VGAM")
install.packages("vtreat")

install.packages("tictoc")     # Functions for timing R scripts, as well as implementations of Stack and List structures.

install.packages("unpivotr")
install.packages("tidyxl")
devtools::install_github("nacnudus/smungs") # excel treatment library - data wrangling

# google cloud platform
install.packages("bigrquery")
install.packages("googleCloudStorageR")
install.packages("googledrive")
install.packages("gargle")

install.packages("Rserve")
install.packages("mvoutlier") #outliers


# Geo and plots 
install.packages("protolite")
install.packages("geojson")

install.packages("OpenStreetMap")
install.packages("PBmapping")
install.packages("RPostgres")
install.packages("spdplyr") #  makes it possible to use the main dplyr verbs
install.packages("geojsonio", dependencies = TRUE) # Convert to GeoJSON
install.packages("rmapshaper") # Simplify GeoJSON file
install.packages("lawn") # view out the coordinates values
install.packages("classInt") # jenks intervals
install.packages("GISTools")
install.packages("leaflet")
# spatial entropy 
install.packages("SpatEntropy")
install.packages("googleway") # Google API keys
install.packages("tidytransit") # Google GTFS (paradas)
install.packages("FNN")
# ********************************************
# in terminal
# sudo add-apt-repository -y ppa:opencpu/jq
# sudo apt-get update
# sudo apt-get install libjq-dev

install.packages("jqr")
install.packages("googleway") # google places 
# ********************************************

# analysis
install.packages("FactoMineR") # correspondence analysis an CP analysis
install.packages("ade4") # dudi.coa
install.packages("factoextra")
install.packages("h2o")
install.packages("C50")
install.packages("rpart")
install.packages("rpart.plot")


# mlflow for model tracking
install.packages("mlflow")
mlflow::install_mlflow(python_version = "3.6")

# string
install.packages("stringdist")
# stringdist("abc","abcd", method = "lv")

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite", "statmod")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}
# Now we download, install and initialize the H2O package for R.
#install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
#install.packages("h2o")        # ML model building

pkgs <- c(
    #"h2o",        # High performance machine learning
    #"lime",       # Explaining black-box models
    #"recipes",    # Creating ML preprocessing recipes
    #"tidyverse",  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr, ...
    "tidyquant",  # Financial time series pkg - Used for theme_tq ggplot2 theme
    "glue",       # Pasting text
    "cowplot",    # Handling multiple ggplots
    #"GGally",     # Data understanding - visualizations
    "skimr",      # Data understanding - summary information
    "fs",         # Working with the file system - directory structure
    "readxl",     # Reading excel files
    "writexl"     # Writing to excel files
)

#install.packages(pkgs, dependencies=TRUE)
#install.packages("rsample")
#install.packages("RJSONIO", dependencies=TRUE)
#install.packages("jsonlite", dependencies=TRUE)
install.packages("rjson", dependencies=TRUE)
#install.packages("bookdown") # for markdown presentation 


# or with devtools: Explore and Visualize Your Data Interactively
devtools::install_github("dreamRs/esquisse")
options("esquisse.display.mode" = "browser")
library(esquisse)
esquisser()

#-- install docker 
#sudo apt install docker.io

#-- Pull the image:
#  sudo docker pull scrapinghub/splash

#-- Start the container:
#  sudo docker run -it -p 8050:8050 --rm scrapinghub/splash

# install libraries in R
devtools::install_github("wch/harbor")
devtools::install_github("hrbrmstr/splashr") 
install.packages("magick")

if (!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}
remotes::install_github("thomas-neitmann/ggcharts", upgrade = "never")


install.packages("distill")
install.packages("hrbrthemes")
install.packages("sodium")

remotes::install_github("reconhub/epicontacts")
remotes::install_github("reconhub/linelist")
remotes::install_github("reconhub/earlyR")
remotes::install_github("reconhub/projections")

install.packages("gt")



https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver15
sudo su
curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
  
#Download appropriate package for the OS version
#Choose only ONE of the following, corresponding to your OS version
#Ubuntu 16.04
curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list > /etc/apt/sources.list.d/mssql-release.list
sudo apt-get update
sudo ACCEPT_EULA=Y apt-get install msodbcsql17
sudo apt-get install unixodbc-dev



