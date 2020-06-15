if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
	if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}


install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))





library(h2o)
h2o.init()

# Import the iris dataset into H2O:
iris <- h2o.importFile("http://h2o-public-test-data.s3.amazonaws.com/smalldata/iris/iris_wheader.csv")

# Set the predictors:
predictors <- c("sepal_len", "sepal_wid", "petal_len", "petal_wid")

# Split the dataset into a train and valid set:
iris_split <- h2o.splitFrame(data = iris, ratios = 0.8, seed = 1234)
train <- iris_split[[1]]
valid <- iris_split[[2]]

# Build and train the model:
iris_kmeans <- h2o.kmeans(k = 10,
													estimate_k = TRUE,
													standardize = FALSE,
													seed = 1234,
													x = predictors,
													training_frame = train,
													validation_frame = valid)

# Eval performance:
perf <- h2o.performance(iris_kmeans)

# Generate predictions on a validation set (if necessary):
pred <- h2o.predict(iris_kmeans, newdata = valid)

h2o.shutdown()