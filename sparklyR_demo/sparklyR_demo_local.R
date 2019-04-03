# Setting up Spark --------------------------------------------------------

library(sparklyr)
library(dplyr)

# Checks which spark versions are available
spark_available_versions()

# Checks which version is installed.
spark_installed_versions()

# Install spark - 2.4 is the latest available version
spark_install(version = "2.4")

# Opens the Spark connection through sparklyr
# Using standalone mode (local master)
mySparkConn <- spark_connect(master = "local")

# Checks whether the connection is opened
mySparkConn %>% connection_is_open()

# Checks Spark version
mySparkConn %>% spark_version()

# Checks Spark version (another way -- without pipeline)
spark_version(mySparkConn)


# Loading data and aggregation pipeline -----------------------------------

# Copy a local R data frame to Spark as SDF
mySparkConn %>% sdf_copy_to(mtcars, overwrite = TRUE)

# Sets up a connection to Spark table
myTbl <- mySparkConn %>% tbl("mtcars")

# Browse the top rows of the table
head(myTbl)

# Performs SQL-style aggregation using dplyr framework
# It runs natively on Spark
myTbl %>% 
  group_by(cyl) %>% 
  summarise(avg_mpg = mean(mpg, na.rm=TRUE))




# Reading / Writing files -------------------------------------------------

# Set up Spark Data Frame using CSV on local file system
# Works in Spark local mode only

# First, write the mtcars spark dataframe to csv
spark_write_csv(myTbl, 'mtcars_local', mode = "overwrite")

# change user to your username/lanid
myCsvLocal <- mySparkConn %>% 
  spark_read_csv(name = "mtcars_local",
                 path = "file:/home/user007/mtcars_local")
myCsvLocal

# You can also write and read parquet files
# write flights to parquet
spark_write_parquet(flights_tbl, 
                    path = "file:/home/user007/flights_local", 
                    mode = "overwrite")

# read it back in from parquet - change user to your username/lanid
myParquetLocal <- mySparkConn %>% 
  spark_read_parquet(name = "flightsnyc13",
                     path = "file:/home/user007/flights_local")

# have a look at myParquetLocal
head(myParquetLocal)





# Data visualisation ------------------------------------------------------
library(nycflights13)
library(ggplot2)
library(dbplot)

# Converts an R object into Spark DataFrame
flights_tbl <- mySparkConn %>% copy_to(flights)

# Runs dplyr pipeline on Spark
# It returns a Spark DataFrame
myFlightsSummary <- flights_tbl %>%
  group_by(year, month) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(year, month)
# Print it out on the console
myFlightsSummary

# Create a line plot
myLineplot <- myFlightsSummary %>%
  dbplot_line(month, mean(avg_dep_delay)) +
  labs(title = "Flights - average departure delay per month",
       ylab = "average departure delay") +
  scale_x_continuous(breaks= seq(1,12,1))
myLineplot

# Compute the histogram of a variable
myHistogram <- flights %>% filter(!is.na(air_time)) %>% 
  dbplot_histogram(air_time, binwidth = 50) +
  labs(title = "Flights - air time") +
  theme_light()
# Plot the histogram
myHistogram






# Machine learning algorithms ---------------------------------------------

# Remove NA data
# Divide the dataset into two halves (training + testing)
flights_tbl %>%
  na.omit() %>%
  sdf_partition(training = 0.8,
                testing = 0.2) %>%
  sdf_register(c("flights_training", "flights_testing"))

# Get a pointer object for the training and testing datasets
flights_training <- mySparkConn %>% tbl("flights_training")
flights_testing <- mySparkConn %>% tbl("flights_testing")

# Run a random forest model
# This can take some time
myFlightsModel <- flights_training %>%
  ml_decision_tree_regressor(dep_delay ~ month + hour + origin + dest + carrier)

# Run the model prediction on both training and testing set
myFlightsPredictionTraining <- myFlightsModel %>% ml_predict(flights_training)
myFlightsPredictionTesting <- myFlightsModel %>% ml_predict(flights_testing)


# Evaluate model performance (Mean-squared error, MSE)
ml_regression_evaluator(myFlightsPredictionTraining, 
                        label_col = "dep_delay",
                        metric_name = "mse")

ml_regression_evaluator(myFlightsPredictionTesting, 
                        label_col = "dep_delay",
                        metric_name = "mse")




# Linear regression -------------------------------------------------------

# Run linear regression locally in R
myModelRLocal <- lm(mpg ~ hp + wt + drat, mtcars)

# Run Spark MLlib models
myModelSparkLocal <- myCsvLocal %>% ml_linear_regression(mpg ~ hp + wt + drat)

# Compare the different linear models
# They should be identical
all.equal(myModelRLocal %>% coefficients(), 
          myModelSparkLocal %>% coefficients(),
          tolerance = 1e-8)




# Gradient Boosting Tree (GBT) --------------------------------------------

# Create training and testing split for mtcars dataset
myCsvLocal %>%
  na.omit() %>%
  sdf_partition(training = 0.5,
                testing = 0.5) %>%
  sdf_register(c("mtcars_training", "mtcars_testing"))

# Get a pointer object for the training and testing datasets
mtcarsTrain <- mySparkConn %>% tbl("mtcars_training")
mtcarsTest <- mySparkConn %>% tbl("mtcars_testing")

# Run a gbt model with type regression
mlGbtReg <- ml_gradient_boosted_trees(mtcarsTrain, mpg ~ hp + wt + drat,
                                      type = "regression")

# Predictions for testing and training
predtrainGbt <- ml_predict(mlGbtReg, mtcarsTrain)
predtestGbt <- ml_predict(mlGbtReg, mtcarsTest)

# Calculate the training and testing error (Mean-squared error)
ml_regression_evaluator(predtrainGbt, 
                        label_col = "mpg",
                        metric_name = "mse")

ml_regression_evaluator(predtestGbt, 
                        label_col = "mpg",
                        metric_name = "mse")


# You can also use GBT for classification problems
# compare the performance of GBT versus a logistic regression
# you can pre-define the formula if you want
# our outcome variable will be am (automatic or manual) instead of mpg
mlFormula <- formula(am ~ hp + wt + drat + mpg)

# Run a gbt model with type classification
mlGbtClass <- ml_gradient_boosted_trees(mtcarsTrain, mlFormula,
                                        type = "classification")

# Create the logistic regression model using the same formula
mlLogReg <- ml_logistic_regression(mtcarsTrain, mlFormula)

# Predictions for the testing set for both GBT and Logistic Regression
predTestClass <- ml_predict(mlGbtClass, mtcarsTest)
predTestLog <- ml_predict(mlLogReg, mtcarsTest)

# Let's create a function to calculate the accuracy of each model
calc_accuracy <- function(data, cutpoint = 0.5){
  data %>% 
    mutate(prediction = if_else(prediction > cutpoint, 1.0, 0.0)) %>%
    ml_multiclass_classification_evaluator("prediction", "am", "accuracy")
}

# Check the accuracy of each model
calc_accuracy(predTestClass)
calc_accuracy(predTestLog)

# Disconnect from Spark
spark_disconnect(mySparkConn)