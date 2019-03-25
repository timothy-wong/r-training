library(sparklyr)
library(dplyr)

# check which spark versions are available
spark_available_versions()

# install spark - 2.4 is the latest available version
spark_install(version = "2.4")

# Opens the Spark connection through sparklyr
# Using standalone mode (local master)
mySparkConn <- sparklyr::spark_connect(master = "local")
# Checks whether the connection is opened
mySparkConn %>% connection_is_open()
# Checks Spark version
mySparkConn %>% spark_version()

# Copy a local R data frame to Spark as SDF
mySparkConn %>% sdf_copy_to(mtcars, overwrite = TRUE)
# Sets up a connection to Spark table
myTbl <- mySparkConn %>% tbl("mtcars")
# Browse the top rows of the table
head(myTbl)
# Performs SQL-style aggregation using dplyr framework
myTbl %>% 
  group_by(cyl) %>% 
  summarise(avg_mpg = mean(mpg, na.rm=TRUE))

# Generate a local tibble with 100000 rows and 30 columns
rows <- 100000
cols <- 30
myData <- matrix(rnorm(rows * cols), rows, cols) %>%
  as_tibble()
# Upload as partitioned SDF
myTblTest <- mySparkConn %>% 
  sdf_copy_to(myData,
              "myData_sdf",
              memory = TRUE,
              repartition = 10,
              overwrite = TRUE)



# data manipulation and visualisation with sparklyr
library(nycflights13)
mySparkConn <- spark_connect(master = "local")
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

# visualisation
library(dbplot)
library(ggplot2)

# create a line plot
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

# Remove NA data and divide the dataset into two halves
flights_tbl %>%
  na.omit() %>%
  sdf_partition(training = 0.5,
                testing = 0.5) %>%
  sdf_register(c("flights_training", "flights_testing"))
# Get a pointer object for the training and testing datasets
flights_training <- mySparkConn %>% tbl("flights_training")
flights_testing <- mySparkConn %>% tbl("flights_testing")
# Run a random forest model
# This can take some time
myFlightsModel <- flights_training %>%
  ml_random_forest(dep_delay ~ month + hour + origin + dest + carrier)
# Run the model prediction on both training and testing set
myFlightsPredictionTraining <- myFlightsModel %>% ml_predict(flights_training)
myFlightsPredictionTesting <- myFlightsModel %>% ml_predict(flights_testing)
# Calculate the training and testing error (Mean-squared error)
# Collect the result back to local memory at the end
myTrainingMse <- myFlightsPredictionTraining %>%
  mutate(error = prediction - dep_delay) %>%
  summarise(mse = mean(error^2, na.rm = TRUE)) %>%
  collect()
myTestingMse <- myFlightsPredictionTesting %>%
  mutate(error = prediction - dep_delay) %>%
  summarise(mse = mean(error^2, na.rm = TRUE)) %>%
  collect()
# Print the MSE
myTrainingMse
myTestingMse

## Set up Spark Data Frame using CSV on local file system
# Works in Spark local mode only

# first, write the mtcars spark dataframe to csv
spark_write_csv(myTbl, 'mtcars_local', mode = 'overwrite')

myCsvLocal <- mySparkConn %>% 
  spark_read_csv(name = "mtcars_local",
                 path = "file:/home/user012/mtcars_local")
myCsvLocal

# You can also write and read parquet files
# write flights to parquet
spark_write_parquet(flights_tbl, 'flights_local', mode = 'overwrite')
# read it back in from parquet
myParquetLocal <- mySparkConn %>% 
  spark_read_parquet(name = "flightsnyc13",
                     path = "file:/home/user012/flights_local")
# have a look at myParquetLocal
head(myParquetLocal)

# Run linear regression locally in R
myModelRLocal <- lm(mpg ~ hp + wt + drat, mtcars)
# Run Spark MLlib models
myModelSparkLocal <- myCsvLocal %>% ml_linear_regression(mpg ~ hp + wt + drat)
# Compare the different linear models
# They should be identical
all.equal(myModelRLocal %>% coefficients(), 
          myModelSparkLocal %>% coefficients(),
          tolerance = 1e-8)

# gradient boosting tree model
# create training and testing split for mtcars dataset
myCsvLocal %>%
  na.omit() %>%
  sdf_partition(training = 0.5,
                testing = 0.5) %>%
  sdf_register(c("mtcars_training", "mtcars_testing"))

# Get a pointer object for the training and testing datasets
mtcarsTrain <- mySparkConn %>% tbl("mtcars_training")
mtcarsTest <- mySparkConn %>% tbl("mtcars_testing")

# run a gbt model with type regression
mlGbtReg <- ml_gradient_boosted_trees(mtcarsTrain, mpg ~ hp + wt + drat,
                                    type = "regression")
# predictions for testing and training
predtrainGbt <- ml_predict(ml_gbt, mtcarsTrain)
predtestGbt <- ml_predict(ml_gbt, mtcarsTest)
# Calculate the training and testing error (Mean-squared error)
myTestingMseGbt <- predtestGbt %>%
  mutate(error = prediction - mpg) %>%
  summarise(mse = mean(error^2, na.rm = TRUE)) %>%
  collect()
myTrainingMseGbt <- predtrainGbt %>%
  mutate(error = prediction - mpg) %>%
  summarise(mse = mean(error^2, na.rm = TRUE)) %>%
  collect()
# Print the MSE
myTrainingMseGbt
myTestingMseGbt

# you can also use GBT for classification problems
# compare the performance of GBT versus a logistic regression
# you can pre-define the formula if you want
# our outcome variable will be am (automatic or manual) instead of mpg
mlFormula <- formula(am ~ hp + wt + drat + mpg)

# run a gbt model with type classification
mlGbtClass <- ml_gradient_boosted_trees(mtcarsTrain, mlFormula,
                                        type = "classification")

# create the logistic regression model using the same formula
mlLogReg <- ml_logistic_regression(mtcarsTrain, mlFormula)

# predictions for the testing set for both GBT and Logistic Regression
predTestClass <- ml_predict(mlGbtClass, mtcarsTest)
predTestLog <- ml_predict(mlLogReg, mtcarsTest)

# Let's create a function to calculate the accuracy of each model
calc_accuracy <- function(data, cutpoint = 0.5){
  data %>% 
    mutate(prediction = if_else(prediction > cutpoint, 1.0, 0.0)) %>%
    ml_multiclass_classification_evaluator("prediction", "am", "accuracy")
}
# check the accuracy of each model
calc_accuracy(predTestClass)
calc_accuracy(predTestLog)

# Disconnect from Spark
spark_disconnect(mySparkConn)
