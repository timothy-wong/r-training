library(profvis) 
profvis({ 
  # Generate some random numbers from normal distribution 
  myDataX <- rnorm(1e6)
  myDataY <- rnorm(1e6) 
  # Run a linear model 
  myModel <- lm(myDataY ~ myDataX) 
  # View the model summary
  summary(myModel) })
