# Task 0 
print("Hello Everyone!")

# Task 1
v      <- c(2,3,4,5,1,5,3,8,1)
len    <- length(v)
middle <- v[ceiling(len/2):ceiling(len/2)+1]
1:3 + 1:10
1:3 + 1:9

# Task 2

df <- data.frame("number"     = c(1,2,3,4),
                 "alpha"      = c("a","b","c","d"),
                 "phys_constants"= c(2.998e08, 6.02e23, 8.85e-12,5.01e-11 )
)

print(df$number)
print(df[,1])

df$animals <- c("monkey","giraffe", "elephant", "hippotamus")
print(head(df))

print(df[1,])

#Task 3
square.it <- function(x){
  return(x*x)
}

square.it(df$number)

#Task 4

is.odd <- function(x){
  remainder <- x%%2
  equalOne  <- remainder == 1
  return(equalOne)
}

system.time(for(i in 1:20){
  if(is.odd(i)){
    print(i)
  }
})

# Task 5

vec <- 1:20
system.time(sapply(vec, function(x){ if(is.odd(x)){print(x)}}))

# Task 6
library(dplyr)
library(nycflights13)
head(as.data.frame(flights))

# Tidyverse examples
flight_times <- mutate(flights, dep_time_minutes = ( dep_time %/% 100 * 60 +dep_time%%100)%%1440)
