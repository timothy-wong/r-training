library(pryr) # Displays the size of an integer value 
# Note the trailing 'L' character, this forces the number to become an integer object_size(3L)
# Significant figure of a numeric valule has no effect on object size 
# Therefore these two values have the same object size object_size(3.14)

object_size(3.14159265359)

# Creates an integer vector 
# This contains 1 million integers
# Consumes 4 MB memory 
myBigVec1 <- 1:1e6

object_size(myBigVec1)


# Both character objects have identical object size. 
# Each consume 120 bytes. 
object_size("My name is Bond.")

object_size("My name is Bond, James Bond.")


#Mem use of the same type
object_size(c("Yes", "No", "No", "Yes"))

object_size(c(TRUE, FALSE, FALSE, TRUE))

object_size(c(1,0,0,1))

# total mem in global envrionment 
mem_used()

# Assign a large vector 
# This will consume memory (+4 MB) 
mem_change({ myBigVec2 <- 1:1e6 })

# Remove a large vector
# This releases memory (-4 MB) 
mem_change({ rm(myBigVec2) })
