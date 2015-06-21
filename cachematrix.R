## makeCacheMatrix:
##1. Sets a matrix;
##2. Gets its values;
##3. Sets up a variable for the matrix inverse; 
##4. Gets the values for matrix inverse
## Above thus saves the matrix in the cache memory 
## cacheSolve:
## 1. Tries to find if the matrix inverse already exists
## 2. If yes, retrieves those values
## 3. If not, computes the inverse of the matrix
## 4. Sets up a variable and assigns the inverse computed to that variable

## Write a short comment describing this function

## Function to commit a matrix saved in the global environment to cache memory

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL			#Null variable for the inverse
  set <- function(y) {
    x <<- y			#Calling out the variable from the global environment into cache
    m <<- NULL
  }
  get <- function() x #Getting the matrix from the cache
  setsolve <- function(inverse) m <<- inverse #Setting the inverse for the cache
  getsolve <- function() m	#Getting the inverse from the cache
  list(set = set, get = get,	#Creating a list that consists all the above and returning it
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

## Function to compute(if required) the inverse of a matrix in the cache memory
## and in turn save the inverse in the cache as well

cacheSolve<- function(x, ...) {
  m <- x$getsolve()  #Retrieving if the inverse is available in the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #Computing the inverse if the corresponding variable in the cache is empty
  x$setsolve(m)
  m
}
