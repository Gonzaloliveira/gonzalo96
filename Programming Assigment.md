# gonzalo96
Coursera R Programming Assigment


# Media vector

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

aVector$set(1:4)          # reset value with a new vector
cachemean(aVector)          # notice mean calculated is mean of 30:50, not 1:10
aVector$getmean()           # retrieve it directly, now that it has been cached


# Inverse matrix


makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  get <- function() m
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(m, ...) {
  inverse <- m$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setInverse(inverse)
  inverse
}

matriz <- matrix(c(3:10, 10), 3, 3)
cacheSolve(makeCacheMatrix(matriz))



