## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## "makeCachematrix is a function which will create a matrix that will cashe its inverse for the input.
makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y){
               x <<- y
               inv <<- NULL
       }
                
       get <- function() x
       setinverse <- function(inverse) inv <<- inverse
       getinverse <- function() inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve is a function which will create an inverse of the matrix created by function makeCachematrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
## In order to test the functionality:
## 1. Create a square matrix "ABC" and execute it
## 2. Create a new object "XYZ" that's linked to makeCachematrix(ABC)
## 3. Execute cacheSolve(XYZ)
}
