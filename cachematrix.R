## The makeCacheMatrix function receives a matrix object as a parameter and builds a special matrix object that can 
## save a matrix and its respective inverse.
##
## This special object is a list of 4 functions:
## 1. set(y) -> Receives a matrix as a parameter and saves it inside the object.
## 2. get() -> Returns the matrix saved inside the object. 
## 3. setinverse(inverse) -> Receives a matrix and saves it inside the object (cache).
## 4. getinverse() -> Returns the inverse of the matrix if it's saved inside the object (cache).

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set <- function(y) {
                x<<-y
                m<<-NULL
        }
        get <-function() x
        setinverse <- function(solve) m<<-solve
        getinverse <- function() m
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## Write a short comment describing this function
## The cacheSolve function returns the inverse of the special matrix object inside the makeCache function.
##
## If the inverse of the matrix is already stored in the object as a cache, cacheSolve() prints a message
## specifying it's getting cached data and quickly returns it.
##
## If, however, the inverse of the matrix isn't stored in the object, the function solve() is utilized to calculate
## the inverse of the matrix inside x, being promptly cached inside the object. cacheSolve() then returns the inverse
## of the matrix.

cacheSolve <- function(x,...) {
        m<-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}

