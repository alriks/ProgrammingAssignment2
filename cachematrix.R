## These functions cache the inverse of a matrix. The first 
## function creates a matrix object that can cache its inverse.
## The second function computes the inverse of the special 
## matrix returned by makeCacheMatrix. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.

## The first function, makeCacheMatrix, creates a special 
## matrix which is a list containing a function to set the 
## value of the matrix, get the value of the matrix, set the 
## value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The second function calculates the mean of the special matrix 
## created above. It first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse 
## of the matrix and sets its value in the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}

