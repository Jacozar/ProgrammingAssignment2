## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix()
## make funtions constructor that create a list with this 4 functions:
##      -set(y): that save the original matrix x in the list
##      -get(): return the matrix from the list
##      -setinv(inverse): that save the inverse in the list
##      -getinv(): return the inverse from the list
## This list will be my cache
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list( set = set, get = get, setinv = setinv, getinv=getinv)
        
}

## Write a short comment describing this function

## cacheSolve()
## Give a especial list 'x' and calculate the inverse if not exist in it
## 'x' is a list who made with 'makeCacheMatrix()' function

cacheSolve <- function(x, ...) {

        ## Get the matrix inverse from 'x' list (cache)
        ## If the value exist then return message, the inverse and finish
        m <- x$getinv()
        if(!is.null(m)) {
                message("Getting cached data & saving time !!!")
                return (m) 
        }
        
        ## This part is only executed if don't exist the inverse in the 'x' list
        
        data <- x$get()      ## load the matrix from 'x' list
        m <- solve(data,...) ## calculate the inverse with 'solve()' R function
        x$setinv(m)          ## save the inverse in the 'x' list (cache)
        m                    ## send the inverse to console and finish
}

