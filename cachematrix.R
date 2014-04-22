##############################################################################
##
## KT
## Programming assignment 2
## R Programming Course (Coursera, April 2014)
##
## Contains a function that creates a special "matrix" that can cache its inverse
## And a second function that calculates or retrieves (depending on whether it
## exists already) the inverse of said "special matrix and returns that inverse
##
##############################################################################



## makeCacheMatrix creates a list that contains functions to:
##      1. set the value of a matrix
##      2. get the value of a matrix
##      3. set the value of an invserse of a matrix
##      4. get the value of an invserse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y #set matrix equal to matrix value
                m <<- NULL #set inverse to NULL
        }
        get <- function() x ## return matrix
        setinverse <- function(inverse) m <<- inverse #set inverse of matrix
        getinverse <- function() m #return the inverse of matrix
        
        #return list with functions 1-4 as listed above in large comment
        list(set = set, 
             get = get, 
             setinverse = setinverse,  
             getinverse = getinverse)
}




## cacheSolve returns the inverse of an invertable matrix.
## First, it checks if the inverse of the matrix has already been calculated
## If so, it "gets" this inverse and returns it. Otherwise, it calculates the 
## inverse, sets the value of the inverted matrix using the 'setinverse' 
## function, and returns the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse() #check if matrix inverse has already been calculated
        if(!is.null(m)) { # if inverse is already calculated
                message("fetching cached data")
                return(m) # return inverse and exit function
        }
        data <- x$get() # if matrix inverse hasn't been calculated, get matrix
        m <- solve(data, ...) # solve the inverse
        x$setinverse(m) # set the inverse
        m # return the inverse and exit function
}
