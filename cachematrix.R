## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
    #myinverse is declared to hold the inverse matrix
    myinverse <- NULL

    #set function sets the value of mymatrix 
    set <- function(y) 
    {
      x <<- y
      # when a new matrix value is set, it clears the cached inverse value
      myinverse <<- NULL 
    }
    #get function gets the value of mymatrix 
    get <- function() { x }

    #input to the function is the matrix already solved, it is saved to local variable myinverse
    setSolved <- function(inverse) 
    { 
        myinverse <<- inverse #the input is saved to a local variable myinverse
    }
    
    #getSolve returns the matrix that was saved with setSolved
    getSolved <- function() 
    { 
        myinverse
    }

    
    list (
        set = set, 
        get = get,
        setSolved = setSolved,
        getSolved = getSolved
    )
}


## the cacheSolve function checks to see if there is already a cached inverse matrix and returns it
## else it continues and then solves the inverse, and stores it into x using x$setSolved() 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'. 
    #This value would have had to be previously set through x$setSolved()
    m <- x$getSolved()
    
    if(!is.null(m)) { #checks if matrix inverse is already solved, or if it is NULL
        message("getting cached data")
        return(m)
    }
    
    #data gets the matrix from x
    data <- x$get()
  
    #calculate the inverse using the solve function
    m <- solve(data, ...)
  
    #assign the calculated inverse into the value within the object
    x$setSolved(m)
    
    #function returns the solved matrix
    m
}
