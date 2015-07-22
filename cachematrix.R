## The makeCacheMatrix provides an independent enviourment that saves the matrix and its inverse matrix
## For example once create a matrix and its inverse, we can call the makeCacheMatrix$set to set the matrix,
## we can also call $setinverse to set the inverse matrix by passing the calculated inverse to it
## Once we setup the values by using makeCacheMatrix functions, we can call cacheSolve to calculated the inverse,
## if the value has been saved, we can directly use the saved value rather than calculate it again.

## Form a function list to set and get the matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix(,nrow = nrow(x), ncol = ncol(x))
        set <- function(y = matrix()) {
                x<<-y
                inv <<- matrix(NULL,nrow = nrow(x), ncol = ncol(x))
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## This function firstly test if there is value that saved in inv, if there is then 
## return the saved inverse matrix, if not, get the matrix first then calculate the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
}
