## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()){
	#create a list to store matrix, it's inverse matrix, getinv method and setinv method
    m <- NULL
    #set function to store the matrix into list.
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    #get the matix from list
    get <- function() x
    #store inverse matrix into list
    setinv <- function(inv) m <<- inv
    #get inverse matrix from list
    getinv <- function() m
    #return the list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x){
	#if the inverse matrix has been calculated, just return it;
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cashed data")
        return(m)
    }
    data <- x$get()
    #check if the matrix is square.
    if(ncol(data)!=ncol(data)){
        message("Error: it is not a square matrix")
        return(NULL)
    }
    #check if the square matrix is inversible
    if(nrow(data)!=qr(data)$rank){
        message("Error: it is a singular square matrix")
        return(NULL)
    }
    #calculate the inverse matrix.
    m <- solve(data)
    #store the inverse matrix.
    x$setinv(m)
    #return result
    m
}
