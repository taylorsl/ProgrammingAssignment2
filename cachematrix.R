
# makeCacheMatrix returns a vector which contains the list that will be passed to cacheInv
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the Inverse
# 4. get the value of the Inverse
# Inv stores the inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL                           
    set <- function (y) {
        x <<- y
        Inv <<- matrix()
    }
    get <- function () x
    setinv <- function (solve) Inv <<- solve
    getinv <- function () Inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)

}


# cacheSolve calculates the Inverse of a matrix
# If the inverse already exists in cache, then it passes through the existing value
# Input variable is the special list created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$getinv()
    
    if (!is.null(Inv)){
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
  
    Inv <- solve (data, ...)
    x$setinv(Inv)
    Inv
    
}
