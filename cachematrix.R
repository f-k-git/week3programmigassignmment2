makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list (set = set,get = get,getinv = getinv, setinv = setinv)
}
cacheSolve <- function(y,...){
        inv <- y$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- y$get()
        inv <- solve(data,...)
        y$setinv(inv)
        inv
}
# Above code will generate the following solution
#> m1 <- matrix(c(6,2,8,4),nrow = 2,ncol = 2)
#> my_matrixobject <- makeCacheMatrix(m1)
#> cacheSolve(my_matrixobject)
#      [,1]  [,2]
#[1,]  0.50 -1.00
#[2,] -0.25  0.75
