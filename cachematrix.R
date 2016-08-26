## Put comments here that give an overall description of what your
## functions do
## I have used the example code for this assignment provided by Prof. Peng and altered that to inverse a matrix instead of calculating the mean. My understanding of the code is based on that provided by Igreski https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md and all valuable contributions in the discussion forum.   
##The function cacheSolve is a fast function to return an inverse matrix of x. cacheSolve first searches in cache for stored computations from makeCacheMatrix. makeCacheheMatrix stores computed values in its environment insted of in each function child environment.    
## Write a short comment describing this function
## The makeCacheMatrix function includes four functions and two objects. MakeCacheMatrix returns the list of these function and objects which is the input to cacheSolve. "x" and "sol" is passed to the parent environment of the setx function (e.g. makeCacheMatrix environment) to be used as cache. 

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        setx <- function(y) {
                x <<- y
                sol <<- NULL
        }
        getx <- function() x
        setinver <- function(solve) sol <<- solve
        getinver <- function() sol
        list(setx = setx, getx = getx,
             setinver = setinver,
             getinver = getinver)
}


## Write a short comment describing this function
## cachSolve first check by x$getinver if input matrix is already computed. If cacheSolve retrieve the invers matrix in the cache the message is printed with the stored inverse matrix. Otherwise it computes the "new" input argument x$getx() from the  environment of makeCacheMatrix by solve. The new computation is stored in sol and update cache by x$setinver(sol) in the environment of makeCacheMatrix.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getinver()
        if (!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        matr <- x$getx()
        sol <- solve(matr, ...)
        x$setinver(sol)
        sol
}
