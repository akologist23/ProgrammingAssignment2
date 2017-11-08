## Summary: makeCacheMatrix() creates an empty cache (list) for future storage of the inverse of a matrix. It does
## store the raw matrix for retrieval but does not calculate the inverse of the matrix.
## Summary: cacheSolve() interfaces with that cache (elements of the list) to calculate and store the inverse
## of a matrix only once, and then in subsequent calls on the cache, retrieves the calculated values without
## performing further calculations.



#Individual functions::::

## makeCacheMatrix() creates a cache for the inverse of a matrix. In particular,
## this function creates a list ("special vector") of named arguments which are 
## functions that allow one to interface with the cache.
## Thus, the cache can 
## (1) return the raw matrix (makeCacheMatrix$getmat()),
## (2) can receive a new matrix (makeCacheMatrix$setmat([new matrix])),
## (3) can receive an inverse matrix (makeCacheMatrix$setinv([inverse matrix])) 
##           and assign it to the "cache" (inv.mat <- an object)
## (4) can return the inverse matrix once cached 
##          (makeCacheMatrix$setinv([inv.mat]))
## Note1: running makeCacheMatrix() with a matrix will reset the cached inversed matrix (inv.mat <- NULL)
## Note2: using makeCacheMatrix$setmat() to load a new matrix will also reset the cached inversed matrix (inv.mat <- NULL)

makeCacheMatrix <- function(x = matrix()) {
    inv.mat <- NULL
    setmat <- function(y) {
            x <<- y                    #Assignment in new environment
            inv.mat <<- NULL           #Assignment in new environment
        }    
    getmat <- function(){
            x
        }   
    setinv <- function(solve){
            inv.mat <<- solve   
            }
    getinv <- function(){
            inv.mat    
        }
        list(setmat = setmat, getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve interacts with the output (=cache) returned by makeCacheMatrix().
## This function first checks whether there is an inverse matrix in "getinv", which is
## a named element of the output. The first time it checks the output (cache), with an
## if else statment, no inverse matrix exists/is in the cache. Thus, the function procedes to:
## (1) Retrieve the raw data matrix from the output list (x$getmat()) and assigns it to "data";
## (2) It calculates the inverse of the matrix using solve() and assigns it to "inv.mat";
## (3) The inverse matrix (inv.mat) is assigned to the "setinv" element "x$setinv" from the
## output list. 
## The second time the output (cache) is run with cacheSolve, an inverse matrix does exist (!is.null) 
## and is returned along with the message "getting cached data". The inverse matrix is not calculated
## the second time.

cacheSolve <- function(x, ...) {
    inv.mat <- x$getinv()
    if(!is.null(inv.mat)) {
        message("getting cached data")
        return(inv.mat)
    } 
    else if (is.null(inv.mat)){
    data <- x$getmat()
    data
    inv.mat <- solve(data, ...)
    inv.mat
    x$setinv(inv.mat)
    inv.mat ## Return a matrix that is the inverse of 'x'
    }  
}



###Test run

#Create test matrix (square and nonsingular)
a <- c(23,234,25,34,44)
b <- c(888,675,456,45,456)
c <- c(23, 65, 87, 90, 143)
d <- c(6, 3, 3, 2, 8)
e <- c(334,35, 376, 55, 2)
testmatrix <- cbind(a,b,c,d,e)


x.out<-makeCacheMatrix(testmatrix)
cacheSolve(x.out)
cacheSolve(x.out)


