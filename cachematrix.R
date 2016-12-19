## The task in hand is to write functions in order to cache inverse of a matrix.
## 

## Below function creates a matrix object thact caches the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {     ## x is the input which is a square matrix
        
        im <- NULL                              ## im stands for inversematrix and in this step it is initialized to NULL with in the makeCacheMAtrix() environment to be used later
        set <- function(y){                     ## setter function with input argument y
                x<<- y                          ## assigns input argument y to the x object in parent enviornment
                im<<- NULL                      ## assigns NULL to the im object in parent enviornment , hence when x is resetted value of im in the memory is cleared
        }
        get <- function() x                     ## defines getter for the matrix x
        setsolve<- function(solve) im<<- solve  ## defines setter for the inverse of matrix 'im'
        getsolve <- function () im              ## getter for inverse of matrix 'im'
        list(set = set, get=get,                ## assigns each of the above functions as an element within a list()
             setsolve=setsolve,
             getsolve= getsolve)

}

## Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                ## argument to the function sould be of type makeCacheMAtrix()
        temp<- x$get()
        im <-x$getsolve()                       ## calls the function getsolve() from makeCacheMAtrix()
        if(!is.null(im)){                       ## checks if object im already has a cached inverse matrix, if already a cached matrix exist for the same input,im is taken from cache
                message ("Inverse already exist, getting cached inverse matrix")              
                return(im)
        }
        data <- x$get()                         ## calls function get() from makeCacheMatrix() to get the new matrix into object called 'data'
        im <-solve(data,...)                    ## solve function inverses matrix stored in data and stores it into im
        x$setsolve(im)                          ## calls setsolve() function from makeCacheMatrix()
        im                                      ## function cacheSolve returns inverse matrix as im
        ## Return a matrix that is the inverse of 'x'
}
