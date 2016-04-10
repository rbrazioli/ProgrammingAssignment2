##
## @name CacheMatrixInverse
## @author Renato Brazioli \email{renato.brazioli@@gmail.com}

## @note As a statistician, I want to be able to compute the inverse of a 
## matrix and cache the result, in order to save time and computastional 
## resources

## @name makeChacheMatrix
## @param baseMatrix, the input matrix to be stored and for which
## the inverse will be cached
## @note receive in input a square matrix (the matirx is assumed to be
## invertible) and store in an "object", holding also the inverse.
## at assignement time, the inverse is not computed, it will be done at the 
## "last responsible  moment", only when is needed.

makeCacheMatrix <- function(baseMatrix = matrix()) {
##
## allocate and initialize to NULL the container for the the inverted matrix 
## cache
  
    inverseMatrix <- NULL
    
##
## define accessor functions
##
    set <- function(newMatrix) {
      baseMatrix <<- newMatrix
      inverseMatrix <<- NULL # set cache to NULL whenever data change
    }

    get <- function() baseMatrix

    setInverted <- function(Inverted) {
      inverseMatrix <<- Inverted
    }

    getInverted <- function() inverseMatrix
    
##
## define and return the list containing accesor functions
##
    list(
      set = set, 
      get = get,
      setInverted = setInverted,
      getInverted = getInverted
      )
    
  } ## makeCacheMatrix

## @name chacheSolve
## @param CachedMatrix, the matrix to be inverted
## @note if availòable, the cached inverse matrix will be used
##

cacheSolve <- function(CachedMatrix, ...) {
  
  inverseMatrix <- CachedMatrix$getInverted() #get cached inverse, if available
  
  if(is.null(inverseMatrix)) { # no cache is available
    inverseMatrix <- solve(CachedMatrix$get(), ...)
    CachedMatrix$setInverted(inverseMatrix)
  }
  else { #just notify that cache is being used
    message("getting cached data") 
  }

   inverseMatrix

} ## cacheSolve
