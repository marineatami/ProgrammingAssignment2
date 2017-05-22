## Return an inverse of an invertable matrix

## Initializes a matrix object that can cache the calculated inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  setCache <- function(y){
    x <<- y
    m_inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inverse) m_inverse <<- inverse
  get_inverse <- function() m_inverse
  
  list(set = setCache, get = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## Returns an inverse of a matrix 
## If the inverse has been calculated before returns the cached value 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inverse <- x$get_inverse()
  if(!is.null(m_inverse)){
    message("Inverse of this matrix has been already calculated")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data, ...)
  x$set_inverse(m_inverse)
  m_inverse
}