## This code contains three functions
##
## makeCacheMatrix() creates an object of psuedo-type "CacheMatrix" in the form of a list that supports caching of a matrix and its inverse after the first time it is calculated
## cacheSolve() takes a CacheMatrix "object" and returns the cached inverse (if set) or calculates it (and caches it) if not set
## unit_test() performs repeatable tests to validate the function behavior

## Assignment requirements:

# Write the following functions:
#   
# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

## This function takes a matrix as input and creates a pseudo-object (methods and properties) in a list() which it returns
#    m_value - member variable that holds the value
#    m_inversion - member variable that holds the cached value
#    set_value - method to set the m_value and null the cache (if the assignment value is different from the previous value (or null))
#    get_value - method to get the m_value
#    set_cache - method to set the cache value
#    get_cache - method to get the cache value

makeCacheMatrix <- function(x = matrix()) {
  m_value <- x # Initialize the value member
  m_inversion <- NULL # Initialize to null the cached inversion
  
  set_value <- function(assignment_value) { # Create the set accessor
    if (!identical(m_value, assignment_value)) { # Only overwrite/reset the cache if the new value is different from the current
      m_value <<- assignment_value # Save the assigned value (matrix)
      m_inversion <<- NULL # And null the cached value (fulfilling the "and the matrix has not changed" requirement)
    } else if (is.null(assignment_value)) { # But if the new value is null, we want to null the cache regardless
      m_inversion <<- NULL
    }
  }
  get_value <- function() m_value # Create the get accessor
  set_cache <- function(inversion) m_inversion <<- inversion # Create the cache set accessor
  get_cache <- function() m_inversion # Create the cache get accessor
  
  list(set = set_value, get = get_value, setcache = set_cache, getcache = get_cache) # Create the object in the guise of a list
}


## This function takes as argument a pseudo-object of our CacheMatrix type and ellipses and returns the inverse of the matrix from cache
## if it is cached.  Otherwise the inverse is calculated and stored in the cache, and then returned.
## Though, in actuality, it does the logic a little differently than that.

cacheSolve <- function(x, ...) {
  if (is.null(x$getcache())) {
    message("cache is empty")
    
    x$setcache(solve(x$get(), ...))
  } else {
    message("cache is set")
  }
  
  x$getcache()
}

unit_test <- function(test_no, seed_val) {
  set.seed(seed_val)
  
  if (1 == test_no) { ## prove that matrix is the same given the set.seed()
    mtx <- matrix(rnorm(9), nrow = 3, ncol = 3)
  } else if (2 == test_no) { ## get the solved matrix, no caching involved
    mtx <- matrix(rnorm(9), nrow = 3, ncol = 3)

    mtx <- solve(mtx)
  } else if (3 == test_no) { ## prove that the solved matrix solves back to the original
    mtx <- matrix(rnorm(9), nrow = 3, ncol = 3)
    
    mtx <- solve(mtx)    
    mtx <- solve(mtx)    
  } else if (4 == test_no) {
    obj <- makeCacheMatrix(matrix(rnorm(9), nrow = 3, ncol = 3)) # Create the caching matrix object
    mtx <- cacheSolve(obj) # Print the inverse (this should be calculated)
  } else if (5 == test_no) {
    obj <- makeCacheMatrix(matrix(rnorm(9), nrow = 3, ncol = 3)) # Create the caching matrix object
    mtx <- cacheSolve(obj) # Get the inverse (this should be calculated)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
  } else if (6 == test_no) {
    obj <- makeCacheMatrix(matrix(rnorm(9), nrow = 3, ncol = 3)) # Create the caching matrix object
    mtx <- cacheSolve(obj) # Get the inverse (this should be calculated)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be cached)
  } else if (7 == test_no) {
    obj <- makeCacheMatrix(matrix(rnorm(9), nrow = 3, ncol = 3)) # Create the caching matrix object
    mtx <- cacheSolve(obj) # Get the inverse (this should be calculated)
    obj$set(mtx) # Update the object (this should reset the cache)
    mtx <- cacheSolve(obj) # Get the inverse again (this should be calculated back to the original)
  } else {
    print("unit_test(1..7, seed_value)")
    
    mtx <- NULL
  }
  
  print(mtx)
}
