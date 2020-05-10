## Put comments here that give an overall description of what your
## functions do
#Overview:
#Create two functions, the first which creates a special matrix that can cache it's inverse
#the second which computer the inverse of this special matrix, but only calculates if it it has not already been calculated 
## Write a short comment describing this function

#This functions nests four functions within one so they can all be stored
#The <<- operator means it will gradually look upwards in the environment hierarchy to find the value, 
#allowing us to save values to the global environment from within a function when it is ran
#This lets us access these values, and these functions, in the latter half
#We are trying to accomplish basically the same as the makeVector function, but the inverse,
#meaning we only need to:
#1)Change 'm' to 'inv'
#2)Change mean to Inverse
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) n <<- inverse
  getInverse <- function() n
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#This calls the getmean function nested above, and will calculate it only if m is not null, i.e.
#cachemean(myVector) (or something of the like) has not already been run
#If it had already been run, m!=NULL, and it would call from global environment (cache)
#Again, we only need to...
#1)Replace 'm' with 'n' which represents the inverse
#2)Change data to a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse()
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  matix <- x$get()
  n <- solve(matix, ...)
  x$setInverse(n)
  n
}

##############TESTING########################
#Testing the function as recommended here: https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1
m1 %*% n1 
n1 %*% m1
solve(m1)
solve(n1)

myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)#Should equal n1
n1
#Does equal n1
#Call it again- should retrieve cached results
cacheSolve(myMatrix_object)
#Does retreieve cached result
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
#Check we can use set() commands to change values of the matrix
myMatrix_object$set(n2)
#CHeck that when we re-run this, as it has been set() it should no longer have a cached value until we run it again
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
#COrrect- is cached second time and not first
