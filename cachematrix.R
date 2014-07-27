# Programming Assignment 2: Caching the Inverse of a Matrix
#### Programming Assignment 2: Caching the Inverse of a Matrix
#
# Matrix inversion is usually a costly computation.
# If the contents of the matrix are not changing in the different calculations,
# there may be some benefit to caching the inverse of a matrix so that when
# it is need again,it is looked up in the cache rather than computing it repeatedly.
#
# Programming Assignment 2 requires to write in R a pair of functions that 
# cache the inverse of a matrix:  
# 1. makeCacheMatrix 
# 2. cacheSolve
#
# These functions are used to create a special object that stores a matrix 
# and caches the inverse of the matrix.
####
##
#### 1.makeCacheMatrix 
# makeCacheMatrix - this function creates a special "matrix" object 
# that can cache its inverse.
# The cache operates in a similar manner as does the main memory (RAM),but is smaller
# and has faster access. It is used by the central processing unit to reduce the time
# access to data in main memory that are used most frequently.
#
# Input parameter - the matrix
# Output - a list containing a function to: 
# 1.  set the matrix 
# 2.  get the matrix 
# 3.  set the inverse of the matrix 
# 4.  get the inverse of the matrix 
#
#
makeCacheMatrix <- function(x = matrix()) {
  invx<-NULL
  set<-function(y){
    x<<-y
    invx<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) invx<<-inverse
  getinverse<-function() invx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
####
##
#### 2. cacheSolve
# cacheSolve - this function computes the inverse of the special "matrix" 
# returned by the function makeCacheMatrix above. 
# If the inverse matrix has already been calculated 
# (and the matrix has not changed), then the function cacheSolve should retrieve 
# the inverse from the cache and skip the calculation.
# Otherwise, it calculates the inverse matrix of the data. 
#
# Input - List output from the function makeCacheMatrix
# Output - Inverse of the matrix from the input List 
#
#
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  invx<-x$getinverse()
  if(!is.null(invx)){
    message("getting cached data")
    return(invx)
  }
  data<-x$get()
  invx<-solve(data,...)
  x$setinverse(invx)
  invx
}
####
##
####
# TEST EXAMPLES 
# This is a test examples to evaluate functions makeCacheMatrix and cacheSolve 
# Results is copied from RStudio Console, C:Users/EGW/ProgrammingAssignment2
####
####
# TEST EXAMPLE 1
#
# 1. Run both functions on Rstudio Console
#
# 2. Define a 2x2 matrix called x
#
> x <- matrix(1:4, ncol=2, nrow=2)
> x
     [,1] [,2]
[1,]    1    3
[2,]    2    4
#
# 3. Store the value of running makeCacheMatrix(x) which is a list.
#
> cachex <- makeCacheMatrix(x)
> cachex
$set
function (y) 
{
  x <<- y
  invx <<- NULL
}
<environment: 0x06178b70>
  
  $get
function () 
  x
<environment: 0x06178b70>
  
  $setinverse
function (inverse) 
  invx <<- inverse
<environment: 0x06178b70>
  
  $getinverse
function () 
  invx
<environment: 0x06178b70>
#
# 4. Compute, cache, and return the inverse of the matrix x
#
> cacheSolve(cachex)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
#
# 5. Show the original matrix stored
#
> cachex$get() 
     [,1] [,2]
[1,]    1    3
[2,]    2    4
#
# 6. Show the inverse of the matrix stored
#
< cachex$getinverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
#
# 7. Show the inverse of x called from cache
#
> cacheSolve(cachex)  
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
####
####
# TEST EXAMPLE 2
#
# 1. Run both functions on Rstudio Console
#
# 2. Define a 3x3 matrix called x
#
> w1<-c(1,2,1)
> w2<-c(2,1,3)
> w3<-c(4,1,5)
> x1<-rbind(w1,w2)
> x1
[,1] [,2] [,3]
w1    1    2    1
w2    2    1    3
> x<-rbind(x1,w3)
> x
[,1] [,2] [,3]
w1    1    2    1
w2    2    1    3
w3    4    1    5
#
# 3. Store the value of running makeCacheMatrix(x) which is a list.
#
> cachex <- makeCacheMatrix(x)
> cachex
$set
function (y) 
{
  x <<- y
  invx <<- NULL
}
<environment: 0x062823dc>
  
  $get
function () 
  x
<environment: 0x062823dc>
  
  $setinverse
function (inverse) 
  invx <<- inverse
<environment: 0x062823dc>
  
  $getinverse
function () 
  invx
<environment: 0x062823dc>
#
# 4. Compute, cache, and return the inverse of matrix x
#
> cacheSolve(cachex)
w1    w2    w3
[1,]  0.5 -2.25  1.25
[2,]  0.5  0.25 -0.25
[3,] -0.5  1.75 -0.75
#
# 5. Show the original matrix stored
#
> cachex$get() 
[,1] [,2] [,3]
w1    1    2    1
w2    2    1    3
w3    4    1    5
#
# 6. Show the inverse of the matrix stored
#
> cachex$getinverse()
w1    w2    w3
[1,]  0.5 -2.25  1.25
[2,]  0.5  0.25 -0.25
[3,] -0.5  1.75 -0.75
#
# 7. Show the inverse of x called from cache
#
> cacheSolve(cachex)  
getting cached data
w1    w2    w3
[1,]  0.5 -2.25  1.25
[2,]  0.5  0.25 -0.25
[3,] -0.5  1.75 -0.75
####
####
# TEST EXAMPLE 3
#
# 1. Run both functions on Rstudio Console
#
# 2. Define a 3x3 matrix called x
#
x<-matrix(rnorm(9),3,3)
> x
            [,1]       [,2]       [,3]
[1,]  0.03108588 -0.4332374 -1.6217800
[2,]  0.14989396 -0.3906885 -0.1119931
[3,] -1.23185053 -1.5456644 -1.2836094
#
# 3. Store the value of running makeCacheMatrix(x) which is a list.
#
> cachex <- makeCacheMatrix(x)
> cachex
$set
function (y) 
{
  x <<- y
  invx <<- NULL
}
<environment: 0x05935964>
  
  $get
function () 
  x
<environment: 0x05935964>
  
  $setinverse
function (inverse) 
  invx <<- inverse
<environment: 0x05935964>
  
  $getinverse
function () 
  invx
<environment: 0x05935964>
#
# 4. Compute, cache, and return the inverse of matrix x
#
> cacheSolve(cachex)
           [,1]       [,2]        [,3]
[1,]  0.3208982  1.9061323 -0.57174702
[2,]  0.3228294 -1.9912191 -0.23414877
[3,] -0.6966952  0.5684646  0.05159069
#
# 5. Show the original matrix stored
#
> cachex$get() 
            [,1]       [,2]       [,3]
[1,]  0.03108588 -0.4332374 -1.6217800
[2,]  0.14989396 -0.3906885 -0.1119931
[3,] -1.23185053 -1.5456644 -1.2836094
#
# 6. Show the inverse of the matrix stored
#
> cachex$getinverse()
[,1]       [,2]        [,3]
[1,]  0.3208982  1.9061323 -0.57174702
[2,]  0.3228294 -1.9912191 -0.23414877
[3,] -0.6966952  0.5684646  0.05159069
#
# 7. Show the inverse of x called from cache
#
> cacheSolve(cachex)
getting cached data
[,1]       [,2]        [,3]
[1,]  0.3208982  1.9061323 -0.57174702
[2,]  0.3228294 -1.9912191 -0.23414877
[3,] -0.6966952  0.5684646  0.05159069
####
####
