# Programming Assignment 2: Caching the Inverse of a Matrix
####
####
# Matrix inversion is usually a costly computation.
# If the contents of the matrix are not changing in the different calculations,
# there may be some benefit to caching the inverse of a matrix so that when
# it is need again,it is looked up in the cache rather than computing it repeatedly.
####
####
# Programming Assignment 2 requires to write in R a pair of functions that 
# cache the inverse of a matrix:  
# 1. makeCacheMatrix 
# 2. cacheSolve
####
# These functions are used to create a special object that stores a matrix 
# and caches the inverse of the matrix.
####
#### 
# makeCacheMatrix - this function creates a special "matrix" object 
# that can cache its inverse.
# The cache operates in a similar manner as does the main memory (RAM), 
# but is smaller and has faster access. It is used by the central processing unit 
# to reduce the time access to data in main memory that are used most frequently.
####
# Input of makeCacheMatrix - the matrix
# Output of makeCacheMatrix - a list containing a function to: 
# 1.  set the matrix 
# 2.  get the matrix 
# 3.  set the inverse of the matrix 
# 4.  get the inverse of the matrix 
####
####
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
####
# cacheSolve - this function computes the inverse of the special "matrix" 
# returned by the function makeCacheMatrix above. 
# If the inverse matrix has already been calculated 
# (and the matrix has not changed), then the function cacheSolve 
# should retrieve the inverse from the cache and skip the calculation.
# Otherwise, it calculates the inverse matrix of the data. 
####
## Input - List output from the makeCacheMatrix
## Output - Inverse of the matrix from the input List 

####
####
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
####

# Example 1

> mat <-makeCacheMatrix()
> mat$set(matrix(1:4,2,2))
> mat$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(mat)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

# Example 2

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
> mat$set(x)
> mat$get()
[,1] [,2] [,3]
w1    1    2    1
w2    2    1    3
w3    4    1    5
> cacheSolve(mat)
w1    w2    w3
[1,]  0.5 -2.25  1.25
[2,]  0.5  0.25 -0.25
[3,] -0.5  1.75 -0.75

# Example 3

> mat$set(matrix(rnorm(9),3,3))
> mat$get()
[,1]       [,2]       [,3]
[1,] 0.03160031 -0.6543501 -0.8039862
[2,] 1.01299693 -0.5173176 -0.5948188
[3,] 1.17794732 -0.1140332 -0.2809336
> cacheSolve(mat)
[,1]      [,2]       [,3]
[1,] -0.6334859  0.753190  0.2182094
[2,]  3.4009234 -7.668384  6.5033291
[3,] -4.0366522  6.270765 -5.2843671

# Example 4

> mat$set(matrix(rnorm(16),4,4))
> mat$get()
[,1]          [,2]       [,3]       [,4]
[1,]  0.6503086  0.0006577193  1.3574458  1.5700283
[2,]  1.4580020  0.1438242966  0.9441227  0.6058667
[3,]  0.1595805  0.7347937186  0.6889564  0.7020681
[4,] -0.3248697 -1.2017201689 -1.3350666 -0.7466441
> cacheSolve(mat)
[,1]        [,2]       [,3]       [,4]
[1,] -0.4550656  0.93884933  0.9341282  0.6832885
[2,] -0.7625410  0.21114105  1.9000865  0.3545229
[3,]  0.6492420 -0.38914164 -3.3318300 -2.0834707
[4,]  0.2644053 -0.05250989  2.4929852  1.5182000
