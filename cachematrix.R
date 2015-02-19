
# This R code has been produced by Meisam Booshehri
# The code has been written exacly according to the format of sample code in the assignment.
# The code consists of two functions in which the "superassignment" operator, i.e. "<<-", has been used
# in conjunction with the lexical scope, where an environment stores "state" for two functions 
# that modify the "state" by using superassignment.By "state", I am refering to the same Cache
# which is the objective of the assignment. The superassignment does the assignment in the 
# enclosing environment. That is, starting with the enclosing frame, it works its way up towards
# the global environment until it finds a variable called "m", and then assigns to it. 
# If it never finds an existing "m" it creates one in the global environment. 

# How To Use This R Code: To get an intuition of the code first run the code as below
#  1-In the console write: "source("cachematrix.R")
#  2- write: "initFunc<-makeCacheMatrix()"
#  3- Create your matrix, namely "myMat",  and write "initFunc$set(myMat)"
#  4- Then run "cacheSolve(initFunc)". if you run this instruction again you will see that the cache is working

# MakeCacheMatrix: In this function you can see a block-structured approach, i.e. there you can see some
# some functions defined in another function. 
# The set function will update the cache, i.e. "x" and "m". By setting a new matrix for computing the 
# the transpose, x will be updated by being assigned to the new matrix and m becomes NULL which is a sign 
# for the cacheSove function, meaning that the cache should be updated and the new transpose matrix should
# be assigned to "m".

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


# In the following function, i.e. cacheSolve, we keep track of any modification made to the the variable "m"
# If "m" is not NULL, it means that cache is up-to-date. But in case of setting a new matrix for computing
# the transpose, "m" becomes NULL and part of cache, i.e. "m",will be updated by cacheSolve.It will hold
# the transpose Matrix.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

  