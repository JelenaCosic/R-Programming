# makeCacheMatrix creates a list containing a function to 
# - set the value of the matrix 
# - get the value of the matrix 
# - set the value of inverse of the matrix 
# - get the value of inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {  
mat <- NULL                         
get <- function() x  
setInv <- function(inv) mat <<- inv       
getInv <- function() mat  
list(get=get, setInv=setInv, getInv=getInv)   
}  


#The following function calculates the invers of the special 
#"matrix"  returned by makeCacheMatrix. 
#However, it first checks to see if the inverse has already 
#been calculated. If so, it gets the inverse from the cache 
#and skips the computation. 
#Otherwise, it calculates the inverse of the given matrix with the
# solve function and sets the 
#value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {  
mat <- x$getInv()  
if(!is.null(mat)) {  
  message("Getting cached data")
return(mat)                    
}  
else{  
data <- x$get()  
mat <- solve(data)  
 x$setInv(mat)  
return(mat)  
   }   
}  


# Running the code to see if everything looks fine and showing the output.

mat<-matrix(c(2,3/4,5,1,1/2,2/3,3,6,7), nrow=3, ncol=3)
print(mat)

#      [,1]      [,2] [,3]
#[1,] 2.00 1.0000000    3
#[2,] 0.75 0.5000000    6
#[3,] 5.00 0.6666667    7

mat1<-makeCacheMatrix(mat)
cacheSolve(mat1)

#           [,1]        [,2]        [,3]
#[1,] -0.02816901 -0.28169014  0.25352113
#[2,]  1.39436620 -0.05633803 -0.54929577
#[3,] -0.11267606  0.20657277  0.01408451
