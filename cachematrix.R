## Coursera Assignment Week 3
## Name: Praveen Kumar Pandey
## Date: 18-Mar-17
## Detail: To Create a special matrix object to cache its inverse
makeCacheMatrix <- function(x = matrix())  ## Define a Function with mCacheMatrix with default parameter as matrix()
{ 
        inv<-NULL               ## initialise the variable inv as NULL
        set<-function(y)        ## define the set function to assign new
        {   
                x<<-y           ##  Value of Matrix in Parent Environment
                inv<<-NULL      ##  if there is a new matrix, reset inv to NULL
        }  
        get <- function() x     ## define the get fucntion - returns value of the matrix argument                  
       setinverse <- function(inverse) ## assigns value of inv in parent environment
       inv <<- inverse                  ## gets the value of inv where called
       getinverse <- function() inv     ## We need this in order to refer                   
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## to the functions with the $ operator
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()     
        if(!is.null(inv)) 
        {         
                message("Get Cached Data")         
                return(inv)       
        }     
        data <- x$get()    
        inv <- solve(data, ...)       
        x$setinverse(inv)   
        inv  
}
