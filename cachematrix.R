##This functions cache the inverse of input matrix

#makeCacheMatrix function sets and gets the value of a matrix and
#sets and gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  #sets a default value for m
        set <- function(y) {    #sets a default matrix
                x <<- y         # cache input matrix
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, #creats a list of four functions
             setmatrix = setmatrix, 
             getmatrix = getmatrix)
}

#cacheSolve function calculates the inverse of special matrix created with above function
#It first checks if inverse is already stored in memory

cacheSolve <- function(x=matrix(), ...) { 
        m <- x$getmatrix() # if an inverse is stored in m, it gets that value
        if(!is.null(m)) {  # checks if m is already stored in cache
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()  #gets the value of input matrix
        m <- solve(matrix, ...)  #inverse matrix calculation
        x$setmatrix(m) #cache the inverse of matrix
        m
}
