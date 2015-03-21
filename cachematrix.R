## The file has two functions: makeCacheMatrix and cacheSolve. The idea is to
##given a square matrix calculate its inverse and buffer it, so that if we need
##to get this value in the future we can just retrieve instead of calculating it
##again.

##The function makeCacheMatrix makes a list given an input matrix x, using the 
# scoping rules to buffer thhe value of the inverse if the function has alreade 
##being called. 

#note the output of this function is a list that has among its components the
#value of the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
#note the output of this function is a list that has among its components the
#value of the matrix and the inverse.

        i <- NULL          #sets the initial value initial value of the inverse
        set <- function(y) {
                x <<- y   #since the function set is part of the output of makeCacheMatrix
                          # then x <<- y makes x equal to the matrix entered as
                          # argument in makeCacheMatrix; the use of <<- makes this
                          #change for the parent environment as well.
                i <<- NULL
                          }
        get <- function() { x } #just returns the input matrix x
        setInv <- function(Inv) { i <<- Inv
                                 } #assign the value Inv to i, note the use of <<-
                                   #to apply this assingment to the parent
                                   # environment
        getInv <- function() { i } # just returns the value of i from setInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

#this is the actual output of the function, a list with four arguments: i) the
#assingment of the argument to the input matrix, ii)the value of the input
# matrix, iii) the assignment of the value of i (the inverse of the matrix),
# iv) and the actual value of the inverse.


#The function cacheSolve uses the list constructed by the makeCacheMatrix and 
#calculates the inverse of the matrix (if it has not been done before) and 
#changes the values in makeCacheMatrix to store the value of the inverse. If
#the inverse has already being calculated it just retrieves its value.
cacheSolve <- function(x, ...) { #note that the argument of this function is the
                                #list obtained applying makeCacheMatrix to a
                                #(square) matrix.
        i <- x$getInv()         # set the value of i as the inverse (4th 
                                # component of the list)
        if(!is.null(i)) {       # this condition is TRUE if Inv is not NA
                message("getting cached data")
                return(i)   # this step returns the value of i previously obtained
                            # if the inverse was already calculate.
                        }
#In case the inverse has not been calculated, the else part of the statement applies.
      data <- x$get()         #this assing the first argument of the list (the
                                #actual matrix whose inverse we want to the variable
                                # "data"
        i <- solve(data, ...)   #calculate the inverse of the matrix.
        x$setInv(i)             #passes I thorugh the function "SetInv to cache
                                #the inverse calculated for future calls.
        i                       #returns the value of the inverse.
}