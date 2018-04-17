## makeCacheMatrix() : Creates a matrix data type and returns a list containing its inbuilt functions
##	It has 4 inbuilt functions setm() , getm() , setinv(), getinv()
##		setm() : resets the matrix to new values
##		getm() : gets the matrix values
##		setinv() : stores the inverse in memory
##		getinv() : gets the inverse from the memory cache

makeCacheMatrix <- function(x = matrix()) {
        
        ## First, inverse is set to NULL
        inv <- NULL
        
        
        
        ## setm() : resets the matrix to new values
        setm <- function(matx) {
                x <<- matx
                inv <<- NULL
        }
        
        
        
        ## getm() : gets the matrix values
        getm <- function() x
        
        
        
        ## setinv() : stores the inverse in memory
        setinv <- function(INVS) inv <<- INVS
        
        
        
        ## getinv() : gets the inverse from the memory cache
        getinv <- function() inv
        
        
        
        ## The 4 functions are returned by as list elements
        list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
}




## cacheSolve() works as follows
##      (1)  First it checks if inverse exists, and returns it if inverse exists
##      (2)  If inverse doesnot exist, it checks if it is a square-matrix
##      (3)  If it is not a square matrix, then inverse is not calculated.
##      (4)  If it is a square-matrix, then inverse is calculated and stored in memory.

cacheSolve <- function(x, ...) {
        
        
        ## First check whether inverse exists or not
        
        IN <- x$getinv()
        if(!is.null(IN)) {
                message("getting cached data")
                return(IN)
        }
        
        
        
        
        ## Now check whether the matrix is square-matrix or not
        ## This is done by getting the matrix diemensions, using dim()
        
        DIMS <- dim(x$getm())
        if(DIMS[1] != DIMS[2]) {
                message("Matrix diemensions are unequal. Inverse cannot be calculated.")
                return(NULL)
        }
        
        
        
        
        ## Calculates the Determinant of matrix to see if it is invertible or not
        ## Determinant is calculated using det()
        
        data <- x$getm()
        DT <- det(data, ...)
        if(DT == 0) {
                message("Determinant is '0'. Inverse cannot be calculated.")
        }
        else {
                IN <- solve(data, ...)
                x$setinv(IN)
                message("Calculation finished. Inverse can now be cached from memory.")
        }
        
        
        ## Returns the inverse
        IN
}

mm <- makeCacheMatrix(matrix(c(1,8,4,3,9,6,5,4,7,3,2,1), 4,3))
mv <- cacheSolve(mm)


mm <- makeCacheMatrix(matrix(c(1,8,4,3,9,6,5,4,7), 3,3))
mv <- cacheSolve(mm)
