#Author: RICARDO LINARES
#Date: 12/23/2018

#English:
#--------
## This functions create a special matrix object and then calculate its inverse matrix.
##  verifying that the matrix does not exist, because it is very costly function.

#Español:
#-------
## Estas funciones crean un objeto de matriz especial y luego calculan su matriz inversa. 
## verificando que la matriz no existe, porque es una función muy costosa.

# EN: Create a special matrix.
# EN: Creando una matriz especial.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(z) i <<- z
  getInverse <- function() i
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## EN: Compute the inverse matrix.
## ES: Computando la matriz inversa.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    #EN: the matrix exists
    #ES: la matriz existe.
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverse(m)
  #EN: return the inverse.
  #ES: retornar la matriz inversa
  m
}

###########################
#EN: Test code.
#ES: Codigo de prueba.
m <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

x <- makeCacheMatrix()
x$setMatrix(m)
x$getMatrix()

cacheSolve(x)
x$getInverse()

