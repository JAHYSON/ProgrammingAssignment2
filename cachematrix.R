## Se muetran abajo 2 funciones que permiten almacenar en cach� la inversa de una matriz.

## La primera de las funciones crear� un objeto especial que almacena en cach� el inverso de a matriz.

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  datos <- function(y){
    x <<- y
    inversa <<- NULL
  }
  clave <- function() x
  datosinvertidos <- function(solveMatrix) inversa <<- solveMatrix
  Inversa <- function() inversa
  list(datos = datos, clave = clave, datosinvertidos = datosinvertidos, Inversa = Inversa)
}


## La funci�n que se muestra abajo permite calcular la inversa de la matriz obtenida con el algoritmo anterior.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$Inversa()
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  datos <- x$clave()
  inversa <- solve(datos)
  x$datosinvertidos(inversa)
  inversa      
}

##Prueba de los c�digos

C=rbind(c(1, -1/4), c(-1/4, 1))  
C

B<-makeCacheMatrix(C)
B
cacheSolve(B)
C%*%cacheSolve(B)
