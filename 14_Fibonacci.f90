PROGRAM Fibonacci
! primeros 100 numeros de la serie de Fibonacci

  IMPLICIT NONE
  
  INTEGER(KIND=8)                      ::n, i
  INTEGER(KIND=8), DIMENSION(100)      ::fib

  fib(1) = 0
  fib(2) = 1
  
  !calculo a partir del 3er termino
  DO i=3, 100
    fib(i) = fib(i-1) + fib(i-2)
    PRINT*, fib(i)
  END DO

END PROGRAM fibonacci
    
	
    

    
  