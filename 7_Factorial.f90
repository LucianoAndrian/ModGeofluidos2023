PROGRAM Factorial
!Calcula un factorial
     IMPLICIT NONE

     INTEGER              :: numero, numero_aux, i
     REAL                 :: fact, resta, FactFun
     
     PRINT *, 'Ingresa un numero: '
     READ *, numero
     
     print *, FactFun(numero)
     !3*2*1
END PROGRAM 

REAL FUNCTION FactFun(numero)
! Calcula un factorial de un numero  
  IMPLICIT NONE
  
  INTEGER                :: numero, numero_aux, i
  REAL                   :: fact, resta
  
  fact = numero
  loop: DO i=1, numero-1
         fact = fact*i       
  END DO loop
 
 FactFun = fact
 RETURN
 
END FUNCTION FactFun
