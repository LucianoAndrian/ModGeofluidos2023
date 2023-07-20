program DescoPrimos
! Descompone un numero menor a 200 en sus numeros primos

  implicit none

  integer, dimension(25) :: numeros_primos 
  ! tengo 25 numeros primos, como se hace de otra manera esto?
  integer                :: i, j, numero, divisor, cont

  ! Lectura del archivo
  ! status='old' para que no lo modifique!
  OPEN(unit=9, file='NumerosPrimos.dat', status='old')
  DO i = 1, 25
    READ(9, *) numeros_primos(i)
  END DO
  CLOSE(10)
  
  PRINT *, 'Numero <= 200: '
  READ *, numero
     
  divisor = 1
  cont = 0

  DO j = 1, 25
    divisor = numeros_primos(j)
    IF (MOD(numero, divisor) == 0) THEN
      cont = 0
      DO WHILE (MOD(numero, divisor) == 0)
        numero = numero / divisor
        cont = cont + 1
        PRINT *, numero*divisor, " = ", divisor
      END DO
    END IF
  END DO

END PROGRAM DescoPrimos