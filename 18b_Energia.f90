PROGRAM EnergiaE18

  IMPLICIT NONE
  
  REAL                  ::t, xf, yi, vi_x, vi_y, ecin, epot, vectorV
  REAL, DIMENSION(13,5) :: datos
  INTEGER               :: i
      
  OPEN(unit=9, file='tiro_oblicuo.dat', status='old')
  DO i = 1, 13 ! por algun motivo la ultima linea del .dat tiene todos ceros... 14-1
    READ(9, *) datos(i,:)
  END DO
  CLOSE(9)

  DO i=1, 14
    PRINT '(10(F6.2,2X))', datos(i,:) 
  END DO
  
  PRINT *, 'Energia total en cada paso de tiempo --------'
  ! masa = 1
!  Epot = m * vel**2 / 2
! Ecin = m * g * h 
  DO i = 1, 13
    vectorV = SQRT(datos(i,4)**2+datos(i,5)**2)
    ecin = vectorV**2/2
    epot = 9.81*datos(i,3)
    PRINT *, 'tiempo: ', i , 'Etotal = ', ecin + epot 
  END DO
END PROGRAM EnergiaE18
