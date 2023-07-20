PROGRAM Matloop

  IMPLICIT NONE 
  
  INTEGER                     :: i, j
  REAL, DIMENSION(10,10)      :: mat, aux
  REAL, DIMENSION(2)          :: sh
  REAL                        :: cont, mean
  REAl, DIMENSION(10)         :: aux2
  
  ! que rebuscado esto... pensar alguna forma mas facil
  cont = 1
  DO i = 1, 10
    DO j = 1, 10
      mat(i,j) = cont
      cont = cont + 1
    END DO
  END DO

  DO i=1, 10
    PRINT '(10(F6.2,2X))', mat(i,:) 
  END DO
  
  PRINT *, '-------------'
  PRINT *, 'Diagonal'
  DO i=1, 10
    PRINT*, mat(i,i)
  END DO
  
  PRINT *, '-------------'
  PRINT *, 'Mean value'
  sh = SHAPE(mat)
  mean = SUM(mat)/(sh(1)*sh(2))
  PRINT*,mean 

  PRINT *, '-------------'
  PRINT *, 'SD'
  

  aux = mat - mean 
  print *, SQRT(SUM(aux**2)/(sh(1)*sh(2)))
  
  PRINT *, '-------------'
  PRINT *, 'Mean value of each column'
  
  DO j=1,10
     PRINT *, SUM(mat(:,j))/(sh(1)) 
  END DO 

  PRINT *, '-------------'
  PRINT *, 'Mean value of each line'
  
  DO j=1,10
     PRINT *, SUM(mat(j,:))/(sh(1)) 
  END DO       
   
  PRINT *, '-------------'
  PRINT *, 'Mean of even values'
  PRINT *, SUM(mat, MASK= MOD(mat,2.)==0)/(sh(1)*sh(2)/2) !esto es medio trucho el /2


  PRINT *, '-------------'
  PRINT *, 'diferencia abajo y arriba de la diagonal'
  DO i=2, 9
    aux2(i-1) = mat(i+1,i+1)-mat(i-1,i-1)
  END DO
  PRINT *, SUM(aux2)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

END PROGRAM
    
  
