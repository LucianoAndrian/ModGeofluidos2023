PROGRAM CoorMatrix
  IMPLICIT NONE
  
  REAL, DIMENSION(5,5)               :: mat
  INTEGER                            :: cont, i, j, valor
  REAL, DIMENSION(100,3)             :: v_result
  CHARACTER(len=10)                  :: condicion
  
  cont = 1
  DO i = 1, 5
    DO j = 1, 5
      mat(i,j) = cont
      cont = cont + 1
    END DO
  END DO
  
  DO i=1, 5
    PRINT '(5(F6.2,2X))', mat(i,:) 
  END DO
  
  PRINT *, 'Condicion a aplicar a la matriz?'
  READ *, condicion
  
  IF (condicion /= 'par' .and. condicion /= 'impar') THEN
    PRINT *, 'Valor'
    READ *, valor
  ELSE
    valor = 0 
  END IF
  
  CALL Cond(condicion, valor, mat, v_result, cont)

  PRINT *, '-----------------'  
  PRINT *, '<<<<RESULTADO>>>>'
  DO i=1, cont-1
    PRINT '(2(F6.2,2X), A, (F6.2,2X))', v_result(i,1:2), ' = ', v_result(i,3)
  END DO
  PRINT *, '-----------------'  

END PROGRAM CoorMatrix

SUBROUTINE Cond(condicion, valor, mat, v_result, cont)

  IMPLICIT NONE
  CHARACTER(len=10), INTENT(in)                    :: condicion   
  INTEGER, INTENT(in)                              :: valor
  REAL, DIMENSION(5,5), INTENT(in)                 ::mat
  
  REAL, DIMENSION(100,3), INTENT(out)              ::v_result
  INTEGER, INTENT(out)                             :: cont
  
  INTEGER                                          :: i, j, cont2
  INTEGER, DIMENSION(2)                            :: sh
  

  sh = SHAPE(mat)
  
  cont = 1
  cont2 = 1
  ! VER COMO HACER ESTO MAS LINDO!!!!!!!!!!!!!!!!!
  
  SELECT CASE(TRIM(condicion))
  CASE('par')
    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF ((MOD(mat(i,j),2.)==0) .or. (mat(i,j)==2.)) cont = cont + 1
      END DO
    END DO
    


    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF ((MOD(mat(i,j),2.)==0) .or. (mat(i,j)==2.)) THEN
          v_result(cont2,1:2)= [i,j]
          v_result(cont2, 3) =  mat(i,j)
          cont2 = cont2 + 1
        END IF
      END DO
    END DO
    
  CASE('impar')
    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF (MOD(mat(i,j),2.)==0) cont = cont + 1
      END DO
    END DO
    


    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF (MOD(mat(i,j),2.)/=0) THEN
          v_result(cont2,1:2)= [i, j]
          v_result(cont2, 3) =  mat(i,j)
          cont2 = cont2 + 1
        END IF         
      END DO
    END DO  
  
  CASE('mayor')
    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF (mat(i,j)>valor) cont = cont + 1
      END DO
    END DO
    


    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF (mat(i,j)>valor) THEN
          v_result(cont2,1:2)= [i,j]
          v_result(cont2, 3) =  mat(i,j)
          cont2 = cont2 + 1
        END IF          
      END DO
    END DO  
  
  CASE('menor')
    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF (mat(i,j)<valor) cont = cont + 1
      END DO
    END DO


    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF (mat(i,j)<valor) THEN
          v_result(cont2,1:2)= [i,j]
          v_result(cont2, 3) =  mat(i,j)
          cont2 = cont2 + 1
        END IF          
      END DO
    END DO  

  CASE('igual')
    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF (mat(i,j)==valor) cont = cont + 1
      END DO
    END DO


    DO i = 1, sh(1)
      DO j = 1, sh(2)
        IF (mat(i,j)==valor) THEN
          v_result(cont2,1:2)= [i,j]
          v_result(cont2, 3) =  mat(i,j)
          cont2 = cont2 + 1
        END IF          
      END DO
    END DO  
  END SELECT
  
END SUBROUTINE Cond
