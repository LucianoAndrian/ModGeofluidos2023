MODULE MatMod
! Selecciona las coordenadas de los elementos de una matriz
! que cumplen con determinado criterio

  IMPLICIT NONE  
  
  REAL, DIMENSION(99,99)               :: mat
  INTEGER                            :: valor,cont
  REAL, DIMENSION(99*99,3)             :: v_result
  CHARACTER(len=10)                  :: condicion


  CONTAINS


  SUBROUTINE Cond(condicion, valor, mat, v_result,cont2)

    IMPLICIT NONE
    CHARACTER(len=10), INTENT(in)                    :: condicion   
    INTEGER, INTENT(in)                              :: valor
    REAL, DIMENSION(99,99), INTENT(in)               :: mat
  
    REAL, DIMENSION(99*99,3), INTENT(out)            :: v_result
    INTEGER                                          :: cont
  
    INTEGER                                          :: i, j, cont2
    INTEGER, DIMENSION(2)                            :: sh
  

   sh = SHAPE(mat)
  
   cont = 1
   cont2 = 1
   
   ! que elegante esto...
  
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

END MODULe MatMod
