MODULE EntraSale
! Abre matrices de 99x99 REAL e INTEGER
  
  IMPLICIT NONE 
  
  CHARACTER(99)                                 ::nombre
  INTEGER, DIMENSION(99,99)                     :: landmask
  REAL, DIMENSION(99,99)                        :: qvmat, tasmat, psmat

  CONTAINS

  SUBROUTINE IMatrix(nombre, datosi)
    IMPLICIT NONE
  
    CHARACTER(len=99), INTENT(in)                 ::nombre
    INTEGER, DIMENSION(99, 99), INTENT(out)       ::datosi
    INTEGER                                       :: i
  
    OPEN(unit=9, file=nombre, status='old')
      DO i = 1, 99
        READ(9, *) datosi(i,:)
      END DO
    CLOSE(9)          

  END SUBROUTINE IMatrix
  
  SUBROUTINE RMatrix(nombre, datosr)
    IMPLICIT NONE
  
    CHARACTER(len=99), INTENT(in)                 ::nombre
    REAL, DIMENSION(99, 99), INTENT(out)          ::datosr
    INTEGER                                       :: i
  
    OPEN(unit=9, file=nombre, status='old')
      DO i = 1, 99
        READ(9, *) datosr(i,:)
      END DO
    CLOSE(9)          

  END SUBROUTINE RMatrix  

END MODULE EntraSale
