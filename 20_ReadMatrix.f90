PROGRAM LeerMatriz
  IMPLICIT NONE
  
  REAL, ALLOCATABLE, DIMENSION(:,:)         :: datosr
  INTEGER, ALLOCATABLE, DIMENSION(:,:)      :: datosi
  INTEGER                                   :: i, ierror, intval
  CHARACTER(len=99)                         :: nombre
  INTEGER, DIMENSION(2)                     :: sh
  CHARACTER(10)                             :: td
  
  PRINT *, 'Nombre del archivo: '
  READ *, nombre
  
  PRINT *, 'Dimensiones(x,y): '
  READ *, sh
  
  PRINT *, 'Tipo de datos: '
  READ *, td
  
  IF (td=='R') THEN
    ALLOCATE(datosr(sh(1), sh(2)))
    CALL RMatrix(nombre, sh, datosr)
    PRINT *, 'Valor Minimo: ', MINVAL(datosr), ', maximo ', MAXVAL(datosr)    
  ELSE IF (td=='I') THEN
    ALLOCATE(datosi(sh(1), sh(2)))
    CALL IMatrix(nombre, sh, datosi)
    PRINT *, 'Valor Minimo: ', MINVAL(datosi), ', maximo ', MAXVAL(datosi)
  END IF
  
END PROGRAM LeerMatriz

SUBROUTINE RMatrix(nombre, sh, datosr)
  IMPLICIT NONE
  
  CHARACTER(len=99), INTENT(in)                 ::nombre
  INTEGER, DIMENSION(2), INTENT(in)             ::sh
  REAL, DIMENSION(sh(1),sh(2)), INTENT(out)     ::datosr
  INTEGER                                       :: i
  
  OPEN(unit=9, file=nombre, status='old')
    DO i = 1, sh(1)
      READ(9, *) datosr(i,:)
    END DO
  CLOSE(9)          

END SUBROUTINE RMatrix


SUBROUTINE IMatrix(nombre, sh, datosi)
  IMPLICIT NONE
  
  CHARACTER(len=99), INTENT(in)                 ::nombre
  INTEGER, DIMENSION(2), INTENT(in)             ::sh
  INTEGER, DIMENSION(sh(1),sh(2)), INTENT(out)  ::datosi
  INTEGER                                       :: i
  
  OPEN(unit=9, file=nombre, status='old')
    DO i = 1, sh(1)
      READ(9, *) datosi(i,:)
    END DO
  CLOSE(9)          

END SUBROUTINE IMatrix


!ftp://cima.fcen.uba.ar/lluis.fita/formacion/CursoFortran23/practicas/ps.dat
