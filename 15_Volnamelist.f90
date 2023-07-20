PROGRAM VolNamelist
!Calcula el volumen de cubo, esferas y tetraedro regular

  IMPLICIT NONE
  
  CHARACTER(len=15)          :: nombre
  REAL                       :: radista, volumen
  ! 17, 18, 19, 27, 30 
  
  NAMELIST /objeto /radista, nombre
  
  OPEN(10, FILE='namelist', STATUS='OLD')
  READ(10, objeto)  
  CLOSE(10)
  
  CALL VolumenesSub(radista, nombre, volumen)
  
  PRINT *, 'El volumen del ' // nombre, 'es ', volumen

END PROGRAM VolNamelist



SUBROUTINE VolumenesSub(radista, nombre, volumen)
! Calcula los volumenes según lo ingresado arriba
  IMPLICIT NONE
  
  REAL, INTENT(in)              :: radista
  REAL, INTENT(out)             :: volumen
  CHARACTER(len=15), INTENT(in) :: nombre
  
  ! acá se podría usar CASE ?
  
  IF (nombre == 'cubo') THEN
  
    volumen = radista**3.
    
  ELSE IF (nombre == 'tetraedro') THEN
  
    volumen = SQRT(2.)/12.*radista**3.
    
  ELSE IF (nombre == 'esfera') THEN
  
     volumen = 4./3.*ACOS(-1.)*radista**3.
     
  END IF
  
  RETURN

END SUBROUTINE VolumenesSub
