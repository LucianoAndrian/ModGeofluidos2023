PROGRAM Volumenes
!Calcula el volumen de cubo, esferas y tetraedro regular

  IMPLICIT NONE
  
  CHARACTER(len=15)          :: forma
  REAL                       :: volumen, radista !radio o arista
  
  
  PRINT *, 'Forma a calcular su volumen (esfera, cubo, tetraedro reg.): '
  READ *, forma
  
  IF (forma == 'cubo' .or. forma == 'tetraedro') THEN
  
    PRINT *, 'largo de arista: '
    READ *, radista
    
  ELSE IF (forma == 'esfera') THEN
  
    PRINT *, 'radio: '
    READ *, radista
    
  END IF
  
  CALL VolumenesSub(radista, forma, volumen)
  
  PRINT *, 'El volumen es: ', volumen
  
END PROGRAM Volumenes


SUBROUTINE VolumenesSub(radista, forma, volumen)
! Calcula los volumenes según lo ingresado arriba
  IMPLICIT NONE
  
  REAL, INTENT(in)              :: radista
  REAL, INTENT(out)             :: volumen
  CHARACTER(len=15), INTENT(in) :: forma
  
  ! acá se podría usar CASE ?
  
  IF (forma == 'cubo') THEN
  
    volumen = radista**3.
    
  ELSE IF (forma == 'tetraedro') THEN
  
    volumen = SQRT(2.)/12.*radista**3.
    
  ELSE IF (forma == 'esfera') THEN
  
     volumen = 4./3.*ACOS(-1.)*radista**3.
     
  END IF
  
  RETURN

END SUBROUTINE VolumenesSub
  
