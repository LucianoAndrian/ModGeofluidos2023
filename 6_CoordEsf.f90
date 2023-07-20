PROGRAM CoordEsf
!Dadas tres coordenadas esfericas las transforma a cartesianas
     IMPLICIT NONE
     
     REAL                 :: radio, tetha, phi, x, y, z
     
     PRINT *, 'radio: '
     READ *, radio, tetha, phi
     
     CALL  EsfACar(radio, tetha, phi, x, y, z)
     
     PRINT *, 'x: ', x
     PRINT *, 'y: ', y
     PRINT *, 'z: ', z
     
END PROGRAM CoordEsf     

SUBROUTINE EsfACar(radio, tetha, phi, x, y, z)
! pasa de esfericas a cartesianas
  IMPLICIT NONE
  
  REAL, INTENT(in)       :: radio, tetha, phi
  REAL, INTENT(out)      :: x, y, z
  
  x = radio*SIN(tetha*ACOS(-1.)/180.)*COS(phi*ACOS(-1.)/180.)
  y = radio*SIN(tetha*ACOS(-1.)/180.)*SIN(phi*ACOS(-1.)/180.)
  z = radio*COS(tetha*ACOS(-1.)/180.)
  
  RETURN
  
END SUBROUTINE EsfACar
     
