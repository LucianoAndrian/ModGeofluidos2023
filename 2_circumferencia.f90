PROGRAM Circumferencia 
! Calcula el area y el volumn de un circunferencia
     IMPLICIT NONE
     
     REAL                 :: circ, area, volumen
     
     PRINT *, 'Circumferencia: '
     READ *, circ
     

     CALL CircFun(circ, area, volumen)
     
     PRINT *, area
     PRINT *, volumen
     
END PROGRAM Circumferencia

SUBROUTINE CircFun(circ, area, volumen)
!Subruitina que calcula el area y volumen dada una circumferencia
  IMPLICIT NONE
  
  REAL, INTENT(in)            :: circ
  REAL, INTENT(out)           :: area, volumen
  REAL                        :: radio
  
  radio = circ/2*ACOS(-1.)
     
  area = 4*ACOS(-1.)*radio**2
  volumen = 4./3.*ACOS(-1.)*radio**3
  
  RETURN
END SUBROUTINE CircFun
