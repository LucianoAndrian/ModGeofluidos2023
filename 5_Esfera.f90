PROGRAM Esfera
!Calcula el volumen  y superficie de la esfera dada su radio
     IMPLICIT NONE
     
     REAL                 :: radio
     REAL                 :: superficie  
     REAL                 :: volumen 
     
     PRINT *, 'radio de la esfera: '
     READ *, radio
    
   
     Call CalcEsfera(radio, superficie, volumen)
     
     PRINT *, 'Volumen de la circumferencia ', volumen
     PRINT *, 'Superficie de la esfera de radio ', radio, ' es: ', superficie
     
END PROGRAM Esfera

SUBROUTINE CalcEsfera(radio, superficie, volumen)
!subrutina que calcula lo mismo que arriba...
    IMPLICIT NONE
    
    REAL, INTENT(in)                 :: radio
    REAL, INTENT(out)                :: superficie
    REAL, INTENT(out)                :: volumen

    
    superficie = 4.*ACOS(-1.)*radio**2.
    volumen = 4./3.*ACOS(-1.)*radio**3.
    RETURN
END SUBROUTINE CalcEsfera
