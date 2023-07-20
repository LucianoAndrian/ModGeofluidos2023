PROGRAM Faranheit
! Pasa de Centrigrado a Farenheit
     IMPLICIT NONE
     
     REAL                 :: gradoC, FaranFun
     
     PRINT *, 'Temperatura en centrigrados: '
     READ *, gradoC
     
     PRINT *, 'La temperatura en Faranheit es', FaranFun(gradoC)
     
END PROGRAM Faranheit

REAL FUNCTION FaranFun(gradoC)
! pasa de C a F
  IMPLICIT NONE
  
  REAL                    :: gradoC
  
  FaranFun = 32 + gradoC*(180./100.)
  
  RETURN
END FUNCTION FaranFun
