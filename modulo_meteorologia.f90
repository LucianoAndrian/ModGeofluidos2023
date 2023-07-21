MODULE Meteo
!Calcula la hr a partir de la presion, la temp
! y qv

  IMPLICIT NONE
  !Funcion
  !REAL                 :: hr
  !Variables que entran
  REAL                 :: ps, tas, qv
  
  
  CONTAINS
  
  REAL FUNCTION hr(ps, tas, qv)
  
    ! entran
    REAL                 :: ps, tas, qv
    ! se calculan
    REAL                 :: es, ws, tasC, psH
    
    tasC = tas - 273.
    psH = ps / 100.
    
    IF ((tasC < 50) .and. (tasC > -30)) THEN
      
      es = 6.1094*EXP((17.625*(tasC))/(tasC + 243.04))
    
      ws = 0.622*(es/(psH-es))
    
      hr = qv/ws
    ELSE
      hr = -100
    END IF
  
    RETURN
  END FUNCTION hr
  
END MODULE Meteo
