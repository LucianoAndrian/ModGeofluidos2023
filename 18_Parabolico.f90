PROGRAM TParabolico
! simula tiro oblicuo  
  IMPLICIT NONE

  REAL                    :: xi, yi, vi, ts, ang
  REAL                    :: xf, yf, t
  REAL                    :: vi_x, vi_y, vf_x, vf_y
  REAL, ALLOCATABLE       :: datos(:,:)
  INTEGER                 :: t_total, t_cont, i

  PRINT *, 'Ingrese altura inicial: '
  READ *, yi

  PRINT *, 'Ingrese velocidad inicial: '
  READ *, vi

  PRINT *, 'Ingrese angulo con la horizonal: '
  READ *, ang
 
  PRINT *, 'Ingrese paso temporal: '
  READ *, ts

  
  ! asumo que donde comienza es cero
  xi = 0

  ! necesito saber cuantos pasos temporales va tener!
  ! formulas segun INTERNE'... REVISAR!
  IF (yi > 0) THEN
    t_total = INT((2*vi_y/9.81 + SQRT((2*vi_y/9.81)**2 + 8*yi/9.81))/ts) + 1
  ELSE IF (yi == 0) THEN
    t_total = INT(2*vi_y/(9.81*ts)) + 1
  END IF

  ALLOCATE(datos(t_total, 5))

  vi_x = vi*COS(ang*(ACOS(-1.)/180.))
  vi_y = vi*SIN(ang*(ACOS(-1.)/180.))

  t = 0.

  datos(1,:) = [t, 0., yi, vi_x, vi_y]
  t_cont = 2
  DO WHILE((yf >= 0) .and. (t<=t_total))  
    t = t + ts

    xf = vi_x * t 
    yf = yi + vi_y*t - 0.5*9.81*t**2
    
    vf_x = vi_x
    vf_y = vi_y - 9.81*t    
    	    
    datos(t_cont, :) = [t, xf, yf, vf_x, vf_y]
    t_cont = t_cont + 1
    
  END DO
  
  ! escribir el archivo
  OPEN(UNIT=9, file='tiro_oblicuo.dat', status='replace')
  DO i=1, t_cont
    WRITE(9,'(5(F6.2,2X))') datos(i,:)
  END DO
  CLOSE(9)

END PROGRAM TParabolico
    
