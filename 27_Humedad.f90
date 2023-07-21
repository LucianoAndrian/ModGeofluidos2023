PROGRAM Humedad
! calcula la media de la humedad relativa sobre el 
! continente a partir de tas, ps y qv

  ! Modulos  
  USE EntraSale ! Lee las matrices REAL (subr RMatrix) o INTEGER (subr IMatrix)
  USE MatMod    ! Busca valores especificos segun se pida (subr Cond)
  USE Meteo     ! Calcula HR a partir de Temp, Pres y Qv (func hr)
  IMPLICIT NONE
  
  INTEGER                          :: i, j
  INTEGER, DIMENSION(2)            :: sh
  REAL, ALLOCATABLE, DIMENSION(:)  :: Hur
  REAL                             :: Hur_test, mean

  ! mascara de tierra
  nombre = 'landmask.dat'
  CALL IMatrix(nombre, landmask)
  
  !selección de los puntos de grilla continentales (=1)
  condicion='igual'
  valor=1
  mat=landmask
  CALL Cond(condicion, valor, mat, v_result, cont)
  
  !Vector donde se van a guardar los puntos de grilla
  !sobre el continente para calcular su media.
  ALLOCATE(hur(cont-1))
  
  ! tas, qv y ps
  nombre = 'tas.dat'  
  CALL RMatrix(nombre, tasmat)
  
  nombre = 'ps.dat'
  CALL RMatrix(nombre, psmat)
  
  nombre = 'qv.dat'
  CALL RMatrix(nombre, qvmat)
  
  ! Test de la guia (da parecido)
  !hur_test=hr(101300., 290., 0.003)   
  !print *, 'hr(101300., 290., 0.003): ', hur_test
  
  ! seleccion de los puntos de grilla correspondientes 
  ! Calculo de HR
  DO i=1, cont-1
    sh = v_result(i,1:2)
    hur(i) = hr(psmat(sh(1), sh(2)), tasmat(sh(1), sh(2)), qvmat(sh(1), sh(2)))
  END DO
    
  ! media
  mean = SUM(hur)/(size(hur))
  print *, 'Humedad relativa media sobre continente', mean

END PROGRAM Humedad
