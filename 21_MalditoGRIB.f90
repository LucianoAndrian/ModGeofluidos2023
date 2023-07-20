PROGRAM MalditoGRIB
!Lee el archivo .dat que se autopercibe .grib

  IMPLICIT NONE

  REAL, DIMENSION(99,99,3)            :: datos
  INTEGER                             :: i, j, ch, cont
  INTEGER                             :: z, y, ini, fin
  CHARACTER(len=99*6)                 :: line
  INTEGER, DIMENSION(11,3)            :: ch_vector
  CHARACTER(20)                       :: nombre, nivel
  
  OPEN(9, file='variables_grib.dat', status='old')

  DO z=1,3 !(**)
    ! saltea esta linea cada vez al completar el ciclo 
    ! es decir, saltea 1, 101 y 201
    READ(9, '(A)') line
    ch = 1
    ini=1
    cont=1
    
    ! guarda el contenido del encabzado
    DO ch=1, LEN(TRIM(LINE)) 
      IF (LINE(ch:ch)==',') THEN
        fin = ch - 1
          ! ICHAR convierte a otros valores...
          ! de todos modos no hace falta convertirlos
          READ(line(ini:fin), '(I10)') ch_vector(cont,z) 
          cont=cont+1
          ini = ch + 1
      END IF
    END DO  
    
    ! este loop es contando a partir de lo salteado antes (en **)
    ! ejemplo, linea 101 salteada -> do de 1 a 99
    ! en cada ciclo de ** guarda en una dimencion diferente de la matriz
    DO y=1,99
     READ(9, *) datos(y,:,z)
    END DO
  END DO ! cierra **
  
  CLOSE(9)  

  ! salida
  
  DO z=1,3
    PRINT *, nombre(ch_vector(:,z)), ch_vector(1,z), '/', ch_vector(2,z), '/', &
           ch_vector(3,z), ch_vector(4,z), 'en ', nivel(ch_vector(:,z)), ch_vector(10,z) 
    PRINT *, 'Min:', MINVAL(datos(:,:,z)), 'Max: ', MAXVAL(datos(:,:,z)), &
         'Mean: ',   SUM(datos(:,:,z)/(99*99))
  END DO

END PROGRAM MalditoGRIB


CHARACTER(20) FUNCTION nombre(character_vector)
! funcion para buscar el nombre
!debe ser definida como character!   
  IMPLICIT NONE  

  INTEGER, DIMENSION(11)        :: character_vector
  INTEGER                       :: code
  
  code = character_vector(8)

  SELECT CASE(code)
  CASE(168)
    nombre = 'tas'
  CASE(131)
    nombre = 'ua'
  END SELECT
  
  RETURN

END FUNCTION nombre


CHARACTER(20) FUNCTION nivel(character_vector)
! funcion para buscar el nivel
  IMPLICIT NONE  

  INTEGER, DIMENSION(11)        :: character_vector
  INTEGER                       :: code
  
  code = character_vector(9)

  SELECT CASE(code)
  CASE(1)
    nivel = 'surface'
  CASE(5)
    nivel = 'eta'
  END SELECT
  
  RETURN

END FUNCTION nivel