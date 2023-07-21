PROGRAM ascii2nc
! Abre el archivo variables_grib.dat que se autopercibe .grib
! organiza todas las variables en un array de 3 dimensiones
! escribe las variables por separado en archivos .nc

! compilar con gfortran 30_ascii2nc.f90 -L/usr/lib/x86_64-linux-gnu/ -lnetcdf -lnetcdff -I/usr/include/ -o ascii2nc

  USE netcdf

  IMPLICIT NONE

  REAL, DIMENSION(99,99,3)            :: datos
  INTEGER                             :: i, j, ch, cont
  INTEGER                             :: z, y, ini, fin
  CHARACTER(len=99*6)                 :: line
  INTEGER, DIMENSION(11,3)            :: ch_vector
  CHARACTER(20)                       :: nombre, nivel
  
  ! para crear el .nc
  INTEGER                             :: idfile, nccode,dz
  INTEGER, DIMENSION(99)              :: lon_var, lat_var
  ! para usar la subrutina
  CHARACTER(99)                       :: file_name, var_name, level,units
  CHARACTER(99)                       :: standard_var_name, long_var_name

  
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
     READ(9, *) datos(:,y,z)
    END DO
  END DO ! cierra **
  CLOSE(9)  

  !<<<<<< salida E21 >>>>>>
  !DO z=1,3
  !  PRINT *, nombre(ch_vector(:,z)), ch_vector(1,z), '/', ch_vector(2,z), '/', &
  !         ch_vector(3,z), ch_vector(4,z), 'en ', nivel(ch_vector(:,z)), ch_vector(10,z) 
  !  PRINT *, 'Min:', MINVAL(datos(:,:,z)), 'Max: ', MAXVAL(datos(:,:,z)), &
  !       'Mean: ',   SUM(datos(:,:,z)/(99*99))
  !END DO
 
  !<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>
  !<<<<<<<<<<<<<<<< ascii2nc >>>>>>>>>>>>>>>>
  DO i=1, 99
    lon_var(i) = i
    lat_var(i) = i
  END DO
    
  ! TAS
  nccode = nf90_create('tas.nc', nf90_NETCDF4, idfile)
  var_name = nombre(ch_vector(:,1))
  standard_var_name = nombre(ch_vector(:,1))
  long_var_name = 'Surface Air Temperature'
  level = nivel(ch_vector(:,1))
  units = 'K'
  dz = 2
  CALL tonetcdf(idfile, datos(:,:,1:2), file_name, var_name, &
               standard_var_name, long_var_name,dz, level, units)
  nccode = nf90_close(idfile)
  PRINT *, 'save to netcdf ', var_name

  ! UA
  nccode = nf90_create('ua.nc', nf90_NETCDF4, idfile)
  var_name = nombre(ch_vector(:,3))
  standard_var_name = 'ua'
  level = nivel(ch_vector(:,3))
  units = 'ms-1'  
  long_var_name = 'Eastward air wind speed'
  dz = 1
  CALL tonetcdf(idfile, datos(:,:,3), file_name, var_name, &
                standard_var_name, long_var_name, dz, level, units)  
  nccode = nf90_close(idfile)
  PRINT *, 'save to netcdf ', var_name

END PROGRAM ascii2nc



SUBROUTINE tonetcdf(idfile, datos, file_name, var_name, & 
                    standard_var_name, long_var_name, dz, level, units)
! crea archivo .nc                    
  USE netcdf
  INTEGER, INTENT(in)                             :: dz
  REAL, DIMENSION(99,99,dz), INTENT(in)           :: datos
  CHARACTER(99), INTENT(in)                       :: file_name, var_name, level, units
  CHARACTER(99) , INTENT(in)                      :: standard_var_name, long_var_name

  INTEGER, INTENT(out)                            :: idfile
  
  INTEGER, DIMENSION(3)                           :: sh
  
  INTEGER                                         :: idlonvar, idvarlat, idvar_field
  INTEGER                                         :: nccode, idlon, idlat, idvar,i
  INTEGER                                         :: idlon_var, idlat_var, idtime, idtime_var
  INTEGER, DIMENSION(dz)                          :: time_var
 
  DO i=1, dz
    time_var(i) = i
  END DO
  
  nccode = nf90_def_dim(idfile, 'longitude', 99, idlon) 
  nccode = nf90_def_dim(idfile, 'latitude', 99, idlat)
  nccode = nf90_def_dim(idfile, 'time', dz, idtime)
  
  nccode = nf90_def_var(idfile, 'longitude',NF90_FLOAT,  idlon, idlon_var)
  nccode = nf90_def_var(idfile, 'latitude',NF90_FLOAT,  idlat, idlat_var)
  nccode = nf90_def_var(idfile, 'time',NF90_FLOAT,  idtime, idtime_var)  
  nccode = nf90_def_var(idfile, var_name,NF90_FLOAT, [idlon,idlat,idtime], idvar_field)
  
  nccode = nf90_put_att(idfile, NF90_GLOBAL, 'Nota', &
          'Ejercicio 30, Curso intensivo Modelado Geofluido, inviero 2023')

  nccode = nf90_put_att(idfile, NF90_GLOBAL, 'Autor', &
          'Luciano Andrian')

  nccode = nf90_put_att(idfile, NF90_GLOBAL, 'Institucion', &
          'CIMA')
          
  nccode = nf90_put_att(idfile, idlon_var, 'units', 'degree_east')
  nccode = nf90_put_att(idfile, idlon_var, 'standard_name', 'lon')
  nccode = nf90_put_att(idfile, idlon_var, 'long_name', 'longitude')  
  
  nccode = nf90_put_att(idfile, idlat_var, 'units', 'degree_north')
  nccode = nf90_put_att(idfile, idlon_var, 'standard_name', 'lat')
  nccode = nf90_put_att(idfile, idlon_var, 'long_name', 'latitude')  
  nccode = nf90_put_att(idfile, idtime, 'units', 'days')

  nccode = nf90_put_att(idfile, idvar_field, 'units', units)
  nccode = nf90_put_att(idfile, idvar_field, 'standard_name', standard_var_name)
  nccode = nf90_put_att(idfile, idvar_field, 'long_name', long_var_name)  
  nccode = nf90_put_att(idfile, idvar_field, 'level', level)    
  nccode = nf90_put_att(idfile, idvar_field, '_FillValue', -2e8)              
  
  nccode = nf90_put_var(idfile, idtime_var, time_var)  
  nccode = nf90_put_var(idfile, idlon_var, lon_var)
  nccode = nf90_put_var(idfile, idlat_var, lat_var)  
  nccode = nf90_put_var(idfile, idvar_field, datos(:,:,1:dz))
  
  nccode = nf90_close(idfile) 

END SUBROUTINE tonetcdf



CHARACTER(20) FUNCTION nombre(character_vector)
! funcion para buscar el nombre
! debe ser definida como character!   
  IMPLICIT NONE  

  INTEGER, DIMENSION(11)        :: character_vector
  INTEGER                       :: code
  
  code = character_vector(8)

  SELECT CASE(code)
    CASE(34)
      nombre = "sst"
    CASE(39)
      nombre = "wsoil1"
    CASE(40)
      nombre = "wsoil2"
    CASE(41)
      nombre = "wsoil3"
    CASE(42)
      nombre = "wsoil4"
    CASE(129)
      nombre = "zg"
    CASE(130)
      nombre = "ta"
    CASE(131)
      nombre = "ua"
    CASE(132)
      nombre = "va"
    CASE(134)
      nombre = "ps"
    CASE(139)
      nombre = "tsoil1"
    CASE(151)
      nombre = "mslp"
    CASE(157)
      nombre = "hur"
    CASE(165)
      nombre = "uas"
    CASE(166)
      nombre = "vas"
    CASE(168)
      nombre = "tas"
    CASE(170)
      nombre = "tsoil2"
    CASE(183)
      nombre = "tsoil3"
    CASE(228)
      nombre = "pr"
    CASE(235)
      nombre = "tsk"
    CASE(236)
      nombre = "tsoil4"
    END SELECT
    
END FUNCTION nombre


CHARACTER(20) FUNCTION nivel(character_vector)
! funcion para buscar el nivel
  IMPLICIT NONE  

  INTEGER, DIMENSION(11)        :: character_vector
  INTEGER                       :: code
  
  code = character_vector(9)

  SELECT CASE(code)
    CASE(1)
      nivel = "surface"
    CASE(3)
      nivel = "pressure"
    CASE(4)
      nivel = "depth"
    CASE(5)
      nivel = "eta"     
  END SELECT
  
  RETURN

END FUNCTION nivel
