PROGRAM bienvenido
! programa para que de bienvenida
     IMPLICIT NONE
     
     CHARACTER(len=15)                 :: nombre
     
     PRINT *, 'Como te llamas?'
     READ *, nombre
     PRINT *, 'Bienvenido a Fortran ' // nombre
     
END PROGRAM bienvenido
