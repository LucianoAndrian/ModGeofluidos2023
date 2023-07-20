PROGRAM VolumenCubo
!Calcula el volumen de un cubo dada su arista
    IMPLICIT NONE
     
     REAL                 :: arista
     REAL                 :: volumen, funcion
     
     PRINT *, 'Longitud de la arista: '
     READ *, arista
     
     CALL volumensub(arista, volumen)
     
     PRINT *, 'El volumen del cubo es', volumen
     
     PRINT *, 'El volumen del cubo con funcion es: ', funcion(arista)

END PROGRAM VolumenCubo     

!Este fue el primero que modifique
!asi que hice las dos opciones
SUBROUTINE volumensub(arista, volumen)
!subrutina que calcula lo mismo que arriba...
    IMPLICIT NONE
     
     REAL, INTENT(in)                 :: arista
     REAL, INTENT(out)                 :: volumen
     
     volumen = arista**3
     
     RETURN
END SUBROUTINE volumensub

REAL FUNCTION funcion(arista)
!hace lo mismo que antes pero creo que es mas apropiado
    IMPLICIT NONE
     
     REAL                 :: arista
     
     funcion = arista**3
     
     RETURN
END FUNCTION funcion

