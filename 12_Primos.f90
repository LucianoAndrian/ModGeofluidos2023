PROGRAM Primos
     IMPLICIT NONE
     
     REAL                      :: r
     INTEGER                   :: i, i2
     LOGICAL                   :: primo
     
     OPEN(UNIT=9, FILE='NumerosPrimos.dat')
     !primo = .true.
     do i=2, 100
        primo = .true.
        do i2 = 2, i-1
           if (MOD(i, i2) == 0) THEN
              primo = .false.
              exit
           END IF
        END DO
        
        if (primo) THEN
           write(9,*) i
        END IF
           
        END DO
   CLOSE(UNIT=9)
   
END PROGRAM 
