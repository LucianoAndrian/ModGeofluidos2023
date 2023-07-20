PROGRAM MinMay
!pasa todas las letras de mayusculas a minisculas y viceversa de una palabra
     IMPLICIT NONE
          
     CHARACTER(len=15)                  :: word

     PRINT *, 'Palabra: '
     READ *, word
     
     CALL LowUp(word)

     PRINT *, word
END PROGRAM    
       
SUBROUTINE LowUp(word)
!pasa de mayusculas a minusculas y minusculas a mayusculas
  IMPLICIT NONE
  
  CHARACTER(len=15), intent(inout)   :: word
  INTEGER                            :: wlen, i, ich
  
  wlen = len(word)
     
  do i=1,wlen
    ich = ICHAR(word(i:i))
       if (ich >= 65 .and. ich <= 90) THEN 
         word(i:i) = CHAR(ich+32)
       ELSE IF (ich >= 97 .and. ich <= 120) THEN 
         word(i:i) = CHAR(ich-32)
       END IF
  end do 
  
  RETURN

END SUBROUTINE LowUp

