PROGRAM SerieDCAO
  IMPLICIT NONE

  INTEGER                               :: i, dcao, num
  INTEGER, ALLOCATABLE                  :: vec_prev(:)


  PRINT *, 'Cuantos numeros de la serie DCAO? '
  READ *, num

  ALLOCATE(vec_prev(4))

  !vec_prev = (/0, 1, 0, 1/)
  vec_prev = [0, 1, 0, 1] 

  DO i = 1, num
    IF (i <= 4) THEN
      dcao = vec_prev(i)
    ELSE
      CALL Updatevec_prev(vec_prev, dcao)
    END IF
    
    PRINT *, dcao  
  END DO

  DEALLOCATE(vec_prev)

CONTAINS

SUBROUTINE Updatevec_prev(vec_prev, dcao)

  IMPLICIT NONE

  INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(inout)     :: vec_prev
  INTEGER, INTENT(out)                                  :: dcao
  
  
  dcao = -vec_prev(1) -vec_prev(2) +vec_prev(3) +vec_prev(4)
  vec_prev = [vec_prev(2), vec_prev(3), vec_prev(4), dcao]

  RETURN

END SUBROUTINE Updatevec_prev

END PROGRAM SerieDCAO


