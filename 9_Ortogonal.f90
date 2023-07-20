PROGRAM Ortogonal
! calcula un vector ortogonal a otro
     IMPLICIT NONE
     
     REAL, DIMENSION(2)                  :: ivector, ovector
     REAL                                :: i11, i12, o21, o22

     PRINT *, 'i11 y i12: '
     READ *, i11, i12
     
     ivector(1)=i11
     ivector(2)=i12
     
     CALL OrtogonalFun(ivector, ovector)   
     PRINT*, ovector     
     
END PROGRAM Ortogonal

SUBROUTINE OrtogonalFun(ivector, ovector)
  IMPLICIT NONE
  
  REAL, DIMENSION(2), intent(in)       ::ivector
  REAL, DIMENSION(2), intent(out)      ::ovector
  
  ovector(1) = -1*ivector(2)
  ovector(2) = ivector(1)
  IF (DOT_PRODUCT(ivector, ovector) == 0) RETURN

END SUBROUTINE OrtogonalFun

