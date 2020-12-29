!Module for calculating the erros to determine when to stop iterating when solving the field equations.
MODULE ERROR_CALC 

  USE ISO_FORTRAN_ENV
  IMPLICIT NONE 
  SAVE

  CONTAINS 

  FUNCTION err_calc(A,B,dx,dy,nx,ny)
!A is phi and B is rho.
!Fortran assumes arrays start at 1 when passed to a function, so LB must be specified for A.
    REAL(REAL64), DIMENSION(0:,0:):: A
    REAL(REAL64), DIMENSION(:,:):: B
    REAL(REAL64) :: err_calc
!The step size for the x and y coordinates finite difference method.
    REAL(REAL64), INTENT(IN):: dx, dy
!The number of nodes in the x and y coordinates.
    INTEGER, INTENT(IN) :: nx, ny
!Loop variables 
    INTEGER :: i, j 
!Variables to store the total error and d_rms.
    REAL(REAL64) :: err_total, d_rms
    
!Setting the initial values to be 0 which will be later summed.
    err_total = 0.0_REAL64
    d_rms = 0.0_REAL64 
    
!Summing over i,j to get the total error.
    DO i = 1 , nx 
      DO j = 1,  ny
        err_total = err_total + ABS(((A(i-1,j)-2*A(i,j)+A(i+1,j))/(dx**2))+((A(i,j-1)-2*A(i,j)+A(i,j+1))/(dy**2))-B(i,j))
      END DO 
    END DO 
    
!Summing over i,j to get d_rms
    DO i = 1, nx 
      DO j = 1, ny
        d_rms =  d_rms + (((A(i-1,j)-2*A(i,j)+A(i+1,j))/(dx**2))+((A(i,j-1)-2*A(i,j)+A(i,j+1))/(dy**2)))**2
      END DO 
    END DO 

!If d_rms is non zero then to take the square root and calculate the ratio of total error and d_rms, otherwise just gives that the ratio is 0.
    IF (d_rms > 0 ) THEN 
      d_rms = SQRT(d_rms) 
      err_calc = err_total / d_rms 
    ELSE 
      err_calc = 0.0_REAL64
    END IF 
     
  END FUNCTION err_calc

END MODULE ERROR_CALC
