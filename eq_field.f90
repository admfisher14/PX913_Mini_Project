MODULE EQ_FIELD
 
  USE ISO_FORTRAN_ENV 
  USE ERROR_CALC

  IMPLICIT NONE 
  
  CONTAINS 

  FUNCTION f_c(rho, dx, dy, nx, ny)
    REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: f_c
!The rho specified by the user
    REAL(REAL64), DIMENSION(:,:), INTENT(IN) :: rho
!Grid size.
    INTEGER, INTENT(IN) :: nx,ny
!Step size.
    REAL(REAL64) :: dx,dy
!Loop variables.
    INTEGER :: i,j
!We want to keep iterating until we are below the fixed tol set below, one can change it if need be.
    REAL(REAL64):: tol = 1.0_REAL64/((10.0_REAL64)**(5))
!Variable to store the error of each iteration.
    REAL(REAL64) :: tot_err
    
    ALLOCATE(f_c(0:(1+nx),0:(1+ny)))

!This is the initial field charge at iteration t = 0, one can change it but it should converge no matter the initial condition.
    f_c = 0.0_REAL64

!I have noticed that this algorithm does not take guard cells into account when updating, is that a possible cause for the error? - Omar

!This do while loops does the finite difference until a certain tolerance is met.     
    DO 
      DO i =  1, nx  
        DO j = 1, ny 
          f_c(i,j) = -((rho(i,j)-((f_c(i+1,j)+f_c(i-1,j))/(dx)**2)-((f_c(i,j+1)+f_c(i,j-1))/(dy)**2))/((2/(dx**2)) + (2/(dy**2))))
        END DO 
      END DO 
      tot_err = err_calc(f_c, rho, dx, dy, nx, ny)
      print*, tot_err
      IF (tot_err < tol) EXIT  
    END DO 
    
  END FUNCTION f_c 

END MODULE EQ_FIELD
