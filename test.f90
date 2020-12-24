PROGRAM Main 

  USE ISO_FORTRAN_ENV
  USE EQ_FIELD  
  !USE command_line
  !USE domain_tools
  !USE ERROR_CALC

  IMPLICIT NONE 

  LOGICAL :: success 
  !CHARACTER(LEN=20) :: problem 
  INTEGER :: dx, dy ,i,j
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: problem, solution
  REAL(REAL64) :: h,k,error 

  dx = 6
  dy= 6
  
  h = 2.0_REAL64/(dx-1) 
  k = 2.0_REAL64/(dy-1)
  
  ALLOCATE(problem(1:dx, 1:dy))
  ALLOCATE(solution(0:dx+1, 0:dy+1))

  problem = 0.0_REAL64 
  !solution = 0.0_REAL64
  
  DO i = 1, dx 
    DO j = 1, dy 
      problem(i,j) = EXP(-((h*(i-1)-1)/0.1_REAL64)**2-((k*(j-1)-1)/0.1_REAL64)**2)
    END DO
  END DO
  
  !DO i = 1, dx 
    !DO j = 1, dy
      !problem(i,j) = EXP(-((h*(i-1)-1+0.25_REAL64)/0.1_REAL64)**2-((k*(j-1)-1+0.25_REAL64)/0.1_REAL64)**2)
    !END DO
  !END DO 
  !solution = f_c(problem, X, Y)
  
  solution = f_c(problem, h,k,dx,dy)
  
  print*, solution 
  

END PROGRAM Main
