PROGRAM Main 

  USE ISO_FORTRAN_ENV
  USE EQ_FIELD
  USE velocity_verlet  
  !USE command_line
  !USE domain_tools
  USE ERROR_CALC

  IMPLICIT NONE 

  LOGICAL :: success 
  !CHARACTER(LEN=20) :: problem 
  INTEGER(INT32) :: nx, ny ,i,j
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: problem, solution
  REAL(REAL64) :: dx,dy,dt,error
  REAL(REAL64), DIMENSION(2) :: init_pos,init_vel, init_acc
  REAL(REAL64), DIMENSION(0:1000,2) ::  pos_hist, vel_hist, acc_hist



  init_pos = 0.0_REAL64
  init_vel = 0.1_REAL64


  nx = 6
  ny= 6
  
  dy = 2.0_REAL64/(nx-1) 
  dx = 2.0_REAL64/(ny-1)
  dt = 0.1_REAL64
  
  ALLOCATE(problem(1:nx, 1:ny))
  ALLOCATE(solution(0:nx+1, 0:ny+1))

  problem = 0.0_REAL64 
  !solution = 0.0_REAL64
  
 ! DO i = 1, nx 
  !  DO j = 1, ny 
   !   problem(i,j) = EXP(-((h*(i-1)-1)/0.1_REAL64)**2-((k*(j-1)-1)/0.1_REAL64)**2)
   ! END DO
 ! END DO
  
  !DO i = 1, nx 
    !DO j = 1, ny
      !problem(i,j) = EXP(-((h*(i-1)-1+0.25_REAL64)/0.1_REAL64)**2-((k*(j-1)-1+0.25_REAL64)/0.1_REAL64)**2)
    !END DO
  !END DO 
  !solution = f_c(problem, X, Y)
  
  solution = f_c(problem, dx,dy,nx,ny)
  

  CALL verlet_solver(solution, init_pos,init_vel,init_acc,pos_hist,vel_hist,acc_hist,dx,dy,dt,nx,ny)
  
  print*, pos_hist(1000,:)

END PROGRAM Main
