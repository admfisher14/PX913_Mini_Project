PROGRAM Main 

  USE ISO_FORTRAN_ENV
  USE EQ_FIELD
  USE velocity_verlet  
  USE command_line
  !USE domain_tools
  USE ERROR_CALC
  USE GAUSSIAN 
  use write_netcdf

  IMPLICIT NONE 

  LOGICAL :: success 
  CHARACTER(LEN=20) :: init_states
  !nx,ny are for storing the number of grid points the user wants.
  INTEGER(INT32) :: nx, ny ,i,j,ierr
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: problem, solution,ex,ey
  REAL(REAL64) :: dx,dy,dt,error
  REAL(REAL64), DIMENSION(2) :: init_pos,init_vel, init_acc
  REAL(REAL64), DIMENSION(0:1000,2) ::  pos_hist, vel_hist, acc_hist



  init_pos(1) = 0.0_REAL64
  init_pos(2) = 0.1_REAL64
  init_vel = 0.0_REAL64

  !Using the command line module provide by @Heather to get user input for nx and ny
  CALL parse_args 
  success = get_arg("nx", nx)
  success = get_arg("ny", ny)
  
  !This is the distance between any two consecutive grid points, note we have used equal spacing.
  dx = 2.0_REAL64/(nx-1) 
  dy = 2.0_REAL64/(ny-1)
  dt = 0.1_REAL64
  
  ALLOCATE(problem(1:nx, 1:ny))
  ALLOCATE(solution(0:nx+1, 0:ny+1))

  ALLOCATE(ex(1:nx, 1:ny))
  ALLOCATE(ey(1:nx, 1:ny))
  
  !Depending on which initial condition the user wants it creates the respective condition here problem = rho.
  !Also depending on the initial conditions, one can also change the initial position and velocity of the particle.
  success = get_arg("init", init_states)
  
  IF (init_states == 'null') THEN 
    problem = 0.0_REAL64
    init_pos = 0.0_REAL64
    init_vel = 0.1_REAL64

  ELSE IF (init_states == 'single') THEN 
    init_pos = (/0.1_REAL64,0.0_REAL64/)
    init_vel = 0.0_REAL64
    DO i = 1, nx 
      DO j = 1, ny 
        problem(i,j) = g(i,j,dx,dy,0.1_REAL64, 0.0_REAL64)
      END DO
    END DO
  ELSE IF (init_states == 'double') THEN 
    init_pos = (/ 0.0_REAL64, 0.5_REAL64/)
    init_vel = 0.0_REAL64
    DO i = 1, nx 
      DO j = 1, ny
        problem(i,j)=g(i,j,dx,dy,0.1_REAL64, 0.25_REAL64)+g(i,j,dx,dy,0.2_REAL64, -0.75_REAL64)
      END DO
    END DO 
  ELSE 
    PRINT*, 'Please choose from null, single or double.'
    STOP !Program should stop if an invalid input is entered.
  END IF
 
  !In this code solution = phi and problem = rho
  
  !To calculate phi
  solution = f_c(problem, dx,dy,nx,ny)
  
  !To apply the velocity Verlet algorythm
  CALL verlet_solver(solution, init_pos,init_vel,init_acc,pos_hist,vel_hist,acc_hist,dx,dy,dt,nx,ny,Ex,Ey)
  
  !Creates the netcdf file that stores all the relevant data.
  call writer_prototype(problem,solution(1:nx,1:ny),pos_hist, vel_hist, acc_hist,"results.nc",init_states,nx,ny,Ex,Ey,ierr)
 
END PROGRAM Main
