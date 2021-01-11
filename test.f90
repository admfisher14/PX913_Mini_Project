PROGRAM Main 

  USE ISO_FORTRAN_ENV
  USE EQ_FIELD
  USE velocity_verlet  
  USE command_line
  !USE domain_tools
  USE ERROR_CALC
  USE GAUSSIAN 

  IMPLICIT NONE 

  LOGICAL :: success 
  CHARACTER(LEN=20) :: init_states
  INTEGER(INT32) :: nx, ny ,i,j
  REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: problem, solution
  REAL(REAL64) :: dx,dy,dt,error
  REAL(REAL64), DIMENSION(2) :: init_pos,init_vel, init_acc
  REAL(REAL64), DIMENSION(0:1000,2) ::  pos_hist, vel_hist, acc_hist



  init_pos(1) = 0.0_REAL64
  init_pos(2) = 0.1_REAL64
  init_vel = 0.0_REAL64

  CALL parse_args 
  success = get_arg("nx", nx)
  success = get_arg("ny", ny)
  
  dx = 2.0_REAL64/(nx-1) 
  dy = 2.0_REAL64/(ny-1)
  dt = 0.1_REAL64
  
  ALLOCATE(problem(1:nx, 1:ny))
  ALLOCATE(solution(0:nx+1, 0:ny+1))
  
  success = get_arg("init", init_states)
  
  IF (init_states == 'null') THEN 
    problem = 0.0_REAL64 
  ELSE IF (init_states == 'single') THEN 
    DO i = 1, nx 
      DO j = 1, ny 
        problem(i,j) = g(i,j,dx,dy,0.1_REAL64, 0.0_REAL64)
      END DO
    END DO
  ELSE IF (init_states == 'double')
    DO i = 1, nx 
      DO j = 1, ny
        problem(i,j)=g(i,j,dx,dy,0.1_REAL64, 0.25_REAL64)+g(i,j,dx,dy,0.2_REAL64, -0.75_REAL64)
      END DO
    END DO 
  ELSE 
    PRINT*, 'Please choose from null, single or double.'
  
  !solution = f_c(problem, X, Y)
  
  solution = f_c(problem, dx,dy,nx,ny)
  

  CALL verlet_solver(solution, init_pos,init_vel,init_acc,pos_hist,vel_hist,acc_hist,dx,dy,dt,nx,ny,Ex,Ey)
  
   open(11,file="positions.txt",status='replace')

  do i = 0,1000
    write(11,*) pos_hist(i,:)
  end do

  close(11)
 
  open(12,file="velocities.txt",status='replace')

  do i = 0,1000
    write(12,*) vel_hist(i,:)
  end do

  close(12)

    open(13,file="acceleration.txt",status='replace')

  do i = 0,1000
    write(13,*) acc_hist(i,:)
  end do

  close(13)
 
 
END PROGRAM Main
