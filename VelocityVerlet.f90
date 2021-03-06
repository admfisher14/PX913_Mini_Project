module velocity_verlet
        use iso_fortran_env
        implicit none

        contains
        
                !>function for working out the acceleration from the cell's current position
                
                function  get_acc(exact_pos,dx,dy,Ex,Ey) result (acc)
                        real(kind=REAL64), dimension(2)                       :: exact_pos,acc
                        integer(kind=INT32)                                   :: cell_x, cell_y
                        real(kind=REAL64)                                     :: dx,dy
                        real(kind=REAL64), dimension(:,:)                     :: Ex,Ey

                        cell_x = floor((exact_pos(1) +1.0_REAL64)/dx) + 1 !>Mathematical formula provided in pdf for calculating
                        cell_y = floor((exact_pos(2) +1.0_REAL64)/dy) + 1 !>particle position appears to be slightly wrong

                        acc(1) = -1.0_REAL64*Ex(cell_x,cell_y)      !> Calculating acceleration based on potential gradient in x and y directions
                        acc(2) = -1.0_REAL64*Ey(cell_x,cell_y)
                                                                    

                        return
                end function

                !>velocity verlet subroutine

                subroutine verlet_solver(field,init_pos,init_vel,init_acc,pos_hist,vel_hist,acc_hist,dx,dy,dt,nx,ny, Ex,Ey)
                        real(kind=REAL64),  dimension(0:,0:),     intent(in)  :: field ! Electric field which needs to be taken in
                                                                                 !Should it be a 3-D array with both dimensions or
                                                                                 !two 2D ones?

                        real(kind=REAL64),  dimension(2),       intent(in)    :: init_pos, init_vel !initial conditions
                        real(kind=REAL64),  dimension(2),       intent(inout) :: init_acc !to be calculated inside the function
                        real(kind=REAL64),  dimension(0:1000,2),intent(out)   :: pos_hist, vel_hist, acc_hist !time histories
                        real(kind=REAL64),                      intent(in)    :: dt,dx,dy 
                        integer(kind=INT32),                    intent(in)    :: nx,ny 

                        integer(kind=INT32)                                             :: i,j 
                        real(kind=REAL64),  dimension(:,:), allocatable, intent(out)    :: Ex,Ey
                        real(kind=REAL64), dimension(2) :: current_pos, current_acc
                        
                        
                        !> Allocation and calculation of potential gradients in the x and y directions using finite differences
                        
                        allocate(Ex(nx,ny))
                        allocate(Ey(nx,ny))

                        do j = 1,ny
                          do i = 1,nx
                            Ex(i,j) = (field(i+1,j) - field(i-1,j))/(2.0_REAL64*dx)
                          end do
                        end do
                        
                        do i = 1,nx
                          do j = 1,ny
                            Ey(i,j) = (field(i,j+1) - field(i,j-1))/(2.0_REAL64*dy)
                          end do
                        end do
                                  
                        pos_hist(0,:) = init_pos   !Adding initial conditions
                        vel_hist(0,:) = init_vel
                        
        
                        init_acc = get_acc(init_pos,dx,dy,Ex,Ey)
                


                        acc_hist(0,:) = init_acc

                        !> Actual Velocity Verlet Algorithm, calculating displacement, velocity and acceleration
                        !> at each timestep and appending it onto the velocity histories

                        do i = 1,1000
                        !> If-else statement stops the particle from moving if the particle moves out of bounds.
                        if (ABS(pos_hist(i-1,1)) > 1.0_REAL64 .OR. ABS(pos_hist(i-1,2)) > 1.0_REAL64) then
                           pos_hist(i,:) = pos_hist(i-1,:)
                           vel_hist(i,:) = vel_hist(i-1,:)
                           acc_hist(i,:) = acc_hist(i-1,:)
                        else

                            pos_hist(i,:) = pos_hist(i-1,:) + vel_hist(i-1,:)*dt + &
                             0.5_REAL64*(acc_hist(i-1,:)*acc_hist(i-1,:))*dt*dt 
                            current_pos = pos_hist(i,:)
                            current_acc = get_acc(current_pos,dx,dy,Ex,Ey)
                            acc_hist(i,:) = current_acc
                            vel_hist(i,:) = vel_hist(i-1,:) + 0.5_REAL64*dt*(acc_hist(i,:)+acc_hist(i-1,:))
                        end if

                        end do 
                end subroutine
end module
