module velocity_verlet
        use iso_fortran_env
        implicit none

        contains
        
                !>function for working out the indices for the cell's current position
                
                function  get_acc(exact_pos,dx,dy,Ex,Ey) result (acc)
                        real(kind=REAL64), dimension(2)                       :: exact_pos,acc
                        integer(kind=INT32)                                   :: cell_x, cell_y
                        real(kind=REAL64)                                     :: dx,dy
                        real(kind=REAL64), dimension(:,:)                     :: Ex,Ey

                        cell_x = floor((exact_pos(1) +1)/dx) + 1 !>I know the pdf says "-1.0_dp"
                        cell_y = floor((exact_pos(2) +1)/dy) + 1 !>But the maths seems weird to me here. Correct

                        acc(1) = -1.0_REAL64*Ex(cell_x,cell_y)
                        acc(2) = -1.0_REAL64*Ey(cell_x,cell_y)
                                                                    

                        return
                end function

                !>velocity verlet subroutine

                subroutine verlet_solver(field,init_pos,init_vel,init_acc,pos_hist,vel_hist,acc_hist,dx,dy,dt,nx,ny)
                        real(kind=REAL64),  dimension(0:,0:),     intent(in)  :: field ! Electric field which needs to be taken in
                                                                                 !Should it be a 3-D array with both dimensions or
                                                                                 !two 2D ones?

                        real(kind=REAL64),  dimension(2),       intent(in)    :: init_pos, init_vel !initial conditions
                        real(kind=REAL64),  dimension(2),       intent(inout) :: init_acc
                        real(kind=REAL64),  dimension(0:1000,2),intent(out)   :: pos_hist, vel_hist, acc_hist !time histories
                        real(kind=REAL64),                      intent(in)    :: dt,dx,dy !pass it as a type? feels clunky
                        integer(kind=INT32),                    intent(in)    :: nx,ny

                        integer(kind=INT32)                                   :: i,j
                        real(kind=REAL64),  dimension(:,:), allocatable       :: Ex,Ey
                        real(kind=REAL64), dimension(2)                       :: current_pos, current_acc

                        allocate(Ex(nx,ny))
                        allocate(Ey(nx,ny))

                        do j = 1,ny
                          do i = 1,nx
                            Ex(i,j) = (field(i+1,j) - field(i-1,j))/(2*dx)
                          end do
                        end do


                        do i = 1,nx
                          do j = 1,ny
                            Ey(i,j) = (field(i,j+1) - field(i,j-1))/(2*dy)
                          end do
                        end do
                                  


                        pos_hist(0,:) = init_pos   !Adding initial conditions
                        vel_hist(0,:) = init_vel
                       
                        init_acc = get_acc(init_pos,dx,dy,Ex,Ey)
                


                        acc_hist(0,:) = init_acc

                        !Actual Velocity Verlet Algorithm

                        do i = 1,1000
                        if (ABS(pos_hist(i-1,1)) > 1.0_REAL64 .OR. ABS(pos_hist(i-1,2)) > 1.0_REAL64) then
                           pos_hist(i,:) = pos_hist(i-1,:)
                           vel_hist(i,:) = vel_hist(i-1,:)
                           acc_hist(i,:) = acc_hist(i-1,:)
                        else

                            pos_hist(i,:) = pos_hist(i-1,:) + vel_hist(i-1,:)*dt + 0.5*(acc_hist(i-1,:)**2)*dt**2
                            current_pos = pos_hist(i,:)
                            current_acc = get_acc(current_pos,dx,dy,Ex,Ey)
                            acc_hist(i,:) = current_acc
                            vel_hist(i,:) = vel_hist(i-1,:) + 0.5*dt*(acc_hist(i,:)+acc_hist(i-1,:))
                        end if


                            print*, pos_hist(i,:), acc_hist(i,:)
                        end do 
                end subroutine
end module
