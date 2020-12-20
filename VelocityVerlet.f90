module velocity_verlet
        use iso_fortran_env
        implicit none

        contains



                !>function for working out the indices for the cell's current position
                
                function  current_cell(exact_pos) 
                        real(kind=REAL64), dimension(2)                       :: exact_pos
                        real(kind=int32) , dimension(2)                       :: current_cell

                        current_cell(1) = floor((exact_pos(1) +1.0_dp)/dx) + 1 !>I know the pdf says "-1.0_dp"
                        current_cell(2) = floor((exact_pos(1) +1.0_dp)/dy) + 1 !>But the maths seems weird to me here. Correct
                                                                               !>me if I'm wrong

                        return
                end function

                !>velocity verlet subroutine

                subroutine verlet_solver(field,init_pos,init_vel,init_acc,pos_hist,vel_hist,acc_hist,dx,dy,dt)
                        real(kind=REAL64),  dimension(:,:),     intent(in)    :: Ex, Ey ! Electric field which needs to be taken in
                                                                                 !Should it be a 3-D array with both dimension or a
                                                                                 !2D one?

                        real(kind=REAL64),  dimension(2),       intent(in)    :: init_pos, init_vel, init_acc !initial conditions
                        real(kind=REAL64),  dimension(0:1000,2),intent(out)   :: pos_hist, vel_hist, acc_hist !time histories
                        real(kind=REAL64),                      intent(in)    :: dt,dx,dy !pass it as a type? feels clunky
                        integer(kind=int32)                                   :: i



                        pos_hist(0) = init_pos   !Adding initial conditions
                        vel_hist(0) = init_vel
                       
                        init_acc(1) = -1* Ex(current_cell(init_pos))
                        init_acc(2) = -1* Ey(current_cell(init_pos))

                        vel_hist(0) = init_acc

                        !Actual Velocity Verlet Algorithm

                        do i = 1,1000
                            pos_hist(i,:) = pos_hist(i-1,:) + vel_hist(i-1,:)*dt + 0.5*(vel_hist(i-1,:)**2)*dt**2
                            acc_hist(i,1) = -1*Ex(current_cell(pos_hist(i-1,:)))
                            acc_hist(i,2) = -1*Ey(current_cell(pos_hist(i-1,:)))
                            vel_hist(i,:) = vel_hist(i-1,:) + 0.5*dt*(acc_hist(i,:)+acc_hist(i-1,:))
                        end do 



                end subroutine






end module
