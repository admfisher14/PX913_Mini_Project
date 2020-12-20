module velocity_verlet
        use iso_fortran_env
        implicit none

        contains

                subroutine verlet_solver(field,init_pos,init_vel,init_acc,pos_hist,vel_hist,acc_hist)
                        real(kind=REAL64),  dimension(:,:),     intent(in)    :: field ! Electric field which needs to be taken in
                        real(kind=REAL64),  dimension(2),       intent(in)    :: init_pos, init_vel, init_acc !initial conditions
                        real(kind=REAL64),  dimension(0:1000,2),intent(out)   :: pos_hist, vel_hist, acc_hist !time histories
                        integer(kind=int32),dimension(2)                      :: current_cell


                        pos_hist(0) = init_pos   !Adding initial conditions
                        vel_hist(0) = init_vel

                        current_cell(1)  = floor((init_pos(1) + 1.0_dp)/dx) + 1 !I know the pdf says "-1.0_dp"
                        current_cell(2)  = floor((init_pos(2) + 1.0_dp)/dy) + 1 !But the maths seems weird to me here. Correct me if
                                                                                !I'm wrong

                        init_acc = field(current_cell(1),current_cell(2))       ! Can I just replace this with field(current_cell) ?
                        vel_hist(0) =i init_acc







end module
