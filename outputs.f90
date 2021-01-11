

MODULE write_netcdf

    USE ISO_FORTRAN_ENV
    USE netcdf
  
    IMPLICIT NONE
  
    CONTAINS
  
    ! This module contains code used and modified from workshop 7 of the px913 course 
  
    SUBROUTINE writer_prototype(larr,pos_hist, vel_hist, acc_hist,filename,init,nx,ny ierr)
  
      INTEGER, INTENT(IN), DIMENSION(:,:) :: larr
      INTEGER, PARAMETER :: ndims = 2
      ! We can use this parameter here, which makes it easier to
      ! replicate this function for different dimensionalities
      INTEGER, DIMENSION(ndims) :: sizes, dim_ids,pos_size,vel_size,acc_size,hist_ids
      CHARACTER(LEN=1), DIMENSION(ndims) :: dims=(/"x", "y" /)
      CHARACTER(LEN=1), DIMENSION(ndims) :: hist_dims=(/"x,y", "t" /)
      CHARACTER(LEN=*), INTENT(IN) :: filename
      INTEGER :: ierr, file_id, var_id, i,ac_id,ac_dims,pos_id,vel_id,acc_id
      CHARACTER(LEN=10) :: init
      REAL(REAL64), DIMENSION(0:1000,2) ::  pos_hist, vel_hist, acc_hist
      
  
  
      sizes = SHAPE(larr)
      pos_size = shape(pos_hist)

  
      ! Create the file, overwriting if it exists
      ierr = nf90_create(filename, NF90_CLOBBER, file_id)
  
      ! I don't want to bomb if there is an error, rather return to caller
      ! This is tricky to do from another sub. so I choose this instead
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
  
      ! Now I am going to do several actions, checking and printing each one
      ! I am using a loop here, to save a bit of duplication. In higher
      ! dimensions, this would really help!
      ierr = nf90_put_att(file_id,NF90_GLOBAL,"ny",ny)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

    ierr = nf90_put_att(file_id,NF90_GLOBAL,"nx",nx)
    IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
    END IF

  
      ierr = nf90_put_att(file_id,NF90_GLOBAL,"inital_setup",init)
      IF (ierr /= nf90_noerr) THEN
          PRINT*, TRIM(nf90_strerror(ierr))
          RETURN
      END IF
  
  
      DO i = 1, ndims
        ierr = nf90_def_dim(file_id, dims(i), sizes(i), dim_ids(i))
        IF (ierr /= nf90_noerr) THEN
          PRINT*, TRIM(nf90_strerror(ierr))
          RETURN
        END IF
        ierr = nf90_def_dim(file_id, hist_dims(i), pos_sizes(i), hist_ids(i))
        IF (ierr /= nf90_noerr) THEN
          PRINT*, TRIM(nf90_strerror(ierr))
          RETURN
        END IF
      END DO
  


  
      ! Define variable type, matching our array
      ierr = nf90_def_var(file_id, "solution", NF90_REAL, dim_ids, var_id)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
  
      ierr = nf90_def_var(file_id, "pos", NF90_REAL, hist_ids, pos_id)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
      ierr = nf90_def_var(file_id, "acc", NF90_REAL, hist_ids, acc_id)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
      ierr = nf90_def_var(file_id, "vel", NF90_REAL, hist_ids, vel_id)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF

  
      ! Finish defining metadata
      ierr = nf90_enddef(file_id)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
  
  
      ! Actually write the variable
      ierr = nf90_put_var(file_id, var_id, larr)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF

      ierr = nf90_put_var(file_id, pos_id, pos_hist)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
      ierr = nf90_put_var(file_id, vel_id, vel_hist)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
      ierr = nf90_put_var(file_id, acc_id, acc_hist)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
  

      ! Close the file
      ierr = nf90_close(file_id)
      IF (ierr /= nf90_noerr) THEN
        PRINT*, TRIM(nf90_strerror(ierr))
        RETURN
      END IF
  
    END SUBROUTINE writer_prototype
  
  
  END MODULE write_netcdf