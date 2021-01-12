!Simple module that gives the creates the Gaussian function
MODULE GAUSSIAN

  USE ISO_FORTRAN_ENV
 
  IMPLICIT NONE
 
  CONTAINS
 
  FUNCTION g(x,y,h,k,rho, sigma)
    !rho and sigma determine the position of the Gaussian, while h and k are dx, dy.
    REAL(REAL64), INTENT(IN) :: rho, sigma, h, k
    REAL(REAL64) :: g
    !This is for the loop variable
    INTEGER :: x,y
   
    g = EXP(-((h*(x-1)-1+sigma)/rho)**2-((k*(y-1)-1 +sigma)/ rho)**2)
   
  END FUNCTION g

END MODULE GAUSSIAN
