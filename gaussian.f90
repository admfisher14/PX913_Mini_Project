MODULE GAUSSIAN

  USE ISO_FORTRAN_ENV
 
  IMPLICIT NONE
 
  CONTAINS
 
  FUNCTION g(x,y,h,k,rho, sigma)
    REAL(REAL64), INTENT(IN) :: rho, sigma, h, k
    REAL(REAL64) :: g
    INTEGER :: x,y
   
    g = EXP(-((h*(x-1)-1+sigma)/rho)**2-((k*(y-1)-1 +sigma)/ rho)**2)
   
  END FUNCTION

END MODULE
