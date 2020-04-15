 SUBROUTINE x_to_j (xj, jx, jj, dxj)
   
!  Get the index jj and interpolation coeficient dxj of point xj on a grid.
!  Points outside the grid are extrapolated from the extremities of the grid.

   IMPLICIT NONE

   REAL,           INTENT (in)  :: xj
   INTEGER,        INTENT (in)  :: jx
   INTEGER,        INTENT (out) :: jj
   REAL,           INTENT (out) :: dxj
   
   IF (xj <  1.) THEN
       jj  = 1
       dxj = 0.
   ELSE if (xj >= REAL (jx)) THEN
       jj  = jx-1
       dxj = 1. 
   ELSE
       jj  = INT (xj)
       dxj = xj - REAL (jj)
   ENDIF

 END SUBROUTINE x_to_j
