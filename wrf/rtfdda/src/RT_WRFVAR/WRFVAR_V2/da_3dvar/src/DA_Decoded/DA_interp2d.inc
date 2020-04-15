!------------------------------------------------------------------------------!

 FUNCTION interp2d (ii, di0, jj, dj0, field2d, dimi, dimj) &
 RESULT  (val)
!------------------------------------------------------------------------------!

    IMPLICIT NONE

    INTEGER :: ii, jj
    INTEGER :: dimi, dimj
    REAL    :: di0,  dj0
    REAL    :: val
    REAL, DIMENSION (dimi, dimj) :: field2d

    REAL    :: di1,  dj1
!------------------------------------------------------------------------------!
         
    di1 = 1. - di0
    dj1 = 1. - dj0

    val  = dj1*(  di1*field2d (ii,  jj  )  &
                + di0*field2d (ii+1,jj  )) &
         + dj0*(  di1*field2d (ii,  jj+1)  &
                + di0*field2d (ii+1,jj+1))

 END FUNCTION interp2d
!------------------------------------------------------------------------------!
