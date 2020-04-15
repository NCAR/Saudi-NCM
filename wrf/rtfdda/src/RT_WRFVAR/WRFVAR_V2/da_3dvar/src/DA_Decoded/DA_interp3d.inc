!------------------------------------------------------------------------------!

 FUNCTION interp3d (ii, di0, jj, dj0, kk, dk0, field3d, dimi, dimj, dimk) &
 RESULT  (val)
!------------------------------------------------------------------------------!

    IMPLICIT NONE

    INTEGER :: ii, jj, kk
    INTEGER :: dimi, dimj, dimk
    REAL    :: di0,  dj0, dk0 
    REAL    :: val
    REAL, DIMENSION (dimi, dimj, dimk) :: field3d

    REAL    :: di1,  dj1, dk1 
    REAL    :: val0, val1
!------------------------------------------------------------------------------!
         
    di1 = 1. - di0
    dj1 = 1. - dj0
    dk1 = 1. - dk0

    val0 = dj1*(  di1*field3d (ii,  jj,  kk)  &
                + di0*field3d (ii+1,jj,  kk)) &
         + dj0*(  di1*field3d (ii,  jj+1,kk)  &
                + di0*field3d (ii+1,jj+1,kk))

    val1 = dj1*(  di1*field3d (ii,  jj,  kk+1)  &
                + di0*field3d (ii+1,jj,  kk+1)) &
         + dj0*(  di1*field3d (ii,  jj+1,kk+1)  &
                + di0*field3d (ii+1,jj+1,kk+1))

   val =  dk0 * val1 + dk1 * val0

 END FUNCTION interp3d
!------------------------------------------------------------------------------!

 SUBROUTINE interp3d_to_1d (xj, yi, field3d, dimi, dimj, dimk, field1d)
!------------------------------------------------------------------------------!

    IMPLICIT NONE

    REAL    :: xj, yi
    INTEGER :: dimi, dimj, dimk
    REAL, DIMENSION (dimi, dimj, dimk) :: field3d
    REAL, DIMENSION (dimk)             :: field1d

    INTEGER :: ii, jj, kk
    REAL    :: di0,  dj0
    REAL    :: di1,  dj1
!------------------------------------------------------------------------------!
         
    CALL  x_to_j (xj, dimj, jj, dj0)
    CALL  x_to_j (yi, dimi, ii, di0)

    di1 = 1. - di0
    dj1 = 1. - dj0

    DO kk = 1, dimk

       field1d (kk) = dj1*(  di1*field3d (ii,  jj,  kk)  &
                           + di0*field3d (ii+1,jj,  kk)) &
                    + dj0*(  di1*field3d (ii,  jj+1,kk)  &
                           + di0*field3d (ii+1,jj+1,kk))

   ENDDO

 END SUBROUTINE interp3d_to_1d
