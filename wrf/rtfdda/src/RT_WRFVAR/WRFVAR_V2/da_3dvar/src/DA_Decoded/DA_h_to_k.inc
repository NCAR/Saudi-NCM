
 SUBROUTINE h_to_kmm5 (xb, pstar, height, kk, dk )
!------------------------------------------------------------------------------!
!  Return the index kk of the sigma level just above the input height and
!  the normalized distance dk to this sigma level.
!  Points above the first half sigma level are assigned (kk=0,    dk=0) 
!  Point below the last half sigma level are assigned   (kk=mkx-1,dk=1)
!  So that any vertical interpolation stated as:
!       val =  dk * aray (k+1) + (1-dk) * val (k) 
!  Will naturally extrapolate.
!
!
!  |------------------ Hsigmah(1) kk = 1, dk = 0
!  |
!  |------------------ Hsigmah (k)  
!  |      |       
!  |      |
!  |      +----------- height kk= k
!  |      |                   dk= [height-Hsigmah(k)/[Hsigmah (k+1)-Hsigmah (k)]
!  |      |
!  |------------------ Hsigmah (k+1)
!  |
!  |
!  |------------------ Hsigmah(mkx) kk = mkx-1, dk = 1
!  |                                
!  |                                kk = mkx,   dk = 0
!  |------------------ Terrain      - - - - - - - - -
!  |                                kk = mkx+1, dk = 0
!  |
!
! F. Vandenberghe, January 2003
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      TYPE (xb_type), INTENT (in)  :: xb
      REAL,    INTENT (in)  :: pstar
      REAL,    INTENT (in)  :: height
      REAL,    INTENT (out) :: dk
      INTEGER, INTENT (out) :: kk

      REAL    :: psigmah_above, psigmah_below
      REAL    :: hsigmah_above, hsigmah_below
      REAL    :: psigmaf_above, psigmaf_below
      REAL    :: hsigmaf_above, hsigmaf_below
      INTEGER :: k

!------------------------------------------------------------------------------!

! 1.1 Model last half sigmal level height at station location
!     ------------------------------------------------------

      psigmah_above = pstar * xb % sigmah (xb % kte) + xb % ptop

      call DA_Ref_height (psigmah_above, hsigmah_above)

! 1.2 Model last full sigmal level height at station location
!     ------------------------------------------------------

      psigmaf_above = xb % ptop

      call DA_Ref_height (psigmaf_above,hsigmaf_above)	

! 1.3 Model 1st half sigma level height at station location
!     ------------------------------------------------------

      psigmah_below = pstar * xb % sigmah (xb % kts) + xb % ptop

      call DA_Ref_height (psigmah_below, hsigmah_below)

! 1.4 Model 1st full sigma level height at station location
!     -------------------------------------------------------

      psigmaf_below = pstar + xb % ptop

      call DA_Ref_height (psigmaf_below,hsigmaf_below)

! 1.5 Extrapolation coef. above first full sigma level at station location
!     --------------------------------------------------------------------

      IF (height >= hsigmaf_above) THEN

          kk = xb % kte + 1
          dk = 0.

! 1.6 Extrapolation coef. above first half sigma level at station location
!     --------------------------------------------------------------------

      ELSE IF (height >= hsigmah_above) THEN

          kk = xb % kte 
          dk = 0.

! 1.7 Interpolation coef. on last half sigma level at station location
!     ----------------------------------------------------------------

      ELSE IF (height < hsigmaf_below) THEN

          kk = xb % kts - 1
          dk = 0.

! 1.8 Interpolation coef. on last half sigma level at station location
!     ----------------------------------------------------------------

      ELSE IF (height < hsigmah_below) THEN

          kk = xb % kts
          dk = 0

! 1.9 Vertical Interpolation coeficients between first and last half sigma level
!     --------------------------------------------------------------------------
      ELSE

          DO k = xb%kts, xb%kte - 1

             psigmah_above = pstar * xb % sigmah (k+1) + xb % ptop
             psigmah_below = pstar * xb % sigmah (k)   + xb % ptop

	     call DA_ref_height (psigmah_above, hsigmah_above)
	     call DA_ref_height (psigmah_below, hsigmah_below)

             IF (hsigmah_above >= height .AND. height > hsigmah_below) THEN
                 kk  = k
                 !dk = (hsigmah_above - height) &
		  dk = ( height - hsigmah_below) &
                    / (hsigmah_above - hsigmah_below)
                 EXIT
             ENDIF

          ENDDO

      ENDIF

!------------------------------------------------------------------------------!
 END SUBROUTINE h_to_kmm5
