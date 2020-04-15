
 SUBROUTINE p_to_kmm5 (xb, xjc, yic, pstarc, pressure, kk, dk, &
                       pressure1d, kx )

!------------------------------------------------------------------------------!
!  Return the index kk of the sigma level just above the input pressure and
!  the normalized distance dk to this sigma level.
!  Points above the first half sigma level are assigned (kk=0,    dk=0)
!  Point below the last half sigma level are assigned   (kk=mkx-1,dk=1)
!  Point
!  So that any vertical interpolation stated as:
!       val =  dk * array (k+1) + (1-dk) * array (k)
!  Will naturally extrapolate.
!
!
!  |------------------ Pmm5 (1) kk = 0, dk = 0
!  |
!  |------------------ Pmm5 (k)
!  |      |
!  |      |
!  |      +----------- Press. dk = [Press.-Hmm5(k)/[Hmm5 (k+1)-Hmm5 (k)]
!  |      |                   kk = k
!  |      |
!  |------------------ Pmm5 (k+1)
!  |
!  |
!  |------------------ Pmm5(mkx) kk = mkx-1, dk = 1
!  |
!  |                             kk = mkx,   dk = 0
!  |------------------ Terrain   - - - - - - - - -
!  |                             kk = mkx+1, dk = 0

!
! F. Vandenberghe, January 2003
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      TYPE (xb_type),       INTENT (in)  :: xb
      REAL,                 INTENT (in)  :: xjc, yic
      REAL,                 INTENT (in)  :: pressure
      REAL,                 INTENT (in)  :: pstarc
      REAL,                 INTENT (out) :: dk
      INTEGER,              INTENT (out) :: kk
      INTEGER,              INTENT (in)  :: kx
      REAL, DIMENSION (kx), INTENT (in)  :: pressure1d

      REAL    :: psigmah_above, psigmah_below
      REAL    :: psigmaf_above, psigmaf_below
      REAL    :: pp_above, pp_below
      REAL    :: pfullh_above, pfullh_below
      REAL    :: pfullf_above, pfullf_below
      REAL    :: pfulle_above, pfulle_below
      REAL    :: ppe_above,    ppe_below
      REAL    :: dic, djc
      INTEGER :: ic, jc
      INTEGER :: k

!------------------------------------------------------------------------------!


! 0. Horizontal interpolation weights at obs location
! ===================================================

      CALL x_to_j (xjc, xb % ite - xb % its + 1, ic, dic)
      CALL x_to_j (yic, xb % jte - xb % jts + 1, jc, djc)


! 1.  PRESSURE ON SIGMA LEVELS
! ============================

! 1.1 Model last half sigma level height at obs location
!     -------------------------------------------------

      psigmah_above = pstarc * xb % sigmah (xb % kte) + xb % ptop

! 1.2 Model last full sigma level height at obs location
!     -------------------------------------------------

      psigmaf_above = xb % ptop

! 1.3 Model 1st half level heights at station location
!     -------------------------------------------------

      psigmah_below = pstarc * xb % sigmah (xb % kts) + xb % ptop

! 1.4 Model 1st full level heights at station location
!     -------------------------------------------------

      psigmaf_below = pstarc  + xb % ptop


! 2.  FULL PRESSURE AT MODEL LEVELS
! =================================

! 2.1 Pressure perturbations
!     ----------------------

      pp_above = pressure1d (xb % kte)
      pp_below = pressure1d (xb % kts)

! 2.2 Full pressures
!     --------------

      pfullh_above = psigmah_above + pp_above
      pfullf_above = psigmaf_above + pp_above

      pfullh_below = psigmah_below + pp_below
      pfullf_below = psigmaf_below + pp_below


! 3.  PROCESS DATA OUTSIDE DOMAIN VERTICAL EXTENSION
! ==================================================

! 3.1 Interpolation coef for obs above model 1st full sigma level
!     -----------------------------------------------------------

      IF (pressure <= pfullf_above) THEN

          kk = xb % kte + 1
          dk = 0.

! 3.2 Interpolation coef for obs above model 1st half sigma level
!     ------------------------------------------------------------

      ELSE IF (pressure <= pfullh_above) THEN

          kk = xb % kte
          dk = 0.

! 3.3 Interpolation coef for obs below model surface
!     ----------------------------------------------

      ELSE IF (pressure > pfullf_below) THEN

          kk = xb % kts - 1
          dk = 0.

! 3.4 Interpolation coef for obs at model surface
!     -------------------------------------------

      ELSE IF (pressure > pfullh_below) THEN

          kk = xb % kts
          dk = 0.

      ELSE

! 4.  PROCESS DATA OUTSIDE DOMAIN VERTICAL EXTENSION
! ==================================================

! 4.1 Loop over the levels
!     --------------------

          kk = 0
          dk = 0

          DO k = xb%kts, xb%kte-1

! 4.2 Pressure on sigma levels
!     ------------------------

             psigmah_above = pstarc * xb % sigmah (k+1) + xb % ptop
             psigmah_below = pstarc * xb % sigmah (k)   + xb % ptop

! 4.3 First estimate of the pressure pertubations (neares neighbourg)
!     --------------------------------------------------------------

             pp_above = pressure1d (k+1)
             pp_below = pressure1d (k)

! 4.4 Full pressures
!     --------------

             pfullh_above = psigmah_above + pp_above
             pfullh_below = psigmah_below + pp_below

! 4.5 Pressure levels below and above obs.
!     ------------------------------------

             IF (pfullh_above <= pressure .AND. pressure < pfullh_below) THEN

! 4.6 Interpolation coeficients
!     -------------------------

                 kk = k 
                 dk = (pfullh_above - pressure) &
                    / (pfullh_above - pfullh_below)

! 4.7 Levels were found, exit
!     -----------------------

                EXIT

             ENDIF

          ENDDO

! 4.8 Process error
!     -------------

          IF (kk <= 0 .AND. print_detail > 0) THEN
              CALL prelog
              WRITE (log_unit,'(A,I4,A)',ADVANCE='no') & 
             " Error: cannot recover height at pressure: ", &
              NINT (pressure/100.),"hPa,"
          ENDIF

      ENDIF

!------------------------------------------------------------------------------!
 END SUBROUTINE p_to_kmm5
