
 SUBROUTINE recoverhp (height, pressure, xp, kts, kte, &
                       v_h, v_p, v_t, v_q, &
                       h_sfc, p_sfc, p_slp)
!------------------------------------------------------------------------------!
! Recover height or pressure at a station location from first model 
! when one or the other is missing.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2004 - 2006.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, August 2006.
!------------------------------------------------------------------------------!
      IMPLICIT NONE
      INTRINSIC MAX

      TYPE (field),         INTENT (inout) :: pressure, height
      TYPE (multi_level_type)              :: platform
      TYPE (xpose_type), INTENT(IN)        :: xp
      INTEGER                              :: kts, kte
      REAL, DIMENSION (kts:kte)            :: v_h, v_p
      REAL, DIMENSION (kts:kte)            :: v_t, v_q
      REAL                                 :: h_sfc, p_sfc, p_slp

      REAL    :: h_obs, p_obs
      REAL    :: h_low, p_low
      REAL    :: h_slp
      REAL    :: aa, bb

      REAL    :: z0, dz0, dz1
      INTEGER :: k0

!------------------------------------------------------------------------------!

! 0.  PRINT-OUT 
! =============

! 0.1 Set sea level height to 0
!     -------------------------
      h_slp = 0.

! 0.2 Print-out input
!     ---------------

      IF (print_detail >= 3) THEN

      WRITE (*,'(/,A)') " -----------------------------------------------------"
     
      ! Station elevation and surface pressure
      WRITE (*,'(2(A,F9.0))') " h_obs = ",height%data, &
                              " p_obs = ",pressure%data

      ! Model highest half sigma level height and pressure at station location
      WRITE (*,'(2(A,F9.0))') " h_hig = ",v_h (kte), &
                              " p_hih = ",v_p (kte)

      ! Model lowest half sigma level height and pressure at station location
      WRITE (*,'(2(A,F9.0))') " h_low = ",v_h (kts), &
                              " p_low = ",v_p (kts)

      ! Model elevation and surface full pressure
      WRITE (*,'(2(A,F9.0))') " h_sfc = ",h_sfc, &
                              " p_sfc = ",p_sfc

      ! Model sea level height and pressure
      WRITE (*,'(2(A,F9.0))') " h_slp = ",h_slp, &
                              " p_slp = ",p_slp

      ENDIF

! 1.  PRESSURE IS MISSING BUT HEIGHT IS AVAILABLE
! ===============================================

      IF ((eps_equal (pressure % data, missing_r, 1.)) .AND. &
          (eps_not   (height   % data, missing_r, 1.))) THEN

         
! 1.1 Discard obs above first model half sigma layer
!     ----------------------------------------------

           IF (height % data > v_h (kte)) THEN

               IF (print_detail >= 2) &
               WRITE (log_unit,'(A)',ADVANCE='no') " Height above highest level"

               RETURN

! 1.2 Regular interpolation between lowest and highest half sigma layers
!     ------------------------------------------------------------------

           ELSE IF (height % data > v_h (kts)) THEN
           
               h_obs = height % data

               CALL to_zk  (h_obs, v_h, xp, v_interp_h, z0) 
               CALL toGrid (z0, kts, kte, k0, dz0, dz1)

               p_obs = dz1*v_p(k0) + dz0*v_p(k0+1)

               pressure % data = p_obs
               pressure % qc   = first_guess

               IF (print_detail >= 3) THEN
               WRITE (*,'(A)') "Missing pressure between levels:"
               WRITE (*,'(A,I2,A,I4)') " h(",k0+1,")  = ",NINT (v_h(k0+1))
               WRITE (*,'(A,I4)')      " h_obs  = ",      NINT (h_obs)
               WRITE (*,'(A,I2,A,I4)') " h(",k0,")  = ",  NINT (v_h(k0))
               WRITE (*,'(A,I2,A,I4)') " p(",k0+1,")  = ",NINT (v_p(k0+1)/100.)
               WRITE (*,'(A,I4)')      " p_obs  = ",      NINT (p_obs/100.)
               WRITE (*,'(A,I2,A,I4)') " p(",k0,")  = ",  NINT (v_p(k0)/100.)
               ENDIF

           ELSE

! 1.3 Interpolate between lowest sigma layer and model surface
!     --------------------------------------------------------

               IF (height % data > h_sfc) THEN

                   h_obs = height % data

                   k0   =  kts
                   dz0 = -(h_obs - v_h (k0))/(v_h (k0)-h_sfc)
                   dz1 =  (h_obs - h_sfc)   /(v_h (k0)-h_sfc)

                   p_obs = dz1*v_p(k0) + dz0*p_sfc

                   pressure % data = p_obs
                   pressure % qc   = first_guess

                   IF (print_detail >= 3) THEN
                   WRITE (*,'(A)') "Missing pressure below lowest levels:"
                   WRITE (*,'(A,I2,A,I4)') " h(",k0,")  = ",NINT (v_h(k0))
                   WRITE (*,'(A,I4)')      " h_obs  = ",    NINT (h_obs)
                   WRITE (*,'(A,I4)')      " h_sfc  = ",    NINT (h_sfc)
                   WRITE (*,'(A,I2,A,I4)') " p(",k0,")  = ",NINT (v_p(k0)/100.)
                   WRITE (*,'(A,I4)')      " p_obs  = ",    NINT (p_obs/100.)
                   WRITE (*,'(A,I4)')      " p_sfc  = ",    NINT (p_sfc/100.)
                   ENDIF

               ELSE

! 1.4 Interpolate between model surface and slp
!     -----------------------------------------

                   h_obs = height % data

                   CALL DA_TPQ_To_Pbelow (v_t, v_q, v_p, h_sfc, p_sfc, &
                                          h_obs, p_obs, xp)

                   pressure % data = p_obs
                   pressure % qc   = first_guess

                   IF (print_detail >= 3) THEN
                   WRITE (*,'(A)') "Missing pressure below surface:"
                   WRITE (*,'(A,I4)') " h_sfc  = ", NINT (h_sfc)
                   WRITE (*,'(A,I4)') " h_obs  = ", NINT (h_obs)
                   WRITE (*,'(A,I4)') " h_slp  = ", NINT (h_slp)
                   WRITE (*,'(A,I4)') " p_sfc  = ", NINT (p_sfc/100.)
                   WRITE (*,'(A,I4)') " p_obs  = ", NINT (p_obs/100.)
                   WRITE (*,'(A,I4)') " p_slp  = ", NINT (p_slp/100.)
                   ENDIF

               ENDIF

           ENDIF

      ENDIF


! 2.  HEIGHT IS MISSING BUT PRESSURE IS AVAILABLE
! ===============================================

      IF ((eps_not   (pressure % data, missing_r, 1.)) .AND. &
          (eps_equal (height   % data, missing_r, 1.))) THEN

         
! 2.1 Discard obs above first model half sigma layer
!     ----------------------------------------------

           IF (pressure % data < v_h (kte)) THEN

               IF (print_detail >= 2) &
               WRITE (log_unit,'(A)',ADVANCE='no') " Pressure above highest level"

               RETURN

! 2.2 Regular interpolation between lowest and highest half sigma layers
!     ------------------------------------------------------------------

           ELSE IF (pressure % data < v_p (kts)) THEN

               p_obs = pressure % data

               CALL to_zk  (p_obs, v_p, xp, v_interp_p, z0) 
               CALL toGrid (z0, kts, kte, k0, dz0, dz1)

               h_obs = dz1*v_h(k0) + dz0*v_h(k0+1)

               height % data = h_obs
               height % qc   = first_guess

               IF (print_detail >= 3) THEN
               WRITE (*,'(A)') "Missing height between levels:"
               WRITE (*,'(A,I2,A,I4)') " h(",k0+1,")  = ",NINT (v_h(k0+1))
               WRITE (*,'(A,I4)')      " h_obs  = ",     NINT (h_obs)
               WRITE (*,'(A,I2,A,I4)') " h(",k0,")  = ",  NINT (v_h(k0))
               WRITE (*,'(A,I2,A,I4)') " p(",k0+1,")  = ",NINT (v_p(k0+1)/100.)
               WRITE (*,'(A,I4)')      " p_obs  = ",     NINT (p_obs/100.)
               WRITE (*,'(A,I2,A,I4)') " p(",k0,")  = ",  NINT (v_p(k0)/100.)
               ENDIF

           ELSE
 
! 2.3 Interpolate height between lowest sigma layer and model surface
!     ---------------------------------------------------------------

               IF (pressure % data < p_sfc) THEN

                   p_obs = pressure % data

                   k0   =  kts
                   dz0 = -(p_obs - v_p (k0))/(v_p (k0)-p_sfc)
                   dz1 =  (p_obs - p_sfc)    /(v_p (k0)-p_sfc)

                   h_obs = dz1*v_h(k0) + dz0*h_sfc

                   height % data = h_obs 
                   height % qc   = first_guess

                   IF (print_detail >= 3) THEN
                   WRITE (*,'(A)') "Missing pressure below lowest:"
                   WRITE (*,'(A,I2,A,I4)')  " h(",k0,") = ",NINT (v_h(k0))
                   WRITE (*,'(A,I4)')       " h_obs  = ",   NINT (h_obs)
                   WRITE (*,'(A,I4)')       " h_sfc  = ",   NINT (h_sfc)
                   WRITE (*,'(A,I2,A,I4)')  " p(",k0,") = ",NINT (v_p(k0)/100.)
                   WRITE (*,'(A,I4)')       " p_obs  = ",   NINT (p_obs/100.)
                   WRITE (*,'(A,I4)')       " p_sfc  = ",   NINT (p_sfc/100.)
                   ENDIF


               ELSE

! 2.4 Interpolate height between model surface and slp
!     ------------------------------------------------

                   p_obs = pressure % data

                   CALL DA_TPQ_To_Hbelow (v_t, v_q, v_p, h_sfc, p_sfc, &
                                          p_obs, h_obs, xp)

                   height % data = h_obs
                   height % qc   = first_guess

                   IF (print_detail >= 3) THEN
                   WRITE (*,'(A)') "Missing pressure below surface:"
                   WRITE (*,'(A,I4)')       " h_sfc = ", NINT (h_sfc)
                   WRITE (*,'(A,I4)')       " h_obs = ", NINT (h_obs)
                   WRITE (*,'(A,I4)')       " h_slp = ", NINT (h_slp)
                   WRITE (*,'(A,I4)')       " p_sfc = ", NINT (p_sfc/100.)
                   WRITE (*,'(A,I4)')       " p_obs = ", NINT (p_obs/100.)
                   WRITE (*,'(A,I4)')       " p_slp = ", NINT (p_slp/100.)
                   ENDIF

	       ENDIF

           ENDIF


! 2.4 Make sure the height is positive (1mm above sea level)
!     ------------------------------------------------------

           height % data = MAX (height % data, 0.001)

      ENDIF


      IF (print_detail >= 3) &
      WRITE (*,'(A)') " -----------------------------------------------------"

!------------------------------------------------------------------------------!
 END SUBROUTINE recoverhp
