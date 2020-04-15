!------------------------------------------------------------------------------
! Compute wind components, dew point, relative humidity and mixing ratio
! when the corresponding input variables are present.
!
! D. GILL,         April 1998
! F. VANDENBERGHE, Feb.  2003
!------------------------------------------------------------------------------

 SUBROUTINE diagnostics_moist (new)
!------------------------------------------------------------------------------

!  This routine computes the derived diagnostic moisture fields Td, Rh

!   USE module_decoded
!   USE module_func

   IMPLICIT NONE
   INTRINSIC MAX

   TYPE ( meas_data )    :: new

   REAL                  :: T_K , Td_K, P_mb, RH
   REAL                  :: es, qs
   REAL                  :: invdiffTd, invTd

!   INCLUDE 'constants.inc'
!!  INCLUDE 'missing.inc'

!------------------------------------------------------------------------------

   !  Given a valid temperature and dew point, the relative humidity is
   !  computed.  The relative humidity QC flag is arbitrarily set to the
   !  temperature flag value.


! 1.  DISCARD NEGATIVE OR NULL TEMPERATURE AND DEW POINT
! ======================================================

! 1.1 Discard negative or null temperature
!     ------------------------------------

      IF (eps_not (new%temperature%data, missing_r, 1.) .AND. &
                  (new%temperature%data  <=   0.)) THEN

                   new%temperature%data = missing_r
                   new%temperature%qc   = missing
!                  new%temperature%qc   = zero_t_td

      ENDIF

! 1.1 Discard negative or null dew point
!     ----------------------------------

      IF (eps_not (new%dew_point%data, missing_r, 1.) .AND. &
                  (new%dew_point%data  <=   0.)) THEN

                      new%dew_point%data = missing_r
                      new%dew_point%qc   = missing
!                     new%dew_point%qc   = zero_t_td

      ENDIF

! 2.  COMPUTE DIAGNOSTICS WHEN TEMPERATURE IS PRESENT
! ===================================================

      IF (eps_not (new%temperature%data, missing_r , 1.)) THEN

! 2.1 Compute RH, if missing, when Td is present
!     ------------------------------------------

      IF ((eps_equal (new%rh%data,        missing_r , 1.)) .AND. &
          (eps_not   (new%dew_point%data, missing_r , 1.))) THEN

           T_K  = MAX (new%temperature%data, 1.E-06)
           Td_K = MAX (new%dew_point%data,   1.E-06)
           RH   = 100. * EXP (L_over_Rv * (1./T_K - 1./Td_K))

           new%rh%data = RH
!          new%rh%qc   = MAX (new%temperature%qc, new%dew_point%qc)!less is best
           new%rh%qc   = MIN (new%temperature%qc, new%dew_point%qc)!more is best

! 2.2 Compute Td, if missing, when RH is present
!     ------------------------------------------

      ELSE IF ((eps_not   (new%rh%data,          missing_r , 1.)) .AND. &
               (eps_equal (new%dew_point%data,   missing_r , 1.))) THEN

           T_K  = MAX (new%temperature%data, 1.E-06)
           RH   = MAX (new%rh%data, 1.E-06)

           invdifftd = LOG (RH/100.) / L_over_Rv
           invtd = 1/T_K  - invdifftd
           Td_K  = 1. / invtd

           new%dew_point%data = Td_K
!          new%dew_point%qc   = MAX (new%temperature%qc, new%rh%qc)!less is best
           new%dew_point%qc   = MIN (new%temperature%qc, new%rh%qc)!more is best

      ENDIF

      ENDIF

! 3.  CHECK ON PERMISSIBLE VALUES
! ===============================

! 3.1 Rh must be between 0 and 100
!     -----------------------------
 
      IF (eps_not (new%rh%data, missing_r , 1.)) THEN
                   new%rh%data = MAX (new%rh%data, 1.E-06 )
                   new%rh%data = MIN (new%rh%data, 100.)
      ELSE
                   new%rh%qc = missing
      ENDIF

! 3.2 Td must be lower than T
!     -----------------------

      IF (eps_not (new%dew_point%data, missing_r , 1.)) THEN
                   new%dew_point%data = MIN (new%dew_point%data, &
                                             new%temperature%data)
      ELSE
                   new%dew_point%qc = missing
      ENDIF
    

 END SUBROUTINE diagnostics_moist
!------------------------------------------------------------------------------

 SUBROUTINE diagnostics_wind (new, longitude)
!------------------------------------------------------------------------------

!  This routine performs some check on wind speed and direction,
!  and compute the wind components on the Earth 
!  (positive blowing from North/East)

!   USE module_decoded
!   USE module_func

   IMPLICIT NONE
   INTRINSIC MAX

   TYPE ( meas_data )    :: new
   REAL                  :: longitude

   REAL :: aearth

!   INCLUDE 'constants.inc'
!!  INCLUDE 'missing.inc'

    REAL, PARAMETER :: conv = 180./ PI
!------------------------------------------------------------------------------

   ! Reject reports coming with wind speed and no direction, 

   IF      ((eps_equal (new%speed%data,     missing_r , 1.)) .OR. &
            (eps_equal (new%direction%data, missing_r , 1.))) THEN
             new%speed%data = missing_r
             new%speed%qc   = missing
             new%direction%data = missing_r
             new%direction%qc   = missing
   ENDIF

   !  First we need the meteorological u and v from the speed and direction.

   IF      ((eps_equal (new%speed%data,     missing_r , 1.)) .OR. &
            (eps_equal (new%direction%data, missing_r , 1.))) THEN

             new%u%data = missing_r
             new%u%qc   = missing
             new%v%data = missing_r
             new%v%qc   = missing

   ELSE IF ((eps_not (new%speed%data, missing_r , 1.)) .AND. &
                     (new%speed%data < 0.)) THEN

!            new%speed%qc = negative_spd
!            new%u%qc     = negative_spd
!            new%v%qc     = negative_spd
             new%speed%qc = missing
             new%u%qc     = missing
             new%v%qc     = missing
             new%u%data   = missing_r
             new%v%data   = missing_r

   ELSE IF ((eps_not (new%speed%data, missing_r , 1.)) .AND. &
                     (new%speed%data <= 0.)) THEN

!            new%speed%qc = zero_spd
!            new%u%qc     = zero_spd
!            new%v%qc     = zero_spd
             new%speed%qc = missing
             new%u%qc     = missing
             new%v%qc     = missing
             new%u%data   = missing_r
             new%v%data   = missing_r

   ELSE IF ((eps_not (new%direction%data, missing_r , 1.)) .AND. &
                    ((new%direction%data <    0.)   .OR. &
                     (new%direction%data >  360.))) THEN

!            new%direction%qc = wrong_direction
!            new%u%qc         = wrong_direction
!            new%v%qc         = wrong_direction
             new%direction%qc = missing
             new%u%qc         = missing
             new%v%qc         = missing
             new%u%data       = missing_r
             new%v%data       = missing_r

   ELSE

!  Simple rotation on Earth

!            aearth =  new%direction%data/conv
!            new%u%data = -new%speed%data*SIN(aearth)
!            new%v%data = -new%speed%data*COS(aearth)

!  Rotation on MM5 Grid
             
             CALL ffdduv (new%speed%data, new%direction%data, &
                          new%u%data, new%v%data, longitude, 1)

!  Update QC
!            new%u%qc   = MAX (new%speed%qc, new%direction%qc)! less is best
!            new%v%qc   = MAX (new%speed%qc, new%direction%qc)! less is best
             new%u%qc   = MIN (new%speed%qc, new%direction%qc)! more is best
             new%v%qc   = MIN (new%speed%qc, new%direction%qc)! more is best

   END IF

 END SUBROUTINE diagnostics_wind
!------------------------------------------------------------------------------!
