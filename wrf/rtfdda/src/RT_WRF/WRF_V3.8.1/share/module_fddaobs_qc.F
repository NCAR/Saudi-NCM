 MODULE module_fddaobs_qc
!----------------------------------------------------------------------------!
! Routines to perform FDDA QC check.
!
! Reports are compared to model interpolated values at the obs locations.
! Differences |O-B| are scaled by the expected obserational error Err and the
! the ratio |O-B|/Err is mapped between 0 and 10 according to a Gaussian
! mapping function with standard deviation "tolerance" defined in namelist.
! QC values between 0 and 10 are coded on the 6th and 7th bit of the field QC 
! integer flag.
!
! It is assumed that model and observational error interpolated values 
! at observation locations are available in the same data structure
! than the obserations.
! 
! QC is performed on specific humidity when namelist parameter
! is set to 1. It is performed on relative humidity otherwise.
! Specific humidity, relative humidity are assigned the same QC value. 
! Wind speed, direction, u and v are assigned the same QC value. 
! 
! THE STORAGE ORDER IN ERRF IS AS FOLLOWS:
!
!        IVAR                VARIABLE TYPE(TAU-1)
!        ----                --------------------
!         1                    U error
!         2                    V error
!         3                    T error
!         4                    Q error
!         5                    Surface press error at T points (not used)
!         6                    Model surface press at T-points
!         7                    Model surface press at U-points
!         8                    Model surface press at V-points
!         9                    RKO at U-points
!
! MISSING DATA OBSERVATIONS ARE DENOTED BY THE DUMMY VALUE 99999.

!----------------------------------------------------------------------------!
! Copyright UCAR (c) 2010.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, September 2010.
!----------------------------------------------------------------------------!
  USE module_model_constants, ONLY : rcp
!----------------------------------------------------------------------------!
  IMPLICIT NONE


! REAL, PARAMETER :: &
!         tolerance_p_sound = 3.0, &
!         tolerance_q_sound = 2.0, &
!         tolerance_t_sound = 3.0, & 
!         tolerance_u_sound = 3.0

! REAL, PARAMETER :: &
!         tolerance_p = 1.6, &
!         tolerance_q = 1.0, &
!         tolerance_t = 2.2, &
!         tolerance_u = 2.0

!! wuyh modified 20130124
!  INTEGER, PARAMETER :: cv_options_hum = 1  ! 1 QC on specific humidity else
!! wuyh modified 20130124
                                            ! 0 QC on relative humidity

  INTEGER :: missing
  REAL    :: missing_r

 CONTAINS

!*******************************************************************
 Subroutine fddaobs_qc (iobs,platform,nsndlev,missing_flag,          &
                      tolerance,tolerance_sound,                     &
                      ivar,cv_options_hum,vobs,prs_obs,errf,oldqc,   &
                      obs_pottemp,t0,lprint,qcr)
!*******************************************************************
    USE module_observ_error_ncep , ONLY : observ_error_ncep_intp
!-------------------------------------------------------------------!
    IMPLICIT NONE

! ARGUMENTS

  INTEGER :: iobs           !series number of obs 
  CHARACTER*40 :: platform
  Real,Parameter::eps=1.0E-6

  INTEGER :: nsndlev
  REAL    :: missing_flag
  REAL    :: tolerance
!  REAL    :: tolerance_q,tolerance_t, tolerance_u
  REAL    :: tolerance_sound
!  REAL    :: tolerance_q_sound,tolerance_t_sound, tolerance_u_sound
  INTEGER :: cv_options_hum   ! 1 QC on specific humidity else
  REAL :: errf
  REAL :: vobs
  REAL :: oldqc
  REAL :: prs_obs

!! wuyh added
  REAL  :: obs_pottemp,t0   !for temperature only
!! wuyh added end

  LOGICAL :: lprint

  REAL :: qcr

  REAL :: temp_mdl, temp_obs
  REAL :: qv_obs, qs_obs 
  REAL :: qv_mdl, qs_mdl
!-------------------------------------------------------------------!
! LOCAL VARIABLES

  INTEGER :: ivar
  CHARACTER*200 :: error_message
  CHARACTER*2, DIMENSION(6)::var_name=(/"U ","V ","T ","Q ","P ","H "/)
  REAL    :: oberr, pob, poberr
!  REAL    :: sigma_p, sigma_q, sigma_u, sigma_t
  REAL   :: tolerances
  INTEGER :: qc
  REAL, PARAMETER :: zero = 0.
!--------------------------------------------------------------------!

! 1.  INITIALISATION
! ==================

! 1.1 Reset qc
!     --------
!      qc=INT(oldqc*10.+0.01) 
      qc  = 0
      qcr = 0.

! 1.2 Missing flag
!     ------------

      missing_r = missing_flag
      missing   = NINT (missing_flag)

! 1.3 Without pressure we cannot do anything
!     --------------------------------------

      IF (eps_equal (prs_obs, missing_r, 1.)) THEN
           IF (lprint) THEN
               WRITE (error_message,'(A)') &
             " Pressure is missing, returning missing QC"
               CALL wrf_message (error_message)
           ENDIF
           qc  = missing
           qcr = missing_r
!           WRITE (269,'(A14,I6,F10.0,I6)') &
!                  platform,iobs,prs_obs,NINT(10.*oldqc(5))   !no oldqc(5) now
           RETURN
      ENDIF

! 1.4 Load the pressure
!     -----------------

      pob =  10.*prs_obs   ! Obs Pressure in hPa

! 1.5 QC standard deviations are different for soundings
!     --------------------------------------------------

      IF (platform (1:11) == "FM-35 TEMP " .OR. &
          platform (1:11) == "FM-35 PIBAL" .OR. &
          platform (1:11) == "FM-32 PILOT" .OR. &
          platform (1:11) == "FM-32 TEMP " .OR. &
          platform (1:5)  == "FM-12"       .OR. & 
! wuyh added 20130816
          platform (1:11) == "FM-97 MDCRS")THEN 
! wuyh added 20130816
!          sigma_p = tolerance_p_sound
!          sigma_q = tolerance_q_sound
!          sigma_u = tolerance_u_sound
!          sigma_t = tolerance_t_sound
          tolerances = tolerance_sound
!          tolerances (2) = tolerance_u_sound
!          tolerances (3) = tolerance_t_sound
!          tolerances (4) = tolerance_q_sound
!          tolerances (5) = tolerance_p_sound
      ELSE
          IF (nsndlev .GT. 1 .AND. lprint) THEN
              WRITE (error_message,'(A,I6,2A)') &
            " WARNING: multi-level observation ",iobs,&
            " has platform ",TRIM(platform)
              CALL wrf_message (error_message)
          ENDIF
!          sigma_p = tolerance_p
!          sigma_q = tolerance_q
!          sigma_u = tolerance_u
!          sigma_t = tolerance_t
          tolerances  = tolerance
!          tolerances (2) = tolerance_u
!          tolerances (3) = tolerance_t
!          tolerances (4) = tolerance_q
!          tolerances (5) = tolerance_p
      ENDIF


! 2.  LOOP OVER VARIABLES TO QC
! =============================


! 2.1 If the data is missing, no need to QC
!     -------------------------------------

      IF (eps_equal(vobs,missing_r,1.)) THEN
          IF (lprint) THEN
              WRITE (error_message,'(A,I6,A,I2,A)') &
            " Observation ",iobs, &
            " Variable ",ivar,"  is missing, returning missing QC"
              CALL wrf_message (error_message)
          ENDIF
          qc   = missing
          qcr  = missing_r
          oberr = missing_r
!          WRITE(168,'(A14,A2,I8,F10.0,F10.2,F10.2,F10.2,F10.2,2I8)') &
!             platform, var_name(ivar), &
!             iobs,pob, vobs,           &
!             errf,oberr,tolerances,    &
!             qc, NINT(oldqc)
             Return
      ENDIF

! 2.2 If ERRF is 0, no need to assimilate either
!     ------------------------------------------

      IF (ABS (errf) .LE. eps) THEN
!          IF (lprint) THEN
!              WRITE (error_message,'(A,I6,A,I2,A)') &
!            " Observation ",iobs, &
!            " Variable ",ivar,"  innovation is 0, returning null QC"
!              CALL wrf_message (error_message)
!          ENDIF
! wuyh added 20110715
          qcr   = 10.
          oberr = 0.
!          qc (ivar) = 0      !wuyh removed
!          oberr = missing_r  !wuyh removed
! wuyh added end 20110715
!          WRITE (169,'(A14,A2,I8,F10.0,F10.2,F10.2,F10.2,F10.2,2I8)') &
!             platform, var_name(ivar), &
!             iobs,pob, vobs,   &
!             errf, oberr,tolerances,   &
!             qc, NINT(oldqc)
          Return
      ENDIF

! 2.3 Observational error
!     -------------------

      oberr = observ_error_ncep_intp &
             (ivar,pob,platform,cv_options_hum,missing_r)


! 3.  TEMPERATURE QC
! ==================
      IF (ivar .eq. 3) THEN

! 3.1 ERRF is potential temperature error, convert back to temperature
!     -----------------------------------
!!! wuyh added
!             temp_obs = vobs (ivar)*(1000/pob)**rcp  ! Obs potential temp
!             temp_obs = obs_pottemp+t0        ! Obs potential temp
!             temp_mdl = temp_obs - errf ! Model Potential temp
!             temp_mdl = temp_mdl*(pob/1000)**rcp ! Model normal temp
             temp_mdl = obs_pottemp
!             write(198,*)obs_pottemp,t0,errf
!             oberr=1.8
!             temp_mdl=271.96
!!! wuyh added end

! 3.2 Temperature QC
!     --------------

            CALL qc_gaussian(vobs,temp_mdl,oberr, &
                             tolerances, qc)
!            write(483,'(4(1x,E15.8))')vobs,temp_mdl,oberr,tolerances
            
! 4.  SPECIFIC HUMIDITY QC
! ========================

      ELSE IF (ivar .eq. 4) THEN

! 4.1 Observed and model specific humidity
!     ------------------------------------

           if ( cv_options_hum .eq. 0 ) then
            qs_obs = vobs
            qs_mdl = errf
           else
            qv_obs = vobs
            qs_obs = qv_obs/(1 + qv_obs)
            qv_mdl = vobs - errf
            qs_mdl = qv_mdl/(1 + qv_mdl)
           endif

! 4.1 Specific humidity QC
!     -------------------

            CALL qc_gaussian (qs_obs, qs_mdl, oberr, &
                              tolerances, qc)
!            write(484,'(4(1x,E15.8))')qs_obs,qs_mdl,oberr,tolerances

! 5.  WIND COMPONENTS QC
! ======================
      ELSE

! 5.1 Wind QC
!     -------
            CALL qc_gaussian (errf, zero, oberr, &
                              tolerances, qc)

!            write(481,'(3(1x,E15.8))')errf,oberr,tolerances
      ENDIF

! 6.  END
! =======


! 6.1 Prin-out
!     --------


!         IF (eps_equal (vobs, missing_r, 1.)) Go To 1173
!         IF (ABS(errf) .LE. 0.) Go To 1173

!         oberr = observ_error_ncep_intp &
!                (ivar,pob,platform,cv_options_hum,missing_r)

!         IF (ivar .eq. 3) THEN

!             temp_obs = vobs *(1000/pob)**rcp  ! Obs potential temp
!             temp_mdl = temp_obs - errf ! Model Potential temp
!             temp_mdl = temp_mdl*(pob/1000)**rcp ! Model normal temp
!             temp_mdl= obs_pottemp 
!             IF(abs(vobs -temp_mdl).LE.20.)then
!              WRITE (170+ivar,1170)            &
!                     platform, var_name(ivar), &
!                     iobs, pob, vobs ,       &
!                      vobs-temp_mdl,         &
!                     oberr,tolerances,         &
!                     qc,NINT(oldqc)
!             Endif
! 1170        Format(A14,A2,I8,F10.0,F10.2,F10.2,F10.2,F10.2,2I8)
!          ELSE IF (ivar .eq. 4) THEN

! 4.1 Observed and model specific humidity
!     ------------------------------------

!             qv_obs = vobs
!             qs_obs = qv_obs/(1 + qv_obs)

!             qv_mdl = vobs - errf
!             qs_mdl = qv_mdl/(1 + qv_mdl)

!             WRITE (170+ivar,1170)                                   &
!                    platform, var_name(ivar),iobs,pob, 1000.*qs_obs, &
!                    1000.*(qs_obs-qs_mdl),1000.*oberr,               &
!                    tolerances,qc, NINT(oldqc)

!         ELSE

!             WRITE (170+ivar,1170)                                   &
!                    platform, var_name(ivar),iobs,pob,vobs,        &
!                    errf, oberr, tolerances,                   &
!                    qc, NINT(oldqc)

!         ENDIF


! 6.2 return a real
!     -------------
! 1173    Continue
      qcr = 1.0 * qc

 END Subroutine fddaobs_qc

!*******************************************************************
 SUBROUTINE qc_gaussian (valo, valm, valerr, qc_stdv, valqc)
!*******************************************************************
!MAPS O-B/ERR TO A 2 DIGIT INTEGER BETWEEN 0 (BAD) AND 10 (GOOD)
!COMPUTE TAU=|O-B|/ERR AND ASSIGN QC based on THE GAUSSIAN RESPONSE
!  FUNCTION 10*EXP[-0,5*TAU**2/STDV**2]
! -----------------------------------------------------------------!

   IMPLICIT NONE

   REAL,    INTENT (in)    :: valo, valm, valerr, qc_stdv
   INTEGER, INTENT (inout) :: valqc

   REAL    :: valerr_max, val
   INTEGER :: i, j
   INTEGER :: qc2, qc35, qc67, qc67_old
   INTEGER, DIMENSION (7) :: qc_old

   REAL    :: X, Y
   INTEGER :: QCBIT2
! wuyh 20110306 added
   Real,Parameter::wueps=1.0E-5
   Real,Parameter::wucri=0.08 
!   QCBIT2 (X,Y) = NINT (10.*EXP (-0.5*(X/Y)**2))
   QCBIT2 (X,Y) = NINT (10.*EXP (wucri-0.5*(X/Y)**2))
! wuyh 20110306 added end
!-----------------------------------------------------------------!

! 6.1 Ratio R = O-B/ERR
!     -----------------

      IF (.NOT. eps_equal (valo,  missing_r, 1.)) THEN

           valerr_max = (valo-valm)/valerr

! 6.2 QC's 6th and 7th bits 
!     ---------------------
!     QC = R/Ratio mapped to [0(worst), 10(best)]

      qc67 = QCBIT2(valerr_max,qc_stdv)

! 6.3 Make sure QC is between 0 and 10
!     --------------------------------

           IF (qc67 <  0) qc67 =  0
           IF (qc67 > 10) qc67 = 10

! 6.3 Decompose existing QC in bits
!     -----------------------------

           val = valqc
           DO i = 1, 7
               j = val / 10
               qc_old(7-i+1)= val-j*10
               val = j
           ENDDO
           qc67_old = qc_old (6)*10 + qc_old (7)

!           write(194,*)'qc67_old=',qc67_old

! 6.4 Average Error Max QC with 6th and 7th bits of existing QC (if > 0)
! ------------------------------------------------------------------

           IF (qc67_old > wueps) THEN 
               qc67 = (qc67_old + qc67) / 2
           ENDIF

! 6.5 Reset bit 3, 4, 5 to 0 (spared for BIAS)  
!     ----------------------------------------
! wuyh removed the following 20110705
!           DO i = 3, 5
!              qc_old (i) = 0
!           ENDDO
! wuyh removed end 20110705

! 6.6 Keep first 5 bits from previous QC and add mean QC on 6th and 7th bits
!     ----------------------------------------------------------------------

           val =  float(0)
!           DO i = 1, 5 wuyh removed 20110705
           DO i = 1, 2  !since qc_old(i)=0 for i=3,5
              val = val+qc_old (i)*10**(7-i)
           ENDDO
           valqc = val + qc67

         ELSE

! 6.7 QC's for missing data is missing
!     -------------------------------
           valqc =  missing  

         ENDIF

 END SUBROUTINE qc_gaussian
!-------------------------------------------------------------------------!

!*******************************************************************
 SUBROUTINE qc_piecewise_rh (valo, valm, valerr, inflection, valqc)
!*******************************************************************
!  MAPS THE DIFF O-B TO A 2 DIGIT INTEGER BETWEEN 0 (BAD) AND 10 (GOOD)
!  COMPUTE X = |O-B| AND ASSIGN QC ACCORDING TO THE PIECEWISE RESPONSE
!  FUNCTION:
!
!    /|\y
!     |
!  y1 +-----------+ 
!     |            \
!     |             \
!     |              \
!     |               \
!  y2 |-----------+----+------->
!     |           x1   x2     x
!
! Where 
!   x1 = 20*inflection
!   x2 =  2*x1
!   y1 = 10
!   y2 =  0
!------------------------------------------------------------------------!
   IMPLICIT NONE

   REAL,    INTENT (in)    :: valo, valm, valerr, inflection
   INTEGER, INTENT (inout) :: valqc

   REAL    :: valerr_max, val
   INTEGER :: i, j
   INTEGER :: qc2, qc35, qc67, qc67_old
   INTEGER, DIMENSION (7) :: qc_old

   REAL, PARAMETER :: y1= 10., y2 = 0
   REAL :: x, x1, x2

!------------------------------------------------------------------------!

! 1.  Inflexion point abcissae
!     ------------------------

      x1 = inflection * 20.

      IF (x1 <= 0. .OR. x1 > 100.) THEN 
          x1 = 20.
      ENDIF

      x2 = 2*x1

      IF (x2 > 100.) THEN 
          x2 = 100.
      ENDIF

! 6.1 Differences R = O-B
!     -------------------

      IF (.NOT. eps_equal (valo,  missing_r, 1.)) THEN

!          x  = ABS ((valo  - valm)  / valerr)
           x  = ABS  (valo  - valm)

! 6.2 QC's 6th and 7th bits 
!     ---------------------

           IF (x <= x1) THEN
               qc67 = INT (y1)
           ELSE IF (x <= x2) THEN
               qc67 = INT (((y2-y1)*x + y1*x2 - y2*x1)/(x2-x1))
           ELSE
               qc67 = INT (y2)
           ENDIF

!          WRITE (51,'(5(1X,F12.5)1X,I6)') &
!          valo, valm, valo - valm, valerr, (valo  - valm)  / valerr, qc67
!          WRITE (51,'(4(1X,F12.5)1X,I6)') &
!          valo, valm, x , x1, qc67

! 6.3 Make sure QC is between 0 and 10
!     --------------------------------

           IF (qc67 <  0) qc67 =  0
           IF (qc67 > 10) qc67 = 10

! 6.4 Decompose existing QC in bits
!     -----------------------------

           val = valqc

           DO i = 1, 7
              j = val / 10
              qc_old (7-i+1) = val - j*10
              val = j
           ENDDO

           qc67_old =  qc_old (6)*10 + qc_old (7)

! 6.5 Average Error Max QC with 6th and 7th bits of existing QC (if > 0)
!     ------------------------------------------------------------------

           IF (qc67_old > 0) THEN 
               qc67 = (qc67_old + qc67) / 2
           ENDIF

! 6.6 Reset bit 3, 4, 5 to 0 (spared for BIAS)  
!     ----------------------------------------

           DO i = 3, 5
              qc_old (i) = 0
           ENDDO

! 6.7 Keep first 5 bits from previous QC and add mean QC on 6th and 7th bits
!     ----------------------------------------------------------------------

           val =  0

           DO i = 1, 5
              val = val + qc_old (i)*10**(7-i)
           ENDDO

           valqc = val + qc67

         ELSE

! 6.8 QC's for missing data is missing
!     -------------------------------
           valqc =  missing 

         ENDIF

 END SUBROUTINE qc_piecewise_rh

!*******************************************************************
 SUBROUTINE qc_piecewise (valo, valm, valerr, inflection, valqc)
!*******************************************************************
!  MAPS THE RATIO O-B/ERR TO A 2 DIGIT INTEGER BETWEEN 0 (BAD) AND 10 (GOOD)
!
!  COMPUTE TAU = |O-B|/ERR AND ASSIGN QC ACCORDING TO THE PIECEWISE RESPONSE
!  FUNCTION:
!
!    /|\y
!     |
!  y1 +-----------+ 
!     |            \
!     |             \
!     |              \
!     |               \
!  y2 |-----------+----+------->
!     |           x1   x2     x
!
! For inflection = 1, qc_piecewise = qc_pearson
!
!-------------------------------------------------------------------------!
   IMPLICIT NONE

   REAL,    INTENT (in)    :: valo, valm, valerr, inflection
   INTEGER, INTENT (inout) :: valqc

   REAL    :: valerr_max, val
   INTEGER :: i, j
   INTEGER :: qc2, qc35, qc67, qc67_old
   INTEGER, DIMENSION (7) :: qc_old

   REAL, PARAMETER :: y1= 10., y2 = 0
   REAL :: x, x1, x2

!--------------------------------------------------------------------------!

! 1.  Inflexion point abcissae
!     ------------------------

      IF (inflection > 10) THEN
          x1 = inflection
          x2 = x1 + 10.
      ELSE
          x1 = inflection - 1.
          x1 = MAX (x1, 0.)
          x2 = 10.
      ENDIF

! 6.1 Ratio R = O-B/ERR
!     -----------------

      IF (.NOT. eps_equal (valo,  missing_r, 1.)) THEN

           x  = ABS ((valo  - valm)  / valerr)

! 6.2 QC's 6th and 7th bits 
!     ---------------------

           IF (x <= x1) THEN
               qc67 = INT (y1)
           ELSE IF (x <= x2) THEN
               qc67 = INT (((y2-y1)*x + y1*x2 - y2*x1)/(x2-x1))
           ELSE
               qc67 = INT (y2)
           ENDIF

!          WRITE (51,'(5(1X,F12.5)1X,I6)') &
!          valo, valm, valo - valm, valerr, (valo  - valm)  / valerr, qc67

! 6.3 Make sure QC is between 0 and 10
!     --------------------------------

           IF (qc67 <  0) qc67 =  0
           IF (qc67 > 10) qc67 = 10

! 6.4 Decompose existing QC in bits
!     -----------------------------

           val = valqc

           DO i = 1, 7
              j = val / 10
              qc_old (7-i+1) = val - j*10
              val = j
           ENDDO

           qc67_old =  qc_old (6)*10 + qc_old (7)

! 6.5 Average Error Max QC with 6th and 7th bits of existing QC (if > 0)
!     ------------------------------------------------------------------

           IF (qc67_old > 0) THEN 
               qc67 = (qc67_old + qc67) / 2
           ENDIF

! 6.6 Reset bit 3, 4, 5 to 0 (spared for BIAS)  
!     ----------------------------------------

           DO i = 3, 5
              qc_old (i) = 0
           ENDDO

! 6.7 Keep first 5 bits from previous QC and add mean QC on 6th and 7th bits
!     ----------------------------------------------------------------------

           val =  0

           DO i = 1, 5
              val = val + qc_old (i)*10**(7-i)
           ENDDO

           valqc = val + qc67

         ELSE

! 6.8 QC's for missing data is missing
!     -------------------------------
           valqc =  missing 

         ENDIF

 END SUBROUTINE qc_piecewise
!------------------------------------------------------------------------!

!*******************************************************************
 SUBROUTINE qc_pearson (valo, valm, valerr, qc_scale, valqc)
!*******************************************************************
   IMPLICIT NONE

   REAL,    INTENT (in)    :: valo, valm, valerr, qc_scale
   INTEGER, INTENT (inout) :: valqc

   REAL    :: valerr_max, val
   INTEGER :: i, j
   INTEGER :: qc2, qc35, qc67, qc67_old
   INTEGER, DIMENSION (7) :: qc_old

!--------------------------------------------------------------------------!
!  STATEMENT FUNCTION: MAPS THE RATIO O-B/ERR TO A 2 DIGIT INTEGER 
!  BETWEEN 0 (BAD) AND 10 (GOOD)
!
!  COMPUTE TAU = |O-B|/ERR AND ASSIGN QC = 10 - INT(TAU), SUCH THAT 
!
!   QC = 10 for           |O-B| <  1*ERR
!   QC =  9 for  1*ERR <= |O-B| <  2*ERR
!   QC =  8 for  2*ERR <= |O-B| <  3*ERR
!   QC =  7 for  3*ERR <= |O-B| <  4*ERR
!   QC =  6 for  4*ERR <= |O-B| <  5*ERR
!   QC =  5 for  5*ERR <= |O-B| <  6*ERR
!   QC =  4 for  6*ERR <= |O-B| <  7*ERR
!   QC =  3 for  7*ERR <= |O-B| <  8*ERR
!   QC =  2 for  8*ERR <= |O-B| <  9*ERR
!   QC =  1 for  9*ERR <= |O-B| < 10*ERR
!   QC =  0 for 10*ERR <= |O-B|
!
! For qc_scale =1, 10-QC is the tolerance factor for Pearson hypothesis test
! Rejecting data with QC <= 8 would correpond to the 4.5% confidence level
!
!------------------------------------------------------------------------!

! 6.1 Ratio R = O-B/ERR
!     -----------------

      IF (.NOT. eps_equal (valo,  missing_r, 1.)) THEN

           valerr_max  = ABS ((valo  - valm)  / valerr )

! 6.2 QC's 6th and 7th bits 
!     ---------------------
!          !  QC = R/Ratio mapped to [0(worst), 10(best)]

           qc67 = 10 - INT (valerr_max/qc_scale)

!          WRITE (51,'(5(1X,F12.5)1X,I6)') &
!          valo, valm, valo - valm, valerr, (valo  - valm)  / valerr, qc67

! 6.3 Make sure QC is between 0 and 10
!     --------------------------------

           IF (qc67 <  0) qc67 =  0
           IF (qc67 > 10) qc67 = 10

! 6.4 Decompose existing QC in bits
!     -----------------------------

           val = valqc

           DO i = 1, 7
              j = val / 10
              qc_old (7-i+1) = val - j*10
              val = j
           ENDDO

           qc67_old =  qc_old (6)*10 + qc_old (7)

! 6.5 Average Error Max QC with 6th and 7th bits of existing QC (if > 0)
!     ------------------------------------------------------------------

           IF (qc67_old > 0) THEN 
               qc67 = (qc67_old + qc67) / 2
           ENDIF

! 6.6 Reset bit 3, 4, 5 to 0 (spared for BIAS)  
!     ----------------------------------------

           DO i = 3, 5
              qc_old (i) = 0
           ENDDO

! 6.7 Keep first 5 bits from previous QC and add mean QC on 6th and 7th bits
!     ----------------------------------------------------------------------

           val =  0

           DO i = 1, 5
              val = val + qc_old (i)*10**(7-i)
           ENDDO

           valqc = val + qc67

         ELSE

! 6.8 QC's for missing data is missing
!     -------------------------------
           valqc =  missing 

         ENDIF

 END SUBROUTINE qc_pearson
!-----------------------------------------------------------------------!

!*******************************************************************
 LOGICAL FUNCTION eps_equal (a , b , eps)
!*******************************************************************
! Compare two reals a and b, and return TRUE if |a=b|<eps

   IMPLICIT NONE

   REAL , INTENT (IN)                     :: a , b , eps

   IF (ABS (a - b) .LT. eps) THEN
      eps_equal = .TRUE.
   ELSE
      eps_equal = .FALSE.
   ENDIF

 END FUNCTION eps_equal


!*******************************************************************
 LOGICAL FUNCTION eps_not (a , b , eps)
!*******************************************************************
!  Compare two reals a and b, and return FALSE if |a-b|<eps

   IMPLICIT NONE

   REAL , INTENT (IN)                     :: a , b , eps

   IF (ABS (a - b) .LT. eps) THEN
      eps_not = .FALSE.
   ELSE
      eps_not = .TRUE.
   ENDIF

 END FUNCTION eps_not
!---------------------------------------------------------------------!
 END MODULE module_fddaobs_qc
