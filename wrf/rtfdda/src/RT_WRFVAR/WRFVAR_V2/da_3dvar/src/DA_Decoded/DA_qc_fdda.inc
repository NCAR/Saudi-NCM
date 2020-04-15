
 SUBROUTINE qc_fdda (max_number_obs, obs, mdl, err, index, number_obs, &
                     ldebug, grid, &
#include <em_dummy_args.inc> 
)
!------------------------------------------------------------------------------!
! Perform FDDA QC check.
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
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2003 - 2006.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, September 2006.
!------------------------------------------------------------------------------!
  IMPLICIT NONE
  INTRINSIC MAX

! ARGUMENTS

  INTEGER,        INTENT (in)  :: max_number_obs
  INTEGER,        INTENT (in)  :: number_obs
  INTEGER,        INTENT (in),  DIMENSION (max_number_obs) :: index
  TYPE (report),  INTENT (in),  DIMENSION (max_number_obs) :: obs
  TYPE (report),  INTENT (out), DIMENSION (max_number_obs) :: mdl
  TYPE (report),  INTENT (out), DIMENSION (max_number_obs) :: err
  LOGICAL,        INTENT (in)  :: ldebug
  TYPE (domain),  INTENT (in)  :: grid

!------------------------------------------------------------------------------!
! DECODED STRUCTURE

  TYPE (measurement), POINTER  :: obser
  TYPE (measurement), POINTER  :: model
  TYPE (measurement), POINTER  :: error
!------------------------------------------------------------------------------!
! OTHER VARIABLES

  INTEGER :: n, nn
  INTEGER :: uo_qc, vo_qc,uvo_qc

  REAL    :: sigma_p, sigma_q, sigma_u, sigma_t
  REAL    :: es,  ws,  qs
  REAL    :: des, dws, dqs
  REAL    :: des_over_es, dws_over_ws

  REAL    :: To, Tdo, po, qo, wo, rho, uo, vo, ho
  REAL    :: Tm, Tdm, pm, qm, wm, rhm, um, vm, hm
  REAL    :: Te, Tde, pe, qe, we, rhe, ue, ve, he
  REAL    :: Tom, pom
  REAL    :: dwm, dqm, dwe
  REAL    :: speedo, speedm, speede
  REAL    :: diro

  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'fdda_qc '
  CHARACTER (LEN = 100) :: error_message


#include <em_dummy_decl.inc> 

!------------------------------------------------------------------------------!

      CALL prelog
      WRITE  (log_unit,'(A)') &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(A)') "QC FDDA:"


! 2.  SCREEN OBSERVATIONS
! =======================

! 2.1 Loop over stations
!     -------------------
stations:&
      DO nn = 1, number_obs

      n = index (nn)

! 2.2 Discard bad reports
!     -------------------

      IF (obs (n) % info % discard) THEN
          mdl (n) % info % discard = .TRUE.
          err (n) % info % discard = .TRUE.
          CYCLE stations
      ENDIF

! 2.3 QC standard deviation
!     ---------------------

      IF (obs (n) % info % platform (1:11) == "FM-35 TEMP " .OR. &
          obs (n) % info % platform (1:11) == "FM-32 PILOT" .OR. &
          obs (n) % info % platform (1:11) == "FM-32 TEMP " ) THEN 
          sigma_p = tolerance_p_sound
          sigma_q = tolerance_q_sound
          sigma_u = tolerance_u_sound
          sigma_t = tolerance_t_sound
      ELSE IF (obs (n) % info % platform (1:11) == "FM-12 SATWI" ) THEN
          sigma_p = tolerance_p/2.
          sigma_q = tolerance_q/2.
          sigma_u = tolerance_u/2.
          sigma_t = tolerance_t/2.
      ELSE
          sigma_p = tolerance_p
          sigma_q = tolerance_q
          sigma_u = tolerance_u
          sigma_t = tolerance_t
      ENDIF


! 2.4 Initialize upper-level pointers
!     -------------------------------

      obser => obs (n) % surface
      model => mdl (n) % surface
      error => err (n) % surface

! 2.5 Loop over upper-level
!     ---------------------

levels:&
      DO WHILE (ASSOCIATED (obser))


! 3.  LOAD MODEL DATA
! ===================

! 3.1 Load model data for that level
!     ------------------------------

      hm   = model % meas % height      % data
      pm   = model % meas % pressure    % data
      Tm   = model % meas % temperature % data
      Tdm  = model % meas % dew_point   % data
      rhm  = model % meas % rh          % data
      um   = model % meas % u           % data
      vm   = model % meas % v           % data

! 3.2 Compute model mixing ratio and specific humidity when possible
!     --------------------------------------------------------------
!... Assume that model data are always available

      !  Saturation water vapor pressure in Pa

      es  = es_alpha * EXP (es_beta*(Tm-T_kelvin) &
                                       /(Tm-T_kelvin+es_gamma))

      !  Water Vapor mixing ratio at saturation in kg/kg

      ws  = rd_over_rv * es /(pm-es)

      !  Water vapor mixing ratio in kg/kg

      wm  = 0.01*rhm*ws

      !  Specific humidity in kg/kg

      qm    = wm / (1. + wm)


! 3.  LOAD OBSERVATIONS
! =====================

! 3.1 At that point, all levels must have pressure and height 
!     -------------------------------------------------------

      IF ((eps_equal (obser % meas % height   % data, missing_r, 1.)) .OR. &
          (eps_equal (obser % meas % pressure % data, missing_r, 1.))) THEN 
           error_message = "Height and pressure are missing, cannot proceed"
           CALL error_handler (log_unit, ABS (missing), &
                proc_name//error_message, .TRUE., .TRUE.)
      ENDIF

! 3.2 Load obs data for that level
!     ----------------------------

      ho   = obser % meas % height      % data
      po   = obser % meas % pressure    % data
      To   = obser % meas % temperature % data
      Tdo  = obser % meas % dew_point   % data
      rho  = obser % meas % rh          % data
      uo   = obser % meas % u           % data
      vo   = obser % meas % v           % data
      diro = obser % meas % direction   % data
      speedo = obser % meas % speed     % data

! 3.3 Compute observed mixing ratio and specific humidity when possible
!     -----------------------------------------------------------------

      IF (eps_equal (rho, missing_r, 1.)) THEN
          wo = missing_r
          qo = missing_r
      ELSE

          ! Replace missing obs with model values if needed

          pom  = po
          Tom  = To

!... Assume that model data are always available
          IF (eps_equal (pom,  missing_r, 1.)) pom  = pm
          IF (eps_equal (Tom,  missing_r, 1.)) Tom  = Tm

          !  Saturation water vapor pressure in Pa

          es  = es_alpha * EXP (es_beta*(Tom-T_kelvin) &
                                       /(Tom-T_kelvin+es_gamma))

          !  Water Vapor mixing ratio at saturation in kg/kg

          ws  = rd_over_rv * es /(pom-es)

          !  Water vapor mixing ratio in kg/kg

          wo = 0.01*rho*ws

          !  Specific humidity in kg/kg

          qo = wo / (1. + wo)

      ENDIF

! 4.  LOAD OBSERVATIONAL ERROR
! ============================

! 4.1 Load observational error data for that level
!     --------------------------------------------

      he   = error % meas % height      % data
      pe   = error % meas % pressure    % data
      Te   = error % meas % temperature % data
      Tde  = error % meas % dew_point   % data
      rhe  = error % meas % rh          % data
      ue   = error % meas % u           % data
      ve   = error % meas % v           % data

! 4.2 Compute specific humidity error from relative humidity error
!     ------------------------------------------------------------

       !  Saturation water vapor pressure in Pa

       es  = es_alpha * EXP (es_beta*(Tm-T_kelvin) &
                           /(Tm-T_kelvin+es_gamma))

       des_over_es = es_beta*es_gamma * Te &
                   /(Tm-T_kelvin+es_gamma)**2

       des = des_over_es * es

       !  Water Vapor mixing ratio at saturation in kg/kg

       ws   = rd_over_rv * es /(pm-es)

       dws_over_ws = (pm*des - es*pe) / (es*(pm-es))

       dws = dws_over_ws * ws

       ! Water vapor mixing ratio

!      wm  = 0.01 * rhm * ws
       dwm = 0.01 * (rhe * ws + rhm * dws)

       !  Specific humidity in kg/kg

!      qm  =  wm / (1. + wm)
       dqm = dwm / (1. + wm) - wm*dwm / (1. + wm)**2

       !  Save mixing ration and specific humidity error

       we  = dwe
       qe  = dqm


! 4.  COMPUTE RT-FDDA QC
! =======================

! 4.1 Height QC
!     ---------

      CALL qc_gaussian (ho, hm, he, sigma_p, &
                        obser % meas % height % qc)


      !  Round to nearest m height derived from first guess

      IF (MOD (obser % meas % height % qc/first_guess, 2) /= 0) &
               obser % meas % height % data = ANINT &
              (obser % meas % height % data)

! 4.2 Pressure QC
!     -----------

      !  Compute QC for pressure, when pressure was not derived from first guess

      CALL qc_gaussian (po, pm, pe, sigma_p, &
                        obser % meas % pressure % qc)

      !  Round to nearest Pa pressure derived from first guess

      IF (obser % meas % pressure % qc >= first_guess) &
          obser % meas % pressure % data = ANINT &
         (obser % meas % pressure % data)

! 4.3 Temperature QC
!     --------------

      CALL qc_gaussian (To, Tm, Te, sigma_t, &
                        obser % meas % temperature % qc)


! 4.4 Moisture QC
!     -----------

      IF (cv_options_hum .eq. 1) THEN

          !  QC based on specific humidity

         CALL qc_gaussian (qo, qm, qe, sigma_q, &
                           obser % meas % rh % qc)

      ELSE

         !  QC based on relative humidity (with respect to water)

!        CALL qc_gaussian (rho, rhm, rhe, sigma_q, &
!                          obser % meas % rh % qc)
         CALL qc_piecewise_rh (rho, rhm, rhe, sigma_q, &
                           obser % meas % rh % qc)

      ENDIF

! 4.5 Dew point QC
!     ------------
!FVDB 20090825  sigma_q has been changed into sigma_t
      CALL qc_gaussian (Tdo, Tdm, Tde, sigma_t, &
                        obser % meas % dew_point % qc)

! 4.6 Wind QC
!     -------

      ! QC on the components when they are both available
      IF (eps_not (uo, missing_r, 1.) .AND. eps_not (vo, missing_r, 1.)) THEN 

          CALL qc_gaussian (uo, um, ue, sigma_u, obser % meas % u % qc)
          CALL qc_gaussian (vo, vm, ve, sigma_u, obser % meas % v % qc)

          !  Keep only one QC value for u, v, speed and direction

          uo_qc = obser % meas % u % qc
          vo_qc = obser % meas % v % qc

          IF (uo_qc == missing .OR. vo_qc == missing) THEN 
              uvo_qc = missing
          ELSE
              uvo_qc = MIN (uo_qc, vo_qc)
          ENDIF

          obser % meas % u % qc = uvo_qc
          obser % meas % v % qc = uvo_qc
          obser % meas % speed     % qc = uvo_qc
          obser % meas % direction % qc = uvo_qc

      ELSE

      ! QC on speed only when it is available and the direction is not
          IF (eps_not   (speedo, missing_r, 1.) .AND. & 
              eps_equal (diro,   missing_r, 1.)) THEN

              speedm = SQRT (um**2 + vm**2)
              speede = SQRT (2.) * ve
              CALL qc_gaussian (speedo, speedm, speede, sigma_u, obser % meas % speed % qc)

          ELSE
      ! Otherwise set speed and direction QC to missing 
              obser % meas % speed % qc = missing
              obser % meas % direction % qc = missing
          ENDIF

      ENDIF

! 4.7 Thickness QC
!     ------------

      !  No check on thickness, assume always good data
      IF (obser % meas % thickness % qc /= missing ) THEN
          obser % meas % thickness % qc  = 10 
      ELSE
          obser % meas % thickness % qc  = missing
      ENDIF


! 5.  FILL DATA STRUCTURES
! ========================

! 5.1 Copied obs QC to model structure
!     --------------------------------

      model % meas % pressure    % qc   = obser % meas % pressure    % qc
      model % meas % height      % qc   = obser % meas % height      % qc
      model % meas % temperature % qc   = obser % meas % temperature % qc
      model % meas % dew_point   % qc   = obser % meas % dew_point   % qc
      model % meas % speed       % qc   = obser % meas % speed       % qc
      model % meas % direction   % qc   = obser % meas % direction   % qc
      model % meas % u           % qc   = obser % meas % u           % qc
      model % meas % v           % qc   = obser % meas % v           % qc
      model % meas % rh          % qc   = obser % meas % rh          % qc
      model % meas % thickness   % qc   = obser % meas % thickness   % qc

! 5.2 Copied obs QC to error structure
!     --------------------------------

      error % meas % pressure    % qc   = obser % meas % pressure    % qc
      error % meas % height      % qc   = obser % meas % height      % qc
      error % meas % temperature % qc   = obser % meas % temperature % qc
      error % meas % dew_point   % qc   = obser % meas % dew_point   % qc
      error % meas % speed       % qc   = obser % meas % speed       % qc
      error % meas % direction   % qc   = obser % meas % direction   % qc
      error % meas % u           % qc   = obser % meas % u           % qc
      error % meas % v           % qc   = obser % meas % v           % qc
      error % meas % rh          % qc   = obser % meas % rh          % qc
      error % meas % thickness   % qc   = obser % meas % thickness   % qc


! 6. END
! =======

! 6.1 Go to next obs level
!      --------------------

      obser => obser % next
      model => model % next
      error => error % next

      ENDDO levels

! 6.2 Go to next station
!     ------------------

      ENDDO stations
  
! 6.3 end here
!     -------

      RETURN

CONTAINS
!------------------------------------------------------------------------------!

 SUBROUTINE qc_gaussian (valo, valm, valerr, qc_stdv, valqc)
!------------------------------------------------------------------------------!
!  MAPS THE RATIO O-B/ERR TO A 2 DIGIT INTEGER BETWEEN 0 (BAD) AND 10 (GOOD)
!
!  COMPUTE TAU = |O-B|/ERR AND ASSIGN QC ACCORDING TO THE GAUSSIAN RESPONSE
!
!  FUNCTION 10*EXP[-0,5*TAU**2/STDV**2]
!------------------------------------------------------------------------------!

   IMPLICIT NONE

   REAL,    INTENT (in)    :: valo, valm, valerr, qc_stdv
   INTEGER, INTENT (inout) :: valqc

   REAL    :: valerr_max, val
   INTEGER :: i, j
   INTEGER :: qc2, qc35, qc67, qc67_old
   INTEGER, DIMENSION (7) :: qc_old

   REAL    :: X, Y
   INTEGER :: QCBIT2

   QCBIT2 (X,Y) = NINT (10.*EXP (-0.5*(X/Y)**2))

!------------------------------------------------------------------------------!

! 6.1 Ratio R = O-B/ERR
!     -----------------

      IF (.NOT. eps_equal (valo,  missing_r, 1.)) THEN

           valerr_max  = (valo  - valm)  / valerr


! 6.2 QC's 6th and 7th bits 
!     ---------------------
!          !  QC = R/Ratio mapped to [0(worst), 10(best)]

           qc67 = QCBIT2 (valerr_max, qc_stdv)

! 6.3 Make sure QC is between 0 and 10
!     --------------------------------

           IF (qc67 <  0) qc67 =  0
           IF (qc67 > 10) qc67 = 10

! 6.3 Decompose existing QC in bits
!     -----------------------------

           val = valqc

           DO i = 1, 7
              j = val / 10
              qc_old (7-i+1) = val - j*10
              val = j
           ENDDO

           qc67_old =  qc_old (6)*10 + qc_old (7)

! 6.4 Average Error Max QC with 6th and 7th bits of existing QC (if > 0)
!     ------------------------------------------------------------------

           IF (qc67_old > 0) THEN 
               qc67 = (qc67_old + qc67) / 2
           ENDIF

! 6.5 Reset bit 3, 4, 5 to 0 (spared for BIAS)  
!     ----------------------------------------

           DO i = 3, 5
              qc_old (i) = 0
           ENDDO

! 6.6 Keep first 5 bits from previous QC and add mean QC on 6th and 7th bits
!     ----------------------------------------------------------------------

           val =  0

           DO i = 1, 5
              val = val + qc_old (i)*10**(7-i)
           ENDDO

           valqc = val + qc67

         ELSE

! 6.7 QC's for missing data is missing
!     -------------------------------
           valqc =  missing 

         ENDIF

 END SUBROUTINE qc_gaussian
!------------------------------------------------------------------------------!

 SUBROUTINE qc_piecewise_rh (valo, valm, valerr, inflection, valqc)
!------------------------------------------------------------------------------!
!  MAPS THE DIFFERENCE O- TO A 2 DIGIT INTEGER BETWEEN 0 (BAD) AND 10 (GOOD)
!
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
!------------------------------------------------------------------------------!
   IMPLICIT NONE

   REAL,    INTENT (in)    :: valo, valm, valerr, inflection
   INTEGER, INTENT (inout) :: valqc

   REAL    :: valerr_max, val
   INTEGER :: i, j
   INTEGER :: qc2, qc35, qc67, qc67_old
   INTEGER, DIMENSION (7) :: qc_old

   REAL, PARAMETER :: y1= 10., y2 = 0
   REAL :: x, x1, x2

!------------------------------------------------------------------------------!

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
!------------------------------------------------------------------------------!
 SUBROUTINE qc_piecewise (valo, valm, valerr, inflection, valqc)
!------------------------------------------------------------------------------!
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
!------------------------------------------------------------------------------!
   IMPLICIT NONE

   REAL,    INTENT (in)    :: valo, valm, valerr, inflection
   INTEGER, INTENT (inout) :: valqc

   REAL    :: valerr_max, val
   INTEGER :: i, j
   INTEGER :: qc2, qc35, qc67, qc67_old
   INTEGER, DIMENSION (7) :: qc_old

   REAL, PARAMETER :: y1= 10., y2 = 0
   REAL :: x, x1, x2

!------------------------------------------------------------------------------!

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
!------------------------------------------------------------------------------!

 SUBROUTINE qc_pearson (valo, valm, valerr, qc_scale, valqc)
!------------------------------------------------------------------------------!
   IMPLICIT NONE

   REAL,    INTENT (in)    :: valo, valm, valerr, qc_scale
   INTEGER, INTENT (inout) :: valqc

   REAL    :: valerr_max, val
   INTEGER :: i, j
   INTEGER :: qc2, qc35, qc67, qc67_old
   INTEGER, DIMENSION (7) :: qc_old

!------------------------------------------------------------------------------!
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
! For qc_scale =1, 10-QC is the tolerance factor for the Pearson hypothesis test
! Rejecting data with QC <= 8 would correpond to the 4.5% confidence level.
!
!------------------------------------------------------------------------------!

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
!------------------------------------------------------------------------------!
 END SUBROUTINE qc_fdda
!------------------------------------------------------------------------------!

 FUNCTION intplin (x,xx,yy) RESULT (val)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx, yy
    REAL                :: x
    REAL                :: val

    INTEGER             :: n,m,jl
!   INTEGER             :: iab, ABORT
!------------------------------------------------------------------------------!

    n = size (xx)
    m = size (yy)

    IF (n .NE. m) THEN
        CALL prelog
        WRITE (UNIT = log_unit, FMT = '(A)' ) &
       "ERROR in intplin: arrays must have same size"
        call ABORT ()
    ENDIF

    jl = locate (x,xx)

    IF (jl .LE. 0) THEN    
        val = yy (1)
    ELSE IF (jl .GE. n) THEN    
        val = yy (n)
    ELSE
        val = (xx (jl+1) - x) * yy (jl) + (x - xx (jl)) * yy (jl+1)
        val = val / (xx (jl+1) - xx (jl))
    ENDIF

 END FUNCTION intplin
!------------------------------------------------------------------------------!

 FUNCTION intplog (x,xx,yy) RESULT (val)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx, yy
    REAL                :: x
    REAL                :: val

    INTEGER             :: n,m,jl
!   INTEGER             :: iab, ABORT
!------------------------------------------------------------------------------!

    n = size (xx)
    m = size (yy)

    IF (n .NE. m) THEN
        CALL prelog
        WRITE (UNIT = log_unit, FMT = '(A)' ) &
       "ERROR in intplog: arrays must have same size"
        call ABORT ()
    ENDIF

    jl = locate (x,xx)

    IF (jl .LE. 0) THEN    
        val = yy (1)
    ELSE IF (jl .GE. n) THEN    
        val = yy (n)
    ELSE
        val = log (xx (jl+1) / x) * yy (jl) + log (x / xx (jl)) * yy (jl+1)
        val = val / log (xx (jl+1) / xx (jl))
    ENDIF

 END FUNCTION intplog
!------------------------------------------------------------------------------!

 FUNCTION locate (x,xx) RESULT (index)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx
    REAL                :: x
    INTEGER             :: index

    INTEGER             :: n,jl,jm,ju
    LOGICAL             :: ascnd
!------------------------------------------------------------------------------!

    n = size (xx)
    ascnd = (xx (n) >= xx (1))   ! True if ascending order, false otherwise
    jl = 0                       ! Initialize lower limit
    ju = n+1                     ! Initialize upper limit

    DO

       IF (ju-jl <= 1) EXIT      ! Repeat until this condition is satisfied

       jm = (ju+jl) / 2.         ! Compute a mid point

       IF (ascnd .EQV. (x >= xx (jm))) THEN
           jl = jm               ! Replace mid point by lower limit
       ELSE
           ju = jm               ! Replace mid point by upper limit
       ENDIF

    ENDDO

    IF (x .EQ. xx (1)) THEN      ! Set the output, being carefull with endpoints
        index = 1
    ELSE IF (x .EQ. xx (n)) THEN
        index = n-1 
    ELSE
        index = jl
    ENDIF

 END FUNCTION LOCATE
!------------------------------------------------------------------------------!
!END SUBROUTINE qc_max
!------------------------------------------------------------------------------!
