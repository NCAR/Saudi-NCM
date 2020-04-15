
 SUBROUTINE err_intp (max_number_obs, obs, mdl, err, index, number_obs, &
                      ldebug, grid, &
#include <em_dummy_args.inc> 
)
!------------------------------------------------------------------------------!
! Perform error max QC check.
!
! Reports are compared to model interpolated values at the obs locations.
! Differences |O-B| are scaled by the expected observational error Err and the
! the ratio |O-B|/Err is mapped between 0 and 10 according to a Gaussian
! mapping function with standard deviation "tolerance" defined in namelist.
! QC values between 0 and 10 are coded on the 6th and 7th bit of the field QC 
! integer flag.
!
! Depending on the namelist string 'error_type', Err is the forecast, 
! observational (representativness+instrumental) or innovation (forecast +
! observational) error.
! 
! Francois Vandenberghe, January 2003
!------------------------------------------------------------------------------!
  IMPLICIT NONE

! ARGUMENTS

  INTEGER,        INTENT (in)   :: max_number_obs
  INTEGER,        INTENT (in)   :: number_obs
  INTEGER,        INTENT (in),  DIMENSION (max_number_obs) :: index
  TYPE (report),  INTENT (in),  DIMENSION (max_number_obs) :: obs
  TYPE (report),  INTENT (out), DIMENSION (max_number_obs) :: mdl
  TYPE (report),  INTENT (out), DIMENSION (max_number_obs) :: err
  TYPE (domain),  INTENT (in)   :: grid
  LOGICAL,        INTENT (in)   :: ldebug

!------------------------------------------------------------------------------!
! DECODED STRUCTURE

  TYPE (measurement), POINTER  :: obser
  TYPE (measurement), POINTER  :: model
  TYPE (measurement), POINTER  :: error
!------------------------------------------------------------------------------!
! OBSERVATIONAL ERROR FILES

  INTEGER, PARAMETER :: err_levels = 33
  REAL,    DIMENSION   (err_levels) :: Coord_mb, Coord_mb_read
  REAL,    DIMENSION   (0:99,err_levels) :: Temp_K, Sp_Hum_gkg,&
                                            uv_ms,  P_mb, H_m, &
                                            RH_water
!------------------------------------------------------------------------------!
! OTHER VARIABLES

  CHARACTER (LEN =  80) :: file_ou
  CHARACTER (LEN = 100) :: error_message
  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'err_intp '

  INTEGER :: io_error
  INTEGER :: unit_ou
  INTEGER :: kmin, kmax
  INTEGER :: unit_err, iunit
  INTEGER :: i, j, k, l, ll, n, nn
  INTEGER :: qcu, qcv, qcuv, qo_qc

  REAL    :: err_t,  err_p, err_q, err_uv, err_rh  ! Innovation error
  REAL    :: err_td, err_rh_lin, err_h, err_dir

  REAL    :: errr_t, errr_uv

  REAL    :: fm, dewm
  REAL    :: es,  ws,  qs
  REAL    :: des, dws, dqs
  REAL    :: des_over_es, dws_over_ws, dfm_over_fm

  REAL    :: pmb
  REAL    :: To, Tdo, po, qo, wo, rho, uo, vo, ho
  REAL    :: Tm, Tdm, pm, qm, wm, rhm, um, vm, hm
  REAL    :: Tom, rhom, pom
  REAL    :: speedm, dirm
  REAL    :: invdifftd, invtd
  REAL    :: dinvdifftd, dinvtd

#include <em_dummy_decl.inc> 

! added by Y. Liu, Nov. 2003
  DATA Coord_mb /1100,1050,1000,950,900,850,800,750,700,650, &
                  600,550,500,450,400,350,300,250,200,150, &
                  100, 75, 50, 40, 30, 20, 10,  5,  4,  3, 2, 1, 0/
! Liu end

!------------------------------------------------------------------------------!

      CALL prelog
      WRITE  (log_unit,'(A)') &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(A)') "Interpolate error profiles at obs locations:"
      CALL prelog
      WRITE  (log_unit,'(A)')


! 1.1 CONSTANTS INITIALIZATION
! ============================
 
! 1.1 Load climatological errors profiles
!     -----------------------------------

      CALL error_levels (err_levels, Coord_mb_read, Temp_K, Sp_Hum_gkg, &
                         RH_water, uv_ms, P_mb, H_m, error_type, ldebug)

! 1.2 Pressure levels
!     ---------------

      Coord_mb_read = ABS (Coord_mb_read - Coord_mb)

      IF (MAXVAL (Coord_mb_read) > 1.) THEN 
          error_message = &
         "Read pressure levels do not match defined pressure levels."
          CALL error_handler (log_unit, 1, proc_name//error_message, &
                              .TRUE., .TRUE.)
      ENDIF

! 1.3 Use radiosonde for all upper-data
!     ---------------------------------

      DO i = 0, 99
         Temp_K (i,:) = Temp_K (20,:)
         uv_ms  (i,:) = uv_ms  (20,:)
         P_mb   (i,:) = P_mb   (20,:)
         H_m    (i,:) = H_m    (20,:)
         RH_water (i,:) = RH_water (20,:)
         Sp_Hum_gkg (i,:) = Sp_Hum_gkg (20,:)
      ENDDO

! 1.4 Define surface (80 and 81) as the lowest level of sounding
!     ----------------------------------------------------------

      Temp_K (80:81,:)     = Temp_K (20,1)
      uv_ms  (80:81,:)     = uv_ms  (20,1)
      P_mb   (80:81,:)     = P_mb   (20,1)
      H_m    (80:81,:)     = H_m    (20,1)
      RH_water (80:81,:)   = RH_water (20,1)
      Sp_Hum_gkg (80:81,:) = Sp_Hum_gkg (20,1)

! 1.5 Write out error in file
!     -----------------------

      file_ou = "errsnd_" // TRIM (error_type) // ".txt"
      unit_ou = 99

      CALL prelog
      WRITE (UNIT = log_unit, FMT = '(A,A)') &
            "Write radiosonde error profiles in file: ",TRIM (file_ou)

! 1.6 Open output ascii file
!     ----------------------

      OPEN (UNIT = unit_ou , FILE = file_ou, FORM = 'FORMATTED'  , &
           ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error /=  0) THEN
          CALL prelog
          WRITE (UNIT = log_unit, FMT = '(A,A,/)') &
         "Unable to open output file ",TRIM (file_ou)
      ELSE

! 1.7 Write header
!     ------------

          WRITE (unit_ou,'(A)') &
         "Level   Temp K    UV m/s    RHum %  Pres hPa   Heigh m"

! 1.8 Write profiles
!     ---------------

          DO k = err_levels, 1, -1
             WRITE (unit_ou,'(I4,5(2X,F8.1))')   NINT (Coord_mb (k)), &
                    Temp_K   (20,k), uv_ms (20,k), &
                    RH_water (20,k), P_mb (20,k),  &
                    H_m (20,k)
          ENDDO

! 1.9 Close file
!     -----------

          CLOSE (UNIT=unit_ou)

      ENDIF

! 2.  INTERPOLATE AT OBSERVATION LOCATIONS
! ========================================

! 2.1 Discard bad reports
!     -------------------

hinterp:&
      DO nn = 1, number_obs

      n = index (nn)

      IF (obs (n) % info % discard) THEN
          mdl (n) % info % discard = .TRUE.
          err (n) % info % discard = .TRUE.
          CYCLE hinterp
      ENDIF

! 2.2 Observational/forecast error depends on platform type
!     -----------------------------------------------------

      iunit = oberr_unit (obs (n) % location % name, &
                          obs (n) % info     % platform)


! 3.  LOOP THOUGH UPPER LEVELS
! ============================

! 3.1 Initialize obs pointer
!     ----------------------

      obser => obs (n) % surface
      model   => mdl (n) % surface

! 3.2 Initialize error ground information arrays
!     ------------------------------------------

      err (n) % location   = obs (n) % location
      err (n) % info       = obs (n) % info
      err (n) % ground     = obs (n) % ground
      err (n) % valid_time = obs (n) % valid_time

      err (n) % info   % elevation   = &
      mdl (n) % info   % elevation 

      err (n) % ground % slp  % data = &
      mdl (n) % ground % slp  % data
      err (n) % ground % slp  % qc   = 0

      err (n) % ground % psfc % data = &
      mdl (n) % ground % psfc % data
      err (n) % ground % psfc % qc   = 0

! 3.3 Initialize model upper-air information pointers
!     -----------------------------------------------

      ALLOCATE (error)

      NULLIFY  (error % next)
      NULLIFY  (err (n) % surface)

vinterp:&
      DO WHILE (ASSOCIATED (obser))

! 3.4 At that point, all levels must have pressure and height 
!     -------------------------------------------------------

      IF ((eps_equal (obser % meas % height   % data, missing_r, 1.)) .OR. &
          (eps_equal (obser % meas % pressure % data, missing_r, 1.))) THEN 
           error_message = "Height and pressure are missing, cannot proceed"
           CALL error_handler (log_unit, ABS (missing), &
                proc_name//error_message, .TRUE., .TRUE.)
           CYCLE vinterp
      ENDIF

! 3.5 Same level observations
!     -----------------------

      ho   = obser % meas % height      % data
      po   = obser % meas % pressure    % data
      To   = obser % meas % temperature % data
      Tdo  = obser % meas % dew_point   % data
      rho  = obser % meas % rh          % data
      uo   = obser % meas % u           % data
      vo   = obser % meas % v           % data

      hm   = model % meas % height      % data
      pm   = model % meas % pressure    % data
      Tm   = model % meas % temperature % data
      Tdm  = model % meas % dew_point   % data
      rhm  = model % meas % rh          % data
      um   = model % meas % u           % data
      vm   = model % meas % v           % data

! 3.6 Need the model mixing ratio 
!     ---------------------------

! 4. VERTICAL INTERPOLATION OF OBSERVATIONAL ERROR BASED ON PRESSURE
! ==================================================================

! 4.1 Interpolate square of error to observation pressure level
!     ---------------------------------------------------------

      pmb = obser % meas % pressure % data * 0.01  ! hPa

      err_t  =  SQRT (intplin (pmb, Coord_mb, Temp_K (iunit,:)**2))     ! K
      err_q  =  SQRT (intplin (pmb, Coord_mb, Sp_Hum_gkg (iunit,:)**2))*1E-3 !kg
      err_uv =  SQRT (intplin (pmb, Coord_mb, uv_ms (iunit,:)**2))      ! m/s
      err_p  =  SQRT (intplin (pmb, Coord_mb, P_mb (iunit,:)**2))*1E+2  ! Pa
      err_h  =  SQRT (intplog (pmb, Coord_mb, H_m  (iunit,:)**2))       ! m
      err_rh =  SQRT (intplin (pmb, Coord_mb, RH_water (iunit,:)**2))   ! %

! 4.  OBSERVATIONAL SPECIFIC HUMIDITY
! ===================================

! 4.1 Skip if RH is missing
!     ---------------------

      IF (eps_not (rho, missing_r, 1.)) THEN

! 4.2 Replace the missing obs with model values
!     ----------------------------------------

          pom  = po
          Tom  = To

          IF (eps_equal (pom,  missing_r, 1.)) pom  = pm
          IF (eps_equal (Tom,  missing_r, 1.)) Tom  = Tm

! 4.3 Compute specific humidity
!     -------------------------

          !  Saturation water vapor pressure in Pa

          es  = es_alpha * EXP (es_beta*(Tom-T_kelvin) &
                                       /(Tom-T_kelvin+es_gamma))

          !  Water Vapor mixing ratio at saturation in kg/kg

          ws  = rd_over_rv * es /(pom-es)

          !  Water vapor mixing ratio in kg/kg

          wo  = 0.01*rho*ws

          !  Specific humidity in kg/kg

          qo    = wo / (1. - wo)
          qo_qc = obser % meas % rh % qc

      ELSE
          qo    = missing_r
          qo_qc = missing
      ENDIF

      WRITE (*,*) "rho = ",rho," wo  = ",wo," qo = ",qo , " qo_qc = ",qo_qc
!----------------------- FILL OBSERVATIONAL ERROR STRUCTURE -------------------!

! 5.  FILL OBSERVATIONAL ERROR STRUCTURE
! =======================================
 
! 5.1 Fill observational error data structure with know errors
!     --------------------------------------------------------

      error % meas % pressure    % data = err_p
      error % meas % height      % data = err_h
      error % meas % temperature % data = err_t
      error % meas % u           % data = err_uv
      error % meas % v           % data = err_uv
      error % meas % speed       % data = sqrt(2.) * err_uv

! 5.2 Reset observational error missing fields
!     -----------------------------------------

      error % meas % dew_point   % data = missing_r
      error % meas % thickness   % data = missing_r
      error % meas % rh          % data = missing_r
      error % meas % direction   % data = missing_r

! 5.3 Compute observational error for relative humidity
!     -------------------------------------------------

       !  Saturation water vapor pressure in Pa

       es  = es_alpha * EXP (es_beta*(Tm-T_kelvin) &
                           /(Tm-T_kelvin+es_gamma))

       des_over_es = es_beta*es_gamma * err_t &
                   /(Tm-T_kelvin+es_gamma)**2

       des = des_over_es * es

       write (*,*) " es = ",es," des_over_es = ",des_over_es
       !  Water Vapor mixing ratio at saturation in kg/kg

       ws   = rd_over_rv * es /(pm-es)

       dws_over_ws = (pm*des - es*err_p) / (es*(pm-es))

       dws = dws_over_ws * ws

       write (*,*) " ws = ",ws," dws_over_ws = ",dws_over_ws
       !  Specific humidity at saturation in kg/kg

       qs  = ws  / (1. + ws)

       dqs = dws / (1. + ws)**2

       write (*,*) " qs = ",qs," dqs = ",dqs
      stop
       !  Relative humidity as function of specific humidity

!      fm = 100. * wm/ws                   ! From Mixing ratio
       fm = 100. * qm/qs * (1.-qs)/(1.-qm) ! From specific humidity

       dfm_over_fm = err_q/(qm*(1.-qm)) - dqs/(qs*(1.-qs))   

       err_rh_lin = dfm_over_fm * fm 
       write (*,*) " fm = ",fm," qs = ",qs," qm = ",qm
       write (*,*) " err_rh_lin = ",err_rh_lin

      !  Bound Rh 0-100% and Rh error to 20%

       fm = MIN (100.,fm)
       err_rh_lin = MIN (err_rh_lin, 20.)

      WRITE (*,*) "fm = ",fm," err_rh_lin = ",err_rh_lin

! 5.4 Compute observational error for dew point
!     -----------------------------------------
       !  Td from rh

       invdifftd = log (fm/100.) / L_over_Rv
       invtd = 1./Tm  - invdifftd

       dewm  = 1./ invtd

       !  Td error from rh

       dinvdifftd = err_rh_lin/(fm*L_over_Rv) 
       dinvtd = -err_t/Tm**2  - dinvdifftd

       err_td = -dinvtd/invtd**2

      !  Bound Td and Td error to 10K

       dewm  = MIN (dewm, Tm)
       err_td =  MIN (err_td, 10.)

      WRITE (*,*) "dewm = ",dewm," err_td = ",err_td

! 5.5 Compute observational error for wind speed (neglect grid rotation)
!     ------------------------------------------

       IF (speedm > 0.) THEN
           err_dir = (vm - um) * err_uv
           err_dir = err_dir / (um**2. + vm**2)
           err_dir = ABS (err_dir)
       ELSE
           err_dir = 20.
       ENDIF

       err_dir = MAX (err_dir, 20.)

! 5.6 Fill with computed observational error fields
!    ----------------------------------------------

      error % meas % direction % data = err_dir
      error % meas % dew_point % data = err_td
      error % meas % rh        % data = err_rh

! 5.7 Copied obs QC
!     --------------

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

!------------------------------------------------------------------------------!

! 6. END
! ======

! 6.1 Link error levels
!      ----------------

      CALL link_levels (err (n) % surface, error)

      ALLOCATE (error)
      NULLIFY  (error % next)

! 6.2 Go to next obs level
!     --------------------

      obser => obser % next
      model => model % next

      ENDDO vinterp

! 6.3 Go to next station
!     ------------------

      ENDDO hinterp
  
! 6.4 end
!     ---

      RETURN

!------------------------------------------------------------------------------!
 END SUBROUTINE err_intp
!------------------------------------------------------------------------------!
