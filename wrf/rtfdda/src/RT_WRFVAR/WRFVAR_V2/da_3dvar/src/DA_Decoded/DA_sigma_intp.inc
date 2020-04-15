
 SUBROUTINE sigma_intp (max_number_obs, obs, mdl, err, index, number_obs, grid, &
                    ldebug, &
#include <em_dummy_args.inc> 
)
!------------------------------------------------------------------------------!
! Interpolate observations onto model half sigma levels.
!
! Francois Vandenberghe, February 2004
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
 
  REAL, ALLOCATABLE :: psad(:,:), pp_c(:,:,:) ,  tmp2d(:,:)

  TYPE(domain),                intent(inout) :: grid
!------------------------------------------------------------------------------!
! DECODED STRUCTURE

  TYPE (measurement), POINTER  :: current, model, error
!------------------------------------------------------------------------------!
! OBSERVATIONAL ERROR FILES

  INTEGER, PARAMETER :: err_levels = 33
  REAL,    DIMENSION   (err_levels) :: ptmp
  REAL,    DIMENSION   (err_levels) :: Coord_mb, Coord_mb_read
  REAL,    DIMENSION   (0:99,err_levels) :: Temp_K, Sp_Hum_gkg,&
                                            uv_ms,  P_mb, H_m, &
                                            RH_water
!------------------------------------------------------------------------------!
! MODEL TURBULENCE OBSERVATIONAL ERROR
! Rawinsonde at Random Location in measurement volume sampling error for
! Kolmogorov horizontal (longitudinal and latitudinal).
! See Frehlich 2001, JAOT, Vo. 18, pp 1749-1172.

  REAL, PARAMETER :: crrlmu = 0.85700  
  REAL, PARAMETER :: crrlmv = 0.85700
  REAL, PARAMETER :: crrlmt = 0.56093 

!------------------------------------------------------------------------------!
! OTHER VARIABLES

  CHARACTER (LEN =  80) :: err_type
  CHARACTER (LEN =  80) :: file_ou
  CHARACTER (LEN = 100) :: error_message

  INTEGER :: io_error
  INTEGER :: unit_ou
  INTEGER :: ic, jc
  INTEGER :: id, jd
  INTEGER :: kc, kd
  INTEGER :: kmin, kmax
  INTEGER :: kmin_next, kmax_next
  INTEGER :: unit_err, iunit
  INTEGER :: i, j, k, l, ll, n, nn
  INTEGER :: nbelow, nabove, nt, nrh, nu ,nv
  INTEGER :: qcu, qcv, qcuv, qo_qc

  REAL    :: djc, djmc, dic, dimc
  REAL    :: djd, djmd, did, dimd
  REAL    :: dkc, dkd
  REAL    :: xjc, yic, zkc
  REAL    :: xjd, yid, zkd
  REAL    :: pstarc, pstard, pslp, pp, psfc_1, hsfc

  REAL    :: err_t,  err_p, err_q, err_uv, err_rh  ! Innovation error
  REAL    :: err_td, err_rh_lin, err_h, err_dir

  REAL    :: errr_t, errr_uv

  REAL    :: sigma_p, sigma_q, sigma_u, sigma_t
  REAL    :: fm,  wm,  qm, dewm
  REAL    :: es,  ws,  qs
  REAL    :: des, dws, dqs
  REAL    :: des_over_es, dws_over_ws, dfm_over_fm

  REAL    :: To, Tdo, po, qo, wo, rho, uo, vo, ho
  REAL    :: Tm, Tdm, rhm, um, vm, edrm, ctm
  REAL    :: Tom, rhom, pom
  REAL    :: prefm, pfullm, ppm, pmb
  REAL    :: psfch, hsfch, pfullh
  REAL    :: hrefm, elevm
  REAL    :: speedm, dirm
  REAL    :: one_third, dx13
  REAL    :: invdifftd, invtd
  REAL    :: dinvdifftd, dinvtd
  REAL    :: prefc, prefd, beta
  REAL    :: pref_above, pref_below
  REAL    :: href_above, href_below

  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'sigma_intp '

  REAL    :: p00 , t00 , a

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
      WRITE  (log_unit,'(A)') "Interpolation on model sigma levels"


! 1.1 CONSTANTS INITIALIZATION
! ============================

! 1.1 Scalars
!     -------

      one_third = 1./3. 
      dx13 = xb % ds **one_third

! 1.2 Load ETA error as backup when turbulence is used for representativness
!     ----------------------------------------------------------------------

      IF (error_type == "TURBULENCE" .OR.  error_type == "turbulence") THEN
          err_type = "etaobs"
          CALL prelog
          WRITE  (log_unit,'(A)') &
         "Will compute Temp and Wind representativness error from turbulence."
          CALL prelog
          WRITE  (log_unit,'(A)') &
         "Use ETA observational error for other variables."
      ELSE
          err_type = error_type
      ENDIF

! 1.3 Load climatological errors profiles
!     -----------------------------------

      CALL error_levels (err_levels, Coord_mb_read, Temp_K, Sp_Hum_gkg, &
                         RH_water, uv_ms, P_mb, H_m, err_type, ldebug)

! 1.4 Pressure levels
!     ---------------

      Coord_mb_read = ABS (Coord_mb_read - Coord_mb)

      IF (MAXVAL (Coord_mb_read) > 1.) THEN 
          error_message = &
         "Read pressure levels do not match defined pressure levels."
          CALL error_handler (log_unit, 1, proc_name//error_message, &
                              .TRUE., .TRUE.)
      ENDIF

! 1.6 Use radiosonde for all upper-data 
!     ---------------------------------

      DO i = 0, 99
         Temp_K (i,:) = Temp_K (20,:)
         uv_ms  (i,:) = uv_ms  (20,:)
         P_mb   (i,:) = P_mb   (20,:)
         H_m    (i,:) = H_m    (20,:)
         RH_water (i,:) = RH_water (20,:)
         Sp_Hum_gkg (i,:) = Sp_Hum_gkg (20,:)
      ENDDO

! 1.7 Define surface (80 and 81) as the lowest level of sounding
!     ----------------------------------------------------------

      Temp_K (80:81,:)     = Temp_K (20,1)
      uv_ms  (80:81,:)     = uv_ms  (20,1)
      P_mb   (80:81,:)     = P_mb   (20,1)
      H_m    (80:81,:)     = H_m    (20,1)
      RH_water (80:81,:)   = RH_water (20,1)
      Sp_Hum_gkg (80:81,:) = Sp_Hum_gkg (20,1)

! 1.8 Write out error in file
!     -----------------------

      file_ou = "errsnd_" // TRIM (err_type) // ".txt"
      unit_ou = 99

      CALL prelog
      WRITE (UNIT = log_unit, FMT = '(A,A)') &
            "Write radiosonde error profiles in file: ",TRIM (file_ou)

! 1.9 Open output ascii file
!     ----------------------

      OPEN (UNIT = unit_ou , FILE = file_ou, FORM = 'FORMATTED'  , &
           ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error /=  0) THEN
          CALL prelog
          WRITE (UNIT = log_unit, FMT = '(A,A,/)') &
         "Unable to open output file ",TRIM (file_ou)
      ELSE

! 1.10 Write header
!     ------------

          WRITE (unit_ou,'(A)') &
         "Level   Temp K    UV m/s    RHum %  Pres hPa   Heigh m"

! 1.11 Write profiles
!     ---------------

          DO k = err_levels, 1, -1
             WRITE (unit_ou,'(I4,5(2X,F8.1))')   NINT (Coord_mb (k)), &
                    Temp_K   (20,k), uv_ms (20,k), &
                    RH_water (20,k), P_mb (20,k),  &
                    H_m (20,k)
          ENDDO

! 1.12 Close file
!     -----------

          CLOSE (UNIT=unit_ou)

      ENDIF

      ALLOCATE( psad( xb%its : xb%ite, xb%jts : xb%jte) )
      ALLOCATE( pp_c( xb%its : xb%ite, xb%jts : xb%jte, xb%kts : xb%kte) )

   !  To define the base state, we call a USER MODIFIED routine to set the three
   !  necessary constants: p00 (sea level pressure, Pa), t00 (sea level temperature, K),
   !  and A (temperature difference, from 1000 mb to 300 mb, K).

   !   CALL const_module_initialize ( p00 , t00 , a )

      p00 = 100000.
      t00 = 290.
      a   = 50.

      do i =  xb%its, xb%ite
      do j =  xb%jts, xb%jte
        ! mub(i,j) = p_surf - p_top
        ! p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*ht(i,j)/a/r_d ) **0.5 )
        ! r_d= 287.04, g=9.81 , p_top = 5000.0
        ! /raid/QC/WRFV2_VAR/share/module_model_constants.F
        ! /raid/QC/WRFV2_VAR/dyn_em/module_initialize_real.F
        mub(i,j) = p00*EXP(-t00/a + ((t00/a)**2-2.*9.81*xb%terr(i,j)/a/287.04)**0.5) - xb%ptop
        do k =  xb%kts, xb%kte
          pp_c(i,j,k) = xb%p(i,j,k) - (xb%sigmah(k) * mub(i,j) + xb%ptop)
        end do
      end do
      end do

      ALLOCATE( tmp2d( xb%its : xb%ite, xb%jts : xb%jte) )
      tmp2d = xb % psac (xb%its:xb%ite, xb%jts:xb%jte)
      CALL CRS_TO_DOT( tmp2d, psad, xb%ite - xb%its + 1, xb%jte - xb%jts + 1 )	

! 2.  MODEL HORIZONTAL INTERPOLATION AT STATIONS LOCATIONS
! ========================================================

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

! 2.2 Station coordinates on model grid
!     ----------------------------------

      CALL llxy (obs (n) % location%latitude, obs (n) % location % longitude, &
                 xjd, yid)
 
     !  Coordinates at cross point
      xjc = xjd - 0.5
      yic = yid - 0.5

! 2.3 Horizontal interpolation weights at cross points
!     ------------------------------------------------

      call x_to_j (yic, xb%jte - xb%jts + 1, jc, djc)
      call x_to_j (xjc, xb%ite - xb%its + 1, ic, dic)

      dimc = 1. - dic
      djmc = 1. - djc

! 2.4 Horizontal interpolation weights at dot points
!     ----------------------------------------------

      call x_to_j (yid, xb%jte - xb%jts + 1, jd, djd)
      call x_to_j (xjd, xb%ite - xb%its + 1, id, did)

      dimd = 1. - did
      djmd = 1. - djd

! 2.5 Model elevations at station location
!     ------------------------------------

      elevm = interp2d (ic, dic, jc, djc, xb%terr, xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
	print*,'xb%terr=',xb%terr

! 2.6 Model surface and sea level pressure at station location
!     --------------------------------------------------------
      pstard = interp2d (id, did, jd, djd, psad,    xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
      tmp2d = xb % psac (xb%its:xb%ite, xb%jts:xb%jte)
      pstarc = interp2d (ic, dic, jc, djc, tmp2d, xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
      tmp2d = xb % slp (xb%its:xb%ite, xb%jts:xb%jte)
      pslp   = interp2d (ic, dic, jc, djc, tmp2d,  xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
      pp     = interp2d (ic, dic, jc, djc, pp_c (:,:, xb%kts),  &
                         xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

      psfch = pstarc * znu (xb % kts) + xb % ptop
      call  DA_Ref_Height (psfch, hsfch)

      pfullh = psfch + pp

! 2.7 Observational/forecast error depends on platform type
!     -----------------------------------------------------

      iunit = oberr_unit (obs (n) % location % name, &
                          obs (n) % info     % platform)


! 3.  DEFINE MODEL INTERPOLATED AND ERROR DATA STRUCTURE
! ======================================================

! 3.1 Initialize model interpolated ground information data structure
!    ----------------------------------------------------------------

      mdl (n) % location   = obs (n) % location
      mdl (n) % info       = obs (n) % info
      mdl (n) % ground     = obs (n) % ground
      mdl (n) % valid_time = obs (n) % valid_time

      mdl (n) % info   % elevation   = ANINT (hsfch)
      mdl (n) % ground % slp  % data = ANINT (pslp)
      mdl (n) % ground % slp  % qc   = 0
      mdl (n) % ground % psfc % data = ANINT (pfullh)
      mdl (n) % ground % psfc % qc   = 0

! 3.2 Initialize error interpolated ground information data structure
!     ----------------------------------------------------------------

      err (n) % location   = obs (n) % location
      err (n) % info       = obs (n) % info
      err (n) % ground     = obs (n) % ground
      err (n) % valid_time = obs (n) % valid_time

      err (n) % info   % elevation   = ANINT (hsfch)
      err (n) % ground % slp  % data = ANINT (pslp)
      err (n) % ground % slp  % qc   = 0
      err (n) % ground % psfc % data = ANINT (pfullh)
      err (n) % ground % psfc % qc   = 0


! 4.  COUNT HOW MANY LEVELS ARE BELOW MODEL LOWEST HALF SIGMA LEVEL
! =================================================================

! 4.1 Initialize pointers
!     -------------------

      nabove = 0
      nbelow = 0
      nt = 0; nu = 0; nv = 0; nrh = 0;

      current => obs (n) % surface

! 4.2 Go through levels
!     -----------------

      DO WHILE (ASSOCIATED (current))

! 4.3 At that point, all levels must have pressure and height 
!     -------------------------------------------------------

         IF ((eps_equal (current % meas % height   % data, missing_r, 1.)) .OR.&
             (eps_equal (current % meas % pressure % data, missing_r, 1.))) THEN
              error_message = "Height and pressure are missing, cannot proceed"
              CALL error_handler (log_unit, ABS (missing), &
                   proc_name//error_message, .TRUE., .TRUE.)
         ENDIF

! 4.4 Level model coordinates
!     -----------------------

         CALL h_to_kmm5 (xb, pstarc, current % meas % height % data, kc, dkc)
         CALL h_to_kmm5 (xb, pstard, current % meas % height % data, kd, dkd)
 
         kmax = MAX (kc, kd)
         kmin = MIN (kc, kd)

! 4.5 Level above model top should not be found here
!     ----------------------------------------------

         IF (kmax > xb % kte) THEN
             error_message = "Level above the model top, cannot interpolate"
             CALL error_handler (log_unit, kmax, &
                  proc_name//error_message, .TRUE., .TRUE.)
         ENDIF


! 4.6 Count levels
!     ------------

         IF (kmin <= xb % kts ) THEN
             nbelow = nbelow + 1
         ELSE

! 4.7 Count the fields above the lowest half sigma levels
!     ---------------------------------------------------

             nabove = nabove + 1

             IF (eps_not (current % meas % temperature % data, missing_r, 1.)) &
                 nt = nt + 1

             IF (eps_not (current % meas % rh % data, missing_r, 1.)) &
                 nrh = nrh + 1

             IF (eps_not (current % meas % u  % data, missing_r, 1.)) &
                 nu = nu + 1

             IF (eps_not (current % meas % v  % data, missing_r, 1.)) &
                 nv = nv + 1

         ENDIF

! 4.8 Go to next level
!     ----------------

         current => current % next

      ENDDO
          
! 4.9 Discard single level obs above model half sigma levels
!     ------------------------------------------------------
!     We just cannot interpolate

      IF (nabove == 1) THEN
          mdl (n) % info % discard = .TRUE.
          err (n) % info % discard = .TRUE.
          CYCLE hinterp
      ENDIF

! 4.10 Initialize pointers
!      -------------------

      ALLOCATE (model)
      NULLIFY  (model % next)
      NULLIFY  (mdl (n) % surface)

      ALLOCATE (error)
      NULLIFY  (error % next)
      NULLIFY  (err (n) % surface)


! 5.  PROCESS LEVELS BELOW MODEL HALF SIGMA LEVELS
! ================================================

! 5.1 Keep the highest below the surface
!     ----------------------------------

below:&
      IF (nbelow > 0) THEN

      current => obs (n) % surface

      DO n = 1, nbelow-1
         current => current % next
      ENDDO
  
! 5.2 Vertical coordinates of lowest level
!     ------------------------------------

      CALL h_to_kmm5 (xb, pstarc, current % meas % height % data, kc, dkc)
      CALL h_to_kmm5 (xb, pstard, current % meas % height % data, kd, dkd)
 
      kmax = MAX (kc, kd)
      kmin = MIN (kc, kd)

! 5.3 Make sure coordinates are reasonable
!     ------------------------------------

      IF (kmin > xb % kts ) THEN
          CALL prelog 
          WRITE (error_message,'(A,2(A,I2))') &
         "Level should be below surface but"," kmin = ",kmin," kmax = ",kmax
          CALL error_handler (log_unit, kmax, &
               proc_name//error_message, .TRUE., .TRUE.)
      ENDIF


! 5.4 Surface station above lowest half sigma level cannot be processed either
!     ------------------------------------------------------------------------

      IF (kmin > xb % kts ) THEN
          error_message = "Surface station above model lowest sigma layer, cannot interpolate"
          CALL error_handler (log_unit, kmax, &
               proc_name//error_message, .TRUE., .TRUE.)
          CYCLE hinterp
      ENDIF

! 5.5 Assign model height
!     -------------------

      ho  = hsfch

! 5.6 Compute pressure at model surface using model lapse rate
!     --------------------------------------------------------

      IF (hsfch > 0) THEN
          beta = ALOG (pslp/psfch)/hsfch
	  po   = current % meas % pressure % data &
	       * EXP (-beta * (hsfch-current % meas % height % data))
      ELSE
          po =  psfch
      ENDIF

! 5.7 Correct surface temperature using a lapse rate
!     ----------------------------------------------

       IF (eps_not (current % meas % temperature % data, missing_r, 1.)) THEN
           To =  current % meas % temperature % data &
	      + (hsfch - current % meas % height % data)*tlapse
       ELSE
           To = missing_r
       ENDIF

! 5.8 Compute mixing ratio from relative humidity
!     -------------------------------------------

       IF (eps_not (current % meas % temperature % data, missing_r, 1.) .AND. &
           eps_not (current % meas % rh % data, missing_r, 1.)) THEN

          !  Saturation water vapor pressure in Pa

          es  = es_alpha * EXP (es_beta*(To - T_kelvin) &
                                       /(To - T_kelvin+es_gamma))

          !  Water Vapor mixing ratio at saturation in kg/kg

          ws  = rd_over_rv * es /(po-es)

          !  Water vapor mixing ratio in kg/kg

          wo  = 0.01*current % meas % rh % data*ws

       ELSE
          wo = missing_r
       ENDIF


! 6.  FILL SURFACE MODEL INTERPOLATED AND DATA STRUCTURES
! =======================================================

! 6.1 Let wind unchanged replace rh with w and and fill the structure
!     ---------------------------------------------------------------
 
      model % meas % pressure    % data   = po
      model % meas % height      % data   = ho
      model % meas % temperature % data   = To
      model % meas % rh          % data   = wo
      model % meas % dew_point   % data   = current % meas % dew_point   % data
      model % meas % speed       % data   = current % meas % speed       % data
      model % meas % direction   % data   = current % meas % direction   % data
      model % meas % u           % data   = current % meas % u           % data
      model % meas % v           % data   = current % meas % v           % data
      model % meas % thickness   % data   = current % meas % thickness   % data

! 6.2 Copy obs QC
!     ------------

      model % meas % pressure    % qc   = current % meas % pressure    % qc
      model % meas % height      % qc   = current % meas % height      % qc
      model % meas % temperature % qc   = current % meas % temperature % qc
      model % meas % dew_point   % qc   = current % meas % dew_point   % qc
      model % meas % speed       % qc   = current % meas % speed       % qc
      model % meas % direction   % qc   = current % meas % direction   % qc
      model % meas % u           % qc   = current % meas % u           % qc
      model % meas % v           % qc   = current % meas % v           % qc
      model % meas % rh          % qc   = current % meas % rh          % qc
      model % meas % thickness   % qc   = current % meas % thickness   % qc


! 6.3 Link model interpolated levels
!     ------------------------------

      CALL link_levels (mdl (n) % surface, model)

      ALLOCATE (model)
      NULLIFY  (model % next)


! 7.  FILL SURFACE ERROR DATA STRUCTURES
! ======================================

! 7.1 Interpolate square of error to observation pressure level
!     ---------------------------------------------------------

      pmb = po / 100.

      err_t  =  SQRT (intplin (pmb, Coord_mb, Temp_K (iunit,:)**2))     ! K
      err_q  =  SQRT (intplin (pmb, Coord_mb, Sp_Hum_gkg (iunit,:)**2))*1E-3 !kg
      err_uv =  SQRT (intplin (pmb, Coord_mb, uv_ms (iunit,:)**2))      ! m/s
      err_p  =  SQRT (intplin (pmb, Coord_mb, P_mb (iunit,:)**2))*1E+2  ! Pa
      err_h  =  SQRT (intplog (pmb, Coord_mb, H_m  (iunit,:)**2))       ! m
      err_rh =  SQRT (intplin (pmb, Coord_mb, RH_water (iunit,:)**2))   ! %

! 7.2 Replace rh with w and and fill the error structure
!     --------------------------------------------------
 
      error % meas % pressure    % data   = err_p
      error % meas % height      % data   = err_h
      error % meas % temperature % data   = err_t
      error % meas % rh          % data   = err_q
      error % meas % dew_point   % data   = err_t
      error % meas % speed       % data   = err_uv*SQRT (2.)
      error % meas % direction   % data   = 20.
      error % meas % u           % data   = err_uv
      error % meas % v           % data   = err_uv
      error % meas % thickness   % data   = missing_r

! 7.3 Copy obs QC
!     ------------

      error % meas % pressure    % qc   = current % meas % pressure    % qc
      error % meas % height      % qc   = current % meas % height      % qc
      error % meas % temperature % qc   = current % meas % temperature % qc
      error % meas % dew_point   % qc   = current % meas % dew_point   % qc
      error % meas % speed       % qc   = current % meas % speed       % qc
      error % meas % direction   % qc   = current % meas % direction   % qc
      error % meas % u           % qc   = current % meas % u           % qc
      error % meas % v           % qc   = current % meas % v           % qc
      error % meas % rh          % qc   = current % meas % rh          % qc
      error % meas % thickness   % qc   = current % meas % thickness   % qc

! 7.4 Link model interpolated levels
!     ------------------------------

      CALL link_levels (err (n) % surface, error)

      ALLOCATE (error)
      NULLIFY  (error % next)

! 7.5 Print-out
!     ---------

      CALL prelog 
      WRITE (log_unit,'(A,2(A,I2))') "Level below surface:"," kmin = ", kmin,&
                                                            " kmax = ",kmax

       WRITE (log_unit,'(4(A,F9.3))') &
                             " Hs = ", Hsfch, &
                             " Ho = ", current % meas % height % data, &
                             " Hm = ", ho, &
                             " He = ", err_h

       WRITE (log_unit,'(4(A,F9.3))') &
                             " Ps = ", pfullh/100., &
                             " Po = ", current % meas % pressure % data/100., &
                             " Pm = ", po/100., &
                             " Pe = ", err_p/100.

       WRITE (log_unit,'(3(A,F9.3))') &
                             " To = ", current % meas % temperature % data, &
                             " Tm = ", To, &
                             " Te = ", err_t

! 7.6 Go to next level
!     ----------------

      current => current % next

      ENDIF below

! 7.7 Skip to next stations if surface station only
!     ---------------------------------------------

      IF (.NOT. ASSOCIATED (current)) CYCLE hinterp


! 8.  UPPER AIR LEVELS
! ====================

      CALL prelog 
      WRITE (log_unit,'(A,2(A,I2))') &
     "Level above surface:"," kmin = ",kmin," kmax = ",kmax

! 8.1 Process when at least two levels are present
!     --------------------------------------------

      IF (nabove <= 1) CYCLE hinterp

! 8.2 Loop over upper levels
!     ----------------------

      DO WHILE (ASSOCIATED (current) .AND. ASSOCIATED (current % next))

! 8.3 Vertical coordinates of current level
!     ------------------------------------

      CALL h_to_kmm5 (xb, pstarc, current % meas % height % data, kc, dkc)
      CALL h_to_kmm5 (xb, pstard, current % meas % height % data, kd, dkd)
 
      kmax = MAX (kc, kd)
      kmin = MIN (kc, kd)

! 8.3 Vertical coordinates of next level
!     -----------------------------------

      CALL h_to_kmm5 (xb, pstarc, current % meas % height % data, kc, dkc)
      CALL h_to_kmm5 (xb, pstard, current % meas % height % data, kd, dkd)
 
      kmax_next = MAX (kc, kd)
      kmin_next = MIN (kc, kd)

! 8.4 Interpolation
!     -------------

!        IF (eps_not (current % meas % temperature % data, missing_r, 1.)) THEN
!            temp (n) % data = current % meas % temperature % data
!            temp (n) % pmb  = current % meas % pressure % data
!            temp (n) % ind  = l
!            n = n + 1
!            l = l + 1
!        ENDIF

         current = current % next

      ENDDO


! 8.3 Go to next station
!     ------------------

      ENDDO hinterp
 
      DEALLOCATE( psad )
      DEALLOCATE( pp_c )
      DEALLOCATE( tmp2d )


 END SUBROUTINE sigma_intp
!------------------------------------------------------------------------------!
