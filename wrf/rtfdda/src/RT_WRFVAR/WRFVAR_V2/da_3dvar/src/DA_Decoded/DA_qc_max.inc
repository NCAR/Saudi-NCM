
 SUBROUTINE qc_max (max_number_obs, obs, mdl, err, index, number_obs, &
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
  INTRINSIC MAX

! ARGUMENTS

  INTEGER,        INTENT (in)  :: max_number_obs
  INTEGER,        INTENT (in)  :: number_obs
  INTEGER,        INTENT (in),  DIMENSION (max_number_obs) :: index
  TYPE (report),  INTENT (in),  DIMENSION (max_number_obs) :: obs
  TYPE (report),  INTENT (out), DIMENSION (max_number_obs) :: mdl
  TYPE (report),  INTENT (out), DIMENSION (max_number_obs) :: err
  LOGICAL,        INTENT (in)  :: ldebug

!------------------------------------------------------------------------------!
! DECODED STRUCTURE

  TYPE (measurement), POINTER  :: current
  TYPE (measurement), POINTER  :: model
  TYPE (measurement), POINTER  :: error
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
! QC weighting with representativness error 
!  QC = QCcut when Err_rep/Err_fct = Rcut
!
!yliu  REAL, PARAMETER :: Rcut = 2.
  REAL, PARAMETER :: Rcut = 2.0
! REAL, PARAMETER :: Rcut = 0.5
  REAL, PARAMETER :: Qcut = 0.5
!
  REAL :: sigma2_cut, coef_cut
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
  INTEGER :: unit_err, iunit
  INTEGER :: i, j, k, l, ll, n, nn
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
  REAL    :: hrefm, elevm
  REAL    :: speedm, dirm
  REAL    :: one_third, dx13
  REAL    :: invdifftd, invtd
  REAL    :: dinvdifftd, dinvtd
  REAL    :: prefc, prefd, beta
  REAL    :: pref_above, pref_below
  REAL    :: href_above, href_below

  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'qc_max '

  REAL, ALLOCATABLE :: psad(:,:) ,  tmp2d(:,:)
  REAL, ALLOCATABLE :: pp_c(:,:,:), tmp3d(:,:,:), tmp3d_2(:,:,:), tmp3d_3(:,:,:)

  integer, parameter                 :: idel  = 3, kdel = 0
  integer, parameter                 :: k1 = 1, nlags = 2 * idel 
  REAL, DIMENSION(:,:,:), ALLOCATABLE:: edr, ct_1
  REAL    :: p00 , t00 , a
  
  TYPE(domain),                intent(inout) :: grid

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
      WRITE  (log_unit,'(A)') "Max error QC:"


! 1.1 CONSTANTS INITIALIZATION
! ============================

! 1.1 Scalars
!     -------

      one_third = 1./3. 
      dx13 = xb % ds **one_third
      coef_cut   = LOG (Qcut)/rcut**2
      sigma2_cut = -1./(2.*coef_cut)

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
      CALL crs_to_dot( tmp2d, psad, xb%ite - xb%its + 1, xb%jte - xb%jts + 1 )

      ALLOCATE( tmp3d( xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte) )
      ALLOCATE( tmp3d_2( xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte) )
      ALLOCATE( tmp3d_3( xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte) )

      ALLOCATE (edr  (xb%its : xb%ite, xb%jts : xb%jte, xb%kts : xb%kte))
      ALLOCATE (ct_1 (xb%its : xb%ite, xb%jts : xb%jte, xb%kts : xb%kte))

      edr = 0.
      ct_1  = 0.

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

      call x_to_j (xjc, xb % ite - xb % its + 1, ic, dic)
      call x_to_j (yic, xb % jte - xb % jts + 1, jc, djc)

      dimc = 1. - dic
      djmc = 1. - djc

! 2.4 Horizontal interpolation weights at dot points
!     ----------------------------------------------

      call x_to_j (xjd, xb % ite - xb % its + 1, id, did)
      call x_to_j (yid, xb % jte - xb % jts + 1, jd, djd)

      dimd = 1. - did
      djmd = 1. - djd

! 2.5 Model terrain elevations at station location
!     ---------------------------------------------
      tmp2d = xb % terr (xb%its:xb%ite, xb%jts:xb%jte)

      elevm = interp2d (ic, dic, jc, djc, tmp2d, xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

! 2.6 Model surface and sea level pressure at station location
!     --------------------------------------------------------
      
      pstard = interp2d (id, did, jd, djd, psad,    xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
      tmp2d = xb % psac (xb%its:xb%ite, xb%jts:xb%jte)
      pstarc = interp2d (ic, dic, jc, djc, tmp2d, xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
      tmp2d = xb % slp (xb%its:xb%ite, xb%jts:xb%jte)
      pslp   = interp2d (ic, dic, jc, djc, tmp2d,  xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
      pp     = interp2d (ic, dic, jc, djc, pp_c (:,:, xb%kts),  &
                         xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

      psfc_1 = pstarc + xb % ptop
      call DA_Ref_Height (psfc_1, hsfc)
      psfc_1 = psfc_1 + pp

      WRITE (*,*) " Terrain = ",elevm
      WRITE (*,*) " Surface = ",hsfc
      WRITE (*,*) " Pressur = ",psfc_1
     

! 2.7 Observational/forecast error depends on platform type
!     -----------------------------------------------------

      iunit = oberr_unit (obs (n) % location % name, &
                          obs (n) % info     % platform)

! 2.8 QC standard deviation
!     ---------------------

      IF (obs (n) % info % platform (1:11) == "FM-35 TEMP " .OR. &
          obs (n) % info % platform (1:11) == "FM-32 PILOT" .OR. &
          obs (n) % info % platform (1:11) == "FM-32 TEMP " ) THEN 
          sigma_p = tolerance_p_sound
          sigma_q = tolerance_q_sound
          sigma_u = tolerance_u_sound
          sigma_t = tolerance_t_sound
      ELSE
          sigma_p = tolerance_p
          sigma_q = tolerance_q
          sigma_u = tolerance_u
          sigma_t = tolerance_t
      ENDIF

! 3.  UPPER LEVELS QC PREPROCESSING
! =================================

! 3.1 Initialize obs pointer
!     ----------------------

      current => obs (n) % surface

! 3.2 Initialize model ground information arrays
!     ------------------------------------------

      mdl (n) % location   = obs (n) % location
      mdl (n) % info       = obs (n) % info
      mdl (n) % ground     = obs (n) % ground
      mdl (n) % valid_time = obs (n) % valid_time

      mdl (n) % info   % elevation   = ANINT (elevm)
      mdl (n) % ground % slp  % data = ANINT (pslp)
      mdl (n) % ground % slp  % qc   = 0
      mdl (n) % ground % psfc % data = ANINT (psfc_1)
      mdl (n) % ground % psfc % qc   = 0

! 3.3 Initialize model ground information arrays
!     ------------------------------------------

      err (n) % location   = obs (n) % location
      err (n) % info       = obs (n) % info
      err (n) % ground     = obs (n) % ground
      err (n) % valid_time = obs (n) % valid_time

      err (n) % info   % elevation   = ANINT (elevm)
      err (n) % ground % slp  % data = ANINT (pslp)
      err (n) % ground % slp  % qc   = 0
      err (n) % ground % psfc % data = ANINT (psfc_1)
      err (n) % ground % psfc % qc   = 0

! 3.3 Initialize model upper-air information pointers
!     -----------------------------------------------


      ALLOCATE (model)
      NULLIFY  (model % next)
      NULLIFY  (mdl (n) % surface)  

      ALLOCATE (error)

      NULLIFY  (error % next)
      NULLIFY  (err (n) % surface)

vinterp:&
      DO WHILE (ASSOCIATED (current))

! 3.4 At that point, all levels must have pressure and height 
!     -------------------------------------------------------

      IF ((eps_equal (current % meas % height   % data, missing_r, 1.)) .OR. &
          (eps_equal (current % meas % pressure % data, missing_r, 1.))) THEN 
           error_message = "Height and pressure are missing, cannot proceed"
           CALL error_handler (log_unit, ABS (missing), &
                proc_name//error_message, .TRUE., .TRUE.)
           CYCLE vinterp
      ENDIF

! 3.5 Same level observations
!     -----------------------

      ho   = current % meas % height      % data
      po   = current % meas % pressure    % data
      To   = current % meas % temperature % data
      Tdo  = current % meas % dew_point   % data
      rho  = current % meas % rh          % data
      uo   = current % meas % u           % data
      vo   = current % meas % v           % data

! 4. VERTICAL INTERPOLATION OF OBSERVATIONAL ERROR BASED ON PRESSURE
! ==================================================================

! 4.1 Interpolate square of error to observation pressure level
!     ---------------------------------------------------------

      pmb = current % meas % pressure % data * 0.01  ! hPa

      err_t  =  SQRT (intplin (pmb, Coord_mb, Temp_K (iunit,:)**2))     ! K
      err_q  =  SQRT (intplin (pmb, Coord_mb, Sp_Hum_gkg (iunit,:)**2))*1E-3 !kg
      err_uv =  SQRT (intplin (pmb, Coord_mb, uv_ms (iunit,:)**2))      ! m/s
      err_p  =  SQRT (intplin (pmb, Coord_mb, P_mb (iunit,:)**2))*1E+2  ! Pa
      err_h  =  SQRT (intplog (pmb, Coord_mb, H_m  (iunit,:)**2))       ! m
      err_rh =  SQRT (intplin (pmb, Coord_mb, RH_water (iunit,:)**2))   ! %

! 5. MODEL INTERPOLATION BASED ON HEIGHT
! ======================================

! 5.1 Vertical coordinates based on height
!     ------------------------------------

      CALL h_to_kmm5 (xb, pstarc, current % meas % height % data, kc, dkc )
      CALL h_to_kmm5 (xb, pstard, current % meas % height % data, kd, dkd )
 
      kmax = MAX (kc, kd)
      kmin = MIN (kc, kd)

! 5.2 Level above model top should not be found here
!     ----------------------------------------------

      IF (kmax > xb % kte) THEN
          error_message = "Level above the model top, cannot interpolate"
          CALL error_handler (log_unit, kmax, &
               proc_name//error_message, .TRUE., .TRUE.)
          CYCLE vinterp
      ENDIF

! 6.  ALL SURFACE STATIONS AND SOUNDING LEVELS BELOW LOWEST HALF SIGMA LEVEL
! ==========================================================================

      IF (.NOT. obs (n) % info % is_sound .OR. kmin <= xb % kts ) THEN

! 6.1 10m wind u-component
!     --------------------
	tmp2d = xb % u10 (xb%its:xb%ite, xb%jts:xb%jte)
        um = interp2d (id, djd, jd, did, tmp2d, xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

! 6.2 10m wind v-component
!     --------------------
	tmp2d = xb % v10 (xb%its:xb%ite, xb%jts:xb%jte)
        vm = interp2d (id, did, jd, djd, tmp2d, xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

! 6.3 2m Temperature
!     --------------

	tmp2d = xb % t2 (xb%its:xb%ite, xb%jts:xb%jte)
	Tm = interp2d (ic, dic, jc, djc, tmp2d, xb%ite - xb%its + 1, xb%jte - xb%jts + 1 )

! 6.4 Dew point
!     ---------
	tmp2d = xb % td (xb%its:xb%ite, xb%jts:xb%jte, xb%kts )
        tdm = interp2d (ic, dic, jc, djc, tmp2d, &
                          xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

! 6.5 Relative Humidity
!     -----------------

	tmp2d = xb % rh (xb%its:xb%ite, xb%jts:xb%jte,xb%kts )
        rhm = interp2d (ic, dic, jc, djc, tmp2d, &
                          xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

! 6.6 Specific Humidity
!     -----------------

        !  Water vapor mixing ratio
	tmp2d = xb % q (xb%its:xb%ite, xb%jts:xb%jte,xb%kts )
        wm = interp2d (ic, dic, jc, djc, tmp2d, &
                         xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

        qm = wm / (1. + wm)

! 6.7 Pressure perturbation
!     ---------------------
        ppm = interp2d (ic, dic, jc, djc, pp_c (:,:,xb%kts ), &
                          xb%ite - xb%its + 1, xb%jte - xb%jts + 1)

! 6.8 Reference pressure
!     ------------------

	call DA_Ref_Pres (current % meas % height % data, prefm)

! 6.9 Full pressure
!     -------------

        pfullm = prefm + ppm

! 6.10 Reference height
!      ----------------

	call DA_ref_height (prefm, hrefm)

! 6.11 Below surface level extrapolation with sea level pres and temp lapse rate
!      -------------------------------------------------------------------------

        IF (kmin < xb % kts) THEN

! 6.12 Correct surface temperature using a lapse rate
!      ----------------------------------------------

           Tm = Tm + (hsfc - current % meas % height % data)*tlapse

! 6.13 Interpolate pressure between surface and sea level
!      --------------------------------------------------

            IF (hsfc > 0) THEN
                  beta   = ALOG (pslp/psfc_1)/hsfc
                  pfullm =  pslp* EXP (-beta * current % meas % height % data)
            ELSE
                  pfullm =  pslp
            ENDIF

! 6.14 Above surface level extrapolation temp lapse rate
!      -------------------------------------------------

          ELSE IF (kmin > xb % kts) THEN

! 6.15 Correct surface temperature using a lapse rate
!      ----------------------------------------------

              Tm = Tm + (hsfc - current % meas % height % data)*tlapse

          ENDIF

! 7.  SOUNDING LEVELS ABOVE LOWEST HALF SIGMA LEVEL
! =================================================

      ELSE IF (kmin > xb % kts) THEN

! 7.1 wind u-component
!     ----------------
	  tmp3d = xb % u (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)
          um = interp3d (id, did, jd, djd, kd, dkd, tmp3d, &
                    xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)

! 7.2 wind v-component
!     ----------------
	  tmp3d = xb % v (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)
          vm = interp3d (id, did, jd, djd, kd, dkd, tmp3d, &
                    xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)

! 7.3 Temperature
!     -----------
          tmp3d = xb % t (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)

          Tm = interp3d (ic, dic, jc, djc, kc, dkc, tmp3d, &
                    xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)

! 7.4 Dew point
!     ---------
	  tmp3d = xb % td (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)
          tdm = interp3d (ic, dic, jc, djc, kc, dkc, tmp3d, &
                     xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)

! 7.5 Relative Humidity
!     -----------------
	  tmp3d = xb % rh (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)
          rhm = interp3d (ic, dic, jc, djc, kc, dkc, tmp3d, &
                     xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)

! 7.6 Specific Humidity
!     -----------------

          !  Water vapor mixing ratio
          tmp3d = xb % q (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)
          wm = interp3d (ic, dic, jc, djc, kc, dkc, tmp3d, &
                         xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)

          qm = wm / (1. + wm)

! 7.7 Pressure perturbation
!     ---------------------

          ppm = interp3d (ic, dic, jc, djc, kc, dkc, pp_c, &
                         xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)

! 7.8 Reference pressure
!     ------------------

          call DA_Ref_Pres (current % meas % height % data, prefm)

! 7.9 Full pressure
!     -------------

          pfullm = prefm + ppm

! 7.10 Reference height
!      ----------------

	  call DA_ref_height (prefm, hrefm)

      ELSE

! 7.11 Other situations are not allowed
!     ---------------------------------

          WRITE (error_message,'(2(A,I3))') & 
         "Unknown location kc = ",kc," kd = ",kd 
          CALL error_handler (log_unit, kmin, &
               proc_name//error_message, .TRUE., .TRUE.)
          CYCLE vinterp

      ENDIF

! 8.  OBSERVATIONAL SPECIFIC HUMIDITY
! ===================================

! 8.1 Skip if RH is missing
!     ---------------------

      IF (eps_not (rho, missing_r, 1.)) THEN

! 8.2 Replace the missing obs with model values
!     ----------------------------------------

          pom  = po
          Tom  = To

          IF (eps_equal (pom,  missing_r, 1.)) pom  = pfullm
          IF (eps_equal (Tom,  missing_r, 1.)) Tom  = Tm

! 8.3 Compute specific humidity
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
          qo_qc = current % meas % rh % qc

      ELSE
          qo    = missing_r
          qo_qc = missing
      ENDIF

! 9.  MODEL-DERIVED TURBULENCE AS REPRESENTATIVNESS ERROR
! =======================================================

      IF (error_type == "TURBULENCE" .OR. error_type == "turbulence" .OR. &
          turbulence_weighting) THEN

! 9.1 Wind energy dissipation rate
!     ----------------------------

         !  edr and ct_1 calculations

         !  Initialization (structure functions coeficients computations)

         CALL sfnedr2dinit (nlags)

         !  edr and ct_1 computations
	 
	 tmp3d   = xb % u (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)
	 tmp3d_2 = xb % v (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)
         tmp3d_3 = xb % t (xb%its:xb%ite, xb%jts:xb%jte, xb%kts:xb%kte)

 	 DO k = xb%kts + kdel + 1, xb%kte - kdel
	   DO j = xb%its + idel+1, xb%ite - idel
           DO i = xb%jts + idel+1, xb%jte - idel
             CALL sfnedr2d (tmp3d, tmp3d_2, xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1, &
                            i,j,k, idel, kdel, k1, nlags, edr (i,j,k))

             CALL sfnct2d  (tmp3d_3, xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1, &
                            i, j, k, idel, kdel, k1, nlags, ct_1 (i,j,k))

           ENDDO
           ENDDO
         ENDDO

         IF (kd  <=  xb % kts ) THEN
             edrm = interp2d (id, did, jd, djd, edr (:,:,xb%kts), &
                              xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
         ELSE
             edrm = interp3d (id, did, jd, djd, kd, dkd, edr, &
                              xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)
         ENDIF

! 9.2 Wind Representativness error
!     ----------------------------

         errr_uv = crrlmu * edrm * dx13

! 9.3 Temperature model energy dissipation rate
!     -----------------------------------------

         IF (kc <= xb % kts) THEN
             ctm = interp2d (ic, dic, jc, djc, ct_1 (:,:,xb%kts), &
                              xb%ite - xb%its + 1, xb%jte - xb%jts + 1)
         ELSE
             ctm = interp3d (ic, dic, jc, djc, kc, dkc, ct_1, &
                             xb%ite - xb%its + 1, xb%jte - xb%jts + 1, xb%kte - xb%kts + 1)
         ENDIF

! 9.4 Temperature representativness error
!     -----------------------------------

         errr_t = crrlmt * ctm * dx13


! 9.5 Wind and Temp instrumental error
!     --------------------------------

         IF (error_type == "TURBULENCE" .OR. error_type == "turbulence") THEN

             CALL iserr_wmo_upr (missing_r, pmb, Temp_K=err_t, uv_ms=err_uv)

! 9.6 Add instrumental error to representativness error 
!     -------------------------------------------------

             err_t  = SQRT (errr_t **2 + err_t**2)
             err_uv = SQRT (errr_uv**2 + err_uv**2)

         ENDIF

       ENDIF

! 10.  MAX ERROR QC
! =================

! 10.1 Height QC
!      ---------
      !  If no QC was applied to height, let it know

      IF (current % meas % height % qc <= 0) &
          current % meas % height % qc  = missing

      !  Round to nearest m height derived from first guess

      IF (MOD (current % meas % height % qc/first_guess, 2) /= 0) &
               current % meas % height % data = ANINT &
              (current % meas % height % data)

! 10.2 Pressure QC
!      -----------

      !  Error max QC for pressure, when not derived from first guess

      CALL qc_gaussian (po, pfullm, err_p, sigma_p, &
                        current % meas % pressure % qc)

      !  Round to nearest Pa pressure derived from first guess

      IF (current % meas % pressure % qc >= first_guess) &
          current % meas % pressure % data = ANINT &
         (current % meas % pressure % data)

! 10.3 Temperature QC
!      --------------

      !  Error max QC for temperature

      CALL qc_gaussian (To, Tm, err_t, sigma_t, &
                        current % meas % temperature % qc)

! 10.4 Temperature QC weighting
!      ------------------------

      IF (turbulence_weighting) THEN
          IF (.NOT. current % meas % temperature % qc == missing) &
              WRITE (log_unit,'(A,I6)')   " qct  before = ",current % meas % temperature % qc
              WRITE (log_unit,'(A,F9.3)') " errr_t      = ",errr_t 
              WRITE (log_unit,'(A,F9.3)') " err_t       = ",err_t 
          current % meas % temperature % qc = NINT &
         (current % meas % temperature % qc * EXP (coef_cut*(errr_t/err_t)**2))
               WRITE (log_unit,'(A,I6,/)') " qct  after  = ",current % meas % temperature % qc
      ENDIF

! 10.5 Moisture QC
!      -----------

      IF (cv_options_hum .eq. 1) THEN

      !  Error max QC for specific humidity

      CALL qc_gaussian (qo, qm, err_q, sigma_q, qo_qc)

      ELSE

      !  Error max QC for relative humidity (with respect to water)

!     CALL qc_gaussian (rho, rhm, err_rh, sigma_q, qo_qc)
      CALL qc_piecewise_rh (rho, rhm, err_rh, sigma_q, qo_qc)

      ENDIF

      !  Same QC as specific humidity for dew point and rh

      current % meas % rh        % qc = qo_qc
      current % meas % dew_point % qc = qo_qc

! 10.6 Wind QC
!      -------

      CALL qc_gaussian (uo, um, err_uv, sigma_u, current % meas % u % qc)
      CALL qc_gaussian (vo, vm, err_uv, sigma_u, current % meas % v % qc)

      !  Keep only one QC value for u, v, speed and direction

      qcu = current % meas % u % qc
      qcv = current % meas % v % qc


      IF (qcu == missing .OR. qcv == missing) THEN 
          qcuv = missing
      ELSE
          qcuv = MIN (qcu, qcv)
      ENDIF

! 10.7 Wind QC weighting
!      -----------------

      IF (turbulence_weighting) THEN
          IF (.NOT. qcuv == missing) THEN
               WRITE (log_unit,'(A,I6)')   " qcuv before = ",qcuv
               WRITE (log_unit,'(A,F9.3)') " errr_uv     = ",errr_uv
               WRITE (log_unit,'(A,F9.3)') " err_uv      = ",err_uv
               WRITE (log_unit,'(A,F9.3)') " exp         = ",EXP (coef_cut*(errr_uv/err_uv)**2)
               
              qcuv = NINT (qcuv * EXP (coef_cut*(errr_uv/err_uv)**2))
               WRITE (log_unit,'(A,I6,/)') " qcuv after  = ",qcuv
          ENDIF
      ENDIF

      current % meas % u % qc = qcuv
      current % meas % v % qc = qcuv
      current % meas % speed     % qc = qcuv
      current % meas % direction % qc = qcuv

! 10.8 Thickness QC
!      ------------

      !  No check on thickness, assume always good data
      IF (current % meas % thickness % qc /= missing ) THEN
          current % meas % thickness % qc  = 10 
      ELSE
          current % meas % thickness % qc  = missing
      ENDIF

!----------------------- FILL MODEL INTERPOLATED STRUCTURE --------------------!

! 11.  FILL MODEL INTERPOLATED STRUCTURE
! ======================================
 
! 11.1 Fill model data structure with existing model fields
!     ----------------------------------------------------

      model % meas % pressure    % data = ANINT (pfullm)
      model % meas % height      % data = ANINT (hrefm)
      model % meas % temperature % data = Tm
      model % meas % u           % data = um
      model % meas % v           % data = vm
      model % meas % rh          % data = rhm

! 11.2 Reset model missing fields
!     --------------------------

      model % meas % dew_point   % data = missing_r
      model % meas % speed       % data = missing_r
      model % meas % direction   % data = missing_r
      model % meas % thickness   % data = missing_r

! 11.3 Compute model missing fields
!     ----------------------------

      !  Wind speed and direction

      CALL ffdduv (speedm, dirm, um, vm, mdl (n) % location % longitude, -1)


! 11.4 Fill with computed model fields
!     -------------------------------

      model % meas % dew_point % data = tdm
      model % meas % speed     % data = speedm
      model % meas % direction % data = dirm

! 11.5 Copied obs QC
!     -------------

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


!----------------------- FILL OBSERVATIONAL ERROR STRUCTURE -------------------!

! 12.  FILL OBSERVATIONAL ERROR STRUCTURE
! =======================================
 
! 12.1 Fill observational error data structure with know errors
!      --------------------------------------------------------

      error % meas % pressure    % data = err_p
!     error % meas % pressure    % data = ANINT (pfullm)
      error % meas % height      % data = err_h
!     error % meas % height      % data = ANINT (hrefm)
      error % meas % temperature % data = err_t
      error % meas % u           % data = err_uv
      error % meas % v           % data = err_uv
      error % meas % speed       % data = sqrt(2.) * err_uv

! 12.3 Reset observational error missing fields
!      -----------------------------------------

      error % meas % dew_point   % data = missing_r
      error % meas % thickness   % data = missing_r
      error % meas % rh          % data = missing_r
      error % meas % direction   % data = missing_r

! 12.4 Compute observational error for relative humidity
!      -------------------------------------------------

       !  Saturation water vapor pressure in Pa

       es  = es_alpha * EXP (es_beta*(Tm-T_kelvin) &
                           /(Tm-T_kelvin+es_gamma))

       des_over_es = es_beta*es_gamma * err_t &
                   /(Tm-T_kelvin+es_gamma)**2

       des = des_over_es * es

       !  Water Vapor mixing ratio at saturation in kg/kg

       ws   = rd_over_rv * es /(pfullm-es)

       dws_over_ws = (pfullm*des - es*err_p) / (es*(pfullm-es))

       dws = dws_over_ws * ws

       !  Specific humidity at saturation in kg/kg

       qs  = ws  / (1. + ws)

       dqs = dws / (1. + ws)**2

       !  Relative humidity as function of specific humidity

!      fm = 100. * wm/ws                   ! From Mixing ratio
       fm = 100. * qm/qs * (1.-qs)/(1.-qm) ! From specific humidity

       dfm_over_fm = err_q/(qm*(1.-qm)) - dqs/(qs*(1.-qs))   

       err_rh_lin = dfm_over_fm * fm 

      !  Bound Rh 0-100% and Rh error to 20%

       fm = MIN (100.,fm)
       err_rh_lin = MIN (err_rh_lin, 20.)

! 12.5 Compute observational error for dew point
!      -----------------------------------------
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

! 12.6 Compute observational error for wind speed (neglect grid rotation)
!      ------------------------------------------

       IF (speedm > 0.) THEN
           err_dir = (vm - um) * err_uv
           err_dir = err_dir / (um**2. + vm**2)
           err_dir = ABS (err_dir)
       ELSE
           err_dir = 20.
       ENDIF

       err_dir = MAX (err_dir, 20.)

! 12.4 Fill with computed observational error fields
!     ----------------------------------------------

      error % meas % direction % data = err_dir
      error % meas % dew_point % data = err_td
      error % meas % rh        % data = err_rh

! 12.5 Copied obs QC
!     --------------

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

!------------------------------------------------------------------------------!

! 13. END
! =======

! 13.1 Link model levels
!      -----------------

      CALL link_levels (mdl (n) % surface, model)
      CALL link_levels (err (n) % surface, error)

      ALLOCATE (model)
      NULLIFY  (model % next)

      ALLOCATE (error)
      NULLIFY  (error % next)

! 13.2 Go to next obs level
!      --------------------

      current => current % next

      ENDDO vinterp

! 13.3 Go to next station
!      ------------------

      ENDDO hinterp
  
      DEALLOCATE( psad )
      DEALLOCATE( pp_c )
      DEALLOCATE( tmp2d )
      DEALLOCATE( tmp3d )
      DEALLOCATE( tmp3d_2 )
      DEALLOCATE( tmp3d_3 )
      DEALLOCATE( edr )
      DEALLOCATE( ct_1 )

! 13.4 end
!      ---

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
 END SUBROUTINE qc_max
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
