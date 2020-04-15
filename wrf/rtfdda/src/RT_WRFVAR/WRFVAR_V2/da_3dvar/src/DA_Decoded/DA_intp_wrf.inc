
 SUBROUTINE intp_wrf (max_number_obs, obs, mdl, index, number_obs, &
                      ldebug, grid, &
#include <em_dummy_args.inc> 
)
!------------------------------------------------------------------------------!
! 3-D interpolation of model data at observation locations.
!
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2003 - 2006.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, August 2006.
!------------------------------------------------------------------------------!
  IMPLICIT NONE
  INTRINSIC MAX

! ARGUMENTS

  INTEGER,        INTENT (in)   :: max_number_obs
  INTEGER,        INTENT (in)   :: number_obs
  INTEGER,        INTENT (in),  DIMENSION (max_number_obs) :: index
  TYPE (report),  INTENT (in),  DIMENSION (max_number_obs) :: obs
  TYPE (report),  INTENT (out), DIMENSION (max_number_obs) :: mdl
  LOGICAL,        INTENT (in)   :: ldebug
  TYPE(domain),   INTENT(inout) :: grid

!------------------------------------------------------------------------------!
! STRUCTURE

  TYPE (measurement), POINTER  :: current
  TYPE (measurement), POINTER  :: model
  TYPE (multi_level_type)      :: platform
  TYPE (field)                 :: pressure, height

!------------------------------------------------------------------------------!
! OTHER VARIABLES

  CHARACTER (LEN =  32), PARAMETER :: proc_name = 'intp_wrf'
  CHARACTER (LEN =  80) :: file_ou
  CHARACTER (LEN = 100) :: error_message

  LOGICAL :: inside_halo, outside

  INTEGER :: k, n, nn
  INTEGER :: i0, j0, k0

  REAL    :: h_hig, p_hig 
  REAL    :: h_low, p_low 
  REAL    :: h_sfc, p_sfc 
  REAL    :: h_slp, p_slp 

  REAL    :: aa, bb
  REAL    :: es,  ws,  qs
  REAL    :: invdifftd, invtd

  REAL    :: to, tdo, po, rho, uo, vo, ho
  REAL    :: tm, tdm, pm, rhm, um, vm, hm
  REAL    :: wm, qm
  REAL    :: speedm, dirm

  REAL    :: x0, dx0, dx1
  REAL    :: y0, dy0, dy1
  REAL    :: zh0, zp0, dz0, dz1

  REAL, DIMENSION (1) :: tmpin, tmpout
  REAL, DIMENSION (:), ALLOCATABLE :: v_h, v_p
  REAL, DIMENSION (:), ALLOCATABLE :: v_t, v_q
 
#include <em_dummy_decl.inc> 

!------------------------------------------------------------------------------!

      CALL prelog
      WRITE  (log_unit,'(A)') &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(A)') "Interpolate model data at obs locations:"

! 0.  CHECK VERTICAL COORDINATE
! =============================
   
      IF (v_interp_coord == v_interp_p) THEN 
          CALL prelog
          WRITE  (log_unit,'(/,A)') "Vertical coordinate is pressure."
      ELSE IF (v_interp_coord == v_interp_h) THEN
          CALL prelog
          WRITE  (log_unit,'(/,A)') " Vertical coordinate is height."
      ELSE
          WRITE (error_message,'(A,I1,1X,A)')  & 
         "Error v_interp_coord = " ,v_interp_coord, &
         ", it must be 1 (pressure) or 2 (height)"
          CALL error_handler (log_unit,  v_interp_p, &
               proc_name//error_message, .TRUE., .TRUE.)
      ENDIF

! 2.  MODEL HORIZONTAL INTERPOLATION AT STATIONS LOCATIONS
! ========================================================

! 2.0 Loop over stations
!     ------------------

hinterp:&
      DO nn = 1, number_obs

      n = index (nn)

! 2.1 Discard bad reports
!     -------------------

      IF (obs (n) % info % discard) THEN
          mdl (n) % info % discard = .TRUE.
          CYCLE hinterp
      ENDIF

! 2.2 Fill the 3D-var obs structure for convenience
!     ---------------------------------------------

      platform%info%platform = obs(n)%info%platform

      WRITE (platform%info%date_char, &
           '(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')  &
             obs (n)%valid_time%date_char ( 1: 4), &
             obs (n)%valid_time%date_char ( 5: 6), &
             obs (n)%valid_time%date_char ( 7: 8), &
             obs (n)%valid_time%date_char ( 9:10), &
             obs (n)%valid_time%date_char (11:12), &
             obs (n)%valid_time%date_char (13:14)

       platform%info%name   = obs(n)%location%name
       platform%info%levels = obs(n)%info%num_vld_fld
       platform%info%lat    = obs(n)%location%latitude
       platform%info%lon    = obs(n)%location%longitude
       platform%info%elv    = obs(n)%info%elevation
       platform%info%id     = obs(n)%location%id
       platform%loc%slp%inv = obs(n)%ground%slp%data
       platform%loc%slp%qc  = obs(n)%ground%slp%qc
       platform%loc%pw%inv  = missing_r
       platform%loc%pw%qc   = missing_r

! 2.3 Station coordinates on model grid
!     ----------------------------------

      !  Compute x and y coordinates on unstagerred grid, in this
      !  context x refers to the first index and j to the second
      !  the coordinates are valid at the N-E p point of WRF A grid.
      !  This is cross points in MM5.

       CALL da_ll_to_xy (platform%info, platform%loc,   &
                         xp, outside, inside_halo )

       i0  = platform%loc%i
       j0  = platform%loc%j
       dx0 = platform%loc%dx
       dy0 = platform%loc%dy
       dx1 = platform%loc%dxm
       dy1 = platform%loc%dym

! 2.4 Model pressure and height profile at all levels at station location
!     -------------------------------------------------------------------

      IF (.NOT. ALLOCATED (v_h)) &
      ALLOCATE (v_h  (xp%kts:xp%kte))
      IF (.NOT. ALLOCATED (v_p)) &
      ALLOCATE (v_p  (xp%kts:xp%kte))
      IF (.NOT. ALLOCATED (v_t)) &
      ALLOCATE (v_t  (xp%kts:xp%kte))
      IF (.NOT. ALLOCATED (v_q)) &
      ALLOCATE (v_q  (xp%kts:xp%kte))

      DO k = xp%kts, xp%kte
         v_h (k) = dy1*(dx1*xb%h(i0,j0  ,k) + dx0*xb%h(i0+1,j0  ,k)) &
                 + dy0*(dx1*xb%h(i0,j0+1,k) + dx0*xb%h(i0+1,j0+1,k))
         v_p (k) = dy1*(dx1*xb%p(i0,j0  ,k) + dx0*xb%p(i0+1,j0  ,k)) &
                 + dy0*(dx1*xb%p(i0,j0+1,k) + dx0*xb%p(i0+1,j0+1,k))
         v_t (k) = dy1*(dx1*xb%t(i0,j0  ,k) + dx0*xb%t(i0+1,j0  ,k)) &
                 + dy0*(dx1*xb%t(i0,j0+1,k) + dx0*xb%t(i0+1,j0+1,k))
         ! Beware xb%q is the specific humidity see routine da_transfer_wrftoxb
         v_q (k) = dy1*(dx1*xb%q(i0,j0  ,k) + dx0*xb%q(i0+1,j0  ,k)) &
                 + dy0*(dx1*xb%q(i0,j0+1,k) + dx0*xb%q(i0+1,j0+1,k))
      ENDDO

      ! Compute water vapor mixing ratio from specific humidity
      WHERE (v_q >= 1.)  v_q = 0.9999 
      v_q  = v_q / (1. - v_q)


! 2.5 Model sea level pressure at station location
!     --------------------------------------------

      p_slp  = dy1*(dx1*xb%slp(i0,j0  ) + dx0*xb%slp(i0+1,j0  )) &
             + dy0*(dx1*xb%slp(i0,j0+1) + dx0*xb%slp(i0+1,j0+1))

! 2.6 Model surface pressure at station location
!     -------------------------------------------

      p_sfc  = dy1*(dx1*xb%psfc(i0,j0  ) + dx0*xb%psfc(i0+1,j0  )) &
             + dy0*(dx1*xb%psfc(i0,j0+1) + dx0*xb%psfc(i0+1,j0+1))

! 2.7 Model surface height (terrain) at station location
!     --------------------------------------------------

      h_sfc = dy1*(dx1*xb%terr(i0,j0  ) + dx0*xb%terr(i0+1,j0  )) &
            + dy0*(dx1*xb%terr(i0,j0+1) + dx0*xb%terr(i0+1,j0+1))

! 2.8 Model sea level height (0) at station location
!     ----------------------------------------------

      h_slp = 0.

!  2.5-7 with Interp_lin_2D routine 
!     -----------------------------

!     CALL Interp_lin_2D (xb % slp, xp%ims, xp%ime, xp%jms, xp%jme, &
!                         i0, j0, dx0, dy0, dx1, dy1, p_slp )

!     CALL Interp_lin_2D (xb % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
!                         i0, j0, dx0, dy0, dx1, dy1, p_sfc ) 

!     CALL Interp_lin_2D (xb % ter, xp%ims, xp%ime, xp%jms, xp%jme,&
!                         i0, j0, dx0, dy0, dx1, dy1, h_sfc ) 

! 2.9 Model pressure and height at lowest level
!     -----------------------------------------

      p_low = v_p (xp%kts)
      h_low = v_h (xp%kts)

! 2.10 Model pressure and height at highest level
!      ------------------------------------------

      p_hig = v_p (xp%kte)
      h_hig = v_h (xp%kte)


! 3.  UPPER LEVELS PROCESSING
! ===========================

! 3.1 Initialize obs pointer
!     ----------------------

      current => obs (n) % surface

! 3.2 Initialize model ground information arrays
!     ------------------------------------------

      mdl (n) % location   = obs (n) % location
      mdl (n) % info       = obs (n) % info
      mdl (n) % ground     = obs (n) % ground
      mdl (n) % valid_time = obs (n) % valid_time

      mdl (n) % info   % elevation   = ANINT (h_sfc)
      mdl (n) % ground % slp  % data = ANINT (p_slp)
      mdl (n) % ground % slp  % qc   = 0
      mdl (n) % ground % psfc % data = ANINT (p_sfc)
      mdl (n) % ground % psfc % qc   = 0

! 3.3 Initialize model upper-air information pointers
!     -----------------------------------------------

      ALLOCATE (model)
      NULLIFY  (model % next)
      NULLIFY  (mdl (n) % surface)  

! 3.4 Loop over upper-level
!     ---------------------

vinterp:&
      DO WHILE (ASSOCIATED (current))

! 3.5 At that point, all levels must have pressure and height 
!     -------------------------------------------------------

      IF ((eps_equal (current % meas % height   % data, missing_r, 1.)) .OR. &
          (eps_equal (current % meas % pressure % data, missing_r, 1.))) THEN 
           error_message = "Height and pressure are missing, cannot proceed"
           CALL error_handler (log_unit, ABS (missing), &
                proc_name//error_message, .TRUE., .TRUE.)
           CYCLE vinterp
      ENDIF

! 3.6 Same level observations
!     -----------------------

      ho   = current % meas % height      % data
      po   = current % meas % pressure    % data
      to   = current % meas % temperature % data
      tdo  = current % meas % dew_point   % data
      rho  = current % meas % rh          % data
      uo   = current % meas % u           % data
      vo   = current % meas % v           % data


! 3.7 Observations above model top half level should not be found here
!     ----------------------------------------------------------------

      IF (ho > v_h (xp%kte) .OR. po < v_p (xp%kte)) THEN
          error_message = "Level above the model top, cannot interpolate"
          CALL error_handler (log_unit, -1, &
               proc_name//error_message, .TRUE., .TRUE.)
          CYCLE vinterp
      ENDIF


! 4.  INTERPOLATE HEIGHT AND PRESSURE AT ALL LEVELS
! =================================================

! 4.1 Vertical coordinate is height: Interpolate pressure
!     ---------------------------------------------------

      IF (v_interp_coord == v_interp_h) THEN

          height % data = ho
          height % qc   = 0
          pressure % data = missing_r
          pressure % qc   = missing

          CALL recoverhp (height, pressure, xp, xp%kts, xp%kte, &
                          v_h, v_p, v_t, v_q, &
                          h_sfc, p_sfc, p_slp)


! 4.2 Vertical coordinates is pressure: Interpolate height
!     ---------------------------------------------------
      ELSE IF (v_interp_coord == v_interp_p) THEN

          height % data = missing_r
          height % qc   = missing
          pressure % data = po
          pressure % qc   = 0

          CALL recoverhp (height, pressure, xp, xp%kts, xp%kte, &
                          v_h, v_p, v_t, v_q, &
                          h_sfc, p_sfc, p_slp)

      ENDIF

! 4.3 Load temperature and pressure
!     -----------------------------

      pm = pressure % data
      hm = height   % data


! 5.  ALL SURFACE OBSERVATIONS USES 10m WIND, 2m T and 2m Qs
! ==========================================================
!
      IF (.NOT. obs (n) % info % is_sound) THEN       ! All surface data

! 5.1 10m wind u-component
!     --------------------

          CALL Interp_lin_2D (xb % u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                              i0, j0, dx0, dy0, dx1, dy1, um ) 

! 5.2 10m wind v-component
!     --------------------

          CALL Interp_lin_2D (xb % v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                              i0, j0, dx0, dy0, dx1, dy1, vm ) 

! 5.3 2m Temperature
!     --------------

          CALL Interp_lin_2D (xb % t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                              i0, j0, dx0, dy0, dx1, dy1, tm ) 

! 5.4 2m Specific Humidity
!     --------------------

          CALL Interp_lin_2D (xb % q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                              i0, j0, dx0, dy0, dx1, dy1, qm ) 


! 5.5 Correct surface temperature using a lapse rate
!     ----------------------------------------------

          tm = tm + (h_sfc - ho)*tlapse

      ELSE

! 6.  UPPER AIR REPORT BELOW LOWEST HALF SIGMA LEVEL
! ==================================================
! Do the same as surface obs

          IF (ho < v_h (xp%kts) .OR. po > v_p (xp%kts)) THEN 

! 6.1 10m wind u-component
!     --------------------

              CALL Interp_lin_2D (xb % u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                                  i0, j0, dx0, dy0, dx1, dy1, um ) 

! 6.2 10m wind v-component
!     --------------------

              CALL Interp_lin_2D (xb % v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                                  i0, j0, dx0, dy0, dx1, dy1, vm ) 

! 6.3 2m Temperature
!     --------------

              CALL Interp_lin_2D (xb % t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                                  i0, j0, dx0, dy0, dx1, dy1, tm ) 

! 6.4 2m Specific Humidity
!     --------------------

              CALL Interp_lin_2D (xb % q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                                  i0, j0, dx0, dy0, dx1, dy1, qm ) 


! 6.5 Correct surface temperature using a lapse rate
!     ----------------------------------------------

!             WRITE (*,*) "po   =  ", po   
!             WRITE (*,*) "pm   =  ", pm   
!             WRITE (*,*) "ho   =  ", ho  
!             WRITE (*,*) "hm   =  ", hm
!             WRITE (*,*) "hsfc =  ", h_sfc
!             WRITE (*,*) "tm   =  ", tm

              tm = tm + (h_sfc - ho)*tlapse

!             WRITE (*,*) "Tsfc =  ", tm
!             WRITE (*,*) "tobs =  ", to
!             WRITE (*,*) "tlapse   =  ", tlapse


! 7.  UPPER AIR REPORT BETWEEN LOWEST HALF AND HIGHEST HALF SIGMA LEVELS
! ======================================================================

          ELSE IF (ho > v_h (xp%kts) .AND. ho < v_h (xp%kte) .AND. &
                   po < v_p (xp%kts) .AND. po > v_p (xp%kte)) THEN

! 7.1 Get vertical coordinates based on height
!     ----------------------------------------

              IF (v_interp_coord == v_interp_h) THEN

                  CALL to_zk  (ho, v_h, xp, v_interp_h, zh0)
                  CALL toGrid (zh0, xp%kts, xp%kte, k0, dz0, dz1)

                  hm = ho
                  tmpin (1) = zh0

! 7.2 Get vertical coordinates based on pressure
!     ------------------------------------------

              ELSE IF (v_interp_coord == v_interp_p) THEN

                  CALL to_zk  (po, v_p, xp, v_interp_p, zp0)
                  CALL toGrid (zp0, xp%kts, xp%kte, k0, dz0, dz1)

                  pm = po
                  tmpin (1) = zp0

              ENDIF

! 7.4 wind u-component
!     ----------------

#ifndef DEREF_KLUDGE
              call Interp_lin_3D (xb%u, xp,                          &
                                  i0, j0, dx0, dy0, dx1, dy1,        &
                                  tmpout, 1, tmpin, 1)
#else
              call Interp_lin_3D (xb%u (xp%ims,xp%jms,xp%kms), xp,   & 
                                  i0, j0, dx0, dy0, dx1, dy1,        &
                                  tmpout, 1, tmpin, 1)
#endif
              um = tmpout (1)

! 7.5 wind v-component
!     ----------------

#ifndef DEREF_KLUDGE
              call Interp_lin_3D (xb%v, xp,                         &
                                  i0, j0, dx0, dy0, dx1, dy1,       &
                                  tmpout, 1, tmpin, 1)
#else
              call Interp_lin_3D (xb%v (xp%ims,xp%jms,xp%kms), xp,  &
                                  i0, j0, dx0, dy0, dx1, dy1,       &
                                  tmpout, 1, tmpin, 1)
#endif
              vm = tmpout (1)

! 7.6 Temperature
!     -----------

#ifndef DEREF_KLUDGE
              call Interp_lin_3D (xb%t, xp,                         &
                                  i0, j0, dx0, dy0, dx1, dy1,       &
                                  tmpout, 1, tmpin, 1)
#else
              call Interp_lin_3D (xb%t (xp%ims,xp%jms,xp%kms), xp,  &
                                  i0, j0, dx0, dy0, dx1, dy1,       &
                                  tmpout, 1, tmpin, 1)
#endif
              tm = tmpout (1)

! 7.7 Specific Humidity 
!     -----------------

#ifndef DEREF_KLUDGE
              call Interp_lin_3D (xb%q, xp,                         &
                                  i0, j0, dx0, dy0, dx1, dy1,       &
                                  tmpout, 1, tmpin, 1)
#else
              call Interp_lin_3D (xb%q (xp%ims,xp%jms,xp%kms), xp,  &
                                  i0, j0, dx0, dy0, dx1, dy1,       &
                                  tmpout, 1, tmpin, 1)
#endif
              qm = tmpout (1)

! 7.8 Mixing ratio 
!     ------------

!             wm = qm / (1. - qm)

! 7.9 Relative Humidity
!     -----------------

#ifndef DEREF_KLUDGE
!             call Interp_lin_3D (xb%rh, xp,                        &
!                                 i0, j0, dx0, dy0, dx1, dy1,       &
!                                 tmpout, 1, tmpin, 1)
#else
!             call Interp_lin_3D (xb%rh (xp%ims,xp%jms,xp%kms), xp, &
!                                 i0, j0, dx0, dy0, dx1, dy1,       &
!                                 tmpout, 1, tmpin, 1)
#endif
!             rhm = tmpout (1)

! 7.10 Dew point
!      ---------

#ifndef DEREF_KLUDGE
!             call Interp_lin_3D (xb%td, xp, 
!                                 i0, j0, dx0, dy0, dx1, dy1,       &
!                                 tmpout, 1, tmpin, 1)
#else
!             call Interp_lin_3D (xb%td (xp%ims,xp%jms,xp%kms), xp, & 
!                                 i0, j0, dx0, dy0, dx1, dy1,       &
!                                 tmpout, 1, tmpin, 1)
#endif
!             tdm = tmpout (1)

          ELSE

! 8.  OTHER SITUATIONS ARE NOT ALLOWED
! ====================================

              WRITE (error_message,'(A,F7.2,A)')             & 
             "Unknown vertical location = ",tmpin(1)," pa/m"
              CALL error_handler (log_unit, NINT (tmpin(1)), &
              proc_name//error_message, .TRUE., .TRUE.)
              CYCLE vinterp

          ENDIF

      ENDIF

! 9.  DERIVE OTHER QUANTITIES
! ===========================

! 9.1 Water vapor mixing ratio
!     ------------------------

      wm = qm / (1. - qm)

! 9.2 Relative Humidity
!     -----------------

      !  Saturation water vapor pressure in Pa

      es  = es_alpha * EXP (es_beta*(tm-T_kelvin) &
                                   /(tm-T_kelvin+es_gamma))

      !  Water Vapor mixing ratio at saturation in kg/kg

      ws   = rd_over_rv * es /(pm-es)

      ! Relative humidity

      rhm = 100. * wm/ws

! 9.3 Dew point
!     --------

      invdifftd = LOG (0.01*rhm) / L_over_Rv
      invtd = 1./tm  - invdifftd

      tdm  = 1./ invtd

!----------------------- FILL MODEL INTERPOLATED STRUCTURE --------------------!

! 8.  FILL MODEL INTERPOLATED STRUCTURE
! ======================================
 
! 8.1 Fill model data structure with existing model fields
!     ----------------------------------------------------

      model % meas % pressure    % data = ANINT (pm)
      model % meas % height      % data = ANINT (hm)
      model % meas % temperature % data = tm
      model % meas % u           % data = um
      model % meas % v           % data = vm

! 8.2 Reset model missing fields
!     --------------------------

      model % meas % dew_point   % data = missing_r
      model % meas % speed       % data = missing_r
      model % meas % direction   % data = missing_r
      model % meas % thickness   % data = missing_r
      model % meas % rh          % data = missing_r

! 8.3 Compute model missing fields
!     ----------------------------

      !  Wind speed and direction

      CALL ffdduv (speedm, dirm, um, vm, mdl (n) % location % longitude, -1)


! 8.4 Fill with computed model fields
!     -------------------------------

      model % meas % rh        % data = rhm
      model % meas % dew_point % data = tdm
      model % meas % speed     % data = speedm
      model % meas % direction % data = dirm

! 8.5 Copied obs QC to model structure
!     --------------------------------

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

!------------------------------------------------------------------------------!

! 9 . END
! =======

! 9.1 Link model levels
!     -----------------

      CALL link_levels (mdl (n) % surface, model)

      ALLOCATE (model)
      NULLIFY  (model % next)

! 9.2 Go to next obs level
!     --------------------

      current => current % next

      ENDDO vinterp

! 9.3 Free memory
!     -----------

      DEALLOCATE( v_p )
      DEALLOCATE( v_h )
      DEALLOCATE( v_t )
      DEALLOCATE( v_q )

! 9.4 Go to next station
!     ------------------

      ENDDO hinterp

! 9.5 end
!     ---

      RETURN

!------------------------------------------------------------------------------!
 END SUBROUTINE intp_wrf
