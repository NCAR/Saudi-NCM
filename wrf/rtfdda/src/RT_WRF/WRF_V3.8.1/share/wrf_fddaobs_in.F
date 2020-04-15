!WRF:MEDIATION_LAYER:IO
!  ---

! This obs-nudging FDDA module (RTFDDA) is developed by the 
! NCAR/RAL/NSAP (National Security Application Programs), under the 
! sponsorship of ATEC (Army Test and Evaluation Commands). ATEC is 
! acknowledged for releasing this capability for WRF community 
! research applications.
!
! The NCAR/RAL RTFDDA module was adapted, and significantly modified 
! from the obs-nudging module in the standard MM5V3.1 which was originally 
! developed by PSU (Stauffer and Seaman, 1994). 
! 
! Yubao Liu (NCAR/RAL): lead developer of the RTFDDA module 
! Al Bourgeois (NCAR/RAL): lead engineer implementing RTFDDA into WRF-ARW
! Nov. 2006
! 
! References:
!   
!   Liu, Y., A. Bourgeois, T. Warner, S. Swerdlin and J. Hacker, 2005: An
!     implementation of obs-nudging-based FDDA into WRF for supporting 
!     ATEC test operations. 2005 WRF user workshop. Paper 10.7.
!
!   Liu, Y., A. Bourgeois, T. Warner, S. Swerdlin and W. Yu, 2006: An update 
!     on "obs-nudging"-based FDDA for WRF-ARW: Verification using OSSE 
!     and performance of real-time forecasts. 2006 WRF user workshop. Paper 4.7. 

!   
!   Stauffer, D.R., and N.L. Seaman, 1994: Multi-scale four-dimensional data 
!     assimilation. J. Appl. Meteor., 33, 416-434.
!
!   http://www.rap.ucar.edu/projects/armyrange/references.html
!

  SUBROUTINE wrf_fddaobs_in (grid ,config_flags)

    USE module_domain
    USE module_configure
    USE module_model_constants        !rovg

    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN)    :: config_flags
#if ( EM_CORE == 1 )

! Local variables
    integer            :: ktau            ! timestep index corresponding to xtime
    integer            :: krest           ! restart timestep
    integer            :: inest           ! nest level
    integer            :: infreq          ! input frequency
    integer            :: nstlev          ! nest level
    integer            :: prev_ratio      ! integer ratio of ktau_prev over infreq
    integer            :: curr_ratio      ! integer ratio of ktau over infreq
    real               :: dtmin           ! dt in minutes
    real               :: xtime           ! forecast time in minutes
    logical            :: iprt_in4dob     ! print flag
    integer            :: Nwe             ! size of obs netw grid, west-east dim
    integer            :: Nsn             ! size of obs netw grid, south-north dim
    integer            :: Nz              ! size of obs netw grid, vertical dim

    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe
    INTEGER ij, its, ite, jts, jte

!!! wuyh added 20130816
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor  !externals
  Integer::n
!!! wuyh added 20130816

!   Modified to also call in4dob intially, since subr in4dob is no
!   longer called from subr fddaobs_init. Note that itimestep is now
!   the model step BEFORE the model integration step, because this
!   routine is now called by med_before_solve_io.

    ktau   = grid%itimestep               ! ktau corresponds to xtime
    krest  = grid%fdob%ktaur
    inest  = grid%grid_id
    nstlev = grid%fdob%levidn(inest)
    infreq = grid%obs_ionf*(grid%parent_grid_ratio**nstlev)
    iprt_in4dob = grid%obs_ipf_in4dob

!yliu 20090727
    !xtime = dtmin*grid%itimestep
    xtime = grid%xtime

! Calculate forecast time.
    dtmin = grid%dt/60.

! ajb 20091109 for adaptive step
! Determine if the obs-nudging calculation is to be done on this timestep.
! NOTE: fdob%calc_step is used by subroutines in4dob and nudob.

     prev_ratio = grid%fdob%ktau_prev/infreq 
     curr_ratio = ktau/infreq 

    if( curr_ratio .ne. prev_ratio) then
      grid%fdob%calc_step = 1         ! set for calc_step when ratio changes
    elseif( ktau .eq. grid%fdob%ktau_prev ) then
      grid%fdob%calc_step = 1         ! set for calc_step if first pass 
    else
      grid%fdob%calc_step = 0
    endif

! Store ktau to use on next timestep
    grid%fdob%ktau_prev = ktau

!   IF( (ktau.GT.krest.AND.MOD(ktau,infreq).EQ.0)                            &
!                                        .OR.(ktau.EQ.krest) ) then
    IF( (ktau.GT.krest.AND.grid%fdob%calc_step.eq.1)                         &
                                         .OR.(ktau.EQ.krest) ) then

      CALL get_ijk_from_grid (  grid ,                                       &
                                ids, ide, jds, jde, kds, kde,                &
                                ims, ime, jms, jme, kms, kme,                &
                                ips, ipe, jps, jpe, kps, kpe    )

! Compute dims of Kalman obs network, from WRF mass-point grid dims
  Nwe = 1 + ( (ide-ids+1) - 2*grid%obs_kalman_lbdy+1) / grid%obs_kalman_lskip
  Nsn = 1 + ( (jde-jds+1) - 2*grid%obs_kalman_lbdy+1) / grid%obs_kalman_lskip
  Nz  = kde-kds

      !$ajb PARALLEL DO   &
      !$ajb PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles
         its = grid%i_start(ij)
         ite = min(grid%i_end(ij),ide-1)
         jts = grid%j_start(ij)
         jte = min(grid%j_end(ij),jde-1)

         CALL in4dob(inest, xtime, ktau, krest, dtmin,                          &
                 grid%julyr, grid%julday, grid%gmt,                             & !obsnypatch
! ajb modified 10202014 for radial wind data
                 grid%obs_nudge_opt,  grid%obs_nudge_wind, grid%obs_nudge_temp, &
                 grid%obs_nudge_mois, grid%obs_nudge_pstr, grid%obs_nudge_vr,   &
                 grid%obs_coef_wind,  grid%obs_coef_temp,  grid%obs_coef_mois,  &
                 grid%obs_coef_pstr,  grid%obs_coef_vr,                         & 
! ajb 10202014 end
                 grid%obs_rinxy,      grid%obs_rinsig,     grid%fdob%window,    &
                 grid%obs_npfi,       grid%obs_ionf,                            &
                 grid%obs_prt_max,    grid%obs_prt_freq,                        &
                 grid%obs_idynin,                                               &
                 grid%obs_dtramp,     grid%fdob, grid%fdob%errf,                &
! ajb added 20150316 for memory reduction
                 grid%fdob%vobs, grid%fdob%vobs_qc,                             &
! FCV additions start
                 grid%fdob%platform_ob, grid%qc_internal,                       &
! FCV additions end
! ajb 05212013 moved height adjustment calculation from errob to in4dob
                 grid%obs_adjust_ht,  grid%ht, grid%domdesc,                    &
! ajb 05212013 end
! ajb 11112014 start
                 grid%fdob%ob_var_type, grid%fdob%ob_ndg_type,                  &
! ajb 11112014 end
                 grid%fdob%timeob,    grid%fdob%nlevs_ob,  grid%fdob%lev_in_ob, &
                 grid%fdob%plfo,      grid%fdob%elevob,    grid%fdob%rio,       &
                 grid%fdob%rjo,       grid%fdob%rko,       grid%fdob%rkmob,     &
                 grid%obs_qc_domain,  grid%obs_qc_alerts,                       & 
                 grid%xlat, grid%xlong,                                         &
                 model_config_rec%cen_lat,                                      &
                 model_config_rec%cen_lon,                                      &
                 config_flags%stand_lon,                                        &
                 config_flags%truelat1, config_flags%truelat2,                  &
                 grid%fdob%known_lat, grid%fdob%known_lon,                      &
                 config_flags%dx, config_flags%dy, rovg, t0,                    &
                 grid%fdob%obsprt,                                              &
                 grid%fdob%latprt, grid%fdob%lonprt,                            &
                 grid%fdob%mlatprt, grid%fdob%mlonprt,                          &
                 grid%fdob%stnidprt,                                            &
                 grid%fdob%ob_is_kalman,grid%fdob%gainname,                     &
! wuyh removed 20120823
!                 grid%fdob%kalman2obs,                                         &
! wuyh removed 20120823 end
!! wuyh added 20121206
                 grid%fdob%NKALR,grid%fdob%KLAST,                               &
!! wuyh added 20121206
                 ids,ide, jds,jde, kds,kde,                                     &
                 ims,ime, jms,jme, kms,kme,                                     &
                 ips, ipe, jps, jpe, kps, kpe,                                  &
                 its,ite, jts,jte,                                              &
                 config_flags%map_proj,                                         &
                 model_config_rec%parent_grid_ratio,                            &
                 model_config_rec%i_parent_start(inest),                        &
                 model_config_rec%j_parent_start(inest),                        &
                 model_config_rec%max_dom, grid%fdob%refprt,                    &
                 model_config_rec%obs_vobs_dim,                                 &
                 model_config_rec%nobs_ndg_vars,                                &
                 model_config_rec%obs_errf_dim,                                 &
                 grid%obs_dmax_nobs, grid%obs_dmax_nkalman, iprt_in4dob,        &
! wuyh added 20110630
                 grid%fdob%flagsnd, grid%fdob%flagbog, grid%fdob%desobs,        &
                 grid%fdob%oblat, grid%fdob%oblon, grid%fdob%stnid,             &
                 grid%fdob%qcu,                                                 &
!!!!grid%fdob%qcv,grid%fdob%qct,grid%fdob%qcq,       &
                 grid%fdob%wudate,grid%fdob%wusource,                           &
! wuyh added 20110630 end 
! ajb added 20150226
                 grid%fdob%NUM_NDG_TERMS, grid%fdob%NDG_TERM_INDXS              &
! ajb added 20150226 end
                 )     
! wuyh added 20120827
        IF(grid%fdob%nstat .gt. grid%fdob%nlast ) then
! wuyh added 20120827 end
         CALL in_kgain(inest, xtime, ktau, krest, Nwe, Nsn, Nz,                 &
                 grid%obs_dmax_nobs, model_config_rec%obs_vobs_dim,             &
                 model_config_rec%nobs_ndg_vars,                                &
                 grid%obs_dmax_nkalman, grid%obs_read_kalman,                   &
                 iprt_in4dob, grid%fdob%nlast,                                  &
                 grid%obs_ndirpow,                                              &
                 grid%obs_kalman_lbdy, grid%obs_kalman_lskip,                   &
                 grid%obs_REKF_files,                                           &
                 grid%fdob%window, grid%fdob, grid%fdob%timeob,                 &
                 grid%fdob%vobs, grid%ht,                                       &
                 grid%fdob%nlevs_ob, grid%fdob%lev_in_ob,                       &
                 grid%fdob%plfo, grid%fdob%elevob,                              &
                 grid%fdob%rio, grid%fdob%rjo, grid%fdob%rko,                   &
                 grid%fdob%nstat, grid%fdob%nstaw,                              &
                 grid%fdob%ob_is_kalman, grid%fdob%gain_at_ob,                  &
                 grid%fdob%oblat, grid%fdob%oblon,                              &
! wuyh removed 20120823 end
                 grid%fdob%obs2kalman,  grid%fdob%gainname,                     &
!! wuyh added 20121206
                 grid%fdob%NKALR,grid%fdob%KLAST,                               &
!! wuyh added 20121206 end
!! wuyh added 20160128
                 grid%fdob%wusource,                                            &
!! wuyh added 20160128 end
                 model_config_rec%obs_nudge_terms(inest),                       &
                 grid%fdob%NUM_NDG_TERMS, grid%fdob%NDG_TERM_INDXS,             &
                 ims,ime, jms,jme, its,ite, jts,jte,                            &
                 ids,ide, jds,jde, kds,kde)
! wuyh added 20120827
        ENDIF
! wuyh added 20120827 end
       ENDDO
      !$ajb END PARALLEL DO

!ajb: suppress this output
!    IF ( wrf_dm_on_monitor()  .and. inest .eq. 3) THEN
!      do n=1,grid%fdob%nstat
!!        do i = 1, 14
!!              wudate = CHAR(fdob%wudate(n))
!!        enddo
!       write(640+inest,1610)n,grid%fdob%timeob(n),grid%fdob%oblat(n),grid%fdob%oblon(n) &
!                             ,grid%fdob%vobs(1,n),grid%fdob%vobs_qc(1,n) &
!      enddo
! 1610 Format(I6,1x,F8.2,1x,2(F7.2,1x),6(F12.4,1x,F10.2))
!    Endif
!    Call flush(640+inest)

! Store obs qc from parent domain if requested
      if ( grid%qc_internal) then
         CALL get_parent_QC(grid, grid%obs_prt_max, grid%obs_prt_freq, grid%fdob,    &
                            grid%fdob%qcdomprt, grid%obs_qc_domain,                  &
                            grid%obs_qc_alerts, inest) 
      endif
    ENDIF
    RETURN
#endif
  END SUBROUTINE wrf_fddaobs_in

#if ( EM_CORE == 1 )
!------------------------------------------------------------------------------
! Begin subroutine in4dob and its subroutines
!------------------------------------------------------------------------------
  SUBROUTINE in4dob(inest, xtime, ktau, ktaur, dtmin,                    &
                    myear, julday, gmt,                                  &     !obsnypatch
                    nudge_opt, iswind, istemp,                           &
                    ismois, ispstr, isvr, giv,                           &
                    git, giq, gip, givr,                                 &
                    rinxy, rinsig, twindo,                               &
                    npfi, ionf, prt_max, prt_freq, idynin,               &
                    dtramp, fdob, errf,                                  &
                    vobs, vobs_qc,                                       &
                    platform_ob, qc_internal_ob,                         &
                    adjust_ht, ht, domdesc,                              &
                    ob_var_type, ob_ndg_type,                            &
                    timeob, nlevs_ob, lev_in_ob,                         &
                    plfo, elevob, rio, rjo, rko, rkmob,                  &
                    qc_domain, qc_alerts,                                &
                    xlat, xlong,                                         &
                    cen_lat,                                             &
                    cen_lon,                                             &
                    stand_lon,                                           &
                    true_lat1, true_lat2,                                &
                    known_lat, known_lon,                                &
                    dxm, dym, rovg, t0,                                  &
                    obs_prt,                                             &
                    lat_prt, lon_prt,                                    &
                    mlat_prt, mlon_prt,                                  &
                    stnid_prt,                                           &
                    ob_is_kalman,filesave,                               &
                    nkalr,Klast,                                         &
                    s_we,e_we, s_sn,e_sn, kds,kde,                       &
                    ims,ime, jms,jme, kms,kme,                           &
                    ips,ipe, jps,jpe, kps,kpe,                           &
                    its,ite, jts,jte,                                    &
                    map_proj,                                            &
                    parent_grid_ratio,                                   &
                    i_parent_start,                                      &
                    j_parent_start,                                      &
                    maxdom, refprt,                                      &
                    vdim, nndgv, nerrf, niobf, kalmax, iprt,             &
                    flagsnd, flagbog, desobs, oblat, oblon, stnid,       &
                    qcu,                                                 &
!!qcv, qct, qcq,
                    wudate, wusource,                                    &
! ajb added 20150226
                    num_ndg_terms, ndg_term_indxs                        &
! ajb added 20150226 end
)

  USE module_domain
  USE module_model_constants, ONLY : rcp
  USE module_date_time , ONLY : geth_idts
! FCV Additions starts
  USE module_observ_error_ncep, ONLY : observ_error_ncep_in
! FCV Additions end
  USE module_llxy
  USE module_fddaobs_utilities

  IMPLICIT NONE

! THIS SUBROUTINE READS AN OBSERVATION DATA FILE AND
! SELECTS ONLY THOSE VALUES OBSERVED AT TIMES THAT FALL
! WITHIN A TIME WINDOW (TWINDO) CENTERED ABOUT THE CURRENT
! FORECAST TIME (XTIME).  THE INCOMING OBS FILES MUST BE
! IN CHRONOLOGICAL ORDER.
!
! NOTE: This routine was originally designed for MM5, which uses
!       a nonstandard (I,J) coordinate system. For WRF, I is the 
!       east-west running coordinate, and J is the south-north
!       running coordinate. So "J-slab" here is west-east in
!       extent, not south-north as for MM5. RIO and RJO have
!       the opposite orientation here as for MM5. -ajb 06/10/2004

!      - See MODULE module_fddaobs_utilities.F for VOBS and
!        VOBS_QC variable storage order.

  INTEGER, intent(in) :: niobf          ! maximum number of observations
  INTEGER, intent(in) :: vdim           ! dimension for vobs, vobs_qc
  INTEGER, intent(in) :: nndgv          ! number of nudge variables
  INTEGER, intent(in) :: nerrf          ! number of error fields
  INTEGER, intent(in) :: kalmax         ! max number of kalman type obs
  INTEGER, intent(in)  :: INEST         ! nest level
  REAL, intent(in)     :: xtime         ! model time in minutes
  INTEGER, intent(in)  :: KTAU          ! current timestep
  INTEGER, intent(in)  :: KTAUR         ! restart timestep
  REAL, intent(in)     :: dtmin         ! dt in minutes
  INTEGER, intent(in)  :: myear         ! model year
  INTEGER, intent(in)  :: julday        ! Julian day
  REAL, intent(in)     :: gmt           ! Model GMT at start of run
  INTEGER, intent(in)  :: nudge_opt     ! obs-nudge flag for this nest
  INTEGER, intent(in)  :: iswind        ! nudge flag for wind
  INTEGER, intent(in)  :: istemp        ! nudge flag for temperature
  INTEGER, intent(in)  :: ismois        ! nudge flag for moisture
  INTEGER, intent(in)  :: ispstr        ! nudge flag for pressure
  INTEGER, intent(in)  :: isvr          ! nudge flag for radial wind
  REAL, intent(in)     :: giv           ! coefficient for wind
  REAL, intent(in)     :: git           ! coefficient for temperature
  REAL, intent(in)     :: giq           ! coefficient for moisture
  REAL, intent(in)     :: gip           ! coefficient for pressure
  REAL, intent(in)     :: givr          ! coefficient for radial wind
  REAL, intent(in)     :: rinxy         ! horizontal radius of influence (km)
  REAL, intent(in)     :: rinsig        ! vertical radius of influence (on sigma)
  REAL, intent(inout)  :: twindo        ! (time window)/2 (min) for nudging
  INTEGER, intent(in)  :: npfi          ! coarse-grid time-step frequency for diagnostics
  INTEGER, intent(in)  :: ionf          ! coarse-grid time-step frequency for obs-nudging calcs
  INTEGER, intent(in)  :: prt_max       ! max number of entries of obs for diagnostic printout
  INTEGER, intent(in)  :: prt_freq      ! frequency (in obs index) for diagnostic printout.
  INTEGER, intent(in)  :: idynin        ! for dynamic initialization using a ramp-down function
  REAL, intent(in)     :: dtramp        ! time period in minutes for ramping
  TYPE(fdob_type), intent(inout)  :: fdob     ! derived data type for obs data
  REAL, intent(inout) :: errf(nerrf, niobf)   ! model errors
  REAL, intent(inout) :: vobs(vdim, niobf)    ! observational variables, 1-ob, 1-var
  REAL, intent(inout) :: vobs_qc(vdim, niobf) ! obs var quality control, 1-ob, 1-var
! ajb 11112014 start
  INTEGER, intent(inout) :: ob_var_type(niobf)  ! Variable type of obs (U=1, V=2, etc)
  INTEGER, intent(inout) :: ob_ndg_type(niobf)  ! Nudge-term type (# term in ndg_term_indxs)
! ajb 11112014 end
  REAL, intent(inout) :: timeob(niobf)        ! model times for each observation (hours)
! FCV additions start
  INTEGER, intent(inout) :: platform_ob(40,niobf) ! Platform type for each observation, ideally a string
  LOGICAL, intent(in)    :: qc_internal_ob        ! Switch to turn on internal obs QC on the fly
! FCV additions end
! ajb 05212013 start
 LOGICAL, intent(inout) :: adjust_ht            ! Switch for near-surface height adjustment
  REAL, intent(in)    :: ht(ims:ime, jms:jme) ! terrain height (m)
  INTEGER, intent(in) :: domdesc              ! external communication descriptor
! ajb 05212013 end
  REAL, intent(inout) :: nlevs_ob(niobf)      ! numbers of levels in sounding obs
  REAL, intent(inout) :: lev_in_ob(niobf)     ! level in sounding-type obs
  REAL, intent(inout) :: plfo(niobf)          ! index for type of obs-platform
  REAL, intent(inout) :: elevob(niobf)        ! elevations of observations  (meters)
  REAL, intent(inout) :: rio(niobf)           ! west-east coord of ob, on non-staggered grid
  REAL, intent(inout) :: rjo(niobf)           ! south-north coord of ob, on non-staggered grid
  REAL, intent(inout) :: rko(niobf)           ! vertical grid coordinate
  REAL, intent(inout) :: rkmob(niobf)         ! observation density indicator
  INTEGER, intent(in) :: qc_domain            ! domain to get qc values from
  INTEGER, intent(in) :: qc_alerts            ! flag to activate/suppress QC alert messages
  REAL, DIMENSION( ims:ime, jms:jme ),                            &
        INTENT(IN )       :: xlat, xlong      ! lat/lon on mass-pt grid (for diagnostics only)
  REAL, intent(in) :: cen_lat                 ! center latitude for map projection
  REAL, intent(in) :: cen_lon                 ! center longiture for map projection
  REAL, intent(in) :: stand_lon               ! standard longitude for map projection
  REAL, intent(in) :: true_lat1               ! truelat1 for Lambert map projection
  REAL, intent(in) :: true_lat2               ! truelat2 for Lambert map projection
  REAL, intent(in) :: known_lat               ! latitude  of domain origin point (i,j)=(1,1)
  REAL, intent(in) :: known_lon               ! longigude of domain origin point (i,j)=(1,1)
  REAL, intent(in) :: dxm                     ! grid size in x (meters)
  REAL, intent(in) :: dym                     ! grid size in y (meters)
  REAL, intent(in) :: rovg                    ! constant rho over g
  REAL, intent(in) :: t0                      ! background temperature
  INTEGER, intent(inout) :: obs_prt(prt_max)  ! For printout of obs index
  REAL, intent(inout) :: lat_prt(prt_max)     ! For printout of obs latitude 
  REAL, intent(inout) :: lon_prt(prt_max)     ! For printout of obs longitude
  REAL, intent(inout) :: mlat_prt(prt_max)    ! For printout of model lat at obs (ri,rj)
  REAL, intent(inout) :: mlon_prt(prt_max)    ! For printout of model lon at obs (ri,rj)
  INTEGER, intent(inout) :: stnid_prt(40,prt_max) ! For printout of obs station id
  INTEGER, intent(inout) :: ob_is_kalman(niobf) ! flags for obs that require kalman gain input
  INTEGER, intent(inout) :: filesave(niobf)   ! gain obs info 
! wuyh removed 20120823
!  INTEGER, intent(in) :: kalman2obs(kalmax)  ! mapping from kalman index to obs index
! wuyh removed 20120823 end

!! wuyh added 20121206 
  INTEGER, intent(out) :: nkalr               ! # of old kalman obs to be removed at current window
  INTEGER, intent(out) :: Klast               ! # of remain kalman obs before read in new obs
!! wuyh added 20121206

! wuyh added 20110630
  logical,intent(inout):: flagsnd(niobf), flagbog(niobf)
  Integer,intent(inout):: desobs(40,niobf)
  Integer, intent(inout) :: stnid(40,niobf)   ! Obs station id
  Real   ,intent(inout):: qcu(niobf)  !!,qcv(niobf), &     ! obs qc values for u, v, t, and q
                                       !! qct(niobf),qcq(niobf)
  Integer, intent(inout) :: wudate(14,niobf)           ! obs date
  Integer, intent(inout) :: wusource(40,niobf)         ! obs source
  Integer, intent(in)    :: num_ndg_terms       ! number of nudging terms
  Integer, intent(in)    :: ndg_term_indxs(MNP,2) ! indices for nudging terms
  Real   , intent(inout) :: oblat(niobf)      ! obs latitude
  Real   , intent(inout) :: oblon(niobf)      ! obs longitude
! wuyh added 20110630 end

! ajb modified 05212013
  INTEGER, intent(in)  :: s_we,e_we, s_sn,e_sn, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte           ! tile   dims.
! ajb end 05212013

  INTEGER, intent(in) :: map_proj             ! map projection index
  INTEGER, intent(in) :: parent_grid_ratio    ! parent to nest grid ration
  INTEGER, intent(in) :: i_parent_start       ! starting i coordinate in parent domain
  INTEGER, intent(in) :: j_parent_start       ! starting j coordinate in parent domain
  INTEGER, intent(in) :: maxdom               ! maximum number of domains
  INTEGER, intent(inout) :: refprt(maxdom)    ! reference obs index for printout
  LOGICAL, intent(in) :: iprt                 ! print flag

!***  DECLARATIONS FOR IMPLICIT NONE                                    
  integer :: n, nn, ndum, nopen, nvol, idate, imm, iss
  integer :: nsta                       ! number of stations held in timeobs array
  integer :: nstaw                      ! # stations within the actual time window
  integer :: nprev                      ! previous n in obs read loop (for printout only)
  integer :: meas_count, imc, njend, njc, njcc, julob, kn
  real    :: hourob, rjulob
  real    :: xhour, tback, tforwd, rjdate1, timanl1, rtimob
  real    :: ri, rj                     ! x,y coords of observation on non-staggered grid
  real    :: rri, rrj                   ! x,y coords of radar location non-staggered grid
  real    :: elevation, pressure_data
  real    :: orig_pressure_qc,orig_pressure_data !ajb 03312010 for ob in ht implementation
  real    :: pressure_qc, height_data, height_qc, temperature_data
  real    :: temperature_qc, u_met_data, u_met_qc, v_met_data
  real    :: orig_u_met_qc, orig_v_met_qc        !ajb 10052011 for internal QC
  real    :: v_met_qc, rh_data, rh_qc, r_data, slp_data, slp_qc
  real    :: ref_pres_data, ref_pres_qc, psfc_data, psfc_qc
  real    :: precip_data, precip_qc, tbar, twdop, r_qc
  real*8  :: tempob
  INTEGER, EXTERNAL :: nvals_le_limit         ! for finding #obs with timeobs <= tforwd
  integer :: nlast                      ! last ob in list of valid obs from prev window

! Local variables
  TYPE (PROJ_INFO)   :: obs_proj        ! Structure for obs projection information.
  character*14 date_char
  character*19 obs_date                                                        !obsnypatch
  integer idts                                                                 !obsnypatch
  character*40 platform,source,id,namef
  character*3 fonc ! 20081212 FCV from MM5's AJM 20071220 changes
  character(len=200) :: msg       ! Argument to wrf_message
  real latitude,longitude
  real radar_lat,radar_lon
  real :: plat_prt=0.0        ! Previous obs latitude in read loop (used for printout)
  real :: plon_prt=0.0        ! Previous obs longitude in read loop (used for printout)
  logical :: newpass          ! New pass flag (used for printout)
  logical is_sound,bogus
  LOGICAL OPENED,exist
  integer :: ieof(5),ifon(5)
  data ieof/0,0,0,0,0/
  data ifon/0,0,0,0,0/
  integer :: msgcnt           ! For limiting "unknown ob" messages that are printed 
  integer :: nmove, nvola
  integer :: iyear, itimob                                                     !obsnypatch
  external :: fcst_hours                                                       !obsnypatch
  real :: fcst_hours                                                           !obsnypatch
! FCV Additions start
  INTEGER :: i, unit_err
  CHARACTER*80 :: file_err
  CHARACTER*80 :: error_type
  INTEGER :: ierr
! FCV Additions end
  DATA NMOVE/0/,NVOLA/61/

! wuyh added 20110630
  Character*40 :: myid,mydes
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor  !externals
! wuyh added 20110630 end

! wuyh added 20130114
  Integer,Dimension(40)::empty

! ajb added for efficiency of ob height adjustment
  logical :: same_sndg   ! false when new sounding, true throughout sounding
  logical :: near_surf   ! true if ob needs height adjustment
  real    :: htij(s_we:e_we,s_sn:e_sn)  ! terrain ht on global grid

!!! wuyh added 20131004
  Real,Parameter    :: terrdif = 800.     ! Max allowed difference between model and ob terrain
  Real              :: terrmod, ob_hto
 !!! wuyh added 20131004

! ajb added 20150316 for memory reduction
  real :: ob_vals(nndgv,numsnd), ob_qcvals(nndgv,numsnd)

! mx added 20170427 for reading obs density/sparseness parameter
  integer :: error  ! true if alphabet letters are found in the input for rkmob 

!!! ajb added 20131113 for height adjustment of soundings
  real, dimension(numsnd) :: sndg_pressure_data, sndg_pressure_qc,           &
                             sndg_height_data, sndg_height_qc,               &
                              sndg_temperature_data, sndg_temperature_qc,     &
                             sndg_u_met_data, sndg_u_met_qc,                 &
                             sndg_v_met_data, sndg_v_met_qc,                 &
                             sndg_rh_data, sndg_rh_qc,                       &
                             sndg_orig_pressure_data, sndg_orig_pressure_qc, &
                             sndg_orig_u_met_qc, sndg_orig_v_met_qc

  integer :: ivar   ! Loop counter over variable types

  same_sndg=.false.  ! false when new sounding, true throughout sounding
  near_surf=.false.   ! true if ob needs height adjustment

  empty = ICHAR(' ')

! ajb added 05212013 - Collect terrain array HT into a global array HTIJ
!!! wuyh modified 20131017
!!!!no matter height adjustment we need htij since 
!!!if abs(terrmod-elevation) > terrdif remove the obs
!!!if(adjust_ht) then
  htij = float(0)                   ! Zero out the terrain array.
  CALL loc2glob(HT, HTIJ, "2D", "REAL", domdesc,                 &
                s_we,e_we, s_sn,e_sn, kds,kde,                   &
                ims,ime, jms,jme, kms,kme,                       &
                ips,ipe, jps,jpe, kps,kpe )
!!!endif
!!! wuyh modified 20131017 end

! ajb 05212013 end

  if(ieof(inest).eq.2.and.fdob%nstat.eq.0) then
    IF (iprt) print *,'returning from in4dob'
    return
  endif
  IF (iprt) print *,'start in4dob ',inest,xtime
  IF(nudge_opt.NE.1)RETURN

! Initialize obs for printout
  obs_prt = -999
  newpass = .true.
  msgcnt = 0

! if start time, or restart time, set obs array to missing value
  IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR) THEN
    TIMEOB=99999.
    fdob%xtime_at_rest = xtime    !yliu 2008012
  ENDIF
! set number of obs=0 if at start or restart
  IF(KTAU.EQ.KTAUR)fdob%NSTAT=0
  NSTA=fdob%NSTAT
  XHOUR=XTIME/60.
  XHOUR=AMAX1(XHOUR,0.0)

10 CONTINUE

! DEFINE THE MAX LIMITS OF THE WINDOW
  TBACK=XHOUR-TWINDO
  TFORWD=XHOUR+TWINDO

  IF (iprt) then
      write(msg,'(2(a,f8.3),a,i6,a,i2)') 'TBACK = ',tback,' TFORWD = ',tforwd, &
                                        ' nsta = ',nsta,'  for nest = ',inest
      call wrf_message(msg)
      write(msg,'(a,2f12.3)')'in4dob last cycle Obs time from 1 to nsta', &
           timeob(1),timeob(max(1,nsta))
      call wrf_message(msg)
  ENDIF

  IF(NSTA.GT.0) THEN
    NDUM=0
    Nkalr = 0
    t_window : DO N=1,NSTA
      IF((TIMEOB(N)-TBACK).LT.0) THEN
        TIMEOB(N)=99999.
        if(ABS(ob_is_kalman(N)) .eq. 1) then
         Nkalr = Nkalr + 1
        endif
      ENDIF
      IF(TIMEOB(N).LT.9.E4) EXIT t_window
      NDUM=N
    ENDDO t_window

!! wuyh added 20120831
!!    nkalr =NDUM
!! wuyh added 20120831 end

!!! Wuyh removed
!!!    nkalr = 0
!!!    kalman : do n = 1, kalmax
!      if(abs(kalman2obs(n)).gt.ndum .or. kalman2obs(n).eq.0) EXIT kalman
!!!     nkalr=nkalr+1
!!!   enddo kalman
!!! wuyh removed

! REMOVE OLD OBS DENOTED BY 99999. AT THE FRONT OF TIMEOB ARRAY
    IF (iprt) print *,'ndum at 20=',ndum
    NDUM=ABS(NDUM)

!!! wuyh modified 20121018
 !!!   NMOVE=NIOBF-NDUM
    NMOVE=NSTA-NDUM
!!! wuyh modified 20121018 end

    IF(NMOVE.GT.0 .AND. NDUM.NE.0 ) THEN
      DO N=1,NMOVE

! wuy added 20110630
      wudate(:,n)     =wudate(:,N+NDUM)
      wusource(:,n)   =wusource(:,N+NDUM)
      platform_ob(:,n)=platform_ob(:,N+NDUM)
      stnid(:,n)      =stnid(:,N+NDUM)
      desobs(:,n)     =desobs(:,N+NDUM)
      flagsnd(n)      =flagsnd(N+NDUM)
      flagbog(n)      =flagbog(N+NDUM)
      oblat(n)        =oblat(N+NDUM)
      oblon(n)        =oblon(N+NDUM)
      qcu(n)          =qcu(N+NDUM)
!!      qcv(n)          =qcv(N+NDUM)
!!      qct(n)          =qct(N+NDUM)
!!      qcq(n)          =qcq(N+NDUM)
! wuy added 20110630 end

        do KN = 1,vdim
          VOBS(KN,N)=VOBS(KN,N+NDUM)
          VOBS_QC(KN,N)=VOBS_QC(KN,N+NDUM)
        enddo
        do KN = 1,nerrf
          ERRF(KN,N)=ERRF(KN,N+NDUM)             ! yliu 20050502
        enddo

! RIO is the west-east coordinate. RJO is south-north. (ajb)
        RJO(N)=RJO(N+NDUM)
        RIO(N)=RIO(N+NDUM)
        RKO(N)=RKO(N+NDUM)
        TIMEOB(N)=TIMEOB(N+NDUM)
        nlevs_ob(n)=nlevs_ob(n+ndum)
        lev_in_ob(n)=lev_in_ob(n+ndum)
        plfo(n)=plfo(n+ndum)
        rkmob(n)=rkmob(n+ndum)
        elevob(n)=elevob(n+ndum)
!ajb For Kalman implementation
        ob_is_kalman(n) = ob_is_kalman(n+ndum)
        filesave(n) = filesave(n+ndum)
!ajb for independent platforms
        ob_var_type(n) = ob_var_type(n+ndum)
        ob_ndg_type(n) = ob_ndg_type(n+ndum)
      ENDDO
    ENDIF
    NOPEN=NMOVE+1
    IF(NOPEN.LE.NSTA) THEN
      DO N=NOPEN,NSTA
        do KN = 1,vdim
          VOBS(KN,N)=99999.
          VOBS_QC(KN,N)=99999.
        enddo
        do KN = 1,nerrf
          ERRF(KN,N)=99999.                      ! yliu 20050502
        enddo
        RIO(N)=99999.
        RJO(N)=99999.
        RKO(N)=99999.
        TIMEOB(N)=99999.
        nlevs_ob(n)=99999.
        lev_in_ob(n)=99999.
        plfo(n)=99999.
        rkmob(n)=99999.
        elevob(n)=99999.
        ob_is_kalman(n) = 0       !ajb 20120821
        filesave(n) = -1000010
      ENDDO
    ENDIF
  ENDIF


! Compute map projection info.
  call set_projection(obs_proj, map_proj, cen_lat, cen_lon,            &
                      true_lat1, true_lat2, stand_lon,                 &
                      known_lat, known_lon,                            &
                      e_we, e_sn, dxm, dym )

! FIND THE LAST OBS IN THE LIST
  NLAST=0
  KLAST=0
  last_ob : DO N=1,NIOBF
!   print *,'nlast,n,timeob(n)=',nlast,n,timeob(n)
    IF(TIMEOB(N).GT.9.E4) EXIT last_ob
    NLAST=N
    if(ABS(ob_is_kalman(N)) .eq. 1) then
       KLAST = KLAST + 1
    endif    
  ENDDO last_ob
  fdob%nlast = nlast    ! Store for use in subroutine nudob 

! print *,'in4dob, after 90 ',nlast,ktau,ktaur,nsta
! open file if at beginning or restart

  IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR) THEN
    fdob%RTLAST=-999.
    INQUIRE (NVOLA+INEST-1,OPENED=OPENED)
    IF (.NOT. OPENED) THEN
      ifon(inest)=1
      write(fonc(1:2),'(i2)')ifon(inest)
      if(fonc(1:1).eq.' ')fonc(1:1)='0'
      INQUIRE (file='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2)  &
              ,EXIST=exist)
      if(exist) then
        IF (iprt) THEN
          print *,'opening first fdda obs file, fonc=',              &
                   fonc(1:2),' inest=',inest
          print *,'ifon=',ifon(inest)
        ENDIF
        OPEN(NVOLA+INEST-1,                                          &
        FILE='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2),        &
              FORM='FORMATTED',STATUS='OLD')
! FCV additions begin
! Load NCEP observational error profiles
        IF (qc_internal_ob) THEN
            file_err   = "OBSERR.TBL"
            unit_err   = 99
              CALL observ_error_ncep_in (file_err, unit_err, iprt, ierr)
        ENDIF
! FCV additions end
      else
! no first file to open
        IF (iprt) print *,'there are no fdda obs files to open'
        return
      endif

    ENDIF
  ENDIF  !end if(KTAU.EQ.0.OR.KTAU.EQ.KTAUR)
! print *,'at jc check1'

!**********************************************************************
!       --------------   BIG 100 LOOP OVER N  --------------
!**********************************************************************
! NOW CHECK TO SEE IF EXTRA DATA MUST BE READ IN FROM THE
! DATA FILE.  CONTINUE READING UNTIL THE REACHING THE EOF
! (DATA TIME IS NEGATIVE) OR FIRST TIME PAST TFORWD. THE
! LAST OBS CURRENTLY AVAILABLE IS IN N=NMOVE.
  N=NLAST

  IF(N.EQ.0)GOTO 110

 1001 continue

! ieof=2 means no more files
! print *,'after 1001,n,timeob(n)=',n,timeob(n)

  IF(IEOF(inest).GT.1) then
    GOTO 130
  endif

100 CONTINUE
!ajb 20070116 bugfix for situation that first obs is not in the domain
  IF(N.ne.0) THEN
    IF(TIMEOB(N).GT.TFORWD.and.timeob(n).lt.99999.) THEN
      GOTO 130
    ENDIF
  ENDIF

!*YLIUV2.2 20070105 start

!*! OBSFILE: no more data in the obsfile
!*    if(ieof(inest).eq.1 )then
!*      ieof(inest)=2
!*      goto 130
!*    endif

!
! open file if at end of previous file, and another file exists
  if(ieof(inest).eq.1) then
    print *,'time to open another file - ifon,inest=',        &
             ifon(inest),inest,ieof(inest)
    if(n.gt.0) print *,'n,timeob,tforwd=',n,timeob(n),tforwd
    ifon(inest)=ifon(inest)+1
!---20081212 if statement added by FCV based on 20071220 A Monaghan' MM5 changes
!---to read more than 100 files
    if(ifon(inest).lt.100)then
    write(fonc(1:2),'(i2)')ifon(inest)
    if(fonc(1:1).eq.' ')fonc(1:1)='0'
    print *,'checking for fdda obs file, fonc=',fonc(1:2),    &
            ' inest=',inest,' ifon=',ifon(inest)
    INQUIRE (file='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2),EXIST=exist)
    if(exist) then
      print *,'opening another fdda obs file, fonc=',fonc(1:2),    &
              ' inest=',inest,' n=',n
      OPEN(NVOLA+INEST-1, FILE='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2), &
           FORM='FORMATTED',STATUS='OLD')
      ieof(inest)=0
    else
! no more files to open
      ieof(inest)=2
      goto 130
    endif
    else  !else if filename >=100
    write(fonc(1:3),'(i3)')ifon(inest)
    print *,'checking for fdda obs file, fonc=',fonc(1:3),    &
            ' inest=',inest,' ifon=',ifon(inest)
    INQUIRE (file='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:3),EXIST=exist)
    if(exist) then
      print *,'opening another fdda obs file, fonc=',fonc(1:3),    &
              ' inest=',inest,' n=',n
      OPEN(NVOLA+INEST-1, FILE='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:3), &
           FORM='FORMATTED',STATUS='OLD')
      ieof(inest)=0
    else
! no more files to open
      ieof(inest)=2
      goto 130
    endif
    endif !20081212 FCV end check of files number >100
  endif

!*YLIUV2.2 end

!**********************************************************************
!       --------------   110 SUBLOOP OVER N  --------------
!**********************************************************************
! THE TIME OF THE MOST RECENTLY ACQUIRED OBS IS .LE. TFORWD,
! SO CONTINUE READING
110 continue

  IF(N.GT.NIOBF-1) CALL stop_122(n,niobf,iprt)
! REPLACE NVOLA WITH LUN 70, AND USE NVOLA AS A FILE COUNTER
    NVOL=NVOLA+INEST-1
    IF(fdob%IEODI.EQ.1)GOTO 111
    read(nvol,101,end=111,err=111)date_char
! yliu 20080202 add the following loop to tolerate bad obs format
    if(date_char(1:4).le."1900".or.date_char(1:4).ge."2050") go to 110
! yliu 20080202 end
101 FORMAT(1x,a14)

    n=n+1

! Convert the form of the observation date for geth_idts.
    call fmt_date(date_char, obs_date)

! Compute the time period in seconds from the model reference
! date (fdob%sdate) until the observation date.

    call geth_idts(obs_date, fdob%sdate(1:19), idts)

! Convert time in seconds to hours.
    ! In case of restart, correct for new sdate.
    idts = idts + nint(fdob%xtime_at_rest*60.)  ! yliu 20080127

    rtimob =float(idts)/3600.
    timeob(n)=rtimob

!   print *,'read in ob ',n,timeob(n),rtimob
    IF(IDYNIN.EQ.1.AND.TIMEOB(N)*60..GT.fdob%DATEND) THEN
      IF (iprt) THEN
        PRINT*,' IN4DOB: FOR INEST = ',INEST,' AT XTIME = ',XTIME,    &
               ' TIMEOB = ',TIMEOB(N)*60.,' AND DATEND = ',           &
                 fdob%DATEND,' :'
        PRINT*,'         END-OF-DATA FLAG SET FOR OBS-NUDGING',       &
               ' DYNAMIC INITIALIZATION'
      ENDIF
      fdob%IEODI=1
      TIMEOB(N)=99999.
      rtimob=timeob(n)
    ENDIF
    read(nvol,102)latitude,longitude
 102  FORMAT(2x,2(f9.4,1x))

    CALL latlon_to_ij(obs_proj, latitude, longitude, ri, rj)

!ajb  Subroutine latlon_to_ij returns ri, rj in the mass-pt coordinate. Here,
!ajb  we reference everything to the non-staggered grid (not mass-pt!), so
!ajb  we add .5 to each ri and rj to put them on the non-staggered grid.

    ri = ri + 0.5      !ajb Adjust from mass-pt to non-staggered grid.
    rj = rj + 0.5      !ajb Adjust from mass-pt to non-staggered grid.

! Store non-staggered grid coordinates for this observation
    rio(n)=ri
    rjo(n)=rj

    read(nvol,1021)id,namef
1021 FORMAT(2x,2(a40,3x))

    read(nvol,103)platform,source,elevation,is_sound,bogus,meas_count
103  FORMAT( 2x,2(a17,1x),f8.0,2x,2(l4,2x),i5)

!     write(6,*) '----- OBS description ----- '
!     write(6,*) 'platform,source,elevation,is_sound,bogus,meas_count:'
!     write(6,*) platform,source,elevation,is_sound,bogus,meas_count

! yliu 
    elevob(n)=elevation
! FCV additions start
    do i = 1, 14
       platform_ob(i,n)=ICHAR(platform (i:i))
    enddo
! MX 20170427
! check platform(15:17) for observation density indicator - used to set radius of influence for nudging
    rkmob(n) = -1.0  
    read(platform(15:17),'(f3.0)',iostat=error) rkmob(n)
!   if(error==0) then
!     write(6,*) 'input pre-calculated obs distance: ',rkmob(n), platform(1:17) 
!   endif

! wuyh modified 20130114
!!    do i=17,40
!!       platform_ob(i,n)=ICHAR(' ')
!!    enddo
    platform_ob(15:40,n) = empty(15:40)
! wuyh modified end 20130114

! jc
! change platform from synop to profiler when needed
    if(namef(2:9).eq.'PROFILER')platform(7:14)='PROFILER'
    if(namef(2:6).eq.'ACARS') platform(7:11)='AMDAR'
! yliu
    if(namef(1:7).eq.'SATWNDS') platform(7:13)='SATWNDS'
    if(namef(1:6).eq.'SATWND')  platform(7:12)='SATWND'
    if(namef(1:8).eq.'CLASS DA')platform(7:10)='TEMP'
    if(platform(7:10).eq.'MWVR') platform(7:10)='TEMP'
! yliu end

    rko(n)=-99.
!yliu 20050706
!     if((platform(7:11).eq.'METAR').or.(platform(7:11).eq.'SPECI').or.
!    1   (platform(7:10).eq.'SHIP').or.(platform(7:11).eq.'SYNOP').or.
!    1    (platform(1:4).eq.'SAMS'))
!    1   rko(n)=1.0
    if(.NOT. is_sound) rko(n)=float(1)
!yliu 20050706 end

! plfo is inFORMATion on what platform. May use this later in adjusting weights
    plfo(n)=99.

!!! wuyh added 20140711
    if(platform(8:12).eq.'RADAR')  plfo(n)=10.
!!! wuyh added 20140711 end

    if(platform(7:11).eq.'METAR')  plfo(n)=1.
    if(platform(7:11).eq.'SPECI')  plfo(n)=2.
    if(platform(7:10).eq.'SHIP')   plfo(n)=3.
    if(platform(7:11).eq.'QSCAT')  plfo(n)=3.
    if(platform(7:11).eq.'SYNOP')  plfo(n)=4.
    if(platform(7:10).eq.'TEMP')   plfo(n)=5.
    if(platform(7:10).eq.'MWVR')   plfo(n)=5.
    if(platform(7:11).eq.'PILOT')  plfo(n)=6.
    if(platform(7:11).eq.'PIBAL')  plfo(n)=6.
    if(platform(7:12).eq.'SATWND') plfo(n)=7.
    if(platform(7:13).eq.'SATWNDS')plfo(n)=7.
! FCV    if(platform(1:7).eq.'SATWNDS')plfo(n)=7.
    if(platform(7:11).eq.'SATWI')  plfo(n)=7.
    if(platform(1:4).eq.'SAMS')    plfo(n)=8.
    if(platform(7:14).eq.'PROFILER')plfo(n)=9.
! yliu: ACARS->SATWINDS
    if(platform(7:12).eq.'TAMDAR')  plfo(n)=7.
! FCV additions start
    if(platform(7:11).eq.'AIREP')  plfo(n)=7.
! FCV additions end
    if(platform(7:11).eq.'AMDAR')  plfo(n)=7.
! Wuyh addded 20130816
    if(platform(7:11).eq.'MDCRS')  plfo(n)=7.
! Wuyh addded 20130816
! yliu: end
    if(plfo(n).eq.99.) then
        msgcnt = msgcnt + 1 
        if(msgcnt.le.100) then
          if (iprt) then
            print *,'n=',n,' unknown ob of type',platform
            if (msgcnt.eq.100) then
              write(6,*) '****** Max of 100 unknown ob msgs printed!'
            endif
          endif
        endif
    endif


!======================================================================
!======================================================================
! THIS PART READS SOUNDING INFO
    IF(is_sound)THEN
      nlevs_ob(n)=real(meas_count)
      lev_in_ob(n)=1.
      read_sounding : DO imc=1,meas_count
!             write(6,*) '0 inest = ',inest,' n = ',n
! the sounding has one header, many levels. This part puts it into 
! "individual" observations. There's no other way for nudob to deal
! with it.
      if(imc.gt.1) then                          ! sub-loop over N
        n=n+1
        if(n.gt.niobf) CALL stop_122(n,niobf,iprt)
        nlevs_ob(n)=real(meas_count)
        lev_in_ob(n)=real(imc)
        timeob(n)=rtimob
        rio(n)=ri
        rjo(n)=rj
        rko(n)=-99.
        if(n > 1) then
          plfo(n)=plfo(n-imc+1)
          rkmob(n)=rkmob(n-imc+1)
        endif
        elevob(n)=elevation
        endif
! FCV additions start
        if(n > 1) platform_ob(:,n) = platform_ob (:,n-imc+1)
        do i = 1, 40
                stnid(i,n)  = ICHAR (id(i:i))
                desobs(i,n) = ICHAR (namef(i:i))
        enddo

! For |isvr|=1, convert radar location lat/lon to grid coordinates.
        if(plfo(n).eq.10 .and. abs(isvr).eq.1) then
          read(id( 1: 9),'(f9.4)') radar_lon
          read(id(11:19),'(f9.4)') radar_lat
          CALL latlon_to_ij(obs_proj, radar_lat, radar_lon, rri, rrj)

          rri = rri + 0.5   ! mass-pt to non-stag grid
          rrj = rrj + 0.5   ! mass-pt to non-stag grid

! Store into stnid in integer form
          stnid(1,n) = nint(100.*rri)
          stnid(2,n) = nint(100.*rrj)
        endif

        do i=1,16
              wusource(i,n) = ICHAR (source(i:i))
        enddo
! wuyh modified 20140114
!!        do i=17,40
!!              wusource(i,n) = ICHAR (' ')
!!        enddo
        wusource(17:40,n) = empty(17:40)
! wuyh modified end 20130114

        do i = 1, 14
              wudate(i,n) = ICHAR(date_char(i:i))
        enddo
!        write(393,*)wudate(:,n),date_char
! FCV additions end

        read(nvol,104)pressure_data,pressure_qc,                  &
                      height_data,height_qc,                      &
                      temperature_data,temperature_qc,            &
                      u_met_data,u_met_qc,                        &
                      v_met_data,v_met_qc,                        &
                      rh_data,rh_qc
 104    FORMAT( 1x,6(f11.3,1x,f11.3,1x))
!!! wuyh added for radar 20160506
        if(plfo(n).eq.10)then
          height_data = height_data + elevation
        endif
!!! wuyh added for radar end 20160506
! yliu 20050425 start  (add)
        IF(RTIMOB.LT.TBACK) then
          n=n-1
          IF(imc.eq.meas_count) GOTO 110
            
! yliu 20050425  end

        ELSE

! copy values into vectors
         sndg_pressure_data(imc) = pressure_data
         sndg_pressure_qc(imc) = pressure_qc
         sndg_height_data(imc) = height_data
         sndg_height_qc(imc) = height_qc
         sndg_temperature_data(imc) = temperature_data
         sndg_temperature_qc(imc) = temperature_qc
         sndg_u_met_data(imc) = u_met_data
         sndg_u_met_qc(imc) = u_met_qc
         sndg_v_met_data(imc) = v_met_data
         sndg_v_met_qc(imc) = v_met_qc
         sndg_rh_data(imc) = rh_data
         sndg_rh_qc(imc) = rh_qc

! save some original data/qc values
         sndg_orig_pressure_qc(imc) = pressure_qc        !ajb 03312010 for ob in ht implementation
         sndg_orig_pressure_data(imc) = pressure_data    !yliu 20100706 - saved for computing Q from RH
         sndg_orig_u_met_qc(imc) = u_met_qc              ! ajb 20110929 for rotating winds
         sndg_orig_v_met_qc(imc) = v_met_qc              ! ajb 20110929 for rotating winds

! wuyh added 20110630 
         oblat(n)=latitude
         oblon(n)=longitude
         flagsnd(n)=is_sound
         flagbog(n)=bogus
! wuyh added 20110630 end

        ENDIF   !end IF(RTIMOB.LT.TBACK) / ELSE

      ENDDO read_sounding

! check if ob is in the domain
      if( (ri.lt.3.).or.(ri.gt.real(e_we-3)).or.(rj.lt.3.).or.         &
          (rj.gt.real(e_sn-3)) ) then

! Adjust n and reset timeob for discarded indices.
          n=n-meas_count
          do imc = n+1, n+meas_count
            timeob(imc) = 99999.
          enddo
          goto 100  !back to beginning of read loop
      endif

      CALL check_sndg_intqc(numsnd, sndg_pressure_data, sndg_height_data,                 &
                            sndg_temperature_data,                                        &
                            sndg_u_met_data,    sndg_v_met_data,  sndg_rh_data,           &
                            sndg_pressure_qc,   sndg_height_qc,   sndg_temperature_qc,    & 
                            sndg_u_met_qc,      sndg_v_met_qc,    sndg_rh_qc,             &
                            meas_count,adjust_ht,                                         &
                            near_surf, htij, elevation, inest,                            &
                            ri, rj, s_we,e_we, s_sn,e_sn)

      CALL store_sndg_intqc(numsnd, sndg_pressure_data, sndg_height_data, elevob,          &
                            sndg_temperature_data, sndg_u_met_data, sndg_v_met_data,       &
                            sndg_rh_data, sndg_pressure_qc, sndg_height_qc,                &
                            sndg_temperature_qc, sndg_u_met_qc, sndg_v_met_qc, sndg_rh_qc, &
                            sndg_orig_pressure_data, oblat, oblon,                         &
                            sndg_orig_u_met_qc, sndg_orig_v_met_qc,                        &
                            nlevs_ob, obs_proj, map_proj, ob_vals, ob_qcvals,              &
                            n-meas_count+1, niobf, vdim, nndgv, iprt)

! wuyh added 20110630 
      imc = 0
      do nn = n-meas_count+1, n
        imc = imc + 1
!!        qcu(nn)=ob_qcvals(U,imc)
!!        qcv(nn)=ob_qcvals(V,imc)
!!        qct(nn)=ob_qcvals(T,imc)
!!        qcq(nn)=ob_qcvals(Q,imc)
      enddo
! wuyh added 20110630 end
!     print *,'--- sdng n=',n,nlevs_ob(n),lev_in_ob(n),timeob(n)
! read in non-sounding obs

    ELSEIF(.NOT.is_sound)THEN

      nlevs_ob(n)=1.
      lev_in_ob(n)=1.

! FCV additions start
      do i = 1, 40
           platform_ob(i,n) = ICHAR (platform (i:i))
                 stnid(i,n) = ICHAR (id(i:i))
                desobs(i,n) = ICHAR (namef(i:i))
      enddo
      do i=1,16
           wusource(i,n) = ICHAR (source(i:i))
      enddo
! wuyh modified 20140114
!!        do i=17,40
!!              wusource(i,n) = ICHAR (' ')
!!        enddo
        wusource(17:40,n) = empty(17:40)
! wuyh modified end 20130114

      do i = 1, 14
              wudate(i,n) = ICHAR(date_char(i:i))
      enddo
! FCV additions end

      read(nvol,105)slp_data,slp_qc,                                   &
                    ref_pres_data,ref_pres_qc,                         &
                    height_data,height_qc,                             &
                    temperature_data,temperature_qc,                   &
                    u_met_data,u_met_qc,                               &
                    v_met_data,v_met_qc,                               &
                    rh_data,rh_qc,                                     &
                    psfc_data,psfc_qc,                                 &
                    precip_data,precip_qc
 105  FORMAT( 1x,9(f11.3,1x,f11.3,1x))

!!! wuyh added 20130522: most of the surface obs set height_data = 0
!!!     if( height_data .gt. -500. .and. height_data .le. elevation ) then
!!! wuyh modified based on ShengZhen's data
     if( elevation .gt. -500. .and. height_data .le. elevation ) then
        height_data = elevation
        height_qc = 10.
      endif
!!! wuyh added 20130522

!!! wuyh added 20131010
      if(platform(7:10) .eq. 'SHIP' .AND. elevation .le. -8888.) then
        elevation = height_data
      endif
!!! wuyh added 20131010

! yliu 20050425 start  (add)
      IF(RTIMOB.LT.TBACK) then
        n=n-1
        GOTO 110
      ENDIF
! yliu 20050425  end

! check if ob is in the domain
      if( (ri.lt.3.).or.(ri.gt.real(e_we-3)).or.(rj.lt.3.).or.         &
          (rj.gt.real(e_sn-3)) ) then

! Adjust n and reset timeob for discarded indices.
          n=n-meas_count
          do imc = n+1, n+meas_count
            timeob(imc) = 99999.
          enddo
          goto 100  !back to beginning of read loop
      endif

! Check and store QC unless it is to be set from a parent grid
!     IF(QC_DOMAIN.eq.INEST) THEN

      orig_u_met_qc = u_met_qc              ! ajb 20110929 for rotating winds
      orig_v_met_qc = v_met_qc              ! ajb 20110929 for rotating winds

      CALL check_surf_intqc(slp_data,   psfc_data,  height_data, temperature_data,  &
                            u_met_data, v_met_data, rh_data,                        &
                            slp_qc,     psfc_qc,    height_qc,   temperature_qc,    &
                            u_met_qc,   v_met_qc,   rh_qc,                          &
                            n, lev_in_ob(n), nlevs_ob(n), adjust_ht,                &
                            near_surf, htij, elevation, terrmod,ob_hto,inest,       &
                            rio(n), rjo(n), s_we,e_we, s_sn,e_sn)

      CALL store_surf_intqc(slp_data,   psfc_data,  height_data, temperature_data,  &
                            u_met_data, v_met_data, rh_data,                        &
                            slp_qc,     psfc_qc,    height_qc,   temperature_qc,    &
                            u_met_qc,      v_met_qc,    rh_qc,                      &
                            latitude, longitude, elevation, plfo, rovg,             &
                            orig_u_met_qc, orig_v_met_qc,                           &
                            obs_proj, map_proj,                                     &
                            n, niobf, nndgv, ob_vals(:,1), ob_qcvals(:,1), iprt,    &
                            terrmod, ob_hto, near_surf, rko)

!     ENDIF  !end IF(QC_DOMAIN.eq.INEST)

! wuyh added 20110630 
      oblat(n)=latitude
      oblon(n)=longitude
      flagsnd(n)=is_sound
      flagbog(n)=bogus
!!      qcu(n)=ob_qcvals(U,1)
!!      qcv(n)=ob_qcvals(V,1)
!!      qct(n)=ob_qcvals(T,1)
!!      qcq(n)=ob_qcvals(Q,1)

!!! wuyh added 20131016
!     write(msg,1341)n,rio(n),rjo(n),terrmod,elevation,near_surf
!     call wrf_message(msg)
 1341 Format(I7,2(1x,F10.2),2(1x,F10.2),1x,L4)

! Set ob values to missing if difference between model terrain and ob elevation is too great.
      if(terrmod .lt. -500. .OR.                                                &
         (elevation .gt. -500. .and. abs(terrmod-elevation) .gt. terrdif) .OR.  &
         (psfc_data .le. 500. .and. elevation .eq. -999999.)) then

        ob_vals(:,1) = missing_r
        ob_qcvals(:,1) = missing_r
      endif
!!! wuyh added 20131016 end

    ELSE
      print*,' ======  '
      print*,'Should not be here unless you screw up the Data input'
      print*,'Assume this is a bad record, skip it and move to the next'
!     IF(RTIMOB.LT.TBACK) then     ! yliu 20080204: comment out
        n=n-1           ! yliu 20050425 added
        GOTO 110
!     ENDIF                        ! yliu 20080204
    ENDIF   !end if(is_sound)
! END OF SFC OBS INPUT SECTION
!======================================================================
!======================================================================

    IF(TIMEOB(N).LT.fdob%RTLAST) THEN
      IF (iprt) THEN
        PRINT *,'2 OBS ARE NOT IN CHRONOLOGICAL ORDER'
        PRINT *,'NEW YEAR?'
        print *,'timeob,rtlast,n=',timeob(n),fdob%rtlast,n,oblat(n),oblon(n)
      ENDIF
      STOP 111
    ELSE
      fdob%RTLAST=TIMEOB(N)
    ENDIF

! Split observation into separate observations, by variable
    CALL split_ob_vars(fdob, niobf, vdim, nndgv, is_sound, numsnd,     &
                       ob_var_type, ob_ndg_type, meas_count,           &
                       nlevs_ob, lev_in_ob, ob_vals, ob_qcvals,        &
                       vobs, vobs_qc, timeob,                          &
                       missing_r, num_ndg_terms, ndg_term_indxs,       &
                       newpass,inest, n, latitude, longitude,          &
                       nlast, nprev, prt_max, prt_freq, iprt,          &
                       id, stnid_prt, rio, rjo, xlat, xlong,           &
                       obs_prt, lat_prt, lon_prt, mlat_prt, mlon_prt,  &
                       e_we, e_sn, ims,ime, jms,jme, its,ite, jts,jte)
    GOTO 100
111 CONTINUE
!**********************************************************************
!       --------------   END BIG 100 LOOP OVER N  --------------
!**********************************************************************
    IF (iprt) write(6,5403) NVOL,XTIME
    IEOF(inest)=1

    close(NVOLA+INEST-1)
    IF (iprt) print *,'closed fdda file for inest=',inest,nsta

    goto 1001

130 CONTINUE

! get here if at end of file, or if obs time is beyond what we
! need right now

!!!****************************************************************
!!  If it is restart time, remove obs that out of stage
!!!****************************************************************

  IF(KTAU.EQ.KTAUR)THEN
    NSTA=0
    keep_obs : DO N=1,NIOBF

! try to keep all obs, but just don't use yet
!  (don't want to throw away last obs read in - especially if
!  its a sounding, in which case it looks like many obs)

      IF(TIMEOB(N).GT.9.e4) EXIT keep_obs
      if(timeob(n).gt.tforwd) then
        if(iprt) write(6,951)inest,n,timeob(n),tforwd
 951    FORMAT('saving ob beyond window,inest,n,timeob,tforwd=',   &
               i3,1x,I8,2(1x,f13.4))
      endif
      NSTA=N
    ENDDO keep_obs

    NDUM=0
! make time=99999. if ob is too old
!   print *,'tback,nsta=',tback,nsta
    old_obs : DO N=1,NSTA+1
      IF((TIMEOB(N)-TBACK).LT.0)THEN
        TIMEOB(N)=99999.
      ENDIF
!     print *,'n,ndum,timeob=',n,ndum,timeob(n)
      IF(TIMEOB(N).LT.9.E4) EXIT old_obs
      NDUM=N
    ENDDO old_obs

! REMOVE OLD OBS DENOTED BY 99999. AT THE FRONT OF TIMEOB ARRAY
    IF (iprt) THEN
      print *,'after 190 ndum=',ndum,nsta
      print *,'timeob=',timeob(1),timeob(2)
    ENDIF
    NDUM=ABS(NDUM)
    NMOVE=NIOBF-NDUM
    IF( NMOVE.GT.0 .AND. NDUM.NE.0) THEN
      DO N=1,NMOVE

!!! wuyh added  20111023
        wudate(:,n)     =wudate(:,N+NDUM)
        wusource(:,n)   =wusource(:,N+NDUM)
        platform_ob(:,n)=platform_ob(:,N+NDUM)
        stnid(:,n)      =stnid(:,N+NDUM)
        desobs(:,n)     =desobs(:,N+NDUM)
        flagsnd(n)      =flagsnd(N+NDUM)
        flagbog(n)      =flagbog(N+NDUM)
        oblat(n)        =oblat(N+NDUM)
        oblon(n)        =oblon(N+NDUM)
!!        qcu(n)          =qcu(N+NDUM)
!!        qcv(n)          =qcv(N+NDUM)
!!       qct(n)          =qct(N+NDUM)
!!        qcq(n)          =qcq(N+NDUM)
!!! wuyh added  20111023 ended

        do KN = 1,vdim
          VOBS(KN,N)=VOBS(KN,N+NDUM)       ! yliu 20050502                   YLIU
          VOBS_QC(KN,N)=VOBS(KN,N+NDUM)    ! yliu 20050502                   YLIU
        enddo
        do KN = 1,nerrf
          ERRF(KN,N)=ERRF(KN,N+NDUM)           ! yliu 20050502                   YLIU
        enddo
        RJO(N)=RJO(N+NDUM)
        RIO(N)=RIO(N+NDUM)
        RKO(N)=RKO(N+NDUM)
        TIMEOB(N)=TIMEOB(N+NDUM)
        nlevs_ob(n)=nlevs_ob(n+ndum)
        lev_in_ob(n)=lev_in_ob(n+ndum)
        plfo(n)=plfo(n+ndum)
        rkmob(n)=rkmob(n+ndum)
      ENDDO
    ENDIF
! moved obs up. now fill remaining space with 99999.
    NOPEN=NMOVE+1
    IF(NOPEN.LE.NIOBF) THEN
      DO N=NOPEN,NIOBF
        do KN = 1,vdim
          VOBS(KN,N)=99999.
          VOBS_QC(KN,N)=99999.
        enddo
        do KN = 1,nerrf
          ERRF(KN,N)=99999.0                   ! yliu 20050502                   YLIU
        enddo
        RIO(N)=99999.
        RJO(N)=99999.
        RKO(N)=99999.
        TIMEOB(N)=99999.
      ENDDO
    ENDIF
  ENDIF  !IF(KTAU.EQ.KTAUR)THEN

!!!*****************************************************************
!!! recalculate the total obs read in at current time window
!!!*****************************************************************

  NSTA=0
! print *,'nsta at restart setting is ',nsta
! recalculate nsta after moving things around
  recalc : DO N=1,NIOBF
! try to save all obs - don't throw away latest read in
    IF(TIMEOB(N).GT.9.e4) EXIT recalc
    NSTA=N
!   nsta=n-1         ! yliu test
  ENDDO recalc
  nstaw = nvals_le_limit(nsta, timeob, tforwd)
  fdob%NSTAT=NSTA
  fdob%NSTAW=NSTAW

!!! wuyh added 20150727
  qcu(:) = vobs_qc(1,:)
!!! wuyh added 20150727 end

  IF (iprt) then
      write(msg,160) KTAU,XTIME,NSTAW,NSTA
      call wrf_message(msg)
  ENDIF

!!!*****************************************************************
!!! at restart time
!!!*****************************************************************

  IF(KTAU.EQ.KTAUR)THEN
    IF(nudge_opt.EQ.1)THEN
      TWDOP=TWINDO*60.
      IF (iprt) THEN
        write(6,1449) INEST,RINXY,RINSIG,TWDOP
        IF(ISWIND.EQ.1) write(6,1450) GIV
        IF(ISTEMP.EQ.1) write(6,1451) GIT
        IF(ISMOIS.EQ.1) write(6,1452) GIQ
        IF(ISPSTR.EQ.1) write(6,1453) GIP
        IF(ISVR  .EQ.1) write(6,1454) GIVR
      ENDIF
    ENDIF
!!!!  ENDIF  ! wuyh comment out 20121211

!!!!  IF(KTAU.EQ.KTAUR)THEN ! wuyh comment out 20121211
!ajb 05042009 These messages are incorrect, remnant of MM5?
!   IF (iprt) THEN
!     write(6,553)
!     write(6,554)
!   ENDIF
    IF(fdob%IWTSIG.NE.1)THEN
      IF (iprt) THEN
        write(6,555)
        write(6,556) fdob%RINFMN*RINXY,fdob%RINFMX*RINXY,fdob%PFREE*10.
      ENDIF
      IF(fdob%RINFMN.GT.fdob%RINFMX)STOP 556
! IS MINIMUM GREATER THAN MAXIMUM?
      IF (iprt) write(6,557) fdob%DPSMX*10.,fdob%DCON
      IF(fdob%DPSMX.GT.10.)STOP 557
    ENDIF
!!!!  ENDIF ! wuyh comment out 20121211
! IS DPSMX IN CB?

!!!!  IF(KTAU.EQ.KTAUR)THEN ! wuyh comment out 20121211
    IF (iprt) write(6,601) INEST,IONF
  ENDIF

555   FORMAT(1X,'   ABOVE THE SURFACE LAYER, OBS NUDGING IS PERFORMED',  &
      ' ON PRESSURE LEVELS,')
556   FORMAT(1X,'   WHERE RINXY VARIES LINEARLY FROM ',E11.3,' KM AT',   &
      ' THE SURFACE TO ',E11.3,' KM AT ',F7.2,' MB AND ABOVE')
557   FORMAT(1X,'   IN THE SURFACE LAYER, WXY IS A FUNCTION OF ',        &
      'DPSMX = ',F7.2,' MB WITH DCON = ',E11.3,                          &
      ' - SEE SUBROUTINE NUDOB')
601   FORMAT('0','FOR EFFICIENCY, THE OBS NUDGING FREQUENCY ',           &
        'FOR MESH #',I2,' IS ',1I2,' CGM TIMESTEPS ',/)
5403  FORMAT(1H0,'-------------EOF REACHED FOR NVOL = ',I3,              &
       ' AND XTIME = ',F10.2,'-------------------')
160   FORMAT('0','*** End of IN4DOB AT KTAU = ',I5,' AND XTIME = ',   &
      F10.2,':NSTAW= ',I7,' NSTA= ',I7,' ***')
1449  FORMAT(1H0,'*****NUDGING INDIVIDUAL OBS ON MESH #',I2,             &
       ' WITH RINXY = ',                                                 &
      E11.3,' KM, RINSIG = ',E11.3,' AND TWINDO (HALF-PERIOD) = ',       &
      E11.3,' MIN')
1450  FORMAT(1X,'NUDGING IND. OBS WINDS WITH GIV = ',E11.3)
1451  FORMAT(1X,'NUDGING IND. OBS TEMPERATURE WITH GIT = ',E11.3)
1452  FORMAT(1X,'NUDGING IND. OBS MOISTURE WITH GIQ = ',E11.3)
1453  FORMAT(1X,'NUDGING IND. OBS SURFACE PRESSURE WITH GIP = ,'E11.3)
1454  FORMAT(1X,'NUDGING IND. OBS RADIAL WIND WITH GIPV = ,'E11.3)
553   FORMAT(1X,'BY DEFAULT: OBS NUDGING OF TEMPERATURE AND MOISTURE ',  &
      'IS RESTRICTED TO ABOVE THE BOUNDARY LAYER')
554   FORMAT(1X,'...WHILE OBS NUDGING OF WIND IS INDEPENDENT OF THE ',   &
      'BOUNDARY LAYER')

  RETURN
  END SUBROUTINE in4dob

  SUBROUTINE stop_122(n, niobf, iprt)
!------------------------------------------------------------------------------
! PURPOSE: THIS ROUTINE IS CALLED IF THERE ARE TOO MANY OBSERVATIONS,
! THAT IS, THE OBSERVATION ARRAYS ARE FULL AND THE MOST RECENTLY
! ACQUIRED OBS STILL HAS TIMEOB .LE. TFORWD.  SO ISSUE A WARNING
! MESSAGE AND STOP THE JOB.
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, intent(in) :: n           ! index of most recently acquired ob
  INTEGER, intent(in) :: niobf       ! maximum number of observations
  LOGICAL, intent(in) :: iprt        ! print flag

  IF (iprt) write(6,121) N,NIOBF
  STOP 122
121   FORMAT('0','  WARNING: NOBS  = ',I7,' IS GREATER THAN MAX_OBS = ', &
      I7,': INCREASE PARAMETER MAX_OBS')

  END SUBROUTINE stop_122

  SUBROUTINE vect(xlon,e1,e2,xlonc,xlatc,xn)

! THIS ROUTINE CONVERTS INCOMING U AND V COMPS INTO MAP U AND V COMPS.
! iproj is projection (1=lamconf, 2=polarst, 3=mercator)
! xlonc is center longitude
! xn is cone factor (.716 for current lc)
!
      REAL, intent(in) :: XLON
      REAL, intent(inout) :: E1
      REAL, intent(inout) :: E2
      REAL, intent(in) :: xlonc
      REAL, intent(in) :: xlatc
      REAL, intent(in) :: xn

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real :: pi, degran, u, v, xlonr, angle

      pi=3.1415926535
      DEGRAN=PI/180.
!
!
         u=e1
         v=e2
         XLONR=XLONC-XLON
         IF(XLONR.GT.180.) XLONR=XLONR-360.
         IF(XLONR.LT.-180.) XLONR=XLONR+360.
         ANGLE=XLONR*XN*DEGRAN
         IF (xlatC.LT.0.0) ANGLE=-ANGLE
         E1=V*SIN(ANGLE)+U*COS(ANGLE)
         E2=V*COS(ANGLE)-U*SIN(ANGLE)
      RETURN
  END SUBROUTINE vect

  SUBROUTINE rh2r(rh,t,p,r,iice)

! convert rh to r
! if iice=1, use saturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
!
      REAL, intent(in)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(out) :: r
      INTEGER, intent(in)  :: iice

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat

      eps=0.62197
      e0=6.1078
      eslcon1=17.2693882
      eslcon2=35.86
      esicon1=21.8745584
      esicon2=7.66
!      t0=260.
      t0=273.15

!     print *,'rh2r input=',rh,t,p
      rh1=rh*.01

! wuyh added from osse 20101230
!       if(iice.eq.1.and.t.le.t0)then
!         esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
!       else
!         esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
!       endif
!!! osse exp
!      esat=10**(-2937.4/t-4.9283*log10(t)+23.5518)  ! in mb
!      if(t < 233. ) esat=10**(10.5553-2667./t)      ! saturation over ice
! wuyh added from osse 20101230 end
!      rsat=eps*esat/(p-esat)
!     print *,'rsat,esat=',rsat,esat
!      r=rh1*rsat
!      if(r.gt.1.) r = 1.
!      if(r.lt.0.) r = 0.
!      print *,'rh2r rh,t,p,r=',rh1,t,p,r

!!! wuyh using WRFDA formula 20120130
      ESAT = 6.112 * EXP (17.67*(T-273.15)/(T-29.60))
      RSAT = 0.622 * ESAT /(P-ESAT)
!!! wuyh using WRFDA formula end 20120130

!     print *,'rsat,esat=',rsat,esat
      r=rh1*rsat
      if(r.gt.1.) r = float(1)
      if(r.lt.0.) r = float(0)
!      print *,'in rh2r rh,t,p,r=',rh1,t,p,r

      return
  END SUBROUTINE rh2r




  SUBROUTINE rh2rold(rh,t,p,r,iice)

! convert rh to r
! if iice=1, use saturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
!
      REAL, intent(in)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(out) :: r
      INTEGER, intent(in)  :: iice

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat

      eps=0.62197
      e0=6.1078
      eslcon1=17.2693882
      eslcon2=35.86
      esicon1=21.8745584
      esicon2=7.66
      t0=260.

!     print *,'rh2r input=',rh,t,p
      rh1=rh*.01

!      if(iice.eq.1.and.t.le.t0)then
!        esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
!      else
!        esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
!      endif

      esat=10**(-2937.4/t-4.9283*log10(t)+23.5518)  ! in mb
      if(t < 233. ) esat=10**(10.5553-2667./t)      ! saturation over ice

      rsat=eps*esat/(p-esat)
!     print *,'rsat,esat=',rsat,esat
      r=rh1*rsat
      if(r.gt.1.) r = 1.
      if(r.lt.0.) r = 0.

!      print *,'rh2r rh,t,p,r=',rh1,t,p,r

      return
  END SUBROUTINE rh2rold

!*************************************************************************
  SUBROUTINE rh2rb(rh,t,p,r,iice)
!*************************************************************************

! convert rh to r
! if iice=1, use daturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb

      REAL, intent(in)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(out) :: r
      INTEGER, intent(in)  :: iice

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat

      eps=0.622
      e0=6.112
      eslcon1=17.67
      eslcon2=29.65
      esicon1=22.514
      esicon2=6.15e3
      t0=273.15

      print *,'rh2r input=',rh,t,p
      rh1=rh*.01

      if(iice.eq.1.and.t.le.t0)then
        esat=e0*exp(esicon1-esicon2/t)
      else
        esat=e0*exp(eslcon1*(t-t0)/(t-eslcon2))
      endif
      rsat=eps*esat/(p-esat)
!     print *,'rsat,esat=',rsat,esat
      r=rh1*eps*rsat/(eps+rsat*(1.-rh1))

      print *,'rh2r rh,t,p,r=',rh1,t,p,r

      return
END SUBROUTINE rh2rb

  SUBROUTINE set_projection (obs_proj, map_proj, cen_lat, cen_lon,     &
                             true_lat1, true_lat2, stand_lon,          &
                             known_lat, known_lon,                     &
                             e_we, e_sn, dxm, dym )

  USE module_llxy

!*************************************************************************
! Purpose: Set map projection information which will be used to map the
!          observation (lat,lon) location to its corresponding (x,y)
!          location on the WRF (coarse) grid. using the selected map
!          projection (e.g., Lambert, Mercator, Polar Stereo, etc).
!*************************************************************************

      IMPLICIT NONE

  TYPE(PROJ_INFO), intent(out)  :: obs_proj   ! structure for obs projection info.
  INTEGER, intent(in) :: map_proj             ! map projection index
  REAL, intent(in) :: cen_lat                 ! center latitude for map projection
  REAL, intent(in) :: cen_lon                 ! center longiture for map projection
  REAL, intent(in) :: true_lat1               ! truelat1 for map projection
  REAL, intent(in) :: true_lat2               ! truelat2 for map projection
  REAL, intent(in) :: stand_lon               ! standard longitude for map projection
  INTEGER, intent(in) :: e_we                 ! max grid index in south-north coordinate
  INTEGER, intent(in) :: e_sn                 ! max grid index in west-east   coordinate
  REAL, intent(in) :: known_lat               ! latitude  of domain origin point (i,j)=(1,1)
  REAL, intent(in) :: known_lon               ! longigude of domain origin point (i,j)=(1,1)
  REAL, intent(in) :: dxm                     ! grid size in x (meters)
  REAL, intent(in) :: dym                     ! grid size in y (meters)

! Set up map transformation structure
      CALL map_init(obs_proj)

      ! Mercator
      IF (map_proj == PROJ_MERC) THEN
         CALL map_set(PROJ_MERC, obs_proj,                                &
                      truelat1 = true_lat1,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)

      ! Lambert conformal
      ELSE IF (map_proj == PROJ_LC) THEN
      CALL map_set(PROJ_LC, obs_proj,                                     &
                      truelat1 = true_lat1,                               &
                      truelat2 = true_lat2,                               &
                      stdlon   = stand_lon,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)

      ! Polar stereographic
      ELSE IF (map_proj == PROJ_PS) THEN
         CALL map_set(PROJ_PS, obs_proj,                                  &
                      truelat1 = true_lat1,                               &
                      stdlon   = stand_lon,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)
      ! Cassini (global ARW)
      ELSE IF (map_proj == PROJ_CASSINI) THEN
         CALL map_set(PROJ_CASSINI, obs_proj,                             &
                      latinc   = dym*360.0/(2.0*EARTH_RADIUS_M*PI),       &
                      loninc   = dxm*360.0/(2.0*EARTH_RADIUS_M*PI),       &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
! We still need to get POLE_LAT and POLE_LON metadata variables before
!   this will work for rotated poles.
                      lat0     = 90.0,                                    &
                      lon0     = 0.0,                                     &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      stdlon   = stand_lon)

      ! Rotated latitude-longitude
      ELSE IF (map_proj == PROJ_ROTLL) THEN
         CALL map_set(PROJ_ROTLL, obs_proj,                               &
! I have no idea how this should work for NMM nested domains
                      ixdim    = e_we-1,                                  &
                      jydim    = e_sn-1,                                  &
                      phi      = real(e_sn-2)*dym/2.0,                    &
                      lambda   = real(e_we-2)*dxm,                        &
                      lat1     = cen_lat,                                 &
                      lon1     = cen_lon,                                 &
                      latinc   = dym,                                     &
                      loninc   = dxm,                                     &
                      stagger  = HH)

      END IF

!       write(6,*) 'ajb init: map_proj = ',map_proj
!       write(6,*) 'ajb: after setting map:'
!       write(6,*) 'truelat1 = ',obs_proj%truelat1
!       write(6,*) 'truelat2 = ',obs_proj%truelat2
!       write(6,*) 'stdlon   = ',obs_proj%stdlon
!       write(6,*) 'lat1     = ',obs_proj%lat1
!       write(6,*) 'lon1     = ',obs_proj%lon1
!       write(6,*) 'knowni   = ',obs_proj%knowni
!       write(6,*) 'knownj   = ',obs_proj%knownj
!       write(6,*) 'dx       = ',obs_proj%dx

  END SUBROUTINE set_projection

  SUBROUTINE fmt_date(idate,odate)                                             !obsnypatch

!*************************************************************************
! Purpose: Re-format a character date string from YYYYMMDDHHmmss form
!          to YYYY-MM-DD_HH:mm:ss form.
! INPUT:
!     IDATE - Date string as YYYYMMDDHHmmss
! OUTPUT:
!     ODATE - Date string as YYYY-MM-DD_HH:mm:ss 
!*************************************************************************

      IMPLICIT NONE

      CHARACTER*14, intent(in)  :: idate        ! input  date string
      CHARACTER*19, intent(out) :: odate        ! output date string

      odate(1:19) = "0000-00-00_00:00:00"
      odate(1:4)   = idate(1:4)                 ! Year
      odate(6:7)   = idate(5:6)                 ! Month
      odate(9:10)  = idate(7:8)                 ! Day
      odate(12:13) = idate(9:10)                ! Hours
      odate(15:16) = idate(11:12)               ! Minutes
      odate(18:19) = idate(13:14)               ! Seconds

      RETURN
  END SUBROUTINE fmt_date

  INTEGER FUNCTION nvals_le_limit(isize, values, limit)
!------------------------------------------------------------------------------
! PURPOSE: Return the number of values in a (real) non-decreasing array that
!          are less than or equal to the specified upper limit.
! NOTE: It is important that the array is non-decreasing!
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: isize           ! Size of input array
  REAL,    INTENT(IN) :: values(isize)   ! Input array of reals
  REAL,    INTENT(IN) :: limit           ! Upper limit

! Local variables
  integer :: n

! Search the array from largest to smallest values.
   find_nvals: DO n = isize, 1, -1
                 if(values(n).le.limit) EXIT find_nvals
               ENDDO find_nvals
  nvals_le_limit = n

  RETURN
  END FUNCTION nvals_le_limit

  SUBROUTINE collect_obs_info(newpass,inest,n,latitude,longitude,             &
                              nlast,nprev,niobf,prt_max,prt_freq,             &
                              station_id,stnid, rio,rjo,xlat,xlong,           &
                              obs, lat,lon, mlat,mlon,                        &
                              e_we,e_sn,ims,ime,jms,jme,its,ite,jts,jte)
!*************************************************************************
! Purpose: Collect the obs index, obs latitude, obs longitude, obs station
!          id, and model latitude and longitude values for print
!          diagnostics. Note that THIS SUBROUTINE IS CALLED INTERATIVELY
!          FROM IN4DOB, WITHIN THE OBS READ LOOP that reads new obser-
!          vations needed for the new time window. Flag newpass is true
!          the first time collect_obs_info is called from the read-loop
!          for a new time window. So for each pass of IN4DOB, newpass is
!          true the first time IN4DOB calls collec_obs_info.

!          OBS (soundings) contain multiple obs levels. So on each sub-
!          sequent call of collect_obs_info for a specific pass of IN4DOB,
!          n will jump by the number of levels in the sounding.
!
!          Here, nlast refers to the index of the last valid-time obs
!          that was read in during the last pass of IN4DOB (after the old
!          obs were removed). This way we can properly start storing
!          obs information for the new obs that are being read on this
!          pass of IN4DOB, beginning with the first newly read obs for
!          this pass of IN4DOB.
!
!          Note that nprev is needed to properly handle soundings. On
!          each pass, n is stored into nprev, and on each subsequent
!          pass of collect_obs_info, a loop is performed from nprev+1 to n.
!*************************************************************************

  IMPLICIT NONE

  LOGICAL, intent(inout) :: newpass        ! New pass flag
  INTEGER, intent(in)    :: inest          ! nest index
  INTEGER, intent(in)    :: n              ! Observation index
  REAL,    intent(in)    :: latitude       ! Latitude of obs
  REAL,    intent(in)    :: longitude      ! Latitude of obs
  INTEGER, intent(in)    :: nlast          ! Last obs of valid obs, prev window
  INTEGER, intent(inout) :: nprev          ! Previous obs in new window read seq
  INTEGER, intent(in)    :: niobf          ! Maximum number of observations
  INTEGER, intent(in)    :: prt_max        ! Max no. of obs for diagnostic printout
  INTEGER, intent(in)    :: prt_freq       ! Frequency for diagnostic printout
  CHARACTER*15, intent(in) :: station_id   ! First 15 chars of station id for obs n
  INTEGER, intent(inout) :: stnid(40,prt_max) ! Station ids for diagnostic printout
  REAL,    intent(in)    :: rio(niobf)     ! West-east coord of ob, non-staggered grid
  REAL,    intent(in)    :: rjo(niobf)     ! South-north coord of ob, non-staggered grid
  REAL, DIMENSION( ims:ime, jms:jme ),                                   &
           intent(in )   :: xlat, xlong    ! Lat/lon on mass-pt grid
  INTEGER, intent(inout) :: obs(prt_max)   ! Obs index for printout
  REAL,    intent(inout) :: lat(prt_max)   ! Obs latitude for printout
  REAL,    intent(inout) :: lon(prt_max)   ! Obs longitude for printout
  REAL,    intent(inout) :: mlat(prt_max)  ! Model latitude at (rio,rjo) for printout
  REAL,    intent(inout) :: mlon(prt_max)  ! Model longitude at (rio,rjo) for printout
  INTEGER, intent(in)    :: e_we           ! Max grid index in south-north
  INTEGER, intent(in)    :: e_sn           ! Max grid index in west-east
  INTEGER, intent(in)    :: ims            ! Grid mem start (west-east)
  INTEGER, intent(in)    :: ime            ! Grid mem end   (west-east)
  INTEGER, intent(in)    :: jms            ! Grid mem start (south-north)
  INTEGER, intent(in)    :: jme            ! Grid mem end   (south-north)
  INTEGER, intent(in)    :: its            ! Grid tile start (west-east)
  INTEGER, intent(in)    :: ite            ! Grid tile end   (west-east)
  INTEGER, intent(in)    :: jts            ! Grid tile start (south-north)
  INTEGER, intent(in)    :: jte            ! Grid tile end   (south-north)

! Local variables
  integer i                       ! Loop counter over station id character
  integer nn                      ! Loop counter over obs index
  integer ndx,ndxp                ! Index into printout arrays (ndx and prev ndx)
  real    :: ri, rj               ! Mass-pt coord of obs
  integer :: ril, rjl             ! Mass-pt integer coord immed sw of obs
  integer :: iend, jend           ! Upper i, j index for interpolation
  real    :: dxob, dyob           ! Grid fractions for interp
  logical :: llsave               ! Save lat/lon values if true
  character(len=200) :: msg       ! Argument to wrf_message

  if(newpass) then
    newpass = .false.
    nprev = nlast       ! Reset in case old obs have been discarded from prev window
  endif

! Start iteration only if we have not yet stored prt_max number of obs for printing.
! Note: The loop below could represent multiple levels in a sounding, so we
!       go ahead and start the loop if the beginning index (ndx) is prt_max or
!       less, and then exit the loop if ndx exceeds prt_max.
    if(prt_freq.gt.0) then
       ndx = (n-nlast-1)/prt_freq + 1
       ndxp = (nprev-nlast-1)/prt_freq + 1
    else
       write(msg,*) 'FATAL ERROR! OBS NAMELIST INPUT obs_prt_freq MUST BE > 0.'
       call wrf_message(msg)
       write(msg,*) 'THE NAMELIST VALUE IS',prt_freq,' FOR NEST ',inest
       call wrf_message(msg)
       call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob STOP' )
    endif

    if(ndxp .lt. prt_max) then

   MODCHK : do nn = nprev+1, n
        llsave = .false.

!       if( mod(nn-1,prt_freq) .eq. 0 ) then
        if( mod(nn-nlast-1,prt_freq) .eq. 0 ) then
           ndx = (nn-nlast-1)/prt_freq + 1
           if(ndx.gt.prt_max) EXIT MODCHK       ! Limit printout to prt_max entries
           llsave = .true.
        endif
        if(llsave) then

! Collect obs index and latitude and longitude.
          obs(ndx) = nn
          lat(ndx) = latitude
          lon(ndx) = longitude

! Collect first 15 chars of obs station id (in integer format).
          do i = 1,10
            stnid(i,ndx) = ichar(station_id(i:i))
          enddo

! Compute and collect the model latitude and longitude at the obs point.
          CALL get_model_latlon(nn,niobf,rio,rjo,xlat,xlong,e_we,e_sn,    &
                                ims,ime,jms,jme,its,ite,jts,jte,          &
                                mlat(ndx),mlon(ndx))

        endif  !end if(llsave)
      enddo MODCHK

    endif  !end if(ndx .le. prt_max)

! Save index of previous obs in read loop.
    nprev = n

  END SUBROUTINE collect_obs_info

  SUBROUTINE get_model_latlon(n,niobf,rio,rjo,xlat,xlong,e_we,e_sn,   &
                              ims,ime,jms,jme,its,ite,jts,jte,        &
                              mlat,mlon)
!*************************************************************************
! Purpose: Use bilinear interpolation to compute the model latitude and 
!          longitude at the observation point.
!*************************************************************************

  IMPLICIT NONE

  INTEGER, intent(in)    :: n              ! Observation index
  INTEGER, intent(in)    :: niobf          ! Maximum number of observations
  REAL,    intent(in)    :: rio(niobf)     ! West-east coord of ob, non-staggered grid
  REAL,    intent(in)    :: rjo(niobf)     ! South-north coord of ob, non-staggered grid
  REAL, DIMENSION( ims:ime, jms:jme ),                                   &
           intent(in )   :: xlat, xlong    ! Lat/lon on mass-pt grid
  INTEGER, intent(in)    :: e_we           ! Max grid index in south-north
  INTEGER, intent(in)    :: e_sn           ! Max grid index in west-east
  INTEGER, intent(in)    :: ims            ! Grid mem start (west-east)
  INTEGER, intent(in)    :: ime            ! Grid mem end   (west-east)
  INTEGER, intent(in)    :: jms            ! Grid mem start (south-north)
  INTEGER, intent(in)    :: jme            ! Grid mem end   (south-north)
  INTEGER, intent(in)    :: its            ! Grid tile start (west-east)
  INTEGER, intent(in)    :: ite            ! Grid tile end   (west-east)
  INTEGER, intent(in)    :: jts            ! Grid tile start (south-north)
  INTEGER, intent(in)    :: jte            ! Grid tile end   (south-north)
  REAL,    intent(out)   :: mlat           ! Model latitude at obs point
  REAL,    intent(out)   :: mlon           ! Model longitude at obs point

! Local variables
  integer ndx                     ! Index into save arrays
  real    :: ri, rj               ! Mass-pt coord of obs 
  integer :: ril, rjl             ! Mass-pt integer coord immed sw of obs 
  integer :: iend, jend           ! Upper i, j index for interpolation
  real    :: dxob, dyob           ! Grid fractions for interp

! Compute model latitude and longitude if point on tile.
  ri  = rio(n) - .5            ! mass-pt west-east obs grid coord
  rj  = rjo(n) - .5            ! mass-pt south-north obs grid coord
  ril = int(ri)
  rjl = int(rj)
  dxob = ri - float(ril)
  dyob = rj - float(rjl)
  iend = min(ite+1,e_we-2)
  jend = min(jte+1,e_sn-2)
  mlat = -999
  mlon = -999

  if(ri.ge.its .and. ri.lt.iend .and. rj.ge.jts .and. rj.lt.jend) then

! bilinear interpolation
     mlat = ((1.-dyob)*((1.-dxob)*xlat(ril,rjl)+             &
            dxob*xlat(ril+1,rjl)                             &
            )+dyob*((1.-dxob)*xlat(ril,rjl+1)+               &
            dxob*xlat(ril+1,rjl+1)))

     mlon = ((1.-dyob)*((1.-dxob)*xlong(ril,rjl)+            &
            dxob*xlong(ril+1,rjl)                            &
            )+dyob*((1.-dxob)*xlong(ril,rjl+1)+              &
            dxob*xlong(ril+1,rjl+1)))
  endif

  END SUBROUTINE get_model_latlon

  SUBROUTINE rotate_vector(lon,u,v,obs_proj,map_proj)

  USE module_llxy

!*************************************************************************
! Purpose: Rotate a single Earth-relative wind vector to a grid-relative
!          wind vector.
!*************************************************************************

  IMPLICIT NONE

  REAL,           intent(in)    :: lon        ! Longitude (deg)
  REAL,           intent(inout) :: u          ! U-component of wind vector
  REAL,           intent(inout) :: v          ! V-component of wind vector
  TYPE(PROJ_INFO),intent(in)    :: obs_proj   ! Structure for obs projection
  INTEGER,        intent(in)    :: map_proj   ! Map projection index

! Local variables
  real diff, alpha
  double precision udbl, vdbl

! Only rotate winds for Lambert conformal or polar stereographic
  if (map_proj == PROJ_LC .or. map_proj == PROJ_PS) then

     diff = obs_proj%stdlon - lon
     if (diff > 180.) then
        diff = diff - 360.
     else if (diff < -180.) then
        diff = diff + 360.
     end if

! Calculate the rotation angle, alpha, in radians
     if (map_proj == PROJ_LC) then
        alpha = diff * obs_proj%cone * rad_per_deg * obs_proj%hemi
     else
        alpha = diff * rad_per_deg * obs_proj%hemi
     end if

     udbl = v*sin(alpha) + u*cos(alpha)
     vdbl = v*cos(alpha) - u*sin(alpha)
     u = udbl
     v = vdbl

  endif
  END SUBROUTINE rotate_vector

!-------------------------------------------------------------------
! Begin Subroutines: check_sndg_intqc, check_surf_intqc
!-------------------------------------------------------------------

  SUBROUTINE check_sndg_intqc(numsnd, pressure_data, height_data,              &
                              temperature_data,u_met_data,v_met_data,rh_data,  &
                              pressure_qc,   height_qc,   temperature_qc,      &
                              u_met_qc,      v_met_qc,    rh_qc,               &
                              nlevs_ob, adjust_ht,                             &
                              near_surf, htij, stn_elev, inest,                &
                              rio, rjo, ids,ide, jds,jde)

!*************************************************************************
! Purpose: Check and possibly reset qc values for the input data. Note
!          that pressure_data may also be reset!
!*************************************************************************
  USE module_fddaobs_utilities, ONLY : near_surface_ob, terrain_height, max_ht2adjust, &
                                       find_hilo_ht, interp_ht, p2h, pphh2h, max_ht,   &
                                       min_ht, missing_r

  IMPLICIT NONE

!! wuyh added 20131114
  Integer, intent(in) :: numsnd      ! Max number of obs in sounding
  Integer,Parameter   :: nadjh = 100
  Integer             :: ksnd_top    !the last sounding obs which need height adjustment
!! wuyh added 20131114 
  REAL, intent(inout) :: pressure_data(numsnd)
  REAL, intent(inout) :: height_data(numsnd)
  REAL, intent(inout) :: temperature_data(numsnd)
  REAL, intent(inout) :: u_met_data(numsnd)
  REAL, intent(inout) :: v_met_data(numsnd)
  REAL, intent(inout) :: rh_data(numsnd)
  REAL, intent(inout) :: pressure_qc(numsnd)
  REAL, intent(inout) :: height_qc(numsnd)
  REAL, intent(inout) :: temperature_qc(numsnd)
  REAL, intent(inout) :: u_met_qc(numsnd)
  REAL, intent(inout) :: v_met_qc(numsnd)
  REAL, intent(inout) :: rh_qc(numsnd)

  Integer, intent(in)  :: nlevs_ob           ! number of levels in sounding obs
  LOGICAL, intent(in) :: adjust_ht           ! namelist switch for applying height adjustment
  LOGICAL, intent(inout) :: near_surf        ! true if ob needs height adjustment
  REAL, intent(in)    :: htij(ids:ide, jds:jde) ! 2D terrain height array (m)
  REAL, intent(in)    :: stn_elev            ! elevations of observations  (meters)
  INTEGER, intent(in) :: inest               ! nest level
  REAL, intent(in)    :: rio                 ! west-east   coord of ob, on non-staggered grid
  REAL, intent(in)    :: rjo                 ! south-north coord of ob, on non-staggered grid
  INTEGER,intent(in)  :: ids,ide, jds,jde    ! domain dims.

!!! wuyh added 20131016
  Real::terrmod,ob_hto
!!! wuyh added 20131016

  Integer :: ii           ! ob index in ob window
  Integer :: lev         ! loop counter over sounding levels
  Logical :: same_sndg   ! dummy arg for near_surface_ob
  Real    :: terr_ht     ! terrain height at observation
  Real    :: hmax        ! maximum height(m) to apply adjustment
  Integer :: lowest      ! lowest ob in sounding with good height data
  Integer :: highest     ! highest ob below (or just above?) hmax with good height data
  character(len=200) :: msg       ! Argument to wrf_message

! Sanity check
  if (nlevs_ob.gt.numsnd) then
    write(msg,*) 'FATAL ERROR: Maximum allowed obs in sounding exceeded! nlevs = ',nlevs_ob,' max = ',numsnd
    call wrf_message(msg)
    call wrf_error_fatal ( 'wrf_fddaobs_in: check_sndg_intq STOP' )
    stop
  endif

! We need model terrain at ob location to calculate hmax.
!!! wuyh modified 20131206
!!  if( rio .gt. 3 .and. rio .lt. (ide - 3) .and. &
!!      rjo .gt. 3 .and. rjo .lt. (jde - 3) )then
      terr_ht = terrain_height(rio,rjo,htij,ids,ide,jds,jde)
!!  else
!!      terr_ht = missing_r
!!  endif
!!! wuyh modified 20131206 end

! Do height adjustment if model terrain and station elev differ by more than 5 meters
!!! wuyh modified for OSSE
!!   if(adjust_ht .AND.  abs(terr_ht-stn_elev) .gt. 10.) then
   if(adjust_ht) then
!!! wuyh modified for OSSE

      hmax = max_ht2adjust(terr_ht, stn_elev)
! Find lowest and highest ob with good height data
      CALL find_hilo_ht(nlevs_ob, numsnd, height_data, height_qc, stn_elev,  &
                      hmax, max_ht, min_ht, lowest, highest)
! Find levels with good height and do the interpolation
      CALL interp_ht(numsnd, lowest, highest, stn_elev, hmax, min_ht, max_ht,  &
                   missing_r, height_data, height_qc, pressure_data,         &
                   temperature_data, u_met_data, v_met_data, rh_data,        &
                   pressure_qc, temperature_qc, u_met_qc, v_met_qc, rh_qc)
!!! wuyh modified for OSSE
!!      write(214,*) "highest=",highest
!!      highest = numsnd
!!! wuyh modified for OSSE
! DO NEAR-SURFACE HEIGHT ADJUSTMENT NOW IF SCHEME TURNED ON
      DO lev = 1, highest
        if(temperature_data(lev).eq.-888888.)temperature_qc(lev)=-888888.
!! wuyh modified 20131206
!!        CALL near_surface_ob(height_data(lev), temperature_data(lev), height_qc(lev), &
!!                             near_surf,htij, stn_elev,terr_ht,ob_hto, &
!!                             rio, rjo, ids,ide, jds,jde )
         CALL near_surface_ob(height_data(lev), temperature_data(lev), height_qc(lev), &
                             near_surf, stn_elev,terr_ht,ob_hto)
!! wuyh modified 20131206
!ajb: suppress this output
!!        write(681,1681)highest,lev,ob_hto,height_data(lev),stn_elev,terrmod

! For near-surf obs, the pressure will be calculated from the adjusted height.
        if(near_surf) then
          pressure_data(lev) = -888888.
          pressure_qc(lev)   = abs(height_qc(lev))-abs(100*INT(height_qc(lev)/100))
        endif
      ENDDO
   else
        near_surf = .false.
   endif

 1681 Format(2(I6,1x),4(1x,F10.2))
!!  endif   !end if( abs(terr_ht-stn_elev) .gt. 5.)

  DO lev = 1, nlevs_ob
! Check pressure and height to see if we need to set data and qc such that pressure will 
! later be calculated from height in subroutine errob. Pressure will be calculated
! from height for obs near surface, or when pressure data is bad and height data is good.

    if(pressure_data(lev) .lt.-8.) then
      if(height_data(lev).gt.min_ht.and.height_qc(lev).ge. -0.01) then
        pressure_qc(lev)=abs(height_qc(lev))-abs(100*INT(height_qc(lev)/100))
      else
        pressure_qc(lev)=-888888.
      endif
    else
      pressure_qc(lev)=abs(pressure_qc(lev))-abs(100*INT(pressure_qc(lev)/100))
    endif
    if(pressure_data(lev).ge.0. .and. pressure_data(lev).le.105000.) then
      pressure_qc(lev)=10.
    endif

    if(height_data(lev).eq.-888888.) then
      height_qc(lev)=-888888.
    else
      height_qc(lev)=abs(height_qc(lev))-abs(100*INT(height_qc(lev)/100))
    endif

    u_met_qc(lev)=abs(u_met_qc(lev))-abs(100*INT(u_met_qc(lev)/100))
    v_met_qc(lev)=abs(v_met_qc(lev))-abs(100*INT(v_met_qc(lev)/100))
    if(u_met_data(lev).eq.-888888.) u_met_qc(lev)=-888888.
    if(v_met_data(lev).eq.-888888.) v_met_qc(lev)=-888888.
  ENDDO

  END SUBROUTINE check_sndg_intqc

  SUBROUTINE check_surf_intqc(slp_data,   psfc_data,  height_data, temperature_data,  &
                              u_met_data, v_met_data, rh_data,                        &
                              slp_qc,     psfc_qc,    height_qc,   temperature_qc,    &
                              u_met_qc,   v_met_qc,   rh_qc,                          &
                              n, lev_in_ob, nlevs_ob, adjust_ht,                      &
                              near_surf, htij, stn_elev,terrmod,ob_hto,inest,         &
                              rio, rjo, ids,ide, jds,jde)
  USE module_fddaobs_utilities, ONLY : near_surface_ob,terrain_height
!****************************************************************************
! Purpose: Check and possibly reset qc values for the input surface obs data.
!****************************************************************************
  IMPLICIT NONE

  REAL, intent(in)    :: slp_data
  REAL, intent(in)    :: psfc_data
  REAL, intent(inout) :: height_data
  REAL, intent(inout) :: temperature_data
  REAL, intent(in)    :: u_met_data
  REAL, intent(in)    :: v_met_data
  REAL, intent(in)    :: rh_data
  REAL, intent(inout) :: slp_qc
  REAL, intent(inout) :: psfc_qc
  REAL, intent(inout) :: height_qc
  REAL, intent(inout) :: temperature_qc
  REAL, intent(inout) :: u_met_qc
  REAL, intent(inout) :: v_met_qc
  REAL, intent(inout) :: rh_qc
  Real, Parameter     :: missing_r = -888888.

!!! wuyh added 20131004
  INTEGER, intent(in) :: n
  INTEGER, intent(in) :: ids,ide, jds,jde    ! domain dims.
  REAL, intent(in)    :: lev_in_ob           ! level in sounding-type ob
  REAL, intent(in)    :: nlevs_ob            ! number of levels in sounding obs
  REAL, intent(in)    :: htij(ids:ide, jds:jde) ! 2D terrain height array (m)
  REAL, intent(in)    :: stn_elev            ! ob elevation (meters)
!!! wuyh added 20131016
  Real, intent(out)   :: terrmod             ! model terrain at obs site
!!! wuyh added 20131016 end

  INTEGER, intent(in) :: inest               ! nest level
  REAL, intent(in)    :: rio                 ! west-east   coord of ob, on non-staggered grid
  REAL, intent(in)    :: rjo                 ! south-north coord of ob, on non-staggered grid
  LOGICAL, intent(in) :: adjust_ht           ! namelist switch for applying height adjustment
  LOGICAL, intent(inout) :: near_surf        ! true if ob needs height adjustment
  Logical::same_sndg
  Real:: ob_hto

  if(temperature_data .le. 100. .OR. temperature_data .gt.350.) temperature_qc=-888888.

  terrmod = terrain_height(rio, rjo, htij, ids,ide, jds,jde)

  if( adjust_ht .and. abs(terrmod-stn_elev) .gt. 10.) then

    CALL near_surface_ob(height_data, temperature_data, height_qc, &
                         near_surf,stn_elev,terrmod,ob_hto)
  else
    near_surf = .false.
  endif

  if(psfc_data.le.-888.) then
    psfc_qc=missing_r
  elseif( psfc_data .gt. 65000.) then
    psfc_qc=abs(psfc_qc)-abs(100*INT(psfc_qc/100))
! yliu 20041231: looks like psfc_qc = 0 with good psfc 
    if(psfc_data.gt.65000. .and. psfc_data.lt.105000.) then
      psfc_qc=5.0
    endif
  endif

  if(slp_data.le.-8.) then
    slp_qc=missing_r
  elseif( slp_qc .gt. -888.) then
    slp_qc=abs(slp_qc)-abs(100*INT(slp_qc/100))
  endif

  if(height_data.eq.missing_r) then
   height_qc=missing_r
  else
   height_qc=abs(height_qc)-abs(100*INT(height_qc/100))
  endif

  temperature_qc=abs(temperature_qc)-abs(100*INT(temperature_qc/100))
  if(temperature_data .le. 100.) temperature_qc=missing_r
  u_met_qc=abs(u_met_qc)-abs(100*INT(u_met_qc/100))
  if(u_met_data.eq.missing_r) u_met_qc=missing_r
  v_met_qc=abs(v_met_qc)-abs(100*INT(v_met_qc/100))
  if(v_met_data.eq.missing_r) v_met_qc=missing_r

!  if(rh_data .le.-888.) rh_qc=missing_r    ! rh_data may be derived from rh_qc when dew point temperature is used

  END SUBROUTINE check_surf_intqc

!-------------------------------------------------------------------
! Begin Subroutines: store_sndg_intqc, store_surf_intqc
!-------------------------------------------------------------------

  SUBROUTINE store_sndg_intqc(numsnd, pressure_data, height_data, elevob,    &
                 temperature_data, u_met_data,    v_met_data,  rh_data,      &
                 pressure_qc,   height_qc,   temperature_qc,                 &
                 u_met_qc,      v_met_qc,    rh_qc,                          &
                 orig_pressure_data, oblat, oblon,                           &
                 orig_u_met_qc, orig_v_met_qc,                               &
                 nlevs_ob, obs_proj, map_proj, ob_vals, ob_qcvals,           &
                 n_start, niobf, vdim, nndgv, iprt)

  USE module_llxy
  USE module_fddaobs_utilities, ONLY: U,V,T,Q,Vr,Rf,P,H, missing_r, calc_Qv, &
                                      r2rhw, encode_rh_Qqc

!*************************************************************************
! Purpose: Store data and data qc into obs arrays.
!*************************************************************************
  IMPLICIT NONE

  INTEGER, intent(in) :: numsnd
  REAL, intent(in)    :: pressure_data(numsnd)
  REAL, intent(in)    :: height_data(numsnd)
  REAL, intent(in)    :: temperature_data(numsnd)
  REAL, intent(in)    :: u_met_data(numsnd)
  REAL, intent(in)    :: v_met_data(numsnd)
  REAL, intent(in)    :: rh_data(numsnd)
  REAL, intent(in)    :: pressure_qc(numsnd)
  REAL, intent(in)    :: height_qc(numsnd)
  REAL, intent(in)    :: temperature_qc(numsnd)
  REAL, intent(in)    :: u_met_qc(numsnd)
  REAL, intent(in)    :: v_met_qc(numsnd)
  REAL, intent(in)    :: rh_qc(numsnd)
  REAL, intent(in)    :: orig_pressure_data(numsnd)
  REAL, intent(in)    :: oblat(niobf)
  REAL, intent(in)    :: oblon(niobf)
  REAL, intent(in)    :: orig_u_met_qc(numsnd)
  REAL, intent(in)    :: orig_v_met_qc(numsnd)
  REAL, intent(in)    :: nlevs_ob(niobf)
  TYPE(PROJ_INFO),intent(in)    :: obs_proj   ! Structure for obs projection
  INTEGER,        intent(in)    :: map_proj   ! Map projection index
  REAL, intent(out)   :: ob_vals(nndgv,numsnd)    ! observational variables, multi-var
  REAL, intent(out)   :: ob_qcvals(nndgv,numsnd)  ! obs var quality control, multi-var
  INTEGER, intent(in) :: n_start        ! starting obs index for sounding
  INTEGER, intent(in) :: niobf          ! maximum number of observations
  INTEGER, intent(in) :: vdim           ! dim for vobs, vobs_qc
  INTEGER, intent(in) :: nndgv          ! number of nudge variables
  Real, intent(in)    :: elevob(niobf)  ! obs elevation
  LOGICAL, intent(in) :: iprt           ! print flag

! Local variables
  real :: r_data, r_qc            ! vapor pressure mixing ratio and qc
  character(len=200) :: msg       ! Argument to wrf_message

  integer :: n           ! ob index in ob window
  integer :: lev         ! loop counter over sounding levels
  integer :: iv          ! loop counter over nudge variables
  real :: rho            ! for r2rhw calc

! Initialize
  ob_vals = missing_r
  ob_qcvals = missing_r
  n = n_start

  DO lev = 1, nint(nlevs_ob(n_start))

!Store pressure
    if(pressure_qc(lev).ge. -0.01 .and.pressure_qc(lev).le.10.01               &
       .and. pressure_data(lev).gt. 0. )then
      ob_vals(P,lev)=pressure_data(lev) *1.e-3
      ob_qcvals(P,lev)=pressure_qc(lev)/10.
    elseif(pressure_qc(lev).ge.-0.01 .and. pressure_data(lev).le. 1.  &
!!! wuyh added 20160529
           .and. height_qc(lev).ge. -0.01 .and. height_data(lev) .gt. -500.) then
!!! wuyh added 20160529 end 
                                                     !yliu: good pressure_qc and missing pressure
                                                     !data, pressure will be computed from height

!yliu: if good pressure_qc and missing pressure, then data and press will be computed from height 
      ob_vals(P,lev)=missing_r
      ob_qcvals(P,lev)=1.        ! yliu: here we believe the recomputed press using height are good
    else
      ob_vals(P,lev)=missing_r
      ob_qcvals(P,lev)=missing_r
      IF (iprt) THEN
        if(ob_vals(H,lev).le.-888.) then
          write(msg,*) 'IN4DOB PROBLEM snd p and ht undefined',oblat(n),oblon(n)
          call wrf_message(msg)
        endif
      ENDIF
    endif

! Store obs height
    if(height_qc(lev).ge. -0.01 .and. height_data(lev) .gt. -500.)then
      ob_vals(H,lev)=height_data(lev)
      ob_qcvals(H,lev)=float(1)
    else
      ob_vals(H,lev)=missing_r
      ob_qcvals(H,lev) = missing_r
    endif

! Store temperature 
!    NOTE: The conversion to potential temperature, performed later in subroutine
!    errob, requires good pressure data, either directly or via good height data.
!    So here, in addition to checking for good temperature data,  we must also
!    do a check for good pressure or height.
    If (temperature_qc(lev).ge. -0.01) then
      if(pressure_qc(lev).ge. -0.01) then
        ob_vals(T,lev) = temperature_data(lev)
        ob_qcvals(T,lev)=temperature_qc(lev)/ 10.
      else
        ob_vals(T,lev) = missing_r
        ob_qcvals(T,lev)=missing_r
      endif
    Else
      ob_vals(T,lev)=missing_r
      ob_qcvals(T,lev)=missing_r
    Endif

!**************************************************************************
!!! ajb added 20140728 for wind turbine data and/or radial wind data.
!**************************************************************************

    if(orig_v_met_qc(lev).ge.500000. .and. orig_v_met_qc(lev).lt.599999.) then
!   If ob is wind turbine (500000 <= orig_v_met_qc value < 600000), need to:
!     1) Assign ob_vals(Vr) from u_met_data. This is (full) windspeed.
!     2) Assign ob_qcvals(Vr) a value of 500000. for identification as turbine data.
      do iv = 1, nndgv
        if(iv.ne.P .and. iv.ne.H) then
          ob_vals(iv,lev) = missing_r
          ob_qcvals(iv,lev) = missing_r
        endif
      enddo

! meixu 20170201 for turbine wind speed 
! Assign u_met_data (full wind speed) to ob_vals(U) and ob_vals(V)
! Encode ob_qcvals(U) and ob_qcvals(V) to a value of 500000-599999: 
!       for identification (leftest digit), retrieval of wind speed (4 middle digits) and qc value (last digit)
! Will partition turbine wind into U,V using model wind direction, & do U and V nudging in module_fddaobs_rtfdda.F
!	obs_val(U) will be used for U-component after the partition, wind speed will be retrieved from ob_qcvals(U)
!
      ob_vals(U,lev) = u_met_data(lev)
      ob_vals(V,lev) = v_met_data(lev)
! assmuing 0<wps<99 m/s, keep 2 digits after decimal
      ob_qcvals(U,lev) = orig_v_met_qc(lev) + int(u_met_data(lev) * 100.0) * 10 
      ob_qcvals(V,lev) = orig_v_met_qc(lev) + int(v_met_data(lev) * 100.0) * 10
! meixu 20170201 end

    elseif(orig_v_met_qc(lev).ge.599999. .and. orig_v_met_qc(lev).lt.700000.) then
!   If ob is radial wind  (600000 <= orig_v_met_qc value < 700000), we assign 
!        ob_vals(Vr) from v_met_data. This is the radial wind, i.e., the
!        component of windspeed in the direction of the radar scan.

      do iv = 1, nndgv
        if(iv.ne.P .and. iv.ne.H) then
          ob_vals(iv,lev) = missing_r
          ob_qcvals(iv,lev) = missing_r
        endif
      enddo
      ob_vals(Vr,lev) = v_met_data(lev)
      ob_qcvals(Vr,lev) = 600001.
      go to 658
    else 
      ob_vals(Vr,lev) = missing_r 
      ob_qcvals(Vr,lev) = missing_r 
!!! end ajb added 20140728 for radial winds and wind-turbines

! Store horizontal wind components - make sure at least 1 of the components is .ne.0
      if((u_met_qc(lev).ge. -0.01 .and.v_met_qc(lev) .ge. -0.01).and.         &
         (u_met_data(lev).ne. 0. .or. v_met_data(lev) .ne. 0.)) then

! If Earth-relative wind vector, need to rotate it to grid-relative coords (ajb 20110929)
        if(orig_u_met_qc(lev).ge.700000. .and. orig_v_met_qc(lev).ge.700000.) then
          CALL rotate_vector(oblon(n),u_met_data(lev),v_met_data(lev),   &
                             obs_proj,map_proj)
        endif

        ob_vals(U,lev)=u_met_data(lev)
        ob_qcvals(U,lev)=u_met_qc(lev) / 10.
        ob_vals(V,lev)=v_met_data(lev)
        ob_qcvals(V,lev)=v_met_qc(lev) / 10.
      else

        ob_vals(U,lev)=missing_r
        ob_qcvals(U,lev)=missing_r
        ob_vals(V,lev)=missing_r
        ob_qcvals(V,lev)=missing_r
      endif
    endif
!!! wuyh modified 20160605
    if(rh_data(lev) .le. -800000. .AND. rh_qc(lev) .le. -800000.) then
      ob_vals(Q,lev) = missing_r
      ob_qcvals(Q,lev) = missing_r
    elseif(rh_data(lev) .ge. 0. .AND. rh_data(lev) .le. 102.1) then !good RH
      if(temperature_data(lev) .gt. 100. .and. temperature_data(lev) .lt. 400.) then  !good T
        if(pressure_data(lev) .gt. 5000. .and. pressure_data(lev) .lt. 105000.) then  !good P
          ob_vals(Q,lev) = 200.+rh_data(lev)
          CALL calc_Qv(ob_vals(Q,lev),temperature_data(lev), ob_vals(P,lev)*10.,ob_vals(Q,lev),rho)
          ob_qcvals(Q,lev) = encode_rh_Qqc(rh_data(lev),rh_qc(lev))
        elseif(pressure_qc(lev) .ge. -0.001 .AND. ob_vals(H,lev) .gt. -500.)then  ! good T and H
          ob_qcvals(Q,lev) = float(INT(ob_vals(T,lev))*100)     
        else
          ob_vals(Q,lev)   = -888887.
          ob_qcvals(Q,lev) = -888887.
        endif
      else  ! good RH, No T, so set vapor pressure mixing ratio to missing.
        ob_vals(Q,lev)   = -888886.
        ob_qcvals(Q,lev) = -888886.
      endif
    elseif(rh_qc(lev) .gt. 777000.)then    ! Td
      ob_vals(Q,lev) = 2000. + rh_qc(lev) - 777000.
      if(ABS(ob_vals(T,lev) - 225.) .le. 125) then    ! good T
        if(ob_qcvals(P,lev) .gt. -0.001)then          ! good P
          CALL calc_Qv(ob_vals(Q,lev),temperature_data(lev),ob_vals(P,lev)*10.,ob_vals(Q,lev),rho)
          ob_qcvals(Q,lev) = encode_rh_Qqc(rho,1.)
        elseif(pressure_qc(lev) .ge. -0.001 .AND. ob_vals(H,lev) .gt. -500.)then ! good H
           ob_qcvals(Q,lev) = float(INT(ob_vals(T,lev))*100)
        else   ! Bad P and H
           ob_vals(Q,lev)   = -888885.
           ob_qcvals(Q,lev) = -888885.
        endif
      endif
    else
        ob_vals(Q,lev) = missing_r
        ob_qcvals(Q,lev) = missing_r
    endif
!!! wuyh modified 20160605 end

!!! wuyh added 20131117
    if((ob_vals(P,lev).lt.0.).and.(ob_vals(H,lev).le.-500.)) then
        ob_vals(U,lev)=missing_r
        ob_qcvals(U,lev)=missing_r
        ob_vals(V,lev)=missing_r
        ob_qcvals(V,lev)=missing_r
        ob_vals(T,lev)=missing_r
        ob_qcvals(T,lev)=missing_r
        ob_vals(Q,lev)=missing_r
        ob_qcvals(Q,lev)=missing_r
    endif
!!! wuyh added 20131117

! Set Rf to missing values
    ob_vals(Rf,lev) = missing_r
    ob_qcvals(Rf,lev) = missing_r

!!! wuyh added 20150508
658 Continue
!!! wuyh added 20150508

    n = n+1
  ENDDO

  Return
  END SUBROUTINE store_sndg_intqc

  SUBROUTINE store_surf_intqc(slp_data, psfc_data, height_data, temperature_data,  &
                              u_met_data,    v_met_data,  rh_data,                 &
                              slp_qc, psfc_qc,   height_qc,   temperature_qc,      &
                              u_met_qc,      v_met_qc,    rh_qc,                   &
                              latitude, longitude, elevation, plfo, rovg,          &
                              orig_u_met_qc, orig_v_met_qc,                        &
                              obs_proj, map_proj,                                  &
                              n, niobf, nndgv, ob_vals, ob_qcvals, iprt,           &
                              terrmod, ob_hto, near_surf, rko)

  USE module_llxy
  USE module_fddaobs_utilities, ONLY: U,V,T,Q,Vr,Rf,P,H, missing_r, ht_to_psur,    &
                                      calc_Qv, r2rhw, encode_rh_Qqc

!*************************************************************************
! Purpose: Store data and data qc into obs arrays.
!*************************************************************************
  IMPLICIT NONE

  REAL, intent(in)    :: slp_data
  REAL, intent(inout) :: psfc_data
  REAL, intent(in)    :: height_data
  REAL, intent(in)    :: temperature_data
  REAL, intent(in)    :: u_met_data
  REAL, intent(in)    :: v_met_data
  REAL, intent(inout) :: rh_data
  REAL, intent(inout) :: slp_qc
  REAL, intent(inout) :: psfc_qc
  REAL, intent(inout) :: height_qc
  REAL, intent(inout) :: temperature_qc
  REAL, intent(inout) :: u_met_qc
  REAL, intent(inout) :: v_met_qc
  REAL, intent(inout) :: rh_qc
  REAL, intent(in)    :: latitude
  REAL, intent(in)    :: longitude
  REAL, intent(in)    :: elevation
  REAL, intent(in)    :: plfo
  REAL, intent(in)    :: rovg                 ! constant rho over g
  REAL, intent(in)    :: orig_u_met_qc
  REAL, intent(in)    :: orig_v_met_qc
  TYPE(PROJ_INFO),intent(in)    :: obs_proj   ! Structure for obs projection
  INTEGER,        intent(in)    :: map_proj   ! Map projection index
  INTEGER, intent(in) :: n              ! obs index
  INTEGER, intent(in) :: niobf          ! maximum number of observations
  INTEGER, intent(in) :: nndgv          ! number of nudge variables
  REAL, intent(out)   :: ob_vals(nndgv)    ! observational variables, multi-var
  REAL, intent(out)   :: ob_qcvals(nndgv)  ! obs var quality control, multi-var
  LOGICAL, intent(in) :: iprt           ! print flag
  LOGICAL,intent(in) :: near_surf
  REAL, intent(in)   :: terrmod
  REAL, intent(in)   :: ob_hto
  REAL, intent(in)   :: rko(niobf)

! Local variables
  real :: r_data, r_qc            ! vapor pressure mixing ratio and qc
  real :: tbar
  real :: rho                     ! for r2rhw calc
  character(len=200) :: msg       ! Argument to wrf_message

! Initialize
  ob_vals = missing_r
  ob_qcvals = missing_r

! Store temperature for WRF
!!! wuyh modified 20160605
  if (ABS(temperature_data - 225.) .le. 125. .AND. ABS(temperature_qc - 5.) .le. 5.1) then
!!! wuyh modified end 20160605
    ob_vals(T) = temperature_data
    ob_qcvals(T)=temperature_qc/10.
  else
    ob_vals(T)=missing_r
    ob_qcvals(T)=missing_r
  endif

! Store pressure
  if(psfc_data.gt.60000. .and.psfc_data.lt.105000.) then
    ob_vals(P)=psfc_data *1.e-3
    ob_qcvals(P)=1.                        !psfc_qc/10.
! calculate psfc if slp is there
  elseif(temperature_qc.ge. -0.01 .and. temperature_qc.le.150. .and.     &
          slp_data.gt.90000.) then
    tbar=temperature_data+0.5*elevation*.0065
    psfc_data=slp_data*exp(-elevation/(rovg*tbar))
    ob_vals(P)=psfc_data*1.e-3
    psfc_qc=4.
    ob_qcvals(P)=psfc_qc / 10.                           !psfc_qc/ 10.0
  else
    ob_vals(P)=missing_r
    ob_qcvals(P)=missing_r
  endif

  if(height_qc .ge. -0.01 .and. height_data .gt. -500.)then
    ob_vals(H) = height_data
    ob_qcvals(H) = height_qc/10.
  else
    ob_vals(H)=missing_r
    ob_qcvals(H)=missing_r
  endif

! Store horizontal wind components - make sure at least 1 of the components is .ne.0
  if((u_met_qc.ge. -0.01 .and. v_met_qc.ge. -0.01).and.               &
     (u_met_data.ge.-888. .or. v_met_data.ge. -888.)) then

! If Earth-relative wind vector, need to rotate it to grid-relative coords (ajb 20110929)
    if(orig_u_met_qc.ge.700000. .and. orig_v_met_qc.ge.700000.) then
      CALL rotate_vector(longitude,u_met_data,v_met_data,   &
                         obs_proj,map_proj)
    endif
    ob_vals(U)=u_met_data
    ob_qcvals(U)=u_met_qc / 10.
    ob_vals(V)=v_met_data
    ob_qcvals(V)=v_met_qc / 10.
  else
    ob_vals(U)=missing_r
    ob_qcvals(U)=missing_r
    ob_vals(V)=missing_r
    ob_qcvals(V)=missing_r
  endif

!c *No* **Very rough** estimate of psfc from sfc elevation if UUtah ob and elev>1000m
! estimate psfc from temp and elevation
!   Do not know sfc pressure in model at this point.
!      if((psfc_qc.lt.0.).and.(elevation.gt.1000.).and.
!     1   (temperature_qc.ge.0..and.temperature_qc.le.10.)
!     1    .and.(platform(7:16).eq.'SYNOP PRET'))then
  if((psfc_qc.lt.0.).and.                                           &
    (temperature_qc.ge. -0.001 .and.temperature_qc.le.10.1)) then
    tbar=temperature_data+0.5*elevation*.0065
    psfc_data=100000.*exp(-elevation/(rovg*tbar))
    ob_vals(P)=missing_r
    ob_qcvals(P)=missing_r
  endif

  if( near_surf .and. ob_vals(H) .gt. -500. .and. ob_vals(H) .le. 6000.)then
!!! wuyh modified 20160605
      if(ob_vals(P) .gt. 50. .AND. ABS(ob_vals(T) - 225.) .le. 125.) then
!!! wuyh modified end 20160605
        ob_vals(P) = ht_to_psur(ob_vals(P),ob_vals(T),elevation,ob_vals(H))
      else
        ob_vals(P)= missing_r
      endif
      ob_qcvals(P)= 1.
  endif

! if a ship ob has rh<70%, then throw out
  if(plfo.eq.3..and.rh_qc.ge. -0.01 .and.rh_qc.le.10.1 .and.rh_data.lt.70.) then
    rh_qc=missing_r
    rh_data=missing_r
  endif

!!!! wuyh modified 20160605
  if(rh_data .le. -800000. .AND. rh_qc .le. -800000.) then    !bad
    ob_vals(Q) = missing_r
    ob_qcvals(Q) = missing_r
  elseif(rh_data .ge. 0. .AND. rh_data .le. 102.1) then       !good RH       
    iF(ABS(ob_vals(T)-225.) .le. 125.)then                    !good T
      ob_vals(Q) = 200.+ rh_data   
      If(ob_vals(P) .gt. 5. .and. ob_vals(P) .lt. 110.)then   !good P
        CALL calc_Qv(ob_vals(Q),ob_vals(T), ob_vals(P)*10.,ob_vals(Q),rho)
        ob_qcvals(Q) = encode_rh_Qqc(rh_data,rh_qc)
      Elseif(ob_qcvals(P) .ge. 0.01 .AND. ob_vals(H) .gt. -500.)then ! good H
          ob_qcvals(Q) = float(INT(ob_vals(T))*100)
      Else  ! No H and P
          ob_vals(Q)   = -888887.
          ob_qcvals(Q) = -888887.
      Endif
    eLSE ! We have rh, but no T, so set vapor pressure mixing ratio to missing.
      ob_vals(Q)   = -888886.
      ob_qcvals(Q) = -888886.
    eNDIF
  elseif(rh_qc .gt. 777000.)then                                ! good Td
      ob_vals(Q) = 2000. + rh_qc - 777000.
      iF(ABS(ob_vals(T) - 225.) .le. 125) then                  ! good T
        If(ob_vals(P) .gt. 5. .and. ob_vals(P) .lt. 110.)then   ! good P
           CALL calc_Qv(ob_vals(Q),ob_vals(T),ob_vals(P)*10.,ob_vals(Q),rho)
           ob_qcvals(Q) = encode_rh_Qqc(rho,1.)
        Elseif(ob_qcvals(P) .ge. 0.01 .AND. ob_vals(H) .gt. -500.)then ! good H
           ob_qcvals(Q) = float(INT(ob_vals(T))*100)
        Else  !No H and P
           ob_vals(Q)   = -888885.
           ob_qcvals(Q) = -888885.
        Endif
!!! wuyh added end 20160605
      eLSE  !! No T
        ob_vals(Q)   = -888884.
        ob_qcvals(Q) = -888884.
      eNDIF
!!!! wuyh modified end 20160605
  else
      ob_vals(Q) = missing_r
      ob_qcvals(Q) = missing_r
  endif

!!! wuyh removed 20160605  
!! Set Vr to missing value
!  ob_vals(Vr)   = missing_r
!  ob_qcvals(Vr) = missing_r
!  ob_vals(Rf)   = missing_r
!  ob_qcvals(Rf) = missing_r
!!! wuyh removed 20160605

  END SUBROUTINE store_surf_intqc

  SUBROUTINE split_ob_vars(fdob, niobf, vdim, nndgv, is_sound, numsnd,      &
                           ob_var_type, ob_ndg_type, meas_count,            &
                           nlevs_ob, lev_in_ob, ob_vals, ob_qcvals,         &
                           vobs, vobs_qc, timeob,                           &
                           missing_r, num_ndg_terms, ndg_term_indxs,        &
                           newpass,inest,n,latitude,longitude,              &
                           nlast,nprev, prt_max,prt_freq,iprt,              &
                           station_id,stnid_prt, rio,rjo,xlat,xlong,        &
                           obs_prt, lat_prt,lon_prt, mlat_prt,mlon_prt,     &
                           e_we,e_sn,ims,ime,jms,jme,its,ite,jts,jte) 
!------------------------------------------------------------------------------
! PURPOSE: Split observation into separate observations, by variable
!------------------------------------------------------------------------------

  USE module_domain, ONLY : fdob_type
  USE module_fddaobs_utilities, ONLY: numndgv, P, H, MNP, sndg_info, &
                                      get_var_pair_indxs

  IMPLICIT NONE

  TYPE(fdob_type), intent(inout)  :: fdob  ! Obs structure
  INTEGER, intent(in)    :: niobf          ! Maximum number of observations
  INTEGER, intent(in)    :: vdim           ! Dim for vobs, vobs_qc
  INTEGER, intent(in)    :: nndgv          ! Number of nudge variables + 2
  LOGICAL, intent(in)    :: is_sound       ! Flag indicating sounding ob
  INTEGER, intent(in)    :: numsnd         ! Max number of obs in sounding
  INTEGER, intent(inout) :: ob_var_type(niobf)  ! Variable type of obs (U=1, V=2, etc)
  INTEGER, intent(inout) :: ob_ndg_type(niobf)  ! Nudge-term type (# term in ndg_term_indxs)
  INTEGER, intent(in)    :: meas_count     ! Levels in sounding
  REAL,    intent(inout) :: nlevs_ob(niobf)   ! numbers of levels in sounding obs
  REAL,    intent(inout) :: lev_in_ob(niobf)  ! level in sounding-type obs
  REAL,    intent(in)    :: ob_vals(nndgv,numsnd)    ! ob variables, from multi-var ob
  REAL,    intent(in)    :: ob_qcvals(nndgv,numsnd)  ! ob quality control, from multi-var ob
  REAL,    intent(out)   :: vobs(vdim,niobf)            ! observational variables, 1-ob, 1-var
  REAL,    intent(out)   :: vobs_qc(vdim,niobf)         ! obs var quality control, 1-ob, 1-var
  REAL,    intent(out)   :: timeob(niobf)  ! model times for each observation (hours)
  REAL,    intent(in)    :: missing_r      ! Value for missing data
  INTEGER, intent(in)    :: num_ndg_terms         ! number of nudging terms
  INTEGER, intent(in)    :: ndg_term_indxs(MNP,2) ! indices for nudging terms
  LOGICAL, intent(inout) :: newpass        ! New pass flag
  INTEGER, intent(in)    :: inest          ! nest index
  INTEGER, intent(inout) :: n              ! Observation index
  REAL,    intent(in)    :: latitude       ! Latitude of obs
  REAL,    intent(in)    :: longitude      ! Latitude of obs
  INTEGER, intent(in)    :: nlast          ! Last obs of valid obs, prev window
  INTEGER, intent(inout) :: nprev          ! Previous obs in new window read seq
  INTEGER, intent(in)    :: prt_max        ! Max no. of obs for diagnostic printout
  INTEGER, intent(in)    :: prt_freq       ! Frequency for diagnostic printout
  LOGICAL, intent(in)    :: iprt           ! print flag
  CHARACTER*15, intent(in) :: station_id   ! First 15 chars of station id for obs n
  INTEGER, intent(inout) :: stnid_prt(40,prt_max) ! Station ids for diagnostic printout
  REAL,    intent(in)    :: rio(niobf)     ! West-east coord of ob, non-staggered grid
  REAL,    intent(in)    :: rjo(niobf)     ! South-north coord of ob, non-staggered grid
  REAL, DIMENSION( ims:ime, jms:jme ),                                   &
           intent(in )   :: xlat, xlong    ! Lat/lon on mass-pt grid
  INTEGER, intent(inout) :: obs_prt(prt_max) ! Obs index for printout
  REAL,    intent(inout) :: lat_prt(prt_max) ! Obs latitude for printout
  REAL,    intent(inout) :: lon_prt(prt_max) ! Obs longitude for printout
  REAL,    intent(inout) :: mlat_prt(prt_max) ! Model latitude at (rio,rjo) for printout
  REAL,    intent(inout) :: mlon_prt(prt_max) ! Model longitude at (rio,rjo) for printout
  INTEGER, intent(in)    :: e_we           ! Max grid index in south-north
  INTEGER, intent(in)    :: e_sn           ! Max grid index in west-east
  INTEGER, intent(in)    :: ims            ! Grid mem start (west-east)
  INTEGER, intent(in)    :: ime            ! Grid mem end   (west-east)
  INTEGER, intent(in)    :: jms            ! Grid mem start (south-north)
  INTEGER, intent(in)    :: jme            ! Grid mem end   (south-north)
  INTEGER, intent(in)    :: its            ! Grid tile start (west-east)
  INTEGER, intent(in)    :: ite            ! Grid tile end   (west-east)
  INTEGER, intent(in)    :: jts            ! Grid tile start (south-north)
  INTEGER, intent(in)    :: jte            ! Grid tile end   (south-north)

! Local variables
  integer :: ndup      ! number of surface ob or sounding duplications needed
  integer :: i, nn, nbase, nbasei, njc, njcc, ivar
  integer :: idup      ! secondary counter for duplicating soundings
  integer :: nlevs     ! sounding levels, for duplicating soundings
 
! for subroutine
  integer :: ob_split(numndgv)   ! variables present in ob (1 if present, 0 otherwise)
  integer :: first               ! first variable in ob with good data
  integer :: icount              ! count of requested nudging pairs for a given variable
  integer :: indxs(MNP)          ! indices  
  integer :: ii,il,ilev          ! loop counters

  TYPE(sndg_info) :: save_sndg   ! temp to hold sounding information

! Initialize
  ob_split(:) = 0
  ndup = 0
  first = 0

  if(.not.is_sound) then     ! Handle surface ob

! Check for ob types (U,V,T,Q,Vr) with good data
    do i = numndgv, 1, -1
!!! wuyh added 20160610
!      if(ob_qcvals(i,1).ge.0.) then
      if( ob_qcvals(i,1) .ge. -0.0001 .AND. ABS(ob_vals(i,1)) .le. 4000.) then
!!! wuyh added 20160610 end
         ndup=ndup+1
         ob_split(i) = 1
         first = i
      endif
    enddo

! Duplicate observation
    if(ndup.eq.0) then

      timeob(n) = 99999. 
      n = n-1      ! delete observation if it contains no good data

    else

      do i = first, numndgv   ! Loop over nudge variables

!       Except for first var present, copy ob information 
        if(ob_split(i).eq.1) then

          if(i.ne.first) then

            n = n + 1
            if(n.gt.niobf) CALL stop_122(n,niobf,iprt)

            ob_var_type(n) = i
            CALL store_ob(nndgv,vdim,i,P,H,ob_vals(:,1),ob_qcvals(:,1),vobs(:,n),vobs_qc(:,n))
            CALL copy_info_for_ob(n, n-1, 1, fdob)
!!! wuyh modified 20150525
!!!            CALL get_var_pair_indxs(i, MNP, ndg_term_indxs(1,1), num_ndg_terms,icount, indxs)
            CALL get_var_pair_indxs(i, MNP, ndg_term_indxs(1,2), num_ndg_terms,icount, indxs)
!!! wuyh modified 20150525
            CALL dup_ob(i, n, MNP, icount, indxs, ndg_term_indxs, fdob)
          else
            ob_var_type(n) = first
            CALL store_ob(nndgv,vdim,first,P,H,ob_vals(:,1),ob_qcvals(:,1),vobs(:,n),vobs_qc(:,n))
            CALL get_var_pair_indxs(first, MNP, ndg_term_indxs(1,1), num_ndg_terms, icount, indxs)
            CALL dup_ob(first, n, MNP, icount, indxs, ndg_term_indxs, fdob)
          endif

          CALL collect_obs_info(newpass,inest,n,latitude,longitude,          &
                                nlast,nprev,niobf,prt_max,prt_freq,          &
                                station_id,stnid_prt,rio,rjo,xlat,xlong,     &
                                obs_prt,lat_prt,lon_prt,mlat_prt,mlon_prt,   &
                                e_we,e_sn,ims,ime,jms,jme,its,ite,jts,jte)
        endif

      enddo

    endif

  else   ! Handle sounding

!   Set ob index to last ob before sounding
    nn = n - meas_count
    nbase = nn

! Check for ob types (U,V,T,Q,Vr) with good data
    do njcc = 1, meas_count
      do i = 1, numndgv
        if(ob_qcvals(i,njcc).ge.0.) ob_split(i) = 1
      enddo
    enddo

    CALL make_sndg_backup(meas_count, nn+1, fdob, save_sndg)

!!! wuyh modified 20150515
!!    do i = 1, numndgv    ! Loop over nudge variables
    do i = 1, num_ndg_terms
!!      write(267,*) "i=",i
!!! wuyh modified 20150515

!!! wuyh removed 20150525
!     Check to see if we need this variable type
!!      CALL get_var_pair_indxs(i, MNP, ndg_term_indxs(1,1), num_ndg_terms, icount, indxs)
!!      if(icount.ge.1) then
!!! wuyh removed 20150525 end

!       Keep only levels with good data 
        il = 0
        nbasei = nn
        do ilev = 1, meas_count
!!! wuyh modified 20150515
!!          if(ob_qcvals(i,ilev).ge.0.) then
          if(ob_qcvals(ndg_term_indxs(i,1),ilev).ge.0.) then
!!! uwyh modified 20150515

            nn = nn + 1    ! increment ob index
            il = il + 1    ! increment sounding level index
            lev_in_ob(nn) = int(il)

            CALL retrieve_sndg_level(nn, ilev, save_sndg, fdob)
!!! wuyh modified 20150515
!!            ob_var_type(nn) = i 
!!            ob_ndg_type(nn) = indxs(1)
            ob_var_type(nn) = ndg_term_indxs(i,1)
            ob_ndg_type(nn) = ndg_term_indxs(i,2)
!!            vobs(1,nn) = ob_vals(i,ilev)
            vobs(1,nn) = ob_vals(ndg_term_indxs(i,1),ilev)
            vobs(2,nn) = ob_vals(P,ilev)
            vobs(3,nn) = ob_vals(H,ilev)
            vobs_qc(1,nn) = ob_qcvals(ndg_term_indxs(i,1),ilev)
            vobs_qc(2,nn) = ob_qcvals(P,ilev)
            vobs_qc(3,nn) = ob_qcvals(H,ilev)

            fdob%lev_in_ob(nn) = il

          endif

        enddo 

!       Now set nlevs_ob for each level that was kept
        do ilev = 1, il 
          nlevs_ob(nbasei+ilev) = lev_in_ob(nbasei+il) 
        enddo

!!! wuyh removed 20150515
!       Make requested duplicates
!!        do idup = 2, icount
!!          nlevs = int(nlevs_ob(nbasei+1))
!!          CALL copy_info_for_ob(nn+1, nbasei+1, nlevs, fdob)

!         Assign copy correct ob_ndg_type
!!          do ilev = nn+1, nn+nlevs
!!            ob_var_type(ilev) = ob_var_type(idup)
!!            ob_ndg_type(ilev) = indxs(idup)
!!          enddo
!!          nn = nn + nlevs_ob(nbasei+1)
!!        enddo
!!! wuyh removed 20150515

! Assign ob index
        n = nn

        CALL collect_obs_info(newpass,inest,n,latitude,longitude,          &
                              nlast,nprev,niobf,prt_max,prt_freq,          &
                              station_id,stnid_prt,rio,rjo,xlat,xlong,     &
                              obs_prt,lat_prt,lon_prt,mlat_prt,mlon_prt,   &
                              e_we,e_sn,ims,ime,jms,jme,its,ite,jts,jte)
!!! wuyh removed 20150515
!!      endif
!!! wuyh removed 20150515
    enddo

!write(6,*) 'SUMMARY: nn = ',nn
!do i = 1,nn
!write(6,'(a,i3,2x,3f12.3,2(a,f4.1))') 'Ob = ',i,                    & 
!           vobs(1,i),vobs(2,i),vobs(3,i)
!           '   nlev = ',lev_in_ob(i),'   tot_levs = ',nlevs_ob(i)
!enddo

! Reset timeob if there is no good sounding data
    if(n.eq.nbase) then
      do ilev = 1, meas_count
        timeob(nbase+ilev) = 99999.
      enddo
    endif

  endif ! if surface ob

  END SUBROUTINE split_ob_vars

  SUBROUTINE copy_info_for_ob(n, m, meas_count, fdob)
!------------------------------------------------------------------------------
!  PURPOSE: Copy ob information (in fdob structure) for ob n from ob m.
!------------------------------------------------------------------------------
  USE module_domain
  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: n             ! array index to copy into
  INTEGER, INTENT(IN)  :: m             ! array index to copy from
  INTEGER, INTENT(IN)  :: meas_count    ! number of levels
  TYPE(fdob_type), intent(inout)  :: fdob  ! Obs structure

! Local variables
  integer :: i,nn,mm    ! loop counters
  
  nn = n-1
  mm = m-1
  do i = 1, meas_count
   nn = nn+1
   mm = mm+1
   fdob%rio(nn)          = fdob%rio(mm)
   fdob%rjo(nn)          = fdob%rjo(mm)
   fdob%rko(nn)          = fdob%rko(mm)
   fdob%timeob(nn)       = fdob%timeob(mm)
   fdob%plfo(nn)         = fdob%plfo(mm)
   fdob%rkmob(nn)       = fdob%rkmob(mm)
   fdob%wudate(:,nn)     = fdob%wudate(:,mm)
   fdob%wusource(:,nn)   = fdob%wusource(:,mm)
   fdob%platform_ob(:,nn)= fdob%platform_ob(:,mm)
   fdob%elevob(nn)       = fdob%elevob(mm)
   fdob%lev_in_ob(nn)    = fdob%lev_in_ob(mm)
   fdob%nlevs_ob(nn)     = fdob%nlevs_ob(mm)
   fdob%ob_is_kalman(nn) = fdob%ob_is_kalman(mm)
   fdob%stnid(:,nn)      = fdob%stnid(:,mm)
   fdob%desobs(:,nn)     = fdob%desobs(:,mm)
   fdob%flagsnd(nn)      = fdob%flagsnd(mm)
   fdob%flagbog(nn)      = fdob%flagbog(mm)
   fdob%oblat(nn)        = fdob%oblat(mm)
   fdob%oblon(nn)        = fdob%oblon(mm)
   fdob%qcu(nn)          = fdob%qcu(mm)
!!   fdob%qcv(nn)          = fdob%qcv(mm)
!!   fdob%qct(nn)          = fdob%qct(mm)
!!   fdob%qcq(nn)          = fdob%qcq(mm)
   fdob%gainname(nn)     = fdob%gainname(mm)
   fdob%ob_var_type(nn)  = fdob%ob_var_type(mm)
  enddo

  END SUBROUTINE copy_info_for_ob

  SUBROUTINE store_ob(nndgv, vdim, iv, P, H, ob_vals, ob_qcvals, vobs, vobs_qc )
!------------------------------------------------------------------------------
!  PURPOSE: Store ob_vals and ob_qcvals into vobs and vobs_qc, respectively.  
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, intent(in)    :: nndgv
  INTEGER, intent(in)    :: vdim
  INTEGER, intent(in)    :: iv                       ! variable value(s) to copy
  INTEGER, intent(in)    :: P                        ! pressure index into ob_vals
  INTEGER, intent(in)    :: H                        ! height index into ob_vals
  REAL,    intent(in)    :: ob_vals(nndgv)           ! ob variables, from multi-var ob
  REAL,    intent(in)    :: ob_qcvals(nndgv)         ! ob quality control, from multi-var ob
  REAL,    intent(out)   :: vobs(vdim)               ! observational variables, 1-ob, 1-var
  REAL,    intent(out)   :: vobs_qc(vdim)            ! obs var quality control, 1-ob, 1-var

  if(vdim.ne.3) then
    CALL abort('store_ob: VDIM is not equal to 3!')
  else
    vobs(1) = ob_vals(iv)
    vobs(2) = ob_vals(P)
    vobs(3) = ob_vals(H)
    vobs_qc(1) = ob_qcvals(iv)
    vobs_qc(2) = ob_qcvals(P)
    vobs_qc(3) = ob_qcvals(H)
  endif

  END SUBROUTINE store_ob

  SUBROUTINE make_sndg_backup(nlevs, n, fdob, save_sndg)
!------------------------------------------------------------------------------
!  PURPOSE: Store sounding information into temp space
!------------------------------------------------------------------------------
  USE module_domain
  USE module_fddaobs_utilities, ONLY: sndg_info
  IMPLICIT NONE

  INTEGER,         intent(in)  :: nlevs      ! Number of levels in sounding
  INTEGER,         intent(in)  :: n          ! Starting ob index for sounding
  TYPE(fdob_type), intent(in)  :: fdob       ! Obs structure
  TYPE(sndg_info), intent(out) :: save_sndg  ! Sounding structure

! Local variables
  integer :: i  ! loop counter
  integer :: m  ! ending ob index for sounding

  m = n + nlevs - 1 

  save_sndg%rio(1:nlevs)           = fdob%rio(n:m)
  save_sndg%rjo(1:nlevs)           = fdob%rjo(n:m)
  save_sndg%rko(1:nlevs)           = fdob%rko(n:m)
  save_sndg%timeob(1:nlevs)        = fdob%timeob(n:m)
  save_sndg%plfo(1:nlevs)          = fdob%plfo(n:m)
  save_sndg%rkmob(1:nlevs)         = fdob%rkmob(n:m)
  save_sndg%wudate(:,1:nlevs)      = fdob%wudate(:,n:m)
  save_sndg%wusource(:,1:nlevs)    = fdob%wusource(:,n:m)
  save_sndg%platform_ob(:,1:nlevs) = fdob%platform_ob(:,n:m)
  save_sndg%elevob(1:nlevs)        = fdob%elevob(n:m)
  save_sndg%lev_in_ob(1:nlevs)     = fdob%lev_in_ob(n:m)
  save_sndg%nlevs_ob(1:nlevs)      = fdob%nlevs_ob(n:m)
  save_sndg%ob_is_kalman(1:nlevs)  = fdob%ob_is_kalman(n:m)
  save_sndg%stnid(:,1:nlevs)       = fdob%stnid(:,n:m)
  save_sndg%desobs(:,1:nlevs)      = fdob%desobs(:,n:m)
  save_sndg%flagsnd(1:nlevs)       = fdob%flagsnd(n:m)
  save_sndg%flagbog(1:nlevs)       = fdob%flagbog(n:m)
  save_sndg%oblat(1:nlevs)         = fdob%oblat(n:m)
  save_sndg%oblon(1:nlevs)         = fdob%oblon(n:m)
!!save_sndg%qcu(1:nlevs)           = fdob%qcu(n:m)
!!save_sndg%qcv(1:nlevs)           = fdob%qcv(n:m)
!!save_sndg%qct(1:nlevs)           = fdob%qct(n:m)
!!save_sndg%qcq(1:nlevs)           = fdob%qcq(n:m)
  save_sndg%gainname(1:nlevs)      = fdob%gainname(n:m)

  END SUBROUTINE make_sndg_backup

  SUBROUTINE retrieve_sndg_level(n, m, save_sndg, fdob)
!------------------------------------------------------------------------------
!  PURPOSE: Copy sounding information from level m of sounding backup, into
!           index n of fdob.
!------------------------------------------------------------------------------
  USE module_domain
  USE module_fddaobs_utilities, ONLY: sndg_info
  IMPLICIT NONE

  INTEGER,         intent(in)  :: n          ! ob index for target fdob
  INTEGER,         intent(in)  :: m          ! ob index for source save_sndg
  TYPE(sndg_info), intent(in)  :: save_sndg  ! Sounding structure
  TYPE(fdob_type), intent(out) :: fdob       ! Obs structure

  fdob%rio(n)           = save_sndg%rio(m)
  fdob%rjo(n)           = save_sndg%rjo(m)
  fdob%rko(n)           = save_sndg%rko(m)
  fdob%timeob(n)        = save_sndg%timeob(m)
  fdob%plfo(n)          = save_sndg%plfo(m)
  fdob%rkmob(n)        = save_sndg%rkmob(m)
  fdob%wudate(:,n)      = save_sndg%wudate(:,m)
  fdob%wusource(:,n)    = save_sndg%wusource(:,m)
  fdob%platform_ob(:,n) = save_sndg%platform_ob(:,m)
  fdob%elevob(n)        = save_sndg%elevob(m)
  fdob%ob_is_kalman(n)  = save_sndg%ob_is_kalman(m)
  fdob%stnid(:,n)       = save_sndg%stnid(:,m)
  fdob%desobs(:,n)      = save_sndg%desobs(:,m)
  fdob%flagsnd(n)       = save_sndg%flagsnd(m)
  fdob%flagbog(n)       = save_sndg%flagbog(m)
  fdob%oblat(n)         = save_sndg%oblat(m)
  fdob%oblon(n)         = save_sndg%oblon(m)
  fdob%qcu(n)           = save_sndg%qcu(m)
!!  fdob%qcv(n)           = save_sndg%qcv(m)
!!  fdob%qct(n)           = save_sndg%qct(m)
!!  fdob%qcq(n)           = save_sndg%qcq(m)
  fdob%gainname(n)      = save_sndg%gainname(m)

  END SUBROUTINE retrieve_sndg_level

  SUBROUTINE dup_ob(ivar, n, MNP, icount, indxs, ndg_term_indxs, fdob)
!------------------------------------------------------------------------------
!  PURPOSE: Make duplicate observations based on nudging terms requested.
!           NOTE: Before calling this subroutine, subroutine get_var_pair_indxs
!                 needs to be called for proper setting of the array indxs, for
!                 variable ivar.
!------------------------------------------------------------------------------
  USE module_domain
  IMPLICIT NONE

  INTEGER,INTENT(IN)    :: ivar            ! nudge variable index
  INTEGER,INTENT(INOUT) :: n               ! current observation index
  INTEGER,INTENT(IN)    :: MNP             ! max nudging terms
  INTEGER,INTENT(IN)    :: icount          ! number occurences of value ivar in ivar_list
  INTEGER,INTENT(IN)    :: indxs(MNP)      ! values of i for which ivar_list(i)=ivar
  INTEGER,INTENT(IN)    :: ndg_term_indxs(MNP,2) ! indices for nudging terms
  TYPE(fdob_type), intent(inout)  :: fdob  ! Obs structure

! Local variables
  integer :: i   ! loop counter for count

  if(icount.lt.0) then
    CALL abort('dup_ob: Count should be non-negative!')

! Eliminate ob if not needed
  elseif(icount.eq.0) then
    n = n-1

  else

    do i = 1, icount
      if(i.eq.1) then
        fdob%ob_var_type(n) = ivar
        fdob%ob_ndg_type(n) = ndg_term_indxs(indxs(1),2)
      else
        n = n+1
        CALL copy_info_for_ob(n, n-1, 1, fdob)
        fdob%ob_ndg_type(n) = ndg_term_indxs(indxs(1),2)
      endif

!write(6,'(a,2i3,2x,3f12.3)') 'dup: ',n,fdob%ob_var_type(n),       &
!                    fdob%vars(1,n),fdob%vobs(2,n),fdob%vobs(3,n)
    enddo
! Set flag and make duplicate obs
  endif

  END SUBROUTINE dup_ob

#endif
!-----------------------------------------------------------------------
! End subroutines for in4dob
!-----------------------------------------------------------------------

#if ( EM_CORE == 1 )
!------------------------------------------------------------------------------
! Begin subroutine in_kgain and its subroutines
!------------------------------------------------------------------------------
  SUBROUTINE in_kgain(inest, xtime, ktau, ktaur, Nwe, Nsn, Nz,           &
                    niobf, vdim, nndgv, kalmax, read_kalman,             &
                    iprt, nlast, ndp,                                    &
                    lbdy, lskip, ipf, twindo,                            & 
                    fdob, timeob, vobs, terrain,                         &
                    nlevs_ob, lev_in_ob,                                 &
                    plfo, elevob, rio, rjo, rko,                         &
                    nstat, nstaw, ob_is_kalman, gain_at_ob,              &
                    oblat,oblon,                                        &
                    obs2kalman, filesave,                                &
!! wuyh added 20121206
                    nkalr,klast,                                         &
!! wuyh added 20121206
!! wuyh added 20160128
                    wusource,                                            &
!! wuyh added 20160128 end
                    nudge_terms,                                         &
                    num_ndg_terms, ndg_term_indxs,                       &
                    ims,ime, jms,jme, its,ite, jts,jte,                  &
                    ids,ide, jds,jde, kds,kde)

  USE module_domain
! For indexing into array vobs
  USE module_fddaobs_utilities

!*************************************************************************
! Purpose: Read Kalman gain data input from files, and interpolate the
!          gain parameters.  If the data is read from:
!
!          1) an obs-station file (located precisely at the observation
!             (Rx,Ry) location), only do vertical interpolation to Vob.
!
!          2) obs-network files (located at Kalman network grid-points),
!             interpolate to the exact observation site (Rx,Ry,Vob).
!
!          Here, (Rx,Ry) is the model grid coordinates, and Vob is the
!          obs pressure or height. (Nx,Ny,Nz) is the obs-netw coordinate,
!          not the WRF grid.
!
!  EX: WRF grid dimx=82 points in west-east and dimy=92 points in south-nth:
!       I=lbdy,dimy-lbdy,lskip, lbdy=13, lskip=2, therefore Nx=(92-13-13)/2+1=34
!       J=lbdy,dimx-lbdy,lskip, lbdy=13, lskip=2, therefore Ny=(82-13-13)/2+1=29
!       Nz=number of half sigma levels
!*************************************************************************

  IMPLICIT NONE

  INTEGER, intent(in) :: inest         ! nest level
  REAL, intent(in)    :: xtime         ! model time in minutes
  INTEGER, intent(in) :: ktau          ! current timestep
  INTEGER, intent(in) :: ktaur         ! restart timestep
  INTEGER, intent(in) :: Nwe           ! west-east dimension of obs netw grid
  INTEGER, intent(in) :: Nsn           ! south-north dimension of obs netw grid
  INTEGER, intent(in) :: Nz            ! vert dimension of obs netw grid
  INTEGER, intent(in) :: niobf         ! maximum number of observations
  INTEGER, intent(in) :: vdim          ! dim for vobs, vobs_qc
  INTEGER, intent(in) :: nndgv         ! number of nudge variables
  INTEGER, intent(in) :: kalmax        ! max number of kalman type obs
  INTEGER, intent(in) :: read_kalman   ! flag for reading kalman gain data
  LOGICAL, intent(in) :: iprt          ! print flag
  INTEGER, intent(in) :: nlast         ! last obs in list of valid obs from prev window
  INTEGER, intent(in) :: ndp           ! product of Kalman angles times number of coeffs
  INTEGER, intent(in) :: lbdy          ! start index for Kalman obs network
  INTEGER, intent(in) :: lskip         ! skip index for Kalman obs network
  LOGICAL, intent(in) :: ipf           ! switch to activate Kalman gain file open messages
  REAL, intent(in)    :: twindo        ! (time window)/2 (min) for nudging
  TYPE(fdob_type), intent(inout)  :: fdob     ! derived data type for obs data
  REAL, intent(in)    :: timeob(niobf)        ! model times for each observation (hours)
  REAL, intent(in)    :: vobs(vdim,niobf)     ! observational variables
  REAL, intent(in)    :: terrain(ims:ime, jms:jme) ! terrain height (m)
  REAL, intent(in)    :: nlevs_ob(niobf)      ! numbers of levels in sounding obs
  REAL, intent(in)    :: lev_in_ob(niobf)     ! level in sounding-type obs
  REAL, intent(in)    :: plfo(niobf)          ! index for type of obs-platform
  REAL, intent(in)    :: elevob(niobf)        ! elevations of observations  (meters)
  REAL, intent(in)    :: rio(niobf)           ! west-east coord of ob, non-staggered grid
  REAL, intent(in)    :: rjo(niobf)           ! south-north coord of ob, non-staggered grid
  REAL, intent(in)    :: rko(niobf)           ! vertical level of ob
  INTEGER, intent(in) :: ims,ime,jms,jme          ! memory dim
  INTEGER, intent(in) :: its,ite,jts,jte          ! tile dim
  INTEGER, intent(in) :: ids,ide,jds,jde,kds,kde  ! domain dim
  INTEGER, intent(in) :: nstat                ! number of obs read in for this model step 
  INTEGER, intent(in) :: nstaw                ! number of obs stations in current twindo 
  INTEGER, intent(inout) :: ob_is_kalman(niobf) ! flags specifying Kalman-type obs
  INTEGER, intent(inout) :: filesave(niobf)

  INTEGER, intent(inout) :: obs2kalman(niobf)   !mapping of obs indx to kalman indx
  INTEGER, intent(in)    :: nkalr,Klast         ! #old kalman obs to remove
  CHARACTER(LEN=*), intent(in) :: nudge_terms   ! var/covar terms (ex: UU UV TVr) for nudging
  INTEGER, intent(in)    :: num_ndg_terms       ! number of nudging terms
  INTEGER, intent(in)    :: ndg_term_indxs(MNP,2) ! indices for nudging terms
  REAL,    intent(inout) :: gain_at_ob(ndp,kalmax) ! gain at ob global, current time

!!! for check gain 
  Real   , intent(in) :: oblat(niobf)      ! obs latitude
  Real   , intent(in) :: oblon(niobf)      ! obs longitude

  REAL   :: gain_at_ob_local(ndp,kalmax) ! gain at ob local, current time

  Real,dimension(ndp,Nz)::gain_kst_t1,gain_kst_t2
  Real,dimension(Nz)::vls_kst_t1,vls_kst_t2

  Real,Parameter::obsri = 54.5 , obsrj = 62.5   ! for output
!  Real,Parameter::obsri = 54.533 , obsrj = 46.843   ! for output
! wuyh modied 20120823 end


!***  DECLARATIONS FOR IMPLICIT NONE                                    
  integer :: n              ! observation index
  integer :: ic, ia         ! loop counters over kalman coefs, angles, vars
  integer :: isw, jsw       ! Kalman obs network coordinate to southwest of obs loc
  integer :: ig, jg         ! equivalent wrf coord for isw, jsw

  integer :: nvol           ! read unit
  logical OPENED, exist     ! logicals for read unit
  integer idts              ! delta time from start to current time
  integer ixsec             ! model time (relative to start) in seconds
  real    dxob_ms, dyob_ms  ! grid frac in x, y, respectively, on mass-pt stag
  real    terr_ht           ! model terrain height (m) at ob location
  real    rio_mg, rjo_mg    ! x, y coords of observation on mass-pt grid stagger
  integer lrio, lrjo        ! int(rio_mg), int(rjo_mg) 
  integer kn_loc            ! index for kalman-type observations read in by this proc
  integer read_ndp          ! dimension for raw kalman data, read from file
  real    vob               ! vertical coord of ob, in pressure(mbar) or height(m) 
  logical read_height       ! read Kalman-file height columns instead of prs columns
  logical interp_in_height  ! interpolation flag (if =T, interpolate in height)

  integer local_kalman2obs(kalmax) ! map from local kalman indx to obs indx

  character*21 filn_stn     ! Kalman data file for obs-station pt
! wuyh added 20120915
  character*13 filn_net      ! Kalman data file for obs-netwk pt to the sw of the obs loc
!  character*13 filn_sw      ! Kalman data file for obs-netwk pt to the sw of the obs loc 
!  character*13 filn_se      ! Kalman data file for obs-netwk pt to the se of the obs loc
!  character*13 filn_nw      ! Kalman data file for obs-netwk pt to the nw of the obs loc
!  character*13 filn_ne      ! Kalman data file for obs-netwk pt to the ne of the obs loc
! wuyh added 20120915 end

!!! wuyh modified 20160128
!  character*14 dir1         ! directory with Kalman data at curr hour
!  character*14 dir2         ! directory with Kalman data at next hour
  Character(Len=19) :: dir1, dir2

!!! wuyh modified 20160128 end
  character(len=200) :: msg ! argument to wrf_message

  logical :: skip_interp    ! flag to skip interpolation if missing ob press and ht

  LOGICAL :: MP_LOCAL_DUMMASK(NIOBF)   ! mask for work to be done on this processor
! wuyh removed 20120823
!  Integer :: station_kgain_data(niobf) ! flags for gain at ob station
! wuyh removed 20120823

  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  LOGICAL, EXTERNAL :: rnet_kalman_netpt_UG    ! Upper air, gridded
  LOGICAL, EXTERNAL :: rnet_kalman_stnpt_US    ! Upper air, station
  LOGICAL, EXTERNAL :: rnet_kalman_netpt_SG    ! Surface,   gridded
  LOGICAL, EXTERNAL :: rnet_kalman_stnpt_SS    ! Surface,   station
  LOGICAL, EXTERNAL :: tile_mask

  DATA NVOL/81/
  Integer::iu,nww
!!! wuyh added 20160128
  Integer, intent(in) :: wusource(40,niobf)         ! obs source
  Character(Len=4)    :: wutype
!!! wuyh added 20160128

! wuyh added 20120708 
  Character(Len=20)::FMT
  Integer::numg,kv
  Integer::ncc
  Integer,parameter::nscal = 1
  Real   ::wscal
  Integer::iii,jjj,aaa,bbb
  integer :: ntrm          ! Index for ob into nudging terms list
  integer :: len_pair_str  ! Length of pair string
  character*5 :: pair_str  ! Pair string for reading Kalman gains (e.g., "VrU")

!  write(6,1036) vdim,ndp,kalmax
!1036 Format("in kgain vdim ndp kalmax = ",3(2x,I8))
!  call flush(6)

  ncc = (ndp+1)/2
  
! wuyh added 20120708
    numg = NINT(SQRT(float(ndp)))
    write(FMT,'("(",I0,"F6.2)")')numg
!ajb: suppress this output
!    write(291,*)FMT,ncc,ndp

! wuyh added 20120708 end

! If read_kalman flag is zero, simply set ob_is_kalman array to zero and return
  if(read_kalman.eq.0) then
    ob_is_kalman = 0
!    filesave = -1000010
    return 
  endif

! Check dimensions for Kalman data
! Read special kgain_param file
   IF ( wrf_dm_on_monitor() ) THEN
 !   OPEN(NVOL,FILE="kgain_param.txt",FORM='FORMATTED',STATUS='OLD')
 !   READ(NVOL,*) read_ndp
 !   write(6,*) 'Value of ndirpow in file kgain_param.txt:',read_ndp
 !   write(6,*) 'Value of ndirpow in namelist:            ',ndp
 !   CLOSE (NVOL)

  !  if(read_ndp.ne.ndp) then
  !    WRITE(wrf_err_message,'(2(a,i5),a)')                                 &
  !             'IN_KGAIN: Value of namelist variable obs_ndirpow = ',ndp,  &
  !             ' does not match value = ',read_ndp,' in kgain_param.txt file.'
  !    CALL wrf_error_fatal ( wrf_err_message )
  !  endif
   ENDIF

! Initialize kalman flags, maps, and counters
!  station_kgain_data = 0     ! =1 if data available at ob station loc
  kn_loc = 0
  local_kalman2obs=0
  gain_at_ob_local = float(0)    !wuyh added

! Store dimensions of Kalman obs-network
  fdob%obs_net_dim(1) = Nwe
  fdob%obs_net_dim(2) = Nsn
  fdob%obs_net_dim(3) = Nz

! Set tile mask array
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  Do n = 1, nstat
!ajbmasspt
! wuyh modified for OSSE 
!    rio_mg = float(NINT(rio(n) - 0.5))  ! get x coord of obs on mass-pt stagger
!    rjo_mg = float(NINT(rjo(n) - 0.5))  ! get y coord of obs on mass-pt stagger
    rio_mg = rio(n) - 0.5    ! get x coord of obs on mass-pt stagger
    rjo_mg = rjo(n) - 0.5    ! get y coord of obs on mass-pt stagger
! wuyh modified for OSSE
!   rio_mg = rio(n)      !non-stagger for now                         
!   rjo_mg = rjo(n)      !non-stagger for now
! wuyh modified for OSSE
!    lrio = nint(rio_mg)
!    lrjo = nint(rjo_mg)
    lrio = nint(rio_mg)
    lrjo = nint(rjo_mg)
! wuyh modified for OSSE

!!! wuyh added 20120820
    lrio = MAX0(1,lrio)
    lrio = MIN0(lrio,ide-1)
    lrjo = MAX0(1,lrjo)
    lrjo = MIN0(lrjo,jde-1)

!    write(322,*)rio_mg,rjo_mg,lrio,lrjo

!   Set mask for obs to be handled by this processor
    MP_LOCAL_DUMMASK(N) = TILE_MASK(lrio, lrjo, its, ite, jts, jte)
  Enddo

#endif

! Loop over (new) observations in current time window
! wuyh added 20120820
!    station_kgain_data(nlast+1:niobf) = 0     ! =1 if data available at ob station loc
    ob_is_kalman(nlast+1:niobf) = 0           ! =1 ob is kalman 
! wuyh added 20120820 end

  do n = nlast+1, nstat

    skip_interp = .false.   ! initialize flag to do interpolation

! Determine the nearest obs netwk grid-pt to the southwest of the obs location
!ajbmasspt
! wuyh modified for OSSE
!    rio_mg = float(NINT(rio(n) - 0.5))  ! get x coord of obs on mass-pt stagger
!    rjo_mg = float(NINT(rjo(n) - 0.5))  ! get y coord of obs on mass-pt stagger
    rio_mg = rio(n) - 0.5    ! get x coord of obs on mass-pt stagger
    rjo_mg = rjo(n) - 0.5    ! get y coord of obs on mass-pt stagger
! wuyh modified for OSSE
!   rio_mg = rio(n)      !non-stagger for now                       
!   rjo_mg = rjo(n)      !non-stagger for now

! wuyh removed  20120915
!!    lrio = nint(rio_mg)
!!    lrjo = nint(rjo_mg)
!    lrio = int(rio_mg)
!    lrjo = int(rjo_mg)
! wuyh modified for nearest network gain 20120831
!!    if(lrio.LT.lbdy) then
!!       isw = -1
!!    else
!!       isw = 1 + NINT(float((lrio-lbdy)/lskip))
!!    endif
!!    if(lrjo.LT.lbdy) then
!!       jsw = -1
!!    else
!!       jsw = 1 + NINT(float((lrjo-lbdy)/lskip))
!!    endif
! wuyh removed 20120915 end

! Only attempt to read data for observations located in this processor patch
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
    IF( MP_LOCAL_DUMMASK(N) ) then
#endif

! Read and interpolate data for up to kalmax number of Kalman-type obs
  if(kn_loc .lt. kalmax) then           ! wuyh added 20111018

! Form directory names for current and next hour
    ixsec = nint(timeob(n)*60. - fdob%xtime_at_rest)*60

!!! wuyh modified 20160128
    if (abs(plfo(n) - 10. ) .le. 0.1) then
     wutype(1:1) = char(wusource(7,n))
     wutype(2:2) = char(wusource(8,n))
     wutype(3:3) = char(wusource(9,n))
     wutype(4:4) = char(wusource(10,n))
    else
     wutype = "COMM"
    endif
    CALL get_kalman_dirn(fdob%sdate, ixsec     , inest, wutype, dir1)
    CALL get_kalman_dirn(fdob%sdate, ixsec+3600, inest, wutype, dir2)
!!! wuyh modified 20160128 end

! ajb added 20120626 so we only read once for soundings
    IF(nint(nlevs_ob(n)).EQ.1 .OR. nint(lev_in_ob(n)).EQ.1) THEN

! Initialize kalman flags for each ob. Each processor will set flags true only for
! obs which are located within its patch, and only if all necessary files are found
! for interpolating the Kalman gains. At the end of this subroutine (see call to
! do_kalman_accounting), the local array content will be message passed so that
! each processor knows the global array.

!    ob_is_kalman(n) = 0     !wuyh modified

! Set flag to read pressure column or height column data
      if(vobs(Prs,n).ne.-888888.) then
          read_height = .false.
      else
          read_height = .true.
      endif

! Get index into nudge term varlist
      CALL get_ob_pair_indx(fdob%ob_var_type(n),fdob%ob_ndg_type(n), MNP,   &
                            fdob%num_ndg_terms, fdob%ndg_term_indxs(:,1),   &
                            fdob%ndg_term_indxs(:,2), ntrm)

! Get the pair-string symbol (e.g., "VrU") for reading gains from netCDF file 
      CALL get_pair_string(fdob%ndg_term_indxs(ntrm,1), fdob%ndg_term_indxs(ntrm,2), &
                           pair_str, len_pair_str)

! Get the filename of surface or upper-air Kalman station data file and read data
      if(rko(n).eq.1.0) then   ! SURFACE DATA - Look for station data first

        CALL get_real_filenm(inest, rio_mg, rjo_mg, 'SS', filn_stn)

        IF( rnet_kalman_stnpt_SS(inest, ipf, nvol, dir1, filn_stn, ndp,                 &
                      Nz, rio_mg, rjo_mg, read_height, pair_str, len_pair_str,          &
                      vls_kst_t1,gain_kst_t1)                                           &
                                               .AND.                                    &
            rnet_kalman_stnpt_SS(inest, ipf, nvol, dir2, filn_stn, ndp,                 &
                      Nz, rio_mg, rjo_mg, read_height, pair_str, len_pair_str,          &
                      vls_kst_t2,gain_kst_t2) ) THEN
     
              ob_is_kalman(n) =  -1
              kn_loc = kn_loc+1
              iii = int(rio_mg)
              jjj = int(rjo_mg)
              aaa = nint(10.*(rio_mg-float(iii)))
              bbb = nint(10.*(rjo_mg-float(jjj)))
              if(aaa.eq.10) then
                iii=iii+1
                aaa=0
              endif
              if(bbb.eq.10) then
                jjj=jjj+1
                bbb=0
              endif
              filesave(n) = 100000*(iii*10+aaa) + (jjj*10+bbb)
        ELSE  ! SURFACE DATA - Now look for netwk data
          CALL get_sw_netwk_grdpt(rio_mg, rjo_mg, lbdy, lskip, Nwe, Nsn, ig, jg, wutype)
          CALL get_intg_filenm(inest, ig, jg, 'SG', filn_net)

          If(rnet_kalman_netpt_SG(inest, ipf, nvol, dir1, filn_net, ndp,      &
                      Nz, read_height, pair_str, len_pair_str,                &
                      vls_kst_t1, gain_kst_t1)                                &
                                               .AND.                          &
             rnet_kalman_netpt_SG(inest, ipf, nvol, dir2, filn_net, ndp,      &
                      Nz, read_height, pair_str, len_pair_str,                &
                      vls_kst_t2, gain_kst_t2)) THEN
              ob_is_kalman(n) = 1
              kn_loc = kn_loc+1
              filesave(n) = 100000*(ig*10) + (jg*10)
          Else  ! SURFACE DATA - use network sounding gain
            CALL get_sw_netwk_grdpt(rio_mg, rjo_mg, lbdy, lskip, Nwe, Nsn, ig, jg, wutype)
            CALL get_intg_filenm(inest, ig, jg, 'UG', filn_net)
            If(.NOT.rnet_kalman_netpt_UG(inest, ipf, nvol, dir1, filn_net, ndp, &
                        Nz, read_height, pair_str, len_pair_str,              &
                        vls_kst_t1, gain_kst_t1)                              &
                                                  .OR.                        &
             .NOT.rnet_kalman_netpt_UG(inest, ipf, nvol, dir2, filn_net, ndp, &
                        Nz, read_height, pair_str, len_pair_str,              &
                        vls_kst_t2, gain_kst_t2)) GOTO 100

             ob_is_kalman(n) = 1
             kn_loc = kn_loc+1
             filesave(n) = 100000*(ig*10) + (jg*10)
          Endif
        ENDIF

      else  ! UPPER AIR DATA - Look for station data first

        CALL get_real_filenm(inest, rio_mg, rjo_mg, 'US', filn_stn)
        IF(rnet_kalman_stnpt_US(inest, ipf, nvol, dir1, filn_stn, ndp,                &
                     Nz, rio_mg, rjo_mg, read_height, pair_str, len_pair_str,         &
                     vls_kst_t1, gain_kst_t1)                                         &
                                              .AND.                                   &
           rnet_kalman_stnpt_US(inest, ipf, nvol, dir2, filn_stn, ndp,                &
                     Nz, rio_mg, rjo_mg, read_height, pair_str, len_pair_str,         &
                     vls_kst_t2, gain_kst_t2)) THEN
              ob_is_kalman(n) =  -1
              kn_loc = kn_loc+1
              iii = int(rio_mg)
              jjj = int(rjo_mg)
              aaa = nint(10.*(rio_mg-float(iii)))
              bbb = nint(10.*(rjo_mg-float(jjj)))
              if(aaa.eq.10) then
                iii=iii+1
                aaa=0
              endif
              if(bbb.eq.10) then
                jjj=jjj+1
                bbb=0
              endif
              filesave(n) = 100000*(iii*10+aaa) + (jjj*10+bbb)
        ELSE  ! UPPER AIR DATA - Now look for netwk data 
          CALL get_sw_netwk_grdpt(rio_mg, rjo_mg, lbdy, lskip, Nwe, Nsn, ig,jg, wutype)
          CALL get_intg_filenm(inest, ig, jg, 'UG', filn_net)
          IF(.NOT.rnet_kalman_netpt_UG(inest, ipf, nvol, dir1, filn_net, ndp, &
                      Nz, read_height, pair_str, len_pair_str,                &
                      vls_kst_t1, gain_kst_t1)                                &
                                               .OR.                           &
             .NOT.rnet_kalman_netpt_UG(inest, ipf, nvol, dir2, filn_net, ndp, &
                      Nz, read_height, pair_str, len_pair_str,                &
                      vls_kst_t2, gain_kst_t2)) GOTO 100

             ob_is_kalman(n) = 1
             kn_loc = kn_loc+1
!!              write(372,1372)n,ob_is_kalman(n),filn_net
!             filesave(n)=filn_net
!             filesave(n) = float(100000)*rio_mg + rjo_mg
!             filesave(n) = 100000*(ig*100) + (jg*100)
             filesave(n) = 100000*(ig*10) + (jg*10)
        ENDIF
1372    Format("n ob_is_kalman file_net=",I7,1x,I1,1x,A)

      endif

!      write(372,*)"net",rio_mg,rjo_mg,filn_net

!ajb: suppress this output
!      if(read_height)then
!        write(295,1295)n,read_height,oblat(n),oblon(n),      &
!            vobs(1,n), vobs(Prs,n),vobs(Hgt,n)
!      endif
 1295  Format(I6,1x,L1,2(1x,F8.2),6(1x,F10.2))

      100  CONTINUE

    ENDIF     ! nlev = 1 ajb added 20120626

! Set ob_is_kalman flags identically for all obs in a sounding (ajb added 20120626)
If(nint(lev_in_ob(n)).gt.1) then
      ob_is_kalman(n) = ob_is_kalman(n+1-nint(lev_in_ob(n)))
      filesave(n)=filesave(n+1-nint(lev_in_ob(n)))
! wuyh removed 20120823
!      station_kgain_data(n) = station_kgain_data(n+1-nint(lev_in_ob(n)))
! wuyh removed 20120823
      if(ABS(ob_is_kalman(n)).gt.0) kn_loc = kn_loc+1
Endif

! Interpolate data for any new Kalman-type obs
    IF(ABS(ob_is_kalman(n)).EQ.1 .and. kn_loc.GT.0) then    ! we have the necessary gain data at the ob location

!!! wuyh modified 20120814      if(vobs(Prs,n).ne.-888888.) then      ! interpolate in pressure
      if(vobs(Prs,n).gt. 5..AND. vobs(Prs,n) .le. 120.) then      ! interpolate in pressure
        vob = 10.*vobs(Prs,n)   ! Convert from kPa to mbar
        interp_in_height = .false.
        local_kalman2obs(kn_loc) = n
! wuyh modified 20111026 to add elevob condition
!!!! wuyh modified 20120814      elseif(vabs(Hgt,n).ne.-888888. .AND. ABS(elevob(n)) .lt. 8600. ) then  ! interpolate in height
      elseif(vobs(Hgt,n).gt.-500. .AND. ABS(elevob(n)) .lt. 8900. ) then  ! interpolate in height
        vob = vobs(Hgt,n)

! Adjust ob height vob for ob near surface. To do this, first need model terrain height at ob location.
        dxob_ms = rio_mg-FLOAT(lrio)
        dyob_ms = rjo_mg-FLOAT(lrjo)
        terr_ht = ((1.-dyob_ms)*((1.-dxob_ms)*terrain(lrio,lrjo)+    &
                  dxob_ms*terrain(lrio+1,lrjo)                       &
                  )+dyob_ms*((1.-dxob_ms)*terrain(lrio,lrjo+1)+      &
                  dxob_ms*terrain(lrio+1,lrjo+1)))

        CALL adjust_ob_height(vob, terr_ht, elevob(n))
        interp_in_height = .true.
        local_kalman2obs(kn_loc) = n
      else
        write(msg,'(A,I8)')'No gain interpolation due to missing ob prs and &
                            ght for ob N = ',n
        write(msg,'(I7,4(1x,F10.2))')n,oblat(n),oblon(n),vobs(Prs,n),vobs(Hgt,n)
        call wrf_message(msg)
        skip_interp = .true.
        ob_is_kalman(n) = 0
        kn_loc = kn_loc-1
      endif

!write(6,'(a,i5,a,l3,2(a,f12.3))') 'N = ',n,                     &
!           ' station_kgain_data(n) = ',station_kgain_data(n),   &
!           ' vobs(Prs,n) = ',vobs(Prs,n),                       &
!           ' vobs(Hgt,n) = ',vobs(Hgt,n)

      If(.NOT.skip_interp) then
!        if(ob_is_kalman(n) .EQ. -1) then
           if(MOD(n,3000) .EQ. 1 ) then          
!ajb: suppress this output
!            Do nww =1,nz
!             write(232,*)n,nww,timeob(n),vob,interp_in_height
!!             write(234,*)n,nww,vls_kst_t2(nww)
!             write(232,FMT)(gain_kst_t2(iu,nww,3),iu=1,ndp)
!            Enddo
           endif

          if(ABS(ob_is_kalman(n)) .EQ. 1) then

            CALL interp_kgain_stn(Nz, ndp, timeob(n),                    &
                                 interp_in_height, vob, rko(n),          &
                                 vls_kst_t1, vls_kst_t2,                 &
                                 gain_kst_t1, gain_kst_t2,               &
                                 gain_at_ob_local(1,kn_loc))

!            write(218,1218)n,kn_loc,vob,gain_kst_t1(ncc,5), &
!               gain_kst_t2(ncc,5),gain_at_ob_local(ncc,kn_loc)
1218        Format(2(I7,1x),F10.2,3(1x,E12.6))

!            if(abs(rio(n)-obsri)+abs(rjo(n)-obsrj) .le. 1.0E-1 .and. &
!                           abs(timeob(n)-12.00000) .le. 1.0E-4 ) then
!ajb: suppress this output
!               write(235,*)n,timeob(n),vob,interp_in_height
!               write(235,FMT)(gain_at_ob_local(iu,3,kn_loc),iu=1,ndp)
!            endif

!!! wuyh added 20101214
!!! scaling the gain
           IF(nscal .eq. 1) then
             wscal =  MAX(abs(gain_at_ob_local(ncc,kn_loc)),1.0E-3)
             Do iu=1,ndp
              gain_at_ob_local(iu,kn_loc)=gain_at_ob_local(iu,kn_loc)/wscal
             Enddo
           Endif
!!! Wuyh added 20101214 end

          endif

      Endif   !.NOT.skip_interp
    ENDIF    !ABS(ob_is_kalman)=1 .and. kn_loc > 0

   else
    write(*,*)'in_kgain: kalman obs reaches its maximum =',kn_loc,kalmax
   endif
! End of section that reads and interpolates Kalman-type obs

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
    ENDIF     !end IF( TILE_MASK(lrio, lrjo, its, ite, jts, jte) )
#endif
  enddo       !end obs loop n=nlast+1,nstat

! Gains for all new Kalman-type obs in this time window have been read and interpolated.
! Now we need to do some accounting. Note that kn_loc is the total number of Kalman-type
! obs that were read by this processor.

IF( nstat .gt. nlast )THEN

  CALL do_kalman_accounting(niobf, kalmax, nstat, nstaw, nlast,        &
                            vdim,                                      &
                            nndgv,nkalr,Klast, kn_loc,ndp, iprt,       &
! wuyh added 20120823
                                  its,ite, jts,jte, ide, jde,          &
! wuyh added 20120823 end

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
                            mp_local_dummask,                          &
#endif
                            inest, ob_is_kalman,                       &
! wuyh modified 20120823
!station_kgain_data,                                                   &
                                  rio,rjo,                             &
! for output use only
                                  vobs,xtime,timeob,                   &
                                  oblat,oblon,                         &
! end for output use only
                                  local_kalman2obs,gain_at_ob_local,   &
                                  obs2kalman,gain_at_ob,filesave)
ENDIF

! IF ( wrf_dm_on_monitor() ) THEN
!   Do n=1,nstat
!    write(420,1420)n,xtime,timeob(n)*60.,gain_at_ob(ncc,n)
!     write(283,*)n
!     write(283,'(13F6.2)')(gain_at_ob(iu,3,n),iu=1,ndp)
!
!   enddo
! Endif
 1420 Format(I7,2(1x,F6.1),(1x,(1x,E11.5)))

  END SUBROUTINE in_kgain

  SUBROUTINE get_intg_filenm(nn, iii, jjj, otyp, filname)
!*************************************************************************
! Purpose: Construct a filename of the form:
!              UGnniiijjj.txt  (UG - Upper-air Grid (netwk-pt), or
!              SGnniiijjj.txt  (SG - Surface   Grid (netwk-pt)
!          where:
!          nn is the domain number (e.g., 01), iii is the west-east
!          integer grid coordinate, and jjj is the south-north integer
!          grid coordinate.
!          Ex: nn=1, ri=41, rj=15 => filename=UG01041015.txt
!*************************************************************************

  IMPLICIT NONE

  INTEGER, intent(in) :: nn            ! nest level
  INTEGER, intent(in) :: iii           ! west-east coordinate
  INTEGER, intent(in) :: jjj           ! south-north coordinate
  CHARACTER*2,  intent(in) :: otyp     ! ob type (UG or SG)
  CHARACTER*13, intent(out) :: filname ! filename to construct

! Local variables
  character*3 ichr          ! char form of iii 
  character*3 jchr          ! char form of jjj
  character*4 nchr          ! char form of UGnn or SGnn

! prepare coordinate part of filename
  write(ichr(1:3),'(i3)') iii
  if(ichr(2:2).eq.' ')ichr(2:2)='0'
  if(ichr(1:1).eq.' ')ichr(1:1)='0'
  write(jchr(1:3),'(i3)') jjj
  if(jchr(2:2).eq.' ')jchr(2:2)='0'
  if(jchr(1:1).eq.' ')jchr(1:1)='0'
! prepare domain index part of filename
  nchr(1:2) = otyp(1:2) 
  write(nchr(3:4),'(i2)') nn 
  if(nchr(3:3).eq.' ')nchr(3:3)='0'
! construct filename
  filname(1:4) = nchr(1:4)
  filname(5:7) = ichr(1:3)
  filname(8:10) = jchr(1:3)
! filname(11:14) = '.txt'
  filname(11:13) = '.nc'

  END SUBROUTINE get_intg_filenm

  SUBROUTINE get_real_filenm(nn, ri, rj, otyp, filname)
!*************************************************************************
! Purpose: Construct a filename of the form:
!              USnniiijjj.aaabbb.txt  (US - Upper-air Station-pt file), or  
!              SSnniiijjj.aaabbb.txt  (SS - Surface   Station-pt file)
!          where:
!          nn is the domain number (e.g., 01),
!          iii is the integer part of the west-east grid coord ri
!          jjj is the integer part of the south-north grid coord rj
!          aaa is the fractional part of the west-east grid coord ri
!          bbb is the fractional part of the south-north grid coord rj
! CURRENT IMPLEMENTATION: Round to the nearest tenth and pad with zeros:
!          Ex: ri=41.142, rj=15.254 => filename=US01041015.100300.txt
!          Ex: ri=39.942, rj=21.994 => filename=US01039022.900000.txt
!*************************************************************************

  IMPLICIT NONE

  INTEGER, intent(in) :: nn            ! nest level
  REAL,    intent(in) :: ri            ! west-east coordinate
  REAL,    intent(in) :: rj            ! south-north coordinate
  CHARACTER*2,  intent(in) :: otyp     ! ob type (US or SS)
  CHARACTER*20, intent(out) :: filname ! filename to construct

! Local variables
  character*3 ichr          ! char form of iii 
  character*3 jchr          ! char form of jjj
  character*3 achr          ! char form of aaa 
  character*3 bchr          ! char form of bbb
  character*4 nchr          ! char form of USnn or SSnn
  integer iii               ! integer part of ri
  integer jjj               ! integer part of rj
  integer aaa               ! fractional part of ri
  integer bbb               ! fractional part of rj

! get integer and fractional parts of coordinates
  iii = int(ri)
  jjj = int(rj)
  aaa = nint(10.*(ri-float(iii)))
  bbb = nint(10.*(rj-float(jjj)))
  if(aaa.eq.10) then
    iii=iii+1
    aaa=0
  endif
  if(bbb.eq.10) then
    jjj=jjj+1
    bbb=0
  endif

! prepare integer coordinate part of filename
  write(ichr(1:3),'(i3)') iii
  if(ichr(2:2).eq.' ')ichr(2:2)='0'
  if(ichr(1:1).eq.' ')ichr(1:1)='0'
  write(jchr(1:3),'(i3)') jjj
  if(jchr(2:2).eq.' ')jchr(2:2)='0'
  if(jchr(1:1).eq.' ')jchr(1:1)='0'

! prepare fractional coordinate part of filename
  write(achr(1:1),'(i1)') aaa
  achr(2:3) = '00'
  write(bchr(1:3),'(i1)') bbb
  bchr(2:3) = '00'

! prepare domain index part of filename
  nchr(1:2) = otyp(1:2) 
  write(nchr(3:4),'(i2)') nn
  if(nchr(3:3).eq.' ')nchr(3:3)='0'
! construct filename
  filname(1:4) = nchr(1:4)
  filname(5:7) = ichr(1:3)
  filname(8:10) = jchr(1:3)
  filname(11:11) = '.'
  filname(12:14) = achr(1:3)
  filname(15:17) = bchr(1:3)
  filname(18:20) = '.nc'

  END SUBROUTINE get_real_filenm

  SUBROUTINE get_kalman_dirn(sdate,ixsec,inest,wutype,dirname)
!*************************************************************************
! Purpose: Form directory name of containing desired Kalman data at a
!          specified model time.
!*************************************************************************
  USE module_date_time , ONLY : geth_newdate

  IMPLICIT NONE

  CHARACTER*19, intent(in)  :: sdate    ! model reference time (YYYY-MM-DD HH:MM:SS.ffff)
  INTEGER,      intent(in)  :: ixsec    ! model time (relative to start) in seconds
  INTEGER,      intent(in)  :: inest    ! nest level
  CHARACTER*19, intent(out) :: dirname  ! directory containing data at requested time

!!! wuyh added 20160128
  Character(len=4):: wutype
!!! wuyh added 20160128 end
  
! Local variables
  character(len=19) :: new_date     ! current date string (YYYY-MM-DD HH:MM:SS.ffff)

! prepare domain index part of filename
!!! wuyh 20160128 modified
  dirname(1:4) = wutype
  dirname(5:5) = '/'
  dirname(6:6) = 'd'
!!! wuyh 20160128 modified end
  write(dirname(7:8),'(i2.2)') inest
  dirname(9:9) = '/'
  call geth_newdate(new_date(1:19), sdate(1:19), ixsec)
  dirname(10:13)  = new_date(1:4)
  dirname(14:15)  = new_date(6:7)
  dirname(16:17)  = new_date(9:10)
  dirname(18:19)  = new_date(12:13)

!  write(201,*)dirname 
!  stop 99
  END SUBROUTINE get_kalman_dirn

  LOGICAL FUNCTION rnet_kalman_stnpt_SS(inest, iprt, nvol, dir, fil, ndp,       &
                                        nz, ri, rj, read_height, pair_str,      &
                                        len_pair_str, vls, gain)
!*************************************************************************
! Purpose: Open the specified surface station-point Kalman gain file,
!          and read the single level data for each field.
!
! Return Value: TRUE if the requested file is found,
!               FALSE otherwise
!
! NOTE: The form of the filename is SSNNIIIJJJ.AAA.BBB.nc, where:
!       NN is the nest,
!       III is the integer part of the west-east grid coord
!       JJJ is the integer part of the south-north grid coord
!       AAA is the fractional part of the west-east grid coord
!       BBB is the fractional part of the south-north grid coord
!       Ex: nn=1, ri=41.142, rj=15.214 => filename=S01041015.142214.txt
!
!*************************************************************************

  IMPLICIT NONE
  include 'netcdf.inc'

  integer,      intent(in)  :: inest   ! nest level
  logical,      intent(in)  :: iprt    ! print flag
  integer,      intent(in)  :: nvol    ! read unit
  character*19, intent(in)  :: dir     ! directory containing file to open and read
  character*20, intent(in)  :: fil     ! file containing Kalman data at obs netw pt  
  integer,      intent(in)  :: ndp     ! no. of gain coeffs for each var and z lev
  integer,      intent(in)  :: nz      ! no. of z-levels to read in 
  real,         intent(in)  :: ri      ! west-east coordinate of ob
  real,         intent(in)  :: rj      ! south-nth coordinate of ob
  logical,      intent(in)  :: read_height ! height flag (if true, read ht instead of prs)
  integer,                     intent(in) :: len_pair_str  ! Length of pair string
  character(len=len_pair_str), intent(in) :: pair_str  ! Pair string for reading Kalman gains (e.g., "VrU")
  real,         intent(inout) :: vls(nz) ! pressure (or ht) levels at ob station locations
  real,         intent(inout) :: gain(ndp,nz) ! Kalman gain at ob station locations

! Local variables
  character*40 filein     ! full file path of Kalman data file to read in
  logical OPENED, exist   ! logicals for read unit
  real lat, lon           ! temp vars for reading Kalman lat/lon fields
  integer ilev, idp       ! loop counters
  character(len=200) :: msg ! Argument to wrf_message
  integer isize           ! data size, read from file
  integer ijump           ! data jump, read from file

  Integer :: numg
  integer :: ncid         ! netCDF file id
  integer :: varid        ! netCDF variable id
  integer :: retval       ! netCDF return code

! Initialize
  rnet_kalman_stnpt_SS = .false.

! Open file and read Kalman station data
  filein(1:19)  = dir(1:19)
  filein(20:20) = '/'
  filein(21:40) = fil(1:20)
  numg = NINT(SQRT(float(ndp)))

! Open the file. NF_NOWRITE tells netCDF we want read-only access.
  retval = nf_open(filein, NF_NOWRITE, ncid)

  if (retval .ne. nf_noerr) then
    IF (iprt) THEN
      write(msg,'(a,a35)') 'Cannot open Kalman station gain file = ',filein
      call wrf_message(msg)
    ENDIF

  else  ! Notify and proceed

    IF (iprt) THEN
      write(msg,'(a,a35,a,i2,a,2f8.3)') 'Opening Kalman station gain file = ',filein,  &
                                        ', nest ',inest,', ob pt ',ri ,rj
      call wrf_message(msg)
    ENDIF

! Read the data
    If(read_height) then

      retval = nf_inq_varid(ncid, 'PH', varid)
      if (retval .ne. nf_noerr) CALL abort('Cannot find term: PH')

      retval = nf_get_var_real(ncid, varid, vls(1))
      if (retval .ne. nf_noerr) CALL abort('Error reading term: PH')

    Else

      retval = nf_inq_varid(ncid, 'P', varid)
      if (retval .ne. nf_noerr) CALL abort('Cannot find term: P')

      retval = nf_get_var_real(ncid, varid, vls(1))
      if (retval .ne. nf_noerr) CALL abort('Error reading term: P')

    Endif

    retval = nf_inq_varid(ncid, pair_str, varid)

    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Cannot find term: ',pair_str
      CALL abort(msg)
    endif

    retval = nf_get_var_real(ncid, varid, gain)
    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Error reading term: ',pair_str
    endif

    retval = nf_inq_varid(ncid, pair_str, varid)

    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Cannot find term: ',pair_str
      CALL abort(msg)
    endif

    retval = nf_get_var_real(ncid, varid, gain)
    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Error reading term: ',pair_str
      CALL abort(msg)
    endif

    rnet_kalman_stnpt_SS = .true.

    retval = nf_close(ncid)
    if (retval .ne. nf_noerr) then
      IF (iprt) THEN
        write(msg,'(a,a35)') 'Error closing Kalman station gain file = ',filein
        call wrf_message(msg)
      ENDIF
    endif

  endif   ! end if(no error opening file)

  RETURN
  END FUNCTION rnet_kalman_stnpt_SS

  LOGICAL FUNCTION rnet_kalman_stnpt_US(inest, iprt, nvol, dir, fil, ndp,      &
                                        nz, ri, rj, read_height, pair_str,     &
                                        len_pair_str, vls, gain)
!*************************************************************************
! Purpose: Open the specified upper-air station-point Kalman gain file,
!          and read the column data for each field.
!
! Return Value: TRUE if the requested file is found,
!               FALSE otherwise
!
! NOTE: The form of the filename is USNNIIIJJJ.AAA.BBB.txt, where:
!       NN is the nest,
!       III is the integer part of the west-east grid coord
!       JJJ is the integer part of the south-north grid coord
!       AAA is the fractional part of the west-east grid coord
!       BBB is the fractional part of the south-north grid coord
!       Ex: nn=1, ri=41.142, rj=15.214 => filename=S01041015.142214.txt
!
!*************************************************************************

  IMPLICIT NONE
  include 'netcdf.inc'

  integer,      intent(in)  :: inest       ! nest level
  logical,      intent(in)  :: iprt        ! print flag
  integer,      intent(in)  :: nvol        ! read unit
  character*19, intent(in)  :: dir         ! directory containing file to open and read
  character*20, intent(in)  :: fil         ! file containing Kalman data at obs netw pt  
  integer,      intent(in)  :: ndp         ! no. of gain coeffs for each var and z lev
  integer,      intent(in)  :: nz          ! no. of z-levels to read in 
  real,         intent(in)  :: ri          ! west-east coordinate of ob
  real,         intent(in)  :: rj          ! south-nth coordinate of ob
  logical,      intent(in)  :: read_height ! height flag (if true, read ht instead of prs)
  integer,                     intent(in) :: len_pair_str  ! Length of pair string
  character(len=len_pair_str), intent(in) :: pair_str  ! Pair string for reading Kalman gains (e.g., "VrU")
  real,         intent(inout) :: vls(nz)   ! pressure (or ht) levels at ob station locations
  real,         intent(inout) :: gain(ndp,nz) ! Kalman gain at ob station locations

! Local variables
  character*40 filein     ! full file path of Kalman data file to read in
  logical OPENED, exist   ! logicals for read unit
  real lat, lon           ! temp vars for reading Kalman lat/lon fields
  integer ilev, idp       ! loop counters
  character(len=200) :: msg ! Argument to wrf_message
  integer isize           ! data size, read from file
  integer ijump           ! data jump, read from file

  Integer :: numg
  integer :: ncid         ! netCDF file id
  integer :: varid        ! netCDF variable id
  integer :: retval       ! netCDF return code

! Initialize
  rnet_kalman_stnpt_US = .false.

! Open file and read Kalman station data
  filein(1:19)  = dir(1:19)
  filein(20:20) = '/'
  filein(21:40) = fil(1:20)
  numg = NINT(SQRT(float(ndp)))

! Open the file. NF_NOWRITE tells netCDF we want read-only access.
  retval = nf_open(filein, NF_NOWRITE, ncid)

  if (retval .ne. nf_noerr) then
    IF (iprt) THEN
      write(msg,'(a,a35)') 'Cannot open Kalman station gain file = ',filein
      call wrf_message(msg)
    ENDIF

  else  ! Notify and proceed

    IF (iprt) THEN
      write(msg,'(a,a35,a,i2,a,2f8.3)') 'Opening Kalman station gain file = ',filein,  &
                                        ', nest ',inest,', ob pt ',ri ,rj 
      call wrf_message(msg)
    ENDIF

! Read the data
    If(read_height) then

      retval = nf_inq_varid(ncid, 'PH', varid)
      if (retval .ne. nf_noerr) CALL abort('Cannot find term: PH')

      retval = nf_get_var_real(ncid, varid, vls)
      if (retval .ne. nf_noerr) CALL abort('Error reading term: PH')

    Else

      retval = nf_inq_varid(ncid, 'P', varid)
      if (retval .ne. nf_noerr) CALL abort('Cannot find term: P')

      retval = nf_get_var_real(ncid, varid, vls)
      if (retval .ne. nf_noerr) CALL abort('Error reading term: P')

    Endif

    retval = nf_inq_varid(ncid, pair_str, varid)

    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Cannot find term: ',pair_str
      CALL abort(msg)
    endif

    retval = nf_get_var_real(ncid, varid, gain)
    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Error reading term: ',pair_str
      CALL abort(msg)
    endif

    rnet_kalman_stnpt_US = .true.
    retval = nf_close(ncid)
    if (retval .ne. nf_noerr) then
      IF (iprt) THEN
        write(msg,'(a,a35)') 'Error closing Kalman station gain file = ',filein
        call wrf_message(msg)
      ENDIF
    endif

  endif   ! end if(no error opening file)

  RETURN
  END FUNCTION rnet_kalman_stnpt_US

  LOGICAL FUNCTION rnet_kalman_netpt_SG(inest, iprt, nvol, dir, fil, ndp,   &
                                        nz, read_height, pair_str,          &
                                        len_pair_str, vls, gain)
!*************************************************************************
! Purpose: Open the specified surface network-point Kalman gain file,
!          and read the single-level data for each field.
!
! Return Value: TRUE if the requested file is found,
!               FALSE otherwise
!
! NOTE: The form of the filename is SGNNIIIJJJ.nc, where NN is the nest,
!       III and JJJ are the WRF i and j grid coordinates, respectively. 
!       The data is stored into the appropriate corner point for later
!       interpolation to the observation, from the obs-network coordinates.
!       equivalent location as (III,JJJ) on the WRF grid. 
!
! This subroutine reads a single file of Kalman column data, for one of
! the following "x" locations relative to the observation point:
!
!     x       x    <---- obs-netwk pts to the nw & ne of obs, respectively
!        o         <---- obs location
!
!     x       x    <---- obs-netwk pts to the sw & se of obs, respectively
!*************************************************************************

  IMPLICIT NONE
  include 'netcdf.inc'

  integer,      intent(in)  :: inest   ! nest level
  logical,      intent(in)  :: iprt    ! print flag
  integer,      intent(in)  :: nvol    ! read unit
  character*19, intent(in)  :: dir     ! directory containing file to open and read
  character*13, intent(in)  :: fil     ! file containing Kalman data at obs netw pt  
  integer,      intent(in)  :: ndp     ! no. of gain coeffs for each var and z lev
  integer,      intent(in)  :: nz      ! no. of z-levels to read in 
!  character*2,  intent(in)  :: scn     ! symbol for which corner pt (sw, se, nw, ne) 
  logical,      intent(in)  :: read_height ! height flag (if true, read ht instead of prs)
  integer,                     intent(in) :: len_pair_str  ! Length of pair string
  character(len=len_pair_str), intent(in) :: pair_str  ! Pair string for reading Kalman gains (e.g., "VrU")
  real,         intent(inout) :: vls(nz) ! pressure (or ht) levels at obs-netw points
  real,         intent(inout) :: gain(ndp,nz) ! Kalman gain at obs-netw points
  
! Local variables
  character*33 filein     ! full file path of Kalman data file to read in
  logical OPENED, exist   ! logicals for read unit
  real lat, lon           ! temp vars for reading Kalman lat/lon fields
  integer ilev, idp       ! loop counters
!  integer inp, jnp        ! Kalman obs-netwk indices for storing data
  character(len=200) :: msg ! Argument to wrf_message
  integer isize           ! data size, read from file
  integer ijump           ! data jump, read from file

  Integer :: numg
  integer :: ncid         ! netCDF file id
  integer :: varid        ! netCDF variable id
  integer :: retval       ! netCDF return code

! Initialize
  rnet_kalman_netpt_SG = .false.

! Open file and read Kalman data
  filein(1:19)  = dir(1:19)
  filein(20:20) = '/'
  filein(21:33) = fil(1:13)
  numg = NINT(SQRT(float(ndp)))

! Open the file. NF_NOWRITE tells netCDF we want read-only access.
  retval = nf_open(filein, NF_NOWRITE, ncid)

  if (retval .ne. nf_noerr) then
    IF (iprt) THEN
      write(msg,'(a,a33)') 'Cannot open Kalman network gain file = ',filein
      call wrf_message(msg)
    ENDIF

  else  ! Notify and proceed

    IF (iprt) THEN
      write(msg,'(a,a33)')'Opening Kalman network gain file = ',filein
      call wrf_message(msg)
    ENDIF

! Read the data
    If(read_height) then

      retval = nf_inq_varid(ncid, 'PH', varid)
      if (retval .ne. nf_noerr) CALL abort('Cannot find term: PH')

      retval = nf_get_var_real(ncid, varid, vls(1))
      if (retval .ne. nf_noerr) CALL abort('Error reading term: PH')

    Else

      retval = nf_inq_varid(ncid, 'P', varid)
      if (retval .ne. nf_noerr) CALL abort('Cannot find term: P')

      retval = nf_get_var_real(ncid, varid, vls(1))
      if (retval .ne. nf_noerr) CALL abort('Error reading term: P')

    Endif

    retval = nf_inq_varid(ncid, pair_str, varid)

    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Cannot find term: ',pair_str
      CALL abort(msg)
    endif

    retval = nf_get_var_real(ncid, varid, gain)
    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Error reading term: ',pair_str
      CALL abort(msg)
    endif

    rnet_kalman_netpt_SG = .true.
    retval = nf_close(ncid)
    if (retval .ne. nf_noerr) then
      IF (iprt) THEN
        write(msg,'(a,a35)') 'Error closing Kalman network gain file = ',filein
        call wrf_message(msg)
      ENDIF
    endif

  endif   ! end if(no error opening file)

  RETURN
  END FUNCTION rnet_kalman_netpt_SG

  LOGICAL FUNCTION rnet_kalman_netpt_UG(inest, iprt, nvol, dir, fil, ndp,  &
                                        nz, read_height, pair_str,         &
                                        len_pair_str, vls, gain)
!*************************************************************************
! Purpose: Open the specified upper-air network-point Kalman gain file,
!          and read the column data for each field.
!
! Return Value: TRUE if the requested file is found,
!               FALSE otherwise
!
! NOTE: The form of the filename is UGNNIIIJJJ.nc, where NN is the nest,
!       III and JJJ are the WRF i and j grid coordinates, respectively. 
!       The data is stored into the appropriate corner point for later
!       interpolation to the observation, from the obs-network coordinates.
!       equivalent location as (III,JJJ) on the WRF grid. 
!
! This subroutine reads a single file of Kalman column data, for one of
! the following "x" locations relative to the observation point:
!
!     x       x    <---- obs-netwk pts to the nw & ne of obs, respectively
!        o         <---- obs location
!
!     x       x    <---- obs-netwk pts to the sw & se of obs, respectively
!*************************************************************************

  IMPLICIT NONE
  include 'netcdf.inc'

  integer,      intent(in)  :: inest   ! nest level
  logical,      intent(in)  :: iprt    ! print flag
  integer,      intent(in)  :: nvol    ! read unit
  character*19, intent(in)  :: dir     ! directory containing file to open and read
  character*13, intent(in)  :: fil     ! file containing Kalman data at obs netw pt  
  integer,      intent(in)  :: ndp     ! no. of gain coeffs for each var and z lev
  integer,      intent(in)  :: nz      ! no. of z-levels to read in 
  logical,      intent(in)  :: read_height ! height flag (if true, read ht instead of prs)
  integer,                     intent(in) :: len_pair_str  ! Length of pair string
  character(len=len_pair_str), intent(in) :: pair_str  ! Pair string for reading Kalman gains (e.g., "VrU")
  real,         intent(inout) :: vls(nz) ! pressure (or ht) levels at obs-netw points
  real,         intent(inout) :: gain(ndp,nz) ! Kalman gain at obs-netw points
  
! Local variables
  character*33 filein     ! full file path of Kalman data file to read in
  logical OPENED, exist   ! logicals for read unit
  real lat, lon           ! temp vars for reading Kalman lat/lon fields
  integer ilev, idp       ! loop counters
!  integer inp, jnp        ! Kalman obs-netwk indices for storing data
  character(len=200) :: msg ! Argument to wrf_message
  integer isize           ! data size, read from file
  integer ijump           ! data jump, read from file

  Integer :: numg
  integer :: ncid         ! netCDF file id
  integer :: varid        ! netCDF variable id
  integer :: retval       ! netCDF return code

! Initialize
  rnet_kalman_netpt_UG = .false.

! Open file and read Kalman data
  filein(1:19)  = dir(1:19)
  filein(20:20) = '/'
  filein(21:33) = fil(1:13)
  numg = NINT(SQRT(float(ndp)))

! Open the file. NF_NOWRITE tells netCDF we want read-only access.
  retval = nf_open(filein, NF_NOWRITE, ncid)

  if (retval .ne. nf_noerr) then
    IF (iprt) THEN
      write(msg,'(a,a33)') 'Cannot open Kalman network gain file = ',filein
      call wrf_message(msg)
    ENDIF
  
  else  ! Notify and proceed

    IF (iprt) THEN
      write(msg,'(a,a33)')'Opening Kalman network gain file = ',filein
      call wrf_message(msg)
    ENDIF

! Read the data
    If(read_height) then

      retval = nf_inq_varid(ncid, 'PH', varid)
      if (retval .ne. nf_noerr) CALL abort('Cannot find term: PH')

      retval = nf_get_var_real(ncid, varid, vls)
      if (retval .ne. nf_noerr) CALL abort('Error reading term: PH')

    Else

      retval = nf_inq_varid(ncid, 'P', varid)
      if (retval .ne. nf_noerr) CALL abort('Cannot find term: P')

      retval = nf_get_var_real(ncid, varid, vls)
      if (retval .ne. nf_noerr) CALL abort('Error reading term: P')

    Endif

    retval = nf_inq_varid(ncid, pair_str, varid)

    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Cannot find term: ',pair_str
      CALL abort(msg)
    endif

    retval = nf_get_var_real(ncid, varid, gain)
    if (retval .ne. nf_noerr) then
      write(msg,'(2a)') 'Error reading term: ',pair_str
      CALL abort(msg)
    endif

    rnet_kalman_netpt_UG = .true.
    retval = nf_close(ncid)
    if (retval .ne. nf_noerr) then
      IF (iprt) THEN
        write(msg,'(a,a35)') 'Error closing Kalman network gain file = ',filein
        call wrf_message(msg)
      ENDIF
    endif

  endif   ! end if(no error opening file)

  RETURN
  END FUNCTION rnet_kalman_netpt_UG

  SUBROUTINE get_sw_netwk_grdpt(rio_mg, rjo_mg, lbdy, lskip, Nwe, Nsn, ig, jg, wutype)
!-----------------------------------------------------------------------------
!  PURPOSE: Return the (integer-valued) southwest network point, relative to
!           the an input (real-valued) location. The application is to find
!           the (isw,jsw) coordinate, given an input obs location.
! 
!-----------------------------------------------------------------------------
  IMPLICIT NONE
  
  real,    intent(in)  :: rio_mg   ! x coord of observation on mass-pt grid stagger
  real,    intent(in)  :: rjo_mg   ! y coord of observation on mass-pt grid stagger
  integer, intent(in)  :: lbdy     ! start index for Kalman obs network
  integer, intent(in)  :: lskip    ! skip index for Kalman obs network
  integer, intent(in)  :: Nwe      ! size of obs netw grid, west-east dim
  integer, intent(in)  :: Nsn      ! size of obs netw grid, south-north dim
  integer, intent(out) :: ig       ! equivalent wrf coord for isw
  integer, intent(out) :: jg       ! equivalent wrf coord for jsw
  Character(Len=4),intent(in) :: wutype  ! COMM or KUPX
! Local variables
  integer :: lrio       ! int(rio_mg)
  integer :: lrjo       ! int(rjo_mg)
  integer :: isw, jsw   ! Kalman obs network coordinate to southwest of obs loc

  lrio = nint(rio_mg)
  lrjo = nint(rjo_mg)

!!!! wuyh for radar case study only 20160323
  if ( wutype .eq. "KPUX" .OR. wutype .eq. "KUEX" .OR. wutype .eq. "KTWX") then
   isw = 1 + NINT(float((lrio-lbdy-1)/lskip))
   jsw = 1 + NINT(float((lrjo-lbdy)/lskip))
   ig = lbdy + (isw-1)*lskip + 1       ! ig is equiv position as isw, in wrf coord
   jg = lbdy + (jsw-1)*lskip
  elseif ( wutype .eq. "KDDC") then
   isw = 1 + NINT(float((lrio-lbdy)/lskip))
   jsw = 1 + NINT(float((lrjo-lbdy-1)/lskip))
   ig = lbdy + (isw-1)*lskip          ! ig is equiv position as isw, in wrf coord
   jg = lbdy + (jsw-1)*lskip + 1
  else
   isw = 1 + NINT(float((lrio-lbdy)/lskip))
   jsw = 1 + NINT(float((lrjo-lbdy)/lskip))
   ig = lbdy + (isw-1)*lskip           ! ig is equiv position as isw, in wrf coord
   jg = lbdy + (jsw-1)*lskip           ! jg is equiv position as jsw, in wrf coord 
  endif
!!!! wuyh for radar case study only 20160323

  Return
  END SUBROUTINE get_sw_netwk_grdpt

!------------- ----------------------------------------------------------------
  SUBROUTINE do_kalman_accounting(niobf, kalmax, nstat, nstaw, nlast,      &
                                  vdim,                                    &
                                  nndgv,nkalr,Klast,kn_loc, ndp, iprt,     &
                                  its,ite, jts,jte, ide, jde,              &
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
                                  mp_local_dummask,                        &
#endif
                                  inest, ob_is_kalman,                     &
                                  rio,rjo,                                 &
! for output use only
                                  vobs,xtime,timeob,                       &
                                  oblat,oblon,                             &
! end for output use only
                                  local_kalman2obs,gain_at_ob_local,       &
                                  obs2kalman,gain_at_ob,filesave)
!-----------------------------------------------------------------------------
! For indexing into array vobs
  USE module_fddaobs_utilities
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  USE module_dm,ONLY:get_full_kalman_flags,get_full_kalman_gain_vector
#endif
!  PURPOSE: Perform accounting tasks for Kalman obs processing:
!           1) Message pass the local ob_is_kalman array so that each processor
!              gets a global version.
!           2) Using the full ob_is_kalman array, produce the kalman2obs array
!              which maps the Kalman obs list to the full obs list. This
!              includes removing obsolete entries from the kalman2obs list.
!           3) Moving the Kalman-gains from the gain_at_ob_local array into
!              the gain_at_ob array. This first requires mapping from the local
!              Kalman-obs index to the global Kalman-obs index.
!           4) Shift gain_at_ob array to account for obsolete obs.
! 
!  NOTE: Precondition: kalmax > 0
!--------------------------------------------------------------------------
  IMPLICIT NONE
  
  INTEGER, INTENT(IN)  :: vdim          ! dimension for array vobs
  INTEGER, INTENT(IN)  :: nndgv
  INTEGER, INTENT(IN)  :: niobf         ! maximum number of observations
  INTEGER, INTENT(IN)  :: kalmax        ! maximum number of kalman obs
  INTEGER, INTENT(IN)  :: nstat         ! total number of obs read in this model step 
  INTEGER, INTENT(IN)  :: nstaw         ! total number of obs in current time window
  INTEGER, INTENT(IN)  :: nlast         ! last obs of valid obs, prev window, still active
  INTEGER, INTENT(IN)  :: nkalr         ! #old kalman obs to remove
  INTEGER, INTENT(IN)  :: klast         ! last kgain ob in current window before read new obs
  INTEGER, INTENT(IN)  :: kn_loc        ! total kalman-type obs read in by this processor
  INTEGER, INTENT(IN)  :: ndp           ! product of Kalman angles times number of coeffs
  LOGICAL, INTENT(IN)  :: iprt          ! print flag
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  LOGICAL, INTENT(IN)  :: MP_LOCAL_DUMMASK(NIOBF)  ! Mask for work to be done on this processor
!! wuyh added 20121211
!!  LOGICAL  :: MC_LOCAL_DUMMASK(NIOBF)  ! Mask for work to be done on this processor
!! wuyh added 20121211 end

  LOGICAL MP_LOCAL_KALMASK(KALMAX)  ! Mask for interpolation done by this cpu
  LOGICAL,external :: Tile_mask
#endif
  INTEGER, INTENT(IN)  :: inest         ! nest level
  INTEGER, INTENT(INOUT) :: ob_is_kalman(niobf)  ! flags specifying Kalman-type obs

!! wuyh added 20121207
  INTEGER,INTENT(INOUT) :: filesave(niobf)
!! wuyh added 20121207

! wuyh moved sub kalman here
! wuyh removed 20120823
!  Integer, INTENT(INOUT) :: station_kgain_data(niobf)  ! flags stn gain (integer, wuyh 20111017)
! wuyh removed 20120823
  INTEGER, INTENT(IN) :: local_kalman2obs(kalmax) ! map local kalman indx to obs indx
  REAL,    INTENT(IN) :: gain_at_ob_local(ndp,kalmax) ! gain at ob locs
  INTEGER :: kalman2obs(kalmax)   ! mapping from kalman index to obs index

  INTEGER, INTENT(INOUT) :: obs2kalman(niobf) !mapp obs indx to kalman indx
  REAL,    INTENT(INOUT) :: gain_at_ob(ndp,kalmax) ! gain at ob locs
  REAL,    INTENT(IN)    :: rio(niobf)    ! west-east coord (non-stagger)
  REAL,    INTENT(IN)    :: rjo(niobf)    ! south-north coord (non-stagger)
  integer :: lrio, lrjo  ! integer parts of obs (i,j) location
  real    :: rio_mg      ! west-east coord of obs on mass-pt grid stagger
  real    :: rjo_mg      ! south_north coord of obs on mass-pt grid stagger
!  Integer,Parameter::nscale=1     ! flag do gain scaling 1: do, 0: no scaling
  Integer::ncc,kv
  INTEGER, INTENT(IN)  :: its,ite, jts,jte ! tile dims.
  INTEGER, INTENT(IN) :: ide               ! Ending i coord for domain
  INTEGER, INTENT(IN) :: jde               ! Ending j coord for domain

! for output use only
  Real,Parameter::obsri = 54.5 , obsrj = 62.5   ! for output
!  Real,Parameter::obsri = 54.533 , obsrj = 46.843   ! for output
  Integer::iu
  Integer::kmove
  Character(Len=20)::FMT
  Integer::numg
  REAL,    INTENT(IN)    :: vobs(vdim,niobf)    ! vertical coord 
  REAL,    INTENT(IN)    :: timeob(niobf)    ! time of ob
  Real,    INTENT(IN)    :: xtime
  Real::wscal

!! wuyh added 20121206
!!! for check gain 
  Real   , intent(in) :: oblat(niobf)      ! obs latitude
  Real   , intent(in) :: oblon(niobf)      ! obs longitude
!! wuyh added 20121206 end

! end for output use only

! wuyh added 20120823 end
!wuyh moved sub kalman here end
! Local variables
  integer :: totkal      ! running total of Kalman-type observations, this window
  integer :: nkal        ! number of Kalman obs
  integer :: n           ! loop index over observations
  integer :: kn          ! loop index over Kalman-type observations
  integer :: kn_glo      ! global-processor index for Kalman-type observations
  integer :: kne         ! index of first Kalman-type obs exceeding max allowed 
  integer :: save_n      ! temp to hold last n in loop that is a kalman-type ob 
  integer :: k2ob_abs    ! index used for constructing local_kalman2obs map 
  character(len=200) :: msg       ! Argument to wrf_message

  integer :: nn   !tmpvar
!*****************************************************************************************
! Example showing ob_is_kalman and kalman2obs arrays along with full list of observations.
! On iteration n, there are a total of 7 obs, with the 2nd, 4th, and 6th having Kalman
! gain data available (ob_is_kalman is T for these obs). For the 2nd and 6th obs, gain
! data at surrounding points on the obs network will be stored for later interpolation to
! the exact ob locations. Hence kalman2obs(1) and kalman2obs(3) are +2 and +6, respectively,
! the positive values meaning that the Kalman gains will be interpolated from obs network
! locations. The station_kgain_data(4) value of T means that the 4th ob has data available
! right AT the ob station, so no spatial interpolation will be necessary. This is designated
! by setting kalman2obs(2) = -4, so the negative value means station data. Note that the
! station_kgain_data array contains only local processor information, that is, 
! station_kgain_data(n) for a given processor will have a value of T only if station data
! is available AND ob n is on that processor's domain patch (mp_local_dummask).
!
! Iteration n:
! n, timeob:          1  6.00   2  6.00   3  6.00   4  6.00   5  6.00   6  6.28   7  6.53
! ob_is_kalman:       0         1         0         1         0         1         0
! station_kgain_data: F         F         F         T         F         F         F
! kalman2obs:         2   -4    6    0    0    0    0
!
! Iteration n+1:
! The first five obs from the full list of obs in iter n have become obsolete, but only 2
! were Kalman (the 2nd and 4th). So ob 6 from iter n moves to ob 1 in iter n+1.
! n, timeob:          1  6.28   2  6.53
! ob_is_kalman:       1         0
! station_kgain_data: F         F
! kalman2obs:         1         0
!
! Variable nkalr tells this routine how many kalman entries have become obsolete.
!*****************************************************************************************
! wuyh added 20120708
      numg = NINT(SQRT(float(ndp)))
      write(FMT,'("(",I0,"F6.2)")')numg
! wuyh added 20120708 end

  if(kalmax.le.0) then
      write(msg,*) 'FATAL ERROR in obs nudging subroutine do_kalman_accounting.'
      CALL wrf_message(msg)
      write(msg,*) 'Precondition kalmax > 0 is violated! KALMAX = ',kalmax
      CALL wrf_error_fatal (msg)
  endif
!!! wuyh added 20121211
!!!  mc_local_dummask=mp_local_dummask    !make sure gainname uses the same dummask
!!! wuyh added 20121211


! At this point the kalman flag array is local only, so message pass to make a global copy
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  CALL get_full_kalman_flags(nstat,niobf,mp_local_dummask,ob_is_kalman)
!! wuyh added 20121207
  CALL get_full_kalman_flags(nstat,niobf,mp_local_dummask,filesave)
!! wuyh added 20121207 end

!!! wuyh added 20111017
!  CALL get_full_kalman_flags(nstat,niobf,mp_local_dummask,station_kgain_data )
!!! wuyh added 20111017 end
#endif

! Report the number of obs for which Kalman gain data has been processed.
  if(iprt) then
    totkal=0
    do n = 1, nstat
      totkal = totkal + ABS(ob_is_kalman(n))
!ajb: suppress this output
!      write(277,1277)n,ob_is_kalman(n),oblat(n),oblon(n),         &
!         vobs(Prs,n),vobs(Hgt,n),rio(n)-0.5,rjo(n)-0.5,           &
!         filesave(n)/100000, filesave(n)-(filesave(n)/100000)*100000
    enddo
    call wrf_message("")

!!!! wuyh modified 20140218
!!    write(msg,'(a,i2,2a,i6)') '*** OBS NUDGING: NEST ',inest,     &
!!           ': Total number of obs in ',      &
!!    'current time window for which Kalman gain data was read: ', &
!!                               totkal
    write(msg,'(a,i2,a,I6,1x,I6)') '*** Do_Kalman_accounting: NEST ',inest,     &
           ': Total obs and obs with Kalman gain are : ', &
                               nstat,totkal
!!!! wuyh modified 20140218 end

    call wrf_message(msg)
  endif
 1277 Format(I6,1x,I2,2(1x,F7.2),2(1x,F11.2),2(1x,F6.2),2(1x,I5))

! From ob_is_kalman flags, set entries of kalman2obs array
! NOTE: kalmax is strictly greater than zero
! kne=niobf+1
!!! wuyh remove  kne=niobf+1
  kne=niobf
  nkal=0
  save_n=0
!!! wuyh added 20120823
  kalman2obs = 0
  obs2kalman = 0
!!! wuyh added 20120823 end

!! wuyh modified 20121211
  k2o : do n = 1, nstat
!  k2o : do n = 1, niobf
!! wuyh modified 20121211
    if(ABS(ob_is_kalman(n)).EQ.1) then
      nkal=nkal+1
      if(nkal.gt.kalmax) then
        kne = n       ! Store index of first Kalman-type obs exceeding limit
        EXIT k2o
      endif
      kalman2obs(nkal) = n
!wuyh added 20120823
      obs2kalman(n)=nkal
!wuyh added 20120823 end
      save_n = n      ! Save n to remember last kalman-type ob in list
    endif
  enddo k2o

! Clear out ob_is_kalman array if kalmax has been exceeded.
  do n = MIN(kne,nstat+1), niobf
    ob_is_kalman(n) = 0
    filesave(n) = -1000010
  enddo

!!! wuyh commented out 20120823
!  if(nkal.lt.kalmax) then
!    do n = nkal+1, kalmax
!      kalman2obs(n) = 0
!    enddo
!  else
!    if(iprt) then
!      call wrf_message("")
!      write(msg,'(2a,i6)') '*** OBS NUDGING WARNING!: The number of Kalman-type obs ',  &
!                          'for this time window exceeds obs_kalmax_max = ',kalmax
!      call wrf_message(msg)
!      write(msg,'(2a,i6)') '*** OBS NUDGING WARNING!: Cressman weighting will be used ', &
!                          'for all obs in this time window with OBS No > ',save_n
!      call wrf_message(msg)
!      call wrf_message("")
!     call wrf_message(msg)
!     write(msg,'(a,i6)') 'FATAL ERROR: SUB IN4DOB, obs_kalman_max = ',kalmax
!     CALL wrf_error_fatal (msg)
!    endif
!  endif
!!! wuyh commented out 20120823

! Now do accounting for Kalman obs that have become obsolete. That is, shift the gain_at_ob
! array entries accordingly. Since gain_at_ob values have not yet been assigned for the newly
! read obs in this time window (done later in subr kalman), the only entries needing shifting
! here are for Kalman obs that were read in previously. 

 ! wuyh added 20120905 
!!  kmove = 0
!!  do n=1,nkalr
!!    if(ABS(ob_is_kalman(n)).EQ.1) then
!!      kmove=kmove + 1
!!    endif
!!  enddo
 ! wuyh added 20120905 

! Compute klast
 !! lastk : do klast = nkal, 1, -1
 !!     if(kalman2obs(klast).le.nlast) EXIT lastk
 !! enddo lastk

!ajb: suppress this output
!  write(221,'(a,3(1x,I8))')'nlast,klast,nkalr=',nlast,klast,nkalr

! Remove obsolete obs and shift existing obs

  if(nkalr.gt.0) then
    do n = 1, min(klast,kalmax-nkalr)
      gain_at_ob(:,n)=gain_at_ob(:,n+nkalr)
    enddo
  endif

!  write(221,*)kmove,nkal,klast
!  write(221,FMT)(gain_at_ob(iu,1,1),iu=1,ndp)

! Update the gain_at_ob array with local-processor contributions of the Kalman-gain values
! that were  read, interpolated, and stored earlier into the gain_at_ob_local array.
! This requires us to map the local Kalman-obs index to the global Kalman-obs index. We
! already have a local_kalman2obs map, so we need to now calculate the obs2kalman map. 

!!! wuyh commented out 20120823
!  obs2kalman=0
!  o2k: do n = 1, kalmax
!    if(kalman2obs(n).eq.0) EXIT o2k
!    k2ob_abs = kalman2obs(n)
!    obs2kalman(k2ob_abs)=n
!  enddo o2k
!!! wuyh commented out 20120823

! write(209,*)nlast+1,local_kalman2obs(1)
! write(209,*)  nstat,local_kalman2obs(kn_loc)

  Do kn = 1, kn_loc
    kn_glo = obs2kalman(local_kalman2obs(kn))
    if(kn_glo .gt. Klast) then
      gain_at_ob(:,kn_glo) = gain_at_ob_local(:,kn)
    else
      write(255,*)'assign kgain is wrong in do kalman account'
      write(255,*)'nlast,kn_loc,kn_glo=',nlast,kn_loc,kn_glo,xtime
!!!      stop 216
    endif
!    if(ABS(rio(kn_glo) - obsri)+ABS(rjo(kn_glo)-obsrj) .le. 1.0E-1 )then
!      write(275,*)kn_glo,rio(kn_glo),rjo(kn_glo),vobs(Prs,kn_glo),timeob(kn_glo)
!      write(275,FMT)(gain_at_ob(iu,kn_glo),iu=1,ndp)
!    endif
  Enddo

!!! wuyh moved sub kalman here 20120823
! Set mask for kalman-type obs that are to be handled by this processor
  nkal=0
  kmask : do n = 1, kalmax
    if(kalman2obs(n).eq.0) EXIT kmask
    k2ob_abs = abs(kalman2obs(n))

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!ajbmasspt
    rio_mg = rio(k2ob_abs) - 0.5    ! get x coord of obs on mass-pt stagger
    rjo_mg = rjo(k2ob_abs) - 0.5    ! get y coord of obs on mass-pt stagger
    lrio = nint(rio_mg)
    lrjo = nint(rjo_mg)

!!! wuyh added 20120820
    if (lrio .le. 1 .OR. lrio .ge. ide-1 .OR. lrjo .le. 1 .OR. &
        lrjo .ge. jde-1 )then
      write(257,*)n,timeob(n),k2ob_abs,lrio,lrjo
    endif
!!! wuyh added 20120820
    lrio = MAX0(1,lrio)
    lrio = MIN0(lrio,ide-1)
    lrjo = MAX0(1,lrjo)
    lrjo = MIN0(lrjo,jde-1)
!!!! wuyh removed 20120817
    MP_LOCAL_KALMASK(N) = TILE_MASK(lrio, lrjo, its, ite, jts, jte)
#endif
    nkal = nkal+1
  enddo kmask

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
! Now message pass gain_at_ob(:,n) so each processor has gains for each ob n. 
  CALL get_full_kalman_gain_vector(nkal, ndp, kalmax, totkal,   &
                                   mp_local_kalmask, gain_at_ob)
#endif

!  Do n=1,nkal
!    k2ob_abs = abs(kalman2obs(n))
!   if(abs(rio(k2ob_abs)-obsri)+abs(rjo(k2ob_abs)-obsrj) .le. 1.0E-1 .and.    &
!      abs(timeob(k2ob_abs)-12.00000) .le. 1.0E-4 ) then
!      write(285,*)k2ob_abs,rio(k2ob_abs),rjo(k2ob_abs),vobs(Prs,k2ob_abs),   &
!       nlast,nstat,timeob(k2ob_abs),xtime/60.
!      write(285,FMT)(gain_at_ob(iu,n),iu=1,ndp)
!   endif
!  Enddo

!!  write(*,*) 'end of do_kalman_accounting!'

!!! wuyh moved sub kalman here 20120823

  END SUBROUTINE do_kalman_accounting

  SUBROUTINE kgain_filg_info(n, lrio, lrjo, d1, d2, fnsw, fnse, fnnw, fnne, rid)
!*************************************************************************
! Purpose: Diagnostic only. Print Kalman file information to standard out,
!          to determine what Kalman-gain filenames have been generated.
!          This routine is for network filenames: DNNIIIJJJ.txt
!*************************************************************************

  IMPLICIT NONE

  integer,      intent(in) :: n          ! index
  integer,      intent(in) :: lrio       ! i grid coordinate 
  integer,      intent(in) :: lrjo       ! j grid coordinate 
  character(*), intent(in) :: d1         ! directory name 1
  character(*), intent(in) :: d2         ! directory name 2
  character(*), intent(in) :: fnsw       ! southwest kgain filename
  character(*), intent(in) :: fnse       ! southeast kgain filename 
  character(*), intent(in) :: fnnw       ! northwest kgain filename
  character(*), intent(in) :: fnne       ! northeast kgain filename
  character*2,  intent(in) :: rid        ! region id

  if(rid.eq.'II') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i3),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' d2 = ',d2,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',fnse,                         &
                          ' fnw = ',fnnw,                         &
                          ' fne = ',fnne,                         &
                          ' INTERIOR  pt = (',lrio,',',lrjo,')'
  elseif(rid.eq.'SS') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i3),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' d2 = ',d2,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',fnse,                         &
                          ' fnw = ',"         ",                  &
                          ' fne = ',"         ",                  &
                          ' SOUTH     pt = (',lrio,',',lrjo,')'
  elseif(rid.eq.'NN') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i2),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',fnsw,                         &
                          ' fnw = ',"         ",                  &
                          ' fne = ',"         ",                  &
                          ' NORTH     pt = (',lrio,',',lrjo,')'

  elseif(rid.eq.'WW') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i3),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' d2 = ',d2,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',"         ",                  &
                          ' fnw = ',fnnw,                         &
                          ' fne = ',"         ",                  &
                          ' WEST      pt = (',lrio,',',lrjo,')'
  elseif(rid.eq.'EE') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i3),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' d2 = ',d2,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',"         ",                  &
                          ' fnw = ',fnnw,                         &
                          ' fne = ',"         ",                  &
                          ' EAST      pt = (',lrio,',',lrjo,')'

  elseif(rid.eq.'SW') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i3),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' d2 = ',d2,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',"         ",                  &
                          ' fnw = ',"         ",                  &
                          ' fne = ',"         ",                  &
                          ' SOUTHWEST pt = (',lrio,',',lrjo,')'

  elseif(rid.eq.'SE') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i3),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' d2 = ',d2,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',"         ",                  &
                          ' fnw = ',"         ",                  &
                          ' fne = ',"         ",                  &
                          ' SOUTHEAST pt = (',lrio,',',lrjo,')'
  elseif(rid.eq.'NW') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i3),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' d2 = ',d2,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',"         ",                  &
                          ' fnw = ',"         ",                  &
                          ' fne = ',"         ",                  &
                          ' NORTHWEST pt = (',lrio,',',lrjo,')'

  elseif(rid.eq.'NE') then
        write(6,'(a,i7,2(a,a10),4(a,a9),2(a,i3),a/)')             &
                          'N= ',n,                                &
                          ' d1 = ',d1,                            &
                          ' d2 = ',d2,                            &
                          ' fsw = ',fnsw,                         &
                          ' fse = ',"         ",                  &
                          ' fnw = ',"         ",                  &
                          ' fne = ',"         ",                  &
                          ' NORTHEAST pt = (',lrio,',',lrjo,')'

  endif
  END SUBROUTINE kgain_filg_info

  SUBROUTINE kgain_fils_info(n, rio_mg, rjo_mg, d1, d2, fnst)
!*************************************************************************
! Purpose: Diagnostic only. Print Kalman file information to standard out,
!          to determine what Kalman-gain filenames have been generated.
!          This routine is for station filenames: SNNIIIJJJ.AAA.BBB.txt
!*************************************************************************

  IMPLICIT NONE

  integer,      intent(in) :: n          ! index
  real,         intent(in) :: rio_mg     ! i grid coordinate on mass grid
  real,         intent(in) :: rjo_mg     ! j grid coordinate on mass grid
  character(*), intent(in) :: d1         ! directory name 1
  character(*), intent(in) :: d2         ! directory name 2
  character(*), intent(in) :: fnst       ! station

  write(6,'(a,i7,2(a,a10),(a,a9),2(a,f6.2),a/)')              &
                    'N= ',n,                                  &
                    ' d1 = ',d1,                              &
                    ' d2 = ',d2,                              &
                    ' fnst = ',fnst,                          &
                    ' STATION pt = (',rio_mg,',',rjo_mg,')'

  END SUBROUTINE kgain_fils_info

  SUBROUTINE interp_kgain_stn(Nz, ndp, timeob, interp_in_height, vob,    &
                              rko, vls_t1, vls_t2, gain_t1, gain_t2, obgain)

!-----------------------------------------------------------------------------
!  PURPOSE: Interpolate the gain to the ob location using the column gain, 
!           column pressure, and ob pressure. 
!--------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: Nz                ! vertical dim for station data column 
  INTEGER, INTENT(IN)  :: ndp               ! product of Kalman-gain #angles x #coeffs
  REAL,    INTENT(IN)  :: timeob            ! time of observation (hours)
  LOGICAL, INTENT(IN)  :: interp_in_height  ! if true, use height, otherwise pressure
  REAL,    INTENT(IN)  :: vob               ! ob vert coord in hPa or m, mass grid
  REAL,    INTENT(IN)  :: rko               ! vertical level of ob
  REAL,    INTENT(IN)  :: vls_t1(Nz)        ! pressure or ht col at ob stn (hPa or m), time 1
  REAL,    INTENT(IN)  :: vls_t2(Nz)        ! pressure or ht col at ob stn (hPa or m), time 2
  REAL,    INTENT(IN)  :: gain_t1(ndp,Nz)   ! kalman column gains at ob station, time 1
  REAL,    INTENT(IN)  :: gain_t2(ndp,Nz)   ! kalman column gains at ob station, time 2
  REAL,    INTENT(OUT) :: obgain(ndp)       ! output (interpolated gain at obs site)

! Local variables
  integer :: itimob   ! timeob, integer part
  real    :: tfr      ! time fraction
  real    :: gt1(ndp) ! gain at time 1, interpolated vertically
  real    :: gt2(ndp) ! gain at time 2, interpolated vertically

! Compute time fraction
  itimob = timeob
  tfr = timeob - itimob

! Interpolation gain vertically, for past file time t1.
  CALL stnvint(Nz, ndp, interp_in_height, vob, rko, vls_t1, gain_t1, gt1)

! Interpolation gain vertically, for next file time t2.
  CALL stnvint(Nz, ndp, interp_in_height, vob, rko, vls_t2, gain_t2, gt2)

! Interpolate in time.
  obgain = (1-tfr)*gt1 + tfr*gt2

  END SUBROUTINE interp_kgain_stn

   SUBROUTINE interp_kgain_nwk(Nx, Ny, Nz, ndp,                           &
                               timeob, rio, rjo, interp_in_height, vob,   &
                               lbdy, lskip,                               &
                               vls_knw_t1, vls_knw_t2,                    &
                               gain_knw_t1, gain_knw_t2, obgain)

!-----------------------------------------------------------------------------
!  PURPOSE: Horizontally and vertically interpolate the Kalman gain values
!           from the obs network pts to the obs site at times t1 and t2.
!--------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: Nx                ! west-east dim for Kalman network data
  INTEGER, INTENT(IN)  :: Ny                ! south-north dim for Kalman network data
  INTEGER, INTENT(IN)  :: Nz                ! vertical dim for station data column 
  INTEGER, INTENT(IN)  :: ndp               ! product of Kalman-gain #angles x #coeffs
  REAL,    INTENT(IN)  :: timeob            ! time of observation (hours)
  REAL,    INTENT(IN)  :: rio               ! x coord of observation on mass-pt grid stagger
  REAL,    INTENT(IN)  :: rjo               ! y coord of observation on mass-pt grid stagger
  LOGICAL, INTENT(IN)  :: interp_in_height  ! if true, use height, otherwise pressure
  REAL,    INTENT(IN)  :: vob               ! ob vert coord in hPa or m, mass grid
  INTEGER, INTENT(IN)  :: lbdy              ! starting index for Kalman obs network
  INTEGER, INTENT(IN)  :: lskip             ! skip index for Kalman obs network
  REAL,    INTENT(IN)  :: vls_knw_t1(2,2,Nz) ! prs or ht lvls (hPa,m) at netw pts, t1
  REAL,    INTENT(IN)  :: vls_knw_t2(2,2,Nz) ! prs or ht lvls (hPa,m) at netw pts, t2
  REAL,    INTENT(IN)  :: gain_knw_t1(ndp,2,2,Nz) ! Kalman gn at obs-netw pts t1
  REAL,    INTENT(IN)  :: gain_knw_t2(ndp,2,2,Nz) ! Kalman gn at obs-netw pts t2
  REAL,    INTENT(OUT) :: obgain(ndp)   ! output (interpolated gain at obs site)

! Local variables
  integer :: itimob   ! timeob, integer part
  real    :: tfr      ! time fraction
  real    :: gt1(ndp) ! gain at time 1, interpolated vertically
  real    :: gt2(ndp) ! gain at time 2, interpolated vertically
  real    :: fx          ! interpolation fraction in west-east coord
  real    :: fy          ! interpolation fraction in south-nth coord
  real    :: fzt1(4)     ! interpol fractions in vertical coord, at time t1
  real    :: fzt2(4)     ! interpol fractions in vertical coord, at time t2
  integer :: klt1(4)     ! vertical level just above ob, at time t1
  integer :: klt2(4)     ! vertical level just above ob, at time t2
  integer :: ilx         ! west-east coord of obs-netw point just southwest of ob 
  integer :: jly         ! sth-north coord of obs-netw point just southwest of ob 
  character*2 :: ptype   ! Kalman point type (SS, SW, SE, NN, NW, NE, etc)
  integer :: n           ! loop counter
!  integer :: U,V,T,Q       ! Kalman gain variable indices

!  data U/1/,V/2/,T/3/,Q/4/

! Compute time fraction
  itimob = timeob
  tfr = timeob - itimob

! First get interpolation factors.
  CALL interp_horiz(Nx, Ny, lbdy, lskip, rio, rjo, fx, fy, ptype, ilx, jly)

  CALL interp_vert (ptype, Nx, Ny, Nz, lbdy, lskip, ilx, jly,            &
                    interp_in_height, vob, vls_knw_t1, fzt1, klt1)

  CALL interp_vert (ptype, Nx, Ny, Nz, lbdy, lskip, ilx, jly,            &
                    interp_in_height, vob, vls_knw_t2, fzt2, klt2)

! Interpolate gain
  CALL interp2site(ptype, ilx, jly, klt1, Nz, ndp, fx, fy, fzt1,         &
                   gain_knw_t1, gt1)

  CALL interp2site(ptype, ilx, jly, klt2, Nz, ndp, fx, fy, fzt2,         &
                   gain_knw_t2, gt2)

     obgain = (1-tfr)*gt1 + tfr*gt2     ! time interpolation

!! Interpolate U gain
!
!  CALL interp2site(ptype, ilx, jly, klt1, Nz, ndp, fx, fy, fzt1,         &
!                   gain_knw_t1(1,1,1,1,U), gt1)
!
!  CALL interp2site(ptype, ilx, jly, klt2, Nz, ndp, fx, fy, fzt2,         &
!                   gain_knw_t2(1,1,1,1,U), gt2)
!
!  obgain(:,U) = (1-tfr)*gt1 + tfr*gt2     ! time interpolation
!
!! Interpolate V gain
!  CALL interp2site(ptype, ilx, jly, klt1, Nz, ndp, fx, fy, fzt1,         &
!                   gain_knw_t1(1,1,1,1,V), gt1)
!
!  CALL interp2site(ptype, ilx, jly, klt2, Nz, ndp, fx, fy, fzt2,         &
!                   gain_knw_t2(1,1,1,1,V), gt2)
!
!  obgain(:,V) = (1-tfr)*gt1 + tfr*gt2     ! time interpolation
!
!! Interpolate T gain
!  CALL interp2site(ptype, ilx, jly, klt1, Nz, ndp, fx, fy, fzt1,         &
!                   gain_knw_t1(1,1,1,1,T), gt1)
! 
!  CALL interp2site(ptype, ilx, jly, klt2, Nz, ndp, fx, fy, fzt2,         &
!                   gain_knw_t2(1,1,1,1,T), gt2)
! 
!  obgain(:,T) = (1-tfr)*gt1 + tfr*gt2     ! time interpolation
!
!! Interpolate Q gain
!  CALL interp2site(ptype, ilx, jly, klt1, Nz, ndp, fx, fy, fzt1,         &
!                   gain_knw_t1(1,1,1,1,Q), gt1)
! 
!  CALL interp2site(ptype, ilx, jly, klt2, Nz, ndp, fx, fy, fzt2,         &
!                   gain_knw_t2(1,1,1,1,Q), gt2)
! 
!  obgain(:,Q) = (1-tfr)*gt1 + tfr*gt2     ! time interpolation

  END SUBROUTINE interp_kgain_nwk

!-----------------------------------------------------------------------------
  SUBROUTINE stnvint(Nz, ndp, interp_in_height, Vob, rko, Vg, gain, obgain)
!-----------------------------------------------------------------------------
!  PURPOSE: Interpolate the gain to the ob location using the column gain, 
!           column pressure (or height), and ob pressure (or height). 
!--------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: Nz            ! vertical dim for station data column 
  INTEGER, INTENT(IN)  :: ndp           ! product of Kalman-gain #angles x #coeffs
  LOGICAL, INTENT(IN)  :: interp_in_height  ! if true, use height, otherwise pressure
  REAL,    INTENT(IN)  :: Vob           ! pressure or height at obs (hPa or m)
  REAL,    INTENT(IN)  :: rko           ! vertical level of ob (=1 => surface ob)
  REAL,    INTENT(IN)  :: Vg(Nz)        ! pressure or height col at ob stn (hPa or m)
  REAL,    INTENT(IN)  :: gain(ndp, Nz) ! kalman gain at ob station
  REAL,    INTENT(OUT) :: obgain(ndp)   ! output (interpolated gain at obs site)

! Local variables
  integer :: mz       ! k-level just above Vob
  integer :: k        ! loop counter over pressure levels
  real :: wz
!!! wuyh added 20121218
  Integer::kw,ncc
!!! wuyh added 20121218 end

  character (len=256):: msg

  ncc = (1+ndp)/2

! Initialize
  obgain=float(0)

! First check for surface ob
  if(rko.eq.1.0) then
    obgain(:)=gain(:,1)
  else

! Check Vob against model levels, from earth surface to top
    if(interp_in_height) then  ! USE HEIGHT

      Do k=1,Nz      ! Check to see if model height >= Vob
        IF(Vg(k).GE.Vob)then
          mz=k
          if(mz.EQ.1)then
            obgain(:)=gain(:,1)
            go to 10 
          else
            wz=(Vob-Vg(mz))/(Vg(mz-1)-Vg(mz))
            obgain(:)=(1.-wz)*gain(:,mz)+wz*gain(:,mz-1)
!!            write(210,1210)wz,gain(ncc,mz),gain(ncc,mz-1),obgain(ncc)
1210        Format(F8.4,3(1x,F8.2))
            go to 10 
          endif
        Endif
      Enddo
      IF(Vg(Nz).LT.Vob) obgain(:)=gain(:,Nz) !top layer
      10 Continue

    else                       ! USE PRESSURE

      Do k=1,Nz      ! Check to see if model pressure <= Vob
        IF(Vg(k).LE.Vob)then
          mz=k
          if(mz.EQ.1)then
            obgain(:)=gain(:,1)
            go to 20 
          else
! wuyh modified 20120511
!          wz=(Vob-Vg(mz))/(Vg(mz-1)-Vg(mz))
!          wz=ALOG(Vob/Vg(mz))/ALOG(Vg(mz-1)/Vg(mz))
!!            wz=float(NINT(ALOG(Vob/Vg(mz))/ALOG(Vg(mz-1)/Vg(mz))))
!           wz = float(0)
! wuyh modified end 20120511
!!            obgain(:)=(1.-wz)*gain(:,mz)+wz*gain(:,mz-1)
!!! wuyh added 20121218
            kw = NINT(ALOG(Vob/Vg(mz))/ALOG(Vg(mz-1)/Vg(mz)))
            obgain(:)= gain(:,mz-kw)
!!            write(210,1310)kw,mz,gain(ncc,mz-kw),obgain(ncc)
1310        Format(2(1x,I3),2(1x,F8.2))
!!! wuyh added 20121218 end
            go to 20 
          endif
        Endif
      Enddo
      IF(Vg(Nz).GT.Vob) obgain(:)=gain(:,Nz) !top layer
      20 Continue
    endif !interp_in_height

  endif ! surface ob

  END SUBROUTINE stnvint

!-----------------------------------------------------------------------------
  SUBROUTINE get_klev(Nz, interp_in_height, Vob, Vg, klev, wz)
!-----------------------------------------------------------------------------
!  PURPOSE: Get the model k-level immediately above the observation location,
!           and compute the interpolation fraction.
!-----------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: Nz            ! vertical dim for station data column 
  LOGICAL, INTENT(IN)  :: interp_in_height  ! if true, use height, otherwise pressure
  REAL,    INTENT(IN)  :: Vob           ! pressure or height at obs (hPa or m)
  REAL,    INTENT(IN)  :: Vg(Nz)        ! pressure or height col on netwrk (hPa or m)
  INTEGER, INTENT(OUT) :: klev          ! model level immediately above ob location
  REAL,    INTENT(OUT) :: wz            ! interpolation fraction in vertical coord

! Local variables
  integer :: mz       ! k-level just above Vob
  integer :: k        ! loop counter over vertical model levels
  character (len=256):: msg

  wz=float(0)
  klev=0

! Check Vob against model levels, from earth surface to top

  if(interp_in_height) then  ! USE HEIGHT

    Do k=1,Nz      ! Check to see if model height >= Vob
        IF(Vg(k).GE.Vob) then
          mz=k
          if(mz.EQ.1) then              !bottom layer
           go to 10
          else
           wz=(Vg(mz)-Vob)/(Vg(mz)-Vg(mz-1))
           go to 10
          endif
        Endif
    Enddo
    IF(Vg(Nz).LT.Vob) mz=Nz+1  !top layer
 10 Continue
    klev = mz

  else                       ! USE PRESSURE

    Do k=1,Nz      ! Check to see if model pressure <= Vob
        IF(Vg(k).LE.Vob) then
          mz=k
          if(mz.EQ.1) then              !bottom layer
           go to 20
          else
! wuyh added 20120511
!           wz=(Vob-Vg(mz))/(Vg(mz-1)-Vg(mz))
           wz=ALOG(Vob/Vg(mz))/ALOG(Vg(mz-1)/Vg(mz))

! wuyh added end 20120511
           go to 20
          endif
        Endif
    Enddo
    IF(Vg(Nz).GT.Vob) mz=Nz+1  !top layer
 20 Continue
    klev = mz
  endif
  END SUBROUTINE get_klev

!------------------------------------------------------------------------------
  SUBROUTINE interp2site(ptype, ilx, jly, mz, Nz, ndp, &
                         wx, wy, wz, gain, obgain)
!------------------------------------------------------------------------------
!  PURPOSE: Use pre-computed interpolation factors to interpolate Kalman gains
!           from a given obs network to the exact observation site.
!           grid coordinates, and Vob is the obs pressure or height. (Nx,Ny,Nz)
!           is the obs-netw coordinate, not the WRF grid.
!
! Note: Absolute obs-network coords ilx and jly are not longer used. To save
! memory, we now keep gains only at the 4 network pts surrounding the obs.
! These are stored into gain(:,1,1,:),gain(:,1,2,:),gain(:,2,1,:),gain(:,2,2,:)
!------------------------------------------------------------------------------
  IMPLICIT NONE

  CHARACTER*2, INTENT(IN) :: ptype ! pt type, II=interior, SS=south, SE=southeast, etc
  INTEGER, INTENT(IN)  :: ilx      ! w-e coord of obs-netw pt just southwest of ob 
  INTEGER, INTENT(IN)  :: jly      ! s-n coord of obs-netw pt just southwest of ob
  INTEGER, INTENT(IN)  :: mz(4)    ! k-level just above ob
  INTEGER, INTENT(IN)  :: Nz       ! vertical dim for Kalman obs network
  INTEGER, INTENT(IN)  :: ndp      ! number of Kalman-gain parameters 
  REAL,    INTENT(IN)  :: wx       ! interpol fraction in west-east coord
  REAL,    INTENT(IN)  :: wy       ! interpol fraction in sth-north coord
  REAL,    INTENT(IN)  :: wz(4)    ! interpol fractions in vertical coord
  REAL,    INTENT(IN)  :: gain(ndp, 2, 2, Nz) ! local obs-netw gain
  REAL,    INTENT(OUT) :: obgain(ndp)  ! output (interpolated gain at obs site)

! Local variables
  integer lx, ly
  real, Dimension(ndp) :: ga,gb,gc,gd  ! intermediate gain values before interpolation
  logical :: iprt     ! print flag
  character (len=256):: msg

  iprt=.false.

  lx=1
  ly=1

! Initialize
  obgain=float(0)
  ga=float(0)
  gb=float(0)
  gc=float(0)
  gd=float(0)

! Determine the nearest obs netwk grid-pt to the southwest of the obs location
! (Tests for mz(1)=1, mz(2)=1, etc are necessary to avoid illegal memory access)

  ptype_select: SELECT CASE (ptype)
  CASE ("II")
      if(mz(1).eq.1) then
         ga(:)=gain(:,lx,ly,1)                              
      elseif(mz(1).eq.Nz+1) then
         ga(:)=gain(:,lx,ly,Nz)                              
      else
         ga(:)=(1.-wz(1))*gain(:,lx,ly,mz(1))+wz(1)*gain(:,lx,ly,mz(1)-1)
      endif

      if(mz(2).eq.1) then
        gb(:)=gain(:,lx+1,ly,1)
      elseif(mz(2).eq.Nz+1) then
        gb(:)=gain(:,lx+1,ly,Nz)
      else
        gb(:)=(1.-wz(2))*gain(:,lx+1,ly,mz(2))+wz(2)*gain(:,lx+1,ly,mz(2)-1)
      endif

      if(mz(3).eq.1) then
        gc(:)=gain(:,lx,ly+1,1)
      elseif(mz(3).eq.Nz+1) then
        gc(:)=gain(:,lx,ly+1,Nz)
      else
        gc(:)=(1.-wz(3))*gain(:,lx,ly+1,mz(3))+wz(3)*gain(:,lx,ly+1,mz(3)-1)
      endif

      if(mz(4).eq.1) then
        gd(:)=gain(:,lx+1,ly+1,1)
      elseif(mz(4).eq.Nz+1) then
        gd(:)=gain(:,lx+1,ly+1,Nz)
      else
        gd(:)=(1.-wz(4))*gain(:,lx+1,ly+1,mz(4))+wz(4)*gain(:,lx+1,ly+1,mz(4)-1)
      endif

      obgain=(1.-wy)*((1.-wx)*ga+wx*gb)+wy*((1.-wx)*gc+wx*gd)

  CASE ("SS")
      if(mz(1).eq.1) then
        ga(:)=gain(:,lx,1,1)
      elseif(mz(1).eq.Nz+1) then
        ga(:)=gain(:,lx,1,Nz)
      else
        ga(:)=(1.-wz(1))*gain(:,lx,1,mz(1))+wz(1)*gain(:,lx,1,mz(1)-1)
      endif
      if(mz(2).eq.1) then
        gb(:)=gain(:,lx+1,1,1)
      elseif(mz(2).eq.Nz+1) then
        gb(:)=gain(:,lx+1,1,Nz)
      else
        gb(:)=(1.-wz(2))*gain(:,lx+1,1,mz(2))+wz(2)*gain(:,lx+1,1,mz(2)-1)
      endif
      obgain=(1.-wx)*ga+wx*gb

  CASE ("SW")
      if(mz(1).eq.1) then
        obgain(:)=gain(:,1,1,1)
      elseif(mz(1).eq.Nz+1) then
        obgain(:)=gain(:,1,1,Nz)
      else
        obgain(:)=(1.-wz(1))*gain(:,1,1,mz(1))+wz(1)*gain(:,1,1,mz(1)-1)
      endif
      
  CASE ("SE")
      if(mz(1).eq.1) then
        obgain(:)=gain(:,1,1,1)
      elseif(mz(1).eq.Nz+1) then
        obgain(:)=gain(:,1,1,Nz)
      else
        obgain(:)=(1.-wz(1))*gain(:,1,1,mz(1))+wz(1)*gain(:,1,1,mz(1)-1)
      endif

  CASE ("WW")
      if(mz(1).eq.1) then
        ga(:)=gain(:,1,ly,1)
      elseif(mz(1).eq.Nz+1) then
        ga(:)=gain(:,1,ly,Nz)
      else
        ga(:)=(1.-wz(1))*gain(:,1,ly,mz(1))+wz(1)*gain(:,1,ly,mz(1)-1)
      endif

      if(mz(2).eq.1) then
        gb(:)=gain(:,1,ly+1,1)
      elseif(mz(2).eq.Nz+1) then
        gb(:)=gain(:,1,ly+1,Nz)
      else
        gb(:)=(1.-wz(2))*gain(:,1,ly+1,mz(2))+wz(2)*gain(:,1,ly+1,mz(2)-1)
      endif
      obgain=(1.-wy)*ga+wy*gb

  CASE ("NW")
      if(mz(1).eq.1) then
        obgain(:)=gain(:,1,1,1)
      elseif(mz(1).eq.Nz+1) then
        obgain(:)=gain(:,1,1,Nz)
      else
        obgain(:)=(1.-wz(1))*gain(:,1,1,mz(1))+wz(1)*gain(:,1,1,mz(1)-1)
      endif

  CASE ("NN")
      if(mz(1).eq.1) then
        ga(:)=gain(:,lx,1,1)
      elseif(mz(1).eq.Nz+1) then
        ga(:)=gain(:,lx,1,Nz)
      else
        ga(:)=(1.-wz(1))*gain(:,lx,1,mz(1))+wz(1)*gain(:,lx,1,mz(1)-1)
      endif
      if(mz(2).eq.1) then
        gb(:)=gain(:,lx+1,1,1)
      elseif(mz(2).eq.Nz+1) then
        gb(:)=gain(:,lx+1,1,Nz)
      else
        gb(:)=(1.-wz(2))*gain(:,lx+1,1,mz(2))+wz(2)*gain(:,lx+1,1,mz(2)-1)
      endif
      obgain=(1.-wx)*ga+wx*gb

  CASE ("NE")
      if(mz(1).eq.1) then
        obgain(:)=gain(:,1,1,1)
      elseif(mz(1).eq.Nz+1) then
        obgain(:)=gain(:,1,1,Nz)
      else
        obgain(:)=(1.-wz(1))*gain(:,1,1,mz(1))+wz(1)*gain(:,1,1,mz(1)-1)
      endif

  CASE ("EE")
      if(mz(1).eq.1) then
        ga(:)=gain(:,1,ly,1)
      elseif(mz(1).eq.Nz+1) then
        ga(:)=gain(:,1,ly,Nz)
      else
        ga(:)=(1.-wz(1))*gain(:,1,ly,mz(1))+wz(1)*gain(:,1,ly,mz(1)-1)
      endif
      if(mz(2).eq.1) then
        gb(:)=gain(:,1,ly+1,1)
      elseif(mz(2).eq.Nz+1) then
        gb(:)=gain(:,1,ly+1,Nz)
      else
        gb(:)=(1.-wz(2))*gain(:,1,ly+1,mz(2))+wz(2)*gain(:,1,ly+1,mz(2)-1)
      endif
      obgain=(1.-wy)*ga+wy*gb

  END SELECT ptype_select

  END SUBROUTINE interp2site

!-----------------------------------------------------------------------------
  SUBROUTINE interp_vert (ptype, Nx, Ny, Nz, lbdy, lskip, lx, ly,          &
                          interp_in_height, Vob, Vg, wz, klev) 
!-----------------------------------------------------------------------------
!  PURPOSE: Get the vertical interpolation fractions wz needed for interpolation
!           from grid to ob site (see subroutine grid2site). The exact
!           observation site is (Rx,Ry,Vob) in WRF coordinates, where Vob is
!           the obs pressure or height. (If interp_in_height is true, use height)
!           (Nx,Ny,Nz) is the obs-netw coordinate, not the WRF grid.
!  EX: WRF grid dimx=82 points in west-east and dimy=92 points in south-nth:
!       I=lbdy,dimy-lbdy,lskip, lbdy=13, lskip=2, therefore Nx=(92-13-13)/2+1=34
!       J=lbdy,dimx-lbdy,lskip, lbdy=13, lskip=2, therefore Ny=(82-13-13)/2+1=29
!       Nz=number of half sigma levels
!--------------------------------------------------------------------------
  IMPLICIT NONE

  CHARACTER*2, INTENT(IN) :: ptype ! pt type, II=interior, SS=south, SE=southeast, etc
  INTEGER, INTENT(IN)  :: Nx           ! global west-east dim for Kalman obs network
  INTEGER, INTENT(IN)  :: Ny           ! global south_nth dim for Kalman obs network
  INTEGER, INTENT(IN)  :: Nz           ! vertical dim for Kalman obs network
  INTEGER, INTENT(IN)  :: lbdy         ! start index for Kalman obs network
  INTEGER, INTENT(IN)  :: lskip        ! skip index for Kalman obs network
  INTEGER, INTENT(IN)  :: lx           ! w-e coord (non-stag) of ob netw pt sw of ob
  INTEGER, INTENT(IN)  :: ly           ! s-n coord (non-stag) of ob netw pt sw of ob
  LOGICAL, INTENT(IN)  :: interp_in_height  ! if true, use height, otherwise pressure
  REAL,    INTENT(IN)  :: Vob               ! ob vert coord in hPa or m, mass grid
  REAL,    INTENT(IN)  :: Vg(2, 2, Nz) ! local netw prs or ht cols (hPa,m)
  REAL,    INTENT(OUT) :: wz(4)        ! fraction in vertical coord
  INTEGER, INTENT(OUT) :: klev(4)      ! vertical level just above ob

! Local variables
  integer :: mz        ! k-level just above Vob
  integer :: k         ! loop counter over pressure (or height) levels
  real    :: Vcol1(Nz) ! buffer1 for pressure or height column 
  real    :: Vcol2(Nz) ! buffer2 for pressure or height column 
  real    :: Vcol3(Nz) ! buffer3 for pressure or height column
  real    :: Vcol4(Nz) ! buffer4 for pressure or height column
  logical :: iprt      ! print flag
  character (len=256):: msg

  real ajbRx,ajbRy

! initialize
  iprt=.false.
  wz=0.
  klev=0

! Print obs netwk grid-pt to the southwest of the ob location
! Note that these indices are no longer used by this routine, other
! than for diagnostic purposes.
  if(iprt) write(6,*) 'lx = ',lx,' ly = ',ly

! Copy network pressure (or height) columns into buffers
    do k=1,Nz
      Vcol1(k) = Vg(1,1,k)
      Vcol2(k) = Vg(2,1,k)
      Vcol3(k) = Vg(1,2,k)
      Vcol4(k) = Vg(2,2,k)
    enddo

! Get vertical fractions at surrounding columns:
! (lx,ly), (lx+1,ly), (lx,ly+1), (lx+1,ly+1)

  ptype_select: SELECT CASE (ptype)

  CASE ("II")
    if(iprt)  write(6,*) 'INTERIOR'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))
    CALL get_klev(Nz, interp_in_height, Vob, Vcol2, klev(2), wz(2))
    CALL get_klev(Nz, interp_in_height, Vob, Vcol3, klev(3), wz(3))
    CALL get_klev(Nz, interp_in_height, Vob, Vcol4, klev(4), wz(4))

  CASE ("SS")
    if(iprt) write(6,*) 'SOUTH'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(2), wz(2))

  CASE ("SW")
    if(iprt) write(6,*) 'SOUTHWEST corner'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))

  CASE ("SE")
    if(iprt) write(6,*) 'SOUTHEAST corner'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))

  CASE ("WW")
    if(iprt) write(6,*) 'WEST'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))
    CALL get_klev(Nz, interp_in_height, Vob, Vcol3, klev(2), wz(2))

  CASE ("NW")
    if(iprt) write(6,*) 'NORTHWEST corner'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))

  CASE ("NN")
    if(iprt) write(6,*) 'NORTH'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(2), wz(2))

  CASE ("NE")
    if(iprt) write(6,*) 'NORTHEAST corner'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))

  CASE ("EE")
    if(iprt) write(6,*) 'EAST'
    CALL get_klev(Nz, interp_in_height, Vob, Vcol1, klev(1), wz(1))
    CALL get_klev(Nz, interp_in_height, Vob, Vcol3, klev(2), wz(2))

  END SELECT ptype_select

  END SUBROUTINE interp_vert

!-----------------------------------------------------------------------------
  SUBROUTINE interp_horiz (Nx, Ny, lbdy, lskip, Rx, Ry, wx, wy, ptype, lx, ly) 
!-----------------------------------------------------------------------------
!  PURPOSE: Get the horizontal interpolation fractions wx, wy, needed for
!           interpolation from grid to ob site (see subroutine interp2site). The
!           exact WRF horizontal coordinates of the observation site is (Rx,Ry).
!           (Nx,Ny) is the obs-netw coordinate, not the WRF grid.
!  NOTE: THIS ROUTINE MUST BE CALLED BEFORE CALLING SUBROUTINE INTERP_VERT, which
!           requires as input coordinates lx, ly, and point type ptype.
!  EX: WRF grid dimx=82 points in west-east and dimy=92 points in south-nth:
!       I=lbdy,dimy-lbdy,lskip, lbdy=13, lskip=2, therefore Nx=(92-13-13)/2+1=34
!       J=lbdy,dimx-lbdy,lskip, lbdy=13, lskip=2, therefore Ny=(82-13-13)/2+1=29
!--------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: Nx         ! global west-east dim for Kalman obs network
  INTEGER, INTENT(IN)  :: Ny         ! global south_nth dim for Kalman obs network
  INTEGER, INTENT(IN)  :: lbdy       ! start index for Kalman obs network
  INTEGER, INTENT(IN)  :: lskip      ! skip index for Kalman obs network
  REAL,    INTENT(IN)  :: Rx         ! observation west-east coord (mass-pt stagger)
  REAL,    INTENT(IN)  :: Ry         ! observation south-north coord (mass-pt stagger)
  REAL,    INTENT(OUT) :: wx         ! fraction in west-east coord 
  REAL,    INTENT(OUT) :: wy         ! fraction in south-nth coord
  CHARACTER*2, INTENT(OUT) :: ptype  ! pt type, II=interior, SS=south, SE=southeast, etc
  INTEGER, INTENT(OUT) :: lx         ! west-east coord of obs-netw pt just southwest of ob
  INTEGER, INTENT(OUT) :: ly         ! nth-south coord of obs-netw pt just southwest of ob

! Local variables
  integer :: ml       ! integer part of Rx, Ry
  logical :: iprt     ! print flag
  character (len=256):: msg

  real ajbRx,ajbRy

  ajbRx=Rx
  ajbRy=Ry
  iprt=.false.

!ajb diagnostics
!  if(iprt) then
!    write(6,'/,2(a,f8.3)') 'interp_horiz: Rx = ',Rx,' Ry = ',Ry
!    if( (Rx.lt.24.7.and.Rx.gt.24.6).and.(Ry.lt.18.8.and.Ry.gt.18.7) ) then
!       ajbRx=24.999
!       ajbRy=17.001
!       write(6,*) 'OVERRIDING INTERIOR PT (Rx,Ry) TO TEST INTERPOLATION:'
!       write(6,*) 'ajbRx = ',ajbRx,' ajbRy = ',ajbRy
!    elseif( (Rx.lt.13.5.and.Rx.gt.13.4).and.(Ry.lt.2.8.and.Ry.gt.2.7) ) then
!       ajbRx=14.95
!       ajbRy= 3.01
!       write(6,*) 'OVERRIDING SOUTH PT (Rx,Ry) TO TEST INTERPOLATION:'
!       write(6,*) 'ajbRx = ',ajbRx,' ajbRy = ',ajbRy
!    elseif( (Rx.lt.2.05.and.Rx.gt.2.04).and.(Ry.lt.18.6.and.Ry.gt.18.5) ) then
!       ajbRx=2.0
!       ajbRy=18.99
!       write(6,*) 'OVERRIDING WEST PT (Rx,Ry) TO TEST INTERPOLATION:'
!       write(6,*) 'ajbRx = ',ajbRx,' ajbRy = ',ajbRy
!    elseif( (Rx.lt.16.3.and.Rx.gt.16.2).and.(Ry.lt.80.3.and.Ry.gt.80.2) ) then
!       ajbRx=15.99 
!       ajbRy=80.0
!       write(6,*) 'OVERRIDING NORTH PT (Rx,Ry) TO TEST INTERPOLATION:'
!       write(6,*) 'ajbRx = ',ajbRx,' ajbRy = ',ajbRy
!    endif
!  endif

! Determine the nearest obs netwk grid-pt to the southwest of the obs location
  ml = int(ajbRx)
  if(ml.LT.lbdy) then
    lx = -1
  else
    lx = 1 + (ml-lbdy)/lskip
  endif
!  ml = int(Ry)
  ml = int(ajbRy)
  if(ml.LT.lbdy) then
    ly = -1
  else
    ly = 1 + (ml-lbdy)/lskip
  endif
  if(iprt) write(6,*) 'lx = ',lx,' ly = ',ly

! Interpolate to the obs site using the 4 surrounding points:
! (lx,ly), (lx+1,ly), (lx,ly+1), (lx+1,ly+1)

!***********************************************
! CASE 1: Obs location is WITHIN the obs network
!***********************************************
  If((lx.GE.1.AND.lx.LT.Nx).AND.(ly.GE.1.AND.ly.LT.Ny)) then
      if(iprt)  write(6,*) 'INTERIOR'
      ptype="II"
      wx=(ajbRx-float(lbdy))/float(lskip)+float(1-lx)      !west-east coord
      wy=(ajbRy-float(lbdy))/float(lskip)+float(1-ly)      !south-nth coord

!*************************************************
! CASE 2: Obs location is SOUTH of the obs network
!*************************************************
  Elseif(ly.LE.0) then

      if(lx.GE.1.AND.lx.LT.Nx) then   ! Handle south point
        if(iprt) write(6,*) 'SOUTH'
        ptype = "SS"
        wx=(ajbRx-float(lbdy))/float(lskip)+float(1-lx)
        wy=0.

      elseif(lx.LE.0) then          ! Handle southwest corner
        if(iprt) write(6,*) 'SOUTHWEST corner'
        ptype = "SW"
        wx=0.
        wy=0.

      elseif(lx.GE.Nx) then         ! Handle southeast corner
        if(iprt) write(6,*) 'SOUTHEAST corner'
        ptype = "SE"
        wx=1.
        wy=0.

      else
        write(msg,'(a,2i5)') 'in interp_horiz SOUTH case, lx,ly= ',lx,ly
        CALL wrf_message(msg)
        write(msg,'(a51)') 'FATAL ERROR in obs-nudging subroutine interp_horiz.'
        CALL wrf_error_fatal (msg)
      endif

!************************************************
! CASE 3: Obs location is WEST of the obs network
!************************************************
  Elseif(lx.LE.0) then
      if(ly.GE.1.AND.ly.LT.Ny) then    ! Handle west point
        if(iprt) write(6,*) 'WEST'
        ptype = "WW"
        wx=0.
        wy=(ajbRy-float(lbdy))/float(lskip)+float(1-ly)

      elseif(ly.GE.Ny)then       ! Handle northwest corner
        if(iprt) write(6,*) 'NORTHWEST corner'
        ptype = "NW"
        wx=0.
        wy=1.

      else
        write(msg,'(a,2i5)') 'in interp_horiz WEST case, lx,ly= ',lx,ly
        CALL wrf_message(msg)
        write(msg,'(a51)') 'FATAL ERROR in obs-nudging subroutine interp_horiz.'
        CALL wrf_error_fatal (msg)
      endif

!*************************************************
! CASE 4: Obs location is NORTH of the obs network
!*************************************************
  Elseif(ly.GE.Ny) then           !NORTH
      if(lx.GE.1.AND.lx.LT.Nx) then
        if(iprt) write(6,*) 'NORTH'
        ptype = "NN"
        wx=(ajbRx-float(lbdy))/float(lskip)+float(1-lx)
        wy=1.

      elseif(lx.GE.Nx) then         !northeast corner
        if(iprt) write(6,*) 'NORTHEAST corner'
        ptype = "NE"
        wx=1.
        wy=1.

      else
        write(msg,'(a,2i5)') 'in interp_horiz NORTH case, lx,ly= ',lx,ly
        CALL wrf_message(msg)
        write(msg,'(a51)') 'FATAL ERROR in obs-nudging subroutine interp_horiz.'
        CALL wrf_error_fatal (msg)
      endif

!************************************************
! CASE 5: Obs location is EAST of the obs network
!************************************************
  Elseif(lx.GE.Nx) then           !EAST
      if(ly.GE.1.AND.ly.LT.Ny) then 
        if(iprt) write(6,*) 'EAST'
        ptype = "EE"
        wx=1.
        wy=(Ry-float(lbdy))/float(lskip)+float(1-ly)
      else
        write(msg,'(a,2i5)') 'in interp_horiz EAST case, lx,ly= ',lx,ly
        CALL wrf_message(msg)
        write(msg,'(a51)') 'FATAL ERROR in obs-nudging subroutine interp_horiz.'
        CALL wrf_error_fatal (msg)
      endif

  Else
        write(msg,'(a,2i5)') 'in interp_horiz UNKNOWN case, lx,ly= ',lx,ly
        CALL wrf_message(msg)
        write(msg,'(a51)') 'FATAL ERROR in obs-nudging subroutine interp_horiz.'
        CALL wrf_error_fatal (msg)
  Endif

  END SUBROUTINE interp_horiz

  LOGICAL FUNCTION TILE_MASK(iloc, jloc, its, ite, jts, jte)
!------------------------------------------------------------------------------
! PURPOSE: Check to see if an i, j grid coordinate is in the tile index range.
!      
! Returns: TRUE if the grid coordinate (ILOC,JLOC) is in the tile defined by
!                  tile-range indices (its,jts) and (ite,jte)
!          FALSE otherwise.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iloc
  INTEGER, INTENT(IN) :: jloc
  INTEGER, INTENT(IN) :: its
  INTEGER, INTENT(IN) :: ite
  INTEGER, INTENT(IN) :: jts
  INTEGER, INTENT(IN) :: jte

! Local variables
  LOGICAL :: retval

  TILE_MASK = (iloc .LE. ite .AND. iloc .GE. its .AND.    &
               jloc .LE. jte .AND. jloc .GE. jts )

  RETURN
  END FUNCTION TILE_MASK

#endif
!-----------------------------------------------------------------------
! End subroutines for in_kgain
!-----------------------------------------------------------------------

#if ( EM_CORE == 1 )
!------------------------------------------------------------------------------
! Begin subroutine get_parent_QC and its subroutines
!------------------------------------------------------------------------------
  SUBROUTINE get_parent_QC(grid, prt_max, prt_freq, fdob, qc_domain_prt, &
                           qc_domain, qc_alerts, inest)

  USE module_domain
! For indexing into array vobs
  USE module_fddaobs_utilities

!*******************************************************************************
! Purpose: Get QC values for each observation from the requested parent domain.
!          Set qc_domain_prt for each observation included in the printed report.
!          Note that fdob%desobs(40,n) is used to over-ride inheritance for
!          observation n if no matching ob is found in the parent domain.
!*******************************************************************************
  IMPLICIT NONE

  TYPE(domain) :: grid
  INTEGER, intent(in) :: prt_max       ! max number of entries of obs for diagnostic printout
  INTEGER, intent(in) :: prt_freq      ! frequency (in obs index) for diagnostic printout.
  TYPE(fdob_type), intent(inout)  :: fdob  ! derived data type for obs data
  INTEGER, intent(out):: qc_domain_prt(prt_max)     ! domain used for QC, for ob report
  INTEGER, intent(in) :: qc_domain         ! domain used for QC values
  INTEGER, intent(in) :: qc_alerts         ! flag to activate/suppress qc alert messages
  INTEGER, intent(in) :: inest             ! nest level

! Local variables
  TYPE(domain) , POINTER         :: parent_ptr
  TYPE(domain) , POINTER         :: qc_parent_ptr
  character(len=200) :: msg  ! Argument to wrf_message
  integer :: ilev            ! Nest levels up for fetching fdob 
  integer :: meas_count      ! Number of levels in sounding
  integer :: lev_in_ob       ! Level of sounding
  integer :: meas_countp     ! Number of levels in parent sounding
  integer :: lev_in_obp      ! Level of sounding in parent
  integer :: il              ! Counter for ilev and meas_count loops
  integer :: n               ! Counter for nstat loop, current nest
  integer :: m               ! Counter for nstat loop on qc mother domain
  integer :: i               ! loop variables for character conversion
  integer :: begin_next_search ! ob in parent domain to begin next search
  integer :: nmatch          ! number of obs matched with parent ob 
  integer :: nover           ! number of failured searches to find match 
  integer :: ndx             ! ob index in printed obs report
  integer :: max_ob_prt      ! max ob index for printed ob report
  logical :: match           ! flag to detect data mismatch
  real    :: tob, tobp
  integer, parameter :: NO_PARENT_QC = 1 
  character*10 :: stn, stnp
  character*14:: wudate

  LOGICAL, EXTERNAL :: wrf_dm_on_monitor  !externals

  if(qc_domain.gt.inest) then
    write(msg,*) 'QC_DOMAIN MUST BE LESS THAN INEST!'
    call wrf_message(msg)
    write(msg,*) 'QC_DOMAIN = ',qc_domain,' INEST = ',inest
    call wrf_message(msg)
    call wrf_error_fatal ( 'wrf_fddaobs_in: get_parent_QC STOP' )
    return

  elseif(qc_domain.eq.inest) then
    qc_domain_prt = inest
    return

  else 

! Initialize
    fdob%desobs(40,:) = 0   ! used to over-ride inheritance if no matching ob in parent domain
    max_ob_prt = prt_freq*(prt_max-1) + 1
    qc_domain_prt = inest   ! initially for all obs .. will reset to parent if match is found

! Initialize pointers to immediate parent
    ilev = inest - qc_domain 
    qc_parent_ptr => grid%parents(1)%ptr
    parent_ptr    => grid%parents(1)%ptr

! Set pointers to requested parent level
    inherit : do il = 1, ilev-1
      if(qc_parent_ptr%grid_id.eq.qc_domain) EXIT inherit
      qc_parent_ptr => parent_ptr%parents(1)%ptr
      parent_ptr    => qc_parent_ptr
    enddo inherit

! Other initialization
    begin_next_search = 1
    n=1 
    nmatch = 0
    nover = 0

    DO WHILE (n.le.fdob%nstat)

      tob = fdob%timeob(n)
      do i = 1,10
        stn(i:i) = char(fdob%stnid(i,n))
      enddo

      meas_count = nint(fdob%nlevs_ob(n))
      lev_in_ob  = nint(fdob%lev_in_ob(n))

      m = begin_next_search  ! for parent domain
      match = .false.
      DO WHILE (m .le. qc_parent_ptr%fdob%nstat)

        tobp = qc_parent_ptr%fdob%timeob(m)
        do i = 1,10
          stnp(i:i) = char(qc_parent_ptr%fdob%stnid(i,m))
        enddo

        meas_countp = nint(qc_parent_ptr%fdob%nlevs_ob(m))
        lev_in_obp  = nint(qc_parent_ptr%fdob%lev_in_ob(m))

        IF(tob .lt. tobp - 1.0E-4) Then
            if(qc_alerts.ge.4) then
              write(6,*)
              write(6,*) "nest domain obs time < parent domain obs time"
              write(6,'(2(a,i5),2(a,i3))')              &
                        'N = ',n,'  m = ',m,            &
                        '  meas_count = ',meas_count,   &
                        '  meas_countp = ',meas_countp
            endif

                !begin_next_search = 1  ! Note here by not setting begin_next_search, it will 
                                        ! take the index when last match is found.
            EXIT   ! Exit do loop. A match should have been found before this happens!

        ELSEIF(tob .gt. tobp + 1.0E-4) then   ! keep searching
          m = m+meas_countp
          CYCLE    ! Interate m loop

        ELSE                                              !time match 

          if(  stnp(1:6).eq.stn(1:6)                    &    !stnid match
              .and.  meas_count .eq. meas_countp        &    !obslev match
              .and. ABS(fdob%oblat(n) - qc_parent_ptr%fdob%oblat(m)) .le. 0.004  & 
              .and. ABS(fdob%oblon(n) - qc_parent_ptr%fdob%oblon(m)) .le. 0.004 ) then 
  
              if( fdob%ob_var_type(n).eq.qc_parent_ptr%fdob%ob_var_type(m) ) then
  
                 match = .true.
                 nmatch = nmatch + meas_count    
              
                 if(qc_alerts.ge.2) then
                   write(6,'(/,a,i2,2(a,i5),4(a,f9.3))')                        &
                            'MATCH found for nest ',inest,                      &
                            '  ob = ',n,' par = ',m,                            &   
                            '  vobs = ',fdob%vobs(1,n),                         &
                            '  parent vobs = ',qc_parent_ptr%fdob%vobs(1,m),    &
                            '  vobs_qc = ',fdob%vobs_qc(1,n),                   &
                            '  parent vobs_qc = ',qc_parent_ptr%fdob%vobs_qc(1,m)
                 endif
                 do il = 0, meas_count-1
                     fdob%vobs_qc(1,n+il) = qc_parent_ptr%fdob%vobs_qc(1,m+il)

                     if(n+il.le.max_ob_prt) then     ! Do this only for obs in report
                       if( mod((n+il-1),prt_freq).eq.0 ) then
                         ndx = 1 + (n+il-1)/prt_freq
                         qc_domain_prt(ndx) = qc_domain   !Store qc_domain for ob report
                       endif
                     endif
                 enddo
                 !begin_next_search = 1 ! save ob index for next search
                 begin_next_search = max(1, m - meas_countp) ! yliu 20160912: a match is found, 
                 EXIT                                        !          remember the searched point.
              else
                 m = m + meas_countp
                 CYCLE                                ! Not the same variables, so continue to next record
              endif  !end if same ob_var_type
          endif  !end if  stnp(1:10) .eq. stn(1:10)

        ENDIF ! tob = tobp obs time is the same

        m = m+meas_countp
      ENDDO  !loop over parents obs

      if(match) then
!       write(6,'(2(a,i3),a,/)') 'Obs ',n,' on nest matches ob ',m,' on coarse grid'
      else
         Do i=1,14
            wudate(i:i) = CHAR (fdob%wudate(i,n))  ! obs date  
         Enddo
         if(qc_alerts.ge.1) then
          write(msg,'(a,i5,a,i2,a,F8.2,1x,F8.2,1x,a6,1x,a)') 'ALERT ob ',n,' on nest ',inest, &
                    ' Parent QC is not found ', fdob%oblat(n),fdob%oblon(n),stn(1:10),wudate
          call wrf_message(msg)
         endif
      endif

! If no match in parent domain, assign NO_PARENT_QC flag value to qc.
      if(match .eq. .false.) then
        if(qc_alerts.ge.3) then
          write(6,'(/,a,i2,a,i5,2(a,f9.3))')              &
                   'NO MATCH found for nest ',inest,      &
                   '  ob = ',n,                           &   
                   '  vobs = ',fdob%vobs(1,n),            &
                   '  vobs_qc = ',fdob%vobs_qc(1,n)
        endif

        do il = 0, meas_count-1
          fdob%desobs(40,n+il) = NO_PARENT_QC
        enddo

        nover = nover + meas_count
      endif

      n = n+meas_count
    ENDDO  !loop over obs

    if(qc_alerts.ge.1) call wrf_message("")
  endif

! Write diagnostics for number of succesful and failed matches
  IF ( wrf_dm_on_monitor()) THEN
    write(6,999) "get_parent_QC: inest nmatch nfail nobs = ",inest,nmatch,nover,fdob%nstat
  Endif
  999 FORMAT(a,i2,3(1x,i7))

  END SUBROUTINE get_parent_QC

  SUBROUTINE abort(msg)
  IMPLICIT NONE
  character(len=200), intent(in) :: msg  ! Argument to wrf_message

  CALL wrf_message(msg)
  CALL wrf_abort

  RETURN
  END SUBROUTINE abort
#endif
!-----------------------------------------------------------------------
! End subroutines for get_parent_QC
!-----------------------------------------------------------------------
