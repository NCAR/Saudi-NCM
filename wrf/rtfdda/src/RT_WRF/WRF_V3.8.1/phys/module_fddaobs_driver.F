!WRF:MODEL_LAYER:PHYSICS
MODULE module_fddaobs_driver

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
 
CONTAINS

!-----------------------------------------------------------------------
SUBROUTINE fddaobs_driver( grid, inest, domdesc, domid,          &
               parid, restart,                                   &
               nudge_opt,                                        &
               iprt_errob,                                       &
               iprt_nudob,                                       &
               fdasta,                                           &
               fdaend,                                           &
               nudge_wind,                                       &
               nudge_temp,                                       &
               nudge_mois,                                       &
!!! wuyh added 20140626
               nudge_vr,nudge_rf,                                &
!!! wuyh added 20140626
               nudge_pstr,                                       &
               coef_wind,                                        &
               coef_temp,                                        &
               coef_mois,                                        &
!!! wuyh added 20140707
               coef_vr,coef_rf,                                  &
!!! wuyh added 20140707
               coef_pstr,                                        &
               rinsig,                                           &
               npfi,                                             &
               ionf,                                             &
               obs_prt_max,                                      &
               obs_prt_freq,                                     &
               idynin,                                           &
               dtramp,                                           &
               parent_grid_ratio,                                &
               maxdom, itimestep,                                &
! yliu 20090726
               xtime,                                            &
               dt, gmt, julday,                                  &
#if ( EM_CORE == 1 ) 
               fdob,                                             &
#endif
               max_obs,                                          &
! FCV additions begin
               qc_internal, qc_domain,                           &
               tolerance_p, tolerance_q,                         &
               tolerance_t, tolerance_u,                         &
               tolerance_p_sound, tolerance_q_sound,             &
               tolerance_t_sound, tolerance_u_sound,             &
! FCV additions end
               obs_vobs_dim,                                     &
               obs_errf_dim,                                     &
               nobs_ndg_vars,                                    &
               kalmax,                                           &
               nobs_scans,                                       &
               nstat, vobs, vobs_qc, errf,                       &
               dx, kpbl, ht,                                     &
               mut, muu, muv,                                    &
               msftx, msfty, msfux, msfuy, msfvx, msfvy,         &
               p_phy, t_tendf,                                   &
               ub, vb, tb,                                       &
               qvb,                                              &
!!! wuyh added 20140626
               ww,qr,                                            &
!!! wuyh added 20140626
               pbase, ptop, pp,                                  &
               phb, ph,                                          &
               th2,                                              &
               uratx, vratx, tratx,                              &
               ru_tendf, rv_tendf,                               &
               moist_tend, savwt,                                &
               z,                                                &
! wuyh added 20110630
               flagsnd, flagbog, desobs, oblat, oblon, stnid,    &
               qcu,wudate,wusource,                              &
               icvar, ivar, num_ndg_terms,                       &
               config_flags,                                     &
! wuyh added 20110630 end
               ids,ide, jds,jde, kds,kde,                        & ! domain dims
               ims,ime, jms,jme, kms,kme,                        & ! memory dims
               ips, ipe, jps, jpe, kps, kpe,                     & ! patch  dims
               its,ite, jts,jte, kts,kte                         ) ! tile   dims
!ajbij         i_start,i_end,j_start,j_end,kts,kte,num_tiles)

!------------------------------------------------------------------------------
  USE module_domain
  USE module_bc
  USE module_model_constants, ONLY : g, t0
  USE module_fddaobs_rtfdda
  USE module_fddaobs_utilities
!!! wuyh added 20110928
  USE module_configure, ONLY : grid_config_rec_type, model_config_rec
!!! wuyh added 20110928 end

!------------------------------------------------------------------------------
! PURPOSE: This driver routine calls subroutines for fdda nudging.
!                            
! HISTORY: 02/04/2004 - Creation of WRF version (taken from MM5)   Al Bourgeois
!          02/28/2006 - Update (rtfdda version RT_3.6)             Al Bourgeois
!------------------------------------------------------------------------------

  IMPLICIT NONE

!=======================================================================
  TYPE(domain) :: grid
!!! wuyh added 20110928
  TYPE(grid_config_rec_type),  INTENT(IN)    :: config_flags
!!! wuyh added 20110928 end

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

!ajbij
! INTEGER, INTENT(IN)  :: num_tiles
! INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
!                         i_start,i_end,j_start,j_end ! tile dims (all)
! INTEGER, intent(in)  :: kts,kte

  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: domdesc       ! external communication descriptor
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: domid(maxdom)           ! Domain IDs
  INTEGER, intent(in)  :: parid(maxdom)           ! Parent domain IDs
  LOGICAL, intent(in)  :: restart
  INTEGER, intent(in)  :: itimestep
  INTEGER, intent(in)  :: nudge_opt
  LOGICAL, intent(in)  :: iprt_errob 
  LOGICAL, intent(in)  :: iprt_nudob
  REAL, intent(in)     :: fdasta
  REAL, intent(in)     :: fdaend
  INTEGER, intent(in)  :: nudge_wind    ! flag for wind nudging: (=0 off)
  INTEGER, intent(in)  :: nudge_temp    ! flag for temperature nudging: (=0 off)
  INTEGER, intent(in)  :: nudge_mois    ! flag for moisture nudging: (=0 off)
  INTEGER, intent(in)  :: nudge_pstr    ! NOT USED
!!! wuyh added 20140626
  INTEGER, intent(in)  :: nudge_vr,nudge_rf    ! flag for radar nudging: (=0 off)
!!! wuyh added 20140626 end
  REAL, intent(in) :: coef_wind         ! nudging coefficient for wind, (s-1)
  REAL, intent(in) :: coef_temp         ! nudging coefficient for temperature (s-1)
  REAL, intent(in) :: coef_mois         ! nudging coefficient for moisture (s-1)
!!! wuyh added 20140707
  REAL, intent(in) :: coef_vr, coef_rf  ! nudging coefficients for radar
!!! wuyh added 20140707
  REAL, intent(in) :: coef_pstr         ! NOT USED
  REAL, intent(inout)  :: rinsig
  INTEGER, intent(in) :: npfi
  INTEGER, intent(in) :: ionf
  INTEGER, intent(in) :: obs_prt_max      ! max number of obs in printout
  INTEGER, intent(in) :: obs_prt_freq     ! frequency (in obs index) printout
  INTEGER, intent(in) :: idynin
  REAL, intent(inout) :: dtramp
  INTEGER, intent(in) :: parent_grid_ratio
  REAL, intent(in)     :: xtime           ! yliu 20090726
  REAL, intent(in)     :: dt
  REAL, intent(in)     :: gmt
  INTEGER, intent(in)  :: julday
  INTEGER, intent(in)  :: max_obs         ! max number of observations
  INTEGER, intent(in)  :: obs_vobs_dim    ! number of vars in vobs, vobs_qc for each ob
  INTEGER, intent(in)  :: obs_errf_dim    ! number of vars in errf each ob
  INTEGER, intent(in)  :: nobs_ndg_vars   ! number of nudge variables
  INTEGER, intent(in)  :: kalmax          ! max number of kalman obs
  INTEGER, intent(in)  :: nobs_scans      ! number of nudging scans
  INTEGER, intent(in)  :: nstat
  REAL, intent(inout)  :: vobs(obs_vobs_dim, max_obs)      ! observational variables, 1-ob, 1-var
  REAL, intent(inout)  :: vobs_qc(obs_vobs_dim, max_obs)   ! obs var quality control, 1-ob, 1-var
  REAL, intent(inout)  :: errf(obs_errf_dim, max_obs)
! FCV additions begin
  LOGICAL, intent (in) :: qc_internal
  INTEGER, intent (in) :: qc_domain            ! domain to get qc values from
  REAL,    intent (in) :: tolerance_p, tolerance_q
  REAL,    intent (in) :: tolerance_t, tolerance_u
  REAL,    intent (in) :: tolerance_p_sound, tolerance_q_sound
  REAL,    intent (in) :: tolerance_t_sound, tolerance_u_sound
! FCV additions end
  REAL, intent(in)     :: dx           ! this-domain grid distance (m)
  INTEGER, INTENT(IN) :: kpbl( ims:ime, jms:jme )  ! vertical layer index for PBL top
  REAL, INTENT(IN) :: ht( ims:ime, jms:jme )       ! terrain height (m)
  REAL, TARGET, INTENT(IN) :: mut( ims:ime , jms:jme )     ! air mass on t-grid 
  REAL, TARGET, INTENT(IN) :: muu( ims:ime , jms:jme )     ! air mass on u-grid 
  REAL, TARGET, INTENT(IN) :: muv( ims:ime , jms:jme )     ! air mass on v-grid
  REAL, TARGET, INTENT(IN) :: msftx( ims:ime , jms:jme )   ! map scale on t-grid
  REAL, TARGET, INTENT(IN) :: msfty( ims:ime , jms:jme )   ! map scale on t-grid
  REAL, TARGET, INTENT(IN) :: msfux( ims:ime , jms:jme )   ! map scale on u-grid
  REAL, TARGET, INTENT(IN) :: msfuy( ims:ime , jms:jme )   ! map scale on u-grid
  REAL, TARGET, INTENT(IN) :: msfvx( ims:ime , jms:jme )   ! map scale on v-grid
  REAL, TARGET, INTENT(IN) :: msfvy( ims:ime , jms:jme )   ! map scale on v-grid

  REAL, INTENT(IN) :: p_phy( ims:ime, kms:kme, jms:jme )  ! pressure (Pa)
  REAL, TARGET, INTENT(INOUT) :: t_tendf( ims:ime, kms:kme, jms:jme )  ! temperature tendency
  REAL, INTENT(INOUT) :: savwt( ims:ime, kms:kme, jms:jme,            &
                                          nobs_scans, 4)
  REAL, INTENT(IN) :: z( ims:ime, kms:kme, jms:jme )

#if ( EM_CORE == 1 )
  TYPE(fdob_type), intent(inout)  :: fdob
#endif

!ajbij  INTEGER :: its,ite, jts,jte   ! local tile dims.
  INTEGER :: ij                 ! loop counter over tiles

!!! wuyh added 20140626
  REAL,   INTENT(IN) :: ww( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: qr( ims:ime, kms:kme, jms:jme )
!!! wuyh added 20140626 end
  REAL,   INTENT(IN) :: ub( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: vb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: qvb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pbase( ims:ime, kms:kme, jms:jme ) ! Base press. (Pa)
  REAL,   INTENT(IN) :: ptop                             ! Model top pressure (Pa)
  REAL,   INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme )  ! Press. pert. (Pa)
  REAL,   INTENT(IN) :: phb( ims:ime, kms:kme, jms:jme ) ! Base geopotential
  REAL,   INTENT(IN) :: ph( ims:ime, kms:kme, jms:jme )  ! Perturbation geopotential
  REAL,   INTENT(IN) :: th2( ims:ime, jms:jme )     !       "
  REAL,   INTENT(IN) :: uratx( ims:ime, jms:jme )     ! On mass points
  REAL,   INTENT(IN) :: vratx( ims:ime, jms:jme )     !       "
  REAL,   INTENT(IN) :: tratx( ims:ime, jms:jme )     !       "
  REAL,   TARGET, INTENT(INOUT) :: ru_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   TARGET, INTENT(INOUT) :: rv_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   TARGET, INTENT(INOUT) :: moist_tend( ims:ime, kms:kme, jms:jme )
! wuyh added 20110630
  LOGICAL,INTENT(IN):: flagsnd(max_obs), flagbog(max_obs)
  INTEGER,INTENT(IN):: desobs(40,max_obs),wusource(40,max_obs)
  INTEGER,INTENT(IN) :: ivar(100)     ! nudge variable to pass to sub nudob 
  INTEGER,INTENT(IN) :: icvar(100)    ! correlation variable to pass to sub nudob 
  INTEGER,INTENT(IN) :: num_ndg_terms       ! number of nudging term pairs
  REAL,   INTENT(IN):: oblat(max_obs), oblon(max_obs)
  INTEGER,INTENT(IN):: stnid(40,max_obs)
  INTEGER,INTENT(IN):: wudate(14,max_obs)
  REAL,   INTENT(IN):: qcu(max_obs)
! wuyh added 20110630 end

! Local variables
  logical            :: nudge_flag   ! Flag for doing nudging 
  integer            :: KTAU         ! Forecast timestep
! real               :: xtime        ! yliu 20090726, passed in from grid%xtime
  real               :: dtmin        ! dt in minutes
  integer            :: i, j, k      ! Loop counters.
  integer            :: idom         ! Loop counter.
  integer            :: nsta         ! Number of observation stations
  integer            :: infr         ! Frequency for obs input and error calc 
  integer            :: idarst       ! Flag for calling sub errob on restart
  real               :: dtr          ! Abs value of dtramp (for dynamic init)
  real               :: tconst       ! Reciprocal of dtr
  integer :: KPBLJ(its:ite)          ! 1D temp array.
!ajbij  integer :: KPBLJ(i_start(1):i_end(num_tiles))   ! 1D temp array.
  integer :: ISCAN                   ! Loop counter
  integer :: nn                      ! Loop counter for ivar 
!ajb ... This (global 2D array) violates WRF DM standard
  real               :: htij(ids:ide,jds:jde)  ! terrain ht on global grid

! Wuyh 20100802 added
  Real,save::Vscal(4)                ! amplitude scale of variable
! Wuyh 20100802 added end

! For tendencies, air mass, map scale
  real, pointer :: tendf(:,:)
  real, pointer :: mu(:)
  real, pointer :: msf_x(:)
  real, pointer :: msf_y(:)

!! wuyh added 20150322
  Real,Dimension(its:ite,kts:kte,jts:jte,4):: wt,wt2err
!ajbij Real,Dimension(i_start(1):i_end(num_tiles),kts:kte,j_start(1):j_end(num_tiles),4):: wt,wt2err
  Real::W2EOWT
  Integer::icut,i_s,i_e,ii
!!! wuyh added 20150322 end

INTEGER :: ndum

!do nn = 1,num_ndg_terms
!  write(6,'(3(a,i3))') 'ENTER fddaobs driver: nn = ',nn,   &
!                       ' ivar(nn) = ', ivar(nn),           &
!                       ' icvar(nn) = ',icvar(nn)
!enddo

#if ( EM_CORE == 1 )
  nudge_flag = (nudge_opt  .eq. 1)

  if (.not. nudge_flag) return

!----------------------------------------------------------------------
! ***************       BEGIN FDDA SETUP SECTION        ***************

! Calculate forecast time.
  dtmin = dt/60.     
! xtime = dtmin*(itimestep-1)  ! yliu 20090727

! Set the time-step counter ... ktau corresponds to xtime
  ktau  = itimestep - 1

! DEFINE NSTA WHEN NOT NUDGING TO IND. OBS.
! print *,'in fddaobs_driver, xtime=',xtime
  IF(ktau.EQ.fdob%ktaur) THEN
     IF (iprt_nudob) PRINT *,3333,fdob%domain_tot
!    print *,'ktau,ktaur,inest=',ktau,fdob%ktaur,inest
3333 FORMAT(1X,'IN fddaobs_driver: I4DITOT = ',I2)
     nsta=0.
  ELSE 
     nsta=fdob%nstat
  ENDIF
  
  infr = ionf*(parent_grid_ratio**fdob%levidn(inest))
  nsta=fdob%nstat
  idarst = 0
  IF(restart .AND. ktau.EQ.fdob%ktaur) idarst=1

! MEIXU: collect terrain array HT into a global array HTIJ
  IF(fdob%terrain .eq. 0) then
    htij = 0.0                   ! Zero out the terrain array.  

    CALL loc2glob(HT, HTIJ, "2D", "REAL", domdesc,                 &
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  ips,ipe, jps,jpe, kps,kpe )

! MEIXU end
!   fdob%terrain = 1      ! Terrain has been collected
  ENDIF

! COMPUTE ERROR BETWEEN OBSERVATIONS and MODEL

!! wuyh 20120905
  IF( fdob%nstaw .GT.0 ) THEN
!!  IF( nsta.GT.0 ) THEN
!! wuyh 20120905

!   IF( MOD(ktau,infr).EQ.0 .OR. idarst.EQ.1) THEN
!   NOTE: fdob%calc_step is set in subroutine wrf_fddaobs_in.
    IF( fdob%calc_step.EQ.1 .OR. idarst.EQ.1) THEN       !ajb 20091109 for adaptive step

        CALL errob(grid, inest, ub, vb, tb, t0, qvb, pbase, pp,      &
                   z,                                                &
                   ww,qr,                                            &
                   th2,                                              &
                   kalmax, fdob%ob_is_kalman,                        &
                   uratx, vratx, tratx, htij, obs_vobs_dim,          &
                   nobs_ndg_vars, obs_errf_dim, max_obs, maxdom,     &
                   fdob%levidn, parid, fdob%nstat, fdob%nstaw,       &
                   nudge_wind, nudge_temp, nudge_mois,               &
                   nudge_vr,nudge_rf,                                &
                   fdob%timeob, fdob%rio, fdob%rjo, fdob%rko,        &
                   fdob%elevob,                                      &
                   fdob%platform_ob, fdob%nlevs_ob,                  &
                   qc_internal, qc_domain,                           &
                   tolerance_p, tolerance_q,                         &
                   tolerance_t, tolerance_u,                         &
                   tolerance_p_sound, tolerance_q_sound,             &
                   tolerance_t_sound, tolerance_u_sound,             &
                   fdob%ob_var_type, fdob%ob_ndg_type,               &
                   fdob%NDG_TERM_INDXS,                              &
                   vobs, vobs_qc, errf, ktau,                        &
                   fdob%ktaur, xtime, dtmin,                         &
                   parent_grid_ratio, npfi,                          &
                   obs_prt_max, obs_prt_freq, iprt_errob,            &
                   fdob%obsprt, fdob%stnidprt,                       &
                   fdob%latprt, fdob%lonprt,                         &
                   fdob%mlatprt, fdob%mlonprt,                       &
                   fdob%qcdomprt,                                    &
                   ionf, flagsnd, flagbog, desobs,                   &
                   oblat, oblon, stnid,                              &
                   qcu,wudate,wusource,                              &
                   config_flags%map_proj,                            &
                   config_flags%truelat1,config_flags%truelat2,      &
                   config_flags%cen_lat, config_flags%cen_lon,       &
                   fdob%Last_Obs_Time,                               &
                   ids,ide, jds,jde, kds,kde,                        &
                   ims,ime, jms,jme, kms,kme,                        &
                   its,ite, jts,jte, kts,kte)
!ajbij             i_start,i_end,j_start,j_end,kts,kte,num_tiles)

    ENDIF
  ENDIF

  fdob%tfaci=1.0 
  IF(idynin.EQ.1.AND.nudge_opt.EQ.1) THEN
    dtr=ABS(dtramp)
    tconst=1./dtr
! FDAEND(IN) IS THE TIME IN MINUTES TO END THE DYNAMIC INITIALIZATION CY
    IF(xtime.LT.fdaend-dtr)THEN
      fdob%tfaci=1.
    ELSEIF(xtime.GE.fdaend-dtr.AND.xtime.LE.fdaend) THEN 
      fdob%tfaci=(fdaend-xtime)*tconst
    ELSE
      fdob%tfaci=0.0
    ENDIF
    IF(ktau.EQ.fdob%ktaur.OR.MOD(ktau,10).EQ.0) THEN
      IF (iprt_nudob)                                                  &
         PRINT*,' DYNINOBS: IN,KTAU,XTIME,FDAEND,DTRAMP,DTR,TCONST',   &
         ',TFACI: ',INEST,KTAU,XTIME,FDAEND,DTRAMP,DTR,TCONST,         &
         fdob%TFACI
    ENDIF
  ENDIF

! ***************        END FDDA SETUP SECTION         ***************
!----------------------------------------------------------------------

!ajbij
! Set local tile dims
!   its = i_start(1)
!   ite = i_end(num_tiles)
!   jts = j_start(1)
!   jte = j_end(num_tiles)

!----------------------------------------------------------------------
! ***************         BEGIN NUDGING SECTION         ***************

  do iscan = 1, nobs_scans      !begin scan loop

    DO J = jts, jte
!
! IF NUDGING SURFACE WINDS IN THE BOUNDARY LAYER, IF IWINDS(INEST+2)=1
! USE A SIMILARITY CORRECTION BASED ON ROUGHNESS TO APPLY 10M
! WIND TO THE SURFACE LAYER (K=KL) AT 40M.  TO DO THIS WE MUST
! STORE ROUGHNESS AND REGIME FOR EACH J SLICE AFTER THE CALL TO
! HIRPBL FOR LATER USE IN BLNUDGD.
!
      DO I=its,ite
        KPBLJ(I)=KPBL(I,J)
      ENDDO

!!! wuyh added 20150322
      WT=float(0)
      WT2ERR=float(0)
!!! wuyh added 20150322

!
!--- OBS NUDGING FOR TEMP AND MOISTURE
!
! Set pointers to appropriate targets for nudobs


      do nn = 1,num_ndg_terms
!if(j.eq.10) then
!  write(6,*)
!  write(6,*) "Call to nudob: nn,ivar,icvar=",nn,ivar(nn),icvar(nn)
!  write(286,*) "fddaobs driver: nn,ivar,icvar=",nn,ivar(nn),icvar(nn)
!endif
        if(ivar(nn).gt.0) then

          if(ivar(nn).eq.U) then
            tendf => ru_tendf(:,:,j)
            mu    => muu(:,j)
            msf_x => msfux(:,j)
            msf_y => msfuy(:,j)
          elseif(ivar(nn).eq.V) then
            tendf => rv_tendf(:,:,j)
            mu    => muv(:,j)
            msf_x => msfvx(:,j)
            msf_y => msfvy(:,j)
          elseif(ivar(nn).eq.T) then
            tendf => t_tendf(:,:,j)
            mu    => mut(:,j)
            msf_x => msftx(:,j)
            msf_y => msfty(:,j)
          elseif(ivar(nn).eq.Q) then
            tendf => moist_tend(:,:,j)
            mu    => mut(:,j)
            msf_x => msftx(:,j)
            msf_y => msfty(:,j)
          else
            tendf => t_tendf(:,:,j)   ! default for now
            mu    => mut(:,j)
            msf_x => msftx(:,j)
            msf_y => msfty(:,j)
          endif

          NSTA=NSTAT

!         if(j.eq.3) then
!           write(6,'(3(a,i3))') 'OBS DRIVER: term ',nn,' ivar = ',ivar(nn),'   icvar = ',icvar(nn)
!           call flush(6)
!         endif

          IF(J .GT. 2 .and. J .LT. jde-1) THEN

            CALL nudob(J, nn, ivar(nn), icvar(nn), tendf,              &
                    inest, restart, ktau, fdob%ktaur, xtime,           &
                    mu, msf_x,msf_y, obs_vobs_dim,                     &
                    nobs_ndg_vars, obs_errf_dim, nobs_scans,           &
                    max_obs, maxdom,                                   &
                    npfi, ionf, fdob%twindo_plt,                       &
                    fdob%levidn,                                       &
                    parid, fdob%nstaw,                                 &
                    fdob, fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,    &
                    parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,  &
                    fdob%rko, fdob%rkmob, fdob%timeob,                 &
                    vobs, vobs_qc, errf,                               &
                    pbase(ims,kms,j), ptop, pp(ims,kms,j),             &
                    nudge_wind, nudge_temp, nudge_mois,                &
                    nudge_vr, nudge_rf,                                &
                    coef_wind, coef_temp, coef_mois,                   &
                    coef_vr,coef_rf,                                   &
!!! wuyh added 20150322
                    wt(its,kts,J,ivar(nn)),                            &
                    wt2err(its,kts,J,ivar(nn)),                        &
!!! wuyh added 20150322 end
!!! wuyh modified 20150505
!!                  savwt(ims,kms,j,1,nn), kpblj, iscan,               &
                    savwt(ims,kms,j,1,ivar(nn)), kpblj, iscan,         &
!!! wuyh modified 20150505 end
                    htij, iprt_nudob,                                  &
                    ids,ide, jds,jde, kds,kde,                         & ! domain dims
                    ims,ime, jms,jme, kms,kme,                         & ! memory dims
                    its,ite, jts,jte, kts,kte,                         &
                    fdob%ndir,fdob%npow,Kalmax,fdob%ndirpow,           &
                    fdob%obs2kalman,fdob%gain_at_ob,qc_internal)

                    if(icvar(nn).eq.Vr .AND. abs(wt2err(its,kts,J,ivar(nn))) .gt. 1.E-5) then
                      write(287,1287)ivar(nn),wt2err(its,kts,J,ivar(nn)),wt(its,kts,J,ivar(nn))
                    endif
1287                Format(I2,2(1x,F12.6))

          ENDIF

        endif      !end if(ivar(nn).gt.0)

      enddo      !end num_ndg_terms loops

! Update tendiencies
!!! wuyh added 20150322
      ICUT=0
      IF(INEST.GT.1)ICUT=1
      i_s = max0(2+icut,its)
      i_e = min0(ide-1-icut,ite)

      Do K=kts,kte
      Do I=i_s,i_e
        do ii=1,4  !!u,v,t,q 

         If(fdob%CALC_STEP.EQ.1.OR.(Restart.AND.KTAU.EQ.fdob%KTAUR))THEN  !ajb 20091109 for adaptivee step
           if(abs(WT(I,K,J,ii)) .gt. 1.E-6 .AND. &
              abs(WT(I,K,J,ii)) .le. 1.E6)Then
              W2EOWT=WT2ERR(I,K,J,ii)/abs(WT(I,K,J,ii))
           else
              W2EOWT=float(0)
           endif
           SAVWT(I,K,j,iscan,ii)=W2EOWT
         Endif
!!         write(199,1199)ii,K,I,J,WT2ERR(I,K,J,ii),WT(I,K,J,ii)
         if(ii .EQ. U)then
          ru_tendf(i,k,j)=ru_tendf(i,k,j)+SAVWT(i,k,j,iscan,ii)
         elseif(ii .EQ. V)then
          rv_tendf(i,k,j)=rv_tendf(i,k,j)+SAVWT(i,k,j,iscan,ii)
         elseif(ii .EQ. T)then
          t_tendf(i,k,j)= t_tendf(i,k,j)+SAVWT(i,k,j,iscan,ii)
         else
          moist_tend(i,k,j)=moist_tend(i,k,j)+SAVWT(i,k,j,iscan,ii)
         endif
        enddo
      Enddo  !loop over I
      Enddo  !loop over K
!! 1199 Format(I3,1x,I3,2(1x,I4),2(1x,E15.8))
!!      call flush(199)
!!! wuyh added 20150322

    ENDDO      !end J loop

  enddo      !end scan loop

! Reset calc_step flag
   fdob%calc_step = 0
!
! --- END OF 4DDA
!
  RETURN
#endif
  END SUBROUTINE fddaobs_driver

END MODULE module_fddaobs_driver
