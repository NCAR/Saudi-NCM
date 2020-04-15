!wrf:model_layer:physics
!
MODULE module_fddaobs_rtfdda

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
! Modification History:
!   03212007 Modified fddaobs_init to compute Lambert cone factor. -Al Bourgeois

CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE fddaobs_init(obs_nudge_opt, maxdom, inest, parid,         &
                          restart, niobf, kalmax, read_kalman,         &
                          nudge_terms,                                 &    !ajb 12092013
                          qc_domain, adjust_ht, nobs_scans,            &
                          obs_twindo_cg, obs_twindo, itimestep, dx,    &
                          rinxy, rinxy_sfc,                            &
                          rinxy_meta, rinxy_spec, rinxy_ship,          &
                          rinxy_syno, rinxy_temp, rinxy_pilo,          &
                          rinxy_satw, rinxy_sams, rinxy_prof,          &
                          rinxy_sl10, rinxy_sl11, rinxy_sl12,          &
                          rinxy_sl13, rinxy_sl14, rinxy_sl15,          &
                          twindo, twindo_sfc,                          &
                          twindo_meta, twindo_spec, twindo_ship,       &
                          twindo_syno, twindo_temp, twindo_pilo,       &
                          twindo_satw, twindo_sams, twindo_prof,       &
                          twindo_sl10, twindo_sl11, twindo_sl12,       &
                          twindo_sl13, twindo_sl14, twindo_sl15,       &
                          rinfmx,                                      &
                          xlat, xlong,                                 &
                          start_year, start_month, start_day,          &
                          start_hour, start_minute, start_second,      &
#if ( EM_CORE == 1 )
                          fdob,                                        &
#endif
                          ndirpow, ndir, npow,                         &
                          iprt,                                        &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          its,ite, jts,jte, kts,kte)     
!-----------------------------------------------------------------------
!  This routine does initialization for real time fdda obs-nudging.
!
!-----------------------------------------------------------------------
  USE module_domain
  USE module_dm                 ! for wrf_dm_min_real
  USE module_fddaobs_utilities  ! for set_indxs, dummy_str
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

!=======================================================================
! Definitions
!-----------
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: obs_nudge_opt(maxdom)
  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: parid(maxdom)
  LOGICAL, intent(in)  :: restart
  INTEGER, INTENT(IN)  :: niobf          ! maximum number of observations
  INTEGER, INTENT(IN)  :: kalmax         ! max number of kalman type obs
  INTEGER, INTENT(IN)  :: read_kalman(maxdom)  ! flag for reading kalman gain data
  CHARACTER(LEN=*), INTENT(IN) :: nudge_terms  ! var/covar terms (ex: UU UV TVr) for nudging
  INTEGER, INTENT(IN)  :: qc_domain(maxdom)  ! domain used to set qc for this nest
  LOGICAL, INTENT(IN)  :: adjust_ht      ! near-surface height-adjustment switch
  INTEGER, intent(in)  :: nobs_scans     ! number of nudging scans
  REAL,    intent(inout)  :: obs_twindo
  REAL,    intent(in)  :: obs_twindo_cg  ! time window on coarse grid
  INTEGER, intent(in)  :: itimestep
  REAL    , INTENT(IN) :: dx                  ! this domain grid size (m)
  REAL    , INTENT(IN) :: rinxy(maxdom)       ! horizontal Radius Of Influence (km)
  REAL    , INTENT(IN) :: rinxy_sfc(maxdom)   ! horizontal Radius Of Influence for surface obs (km)
  REAL    , INTENT(IN) :: rinxy_meta(maxdom)  ! horiz ROI, platfm type metar   (km)
  REAL    , INTENT(IN) :: rinxy_spec(maxdom)  ! horiz ROI, platfm type speci   (km)
  REAL    , INTENT(IN) :: rinxy_ship(maxdom)  ! horiz ROI, platfm type ship    (km)
  REAL    , INTENT(IN) :: rinxy_syno(maxdom)  ! horiz ROI, platfm type synop   (km)
  REAL    , INTENT(IN) :: rinxy_temp(maxdom)  ! horiz ROI, platfm type temp    (km)
  REAL    , INTENT(IN) :: rinxy_pilo(maxdom)  ! horiz ROI, platfm type pilot   (km)
  REAL    , INTENT(IN) :: rinxy_satw(maxdom)  ! horiz ROI, platfm type satwnds (km)
  REAL    , INTENT(IN) :: rinxy_sams(maxdom)  ! horiz ROI, platfm type sams    (km)
  REAL    , INTENT(IN) :: rinxy_prof(maxdom)  ! horiz ROI, platfm type profiler(km)
  REAL    , INTENT(IN) :: rinxy_sl10(maxdom)  ! horiz ROI, platfm type slot10  (km)
  REAL    , INTENT(IN) :: rinxy_sl11(maxdom)  ! horiz ROI, platfm type slot11  (km)
  REAL    , INTENT(IN) :: rinxy_sl12(maxdom)  ! horiz ROI, platfm type slot12  (km)
  REAL    , INTENT(IN) :: rinxy_sl13(maxdom)  ! horiz ROI, platfm type slot13  (km)
  REAL    , INTENT(IN) :: rinxy_sl14(maxdom)  ! horiz ROI, platfm type slot14  (km)
  REAL    , INTENT(IN) :: rinxy_sl15(maxdom)  ! horiz ROI, platfm type slot15  (km)
  REAL    , INTENT(IN) :: twindo(maxdom)      ! half-period time window       (hrs)
  REAL    , INTENT(IN) :: twindo_sfc(maxdom)  ! half-period time window for surface obs (hrs)
  REAL    , INTENT(IN) :: twindo_meta(maxdom) ! h-p twindo for platfm metar   (hrs)
  REAL    , INTENT(IN) :: twindo_spec(maxdom) ! h-p twindo for platfm speci   (hrs)
  REAL    , INTENT(IN) :: twindo_ship(maxdom) ! h-p twindo for platfm ship    (hrs)
  REAL    , INTENT(IN) :: twindo_syno(maxdom) ! h-p twindo for platfm synop   (hrs)
  REAL    , INTENT(IN) :: twindo_temp(maxdom) ! h-p twindo for platfm temp    (hrs)
  REAL    , INTENT(IN) :: twindo_pilo(maxdom) ! h-p twindo for platfm pilot   (hrs)
  REAL    , INTENT(IN) :: twindo_satw(maxdom) ! h-p twindo for platfm satwnds (hrs)
  REAL    , INTENT(IN) :: twindo_sams(maxdom) ! h-p twindo for platfm sams    (hrs)
  REAL    , INTENT(IN) :: twindo_prof(maxdom) ! h-p twindo for platfm profiler(hrs)
  REAL    , INTENT(IN) :: twindo_sl10(maxdom) ! h-p twindo for platfm slot10  (hrs)
  REAL    , INTENT(IN) :: twindo_sl11(maxdom) ! h-p twindo for platfm slot11  (hrs)
  REAL    , INTENT(IN) :: twindo_sl12(maxdom) ! h-p twindo for platfm slot12  (hrs)
  REAL    , INTENT(IN) :: twindo_sl13(maxdom) ! h-p twindo for platfm slot13  (hrs)
  REAL    , INTENT(IN) :: twindo_sl14(maxdom) ! h-p twindo for platfm slot14  (hrs)
  REAL    , INTENT(IN) :: twindo_sl15(maxdom) ! h-p twindo for platfm slot15  (hrs)
  REAL, intent(in)     :: rinfmx(maxdom) ! scale for max horiz radius of influence
  REAL, DIMENSION( ims:ime, jms:jme ),                            &
           INTENT(IN ) :: xlat, xlong  ! lat/long locations on mass point grid
  INTEGER, intent(in)  :: start_year   ! Model start year
  INTEGER, intent(in)  :: start_month  ! Model start month
  INTEGER, intent(in)  :: start_day    ! Model start day
  INTEGER, intent(in)  :: start_hour   ! Model start hour
  INTEGER, intent(in)  :: start_minute ! Model start minute
  INTEGER, intent(in)  :: start_second ! Model start second
  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

#if ( EM_CORE == 1 ) 
  TYPE(fdob_type), intent(inout)  :: fdob
#endif
  INTEGER , INTENT(IN) :: ndirpow      ! dimension for raw Kalman gain data
  INTEGER , INTENT(IN) :: ndir         ! dim for reduced gain data: half-length
  INTEGER , INTENT(IN) :: npow         ! dim for reduced gain data: jump
  LOGICAL,  INTENT(IN) :: iprt         ! flag enabling printing warning messages

! Local variables
  logical             :: nudge_flag      ! nudging flag for this nest 
  integer             :: ktau            ! current timestep
  integer             :: nest            ! loop counter
  integer             :: n               ! loop counter
  integer             :: idom            ! domain id
  integer             :: parent          ! parent domain
  integer             :: iplt            ! loop counter over platform types
  integer             :: ip              ! loop counter over nudging terms
  real                :: known_lat       ! latitude of domain point (i,j)=(1,1)
  real                :: known_lon       ! longitude of domain point (i,j)=(1,1)
  character (len=256) :: message
  character (len=5)   :: pair_list(MNP)  ! list of nudging terms (e.g., UU, -UVr, etc)
  integer             :: num_chars(MNP)  ! number of characters for each pair_list string
  character (LEN=MCH) :: varlist         ! temp for holding variable list
  integer lenstr                         ! number of characters in fdob%varlist string

#if ( EM_CORE == 1 ) 
! Check to see if the nudging flag has been set. If not,
! simply RETURN.
  nudge_flag = (obs_nudge_opt(inest) .eq. 1)
  if (nudge_flag) then
    WRITE(message,'(a,i2)') 'OBS NUDGING IS ACTIVATED FOR DOMAIN ',inest
    CALL wrf_message( TRIM(message) )
    if (adjust_ht) then
    CALL wrf_message("*** NEAR-SURFACE HEIGHT ADJUSTMENT SCHEME IS TURNED ON")
    else
    CALL wrf_message("*** NEAR-SURFACE HEIGHT ADJUSTMENT SCHEME IS TURNED OFF")
    endif
  else
    return
  endif

  ktau  = itimestep
  fdob%calc_step = 0                 !ajb 20091109 for adaptive step
  fdob%ktau_prev = ktau              !ajb 20091109 for adaptive step
  if(restart) then
    fdob%ktaur = ktau
  else
    fdob%ktaur = 0 
  endif

! Create character string containing model starting-date
  CALL date_string(start_year, start_month, start_day, start_hour,        &
                   start_minute, start_second, fdob%sdate)

! Check obs-scan setup
  if(nobs_scans.eq.1) then
    WRITE(message,'(a,i2,a)') 'Obs-nudging for domain ',inest,            &
                              ' is configured for Single-Scan Mode'
    CALL wrf_message( TRIM(message) )
  elseif (nobs_scans.eq.2) then
    WRITE(message,'(a,i2,a)') 'Obs-nudging for domain ',inest,            &
                              ' is configured for Single-Scan Mode'
    CALL wrf_message( TRIM(message) )
  else
    WRITE( wrf_err_message , * ) 'Obs-nudging selection nobs_scans = ',  &
                                 nobs_scans,' must equal 1 or 2.'
    CALL wrf_error_fatal ( wrf_err_message )
  endif

! Set flag for nudging on pressure (not sigma) surfaces
  fdob%iwtsig = 0

! Set ending nudging date (used with dynamic ramp-down) to zero.
  fdob%datend = 0.

! Set flag to show that terrain has not been collected yet.
  fdob%terrain = 0.

! Do check on twindo to make sure that the user set it.
  if(inest.eq.1) then
    if(obs_twindo .eq. 0.) then
      if(iprt) then
        write(6,'(/a)') '*** FATAL: TWINDO=0 on the coarse domain.'
        write(6,'(a/)') '*** Did you forget to set twindo in the fdda namelist?'
      endif
      CALL wrf_error_fatal ( 'obs_twindo is 0. Please check namelist.input file')
    endif
  else        ! nest
    if(obs_twindo .eq. 0.) then
      fdob%window = obs_twindo_cg
      if(iprt) then
        write(6,'(/a,i2)') 'WARNING: TWINDO=0. for nest ',inest
        write(6,'(a,f12.5,a/)') 'Default to coarse-grid value of ', obs_twindo_cg,' hrs'
      endif
    endif
  endif

! Initialize flags.

  fdob%domain_tot=0
  do nest=1,maxdom
    fdob%domain_tot = fdob%domain_tot + obs_nudge_opt(nest)
  end do

! Set parameters.

  fdob%pfree = 50.0
  fdob%rinfmn = 1.0
  fdob%rinfmx = rinfmx(inest)
  fdob%dpsmx = 7.5
  fdob%dcon = 1.0/fdob%dpsmx

!! wuyh added 20130418
  fdob%Last_Obs_Time = -1.
!! wuyh added 20130418 end

  if(iprt) write(6,'(2(a,f8.3))') '    RINFMX = ',fdob%rinfmx,'  DCON = ',fdob%dcon

! Get known lat and lon and store these on all processors in fdob structure, for
! later use in projection x-formation to map (lat,lon) to (x,y) for each obs.
  IF (its .eq. 1 .AND. jts .eq. 1) then
     known_lat = xlat(1,1)
     known_lon = xlong(1,1)
  ELSE
     known_lat = 9999.
     known_lon = 9999.
  END IF
  fdob%known_lat = wrf_dm_min_real(known_lat)
  fdob%known_lon = wrf_dm_min_real(known_lon)

! Set time window for the various platform types. 15 types are allowed,
! including the extra slots sl10, sl11, etc. For clarity, when a type is 
! added, change the 4-char slot name slxx to the first 4 chars of the
! platform name (ex meta, syno). This should be done in the namelist, in
! this argument list, and in the parameter list in sub set_platform_val.
  CALL set_platform_val( inest, maxdom, fdob%twindo_plt, twindo,   &
                         twindo_sfc,                               &
                         twindo_meta, twindo_spec, twindo_ship,    &
                         twindo_syno, twindo_temp, twindo_pilo,    &
                         twindo_satw, twindo_sams, twindo_prof,    &
                         twindo_sl10, twindo_sl11, twindo_sl12,    &
                         twindo_sl13, twindo_sl14, twindo_sl15)

!write(6,'/a,i2,15f6.0/') 'set_platform_val: ',inest, fdob%twindo_plt

! Set horizontal radius of influence for the various platform types.
! See comment above for adding a platform type.
  CALL set_platform_val( inest, maxdom, fdob%rinxy_plt, rinxy,     &
                         rinxy_sfc,                                &
                         rinxy_meta, rinxy_spec, rinxy_ship,       &
                         rinxy_syno, rinxy_temp, rinxy_pilo,       &
                         rinxy_satw, rinxy_sams, rinxy_prof,       &
                         rinxy_sl10, rinxy_sl11, rinxy_sl12,       &
                         rinxy_sl13, rinxy_sl14, rinxy_sl15)

  CALL write_rinxy_info( inest, dx, fdob%rinxy_plt )

! Set the time window for reading in obs. This needs to be the maximum
! over all the twindos for the individual platform types.
  fdob%window = fdob%twindo_plt(1)
  do iplt = 2, 15 
    if(fdob%window.lt.fdob%twindo_plt(iplt)) fdob%window=fdob%twindo_plt(iplt)
  enddo

! Do initialization for Kalman weighting.
  fdob%ndirpow = ndirpow
  fdob%ndir    = ndir
  fdob%npow    = npow
  fdob%ndum    = 0

! wuyh removed 20120823
!  do n = 1, kalmax
!     fdob%local_kalman2obs(n) = 0
!  enddo
! wuyh removed 20120823 end

  do n = 1, niobf
     fdob%obs2kalman(n)   = 0
     fdob%ob_is_kalman(n) = 0 
     fdob%gainname(n) = -1000010
  enddo

! Check for Kalman-obs processing
  if(read_kalman(inest).eq.0) then
    if(iprt) then
      write(message,'(a,i2)') '*** KALMAN OBS PROCESSING IS TURNED OFF FOR DOMAIN ',inest
      CALL wrf_message( TRIM(message) )
      if(TRIM(string_comp(nudge_terms)).ne."UU VV TT QQ") then
        write(message,'(3a)')                           &
          'YOUR NAMELIST VARIABLE OBS_NUDGE_TERMS FOR THIS DOMAIN IS SET TO "', &
          TRIM(string_comp(nudge_terms)),'"'
        CALL wrf_message( TRIM(message) )
        CALL wrf_error_fatal ('DEFAULT VALUE "UU VV TT QQ" MUST BE USED FOR CRESSMAN PROCESSING')
      endif
    endif
  else
    if(iprt) then
      write(message,'(a,i2)') '*** KALMAN OBS PROCESSING IS TURNED ON FOR DOMAIN ',inest
      CALL wrf_message( TRIM(message) )
    endif
  endif

! Form pair_list array and store number of nudging terms into fdob%num_ndg_terms.
! (ex: nudge_terms = "UU   VV -TT" ==> pair_list(1)="UU", pair_list(2)="VV", pair_list(3)="-TT")

  CALL make_pair_list(nudge_terms, pair_list, fdob%num_ndg_terms, num_chars)

! Convert nudge_terms string to index form ndg_term_indxs (ex: UU=1,1 VT=2,3 etc).
  CALL set_indxs(pair_list, fdob%num_ndg_terms, num_chars, fdob%ndg_term_indxs)

! Construct a single-string variable list from the pair list, with only a single space
! between pairs and the entire string enclosed in double quotes. This string will be
! used to match the data string at the beginning of each Kalman-gain file.
 
  CALL get_varlist(fdob%num_ndg_terms, pair_list, num_chars, varlist, lenstr)
  fdob%varlist(1:lenstr) = varlist(1:lenstr)
  fdob%len_varlist = lenstr

! varlist string for later use in confirming Kalman-gain data files
  if(iprt) then
    CALL wrf_message("")
    write(message,'(a,i2,a)') '*** NUDGING TERMS REQUESTED FOR DOMAIN ',inest,':'
    CALL wrf_message(message)
    do ip = 1, fdob%num_ndg_terms
      if(pair_list(ip)(1:1).eq."-") then
        write(message,'(1x,a,2x,a)') pair_list(ip)(1:num_chars(ip)), dummy_str
      else
        write(message,'(2x,a)') pair_list(ip)(1:num_chars(ip))
      endif
   
      CALL wrf_message( TRIM(message) )
    enddo
  endif

! Initialize qcdomprt for diagnostic report. It will be reset later in
! subroutine get_parent_QC if qc_internal is true
  fdob%qcdomprt = inest
 
! Print information about obs qc settings for each nest
  if(iprt) then
    CALL wrf_message("")
    write(message,'(a,i2,a)') '*** OBS_QC_DOMAIN SETTING FOR NEST ',inest,':'
    CALL wrf_message( TRIM(message) )
  endif
  if(qc_domain(inest).gt.inest) then
    if(iprt) then
      write(message,'(a,i2)') 'FATAL: Can not request QC from child domain ',qc_domain(inest)
      CALL wrf_message( TRIM(message) )
    endif
    CALL wrf_error_fatal ( 'QC is set to child domain! Please check namelist obs_qc_domain.')
  elseif(qc_domain(inest).eq.inest) then
    if(iprt) then
      CALL wrf_message('QC VALUES FOR THIS DOMAIN WILL BE CALCULATED')
    endif
  else
    if(iprt) then
write(message,'(a,i2,1x,i2)') 'QC FOR THIS DOMAIN WILL BE SET FROM PARENT DOMAIN ',qc_domain(inest)
      CALL wrf_message( TRIM(message) )
    endif
  endif
  CALL wrf_message("")

! Calculate the nest levels, levidn. Note that each nest
! must know the nest levels levidn(maxdom) of each domain.
  do nest=1,maxdom

! Initialize nest level for each domain.
    if (nest .eq. 1) then
      fdob%levidn(nest) = 0  ! Mother domain has nest level 0
    else
      fdob%levidn(nest) = 1  ! All other domains start with 1
    endif
    idom = nest
100 parent = parid(idom)      ! Go up the parent tree

      if (parent .gt. 1) then   ! If not up to mother domain
        fdob%levidn(nest) = fdob%levidn(nest) + 1
        !idom = parid(parent)
        idom = parent    ! 20070220 YLIU -- Al. B fix for d4?
        goto 100
      endif
  enddo
  RETURN
#endif
  END SUBROUTINE fddaobs_init

#if ( EM_CORE == 1 )
!-----------------------------------------------------------------------
SUBROUTINE errob(grid,                                          &
                 inest, ub, vb, tb, t0, qvb, pbase, pp,         &
                 z,                                             &
                 wb,qr,                                         &
                 th2,                                           &
                 kalmax, ob_is_kalman,                          &
                 uratx, vratx, tratx, htij, vdim,               &
                 nndgv, nerrf, niobf, maxdom,                   &
                 levidn, parid, nstat, nstaw,                   &
                 iswind, istemp, ismois,                        &
                 isvr,isrf,                                     &
                 timeob, rio, rjo, rko, elevob,                 &
                 platform_ob, nlevs_ob,                         &
                 qc_internal, qc_domain,                        &
                 tolerance_p, tolerance_q,                      &
                 tolerance_t, tolerance_u,                      &
                 tolerance_p_sound, tolerance_q_sound,          &
                 tolerance_t_sound, tolerance_u_sound,          &
                 ob_var_type, ob_ndg_type, ndg_term_indxs,      &
                 vobs, vobs_qc, errf, ktau,                     &
                 ktaur, xtime, dtmin,                           &
                 iratio, npfi,                                  &
                 prt_max, prt_freq, iprt,                       &
                 obs_prt, stnid_prt, lat_prt, lon_prt,          &
                 mlat_prt, mlon_prt, qcdom_prt,                 &
                 ionf, flagsnd, flagbog, desobs,                &
                 oblat, oblon, stnid,                           &
                 qcu,                                           &
!!qcv,qct,qcq,
                 wudate,wusource,                               &
                 map_proj,                                      &
                 truelat1,truelat2,                             &
                 cen_lat,cen_lon,                               &
                 Last_Obs_Time,                                 &
                 ids,ide, jds,jde, kds,kde,                     &
                 ims,ime, jms,jme, kms,kme,                     &
                 its,ite, jts,jte, kts,kte  )
!ajbij           i_start,i_end,j_start,j_end,kts,kte,num_tiles )

!-----------------------------------------------------------------------
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  USE module_dm,ONLY:get_full_obs_vector,get_full_obs_qc,get_full_t_qc
#endif
  USE module_model_constants, ONLY : rcp
  USE module_fddaobs_qc, ONLY : fddaobs_qc
  USE module_fddaobs_utilities
  USE module_domain   ! grid type, get_parent_QC

!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
!
! PURPOSE: THIS SUBROUTINE CALCULATES THE DIFFERENCE BETWEEN THE
!     OBSERVED VALUES AND THE FORECAST VALUES AT THE OBSERVATION
!     POINTS.  THE OBSERVED VALUES CLOSEST TO THE CURRENT
!     FORECAST TIME (XTIME) WERE DETERMINED IN SUBROUTINE
!     IN4DOB AND STORED IN ARRAY VOBS.  THE DIFFERENCES
!     CALCULATED BY SUBROUTINE ERROB WILL BE STORED IN ARRAY
!     ERRF FOR THE NSTA OBSERVATION LOCATIONS.  MISSING
!     OBSERVATIONS ARE DENOTED BY THE DUMMY VALUE 99999.
!
!------------------------------------------------------------------------------
!
! THE STORAGE ORDER IN VOBS AND ERRF IS AS FOLLOWS:
!        IVAR                VARIABLE TYPE(TAU-1)
!        ----                --------------------
!         1                    U error
!         2                    V error
!         3                    T error
!         4                    Q error
!         5                    VR error
!         6                    RF error
!         7                    Model surface press at T-points
!         8                    Model surface press at U-points
!         9                    Model surface press at V-points
!        10                    RKO at U-points
!
!-----------------------------------------------------------------------
!
!     Description of input arguments.
!
!-----------------------------------------------------------------------

  TYPE(domain) :: grid
  INTEGER, INTENT(IN) :: inest                  ! Domain index.
  INTEGER, INTENT(IN) :: vdim                   ! Dim for arrays vobs, vobs_qc
  INTEGER, INTENT(IN) :: nndgv                  ! Number of nudge variables
  INTEGER, INTENT(IN) :: nerrf                  ! Number of error fields
  INTEGER, INTENT(IN) :: niobf                  ! Number of observations
  INTEGER, INTENT(IN) :: kalmax                 ! Maximum number of kalman obs
  INTEGER, INTENT(IN) :: maxdom                 ! Maximum number of domains
  INTEGER, INTENT(IN) :: levidn(maxdom)         ! Level of nest
  INTEGER, INTENT(IN) :: parid(maxdom)          ! Id of parent grid
  INTEGER, INTENT(IN) :: ktau                   ! Forecast timestep index
  INTEGER, INTENT(IN) :: ktaur                  ! Restart timestep index
  REAL,    INTENT(IN) :: xtime                  ! Model time (min) at time t-1
  REAL,    INTENT(IN) :: dtmin                  ! Model timestep in minutes
  INTEGER, INTENT(IN) :: iratio                 ! Nest to parent gridsize ratio
  INTEGER, INTENT(IN) :: npfi                   ! Coarse-grid diagnostics freq
  INTEGER, INTENT(IN) :: prt_max                ! Max number of obs to print.
  INTEGER, INTENT(IN) :: prt_freq               ! Frequency of obs to print.
  LOGICAL, INTENT(IN) :: iprt                   ! Print flag
  INTEGER, INTENT(IN) :: obs_prt(prt_max)       ! Print obs indices
  INTEGER, INTENT(IN) :: stnid_prt(40,prt_max)  ! Print obs station ids
  REAL, INTENT(IN)    :: lat_prt(prt_max)       ! Print obs latitude
  REAL, INTENT(IN)    :: lon_prt(prt_max)       ! Print obs longitude
  REAL, INTENT(IN)    :: mlat_prt(prt_max)      ! Print model lat at obs loc
  REAL, INTENT(IN)    :: mlon_prt(prt_max)      ! Print model lon at obs loc
  INTEGER, INTENT(IN) :: qcdom_prt(prt_max)     ! Print domain used for qc values
  Integer,intent(in) :: ionf                    ! Model-step input frequency for obs
  logical,intent(in) :: flagsnd(niobf), flagbog(niobf)
  Integer,intent(in) :: desobs(40,niobf),wusource(40,niobf)
  Real,   intent(in) :: oblat(niobf), oblon(niobf)
  Integer,intent(in) :: stnid(40,niobf)
  Integer,intent(in) :: wudate(14,niobf)
  Real,Dimension(niobf),intent(in):: qcu

  INTEGER, intent(in) :: map_proj        ! map projection index
  REAL, intent(in) :: truelat1          ! truelat1 for Lambert map projection
  REAL, intent(in) :: truelat2          ! truelat2 for Lambert map projection
  REAL, intent(in) :: cen_lat            ! center latitude for map projection
  REAL, intent(in) :: cen_lon            ! center longiture for map projection

  INTEGER, INTENT(IN) :: nstat                  ! # stations held for use
  INTEGER, INTENT(IN) :: nstaw                  ! # stations in current window
  INTEGER, INTENT(IN) :: iswind                 ! Wind nudging: (=0 off)
  INTEGER, INTENT(IN) :: istemp                 ! Temperature nudging: (=0 off)
  INTEGER, INTENT(IN) :: ismois                 ! Moisture nudging: (=0 off)
  INTEGER, INTENT(IN) :: isvr           ! radial wind: (=0 off, =-1 U,V, =+1 Vr )
  INTEGER, INTENT(IN) :: isrf           ! Z: (=0 off)
  INTEGER, INTENT(IN) :: ids,ide, jds,jde, kds,kde   ! domain dims
  INTEGER, INTENT(IN) :: ims,ime, jms,jme, kms,kme   ! memory dims
  INTEGER, INTENT(IN) :: its,ite, jts,jte, kts,kte   ! tile   dims

!ajbij
! INTEGER, INTENT(IN) :: num_tiles
! INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
!                        i_start,i_end,j_start,j_end ! tile dims (all)
! INTEGER, INTENT(IN) :: kts,kte

  REAL,    INTENT(IN) :: ub(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: vb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: tb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: t0                          ! Reference temperature
  REAL,    INTENT(IN) :: qvb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: pbase(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: pp( ims:ime,kms:kme,jms:jme ) ! perturbation pres (Pa)
  REAL,    INTENT(IN) :: z( ims:ime, kms:kme, jms:jme ) ! Ht above sl on half-levels
  REAL,    INTENT(IN) :: uratx(ims:ime,jms:jme) ! U to U10 ratio on mass points
  REAL,    INTENT(IN) :: vratx(ims:ime,jms:jme) ! V to V10 ratio on mass points
  REAL,    INTENT(IN) :: tratx(ims:ime,jms:jme) ! T to TH2 ratio on mass points
!!! wuyh added 20140626 for radial wind nudging
  REAL,    INTENT(IN) :: qr(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: wb(ims:ime,kms:kme,jms:jme)
!!! wuyh added 20140626 end
  REAL,    INTENT(IN) :: th2(ims:ime,jms:jme)   ! 2m Potential temperature
  REAL,    INTENT(IN) :: htij(ids:ide, jds:jde) ! terrain ht on global grid
  INTEGER, INTENT(IN) :: ob_is_kalman(niobf)    ! flags designating kalman obs
  REAL,    INTENT(IN) :: timeob(niobf)          ! Obs time (hrs)
  REAL,    INTENT(IN) :: rio(niobf)             ! Obs west-east coordinate (non-stag grid).
  REAL,    INTENT(IN) :: rjo(niobf)             ! Obs south-north coordinate (non-stag grid).
  REAL,    INTENT(INOUT) :: rko(niobf)          ! Obs bottom-top coordinate
  REAL,    INTENT(IN) :: elevob(niobf)          ! Obs elevation (meters)
  INTEGER, INTENT(IN) :: platform_ob (40,niobf)
  REAL,    INTENT(IN) :: nlevs_ob(niobf)
  LOGICAL, INTENT(IN) :: qc_internal
  INTEGER, INTENT(IN) :: qc_domain              ! domain to get qc values from
  REAL,    INTENT(IN) :: tolerance_p, tolerance_q
  REAL,    INTENT(IN) :: tolerance_t, tolerance_u
  REAL,    INTENT(IN) :: tolerance_p_sound, tolerance_q_sound
  REAL,    INTENT(IN) :: tolerance_t_sound, tolerance_u_sound
  INTEGER, INTENT(IN) :: ob_var_type(niobf)     ! Observation variable type (U,V,T,etc)
  INTEGER, INTENT(IN) :: ob_ndg_type(niobf)     ! Nudge-term type (# term in ndg_term_indxs)
  INTEGER, INTENT(IN) :: ndg_term_indxs(MNP,2)  ! indices for nudging terms
  REAL,    INTENT(INOUT) :: vobs(vdim, niobf)          ! observational variables, 1-ob, 1-var
  REAL,    INTENT(INOUT) :: vobs_qc(vdim, niobf)       ! obs var quality control, 1-ob, 1-var
  REAL,    INTENT(INOUT) :: errf(nerrf, niobf)         ! Model innovation

! Local variables
!ajbij  INTEGER :: its,ite, jts,jte  ! local tile dims
!ajbij  INTEGER :: ij                ! loop counter over tiles
  INTEGER :: iobmg(niobf)   ! Obs i-coord on mass grid
  INTEGER :: jobmg(niobf)   ! Obs j-coord on mass grid
  REAL :: dxobmg(niobf)     ! Interp. fraction (x dir) referenced to mass-grid
  REAL :: dyobmg(niobf)     ! Interp. fraction (y dir) referenced to mass-grid
  REAL :: ra(niobf)
  REAL :: rb(niobf)
  REAL :: rc(niobf)
  INTEGER :: ia(niobf)
  INTEGER :: ib(niobf)
  INTEGER :: ic(niobf)

  real :: uratio( ims:ime, jms:jme )   ! U to U10 ratio on momentum points.
  real :: vratio( ims:ime, jms:jme )   ! V to V10 ratio on momentum points.
  real :: pug1,pug2,pvg1,pvg2

! Define staggers for U, V, and T grids, referenced from non-staggered grid.
  real, parameter :: gridx_t = 0.5     ! Mass-point x stagger
  real, parameter :: gridy_t = 0.5     ! Mass-point y stagger
  real, parameter :: gridx_u = 0.0     ! U-point x stagger
  real, parameter :: gridy_u = 0.5     ! U-point y stagger
  real, parameter :: gridx_v = 0.5     ! V-point x stagger
  real, parameter :: gridy_v = 0.0     ! V-point y stagger

  real :: dummy = 99999.
  real, parameter :: tiny = 1.E-4

  real :: obs_pottemp                  ! Potential temperature at observation

!***  DECLARATIONS FOR IMPLICIT NONE
  integer :: nsta,ivar,n,ityp
  integer :: iob,job,kob,iob_ms,job_ms
  integer :: nlb,nle
  integer :: iobm,jobm,iobp,jobp,kobp,inpf
  integer :: i_start,i_end,j_start,j_end    ! loop ranges for uratio,vratio calc.
  integer :: k_start,k_end
!ajbij  integer :: i_lo,i_hi,j_lo,j_hi    ! loop ranges for uratio,vratio calc.
!ajbij  integer :: k_lo,k_hi
  integer ips                            ! For printing obs information

  real :: gridx,gridy,dxob,dyob,dzob
  real :: uratiob,vratiob,tratxob,fnpf
  real :: xhour                       ! Current model time in hours.
  REAL :: qc_new
  INTEGER :: nsndlev
  CHARACTER*40 :: platform

  Real::pppa,pppb
  Real,Dimension(NIOBF)::varmodel
  Integer::iwu
  Real,Dimension(kde-1)::pmod
  Integer::kmod
  Real::DZmod
  Real::th2ob           !th2 at obs site
  Real:: elevmod,ratmix,rhm,rho
  Real:: oldqc    ! needed for encoding/retrieving moisture qc 

  INTEGER,parameter :: opt_hum = 0   ! 1:specific humidity, 0:relative humidity

  Real::Last_Obs_Time
  Real::thetaobs
  Real :: terrmod
  Real,Dimension(NIOBF)::wwmodel
!!! wuyh added 20150729
  Real :: tmwu,rhobs,wutob
!!! wuyh added 20150729 end
!!! meixu 20170130
  real :: winddir_wrf                 ! model wind direction for turbine data calc
  real :: windsp_obs                  ! observed wind speed for turbine data calc
  real :: umod_tmp, vmod_tmp          ! temp for model wind for turbine data calc
!!! meixu 20170130 end

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  LOGICAL MP_LOCAL_UOBMASK(NIOBF)  ! Dot-point mask
  LOGICAL MP_LOCAL_VOBMASK(NIOBF)  ! Dot-point mask
  LOGICAL MP_LOCAL_COBMASK(NIOBF)  ! Cross-point mask
#endif

!ajbij
!! Set local tile dims
!    its = i_start(1)
!    ite = i_end(num_tiles)
!    jts = j_start(1)
!    jte = j_end(num_tiles)

! Initialize arrays used to hold model values at obs locations
!!! wuyh modified 20150727
!!  CALL init_model2missing(missing_r, niobf, uumodel, vvmodel, tmodel, rhmodel, rhobs)
  varmodel = missing_r
!!! wuyh modified 20150727
  NSTA=NSTAT

! FIRST, DETERMINE THE GRID TYPE CORRECTION FOR U-momentum, V-momentum,
! AND MASS POINTS, AND WHEN INEST=2, CONVERT THE STORED COARSE MESH INDICES
! TO THE FINE MESH INDEX EQUIVALENTS

! ITYP=1 FOR U-POINTS, ITYP=2 for V-POINTS, and ITYP=3 FOR MASS POINTS

  if (iprt) then
    write(6,'(a,i6,a,i2,a,i8,a)') '++++++CALL ERROB AT KTAU = ', &
            KTAU,' AND INEST = ',INEST,':  NSTA = ',NSTAW,' ++++++'
  endif

! Compute model time in hours.
  xhour = (xtime - 2.*dtmin) / 60.
  xhour = AMAX1(xhour-0.1,0.0)

! Zero out errf array only on first timestep.
  if (ktau .eq. 0 .or. ktau .eq. ktaur) then
    errf = float(0)    ! Zero out errf array
        write(6,*) 'errob check: errf array has been zeroed'
  endif

! Zero out errf array if obs time is in the future.
! do ivar = 1, nerrf
    do n = 1, nsta
      if(timeob(n) .ge. xhour) errf(1,n) = float(0)
    enddo
! enddo

! Set up loop bounds for this grid's boundary conditions
  i_start = max( its-1,ids )
  i_end   = min( ite+1,ide-1 )
  j_start = max( jts-1,jds )
  j_end   = min( jte+1,jde-1 )
  k_start = kts
  k_end = min( kte, kde-1 )
!ajbij
! i_lo    = max( its-1,ids )
! i_hi    = min( ite+1,ide-1 )
! j_lo    = max( jts-1,jds )
! j_hi    = min( jte+1,jde-1 )
! k_lo    = kts
! k_hi    = min( kte, kde-1 )

  DO ITYP=1,4   ! Big loop: 1 for U, 2 for V, 3 for T,Q,SP, 4 for Radar 

!   Set grid stagger
    IF(ITYP.EQ.1) THEN        ! U-POINT CASE
       GRIDX = gridx_u
       GRIDY = gridy_u
    ELSE IF(ITYP.EQ.2) THEN   ! V-POINT CASE
       GRIDX = gridx_v
       GRIDY = gridy_v
    ELSE                      ! MASS-POINT CASE
       GRIDX = gridx_t
       GRIDY = gridy_t
    ENDIF

!   Compute URATIO and VRATIO fields on momentum (u,v) points.
    IF(ityp.eq.1)THEN
      call upoint(i_start,i_end, j_start,j_end, ids,ide, ims,ime, jms,jme, uratx, uratio)
    ELSE IF (ityp.eq.2) THEN
      call vpoint(i_start,i_end, j_start,j_end, jds,jde, ims,ime, jms,jme, vratx, vratio)
    ENDIF
!ajbij
!   IF(ityp.eq.1)THEN
!     call upoint(i_lo,i_hi, j_lo,j_hi, ids,ide, ims,ime, jms,jme, uratx, uratio)
!   ELSE IF (ityp.eq.2) THEN
!     call vpoint(i_lo,i_hi, j_lo,j_hi, jds,jde, ims,ime, jms,jme, vratx, vratio)
!   ENDIF

!   PLACE EACH OBSERVATION IN THE MODEL MESH
    CALL place_Obs_in_Mesh( inest, ityp, nsta, niobf, vdim, nndgv,        &
                       elevob, rio, rjo, t0, rcp, pbase, pp, tb, z,       &
                       gridx, gridy, gridx_t, gridy_t, missing_r,         &
                       ims,ime, jms,jme, kms,kme, its,ite, jts,jte,       &
                       ide, jde, kds,kde, k_start, k_end, vobs, vobs_qc,  &
!ajbij                 ide, jde, kds,kde, k_lo, k_hi, vobs, vobs_qc,  & 
                       iobmg, jobmg, dxobmg, dyobmg, ra, rb, rc, rko )
    write(6,*) "after place_obs_in_Mesh with ITYP =",ITYP
!   INITIALIZE THE ARRAY OF DIFFERENCES BETWEEN THE OBSERVATIONS
!   AND THE LOCAL FORECAST VALUES TO ZERO.  FOR THE FINE MESH
!   ONLY, SET THE DIFFERENCE TO A LARGE DUMMY VALUE IF THE
!   OBSERVATION IS OUTSIDE THE FINE MESH DOMAIN.

!   SET DIFFERENCE VALUE TO A DUMMY VALUE FOR OBS POINTS OUTSIDE
!   CURRENT DOMAIN

    IF(ITYP.EQ.1) THEN
      NLB=U
      NLE=U
    ELSE IF(ITYP.EQ.2) THEN
      NLB=V
      NLE=V
    ELSE IF(ITYP.EQ.3) THEN
      NLB=T
      NLE=Q
    ELSE
      NLB=Vr
      NLE=Vr
    ENDIF

    DO IVAR=NLB,NLE
      DO N=1,NSTA
        IF(ob_var_type(n).eq.ivar) THEN
          if((RA(N)-1.).LT.0) then
            ERRF(1,N)=ERRF(1,N)+DUMMY
!           ERRF(2,N)=ERRF(2,N)+DUMMY
          endif
          if((RB(N)-1.).LT.0) then
            ERRF(1,N)=ERRF(1,N)+DUMMY
!           ERRF(2,N)=ERRF(2,N)+DUMMY
          endif
          if((FLOAT(ide)-2.0*GRIDX-RA(N)-1.E-10).LT.0) then
            ERRF(1,N)=ERRF(1,N)+DUMMY
!           ERRF(2,N)=ERRF(2,N)+DUMMY
          endif
          if((FLOAT(jde)-2.0*GRIDY-RB(N)-1.E-10).LT.0) then
            ERRF(1,N)=ERRF(1,N)+DUMMY
!           ERRF(2,N)=ERRF(2,N)+DUMMY
          endif
          if(rc(n).lt.1.) then
            ERRF(1,N)=ERRF(1,N)+DUMMY
!           ERRF(2,N)=ERRF(2,N)+DUMMY
          endif
        ENDIF
      ENDDO
    ENDDO

!   NOW FIND THE EXACT OFFSET OF EACH OBSERVATION FROM THE
!   GRID POINT TOWARD THE LOWER LEFT
    DO N=1,NSTA
        IA(N)=RA(N)
        IB(N)=RB(N)
        IC(N)=RC(N)
    ENDDO
    DO N=1,NSTA
        RA(N)=RA(N)-FLOAT(IA(N))
        RB(N)=RB(N)-FLOAT(IB(N))
        RC(N)=RC(N)-FLOAT(IC(N))
    ENDDO
!   PERFORM A TRILINEAR EIGHT-POINT (3-D) INTERPOLATION
!   TO FIND THE FORECAST VALUE AT THE EXACT OBSERVATION
!   POINTS FOR U, V, T, AND Q.

!**********************************************************
!   PROCESS U VARIABLE (IVAR=1)
!**********************************************************
    IF(ITYP.EQ.1) THEN

!   Compute local mask for U points.
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for U-momemtum points to be handled by this processor
        MP_LOCAL_UOBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      DO N=1,NSTA
        IF(MP_LOCAL_UOBMASK(N)) THEN
          ERRF(Rk,N)=rko(n)       !RKO is needed by neighboring processors     !ajb
        ENDIF
      ENDDO
#else
      DO N=1,NSTA
          ERRF(Rk,N)=rko(n)       !Use ERRF to hold rko for consistency        !ajb
      ENDDO
#endif
      IF(ISWIND.EQ.1) THEN
        DO N=1,NSTA
         if(ob_var_type(N).eq.U) then
          IOB=MAX0(2,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(2,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          KOB=MIN0(K_END,IC(N))
          KOBP=MIN0(KOB+1,k_end)
!ajbij    KOB=MIN0(K_HI,IC(N))
!ajbij    KOBP=MIN0(KOB+1,k_hi)

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_UOBMASK(N))THEN     ! Do if obs on this processor
#endif
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

!           Compute surface pressure values at surrounding U and V points
            PUG1 = .5*( pbase(IOBM,1,JOB) + pbase(IOB,1,JOB) )
            PUG2 = .5*( pbase(IOB,1,JOB) + pbase(IOBP,1,JOB) )

!!! wuyh modified 20140129
!            if(timeob(n) .ge. xhour) then
            if(timeob(n) .ge. xhour .OR. ktau .eq. 0 .OR. ktau .eq. ktaur)then
!!! wuyh modified 20140129

              IF(rko(n) .gt. 0.9)Then

!              This is to correct obs to model sigma level using reverse
!              similarity theory
               if(abs(rko(n)-1.) .LE. 1.0E-4)then
                 uratiob= (1.-DYOB)*((1.-DXOB)*uratio(IOB,JOB)      &
                                        +DXOB *uratio(IOBP,JOB))    &
                             +DYOB *((1.-DXOB)*uratio(IOB,JOBP)     &
                                        +DXOB *uratio(IOBP,JOBP))
               else
                 uratiob=1.
               endif
!YLIU          Some PBL scheme do not define the vratio/uratio
               if(abs(uratiob).lt.1.0e-3) then
                 uratiob=1.
               endif

!              INITIAL ERRF(1,N) IS ZERO FOR OBSERVATIONS POINTS
!              WITHIN THE DOMAIN, AND A LARGE DUMMY VALUE FOR POINTS
!              OUTSIDE THE CURRENT DOMAIN

!              U COMPONENT WIND ERROR
               varmodel(n) = ((1.-DZOB)*                                  &
                         ((1.-DyOB)*((1.-                                 &
                         DxOB)*UB(IOB,KOB,JOB)+DxOB*UB(IOB+1,KOB,JOB)     &
                         )+DyOB*((1.-DxOB)*UB(IOB,KOB,JOB+1)+DxOB*        &
                         UB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                         *UB(IOB,KOBP,JOB)+DxOB*UB(IOB+1,KOBP,JOB))+      &
                         DyOB*((1.-DxOB)*UB(IOB,KOBP,JOB+1)+DxOB*         &
                         UB(IOB+1,KOBP,JOB+1))))
! meixu 20170201 for turbine wind
!
               windsp_obs = -888888.0
               if (vobs_qc(1,n).ge.500000. .and. vobs_qc(1,n).le.599999. ) then
!                print*,'obs is turbine wind speed, compute model wind direction'
                 umod_tmp = varmodel(n)
                 vmod_tmp =((1.-DZOB)*                                   &
                        ((1.-DyOB)*((1.-                                  &
                         DxOB)*VB(IOB,KOB,JOB)+DxOB*VB(IOB+1,KOB,JOB)     &
                         )+DyOB*((1.-DxOB)*VB(IOB,KOB,JOB+1)+DxOB*        &
                         VB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                         *VB(IOB,KOBP,JOB)+DxOB*VB(IOB+1,KOBP,JOB))+      &
                         DyOB*((1.-DxOB)*VB(IOB,KOBP,JOB+1)+DxOB*         &
                         VB(IOB+1,KOBP,JOB+1))))
                 if ( (umod_tmp.ne.0.).and.(vmod_tmp.ne.0.) ) then
                   CALL calc_winddir(umod_tmp, vmod_tmp, winddir_wrf)
                 endif
! ====== re-calculate u, v from obs using model wind dir
                 windsp_obs = int((vobs_qc(1,n) - 500000.)/10.) / 100.0
                 vobs_qc(1,n) = vobs_qc(1,n) - int(vobs_qc(1,n)/10.) * 10.
                 vobs(1,n) = -windsp_obs*sin(4*atan(1.0)*winddir_wrf/180.)   ! obs U component
               endif 
! meixu 20170201 end for turbine wind

               if(max(abs(vobs(1,n)),abs(varmodel(n))) .lt. 400 )then

                 ERRF(1,N)=ERRF(1,N)+uratiob*VOBS(1,N)-varmodel(n)

                 !if(N.le.100) then
                 !write(6,'(a,i3,a,f14.6)') 'U: N = ',n,' ERRF(1,N) = ',ERRF(1,N)
                 !endif

!wuyh 20110530 added oneline QC 
                 IF ( qc_internal.and.(qc_domain.eq.inest.or.desobs(40,n).eq.1) ) THEN
                   if(vobs(Prs,n) .LE. -100.) THEN ! Skip obs without pressure
                     write(6,*)"Errob obs pressure in U for obs n=",n,oblat(n),oblon(n)
                     qc_new = vobs_qc(1,n)
                     cycle
                   endif
                   DO iwu = 1, 40 ! Grab the platform type
                    platform (iwu:iwu) = CHAR(platform_ob(iwu,n)) ! obs platorm
                   ENDDO
                   nsndlev = INT(nlevs_ob(n)+0.05) ! number of vertical levels

                   Call fddaobs_qc (n,platform,nsndlev,missing_r,       &
                         tolerance_u,tolerance_u_sound,                 &
                         U,opt_hum,vobs(1,n),vobs(Prs,n),errf(1,n),     &
                         vobs_qc(1,n),0.,t0,iprt,qc_new)

                   qc_new = qc_new/10.
                   vobs_qc(1,n)=qc_new
                 ENDIF

! meixu 20170201 for turbine wind
                 if (windsp_obs .gt. -400.) then
                   vobs_qc(1,n) = 500000.0 + int(windsp_obs * 100.0) * 10 + vobs_qc(1,n)
!                  print*,'for turbine wind, encode the original obs of wind speed in vobs_qc',vobs_qc(1,n)
                 endif
! meixu 20170201 end for turbine wind

! wuyh 20110530 added end
               endif  !judge if (vobs(1,n).LT.400)
              ELSE
               varmodel(n) = missing_r
               vobs_qc(1,n) =   0.
              ENDIF
            endif

!           Store model surface pressure (not the error!) at U point.
            ERRF(SPu,N)=.001*( (1.-DXOB)*PUG1 + DXOB*PUG2 )
  
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_UOBMASK(N) )
#endif
         endif  ! if(ob_var_type(N).eq.U)

        ENDDO    ! END U-POINT LOOP OVER OBS

      ENDIF    ! end if(iswind.eq.1)

    ENDIF   ! ITYP=1: PROCESS U

!**********************************************************
!   PROCESS V VARIABLE (IVAR=2)
!**********************************************************
    IF(ITYP.EQ.2) THEN

!   Compute local mask for V points.
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for V-momentum points to be handled by this processor
        MP_LOCAL_VOBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO

      IF(ISWIND.EQ.1) THEN
        DO N=1,NSTA
         if(ob_var_type(N).eq.V) then
          IOB=MAX0(2,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(2,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          KOB=MIN0(K_END,IC(N))
!ajbij    KOB=MIN0(K_HI,IC(N))

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_VOBMASK(N))THEN     ! Do if obs on this processor
#endif
            KOBP=MIN0(KOB+1,k_end)
!ajbij      KOBP=MIN0(KOB+1,k_hi)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

!           Compute surface pressure values at surrounding U and V points
            PVG1 = .5*( pbase(IOB,1,JOBM) + pbase(IOB,1,JOB) )
            PVG2 = .5*( pbase(IOB,1,JOB) + pbase(IOB,1,JOBP) )

!!! wuyh modified 20140129
!            if(timeob(n) .ge. xhour) then
            if(timeob(n) .ge. xhour .OR. ktau .eq. 0 .OR. ktau .eq. ktaur)then
!!! wuyh modified 20140129
              IF(rko(n) .gt. 0.9)Then

!              This is to correct obs to model sigma level using reverse
!              similarity theory
               if(abs(rko(n)-1.) .LE. 1.0E-4)then
                 vratiob= (1.-DYOB)*((1.-DXOB)*vratio(IOB,JOB)      &
                                        +DXOB *vratio(IOBP,JOB))    &
                             +DYOB *((1.-DXOB)*vratio(IOB,JOBP)     &
                                        +DXOB *vratio(IOBP,JOBP))
               else
                 vratiob=1.
               endif
!YLIU          Some PBL scheme do not define the vratio/uratio
               if(abs(vratiob).lt.1.0e-3) then
                 vratiob=1.
               endif

!              INITIAL ERRF(1,N) IS ZERO FOR OBSERVATIONS POINTS
!              WITHIN THE DOMAIN, AND A LARGE DUMMY VALUE FOR POINTS
!              OUTSIDE THE CURRENT DOMAIN

!              V COMPONENT WIND ERROR
               varmodel(n) =((1.-DZOB)*                                   &
                        ((1.-DyOB)*((1.-                                  &
                         DxOB)*VB(IOB,KOB,JOB)+DxOB*VB(IOB+1,KOB,JOB)     &
                         )+DyOB*((1.-DxOB)*VB(IOB,KOB,JOB+1)+DxOB*        &
                         VB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                         *VB(IOB,KOBP,JOB)+DxOB*VB(IOB+1,KOBP,JOB))+      &
                         DyOB*((1.-DxOB)*VB(IOB,KOBP,JOB+1)+DxOB*         &
                         VB(IOB+1,KOBP,JOB+1))))
! meixu 20170201 for turbine wind
!
               windsp_obs = -888888.0
               if (vobs_qc(1,n).ge.500000. .and. vobs_qc(1,n).le.599999. ) then
                 print*,'obs is turbine wind speed, compute model wind direction'
                 vmod_tmp = varmodel(n)
                 umod_tmp = ((1.-DZOB)*                                  &
                         ((1.-DyOB)*((1.-                                 &
                         DxOB)*UB(IOB,KOB,JOB)+DxOB*UB(IOB+1,KOB,JOB)     &
                         )+DyOB*((1.-DxOB)*UB(IOB,KOB,JOB+1)+DxOB*        &
                         UB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                         *UB(IOB,KOBP,JOB)+DxOB*UB(IOB+1,KOBP,JOB))+      &
                         DyOB*((1.-DxOB)*UB(IOB,KOBP,JOB+1)+DxOB*         &
                         UB(IOB+1,KOBP,JOB+1))))
                 if ( (umod_tmp.ne.0.).and.(vmod_tmp.ne.0.) ) then
                   CALL calc_winddir(umod_tmp, vmod_tmp, winddir_wrf)
                 endif
! ====== re-calculate u, v from obs using model wind dir
                 windsp_obs = int((vobs_qc(1,n) - 500000.)/10.) / 100.0
                 vobs_qc(1,n) = vobs_qc(1,n) - int(vobs_qc(1,n)/10.0) * 10.0
                 vobs(1,n) = -windsp_obs*cos(4*atan(1.0)*winddir_wrf/180.)   ! V component
               endif
! meixu 20170201 end for turbine wind

               if ( max(abs(vobs(1,n)),abs(varmodel(n))) .lt.400.)then

                 ERRF(1,N)=ERRF(1,N)+vratiob*VOBS(1,N)-varmodel(n)

                 !if(N.le.100) then
                 !write(6,'(a,i3,a,f14.6)') 'V: N = ',n,' ERRF(1,N) = ',ERRF(1,N)
                 !endif

!wuyh 20110530 added oneline QC 
                 IF ( qc_internal.and.(qc_domain.eq.inest.or.desobs(40,n).eq.1) ) THEN
                   if(vobs(Prs,n) .LE. -100.) THEN ! Skip obs without pressure
                     write(6,*)"Errob obs pressure in V for obs n=",n,oblat(n),oblon(n)
                     qc_new = vobs_qc(1,n)
                     cycle
                   endif
                   DO iwu = 1, 40 ! Grab the platform type
                    platform (iwu:iwu) = CHAR(platform_ob(iwu,n))
                   ENDDO
                   nsndlev = INT(nlevs_ob(n)+0.05) ! number of vertical levels
                   Call fddaobs_qc (n,platform,nsndlev,missing_r,      &
                        tolerance_u,tolerance_u_sound,                 &
                        V,opt_hum,vobs(1,n),vobs(Prs,n),errf(1,n),   &
                        vobs_qc(1,n),0.,t0,iprt,qc_new)

                   qc_new=qc_new/10.
                   vobs_qc(1,n)=qc_new
                 ENDIF

! meixu 20170201 for turbine wind
                 if (windsp_obs .gt. -400.) then
                   vobs_qc(1,n) = 500000.0 + int(windsp_obs * 100.0) * 10 + vobs_qc(1,n)
!                  print*,'for turbine wind, encode the original obs of wind speed in vobs_qc',vobs_qc(1,n)
                 endif
! meixu 20170201 end for turbine wind

               endif ! (vobs(1,n) .lt. 400)
! wuyh 20110530 added end

             ELSE
                varmodel(n)=-888888.
                vobs_qc(1,n) = 0.
             ENDIF
            endif

!           Store model surface pressure (not the error!) at V point.
            ERRF(SPv,N)=.001*( (1.-DYOB)*PVG1 + DYOB*PVG2 )
  
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_VOBMASK(N) )
#endif
         endif  ! if(ob_var_type(N).eq.V) then

        ENDDO    ! END V-POINT LOOP OVER OBS

      ENDIF    ! end if(iswind.eq.1)

    ENDIF   ! ITYP=2: PROCESS V

!**********************************************************
!   PROCESS MASS-POINT VARIABLES IVAR=3 (T) AND IVAR=4 (QV)
!**********************************************************
    IF(ITYP.EQ.3) THEN
!   Compute local mask for T points.
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for cross (mass) points to be handled by this processor
        MP_LOCAL_COBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO

      IF(ISTEMP.EQ.1 .OR. ISMOIS.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
          elevmod=missing_r
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_COBMASK(N)) THEN     ! Do if obs on this processor
#endif
            KOB=MIN0(k_end,IC(N))
            KOBP=MIN0(KOB+1,K_END)
!ajbij      KOB=MIN0(k_hi,IC(N))
!ajbij      KOBP=MIN0(KOB+1,K_HI)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

            IF(ob_var_type(N).eq.T) THEN
              if(timeob(n) .ge. xhour .OR. ktau .eq. 0 .OR. ktau .eq. ktaur)then

                IF(rko(n) .gt. 0.9)Then

! Calculate potential temperature innovation at the observation location.
                  CALL calc_pottemp_innov( rko(n), tb, th2, tratx,             &
                                 vobs(Prs,n), vobs(1,n), t0,                   &
                                 iob, job, kob, kobp, dxob, dyob, dzob,        &
                                 ims,ime, jms,jme, kms,kme, kde, missing_r,    &
                                 obs_pottemp )

! Calculate model temperature at the observation location.
                  CALL calc_tmodel( tb, vobs(Prs,n), t0, pbase, pp,            &
                                    iob, job, dxob, dyob,                      &
                                    ims,ime, jms,jme, kms,kme, kde, missing_r, &
                                    varmodel(n) )

                  if( abs(VOBS(1,N)).lt.400. .AND. abs(obs_pottemp) .lt. 500.) then

                    ERRF(1,N)=ERRF(1,N)+obs_pottemp

                    !if(N.le.100) then
                    !write(6,'(a,i3,a,f14.6)') 'T: N = ',n,' ERRF(1,N) = ',ERRF(1,N)
                    !endif

!wuyh 20110530 added oneline QC 
                    IF ( qc_internal.and.(qc_domain.eq.inest.or.desobs(40,n).eq.1) ) THEN

                      if(vobs(Prs,n) .LE. 4.0)then
                        write(6,*)"Errob obs pressure in T for obs n=",n,oblat(n),oblon(n)
                        write(6,*)"Errob T=",rko(n),vobs(Prs,n),vobs(Hgt,n)
                        qc_new = vobs_qc(1,n)
                        cycle
                      endif
                      DO iwu = 1, 40 ! Grab the platform type
                        platform (iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platorm
                      ENDDO
                      nsndlev = INT(nlevs_ob(n)+0.05) ! number of vertical levels
                      thetaobs=varmodel(n)

                      qc_new=vobs_qc(1,n)*10.
                      Call fddaobs_qc (n,platform,nsndlev,missing_r,         &
                           tolerance_t,tolerance_t_sound,                    &
                           T,opt_hum,vobs(1,n),vobs(Prs,n),errf(1,n),      &
                           vobs_qc(1,n),thetaobs,t0,iprt,qc_new)

                      qc_new=qc_new/10.
                      vobs_qc(1,n)=qc_new
                    ENDIF

! Store model surface pressure (not the error!) at T-point
                    ERRF(SPt,N)= .001*                                        &
                            ((1.-DyOB)*((1.-DxOB)*pbase(IOB,1,JOB)+DxOB*      &
                            pbase(IOB+1,1,JOB))+DyOB*((1.-DxOB)*              &
                            pbase(IOB,1,JOB+1)+DxOB*pbase(IOB+1,1,JOB+1) ))

                  endif ! vobs(1,n).lt. 600. .AND. abs(obs_pottemp) .lt. 600.)

                ELSE
                  varmodel(n) = missing_r
                  vobs(1,n)= missing_r
                  vobs_qc(1,n)=0.
                ENDIF

              endif  ! (timeob(n) .ge. xhour .OR. ktau .eq. 0 .OR. ktau .eq. ktaur)

            ELSEIF(ob_var_type(N).eq.Q) THEN
          
              if(timeob(n) .ge. xhour .OR. ktau .eq. 0 .OR. ktau .eq. ktaur)then

                IF(rko(n) .gt. 0.9)Then

! MOISTURE ERROR
                  ratmix =    (1.-DZOB)*((1.-DyOB)*((1.-DxOB)*QVB(IOB,KOB,JOB)    &
                         +                           DxOB *QVB(IOB+1,KOB,JOB))    &
                         +                DyOB *((1.-DxOB)*QVB(IOB,KOB,JOB+1)     &
                         +                           DxOB *QVB(IOB+1,KOB,JOB+1))) &
                         +     DZOB *((1.-DyOB)*((1.-DxOB)*QVB(IOB,KOBP,JOB)      &
                         +                           DxOB *QVB(IOB+1,KOBP,JOB))   &
                         +                DyOB *((1.-DxOB)*QVB(IOB,KOBP,JOB+1)    &
                         +                           DxOB *QVB(IOB+1,KOBP,JOB+1)))

                  CALL calc_tmodel( tb, vobs(Prs,n), t0, pbase, pp,            &
                                    iob, job, dxob, dyob,                      &
                                    ims,ime, jms,jme, kms,kme, kde, missing_r, &
                                    tmwu )

!!!! wuyh added 20160529
                  If(vobs(1,n) .ge. 200. .AND. vobs_qc(1,n) .ge. 1.20E4)then
                    wutob = vobs_qc(1,n)/100.
                    CALL calc_Qv(vobs(1,n),wutob,vobs(Prs,n)*10.,vobs(1,n),rhobs)

                  Elseif(vobs(1,n) .gt. -0.0001 .AND. vobs(1,n) .le. 1.)then

                    rhobs = retrieve_rh(vobs_qc(1,n))

                  Endif
!!!! wuyh added 20160529 end
!!! wuyh added 20130125 for relative humidity QC
                  If(opt_hum .eq. 0 .AND. vobs(Prs,n).LE.150. .AND. vobs(Prs,n).GT.5.)then
                    pppa=vobs(Prs,n)*10.                                   !mb
                    IF(tmwu .GT. 100. .AND. tmwu .LE. 400.    &
                     .and. ratmix .gt. -0.001)then
                     call  r2rhw(ratmix,tmwu,pppa,rhm,0)
                     varmodel(n) = ratmix
                    Else
                     varmodel(n) = missing_r
                    Endif

                    oldqc = retrieve_Qqc(vobs_qc(1,n))

                    if( rhobs .lt. 0 .or. rhobs .gt.102.1) then
                      rhobs = missing_r
                    Else
                    Endif
                  Else
                   varmodel(n) = missing_r
                   rhobs = missing_r
                  Endif
!!! wuyh added 20130125 end

!!! wuyh modified 20160608
!                  if(abs(VOBS(1,N)) .lt. 5.)then
                  if(VOBS(1,N) .gt. -0.001 .AND. VOBS(1,N) .lt. 1.)then
!!! wuyh modified 20160608 end

                    ERRF(1,N)=ERRF(1,N)+VOBS(1,N)-ratmix

                    !if(N.le.100) then
                    !write(6,'(a,i3,a,f14.6)') 'Q: N = ',n,' ERRF(1,N) = ',ERRF(1,N)
                    !endif
                    IF ( qc_internal.and.(qc_domain.eq.inest.or.desobs(40,n).eq.1) ) THEN
                      If (vobs(Prs,n) .LE. -100.) THEN
                        write(6,*)"Errob obs pressure in Q for obs n=",n,oblat(n),oblon(n)
                        qc_new = 0.
                        cycle
                      Endif
                      Do iwu = 1, 40  !Grab the platform type
                        platform (iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platorm
                      Enddo
                      nsndlev = INT(nlevs_ob(n)+0.05) !number of vertical level
                      If(opt_hum .eq. 0 ) then
                        if(vobs(Prs,n).LE.150. .AND. vobs(Prs,n).GT.5.)then
                         Call fddaobs_qc (n,platform,nsndlev,missing_r,       &
                               tolerance_q,tolerance_q_sound,                 &
                               Q,opt_hum,rhobs,vobs(Prs,n),rhm,               &
                               oldqc,0.,t0,iprt,qc_new)
!                         varmodel(n) = rhm
                        else
                         qc_new = 0.
                         varmodel(n)= missing_r
                        endif
                      Else  
                        Call fddaobs_qc (n,platform,nsndlev,-888888.,        &
                            tolerance_q,tolerance_q_sound,                   &
                            Q,opt_hum,vobs(1,n),vobs(Prs,n),errf(1,n),       &
                            oldqc,0.,t0,iprt,qc_new)
                        varmodel(n)= ratmix
                      Endif
                      qc_new = qc_new/10.
                      vobs_qc(1,n) = encode_rh_Qqc(rhobs,qc_new)
!ajb commented out:   write(295,1295)n,wutob,rhobs,qc_new,vobs_qc(1,n)
1295 Format(I8,1x,F8.1,2x,F8.2,1x,F10.2,1x,F12.2)
                    ENDIF ! (qc_internal.and.qc_domain.eq.inest)

! Store model surface pressure (not the error!) at T-point
                    ERRF(SPt,N)= .001*                                        &
                            ((1.-DyOB)*((1.-DxOB)*pbase(IOB,1,JOB)+DxOB*      &
                            pbase(IOB+1,1,JOB))+DyOB*((1.-DxOB)*              &
                            pbase(IOB,1,JOB+1)+DxOB*pbase(IOB+1,1,JOB+1) ))
                  else
                   vobs(1,N) = missing_r
                   vobs_qc(1,N) = missing_r 
                   rhobs      = missing_r
                   varmodel(n)    = missing_r
                  endif ! abs(VOBS(1,N)).lt. 600.
               ENDIF

              ELSE
                rhobs      = missing_r
                varmodel(n)    = missing_r
                vobs(1,n)   = missing_r
                vobs_qc(1,n)= 0.
              ENDIF ! (rko(n) .gt. 0.9)

            endif ! (timeob(n) .ge. xhour .OR. ktau .eq. 0 .OR. ktau .eq. ktaur)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF ! end IF( MP_LOCAL_COBMASK(N) )
#endif
        ENDDO ! END T and QV LOOP OVER OBS

      ENDIF !end if(istemp.eq.1 .or. ismois.eq.1)

    ENDIF ! end if(ityp.eq.3)

!!!!! wuyh added for radial wind 20140625
!**********************************************************
!   PROCESS RADIAL WIND (Vr)
!**********************************************************

    IF(ITYP.EQ.4) THEN

      IF(ABS(ISVR).EQ.1) THEN

        CALL errf_from_Vr(errf, varmodel,vobs, vobs_qc, isvr,                   &
                          niobf, vdim, nndgv, nerrf,                            &
                          nsta, xhour, ktau, ktaur,                             &
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
                          MP_LOCAL_COBMASK,                                     &
                          MP_LOCAL_UOBMASK, MP_LOCAL_VOBMASK,                   &
#endif
                          ob_var_type, ob_ndg_type, ndg_term_indxs,             &
                          gridx_u, gridy_u, gridx_v, gridy_v, gridx_t, gridy_t, &
                          timeob, rio, rjo, rko, ub, vb, wb, tb, th2,           &
                          tratx, t0, pbase, pp, qr, missing_r, stnid, elevob,   &
                          grid%dx, grid%dy, ide, jde, k_end, its,ite, jts,jte,  &
!ajbij                    grid%dx, grid%dy, ide, jde, k_hi, its,ite, jts,jte,  &
                          ims,ime, jms,jme, kms,kme, kde)

      ENDIF

    ENDIF

  ENDDO   ! END BIG LOOP

! Do QC inheritance
  if ( qc_internal) then
    CALL get_parent_QC(grid, grid%obs_prt_max, grid%obs_prt_freq, grid%fdob,    &
                       grid%fdob%qcdomprt, grid%obs_qc_domain,                  &
                       grid%obs_qc_alerts, inest)
  endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
! Gather the errf values calculated by the processors with the obs.
  CALL get_full_obs_vector(nsta, nerrf, niobf, mp_local_uobmask,    &
                           mp_local_vobmask, mp_local_cobmask,      &
                           ob_var_type, errf)
! wuyh added 20110630 
  CALL get_full_obs_qc(nsta, vdim, niobf, mp_local_uobmask,         &
                       mp_local_vobmask, mp_local_cobmask,          &
                       ob_var_type, vobs)

  CALL get_full_obs_qc(nsta, vdim, niobf, mp_local_uobmask,         &
                       mp_local_vobmask, mp_local_cobmask,          &
                       ob_var_type, vobs_qc)
!wuyh added 20110630 end

! wuyh added 20110630 
!!  CALL get_full_t_qc(nsta, niobf, mp_local_uobmask,uumodel)
!!  CALL get_full_t_qc(nsta, niobf, mp_local_vobmask,vvmodel)
  CALL get_full_t_qc(nsta, niobf, mp_local_cobmask,varmodel)
!!  CALL get_full_t_qc(nsta, niobf, mp_local_cobmask,rhmodel)
!!  CALL get_full_t_qc(nsta, niobf, mp_local_cobmask,rhobs)
!!  CALL get_full_t_qc(nsta, niobf, mp_local_cobmask,Vtm)
!wuyh added 20110630 end

!!! wuyh added 20140626
!!    CALL get_full_t_qc(nsta, niobf, mp_local_cobmask,wwmodel)
!   CALL get_full_t_qc(nsta, niobf, mp_local_cobmask,    vtm)
!!! wuyh added 20140626

! Do n=1,10
!   write(6,*) "wuyh rhobs=",n,rhobs(n)
! Enddo
! 02252010: Go ahead and assign rko for "obs-off" processors here, to
!           fix the problem where duplicate obs can be dropped from
!           the "obs-on" processor, but not from the others, due to
!           rko being -99 on the "obs-off" processors.
  do n=1,nsta
    rko(n) = errf(Rk,n)
!ajb: suppress this output
!   write(222,1222)N,vobs(Prs,n),rko(n)
 1222   Format(I8,1x,F10.2,1x,F8.2)
  enddo
! 02252010: End bugfix.
#endif

! Print obs information.
  CALL print_obs_info(iprt,inest,niobf,rio,rjo,rko,prt_max,prt_freq,       &
                      obs_prt,stnid_prt,lat_prt,lon_prt,mlat_prt,mlon_prt, &
                      qcdom_prt,timeob,xtime,kalmax,ob_is_kalman,          &
                      platform_ob,ob_var_type) 

! DIFFERENCE BETWEEN OBS AND FCST IS COMPLETED
  IF(INEST.EQ.1)THEN
    INPF=NPFI
  ELSE
    FNPF=IRATIO**LEVIDN(INEST)
    INPF=FNPF*NPFI
  ENDIF

! Gross error check for temperature. Set all vars bad.
  CALL gross_error_chk(inest,nsta,vdim,nndgv,nerrf,niobf,ob_var_type,vobs, &
                       vobs_qc, errf, varmodel,                            &
                       flagsnd,platform_ob,oblat,oblon,timeob)

  CALL write_everything(inest,nsta,nndgv,vdim,niobf,nerrf,opt_hum, &
                        xtime,dtmin,timeob,oblat,oblon,     &
                        varmodel,desobs, &
                        stnid,ionf, wudate,platform_ob, wusource, &
                        ob_var_type,vobs, vobs_qc, &
                        errf,nlevs_ob,elevob,flagsnd, flagbog, qcu, &
                        map_proj,truelat1,truelat2,cen_lat,cen_lon,Last_Obs_Time)
  write(6,*) "end of errob!"

  END SUBROUTINE errob

  SUBROUTINE upoint(i_start,i_end, j_start,j_end, ids,ide, ims,ime, jms,jme,  &
                    arrin, arrout)
!------------------------------------------------------------------------------
!     PURPOSE: This subroutine interpolates a real 2D array defined over mass
!              coordinate points, to U (momentum) points.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: i_start     ! Starting i index for this model tile
  INTEGER, INTENT(IN) :: i_end       ! Ending   i index for this model tile
  INTEGER, INTENT(IN) :: j_start     ! Starting j index for this model tile
  INTEGER, INTENT(IN) :: j_end       ! Ending   j index for this model tile
  INTEGER, INTENT(IN) :: ids         ! Starting i index for entire model domain
  INTEGER, INTENT(IN) :: ide         ! Ending   i index for entire model domain
  INTEGER, INTENT(IN) :: ims         ! Starting i index for model patch 
  INTEGER, INTENT(IN) :: ime         ! Ending   i index for model patch 
  INTEGER, INTENT(IN) :: jms         ! Starting j index for model patch 
  INTEGER, INTENT(IN) :: jme         ! Ending   j index for model patch 
  REAL,   INTENT(IN)  :: arrin ( ims:ime, jms:jme )  ! input array on mass points
  REAL,   INTENT(OUT) :: arrout( ims:ime, jms:jme )  ! output array on U points 

! Local variables
  integer :: i, j

! Do domain interior first
  do j = j_start, j_end
    do i = max(2,i_start), i_end
       arrout(i,j) = 0.5*(arrin(i,j)+arrin(i-1,j))
    enddo
  enddo

! Do west-east boundaries
  if(i_start .eq. ids) then
    do j = j_start, j_end
      arrout(i_start,j) = arrin(i_start,j)
    enddo
  endif
  if(i_end .eq. ide-1) then
    do j = j_start, j_end
      arrout(i_end+1,j) = arrin(i_end,j)
    enddo
  endif

  RETURN
  END SUBROUTINE upoint


  SUBROUTINE vpoint(i_start,i_end, j_start,j_end, jds,jde, ims,ime, jms,jme,  &
                    arrin, arrout)
!------------------------------------------------------------------------------
!     PURPOSE: This subroutine interpolates a real 2D array defined over mass
!              coordinate points, to V (momentum) points.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: i_start     ! Starting i index for this model tile
  INTEGER, INTENT(IN) :: i_end       ! Ending   i index for this model tile
  INTEGER, INTENT(IN) :: j_start     ! Starting j index for this model tile
  INTEGER, INTENT(IN) :: j_end       ! Ending   j index for this model tile
  INTEGER, INTENT(IN) :: jds         ! Starting j index for entire model domain
  INTEGER, INTENT(IN) :: jde         ! Ending   j index for entire model domain
  INTEGER, INTENT(IN) :: ims         ! Starting i index for model patch 
  INTEGER, INTENT(IN) :: ime         ! Ending   i index for model patch 
  INTEGER, INTENT(IN) :: jms         ! Starting j index for model patch 
  INTEGER, INTENT(IN) :: jme         ! Ending   j index for model patch 
  REAL,   INTENT(IN)  :: arrin ( ims:ime, jms:jme )  ! input array on mass points
  REAL,   INTENT(OUT) :: arrout( ims:ime, jms:jme )  ! output array on V points 

! Local variables
  integer :: i, j

! Do domain interior first
  do j = max(2,j_start), j_end
    do i = i_start, i_end
      arrout(i,j) = 0.5*(arrin(i,j)+arrin(i,j-1))
    enddo
  enddo

! Do south-north boundaries
  if(j_start .eq. jds) then
    do i = i_start, i_end
      arrout(i,j_start) = arrin(i,j_start)
    enddo
  endif
  if(j_end .eq. jde-1) then
    do i = i_start, i_end
      arrout(i,j_end+1) = arrin(i,j_end)
    enddo
  endif

  RETURN
  END SUBROUTINE vpoint

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

  TILE_MASK = (iloc .LE. ite .AND. iloc .GE. its .AND.    &
               jloc .LE. jte .AND. jloc .GE. jts )

  RETURN
  END FUNCTION TILE_MASK

!-----------------------------------------------------------------------
  SUBROUTINE nudob(j, ntrm, ivar, icvar,  aten,                       & 
                   inest, ifrest, ktau, ktaur,xtime,                  &
                   mu, msfx, msfy,                                    &
                   vdim, nndgv, nerrf, nscan,                         &
                   niobf, maxdom,                                     &
                   npfi, ionf, twindo_plt,                            &
                   levidn,                                            &
                   parid, nstat,                                      &
                   fdob, lev_in_ob, plfo, nlevs_ob,                   &
                   iratio, dx, dtmin, rio, rjo,                       &
                   rko, rkmob, timeob,                                &
                   vobs, vobs_qc, errf,                               &
                   pbase, ptop, pp,                                   &
                   iswind, istemp, ismois,                            &
                   isvr, isrf,                                        &
                   giv, git, giq,                                     &
                   givr,girf,                                         &
!!! wuyh added 20150322
                   wt,wt2err,                                         &
!!! wuyh added 20150322 end
                   savwt, kpblt, iscan,                               &
                   htij, iprt,                                        &
                   ids,ide, jds,jde, kds,kde,                         &                  ! domain dims
                   ims,ime, jms,jme, kms,kme,                         &                  ! memory dims
                   its,ite, jts,jte, kts,kte,                         &
                   Ndir,Npow,Kalmax,ndirpow,                          &
                   obs2kalman,swt,qc_internal)

!-----------------------------------------------------------------------
  USE module_model_constants
  USE module_domain
! FCV additions start
  USE module_fddaobs_qc
! FCV additions end
  USE module_fddaobs_utilities
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
! PURPOSE: THIS SUBROUTINE GENERATES NUDGING TENDENCIES FOR THE J-TH
!     VERTICAL SLICE (I-K PLANE) FOR FOUR-DIMENSIONAL DATA
!     ASSIMILATION FROM INDIVIDUAL OBSERVATIONS.  THE NUDGING
!     TENDENCIES ARE FOUND FROM A ONE-PASS CALCULATION OF
!     WEIGHTING FACTORS SIMILAR TO THE BENJAMIN-SEAMAN OBJECTIVE
!     ANALYSIS.  THIS SUBROUTINE IS DESIGNED FOR RAPID EXECUTION
!     AND MINIMAL STORAGE REQUIREMENTS.  ALGORITHMS SHOULD BE
!     VECTORIZED WHEREVER POSSIBLE.
!
!     HISTORY: Author, for MM5/RTFDDA                   Yubao Liu
!              02/04/2004 - Modified for WRF.           Al Bourgeois
!              08/28/2006 - F77 to F90                  Al Bourgeois
!------------------------------------------------------------------------------
!
! NOTE: This routine was originally designed for MM5, which uses
!       a nonstandard (I,J) coordinate system. For WRF, I is the 
!       east-west running coordinate, and J is the south-north
!       running coordinate. So a "J-slab" here is west-east in
!       extent, not south-north as for MM5.      -ajb 06/10/2004
!
!     NET WEIGHTING (WT) OF THE DIFFERENCE BETWEEN THE OBSERVATIONS
!     AND LOCAL FORECAST VALUES IS BASED ON THE MULTIPLE OF THREE
!     TYPES OF FACTORS:
!       1) TIME WEIGHTING - ONLY OBSERVATIONS WITHIN A SELECTED
!          TIME WINDOW (TWINDO) CENTERED AT THE CURRENT FORECAST
!          TIME (XTIME) ARE USED.  OBSERVATIONS CLOSEST TO
!          XTIME ARE TIME-WEIGHTED MOST HEAVILY (TIMEWT)
!       2) VERTICAL WEIGHTING - NON-ZERO WEIGHTS (WTSIG) ARE
!          CALCULATED WITHIN A VERTICAL REGION OF INFLUENCE
!          (RINSIG).
!       3) HORIZONTAL WEIGHTING - NON-ZERO WEIGHTS (WTIJ) ARE
!          CALCULATED WITHIN A RADIUS OF INFLUENCE (RINXY).  THE
!          VALUE OF RIN IS DEFINED IN KILOMETERS, AND CONVERTED
!          TO GRID LENGTHS FOR THE APPROPRIATE MESH SIZE.
!
!     THE FIVE FORECAST VARIABLES ARE PROCESSED BY CHANGING THE
!     VALUE OF IVAR AS FOLLOWS:
!             IVAR                     VARIABLE(TAU-1)
!             ----                     ---------------
!               1                             U
!               2                             V
!               3                             T
!               4                             QV
!               5                          PRESSURE
!
!-----------------------------------------------------------------------
!
!     Description of input arguments.
!
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, INTENT(IN)  :: j                          ! south-north running coordinate.
  INTEGER, INTENT(IN)  :: ntrm                       ! index of nudging term
  INTEGER, INTENT(IN)  :: ivar                       ! variable to nudge
  INTEGER, INTENT(IN)  :: icvar                      ! correlation variable
  INTEGER, INTENT(IN)  :: inest                      ! domain index
  LOGICAL, INTENT(IN)  :: ifrest
  INTEGER, INTENT(IN)  :: ktau
  INTEGER, INTENT(IN)  :: ktaur
  REAL, INTENT(IN)     :: xtime                      ! forecast time in minutes
  INTEGER, INTENT(IN)  :: vdim                       ! dim for vobs, vobs_qc
  INTEGER, INTENT(IN)  :: nndgv                      ! number of nudge variables
  INTEGER, INTENT(IN)  :: nerrf                      ! number of error fields
  INTEGER, INTENT(IN)  :: nscan                      ! number of nudging scans
  INTEGER, INTENT(IN)  :: niobf                      ! number of observations
  INTEGER, INTENT(IN)  :: maxdom                     ! maximum number of domains
  INTEGER, INTENT(IN)  :: npfi 
  INTEGER, INTENT(IN)  :: ionf
  REAL, INTENT(IN)     :: twindo_plt(15)             ! hlf-per twindo for each platfm type
  INTEGER, INTENT(IN)  :: levidn(maxdom)             ! level of nest
  INTEGER, INTENT(IN)  :: parid(maxdom)              ! parent domain id
  INTEGER, INTENT(IN)  :: nstat                      ! number of obs stations
  TYPE(fdob_type), intent(inout)  :: fdob
  REAL, INTENT(IN)     :: lev_in_ob(niobf)           ! Level in sounding-type obs.
  REAL, intent(IN)     :: plfo(niobf)
  REAL, intent(IN)     :: rkmob(niobf)               ! mean distance between observations
  REAL, INTENT(IN)     :: nlevs_ob(niobf)            ! Number of levels in sounding.
  INTEGER, INTENT(IN)  :: iratio                     ! Nest to parent gridsize ratio.
  REAL, INTENT(IN)     :: dx                         ! This domain grid cell-size (m)
  REAL, INTENT(IN)     :: dtmin
  REAL, INTENT(IN)     :: rio(niobf)
  REAL, INTENT(IN)     :: rjo(niobf)
  REAL, INTENT(INOUT)  :: rko(niobf)
  REAL, INTENT(IN)     :: timeob(niobf)
  REAL, INTENT(INOUT)  :: vobs(vdim, niobf)          ! observational variables, 1-ob, 1-var
  REAL, INTENT(INOUT)  :: vobs_qc(vdim, niobf)       ! obs var quality control, 1-ob, 1-var
  REAL, INTENT(IN)     :: errf(nerrf, niobf)
  REAL, INTENT(IN)     :: pbase( ims:ime, kms:kme )  ! Base pressure.
  REAL, INTENT(IN)     :: ptop                       ! Model top pressure (Pa)
  REAL, INTENT(IN)     :: pp( ims:ime, kms:kme ) ! Pressure perturbation (Pa)
  REAL, INTENT(IN)     :: mu(ims:ime)   ! Air mass on u, v, or mass-grid
  REAL, INTENT(IN)     :: msfx(ims:ime)  ! Map scale (only used for vars u & v)
  REAL, INTENT(IN)     :: msfy(ims:ime)  ! Map scale (only used for vars u & v)
  INTEGER, intent(in)  :: iswind        ! Nudge flag for wind
  INTEGER, intent(in)  :: istemp        ! Nudge flag for temperature
  INTEGER, intent(in)  :: ismois        ! Nudge flag for moisture
!!! wuyh 20140702 add
  INTEGER, INTENT(IN)  :: isvr           ! radial wind: (=0 off, =-1 U,V, =+1 Vr )
  INTEGER, INTENT(IN)  :: isrf           ! Z: (=0 off)
!!! wuyh 20140702 add end
  REAL, intent(in)     :: giv           ! Coefficient for wind
  REAL, intent(in)     :: git           ! Coefficient for temperature
  REAL, intent(in)     :: giq           ! Coefficient for moisture
!!! wuyh 20140702 add
  REAL, INTENT(IN)     :: givr          ! Coefficient for radial wind 
  REAL, INTENT(IN)     :: girf          ! Coefficient for reflectivity
!!! wuyh 20140702 add end
  REAL, INTENT(INOUT)  :: aten( ims:ime, kms:kme)
  REAL, INTENT(INOUT)  :: savwt( ims:ime, kms:kme, nscan )
  INTEGER, INTENT(IN)  :: kpblt(its:ite)
  INTEGER, INTENT(IN)  :: iscan                      ! scan index
  REAL, INTENT(IN)     :: htij(ids:ide, jds:jde)     ! terrain ht on global grid
  LOGICAL, INTENT(IN)  :: iprt                       ! print flag

! Local variables
  real :: ra(niobf)    
  real :: rb(niobf)    
  real :: psurf(niobf)
!!! wuyh modified to output these variables 20150322
   real :: wt(its:ite,kts:kte),wt2err(its:ite,kts:kte)
   real :: wu(its:ite,kts:kte),wu2err(its:ite,kts:kte)
!!  real :: wt(ims:ime,kms:kme),wt2err(ims:ime,kms:kme)
!!! wuyh modified to output these variables 20150322
  real :: rscale(ims:ime)           ! For converting to rho-coupled units.
  integer iplf                      ! temp to hold platform type as integer
  real :: rinxy                     ! temp to hold fdob%rinxy_plt value

!***  DECLARATIONS FOR IMPLICIT NONE
! integer :: i,k,icut,inpf,infr
  integer :: i,k,icut,inpf          !ajb 20091109 for adaptive step
  integer :: igrid,n
  integer :: maxj,minj,nnn,nsndlev,njcsnd,kob
  integer :: i_s,i_e
  integer :: istq
  real :: gfactor,rfactor,gridx,gridy,rindx,ris
  real :: timewt
  real :: pob
  real :: w2eowt,gitq

! yliu 20081126  add rindxs
  real :: rindxs
  integer :: MINJS, MAXJS
! yliu end
  real,parameter:: wuamp=5.
! wuyh added 20111102
  Real,Parameter::errcri = 15.
! wuyh added 20111102 end

  real :: twindo                    ! to store time-windo for a platform type

! Wuyh 20101211 added        
!  LOGICAL :: kalman=.false.

  Integer,Intent(IN)::Npow      !power  of polynomials
  Integer,Intent(IN)::Ndir      !number of directions
  Integer,Intent(IN)::ndirpow   !dimension of parameterized gain
                                !ndirpow=(Ndim+1)*(Ndim+2)/2
  Integer::Ndim                 !dimension of polynomial
  Integer,Intent(IN)::Kalmax    !maximum number of obs using Kalman gain
  Integer,Intent(IN)::obs2kalman(niobf)  !index in total obs series 
                                         !of those using Kalman gain  
  REAL,Dimension(ndirpow,kalmax),INTENT(IN)::swt ! gain at ob locations
  LOGICAL, INTENT(IN) :: qc_internal

  Real,Parameter:: pi=3.141592654 
  Integer,Parameter::mwt=1     !formula of weighting 
                               !1: stauffer 2: new
  Integer,Parameter::pnflag =1 !flag if using negative gain
                               !1 : using negative gain, 0: nonnegative 
  Real,Parameter ::errlim=1.0E-6
  Real,parameter ::wtijmax = 2.

! intermediate
  Real::theta0
  Real::dxkm           !dx in km
  Real::ptopk
! Real::trunca
  Real::GIVwu,GITQwu
  Real::wuaten
  Integer::Jout,Iout
  Integer::ndir2
  Real::rgainjp          !gain jump number,=2, gain is save every 2 grid points
  Integer,Parameter::minGain = 10  !if kalmax < minGain, we think we use Cressman as weight

! wuyh 20120709 added end

! wuyh 20120724 added
  Integer,Dimension(1:2)::ORDER2
  Character(Len=20)::FMT
  Integer::iu,ju
!  Real,Parameter::obsri = 54.533 , obsrj = 46.843   ! for output
  Real,Parameter::obsri = 54.5 , obsrj = 62.5   ! for output
! wuyh 20120724 added end

! ajb 02232015 added
!!! wuyh modified to save memory
!!  Integer :: ITYPE(nstat)     ! type of ob (index into ndg_term_indxs)
    Integer :: ITYPE
!!! wuyh modified to save memory
  Integer :: IVAROB(nstat)    ! observation ivar
  Integer :: ICVAROB(nstat)   ! observation icvar
! ajb 02232015 added end

  ORDER2 = (/ 1, 2 /)

  Jout=77
  Iout=35

  Ndim=Ndir
  ptopk = ptop/1000.
! wuyh 20120709 added
  Ndir2=2*Ndim+1
  rgainjp = float(npow)
  write(FMT,'("(",I0,"F6.2)")')Ndir2
! wuyh 20120709 added end

!yliu start -- for multi-scans: NSCAN=1: original
!                               NSCAN=2: added a scan with a larger Ri and smaller G
  GFACTOR=1. +  (ISCAN-1)*(-1. + 0.33333) 
  RFACTOR=1. +  (ISCAN-1)*(-1. + 3.0)
!yliu end
! jc

!!! Wuyh 20100802 added
  theta0=pi*float(2)/float(Ndir)
  dxkm=dx/1000.
!!! wuyh added 20150523
  wu = float(0)
  wu2err = float(0)
!!! wuyh added 200150523 end

!!! wuyh modified 20150612
!!  if(inest.eq.1.and.ivar.ge.3.and.(j.le.2.or.j.ge.jde-2)) then
  if(inest.eq.1 .and. (j.le.2.or.j.ge.jde-2)) then
!!! wuyh modified 20150612
!       write(6,*) '2 RETURN: IVAR = ',ivar,' J = ',j,
!    $             ' too close to boundary.'
    return
  endif

  ICUT=0
  IF(INEST.GT.1)ICUT=1
  i_s = max0(2+icut,its)
  i_e = min0(ide-1-icut,ite)

! DEFINE GRID-TYPE OFFSET FACTORS, IGRID AND GRID
  
  INPF=(IRATIO**LEVIDN(INEST))*NPFI
! INFR=(IRATIO**LEVIDN(INEST))*IONF      !ajb 20091109 for adaptive step

  GRIDX=float(0)
  GRIDY=float(0)
  IGRID=0
  IF(IVAR.GE.T)THEN
    GRIDX=0.5
    GRIDY=0.5
    IGRID=1
  ELSEIF(IVAR.eq.U) THEN
    GRIDY=0.5
    GRIDX=float(0)
    IGRID=1 
  ELSEIF(IVAR.eq.V) THEN
    GRIDX=0.5
    GRIDY=float(0)
    IGRID=1
  ENDIF

!********************************************************************
!ajb_07012008  Setting ra and rb for the fine mesh here is now simple.
!              Values are no longer calculated here based on the
!              coarse mesh, since direct use of WRF map projections
!              on each nest was implemented in subroutine in4dob.
!********************************************************************
! SET RA AND RB
  DO N=1,NSTAT
    RA(N)=RIO(N)-GRIDX
    RB(N)=RJO(N)-GRIDY
  ENDDO

! OUTPUT OBS PER GRID EVERY HOUR
if ( mod(xtime,60.).gt.59. .and. ivar.eq.3 .and. j.eq.10) then
    CALL print_obs_per_grid(iprt, inest, xtime, dtmin, twindo_plt, nstat, niobf, &
                            plfo, ra, rb, timeob, lev_in_ob, ide, jde)
  endif  ! END OUTPUT OBS PER GRID EVERY HOUR

  IF(IFREST.AND.KTAU.EQ.KTAUR)GOTO 5
!  IF(MOD(KTAU,INFR).NE.0)GOTO 126
   IF(fdob%CALC_STEP.NE.1)GOTO 126     !ajb 20091109 for adaptive step
5 CONTINUE
  IF (iprt) THEN
     rindx = 1000.*fdob%rinxy_plt(7)/DX * rfactor
   IF(J.EQ.10) write(6,6) INEST,J,KTAU,XTIME,IVAR,ISCAN,rindx
  ENDIF
6 FORMAT(1X,'OBS NUDGING FOR DOM_ID,J,KTAU,XTIME,',                &
            'IVAR,ISCAN: ',I2,1X,I2,1X,I5,1X,F8.2,1X,I2,1X,I2,     &
            ' rindx (plt=7,synop) =',f4.1)

! INITIALIZE WEIGHTING ARRAYS TO ZERO
!!! wuyh commented out 20150612
!!  DO I=its,ite
!!    DO K=1,kte
!!      WT(I,K)=float(0)
!!      WT2ERR(I,K)=float(0)
!!    ENDDO
!!  ENDDO
!!! wuyh comment out 20150612

  DO N=1,NSTAT
    ITYPE   = fdob%ob_ndg_type(N)
    IVAROB(N)  = fdob%ob_var_type(N)
    ICVAROB(N) = fdob%ndg_term_indxs(ITYPE,2)
  ENDDO

! ajb 02232015 added end

! DO P* COMPUTATIONS ON DOT POINTS FOR IVAR.LT.3 (U AND V)
! AND CROSS POINTS FOR IVAR.GE.3 (T,Q,P*).
!
! COMPUTE P* AT OBS LOCATION (RA,RB).  DO THIS AS SEPARATE VECTOR LOOP H
! SO IT IS ALREADY AVAILABLE IN NSTAT LOOP 120 BELOW

! PSURF IS NOT AVAILABLE GLOBALLY, THEREFORE, THE BILINEAR INTERPOLATION
! AROUND THE OBS POINT IS DONE IN ERROB() AND STORED IN ERRF([678],N) FOR
! THE POINT (6=PRESS, 7=U-MOM, 8=V-MOM).
  DO N=1,NSTAT
    IF(IVAROB(N).EQ.T.OR.IVAROB(N).EQ.Q)THEN
      PSURF(N)=ERRF(SPt,N)
    ELSE
      IF(IVAROB(N).EQ.U .OR. (IVAROB(N).EQ.Vr.AND.ICVAROB(N).EQ.U))THEN
        PSURF(N)=ERRF(SPu,N)        ! U-points
      ELSEIF(IVAROB(N).EQ.V .OR. (IVAROB(N).EQ.Vr.AND.ICVAROB(N).EQ.V)) THEN
        PSURF(N)=ERRF(SPv,N)        ! V-points
      ENDIF
    ENDIF
  ENDDO

  n=1

!***********************************************************************
  DO nnn=1,NSTAT   ! BEGIN OUTER LOOP FOR THE NSTAT OBSERVATIONS

!***********************************************************************
! Soundings are consecutive obs, but they need to be treated as a single 
! entity. Thus change the looping to nnn, with n defined separately.

!-----------------------------------------------------------------------
! ajb 20100311 - Set the Horiz Rad of Infl inside obs loop since it
!                is now platform dependent.
!-----------------------------------------------------------------------
! TRANSFORM THE HORIZONTAL RADIUS OF INFLUENCE, RINXY, FROM
! KILOMETERS TO GRID LENGTHS, RINDX

    iplf = nint(plfo(n))
    rinxy = fdob%rinxy_plt(iplf)
!  MX 20170427 - use obs dansity info if available
   if ( rkmob(n) .ge. 1. .and. rkmob(n) .le. 990. ) rinxy = rkmob(n)

!  If(J.EQ.22)then
!   write(802,1082)n,iplf,rinxy,RFACTOR
!  Endif
! 1082 Format(I7,1x,I8,2(1x,F10.2))

    RINDX=RINXY*1000./DX * RFACTOR   !yliu *RFACTOR

! yliu 20081126 - reduce influence radius for sfc data on coarse mesh
      if(DX .gt. 9999. .and. rinxy .ge. fdob%rinxy_plt(99) ) then
      rindxs=rindx* 0.5    ! yliu 20040502: half for Rinf at sfc for dx >= 10km
      else
      rindxs=rindx
      endif
! yliu 20081126 end

    RIS=RINDX*RINDX

! DETERMINE THE LIMITS OF THE SEARCH REGION FOR THE CURRENT
! J-STRIP

    MAXJ=J+IFIX(RINDX*fdob%RINFMX+0.99)                             !ajb
    MINJ=J-IFIX(RINDX*fdob%RINFMX+0.99)                             !ajb

! yliu 20081126
    MAXJS=J+IFIX(RINDXS+0.99)
    MINJS=J-IFIX(RINDXS+0.99)
! yliu 20081126 end
! ajb 20100311 end
!-----------------------------------------------------------------------

!yliu 
!  note for sfc data: nsndlev=1 and njcsnd=1
    nsndlev=int(nlevs_ob(n)-lev_in_ob(n))+1

! yliu start -- set together with the other parts
! test: do the sounding levels as individual obs
!   nsndlev=1
! yliu end
    njcsnd=nsndlev
! set pob here, to be used later
    pob=vobs(Prs,n)
! CHECK TO SEE OF STATION N HAS DATA FOR VARIABLE IVAR
! AND IF IT IS SUFFICIENTLY CLOSE TO THE J STRIP.  THIS
! SHOULD ELIMINATE MOST STATIONS FROM FURTHER CONSIDER-
! ATION.

!!! wuyh modified 20150520
!!    if(IVAROB(N).eq.ivar .and. ICVAROB(N).eq.icvar) then
    if(IVAROB(N).eq.icvar .and. ICVAROB(N).eq.ivar) then
!ajb note: This write causes hangs on multi-node runs. Must do monitor only.
!      write(216,1216)n,ivarob(n),icvar,icvarob(n),ivar
!1216  Format(I7,4(2x,i2))
!!! wuyh modified 20150520

      IF( ABS(ERRF(1,N)).GT.9.E4 .and. njcsnd.eq.1 ) THEN
!       print *, " bad obs skipped"
      ELSEIF( RB(N).LT.FLOAT(MINJ) .OR. RB(N).GT.FLOAT(MAXJ) ) THEN
!       print *, " skipped obs far away from this J-slice"
!yliu 20081126 sfc data smaller influence
      ELSEIF( (plfo(n).ne.5..and.plfo(n).ne.6.and.plfo(n).ne.7.and.plfo(n).ne.9) &
        .and.(RB(N).LT.FLOAT(MINJS) .OR. RB(N).GT.FLOAT(MAXJS)) ) THEN
!       print *, " skipped sfc obs far away from this J-slice"
!yliu 20081126 end
!----------------------------------------------------------------------
      ELSE    ! BEGIN SECTION FOR PROCESSING THE OBSERVATION
!----------------------------------------------------------------------

! DETERMINE THE LIMITS OF APPLICATION OF THE OBS IN THE VERTICAL
! FOR THE VERTICAL WEIGHTING, WTSIG

! ASSIMILATE OBSERVATIONS ON PRESSURE LEVELS, EXCEPT FOR SURFACE
!ajb 20021210: (Bugfix) RKO is not available globally. It is computed in
!ajb ERROB() by the processor handling the obs point, and stored in ERRF(Rk,N).

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
        rko(n) = errf(Rk,n)        !ajb 20021210
#endif
        KOB=nint(RKO(N))
        KOB=MIN0(kte,KOB)
        KOB=MAX0(1,KOB)

! Compute temporal weight
        twindo = twindo_plt(iplf)
        timewt = get_timewu(qc_internal,xtime,dtmin,twindo,1.0,timeob(n))
!!! wuyh added for vr 20150322
        if(icvar .eq. Vr) then
         timewt = get_timewu(qc_internal,xtime,dtmin,twindo,1.0,timeob(n))
        endif
!!! wuyh added for vr 20150322 end

!----------------------------------------------------------------------
! SECTION FOR PROCESSING OBSERVATIONS
!----------------------------------------------------------------------
        if(KOB.EQ.1.and.nlevs_ob(n).lt.1.5) THEN

! ASSIMILATE SURFACE LAYER DATA
          CALL surface_obs( inest, j, ivar, icvar, ntrm, n, iprt, isvr,               &
                            kalmax, ra(n), rb(n), rindxs, vobs(1,n),                  &
                            vobs_qc(1,n), errf(1,n), psurf(n), pbase,                 &
                            fdob%dcon, obs2kalman(n), ndir, ndir2, ndirpow, rgainjp,  &
                            swt, wtijmax, minGain, mwt, pnflag, kpblt, ktau, timewt,  &
                            its,ite,kte, ims,ime, kms,kme, ide,jde, wu, wu2err ) 
        else

!  ASSIMILATE UPPER AIR DATA
          CALL upper_air_obs( inest, j, ivar, icvar, ntrm, n, iprt, niobf,                 &
                              vdim, nndgv, nerrf,                                          &
                              kalmax, ra, rb, rindx, vobs, vobs_qc, errf, psurf, pbase,    &
                              pp, obs2kalman, ndir, ndir2, ndirpow, rgainjp,               &
                              swt, wtijmax, minGain, mwt, pnflag, kpblt, ktau, timewt,     &
                              fdob%rinfmn, fdob%rinfmx, fdob%pfree, pob, nsndlev, igrid,   &
                              lev_in_ob, nlevs_ob, isvr,                                   &
                              its,ite,kte, ims,ime, kms,kme, ide,jde, njcsnd, wu, wu2err )
  
        endif !end IF(KOB.EQ.1.AND.IVAR.LE.4.and.nlevs_ob(n).lt.1.5)

!----------------------------------------------------------------------
      ENDIF  ! END SECTION FOR PROCESSING OBSERVATIONS
!----------------------------------------------------------------------

    endif  ! if(IVARX(N).eq.ivar .and. ICVAROB(N).eq.icvar)

    n=n+njcsnd  !njcsnd is the number of obs for one station

    if(n.gt.nstat)then   

!! Wuyh 20100802 added nstat is the actual total number of obs
!! if the number of obs n > nstat, then stops and 
!! calculate the tendencies

!     print *,'n,nstat=',n,nstat,ivar,j
      go to 1203
    endif
!   print *, "e-- n=,nsndlev",n,njcsnd,nlevs_ob(n),lev_in_ob(n) 

!!!A2AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  ENDDO  ! END OUTER LOOP FOR THE NSTAT OBSERVATIONS
!!!A2AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


 1203 continue


! WEIGHTS AND WEIGHTED DIFFERENCES HAVE BEEN SUMMED.  NOW
! APPLY THE NUDGING FACTOR AND THE RESULTANT TENDENCY TO
! THE ATEN ARRAY
! ASSURE THAT WT(I,K) AND WTP(I,K) ARE NONZERO SINCE
! THEY ARE USED BELOW IN THE DENOMINATOR.
!!! wuyh modified 20150520
  DO K=kts,kte
    DO I=its,ite
      IF(WU(I,K).EQ.0.)THEN
        WU2ERR(I,K)=0.
!     ENDIF
!     IF(WU(I,K).EQ.0)THEN
        WU(I,K)=1.0E-8
      ENDIF
    ENDDO
  ENDDO
!!! wuyh modified 20150520

126 CONTINUE

  IF(IVAR.GT.Q) THEN
    CALL wrf_message("Error in subroutine NUDOB: IVAR is greater than 4.")
    CALL wrf_abort
  ENDIF
  IF(IVAR.GE.T)GOTO 170
! this is for u,v
! 3-D DOT POINT TENDENCIES

! Calculate scales for converting nudge factor from u (v)
! to rho_u (or rho_v) units.

  IF (IVAR == U) THEN
     call calc_rcouple_scales(mu,msfy,rscale,ims,ime,its,ite)
  ELSE IF (IVAR == V) THEN
     call calc_rcouple_scales(mu,msfx,rscale,ims,ime,its,ite)
  END IF

!!!*************************************************************************
! Calculate the tendencies of U,V
!!!*************************************************************************
  GIVwu=GIV !wuyh added 20100902

!!! wuyh added 20140707
  IF(ICVAR == Vr) Then
     GIVwu=givr
!    write(602,*)"ICVAR,ampli=",ICVAR,GIVwu
  EndIF
!!! wuyh added 20140707 end

!!! wuyh modified 20150322
  DO K=1,kte
    DO I=i_s,i_e

      IF(fdob%CALC_STEP.EQ.1.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN     !ajb 20091109 for adaptive step

      IF(abs(WU(i,k)) .gt. 1.E-6 .AND. abs(WU(i,k)) .lt. 3.E4)then
        WT2ERR(i,k)=WT2ERR(i,k)+GIVwu*RSCALE(I)*WU2ERR(i,k)*fdob%TFACI*ISWIND*GFACTOR
        WT(i,k) = WT(i,k)+WU(i,k)
      ENDIF
     ENDIF
!    if(icvar == Vr) then
!       write(197,1197)ivar,K,I,J,WU2ERR(I,K),WU(I,K),WT2ERR(I,K)
!    endif
    ENDDO
  ENDDO
 1197 Format(I3,1x,I3,2(1x,I4),3(1x,E15.8))

  RETURN

170 CONTINUE

!!!*************************************************************************
! Calculate the tendencies of T, Qv
!!!*************************************************************************
! 3-D CROSS-POINT TENDENCIES
! this is for t (ivar=3) and q (ivar=4)

  IF(3-IVAR.LT.0)THEN
    GITQ=GIQ
  ELSE
    GITQ=GIT
  ENDIF
  IF(3-IVAR.LT.0)THEN
    ISTQ=ISMOIS
  ELSE
    ISTQ=ISTEMP
  ENDIF

  GITQwu=GITQ
!!! wuyh comment out 20150322 with !!
  DO K=1,kte
    DO I=i_s,i_e
      IF(fdob%CALC_STEP.EQ.1.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN    !ajb 20091109 for adaptive step

      WT2ERR(i,k)=WT2ERR(i,k)+GITQwu*MU(I)*WU2ERR(i,k)*fdob%TFACI*ISTQ*GFACTOR
      WT(i,k) = WT(i,k)+WU(i,k)
!!      write(197,1197)ivar,K,I,J,WU2ERR(I,K),WU(I,K),WT2ERR(I,K)
     ENDIF
    ENDDO
  ENDDO

  RETURN
  END SUBROUTINE nudob

  SUBROUTINE surface_obs( inest, j, ivar, icvar, ntrm, n, iprt, kalmax, ivr,       &
                          ra, rb, rindxs, vobs, vobs_qc, errf, psurf, pbase,       &
                          dcon, obs2kalman, ndir, ndir2, ndirpow, rgainjp,         &
                          swt, wtijmax, minGain, mwt, pnflag, kpblt, ktau, timewt, &
                          its,ite,kte, ims,ime, kms,kme, ide,jde, wt, wt2err )
!----------------------------------------------------------------------------
!  PURPOSE: ASSIMILATE SURFACE LAYER DATA ON SIGMA
!----------------------------------------------------------------------------
  USE module_fddaobs_utilities, ONLY: wucri, retrieve_Qqc
  IMPLICIT NONE
!----------------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: inest                      ! Domain index 
  INTEGER, INTENT(IN)  :: j                          ! south-north running coordinate 
  INTEGER, INTENT(IN)  :: ivar                       ! variable to nudge
  INTEGER, INTENT(IN)  :: icvar                      ! correlation variable (not used)
  INTEGER, INTENT(IN)  :: ntrm                       ! index of nudging term
  INTEGER, INTENT(IN)  :: n                          ! observation index (not used)
  LOGICAL, INTENT(IN)  :: iprt                       ! flag enabling printing warning messages
  INTEGER, INTENT(IN)  :: kalmax                     ! max number of kalman type obs
  INTEGER, INTENT(IN)  :: ivr                        ! Radial wind flag: 1 for Vr, -1 Vr2UV, 0 off
  REAL,    INTENT(IN)  :: ra
  REAL,    INTENT(IN)  :: rb
  REAL,    INTENT(IN)  :: rindxs
  REAL,    INTENT(IN)  :: vobs                       ! observation values (not used)
  REAL,    INTENT(IN)  :: vobs_qc                    ! obs qc flag
  REAL,    INTENT(IN)  :: errf                       ! innovation
  REAL,    INTENT(IN)  :: psurf                      ! surface pressure
  REAL,    INTENT(IN)  :: pbase( ims:ime, kms:kme )  ! Base pressure.
  REAL,    INTENT(IN)  :: dcon                       ! 1/DPSMX
  INTEGER, INTENT(IN)  :: obs2kalman                 ! index in total obs series 
  INTEGER, INTENT(IN)  :: ndir                       ! number of directions
  INTEGER, INTENT(IN)  :: ndir2                      ! 2*Ndir+1
  INTEGER, INTENT(IN)  :: ndirpow        ! dim for parameterized gain: ndirpow=(Ndir+1)*(Ndir+2)/2
  REAL,    INTENT(IN)  :: rgainjp                    ! power of polynomials
  REAL,Dimension(ndirpow,kalmax),INTENT(IN)::swt     ! gains at ob locations 
  REAL,    INTENT(IN)  :: wtijmax                    ! max value for wtij weight
  INTEGER, INTENT(IN)  :: minGain                    ! parameter for decision to use Cressman wt
  INTEGER, INTENT(IN)  :: mwt                        ! formula of weighting (=1: stauffer, =2: new)
  INTEGER, INTENT(IN)  :: pnflag                ! flag for negative gain (=1: neg gain, =0: nonneg) 
  INTEGER, INTENT(IN)  :: kpblt(its:ite)             ! PBL top
  INTEGER, INTENT(IN)  :: ktau                       ! current timestep
  REAL,    INTENT(INOUT) :: timewt                   ! temporal weight
  INTEGER, INTENT(IN)  :: its,ite, kte               ! needed tile dims.
  INTEGER, INTENT(IN)  :: ims,ime, kms,kme           ! needed memory dims.
  INTEGER, INTENT(IN)  :: ide,jde                    ! needed domain dims.
  REAL,    INTENT(OUT) :: wt(its:ite,1:kte)
  REAL,    INTENT(OUT) :: wt2err(its:ite,1:kte)

! Local variables
  real    :: wtsig(kms:kme)
  integer :: MINIS, MAXIS
  real    :: irx, iry
  integer :: i, k
  real    :: ri,rj,rx,ry,rsq,wtij,pdfac,erfivr,dk
  real    :: ris
  real    :: scratch
  integer :: komin,komax
  real    :: sigwt,wut,wue,wuinvo
  real    :: qc
!!! wuyh added 20150525
  integer :: iwuvar, iwuwgt
  Real :: varcri
!!! wuyh added 200150525

  DO K=1,kte
    WTSIG(K)=float(0)
  ENDDO
! DEFINE WTSIG: (FOR SRP: SPREAD SURFACE DATA THROUGH LOWEST 200 M)
!       WTSIG(1)=1.0
!       WTSIG(2)=0.67
!       WTSIG(3)=0.33
!       KOMIN=3
!       KOMAX=1
! DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO
! WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX (IN GRID LENGTHS).
! fix this because kpblt at 1 and il is 0
! yliu 20081126
!       MAXI=IFIX(RA(N)+0.99+RINDX)
!       MAXI=MIN0(ide-1,MAXI)
  MAXIS=IFIX(RA+0.99+RINDXS)
  MAXIS=MIN0(ide-1,MAXIS)
  MINIS=IFIX(RA-RINDXS-0.99)
  MINIS=MAX0(2,MINIS)
!!! wuyh modified 20150525
  IF(ivr .EQ. 1) Then
    iwuvar = icvar
    iwuwgt = ntrm
  Else
    iwuvar = ivar
    iwuwgt = ivar
  Endif 
  varcri = wucri(ivar)

!ajb note: This write causes hangs on multi-node runs. Must do monitor only.
!  write(284,*) "nudob iwuvar iwuwgt =",iwuvar,iwuwgt
!!! wuyh modified 20150525
   

! yliu 20081126 end
!yliu start
! use also obs outside of this domain  -- surface obs
!     if(RA.LT.0.-RINDX .or. RA.GT.float(IL+RINDX) .or.
!    &   RB.LT.0.-RINDX .or. RB.GT.float(JL+RINDX)) then
!        print *, " skipped obs far away from this domain"
! currently can use obs within this domain or ones very close to (1/3
!   influence of radius in the coarse domain) this
!   domain. In later case, use BC column value to approximate the model value
!   at obs point -- ERRF need model field in errob.F !!
! yliu 20080222: do not use obs outside the domain
!       if (  RA.GE.(0.-RINDX/3)                        &
!       .and. RA.LE.float(ide)+RINDX/3                  &
!       .and. RB.GE.(0.-RINDX/3)                        &
!       .and. RB.LE.float(jde)+RINDX/3) then
  if (  RA.GE.1.                        &
  .and. RA.LE.float(ide)                &
  .and. RB.GE.1.                        &
  .and. RB.LE.float(jde) ) then
! yliu 20080220 end
! or use obs within this domain only
!     if(RA.LT.1 .or. RA.GT.float(IL) .or.
!    &   RB.LT.1 .or. RB.GT.float(JL)) then
!        print *, " skipped obs far outside of this domain"
!        if(j.eq.3 .and. ivar.eq.3) then
!           write(6,*) 'N = ',n,' exit 120 3'
!        endif
!yliu end
!
! LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING
! OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO
! THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS
    RJ=FLOAT(J)
    RX=RJ-RB
    IRX=float(NINT(RX))

! Retrieve encoded qc if necessary

!!! wuyh modified 20160529
!!    if(ivar.eq.Q) then
      qc = retrieve_Qqc(vobs_qc)
!!    else
!!      qc = vobs_qc
!!    endif
!!! wuyh modified end 20160529

! CALCULATE WEIGHTS FOR THE 3-D VARIABLES IF DATA IS GOOD
!!!! wuyh modified 20160529     
    if(qc .gt. 0. .AND. qc .le. 1.1       &
       .and. ABS(errf) .lt. varcri )then

      ERFIVR=ERRF * qc

      ILOOP: DO I=max0(its,MINIS),min0(ite,MAXIS)

        RI=FLOAT(I)
        RY=RI-RA
        IRY=float(NINT(RY))
        RIS=RINDXS*RINDXS
        RSQ=IRX*IRX+IRY*IRY

        If(RSQ .GT. RIS)Then
          goto 55553
        Endif

! THIS FUNCTION DECREASES WTIJ AS PSFC CHANGES WITHIN SEARCH RADIUS
        scratch = (abs(psurf-.001*pbase(i,1))*DCON)
        pdfac=1.-AMIN1(1.0,scratch)

        if(obs2kalman.GE.1) then
          wtij=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,obs2kalman))

          if(abs(wtij).GT.wtijmax)then
            wtij=wtijmax*wtij/abs(wtij)
          endif
          wtij=wtij*trunca(RSQ,RIS)
          wtij=wtij*pdfac

! no mix use of kalman gain and cressman, so if no gain, neglect the obs
        elseif(kalmax .ge. minGain )then
          wtij = float(0)
          EXIT ILOOP
        else
          wtij=(ris-rsq)/(ris+rsq)
          wtij=wtij*pdfac
          WTIJ=AMAX1(0.0,WTIJ)             
        endif

! try making sfc obs weighting go thru pbl
! jc kpbl is at dot or cross only - need to interpolate?
!          wtsig(1)=1.
            komax=max0(3,kpblt(i))

! jc arbitrary check here
! yliu-- used to be 25, now change to 2/3 of kte  20080721
        IF (iprt) THEN
          if (kpblt(i).gt.kte*2/3 .and. ktau.ne.0) then
             write(6,552)inest,i,j,kpblt(i)
          endif
552         FORMAT('kpblt is gt kte*2/3, inest,i,j,kpblt=',4i4)
        ENDIF

        if(kpblt(i).gt.kte*2/3) komax=kte*2/3
        komin=1
        dk=float(komax)

        do k=komin,komax

          wtsig(k)=float(komax-k+1)/dk
          sigwt=WTSIG(K)
          wuinvo=ERFIVR

          Call Jiaquan(mwt,pnflag,TIMEWT,sigwt,WTIJ,wuinvo,wut,wue)

! if(i.eq.48.and.j.eq.5.and.k.eq.1) then
!   write(6,'(2(a,i2),5(a,f9.5))')                                                   &
!         'surface_obs after Jiaquan: i=48,j=5,k=1: N = ',n,'  ivar = ',ivar,        &
!         '  wt2err = ',WT2ERR(I,K),'  wue = ',wue, '  wt = ',wt(i,k),               &
!         '  wut = ',wut,'  wuinvo = ',wuinvo
! endif

          WT(I,K)=WT(I,K)+wut
          WT2ERR(I,K)=WT2ERR(I,K)+wue

        enddo
55553   Continue
      ENDDO ILOOP

    endif ! end calculate weights if data good

!         print *, "  Surface "

  endif   ! end check for obs in domain
! END SURFACE-LAYER U OR V OBS NUDGING

  END SUBROUTINE surface_obs

  SUBROUTINE upper_air_obs( inest, j, ivar, icvar, ntrm, n, iprt,                           &
                            niobf, vdim, nndgv, nerrf,                                      &
                            kalmax, ra, rb, rindx, vobs, vobs_qc, errf, psurf, pbase,       &
                            pp, obs2kalman, ndir, ndir2, ndirpow, rgainjp,                  &
                            swt, wtijmax, minGain, mwt, pnflag, kpblt, ktau, timewt,        &
                            rinfmn, rinfmx, pfree, pob, nsndlev, igrid,                     &
                            lev_in_ob, nlevs_ob, ivr,                                       &
                            its,ite,kte, ims,ime, kms,kme, ide,jde, njcsnd, wt, wt2err )
!----------------------------------------------------------------------------
!  PURPOSE: BEGIN CALCULATIONS TO SPREAD OBS INFLUENCE ALONG PRESSURE LEVELS
!----------------------------------------------------------------------------
  USE module_fddaobs_utilities
  IMPLICIT NONE
!----------------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: inest                      ! Domain index 
  INTEGER, INTENT(IN)  :: j                          ! south-north running coordinate 
  INTEGER, INTENT(IN)  :: ivar                       ! variable to nudge
  INTEGER, INTENT(IN)  :: icvar                      ! correlation variable
  INTEGER, INTENT(IN)  :: ntrm                       ! index of nudging term
  INTEGER, INTENT(IN)  :: n                          ! observation index
  LOGICAL, INTENT(IN)  :: iprt                       ! flag enabling printing warning messages
  INTEGER, INTENT(IN)  :: niobf                      ! number of observations
  INTEGER, INTENT(IN)  :: vdim                       ! Dim for arrays vobs, vobs_qc
  INTEGER, INTENT(IN)  :: nndgv                      ! number of nudge variables
  INTEGER, INTENT(IN)  :: nerrf                      ! number of error fields
  INTEGER, INTENT(IN)  :: kalmax                     ! max number of kalman type obs
  REAL,    INTENT(IN)  :: ra(niobf)    
  REAL,    INTENT(IN)  :: rb(niobf)    
  REAL,    INTENT(IN)  :: rindx
  REAL,    INTENT(IN)  :: vobs(vdim,niobf)           ! observation values (not used)
  REAL,    INTENT(IN)  :: vobs_qc(vdim,niobf)        ! obs qc flag
  REAL,    INTENT(IN)  :: errf(nerrf, niobf)         ! innovation
  REAL,    INTENT(IN)  :: psurf(niobf)               ! surface pressure
  REAL,    INTENT(IN)  :: pbase( ims:ime, kms:kme )  ! Base pressure.
  REAL,    INTENT(IN)  :: pp( ims:ime, kms:kme )     ! Pressure perturbation (Pa)
  INTEGER, INTENT(IN)  :: obs2kalman(niobf)          ! index in total obs series 
  INTEGER, INTENT(IN)  :: ndir                       ! number of directions
  INTEGER, INTENT(IN)  :: ndir2                      ! 2*Ndir+1
  INTEGER, INTENT(IN)  :: ndirpow        ! dim for parameterized gain: ndirpow=(Ndir+1)*(Ndir+2)/2
  REAL,    INTENT(IN)  :: rgainjp                    ! power of polynomials
  REAL,Dimension(ndirpow,kalmax),INTENT(IN)::swt     ! gains at ob locations 
  REAL,    INTENT(IN)  :: wtijmax                    ! max value for wtij weight
  INTEGER, INTENT(IN)  :: minGain                    ! parameter for decision to use Cressman wt
  INTEGER, INTENT(IN)  :: mwt                        ! formula of weighting (=1: stauffer, =2: new)
  INTEGER, INTENT(IN)  :: pnflag                ! flag for negative gain (=1: neg gain, =0: nonneg) 
  INTEGER, INTENT(IN)  :: kpblt(its:ite)             ! PBL top
  INTEGER, INTENT(IN)  :: ktau                       ! current timestep
  REAL,    INTENT(INOUT) :: timewt                   ! temporal weight
  REAL,    INTENT(IN)  :: rinfmn
  REAL,    INTENT(IN)  :: rinfmx
  REAL,    INTENT(IN)  :: pfree
  REAL,    INTENT(IN)  :: pob
  INTEGER, INTENT(INOUT) :: nsndlev
  INTEGER, INTENT(IN)  :: igrid
  REAL,    INTENT(IN)  :: lev_in_ob(niobf)           ! level in sounding-type obs.
  REAL,    INTENT(IN)  :: nlevs_ob(niobf) 
!!! Wuyh added 20140815
  INTEGER, INTENT(IN)  :: ivr                        ! Radial wind flag: 1 for Vr, -1 Vr2UV, 0 off
!!! Wuyh added 20140815 end
  INTEGER, INTENT(IN)  :: its,ite, kte               ! needed tile dims.
  INTEGER, INTENT(IN)  :: ims,ime, kms,kme           ! needed memory dims.
  INTEGER, INTENT(IN)  :: ide,jde                    ! needed domain dims.
  INTEGER, INTENT(OUT) :: njcsnd                     ! number levels in sounding
  REAL,    INTENT(inOUT) :: wt(its:ite,1:kte)
  REAL,    INTENT(inOUT) :: wt2err(its:ite,1:kte)
!!! wuyh modified 20150525

! Local variables
  real    :: slope
  real    :: rinfac
  integer :: MINI, MAXI
  real    :: ri,rj,rx,ry,rsq,wtij,erfivr
  real    :: irx, iry
  real    :: reserf(numsnd), resqc(numsnd)
  real    :: wtsig(kms:kme)
  integer :: i,k,nn,komin,komax,kobs
  real    :: ris,rinprs
  real    :: pijk,pobhi,poblo,pdiffj
  real    :: sigwt,wut,wue,wuinvo
  real    :: fkk
  real    :: wtijhi,wtijlo       !horizontal weighting at nhi & nlo level for sounding 
  integer :: nhi,nlo,nnjc
  real    :: qc(numsnd)
  real, Parameter :: vgmin = 0.25   !minimum k-level diff
  real, Parameter :: qcmin = 0.10   !minimum k-level diff
!!! wuyh added 200150525
  Integer :: iwuvar, iwuwgt
  Real :: varcri
!!! wuyh added 200150525

! print *,'in upper air section'
! DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO
! WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX, AND RINFAC.
! RINFAC VARIES AS A LINEAR FUNCTION FROM FROM RINFMN AT P*+PTOP
! TO RINFMX AT PFREE AND "ABOVE" (LOWER PRESSURE).

        slope = (RINFMN-RINFMX)/(psurf(n)-PFREE)

        RINFAC=SLOPE*POB+RINFMX-SLOPE*pfree
        RINFAC=AMAX1(RINFAC,RINFMN)
        RINFAC=AMIN1(RINFAC,RINFMX)
! For multilevel upper-air data, take the maximum for the I loop.
        if(nsndlev.gt.1) RINFAC = RINFMX

        MAXI=IFIX(RA(N)+0.99+RINDX*RINFAC)
        MAXI=MIN0(ide-IGRID,MAXI)
        MINI=IFIX(RA(N)-RINDX*RINFAC-0.99)
        MINI=MAX0(1,MINI)

!!! wuyh added 20150525
        IF(ivr .EQ. 1) Then
          iwuvar = icvar
          iwuwgt = ntrm
        Else
          iwuvar = ivar
          iwuwgt = ivar
        Endif
        varcri = wucri(ivar)
!!! wuyh modified 20150525

! use also obs outside of but close to this domain  -- upr data   
!     if(   RA(N).LT.(0.-RINFAC*RINDX)
!    & .or. RA(N).GT.float(IL)+RINFAC*RINDX
!    & .or. RB(N).LT.(0.-RINFAC*RINDX)
!    & .or. RB(N).GT.float(JL)+RINFAC*RINDX)then          
!        print *, " skipped obs far away from this I-range"
! currently can use obs within this domain or ones very close to (1/3
!   influence of radius in the coarse domain) this 
!   domain. In later case, use BC column value to approximate the model value 
!   at obs point -- ERRF need model field in errob.F !!
        if(   RA(N).GE.(0.-RINFAC*RINDX/3)                      &
        .and. RA(N).LE.float(ide)+RINFAC*RINDX/3                &
        .and. RB(N).GE.(0.-RINFAC*RINDX/3)                      &
        .and. RB(N).LE.float(jde)+RINFAC*RINDX/3) then

! LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING
! OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO
! THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS
          RJ=FLOAT(J)
          RX=RJ-RB(N)
          IRX=float(NINT(RX))

! Retrieve encoded qc if necessary
          do nn = 1, nlevs_ob(n)
!!            if(ivar.eq.Q) then
              qc(nn) = retrieve_Qqc(vobs_qc(1,n+nn-1))
!!            else
!!              qc(nn) = vobs_qc(1,n+nn-1)
!!               - float(100*INT(vobs_qc(1,n+nn-1)/100.+1.E-4))
!!            endif
          enddo

! WEIGHTS FOR THE 3-D VARIABLES
          if(qc(1) .gt. 0.1 .AND. qc(1) .le. 1.1 &
               .and. ABS(errf(1,n)) .lt. varcri )then
!             write(582,1584)n,ivar,icvar,ntrm,errf(iwuvar,n)," G"
              ERFIVR=ERRF(1,N) * qc(1)
          else
!             write(584,1584)n,ivar,icvar,ntrm,errf(iwuvar,n)," B"
              ERFIVR=float(0)
          endif
1584      Format("n,ivar,icvar,ntrm,errf=",I8,3(1x,I3),1x,F10.2,A)
! jc
          nsndlev=int(nlevs_ob(n)-lev_in_ob(n))+1
          njcsnd=nsndlev

          ILOOP: DO I=max0(its,MINI),min0(ite,MAXI)
! jc
            RI=FLOAT(I)
            RY=RI-RA(N)
            IRY=float(NINT(RY))
            RIS=(RINDX*RINFAC)*(RINDX*RINFAC)
!!            write(231,1231)ivr,icvar
1231        Format("ivr icvar =",I2,1x,I2)
            RSQ=RX*RX+RY*RY

! weight ob in vertical with +- 50 mb
! yliu: 75 hba for single upper-air, 30hba for multi-level soundings
            if(nsndlev.eq.1) then
              rinprs=7.5
            else
             rinprs=3.0
            endif

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   ---  HANDLE 1-LEVEL and MULTI-LEVEL OBSERVATIONS SEPARATELY  ---
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

            if(nsndlev.eq.1)then
!----------------------------------------------------------------------
!         ---   HANDLE 1-LEVEL OBSERVATIONS  ---
!----------------------------------------------------------------------
            IF(RSQ.GT.RIS) Go To 55557

            if(obs2kalman(n).GE.1)then
!!! wuyh modified 20140701
              wtij=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,obs2kalman(n)))
!ajb          wtij=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,ivar,obs2kalman(n)))

!!! wuyh modified 20140701
              if(abs(wtij).GT.wtijmax)then
                wtij=wtijmax*wtij/abs(wtij)
              endif
              wtij=wtij*trunca(RSQ,RIS)
            elseif(kalmax .ge. minGain) then
              wtij = float(0)
              EXIT ILOOP
            else
              WTIJ=(RIS-RSQ)/(RIS+RSQ)
              WTIJ=AMAX1(float(0),WTIJ)
            endif

!         if(I.eq.MINI) print *, "  Single snd "
! ERFIVR is the residual (difference) between the ob and the model
! at that point. We can analyze that residual up and down.
! First find komin for ob.
              do k=kte,1,-1
                pijk = .001*(pbase(i,k)+pp(i,k))
                if(pijk.ge.(pob+rinprs)) then
                  komin=k
                  go to 325
                endif
              enddo
              komin=1
 325          continue
! now find komax for ob
              do k=3,kte
                pijk = .001*(pbase(i,k)+pp(i,k))
                if(pijk.le.(pob-rinprs)) then
                  komax=k
                  go to 326
                endif
              enddo
              komax=kte   ! yliu 20050706
 326          continue

! Single-level upper-air data will act either above or below the PBL top
! Reset komin or komax. if kobs>kpblt, komin=kpblt+1, else komax=kpblt

              if( (kpblt(i).le.komax) .and. (kpblt(i).ge.komin) ) then
                 kobs = 1
                 OBS_K: do k = komin, komax
                     if( pob .gt. .001*(pbase(i,k)+pp(i,k)) ) then
                        kobs = k
                        EXIT OBS_K
                     endif
                 enddo OBS_K

                 if(kobs.gt.kpblt(i)) then
                     komin=max0(kobs, komin)   ! kobs here is kpblt(i)+1
                 else
                     komax=min0(kpblt(i), komax)
                 endif
              endif

              do k=1,kte
                reserf(k)=float(0)
                wtsig(k)=float(0)
              enddo

              do k=komin,komax
                pijk = .001*(pbase(i,k)+pp(i,k))
                reserf(k)=erfivr
                wtsig(k)=1.-abs(pijk-pob)/rinprs
                wtsig(k)=amax1(wtsig(k),float(0))

! Now calculate WT and WT2ERR for each i,j,k point                      cajb
               sigwt=WTSIG(K)
               wuinvo=reserf(k)

               Call Jiaquan(mwt,pnflag,TIMEWT,SIGwt,WTIJ,wuinvo,wut,wue)

               WT(I,K)=WT(I,K)+wut
               WT2ERR(I,K)=WT2ERR(I,K)+wue

              enddo

            else
!----------------------------------------------------------------------
!         ---   HANDLE MULTI-LEVEL OBSERVATIONS  ---
!----------------------------------------------------------------------

              if(nlevs_ob(n+nsndlev-1).ne.lev_in_ob(n+nsndlev-1)) then
                IF (iprt) THEN
                  print *, "n = ",n,"nsndlev = ",nsndlev
                  print *, "nlevs_ob,lev_in_ob",                        &
                           nlevs_ob(n+nsndlev-1),lev_in_ob(n+nsndlev-1)
                  print*, "in nudobs.F: sounding level messed up, stopping"
                ENDIF
                stop
              endif

! This is for a multi-level observation
! The trick here is that the sounding is "one ob". You don't
!    want multiple levels to each be treated like separate
!    and independent observations.
! At each i,j want to interpolate sounding to the model levels at that
!    particular point.
              komin=1
              komax=kte                  !!- 1            !-2 wuyh removed 20101002
! this loop goes to 1501
! do from kte-2 to 1 so don't adjust top of model. Arbitrary.

              do k=1,kte
                reserf(k)=float(0)
                wtsig(k)=float(0)
              enddo
              RSQ=RX*RX+RY*RY

             slope=(RINFMN-RINFMX)/(.001*pbase(i,1)-PFREE)

             DO k=komax,komin,-1             !from top to lower atmosphere

                fkk = float(k)

                pijk = .001*(pbase(i,k)+pp(i,k))  !model pressure in KPa

                RINFAC=SLOPE*pijk+RINFMX-SLOPE*PFREE
                RINFAC=AMAX1(RINFAC,RINFMN)
                RINFAC=AMIN1(RINFAC,RINFMX)
                RIS=RINDX*RINFAC*RINDX*RINFAC

                IF(RSQ.GT.RIS) cycle
                WTIJ=(RIS-RSQ)/(RIS+RSQ)
                WTIJ=AMAX1(float(0),WTIJ)

                pobhi=-888888.
                poblo=-888888.

!****** case 1: all obs above pijk level**************************************
               if(fkk .LE. errf(Rk,n)) then !model sigma <= lowest ob sigma

                  if((errf(Rk,n)-fkk) .LE. vgmin )then

                     If(obs2kalman(n).GE.1)Then
!!! wuyh modified 20140701
                      wtij=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,obs2kalman(n)))
!ajb                  wtij=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,ivar,obs2kalman(n)))
!!! wuyh modified 20140701 end
                      if(abs(wtij).GT.wtijmax)then
                        wtij=wtijmax*wtij/abs(wtij)
                      endif
                      wtij=wtij*trunca(RSQ,RIS)
                      pobhi=-5556.
                      poblo=-5556.
                     Elseif(kalmax .ge. minGain) then
                      wtij = float(0)
                      EXIT ILOOP
                     Else  !cressman
                      WTIJ=(RIS-RSQ)/(RIS+RSQ)
                      WTIJ=AMAX1(float(0),WTIJ)
                      pobhi=-5558.
                      poblo=-5558.
                     Endif

                     If(ABS(errf(1,n)).LT. varcri    &
                        .and. qc(1) .gt. qcmin       &
                        .and. qc(1) .le. 1.1)then
                        reserf(k)=errf(1,n)
                        resqc(k) = qc(1)
                       wtsig(k)=float(1)
                       pobhi=-5558.
                       poblo=vobs(Prs,n)
                       go to 1501
                     Else
                       cycle
                     Endif

                  else
                       cycle
                  endif

               elseif(fkk .GE. errf(Rk,n+nsndlev-1)) then         !top

!b. if sigma level pressure < the highest ob level, don't interpolate
!****** case 2: all obs below pijk level******************************
                 nn= n+nsndlev-1

                 IF(fkk-errf(Rk,nn) .LT. vgmin)then

                    if(obs2kalman(nn).GE.1)then
!!! wuyh modified 20140701
                      wtij=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,obs2kalman(n)))
!ajb                  wtij=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,ivar,obs2kalman(n)))
!!! wuyh modified 20140701 end
                      if(abs(wtij).GT.wtijmax)then
                        wtij=wtijmax*wtij/abs(wtij)
                      endif
                      wtij=wtij*trunca(RSQ,RIS)
                      pobhi=-1188.
                      poblo=-1188.
                    Elseif(kalmax .ge. minGain) then
                      wtij = float(0)
                      EXIT ILOOP
                    else
                      WTIJ=(RIS-RSQ)/(RIS+RSQ)
                      WTIJ=AMAX1(float(0),WTIJ)
                      pobhi=-1186.
                      poblo=-1186.
                    endif
                    If(ABS(errf(1,nn)).LT. 20.             &
                       .and. qc(nn-n+1) .gt. -0.01         &
                       .and. qc(nn-n+1) .le. 1.01)then
                         reserf(k)=errf(1,nn)* qc(nn-n+1)
                         wtsig(k)=float(1)
                         go to 1501
                    Else
                        cycle
                    Endif
                 ELSE
                    cycle
                 ENDIF

! now interpolate sounding to this k
! yliu start-- recalculate WTij for each k-level
!                slope = (RINFMN-RINFMX)/ (.001*pbase(i,1)-PFREE)
!                RINFAC=SLOPE*pijk+RINFMX-SLOPE*PFREE
!                RINFAC=AMAX1(RINFAC,RINFMN)
!                RINFAC=AMIN1(RINFAC,RINFMX)
!                RIS=RINDX*RINFAC*RINDX*RINFAC
!                RSQ=RX*RX+RY*RY
!                WTIJ=(RIS-RSQ)/(RIS+RSQ)
!                WTIJ=AMAX1(0.0,WTIJ)
!wuyh added 20100804
! yliu end

!!!*********************************************
!!! wuyh added 20111019 get the weighting
!!!*********************************************
               else
                Do nn=n,n+nsndlev-2      ! from bottom to top
                  if(errf(Rk,nn).le.fkk .and. errf(Rk,nn+1).gt.fkk)then

                    if(obs2kalman(nn).GE.1 .AND. obs2kalman(nn+1).GE.1)then
                       pdiffj=float(NINT((fkk-errf(Rk,nn))/(errf(Rk,nn+1)-errf(Rk,nn))))
!!! wuyh modified 20140701
                       wtijlo=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,obs2kalman(nn)))
!ajb                   wtijlo=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,ivar,obs2kalman(nn)))
!!! wuyh modified 20140701 end
                       if(abs(wtijlo).GT.wtijmax)then
                        wtijlo=wtijmax*wtijlo/abs(wtijlo)
                       endif
!!! wuyh modified 20140701
                       wtijhi=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,obs2kalman(nn+1)))
!ajb                   wtijhi=polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,swt(:,ivar,obs2kalman(nn+1)))
!!! wuyh modified 20140701 end
                       if(abs(wtijhi).GT.wtijmax)then
                        wtijhi=wtijmax*wtijhi/abs(wtijhi)
                       endif
                       wtij=(wtijlo+(wtijhi-wtijlo)*pdiffj)*trunca(RSQ,RIS)

                    endif  !judge if it is a kalman obs

                  endif   !judge if upper & lower levels exist
                Enddo
!!!*********************************************
!!! wuyh added 20111019 
!!!*********************************************
                do nn=n+1,n+nsndlev-1      ! from bottom to top

                  if(abs(errf(1,nn)).lt. varcri                     &
                     .and. qc(nn-n+1).gt.qcmin                   &
                     .and. qc(nn-n+1).le.1.1                     &
                     .and. errf(Rk,nn) .gt. fkk                  &
                     .and. abs(vobs(Prs,nn)-pijk) .lt. 10.) then !yliu 20110929, avoid bad obs (qc <= 0.2)
                      nhi=nn

                      go to 1502
                  endif
                enddo

!!! wuyh added 20111019 if no lower obs level only upper obs level avaiable
                do nnjc=n+nsndlev-1,n,-1
                   If(    abs(errf(1,nnjc)) .lt. varcri         &
                    .and. qc(nnjc-n+1) .gt. qcmin               &
                    .and. qc(nnjc-n+1) .le. 1.1                 &
                    .and. errf(Rk,nnjc).le. fkk                 &
                    .and. abs(vobs(Prs,nnjc)-pijk).lt.10.) then  !yliu 20110929, avoid bad obs (qc <= 0.2)
                      nlo=nnjc
                      reserf(k)=errf(1,nlo)
                      resqc(k) =qc(nlo-n+1)
                      reserf(k)=reserf(k)* resqc(k)
                      wtsig(k)=float(1)
                      go to 1501        ! find an obs within 100mb of obs height
                   Endif
                enddo

                wtij = float(0)
                pobhi=-888883.
                cycle

!!! wuyh added 20111019 end if no lower obs level only upper obs level avaiable

 1502           continue

                do nnjc=nhi-1,n,-1
                  if(abs(errf(1,nnjc)).lt. varcri               &
                    .and. qc(nnjc-n+1).gt.qcmin                 &
                    .and. qc(nnjc-n+1).le.1.1                   &
                    .and. errf(Rk,nnjc).le. fkk                 &
                    .and. abs(vobs(Prs,nnjc)-pijk).lt. 10.) then !yliu20110929, avoid bad obs (qc <= 0.2)
                      nlo=nnjc
                      go to 1505
                  endif
                enddo

                poblo=-888886.        !wuyh added 
!yliu end --
                reserf(k)=errf(1,nhi)*qc(nhi-n+1)
                wtsig(k)=float(1)
                pobhi=-3388.
                poblo=-3008.

                go to 1501  ! did not find any ob below within 200 mb

 1505           continue

! interpolate to model level
                pdiffj=(fkk-errf(Rk,nlo))/(errf(Rk,nhi)-errf(Rk,nlo))

                reserf(k)=errf(1,nlo)+                           &
                            (errf(1,nhi)-errf(1,nlo))*pdiffj
                resqc(k)=qc(nlo-n+1) +                           &
                  (qc(nhi-n+1)-qc(nlo-n+1))*pdiffj
                reserf(k) = reserf(k)     * resqc(k)
                wtsig(k)=float(1)

 1501           continue

! now calculate WT and WT2ERR for each i,j,k point               cajb

                  sigwt=WTSIG(K)
                  wuinvo=reserf(k)
                  Call Jiaquan(mwt,pnflag,TIMEWT,sigwt,WTIJ,wuinvo,wut,wue)

                  WT(I,K)=WT(I,K)+wut
                  WT2ERR(I,K)=WT2ERR(I,K)+wue

 8042            Format(3(I3,1x),F8.2,3(1x,F10.2),4(2x,E11.5),4(1x,F6.2))
               endif    !judge the location of k level model   

!wuyh 20100722 end

 55556         Continue
              ENDDO   ! enddo k levels

!----------------------------------------------------------------------
            endif  ! end if(nsndlev.eq.1)
!----------------------------------------------------------------------

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   END 1-LEVEL AND MULTI-LEVEL OBSERVATIONS
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! wuyh added 20110209
 55557 Continue

! wuyh added 20110209 ended
          ENDDO ILOOP ! END DO MINI,MAXI LOOP

        endif ! check for obs in domain

! END OF NUDGING TO OBS ON PRESSURE LEVELS

  END SUBROUTINE upper_air_obs

  SUBROUTINE errob_diagnostic(n, nout, ivar, inest, iob, job, kob, kobp,     &
                  nndgv, nerrf, niobf, dxob, dyob, dzob, nv,                 &
                  obval, mval, obpval, errf, ims,ime, jms,jme, kms,kme,      &
                  pbase, Vr_ob, Vr_model)
!------------------------------------------------------------------------------
!  PURPOSE: Used by subroutine ERROB for diagnostic purposes only.
!           Writes out U, V, T, or Q model, obs, and error values,
!           along with computed nudging tendencies.
!-----------------------------------------------------------------------
  USE module_fddaobs_utilities
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN) :: n                 ! Obs station index 
  INTEGER, INTENT(IN) :: nout              ! Number of stations to print
  INTEGER, INTENT(IN) :: ivar              ! 1=U, 2=V, 3=T, 4=Q
  INTEGER, INTENT(IN) :: inest             ! Nest index
  INTEGER, INTENT(IN) :: iob               ! I coordinate of obs
  INTEGER, INTENT(IN) :: job               ! J coordinate of obs
  INTEGER, INTENT(IN) :: kob               ! K coordinate of obs 
  INTEGER, INTENT(IN) :: kobp              ! K+1 or model top level
  INTEGER, INTENT(IN) :: nndgv             ! Number of nudge variables
  INTEGER, INTENT(IN) :: nerrf             ! Number of error fields
  INTEGER, INTENT(IN) :: niobf             ! Number of observations
  REAL,    INTENT(IN) :: dxob              ! Interpolation fraction in x
  REAL,    INTENT(IN) :: dyob              ! Interpolation fraction in y
  REAL,    INTENT(IN) :: dzob              ! Interpolation fraction in z
  REAL,    INTENT(IN) :: nv(ims:ime,kms:kme,jms:jme)    ! Nudge variable
  REAL,    INTENT(IN) :: obval             ! Observation value
  REAL,    INTENT(IN) :: mval              ! Model value
  REAL,    INTENT(IN) :: obpval            ! Observation pressure value
  REAL,    INTENT(IN) :: errf(nerrf, niobf)    ! Model errors
  INTEGER, INTENT(IN) :: ims,ime, jms,jme, kms,kme      ! Memory dims
  REAL,    INTENT(IN), OPTIONAL :: pbase(ims:ime,kms:kme,jms:jme) ! Model pressure
  REAL,    INTENT(IN), OPTIONAL :: Vr_ob      ! Observed radial wind
  REAL,    INTENT(IN), OPTIONAL :: Vr_model   ! Model radial wind

! Local variables
  character*16 :: vars ="UVTQ"   ! Variable names array
  character*4  :: varname        ! Variable name

  varname(1:1) = vars(ivar:ivar)

  if(n.le.nout) then

    write(6,*)
    write(6,'(a)') 'DIAGNOSTICS FOR ERROB CALCULATION:'
    write(6,'(a1,a,3i3,a,i4,a,i2)') varname(1:1),         &
                       ' error at: ', iob,job,kob,        &
                       ' for N = ',n,' inest = ',inest
    write(6,*) 'OBS VALUE            = ',obval
    write(6,*) 'MODEL VALUE FOR ',varname(1:1),'    = ',mval
    IF( PRESENT(pbase) )    write(6,*) 'MODEL SF PRESS (pa)  = ',pbase(IOB,1,JOB)
    IF( PRESENT(Vr_ob) )    write(6,*) 'OBSERVED RADIAL WIND = ',Vr_ob
    IF( PRESENT(Vr_model) ) write(6,*) 'MODEL RADIAL WIND    = ',Vr_model
    write(6,*) 'OBS PRESSURE (pa)    = ',obpval*1000.
    write(6,*) varname(1:1),'(IOB,KOB,JOB)       = ',NV(IOB,KOB,JOB)
    write(6,*) varname(1:1),'(IOB+1,KOB,JOB)     = ',NV(IOB+1,KOB,JOB)
    write(6,*) varname(1:1),'(IOB,KOB,JOB+1)     = ',NV(IOB,KOB,JOB+1)
    write(6,*) varname(1:1),'(IOB+1,KOB,JOB+1)   = ',NV(IOB+1,KOB,JOB+1)
    write(6,*) varname(1:1),'(IOB,KOBP,JOB)      = ',NV(IOB,KOBP,JOB)
    write(6,*) varname(1:1),'(IOB+1,KOBP,JOB)    = ',NV(IOB+1,KOBP,JOB)
    write(6,*) varname(1:1),'(IOB,KOBP,JOB+1)    = ',NV(IOB,KOBP,JOB+1)
    write(6,*) varname(1:1),'(IOB+1,KOBP,JOB+1)  = ',NV(IOB+1,KOBP,JOB+1)
    write(6,*) 'DXOB,DYOB,DZOB       = ',DXOB,DYOB,DZOB
    write(6,*) 'MODEL ERROR          = ',errf(1,n)
    write(6,*)
  endif

  RETURN
  END SUBROUTINE errob_diagnostic

  SUBROUTINE nudob_diagnostic(i, j, k, ityp, ivar, wt2err, timewt,    &
                              wtij, wtsig, erfivr, wt, iscan)
!------------------------------------------------------------------------------
!  PURPOSE: Used by subroutine NUDOB for diagnostic purposes only.
!           Writes out components of nudging weights (WT2ERR)
!           computed for U, V, T, or Q at a specific gridpoint.
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN) :: i
  INTEGER, INTENT(IN) :: j
  INTEGER, INTENT(IN) :: k
  INTEGER, INTENT(IN) :: ityp
  INTEGER, INTENT(IN) :: ivar
  REAL,    INTENT(IN) :: wt2err
  REAL,    INTENT(IN) :: timewt
  REAL,    INTENT(IN) :: wtij
  REAL,    INTENT(IN) :: wtsig
  REAL,    INTENT(IN) :: erfivr
  REAL,    INTENT(IN) :: wt
  INTEGER, INTENT(IN) :: iscan

! Local variables
  character*36 obs_type          ! Obs type (1=Surface, 2=Sounding)
  character*16 :: vars ="UVTQ"   ! Variable names array
  character*4  :: varname        ! Variable name

  varname(1:1) = vars(ivar:ivar)

! Set obs type description
  if(ityp .eq. 1) then
     obs_type(1:9) = 'Surf Obs:'
  else
     obs_type(1:9) = 'Sounding:'
  endif

  write(6,'(a9,a1,a1,                                 &
            a3,i2, a3,i2, a3,i2,                      &
            a7,i1,                                    &
            a8,f8.3,                                  &
            a8,f5.3,                                  &
            a6,f7.4,                                  &
            a7,f4.2,                                  &
            a8,f7.3,                                  &
            a4,f6.3)')                                &
      obs_type(1:9),' ',varname(1:1),                 &
      ' i=',i,' j=',j,' k=',k,                        &
      ' ISCAN=',iscan,                                &
      ' WT2ERR=',wt2err,                              &
      ' TIMEWT=',timewt,                              &
      ' WTIJ=',wtij,                                  &
      ' WTSIG=',wtsig,                                &
      ' ERFIVR=',erfivr,                              &
      ' WT=',wt

  RETURN
  END SUBROUTINE nudob_diagnostic

  SUBROUTINE date_string(year, month, day, hour, minute, second, cdate)
!-----------------------------------------------------------------------
!  PURPOSE: Form a date string (YYYY-MM-DD_hh:mm:ss) from integer
!           components.
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
    
  INTEGER, INTENT(IN)  :: year
  INTEGER, INTENT(IN)  :: month
  INTEGER, INTENT(IN)  :: day
  INTEGER, INTENT(IN)  :: hour
  INTEGER, INTENT(IN)  :: minute
  INTEGER, INTENT(IN)  :: second
  CHARACTER*19, INTENT(INOUT) :: cdate

! Local variables
  integer   :: ic                    ! loop counter

      cdate(1:19)  = "0000-00-00_00:00:00"
      write(cdate( 1: 4),'(i4)') year
      write(cdate( 6: 7),'(i2)') month
      write(cdate( 9:10),'(i2)') day
      write(cdate(12:13),'(i2)') hour
      write(cdate(15:16),'(i2)') minute
      write(cdate(18:19),'(i2)') second
      do ic = 1,19
        if(cdate(ic:ic) .eq. " ") cdate(ic:ic) = "0"
      enddo

  RETURN
  END SUBROUTINE date_string 

  SUBROUTINE calc_rcouple_scales(a, msf, rscale, ims,ime, its,ite)
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ims,ime           ! Memory dimensions
  INTEGER, INTENT(IN)  :: its,ite           ! Tile   dimensions
  REAL, INTENT(IN)     :: a( ims:ime )      ! Air mass array 
  REAL, INTENT(IN)     :: msf( ims:ime )    ! Map scale factor array
  REAL, INTENT(OUT)    :: rscale( ims:ime ) ! Scales for rho-coupling

! Local variables
  integer :: i

! Calculate scales to be used for producing rho-coupled nudging factors.
  do i = its,ite
    rscale(i) = a(i)/msf(i)
  enddo

  RETURN
  END SUBROUTINE calc_rcouple_scales

!ajb: Not used
  SUBROUTINE set_real_array(rscale, value, ims,ime, its,ite)
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ims,ime           ! Memory dimensions
  INTEGER, INTENT(IN)  :: its,ite           ! Tile   dimensions
  REAL, INTENT(IN)     :: value             ! Constant array value
  REAL, INTENT(OUT)    :: rscale( ims:ime ) ! Output array

! Local variables
  integer :: i

! Set array to constant value
  do i = its,ite
    rscale(i) = value
  enddo

  RETURN
  END SUBROUTINE set_real_array

  SUBROUTINE print_obs_info(iprt,inest,niobf,rio,rjo,rko,prt_max,prt_freq,  &
                            obs,stnid,lat,lon,mlat,mlon,qcdom,timeob,xtime, &
                            kalmax,ob_is_kalman,platform_ob,ob_var_type)

  USE module_fddaobs_utilities,ONLY: NUMNDGV, VSYMBOL
!*************************************************************************
! Purpose: Print obs information.
!*************************************************************************

  IMPLICIT NONE

  LOGICAL, intent(in)    :: iprt          ! Print flag
  INTEGER, intent(in)    :: inest         ! Nest level
  INTEGER, intent(in)    :: niobf         ! Maximum number of observations
  REAL,    intent(in)    :: rio(niobf)    ! West-east coord (non-stagger)
  REAL,    intent(in)    :: rjo(niobf)    ! South-north coord (non-stagger)
  REAL,    intent(in)    :: rko(niobf)    ! Bottom-top north coord (non-stagger)
  INTEGER, intent(in)    :: prt_max        ! Max no. of obs for diagnostic printout
  INTEGER, intent(in)    :: prt_freq       ! Frequency for diagnostic printout
  INTEGER, intent(in)    :: obs(prt_max)  ! Saved obs indices to print
  INTEGER, intent(in)    :: stnid(40,prt_max) ! Saved station ids (encoded)
  REAL,    intent(in)    :: lat(prt_max)  ! Saved latitudes
  REAL,    intent(in)    :: lon(prt_max)  ! Saved longitudes
  REAL,    intent(in)    :: mlat(prt_max) ! Saved model latitudes
  REAL,    intent(in)    :: mlon(prt_max) ! Saved longitudes
  INTEGER, intent(in)    :: qcdom(prt_max) ! Saved qcdomprt
  REAL,    intent(in)    :: timeob(niobf) ! Time of each observation (hours)
  REAL,    intent(in)    :: xtime         ! Model time in minutes
  INTEGER, intent(in)    :: kalmax        ! Max number of kalman-type obs
  INTEGER, intent(in)    :: ob_is_kalman(niobf)    ! flags designating kalman obs
  INTEGER, intent(in)    :: platform_ob (40,niobf) ! platform type (encoded)
  INTEGER, intent(in)    :: ob_var_type(niobf) ! Variable type of each ob 

! Local variables
  integer :: i                    ! Loop counter over obs station chars
  integer :: n                    ! Loop counter over obs
  integer :: pnx                  ! Obs index for printout
  character(len=200) :: msg       ! Argument to wrf_message
  character(len=10)  :: station_id ! Station id of observation
  character(len=10)  :: platform   ! Platform type
  character*8        :: ptxt      ! String to hold ob-type 
  character*2        :: qcstr     ! String to hold qc domain
  character*2        :: vtype     ! String to hold ob variable type

  if(iprt) then
    if(prt_max.gt.0) then

      if(obs(1).ne.-999) then

        call wrf_message("")
        write(msg,'(a,i4,a,f8.1,a)') 'REPORTING OBS MASS-PT LOCS FOR NEST ',  &
                                     inest,' AT XTIME=',xtime,' MINUTES'
        call wrf_message(msg)

        write(msg,'(a,i4,a,i5,a)') 'FREQ=',prt_freq,', MAX=',prt_max,         &
                           ' LOCS, NEWLY READ OBS ONLY, -999 => OBS OFF PROC'
        call wrf_message(msg)
        call wrf_message("")

        write(msg,'(3a)') '    OBS#     I       J       K     OB LAT',        &
                          '   OB LON    XLAT(I,J)  XLONG(I,J)  TIME(hr)',      &
                          ' STN ID      PLATFORM    QC  TYPE'
        call wrf_message(msg)

      endif
    endif

! Note: rio and rjo are referenced to non-staggered grid (not mass-point!)
!       Hence subtract .5 from each to get mass-point coords.
    do n=1,prt_max
      pnx = obs(n)

      if(kalmax.eq.0) then
        ptxt = "NoKalman"
      elseif(ABS(ob_is_kalman(pnx)).eq.1) then
        ptxt = "-Kalman-"
      else
        ptxt = "        "
      endif 

      if(pnx.ne.-999) then

!       Retrieve variable type
        vtype(1:2) = "??"
        do i = 1, NUMNDGV
          if(ob_var_type(pnx).eq.i) vtype(1:2) = VSYMBOL(i)(1:2)
        enddo

!       Retrieve 10 chars of station id
        do i = 1,10
          station_id(i:i) = char(stnid(i,n))
        enddo
!       Retrieve 10 chars of platform type
        DO i = 1,10
          platform (i:i) = char(platform_ob(6+i,n)) ! obs platform
        ENDDO
!       Set qc string
        qcstr(1:1) = 'd'
        write(qcstr(2:2),'(i1)') qcdom(n)

!       Write line for ob n to diagnostic report
        write(msg,1001) pnx,rio(pnx)-.5,rjo(pnx)-.5,rko(pnx),lat(n),lon(n), &
            mlat(n),mlon(n),timeob(pnx),station_id,platform,qcstr,vtype,ptxt
        call wrf_message(msg)
      endif
    enddo
    if(obs(1).ne.-999) call wrf_message("")
  endif
  1001 Format(2x,i7,3f8.3,2f9.3,2x,f9.3,2x,f9.3,3x,f6.2,4x,2(a10,2x),a2,3x,a2,1x,a8)
  END SUBROUTINE print_obs_info

  REAL FUNCTION get_timewu(qc_internal,xtime,dtmin,twindo,scalef,obtime)
!*************************************************************************
! Purpose: Compute the temporal weight factor for an observation
!*************************************************************************

  IMPLICIT NONE

  REAL, INTENT(IN)  :: xtime              ! model time (minutes)
  REAL, INTENT(IN)  :: dtmin              ! model timestep (minutes)
  REAL, INTENT(IN)  :: twindo             ! half window (hours)
  REAL, INTENT(IN)  :: scalef             ! window scale factor
  REAL, INTENT(IN)  :: obtime             ! observation time (hours)
    
! Local variables
  real :: fdtim            ! reference time (minutes)
  real :: tw1              ! half of twindo, scaled, in minutes
  real :: tw2              ! twindo, scaled, in minutes
  real :: tconst           ! reciprical of tw1
  real :: ttim             ! obtime in minutes
  real :: dift             ! | fdtim-ttim |
  real :: timewt           ! returned weight
  real,parameter:: wuamp=3.0 * 2.56      !64/25=2.56
  real::TW4
  LOGICAL, INTENT(IN) :: qc_internal

!! wuyh added 20110902
! DETERMINE THE TIME-WEIGHT FACTOR FOR N
      FDTIM=XTIME-DTMIN  !model time in minutes
      TW1=TWINDO*30.     ! TWINDO IS IN MINUTES
      TW2=TWINDO*60.
!      TW1=TWINDO*30.*scalef
!      TW2=TWINDO*60.*scalef
      TCONST=float(1)/TW1
      TIMEWT=float(0) 
      TTIM=obtime*float(60) !obs in minutes
!***********TTIM=TARGET TIME IN MINUTES

    if(qc_internal)then
      TW4=TW1/float(2)
      DIFT=(FDTIM-TTIM)
!      IF(DIFT.GT.-TW2.AND.DIFT.LE.TW1) THEN
!        TIMEWT=-wuamp*16./(9.*TW2*TW2)*(DIFT+TW2)*(DIFT-TW1)
      IF(DIFT.GT.-TW2.AND.DIFT.LE.TW4) THEN
        TIMEWT=wuamp/(TW2*TW2)*(DIFT+TW2)*(TW4-DIFT)
      ELSE
        TIMEWT=float(0)
      ENDIF
    else
      DIFT=ABS(FDTIM-TTIM)
      IF(DIFT.LE.TW1)TIMEWT=1.0
      IF(DIFT.GT.TW1.AND.DIFT.LE.TW2) THEN
        IF(FDTIM.LT.TTIM)TIMEWT=(FDTIM-(TTIM-TW2))*TCONST
        IF(FDTIM.GT.TTIM)TIMEWT=((TTIM+TW2)-FDTIM)*TCONST
      ENDIF
    endif
!! wuyh added 20110902 end
      get_timewu = timewt
  END FUNCTION get_timewu

  SUBROUTINE print_obs_per_grid(iprt, inest, xtime, dtmin, twindo_plt, nstat,  &
                                niobf, plfo, ra, rb, timeob, lev_in_ob,        &
                                ide, jde)

!------------------------------------------------------------------------------
!  PURPOSE: Print out number of obs per grid for the various platforms
!------------------------------------------------------------------------------

  IMPLICIT NONE

  LOGICAL, intent(in) :: iprt              ! Print flag
  INTEGER, intent(in) :: inest             ! Nest level
  REAL,    intent(in) :: xtime             ! Model time in minutes
  REAL,    intent(in) :: dtmin             ! Model time step in minutes
  REAL,    intent(in) :: twindo_plt(15)    ! Half-period time window for nudging
  INTEGER, intent(in) :: nstat             ! Number of obs stations
  INTEGER, intent(in) :: niobf             ! Maximum number of observations
  REAL,    intent(in) :: plfo(niobf)       ! Obs platform type
  REAL,    intent(in) :: ra(niobf)         ! I-coord of obs
  REAL,    intent(in) :: rb(niobf)         ! J-coord of obs
  REAL,    intent(in) :: timeob(niobf)     ! Time of each observation (hours)
  REAL,    intent(in) :: lev_in_ob(niobf)  ! Level in sounding-type obs.
  INTEGER, intent(in) :: ide               ! Ending i coord for domain
  INTEGER, intent(in) :: jde               ! Ending j coord for domain

! Local variables 
  integer :: nsthis,nsmetar,nsspeci,nsship
  integer :: nssynop,nstemp,nspilot,nssatwnds,nssams,nsprofs
  integer :: jjjn
  real :: fdtim,tw1,tw2,tconst,timewt,timewt2,ttim,dift
  integer :: iplat
  real :: twindo
  character*3 chr_hr   

    IF (iprt) print *,'outputting obs number on grid ',    &
                 inest,' at time=',xtime
    write(chr_hr(1:3),'(i3)')nint(xtime/60.)
    if(chr_hr(1:1).eq.' ')chr_hr(1:1)='0'
    if(chr_hr(2:2).eq.' ')chr_hr(2:2)='0'
    IF (iprt) print *,'chr_hr=',chr_hr(1:3),nint(xtime/60.)
    open(91,file=                                             &
        'obs_g'//char(inest+ichar('0'))//'_'//chr_hr(1:3),    &
        form='FORMATted',status='unknown')
    write(91,911)nstat
    write(6,911)nstat
911 FORMAT('total obs=',i8)
    nsthis=0
    nsmetar=0
    nsspeci=0
    nsship=0
    nssynop=0
    nstemp=0
    nspilot=0
    nssatwnds=0
    nssams=0
    nsprofs=0
!   print *,'ide,jde=',ide,jde
    do jjjn=1,nstat
! DETERMINE THE TIME-WEIGHT FACTOR FOR N
      FDTIM=XTIME-DTMIN

!if( mod(jjjn,100) .eq. 1) then
!       write(6,*) 'jjjn = ',jjjn,' plfo(jjjn) = ',plfo(jjjn)
!endif

      iplat = nint(plfo(jjjn))
      twindo = twindo_plt(iplat)



! CONVERT TWINDO AND TIMEOB FROM HOURS TO MINUTES:
      TW1=TWINDO/2.*60.
      TW2=TWINDO*60.
      TCONST=1./TW1
      TIMEWT2=0.0
      TTIM=TIMEOB(jjjn)*60.
!***********TTIM=TARGET TIME IN MINUTES
      DIFT=ABS(FDTIM-TTIM)
      IF(DIFT.LE.TW1)TIMEWT2=1.0

      IF(DIFT.GT.TW1.AND.DIFT.LE.TW2) THEN
        IF(FDTIM.LT.TTIM)TIMEWT2=(FDTIM-(TTIM-TW2))*TCONST
        IF(FDTIM.GT.TTIM)TIMEWT2=((TTIM+TW2)-FDTIM)*TCONST
      ENDIF


!   if( mod(jjjn,100) .eq. 1) then
!     write(6,'a,i5,3(a,f8.2)') 'N = ',jjjn,                         &
!                '  ttim = ',ttim,' fdtim = ',fdtim,' timewt2 = ',timewt2
!   endif

!     print *,'timewt2=',timewt2,ttim,fdtim
      if (ra(jjjn).ge.1. .and. rb(jjjn).ge.1.                    &
      .and.ra(jjjn).le.real(ide) .and. rb(jjjn).le.real(jde)     &
      .and.timewt2.gt.0.) then
        if(lev_in_ob(jjjn).eq.1.)nsthis=nsthis+1
        if(plfo(jjjn).eq.1.)nsmetar=nsmetar+1
        if(plfo(jjjn).eq.2.)nsspeci=nsspeci+1
        if(plfo(jjjn).eq.3.)nsship=nsship+1
        if(plfo(jjjn).eq.4.)nssynop=nssynop+1
        if(plfo(jjjn).eq.5..and.lev_in_ob(jjjn).eq.1.) nstemp=nstemp+1
        if(plfo(jjjn).eq.6..and.lev_in_ob(jjjn).eq.1.) nspilot=nspilot+1
        if(plfo(jjjn).eq.7.)nssatwnds=nssatwnds+1
        if(plfo(jjjn).eq.8.)nssams=nssams+1
        if(plfo(jjjn).eq.9..and.lev_in_ob(jjjn).eq.1.) nsprofs=nsprofs+1
      endif
    enddo
    write(91,912)nsthis
    write(6,912)nsthis
912 FORMAT('total obs on this grid=',i8)
    write(91,921)nsmetar
    write(6,921)nsmetar
921 FORMAT('total metar obs on this grid=',i8)
    write(91,922)nsspeci
    write(6,922)nsspeci
922 FORMAT('total special obs on this grid=',i8)
    write(91,923)nsship
    write(6,923)nsship
923 FORMAT('total ship obs on this grid=',i8)
    write(91,924)nssynop
    write(6,924)nssynop
924 FORMAT('total synop obs on this grid=',i8)
    write(91,925)nstemp
    write(6,925)nstemp
925 FORMAT('total temp obs on this grid=',i8)
    write(91,926)nspilot
    write(6,926)nspilot
926 FORMAT('total pilot obs on this grid=',i8)
    write(91,927)nssatwnds
    write(6,927)nssatwnds
927 FORMAT('total sat-wind obs on this grid=',i8)
    write(91,928)nssams
    write(6,928)nssams
928 FORMAT('total sams obs on this grid=',i8)
    write(91,929)nsprofs
    write(6,929)nsprofs
929 FORMAT('total profiler obs on this grid=',i8)
    close(91)
END SUBROUTINE print_obs_per_grid

      Subroutine interpwu(mkz,pob,pmod,kmod,DZmod)

      Implicit None

! input
      Integer::mkz   !vertical levels
      Real::pob      !obs pressure
      Real,Dimension(mkz)::pmod
!output
      Integer,intent(out)::kmod
      Real,intent(out)::DZmod

! intermidiate
     Integer:: k,kbot

      If(pob .gt.0.)then

       if(pob .GE. pmod(1))then
         kmod=2
         DZmod=1.
       elseif(pob.LE.pmod(mkz))then
         kmod=mkz
         DZmod=0.
       else
        do k=2,mkz
          kbot = k
          if(pob .ge. pmod(k)) then
            goto 198
          endif
        enddo
 198    continue
        DZmod=ALOG(pob/pmod(kbot))/ALOG(pmod(kbot-1)/pmod(kbot))
        kmod=k
       endif
      Else
        DZmod=-888888.
        kmod=-99
      Endif

      Return

      End Subroutine interpwu

  SUBROUTINE set_platform_val( inest, maxdom, x_plt, x, xs,  &
                               x_sl01, x_sl02, x_sl03,       &
                               x_sl04, x_sl05, x_sl06,       &
                               x_sl07, x_sl08, x_sl09,       &
                               x_sl10, x_sl11, x_sl12,       &
                               x_sl13, x_sl14, x_sl15)

!-------------------------------------------------------------------------------
!  PURPOSE: Set platform value array for each platform type, from the platform-
!           specific input (namelist) values. If the platform-specific value is 
!           zero, then the generic-platform input (namelist) value is used.
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: inest     ! nest level
  INTEGER, INTENT(IN)  :: maxdom    ! maximum number of domains
  REAL,    INTENT(OUT) :: x_plt(99) ! values to be for each platform, this nest (km)
  REAL,    INTENT(IN)  :: x(maxdom)       ! input value for generic platform    (km)
  REAL,    INTENT(IN)  :: xs(maxdom)      ! input value for generic platform    (km)
  REAL,    INTENT(IN)  :: x_sl01(maxdom)  ! input value for platfm type metar   (km)
  REAL,    INTENT(IN)  :: x_sl02(maxdom)  ! input value for platfm type speci   (km)
  REAL,    INTENT(IN)  :: x_sl03(maxdom)  ! input value for platfm type ship    (km)
  REAL,    INTENT(IN)  :: x_sl04(maxdom)  ! input value for platfm type synop   (km)
  REAL,    INTENT(IN)  :: x_sl05(maxdom)  ! input value for platfm type temp    (km)
  REAL,    INTENT(IN)  :: x_sl06(maxdom)  ! input value for platfm type pilot   (km)
  REAL,    INTENT(IN)  :: x_sl07(maxdom)  ! input value for platfm type satwnds (km)
  REAL,    INTENT(IN)  :: x_sl08(maxdom)  ! input value for platfm type sams    (km)
  REAL,    INTENT(IN)  :: x_sl09(maxdom)  ! input value for platfm type profiler(km)
  REAL,    INTENT(IN)  :: x_sl10(maxdom)  ! input value for platfm type slot 10 (km)
  REAL,    INTENT(IN)  :: x_sl11(maxdom)  ! input value for platfm type slot 11 (km)
  REAL,    INTENT(IN)  :: x_sl12(maxdom)  ! input value for platfm type slot 12 (km)
  REAL,    INTENT(IN)  :: x_sl13(maxdom)  ! input value for platfm type slot 13 (km)
  REAL,    INTENT(IN)  :: x_sl14(maxdom)  ! input value for platfm type slot 14 (km)
  REAL,    INTENT(IN)  :: x_sl15(maxdom)  ! input value for platfm type slot 15 (km)

! Local variables 
  integer, parameter :: META=1,SPEC=2,SHIP=3,SYNO=4,TEMP=5,PILO=6,SATW=7,SAMS=8,PROF=9, &
       SL10=10,SL11=11,SL12=12,SL13=13,SL14=14,SL15=15,SL16=16,SL17=17,SL18=18,SL19=19, &
       SL20=20,SL21=21,SL22=22,SL23=23,SL24=24,SL25=25,SL26=26,SL27=27,SL28=28,SL29=29, &
       SL30=30,SL31=31,SL32=32,SL33=33,SL34=34,SL35=35,SL36=36,SL37=37,SL38=38,SL39=39, &
       SL40=40,SL41=41,SL42=42,SL43=43,SL44=44,SL45=45,SL46=46,SL47=47,SL48=48,SL49=49, &
       SL50=50,SL51=51,SL52=52,SL53=53,SL54=54,SL55=55,SL56=56,SL57=57,SL58=58,SL59=59, &
       SL60=60,SL61=61,SL62=62,SL63=63,SL64=64,SL65=65,SL66=66,SL67=67,SL68=68,SL69=69, &
       SL70=70,SL71=71,SL72=72,SL73=73,SL74=74,SL75=75,SL76=76,SL77=77,SL78=78,SL79=79, &
       SL80=80,SL81=81,SL82=82,SL83=83,SL84=84,SL85=85,SL86=86,SL87=87,SL88=88,SL89=89, &
       SL90=90,SL91=91,SL92=92,SL93=93,SL94=94,SL95=95,SL96=96,SL97=97,SL98=98,SL99=99

  integer :: iplat

! Set platform array values for this nest from inputs.
  x_plt(META) = x_sl01(inest)
  x_plt(SPEC) = x_sl02(inest)
  x_plt(SHIP) = x_sl03(inest)
  x_plt(SYNO) = x_sl04(inest)
  x_plt(TEMP) = x_sl05(inest)
  x_plt(PILO) = x_sl06(inest)
  x_plt(SATW) = x_sl07(inest)
  x_plt(SAMS) = x_sl08(inest)
  x_plt(PROF) = x_sl09(inest)
  x_plt(SL10) = x_sl10(inest)
  x_plt(SL11) = x_sl11(inest)
  x_plt(SL12) = x_sl12(inest)
  x_plt(SL13) = x_sl13(inest)
  x_plt(SL14) = x_sl14(inest)
  x_plt(SL15) = x_sl15(inest)

! If platform-specific input is zero (which indicates it was not provided in
! the namelist), use the generic platform value.
  do iplat = 1, 15
! for surface platforms
    if (iplat.ne.5 .and. iplat.ne.6 .and. iplat.ne.7 .and. iplat.ne.9) then
      if(x_plt(iplat) .eq. 0.) x_plt(iplat) = xs(inest)
    endif
! for all platforms
    if(x_plt(iplat) .eq. 0.) x_plt(iplat) = x(inest)
  enddo

! Use generic platform value for unknown platform type.
  x_plt(99) = x(inest)
  x_plt(98) = xs(inest)

  END SUBROUTINE set_platform_val

  SUBROUTINE write_rinxy_info( inest, dx, r_plt )

!-------------------------------------------------------------------------------
!  PURPOSE: Write a table of rinxy values for each platform type. 
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: inest         ! nest level
  REAL,    INTENT(IN)  :: dx            ! grid size (m)
  REAL,    INTENT(IN)  :: r_plt(15)     ! rinxy for each platform (km)

! Local variables
  character(len=200) :: msg       ! Argument to wrf_message
  character(len=8)   :: plat(9)   ! Platform types
  integer :: i
  data plat/'METAR','SPECI','SHIP','SYNOP','TEMP','PILOT','SATWNDS','SAMS','PROFILER'/

  call wrf_message("")
  write(msg,'(a,i2)') 'RINXY FOR EACH PLATFORM TYPE ON DOMAIN ', inest
  call wrf_message(msg)
  call wrf_message("PLATFORM#    PLATFORM TYPE   VALUE IN GRID UNITS")

  do i = 1, 9
    write(msg,'(3x,i3,10x,a8,8x,f5.1)') i, plat(i)(1:8), 1000.*r_plt(i)/dx
    call wrf_message(msg)
  enddo

! write(msg,'(3x,a19,8x,f5.1)') ' 2            SPECI   ', 1000.*r_plt(2)/dx
! call wrf_message(msg)
! write(msg,'(3x,a19,8x,f5.1)') ' 3            SHIP    ', 1000.*r_plt(3)/dx
! call wrf_message(msg)
! write(msg,'(3x,a19,8x,f5.1)') ' 4            SYNOP   ', 1000.*r_plt(4)/dx
! call wrf_message(msg)
! write(msg,'(3x,a19,8x,f5.1)') ' 5            TEMP    ', 1000.*r_plt(5)/dx
! call wrf_message(msg)
! write(msg,'(3x,a19,8x,f5.1)') ' 6            PILOT   ', 1000.*r_plt(6)/dx
! call wrf_message(msg)
! write(msg,'(3x,a19,8x,f5.1)') ' 7            SATWNDS ', 1000.*r_plt(7)/dx
! call wrf_message(msg)
! write(msg,'(3x,a19,8x,f5.1)') ' 8            SAMS    ', 1000.*r_plt(8)/dx
! call wrf_message(msg)
! write(msg,'(3x,a19,8x,f5.1)') ' 9            PROFILER', 1000.*r_plt(9)/dx
! call wrf_message(msg)

  END SUBROUTINE write_rinxy_info

  SUBROUTINE wucone(rad_per_deg,truelat1, truelat2, cone)

   ! Subroutine to compute the cone factor of a Lambert Conformal projection

      IMPLICIT NONE

      ! Input Args
      REAL, INTENT(IN)             :: truelat1  ! (-90 -> 90 degrees N)
      REAL, INTENT(IN)             :: truelat2  !   "   "  "   "     "

      ! Output Args
      REAL, INTENT(OUT)            :: cone
      Real:: rad_per_deg
      ! Locals

      ! BEGIN CODE

      ! First, see if this is a secant or tangent projection.  For tangent
      ! projections, truelat1 = truelat2 and the cone is tangent to the 
      ! Earth's surface at this latitude.  For secant projections, the cone
      ! intersects the Earth's surface at each of the distinctly different
      ! latitudes

      IF (ABS(truelat1-truelat2) .GT. 0.1) THEN
       cone=ALOG10(COS(truelat1*rad_per_deg))-ALOG10(COS(truelat2*rad_per_deg))
       cone=cone/(ALOG10(TAN((45.0-ABS(truelat1)/2.0)*rad_per_deg))    &
            -     ALOG10(TAN((45.0-ABS(truelat2)/2.0)*rad_per_deg)))
      ELSE
         cone = SIN(ABS(truelat1)*rad_per_deg )
      ENDIF

      RETURN

  END SUBROUTINE wucone

!!!*************************************************************
      REAL Function trunca(r,RIS)
!!!*************************************************************
!     Localization through the Cressman formula truncation

      Implicit None

      Real,Intent(IN) ::RIS     !square of influence radius
      Real,Intent(IN) ::r       !square of distance
!     Real,Intent(OUT)::trunca
      Real            ::Rno     ![0,Rno] no truncation

      Rno=RIS/float(4)               ! original half radius of influence        
!      Rno=RIS/1.5                   ! original half radius of influence

      IF(r.LT.Rno)Then
       trunca=float(1)
      Elseif(r.LT.RIS)then
       trunca=(RIS-r)/(RIS+r-2.*Rno)
      Else
       trunca=float(0)
      Endif
      Return
      End function trunca
 
!!! Wuyh added end 20100723

!!! Wuyh 20100805 added
!!!*************************************************************
      REAL Function cohn(r)
!!!*************************************************************
!     Localization through the Gaspri and Cohn formula truncation

      Implicit None

      Real,Intent(IN)::R       !distance
!     Real,Intent(OUT)::cohn

      IF(R.LT.1.)Then
       cohn=1.-(r**3/4.-r**2/2.-5.*r/8.-5./3.)*r*r
      Elseif((1.LE.R).AND.(R.LE.2.))then
       cohn=(r**3/12.-r**2/2.+5.*r/8.+5./3.)*r*r-5.*r+4.-2./(3.*r)       
      Else
       cohn=float(0)
      Endif
      Return
      End function cohn
 
!!! Wuyh added end 20100805

!!! Wuyh added 20100830
!!!********************************************************************
      Subroutine Jiaquan(mwt,pnflag,TIMEWT,WTSIG,WTIJ,ERFIVR,WT,WT2ERR)
!!!********************************************************************
! Purpose: determine how to weight the innovation by flag mwt 

      Implicit None
      Integer,Intent(In)::mwt
      Integer,Intent(In)::pnflag  !!whether use negative gain (0: no use)
      Real, Intent(In) :: WTSIG,ERFIVR,TIMEWT
      Real, Intent(InOut)::WTIJ
      Real,Intent(out)::WT,WT2ERR

      WT=float(0)
      WT2ERR=float(0) 

      if(pnflag.EQ.0)then
         WTIJ=MAX(float(0),WTIJ)
      endif
       
      IF(mwt.EQ.1)then  !similar to Stauffer and Seaman
           WT=WT+TIMEWT*WTSIG*ABS(WTIJ)
           WT2ERR=WT2ERR+TIMEWT*TIMEWT*               &
                ABS(WTIJ)*WTIJ*WTSIG*WTSIG*ERFIVR
      Elseif(mwt.EQ.2)then 
         WT=WT+TIMEWT*WTSIG
         WT2ERR=WT2ERR+TIMEWT*TIMEWT*               &
                WTIJ*WTSIG*WTSIG*ERFIVR
      Else
         print*,'mwt=',mwt,' is wrong'
         stop 931
      Endif

      Return
      End Subroutine Jiaquan
!!! Wuyh added 20100830 end

!!! Wuyh added 20100830
!!!***********************************************************************
  Function opolyval(ndim,ntt,x,y,gainx)
!!!***********************************************************************
!!! obtain gain from gain parameters gainx

  implicit none
  integer, intent(in)::ndim  !dimension of poly
  integer, intent(in)::ntt   !ntt=ndim*(ndim+1)
  Real,intent(in)::x,y       !grid coord to the center 
  Real,Dimension(ntt),intent(in)::gainx   !gain parameter
  real:: opolyval
  Integer::n1,n2,k

  IF(abs(x)+abs(y).LT.3.0E-2)then
    opolyval=gainx(1)
  Else
   opolyval=float(0)
   k=0
   Do n1=0,ndim
   Do n2=0,n1
    k=k+1
    opolyval=opolyval+gainx(k)*((x**(n1-n2))*(y**n2))
    write(6,*) n1,n2,gainx(k)
   Enddo
   Enddo
  Endif
  Return
  End function opolyval
!!! Wuyh added 20101110 end


!!!***********************************************************************
  REAL Function polyval(ndim,ndim2,ntt,rgainjp,xo,yo,gainx)
!!!***********************************************************************
!!! polyval(ndir,ndir2,ndirpow,rgainjp,IRY,IRX,weighta)
!!! obtain gain from gain parameters gainx

  Implicit none
  integer, intent(in)::ndim               !dimension of gain in x or y dir
  integer, intent(in)::ndim2              !2*ndim+1 
  integer, intent(in)::ntt                !ntt=(2*ndim+1)**2
  Real, intent(in)::rgainjp               !gain jump every ngainjp points
  Real,intent(in)::xo,yo                  !(I-Ry(N),J-RB(N) grid-obs grids 
  Real,Dimension(-ndim:ndim,-ndim:ndim),intent(in)::gainx   !gain parameter
! intermediate
!                                                         nnw ---- nne
!  Integer::nsw,nse,nnw,nne   !number of 4 points in gain   |        |
!                                                         nsw ---- nse
  Integer::io,jo
  Real::wxo,wyo

! -------------------------------------------------------------
!  write(299,*)"rgainjp=",rgainjp

  io=FLOOR(xo/rgainjp)
  jo=FLOOR(yo/rgainjp)

  wxo = xo/rgainjp - float(io)
  wyo = yo/rgainjp - float(jo)

  IF(abs(io) .LT. ndim .AND. abs(jo) .LT. ndim )then
    polyval=(1.-wyo)*((1.-wxo)*gainx(io,jo  )+wxo*gainx(io+1,jo  ))   &
           +    wyo *((1.-wxo)*gainx(io,jo+1)+wxo*gainx(io+1,jo+1))
!    write(271,*)"wxo,wyo=",wxo,wyo
  Else
   polyval=float(0)
  Endif

  Return
  End function polyval
!!! Wuyh added 20120307 end

!!!***********************************************************************
  Function polyvaloo(ndim,ntt,ngainjp,xo,yo,gainx)
!!!***********************************************************************
!!! obtain gain from gain parameters gainx

  Implicit none
  integer, intent(in)::ndim     !dimension o
  integer, intent(in)::ngainjp  !gain jump every ngainjp points
  integer,intent(in)::ntt                 !ntt=(2*ndim+1)**2
  Real,intent(in)::xo,yo                  !(I-Ry(N),J-RB(N) grid-obs grids 
  Real,Dimension(ntt),intent(in)::gainx   !gain parameter
  real:: polyvaloo
! intermidiate
!                                                         nnw ---- nne
  Integer::nsw,nse,nnw,nne   !number of 4 points in gain   |        |
!                                                         nsw ---- nse
  integer::ndim2
  Integer::io,jo
  Real::wxo,wyo,rgainjp

! --------------------------------------------------------------
  ndim2 = 2*ndim + 1
  rgainjp = float(ngainjp)

  io=FLOOR(xo/rgainjp)
  jo=FLOOR(yo/rgainjp)

  wxo = xo/rgainjp - float(io)
  wyo = yo/rgainjp - float(jo)

! get the four neearby point

  nsw = (jo+ndim)*ndim2 +(io+ndim) + 1
  nse = nsw + 1
  nnw = nsw + ndim2
  nne = nnw + 1

  IF(abs(io) .LT. ndim .AND. abs(jo) .LT. ndim )then
    polyvaloo=(1.-wyo)*((1.-wxo)*gainx(nsw)+wxo*gainx(nse))   &
           +    wyo *((1.-wxo)*gainx(nnw)+wxo*gainx(nne))
  Else
   polyvaloo=float(0)
  Endif

  Return
  End function polyvaloo
!!! Wuyh added 20120307 end

  SUBROUTINE gross_error_chk(inest, nsta, vdim, nndgv, nerrf, niobf,     &
             ob_var_type, vobs, vobs_qc, errf, varmodel,                 &
             flagsnd, platform_ob, oblat, oblon, timeob)

!------------------------------------------------------------------------------
!  PURPOSE: Gross error check for temperature. Set all vars bad.
!------------------------------------------------------------------------------
  USE module_fddaobs_utilities

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: inest          ! nested domain id
  INTEGER, INTENT(IN) :: nsta           ! # stations held for use
  INTEGER, INTENT(IN) :: vdim           ! Dim for arrays vobs, vobs_qc
  INTEGER, INTENT(IN) :: nndgv          ! Number of nudge variables
  INTEGER, INTENT(IN) :: nerrf          ! Number of error fields
  INTEGER, INTENT(IN) :: niobf          ! Number of observations
  INTEGER, INTENT(IN) :: ob_var_type(niobf) ! Variable type of each ob 
  REAL,    INTENT(INOUT) :: vobs(vdim, niobf)       ! Observational variables
  REAL,    INTENT(INOUT) :: vobs_qc(vdim, niobf)    ! Obs var quality control
  REAL,    INTENT(INOUT) :: errf(nerrf, niobf)         ! Model innovation
  REAL,    INTENT(INOUT) :: varmodel(niobf)      ! U model state at obs loc
!!  REAL,    INTENT(INOUT) :: vvmodel(niobf)      ! V model state at obs loc
!!  REAL,    INTENT(INOUT) :: tmodel(niobf)       ! T model state at obs loc
!!! wuyh added 20130819 
!!  REAL,    INTENT(INOUT) :: rhmodel(niobf)      ! rh model state at obs loc
  logical,intent(in) :: flagsnd(niobf)
  INTEGER, INTENT(IN) :: platform_ob (40,niobf) ! Observation platform type
  REAL,Dimension(niobf),INTENT(IN)::oblat,oblon,timeob
  INTEGER,Dimension(5):: itamdar
  Character,Dimension(5):: Tamdar
  Character*16::platform
!!! wuyh added 20130819 end

! Local variables
  integer  :: n,i     ! loop counter over obs

!!! wuyh added 20130819 
  Real,Parameter::errcrt = 10., errcru=18., errcrq = 0.05
  Integer::jnest,iwu
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor  !externals

  jnest = inest*10 + 700
  Tamdar= "FM-97"
  do i = 1, 5
    itamdar(i)=ICHAR(Tamdar(i))
  enddo
!!! wuyh added 20130819  end

  do n=1,nsta
    if( ob_var_type(n).eq.T ) then
      if( vobs(1,n).GT.600. .OR. vobs(1,n).LT.100.)then
        vobs(1,n)=missing_r  !wuyh comment out
        vobs_qc(1,n)=-13.5
      endif

      if(abs(errf(1,n)).gt. errcrt .and. errf(1,n).gt.-400.)then
         If( wrf_dm_on_monitor() ) THEN
            Do iwu =1,16
             platform(iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platform
            Enddo
!ajb: suppress this output
!         write(3+jnest,1701)n,timeob(n),flagsnd(n),platform,oblat(n), &
!            oblon(n),vobs(1,n),tmodel(n),vrobs_qc(1,n),errf(1,n)
         Endif
         vobs(1,n)=missing_r  !wuyh comment out
         vobs_qc(1,n)=-13.
         errf(1,n)=missing_r
         If(ALL(platform_ob(1:5,n) .eq. itamdar(1:5))) then
!!! wuyh added based on Yubao's suggestion on TAMDAR data 20131003
           if(vobs_qc(1,n) .gt. -0.01 .and. vobs_qc(1,n).lt.2.0) then
             vobs_qc(1,n)=-15.
           endif
!!! wuyh added based on Yubao's suggestion on TAMDAR data 20131003
           if( wrf_dm_on_monitor() ) THEN
             Do iwu =1,16
               platform(iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platform
             Enddo
!ajb: suppress this output
!          write(5+jnest,1701)n,timeob(n),flagsnd(n),platform,oblat(n),&
!             oblon(n),vobs(1,n),tmodel(n),vobs_qc(1,n),errf(1,n)
           endif
         Endif
      endif
    endif

    if( ob_var_type(n).eq.U ) then
      if(abs(errf(1,n)).gt. errcru .and. errf(1,n) .gt.-100.)then
        If( wrf_dm_on_monitor() ) THEN
          Do iwu =1,16
            platform(iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platform
          Enddo
!ajb: suppress this output
!      write(1+jnest,1701)n,timeob(n),flagsnd(n),platform,oblat(n), &
!           oblon(n),vobs(1,n),uumodel(n),vobs_qc(1,n),errf(1,n)
        Endif
        errf(1,n)=missing_r
        vobs_qc(1,n)=-11.
      endif
    endif

    if( ob_var_type(n).eq.V ) then
      if(abs(errf(1,n)).gt. errcru .and. errf(1,n) .gt.-100.)then
        If( wrf_dm_on_monitor() ) THEN
          Do iwu =1,16
            platform(iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platform
          Enddo
!ajb: suppress this output
!      write(2+jnest,1701)n,timeob(n),flagsnd(n),platform,oblat(n), &
!           oblon(n),vobs(1,n),vvmodel(n),vobs_qc(1,n),errf(1,n)
        Endif
        errf(1,n)=missing_r
        vobs_qc(1,n)=-12.
      endif
    endif

    if( ob_var_type(n).eq.Vr ) then
      if(abs(errf(1,n)).gt. errcru .and. errf(1,n) .gt.-100.)then
!        If( wrf_dm_on_monitor() ) THEN
!          Do iwu =1,16
!            platform(iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platform
!          Enddo
!ajb: suppress this output
!!!          write(1330+inest,1702)n,oblat(n),oblon(n),vobs(1,n),vobs_qc(1,n),errf(1,n)
1702      Format(I8,2(1x,F10.2),3(1x,F12.4))
!        Endif
        errf(1,n)=missing_r
        vobs_qc(1,n)=600100.
      endif
    endif

!!! wuyh modified 20130816
!!! ajb added missing_r check 20141120 for splitting out obs fields
!   if(vobs_qc(1,n).ne.missing_r.and.vobs_qc(1,n).ne.missing_r) then
!     vobs_qc(1,n)=MIN(vobs_qc(1,n),vobs_qc(1,n))
!     vobs_qc(1,n)=vobs_qc(1,n)
!   endif
!!! wuyh modified 20130816

    if( ob_var_type(n).eq.Q ) then
      if(abs(errf(1,n)) .gt. errcrq .and. errf(1,n) .gt.-800.)then
        If( wrf_dm_on_monitor() ) THEN
          Do iwu =1,16
            platform(iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platform
          Enddo
!ajb: suppress this output
!      write(4+jnest,1701)n,timeob(n),flagsnd(n),platform,oblat(n), &
!           oblon(n),vobs(1,n),rhmodel(n),vobs_qc(1,n),errf(1,n)
        Endif
        errf(1,n)=missing_r
        vobs_qc(1,n)=-14.
      endif
    endif
  enddo

 1701 Format(I7,1x,F8.2,1x,L4,1x,A16,2(1x,F7.2),4(1x,F10.2))
  END SUBROUTINE gross_error_chk

  SUBROUTINE init_model2missing(missing_r, niobf, uumodel, vvmodel, tmodel, rhmodel, rhobs)

  IMPLICIT NONE

  REAL,    INTENT(IN)    :: missing_r              ! Initial value for model
  INTEGER, INTENT(IN)    :: niobf                  ! Number of observations
  REAL,    INTENT(INOUT) :: uumodel(niobf)         ! U  model state at obs loc
  REAL,    INTENT(INOUT) :: vvmodel(niobf)         ! V  model state at obs loc
  REAL,    INTENT(INOUT) :: tmodel(niobf)          ! T  model state at obs loc
  REAL,    INTENT(INOUT) :: rhmodel(niobf)         ! model RH state at obs loc
  REAL,    INTENT(INOUT) :: rhobs(niobf)           ! obs Rh state at obs loc

  uumodel = missing_r
  vvmodel = missing_r
  tmodel  = missing_r
  rhmodel = missing_r
  rhobs   = missing_r

  RETURN
  END SUBROUTINE init_model2missing 

 SUBROUTINE write_everything(inest, nsta, nndgv, vdim, niobf, nerrf, opt_hum,                   &
                        xtime, dtmin, timeob,oblat, oblon,                                      &
                        varmodel,desobs,                                                        &
                        stnid, ionf, wudate, platform_ob, wusource, ob_var_type,vobs, vobs_qc,  &
                        errf, nlevs_ob, elevob, flagsnd, flagbog, qcu,                          &
                        map_proj, truelat1, truelat2, cen_lat, cen_lon,Last_Obs_Time)
!------------------------------------------------------------------------------
!  PURPOSE: Write out model state, observations, etc.
!     NOTE: Code written by wuyh, put into subroutine by ajb
!------------------------------------------------------------------------------

! For indexing into array vobs
  USE module_fddaobs_utilities
!!!! wuyh added 20131206
  USE module_dm,ONLY: wrf_dm_sum_real
!!!! wuyh added 20131206

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: inest                  ! nested domain id
  INTEGER, INTENT(IN) :: nsta                   ! Number of stations held for use
  INTEGER, INTENT(IN) :: nndgv                  ! Number of nudge variables
  INTEGER, INTENT(IN) :: vdim                   ! Dim for arrays vobs, vobs_qc
  INTEGER, INTENT(IN) :: niobf                  ! Number of observations
  INTEGER, INTENT(IN) :: nerrf                  ! Number of error fields
  INTEGER, INTENT(IN) :: opt_hum
  REAL,    INTENT(IN) :: xtime                  ! Model time (min) at time t-1
  REAL,    INTENT(IN) :: dtmin                  ! Model timestep in minutes
  REAL,    INTENT(IN) :: timeob(niobf)          ! Obs time (hrs)
  REAL,    INTENT(IN) :: oblat(niobf)           ! Latitude  at observation loc
  REAL,    INTENT(IN) :: oblon(niobf)           ! Longitude at observation loc
  REAL,    INTENT(IN) :: varmodel(niobf)         ! U  model state at obs loc
!  REAL,    INTENT(IN) :: vvmodel(niobf)         ! V  model state at obs loc
!  REAL,    INTENT(INOUT) :: tmodel(niobf)          ! T  model state at obs loc
!  REAL,    INTENT(IN) :: rhmodel(niobf)         ! Rh model state at obs loc
!  REAL,    INTENT(IN) :: rhobs(niobf)           ! Rh model state at obs loc
!  REAL,    INTENT(IN) :: Vtm(niobf)             ! model Vr
  INTEGER, INTENT(IN) :: desobs(40,niobf)
  INTEGER, INTENT(IN) :: stnid(40,niobf)
  INTEGER, INTENT(IN) :: ionf                   ! Model-step input frequency for obs
  INTEGER, INTENT(IN) :: wudate(14,niobf)
  INTEGER, INTENT(IN) :: platform_ob (40,niobf) ! Observation platform type
  INTEGER, INTENT(IN) :: wusource(40,niobf)
  INTEGER, INTENT(IN) :: ob_var_type(niobf)     ! Observation variable type(U,V,T,etc)
  REAL,    INTENT(INOUT) :: vobs(vdim, niobf)     ! Observational variables (see sub r2rhw)
  REAL,    INTENT(INOUT) :: vobs_qc(vdim, niobf)  ! Obs var quality control
  REAL,    INTENT(IN) :: errf(nerrf, niobf)       ! Model innovation
  REAL,    INTENT(IN) :: nlevs_ob(niobf)
  REAL,    INTENT(IN) :: elevob(niobf)            ! Obs elevation (meters)
  LOGICAL, INTENT(IN) :: flagsnd(niobf), flagbog(niobf)
  REAL,    INTENT(IN) :: qcu(niobf)
!!  REAL,    INTENT(IN) :: qcv(niobf)
!!  REAL,    INTENT(IN) :: qct(niobf)
!!  REAL,    INTENT(IN) :: qcq(niobf)
  INTEGER, INTENT(IN) :: map_proj          ! map projection index
  REAL,    INTENT(IN) :: truelat1          ! truelat1 for Lambert map projection
  REAL,    INTENT(IN) :: truelat2          ! truelat2 for Lambert map projection
  REAL,    INTENT(IN) :: cen_lat           ! center latitude for map projection
  REAL,    INTENT(IN) :: cen_lon           ! center longiture for map projection
  Real, INTENT(inout) :: Last_Obs_time

  Real :: timelow,timeupp
!! wuyh added 20130418 end

  LOGICAL, EXTERNAL :: wrf_dm_on_monitor  !externals

! Local variables
  integer :: n                 ! loop counter over obserations
  integer :: nsndlev
  integer :: istepf
  integer :: iwu               ! loop counter for character decoding
  integer :: kwu               ! loop counter over sounding levels
  real    :: dtminh
  real    :: dtmin2
  real    :: dtlow
  real    :: dtupp
  real    :: xhourf
  real    :: pppa              ! temp for pressure term
  character*40 :: platform     ! temp for obs platform type
  character*40 :: obsstnid     ! temp for obs station id 
  character*40 :: obsdes       ! temp for obs description
  character*40 :: source
  character*20 :: mydate

  CHARACTER*120,Parameter::rpt_format=       &
   '(2F20.5,4A40,F20.5,5I10,3L10,2I10,A20,13(F13.5,I7))'
  CHARACTER*120,Parameter::meas_format='(10(F13.5,I7))'
  CHARACTER*120,Parameter::end_format='(3(I7))'

  Integer::nfirst
  Real::tdo,spdo,diro,tdm,spdm,dirm,thick
  Real,Parameter::eps=1.0E-4
  Integer::td_qc,spd_qc,dir_qc,thick_qc
  Integer::littleo,lpair,lpair1,littlem
  Integer::iqc(nndgv)
  Integer::iseq_num,nk
  Real::slp,ref_pres,precip
  Real::ratmix,rad_per_deg
  Real::cone
  Real::Ue,Ve,rhm,rho

 rad_per_deg=3.141592654/float(180)
 call wucone(rad_per_deg,truelat1, truelat2, cone)
 xhourf= xtime/60.
 dtminh=dtmin/60.
! wuyh modified 20130814
! dtmin2 = 0.6*dtminh
 dtmin2 = dtminh*float(3**(inest-1))
! wuyh modified 20130814

! wuyh modified 20130116
 dtlow = dtmin2*float(ionf)
!    dtlow = dtmin2*float(ionf)
! wuyh modified 20130116
 dtupp = min(dtmin2*float(ionf),0.06)
 timelow = MAX(xhourf-dtlow,0.)
 timeupp = xhourf+dtupp
 istepf= INT((xtime+5.0E-2)/dtmin)
!   Do n=1,nsta
!    if(xhourf .gt. timeob(n)-dtmin2.AND. xhourf .le. timeob(n)+dtminh)THEN
!     write(417,1417)n,xtime,timeob(n)*60.,oblat(n),oblon(n), &
!        varobs(5,n),uumodel(n),vvmodel(n), &
!                     tmodel(n),rhmodel(n)
!    endif
!   Enddo 
! 1417 Format(I8,2(1x,F10.2),1x,F7.2,1x,F8.2,5(1x,F12.4))
! write(306,*) "begin Last_Obs_time=",Last_Obs_time,timelow,timeupp

! Set unassigned variables
    tdm       = missing_r
    spdm      = missing_r
    dirm      = missing_r
    tdo       = missing_r
    spdo      = missing_r
    diro      = missing_r
    thick    = missing_r
    slp      = missing_r
    ref_pres = missing_r
    precip   = missing_r
    td_qc    = missing_i
    spd_qc   = missing_i
    dir_qc   = missing_i
    thick_qc = missing_i
    iseq_num = missing_i

!    littlem = 300+inest
!    littleo = 400+inest
    lpair   = 500+inest
!    lpair1  = 510+inest

!!! wuyh added 20131205
! IF ( wrf_dm_on_monitor() ) THEN   !ajb modified 03072013 to fix DM bug
!!! wuyh added 20131205 end

  n=1
  do while ( n .le. nsta )
!    write(6,*) "wuyh write=",inest,Last_Obs_Time,timeupp,timeob(n)
    nsndlev = INT(nlevs_ob(n)+0.05)       ! levels of obs

    if(timeob(n) .gt. Last_Obs_Time .AND. timeob(n) .le. timeupp) then

!       write(418,1417)n,xtime,timeob(n)*60.,oblat(n),oblon(n), &
!       varobs(5,n),uumodel(n),vvmodel(n), &
!                    tmodel(n),rhmodel(n)

        DO iwu = 1, 40
            obsdes(iwu:iwu) = CHAR (     desobs(iwu,n)) ! obs description
          obsstnid(iwu:iwu) = CHAR (     stnid(iwu,n))  ! obs id
          platform(iwu:iwu) = CHAR (platform_ob(iwu,n)) ! obs platform
            source(iwu:iwu) = CHAR (   wusource(iwu,n)) ! obs origin
        ENDDO
        mydate(1:6)='      '
        DO iwu = 1, 14
            mydate(iwu+6:iwu+6) = CHAR (wudate(iwu,n))  ! obs date
        ENDDO

! header
 ! IF ( wrf_dm_on_monitor() ) THEN   !ajb modified 03072013 to fix DM bug
 !       WRITE(littlem,ERR = 519,FMT = rpt_format)                   &
 !         oblat(n),oblon(n),                                        &
 !         obsstnid,obsdes,platform,source,                          &
 !         elevob(n),nsndlev, 0,0,iseq_num,0,                        &
 !            flagsnd(n),flagbog(n),.false.,                         &
 !             -888888, -888888, mydate ,                            &
 !             slp,missing_i,missing_r,missing_i,                    &
 !        missing_r,missing_i,missing_r,missing_i,                   &
 !        missing_r,missing_i,missing_r,missing_i,                   &
 !        missing_r,missing_i,missing_r,missing_i,                   &
 !        missing_r,missing_i,missing_r,missing_i,                   &
 !        missing_r,missing_i,missing_r,missing_i, missing_r,missing_i
 !        WRITE(littleo,ERR = 519,FMT = rpt_format)                   &
 !          oblat(n),oblon(n),                                        &
 !          obsstnid,obsdes,platform,source,                          &
 !          elevob(n),nsndlev, 0,0,iseq_num,0,                        &
 !             flagsnd(n),flagbog(n),.false.,                         &
 !              -888888, -888888, mydate ,                            &
 !              slp,missing_i,missing_r,missing_i,                    &
 !         missing_r,missing_i,missing_r,missing_i,                   &
 !         missing_r,missing_i,missing_r,missing_i,                   &
 !         missing_r,missing_i,missing_r,missing_i,                   &
 !         missing_r,missing_i,missing_r,missing_i,                   &
 !         missing_r,missing_i,missing_r,missing_i, missing_r,missing_i
 ! ENDIF

! body
!!        Do iwu=1,nndgv
!!         if(abs(vobs_qc(iwu,n)).LE.1.1)then
!!           iqc(iwu)=INT(10.*vobs_qc(iwu,n)+1.0e-3)
!!         else
!!           iqc(iwu)=missing_i
!!         endif
!!        Enddo

!       if(n .EQ. 60075)then
!        write(415,1415)n,xtime,timeob(n)*60.,oblon(n), &
!        varobs(5,n),uumodel(n),vvmodel(n),tmodel(n),rhmodel(n)
!       endif
 1415   Format(I8,2(1x,F10.2),1x,F10.4,5(1x,F12.4))

!        Call sd2uv(map_proj,missing_r,rad_per_deg,CONE, &
!         cen_lat,cen_lon,oblon(n),uumodel(n),vvmodel(n),Ue,Ve,spdm,dirm)

!        Call sd2uv(map_proj,missing_r,rad_per_deg,CONE, &
!         cen_lat,cen_lon,oblon(n),varobs(U,n),varobs(V,n),Ue,Ve,spdo,diro)

!        spd_qc=iqc(U)
!        dir_qc=iqc(U)

        IF(vobs(Prs,n).LE.150.1 .AND. vobs(Prs,n) .GT. 5.)then
          pppa=vobs(Prs,n)*10.                                   !mb
!          if(opt_hum.eq.0)then
!            rhm = rhmodel(n)
!            rho = rhobs(n)
!          else
!            IF(tmodel(n).GT. 100. .AND. tmodel(n) .LE. 400.    &
!             .and. rhmodel(n) .gt. -0.001)then
!             ratmix =rhmodel(n)
!             call  r2rhw(ratmix,tmodel(n),pppa,rhm,0)
!            Else
!             rhm = missing_r
!            Endif
!            IF(varobs(Q,n) .LE.5. .AND. varobs(Q,n) .GT. -0.001 &
!                 .AND. varobs(T,n).GT.100. .AND. varobs(T,n).LE.400.)then
!             call  r2rhw(varobs(Q,n),varobs(T,n),pppa,rho,0)
!            Else
!             rho = missing_r
!            Endif
!          endif
          pppa=pppa*100.
        ELSE
          pppa = missing_r
!          iqc(P)=missing_i
!          tmodel(n)=missing_r
!          varobs_qc(T,n)=missing_r
!          iqc(T)=missing_i
!          rhm = missing_r
!          rho = missing_r
        ENDIF


!       if(n .EQ. 60075)then
!        write(416,1415)n,xtime,timeob(n)*60.,oblon(n), &
!        varobs(5,n),uumodel(n),vvmodel(n),tmodel(n),rhmodel(n)
!       endif

        IF ( wrf_dm_on_monitor() ) THEN   !ajb modified 03072013 to fix DM bug
!          WRITE (littlem , ERR = 520 , FMT = meas_format )          &
!               pppa, iqc(P),  varobs(H,n), iqc(H),                  &
!               tmodel(n),iqc(T),                                    &
!               tdm,    td_qc,                                       &
!               spdm, spd_qc, dirm,dir_qc,                           &
!               uumodel(n),  iqc(U),                                 &
!               vvmodel(n),  iqc(V),                                 &
!               rhm,  iqc(Q),                                        &
!               thick, thick_qc

!          WRITE (littleo , ERR = 520 , FMT = meas_format )          &
!               pppa, iqc(P),  varobs(H,n), iqc(H),                  &
!               varobs(T,n),iqc(T),                                  &
!               tdo,    td_qc,                                       &
!               spdo, spd_qc, diro,dir_qc,                           &
!               varobs(U,n),  iqc(U),                                &
!               varobs(V,n),  iqc(V),                                &
!               rho,  iqc(Q),                                        &
!               thick, thick_qc

          write(lpair,1298)n,xhourf,timeob(n),mydate(7:20),         &
             flagsnd(n),flagbog(n),                                 &
             obsstnid(1:5),obsdes(1:32),platform(1:14),source(1:16),&
             oblat(n),oblon(n),                                     &
             vobs_qc(Prs,n),vobs(Prs,n)*10.,vobs_qc(Hgt,n), vobs(Hgt,n),    &
             elevob(n), ob_var_type(n),           &
             qcu(n),vobs(1,n),vobs_qc(1,n),varmodel(n)
        ENDIF


        IF(nsndlev.GT.1)Then
         nfirst=n
         do kwu=2,nsndlev
          nk=nfirst+kwu-1
!          if(abs(oblat(nk)-oblat(nfirst))                           &
!           +abs(oblon(nk)-oblon(nfirst)).LE.eps                     &
!           .AND.elevob(nk).EQ.elevob(nfirst))then
!             Do iwu=1,nndgv
!              if(abs(varobs_qc(iwu,nk)).LE.1.1)then
!               iqc(iwu)=INT(10.*varobs_qc(iwu,nk)+1.0e-3)
!              else
!               iqc(iwu)=missing_i
!              endif
!             Enddo

!             Call sd2uv(map_proj,missing_r,rad_per_deg,CONE,cen_lat,cen_lon,  &
!                        oblon(nfirst),uumodel(nk),vvmodel(nk),Ue,Ve,spdm,dirm)
!             Call sd2uv(map_proj,missing_r,rad_per_deg,CONE,cen_lat,cen_lon,  &
!                        oblon(nfirst),varobs(U,nk),varobs(V,nk),Ue,Ve,spdo,diro)
!             spd_qc=iqc(U)
!             dir_qc=iqc(U)

             if(ABS(vobs(3,nk)).LE.150.1)then
              pppa=vobs(3,nk)*10.
!!              if(opt_hum.eq.0)then
!!               rhm = rhmodel(nk)
!!               rho = rhobs(nk)
!!              else
!!               IF(tmodel(nk).GT. 100. .AND. tmodel(nk) .LE. 400.    &
!!                  .and. rhmodel(nk) .gt. -0.001)then
!!                 ratmix =rhmodel(nk)
!!                 call  r2rhw(ratmix,tmodel(nk),pppa,rhm,0)
!!               Else
!!                 rhm = missing_r
!!               Endif
!!               IF(varobs(Q,nk) .LE.5. .AND. varobs(Q,nk) .GT. -0.001 &
!!                  .AND. varobs(T,nk).GT.100. .AND. varobs(T,nk).LE.400.)then
!!                 call  r2rhw(varobs(Q,nk),varobs(T,nk),pppa,rho,0)
!!               Else
!!                 rho = missing_r
!!               Endif
!!              endif
              pppa=pppa*100.
             elseif(vobs(3,nk).LE.-800000.)then
              pppa=vobs(3,nk)
              iqc(P)=missing_i
!!              rhm = missing_r
!!              rho = missing_r
             else
!              write(292,*)'wrong unit of prs for n=',n
             endif

!             if(abs(varobs(T,nk)).GT.1000.)then
!              varobs(T,nk)=missing_r
!              tmodel(nk)=missing_r
!             endif

!             IF ( wrf_dm_on_monitor() ) THEN   !ajb modified 03072013 to fix DM bug
!               WRITE (littlem , ERR = 521 , FMT = meas_format )      &
!               pppa, iqc(P),                                         & !prs
!               varobs(H,nk), iqc(H),                                 & !Z
!               tmodel(nk),  iqc(T),                                  & !T
!               tdm,    td_qc,                                        & !Td
!               spdm, spd_qc, dirm,   dir_qc,                         & !spd dir
!               uumodel(nk),iqc(U),                                   & !u
!               vvmodel(nk),iqc(V),                                   & !v
!               rhm,iqc(Q),                                           & !rh
!               thick, thick_qc                                         !thick  
!               WRITE (littleo , ERR = 521 , FMT = meas_format )      &
!               pppa, iqc(P),                                         & !prs
!               varobs(H,nk), iqc(H),                                 & !Z
!               varobs(T,nk),  iqc(T),                                & !T
!               tdo,    td_qc,                                        & !Td
!               spdo, spd_qc, diro,   dir_qc,                         & !spd dir
!               varobs(U,nk),iqc(U),                                  & !u
!               varobs(V,nk),iqc(V),                                  & !v
!               rho,iqc(Q),                                           & !rh
!               thick, thick_qc
!             ENDIF
!          endif
!        write(620+nk,1698)nk,xhourf,timeob(nk),                     &
!             platform,                                              &
!             oblat(nk),oblon(nk),                                   &
!             varobs(5,nk),                                          &
!             varobs_qc(1,nk),qcu(nk),errf(1,nk),                    &
!             varobs_qc(2,nk),qcv(nk),errf(2,nk),                    &
!             varobs_qc(3,nk),qct(nk),tmodel(nk),                    &
!             varobs_qc(4,nk),qcq(nk),errf(4,nk)

          IF ( wrf_dm_on_monitor() ) THEN   !ajb modified 03072013 to fix DM bug
             write(lpair,1298)nk,xhourf,timeob(n),mydate(7:20),       &
             flagsnd(n),flagbog(n),                                   &
             obsstnid(1:5),obsdes(1:32),platform(1:14),source(1:16),  &
             oblat(n),oblon(n),                                       &
             vobs_qc(Prs,nk),vobs(Prs,nk)*10.,vobs_qc(Hgt,nk),vobs(Hgt,nk),  &
             elevob(nk), ob_var_type(nk),                                    &
             qcu(nk),vobs(1,nk),vobs_qc(1,nk),varmodel(nk)
          ENDIF

!        write(358,1358)nk,uumodel,vvmodel,tmodel(nk),rhmodel

 1298        Format(I8,2(1x,F10.4),1x,A14,2(1x,l2),                   &
             1x,A5,1x,A32,1x,A14,1x,A16,                              &
             2(1x,F7.2),2(1x,F10.2,1x,F9.1),(1x,F9.1),2x,I2,          &
             2(2x,F10.2,1x,E12.6))
         enddo
        ENDIF

! tails of obs
        IF ( wrf_dm_on_monitor() ) THEN   !ajb modified 03072013 to fix DM bug
!          WRITE ( littlem , ERR = 522 , FMT = meas_format )           &
!          -777777., 0, -777777.,0,                                    &
!          float(nsndlev),0, missing_r,0,                              &
!          missing_r, 0, missing_r,0,                                  &
!          missing_r, 0, missing_r,0,                                  &
!          missing_r, 0, missing_r,0
!          WRITE(littlem,ERR=522,FMT=end_format) nsndlev, 0, 0

!          WRITE ( littleo , ERR = 522 , FMT = meas_format )           &
!          -777777., 0, -777777.,0,                                    &
!          float(nsndlev),0, missing_r,0,                              &
!          missing_r, 0, missing_r,0,                                  &
!          missing_r, 0, missing_r,0,                                  &
!          missing_r, 0, missing_r,0
!          WRITE(littleo,ERR=522,FMT=end_format) nsndlev, 0, 0
!         WRITE(littlem,ERR=522,FMT=end_format) nsndlev, n, n+nsndlev-1
          Last_Obs_Time = timeob(n) - 1.0E-5
        ENDIF

    elseif( timeob(n) .gt. timeupp) then
          write(6,*)"write_everything quit at n=",n
          IF ( wrf_dm_on_monitor() ) THEN   !ajb modified 03072013 to fix DM bug
!!! wuyh modified 20140225
             Last_Obs_Time = max(timeupp,0.)
!             Last_Obs_Time = timeob(n-1)    ! yliu 20131222 
!!! wuyh modified 20140225
          ENDIF
          go to 558
    endif  !judge if forecast time match obs time    
      n=n+nsndlev
    enddo      !end while over obs from 1 to nsta 
    Last_Obs_time = timeob(n)
    go to 558

519   continue
      print*,' troubles header n,nsta= ',n,nsta
      stop 619
520   continue
      print*,' troubles in surf obs n= ',n
      stop 620
521   continue
      print*,' troubles in sounding obs n= ',n
      stop 621
522   continue
      print*,' troubles in tail n= ',n
      stop 622

 558  write(6,*) 'finish write obs at xhourf=',xhourf
      IF ( wrf_dm_on_monitor() ) THEN   !ajb modified 03072013 to fix DM bug
       Last_Obs_Time = Last_Obs_Time + 1.0E-5
      ELSE
       Last_Obs_Time = float(0)
      ENDIF
#if ( defined(DM_PARALLEL))
       Last_Obs_Time = wrf_dm_sum_real(Last_Obs_Time)
#endif 

 1306 Format( "End Last_Obs_time =",3(1x,F10.2))

!*************************************************************************
! ONLINE QC CALCULATION
!*************************************************************************
! FCV additions start
! Re-evaluate QC value

!  IF (qc_internal) THEN

!      DO n = 1, nsta

         ! Skip obs without pressure, usually missing data
!         IF (varobs(5,n) .LE. 0.) THEN
!             qc_new = varobs_qc(:,n)
!             CYCLE
!         ENDIF

         ! Grab the platform type
!         DO i = 1, 40
!            platform (i:i) = CHAR (platform_ob(i,n)) ! observational platorm
!         ENDDO

         ! grab the number of vertical levels
!         nsndlev = nlevs_ob(n) 

         ! Perform QC on the fly
!         thetaobs=tmodel(n)         
!         write(199,*)'before',n,thetaobs,varobs(3,n)
!         Call fddaobs_qc(n,platform,nsndlev,missing_r,               &
!                              tolerance_p, tolerance_q,              &
!                              tolerance_t, tolerance_u,              &
!                              tolerance_p_sound, tolerance_q_sound,  &
!                              tolerance_t_sound, tolerance_u_sound,  &
!                              nndgv,varobs(:,n),errf(:,n),           &
!                              varobs_qc(:,n),thetaobs,t0,iprt,qc_new)
!         write(199,*)'after',n,thetaobs,varobs(3,n)

         ! Same QC value for U and V, use lowest value
!         qc_new (1) = MIN (qc_new (1), qc_new(2))
!         qc_new (2) = qc_new (1)


! wuyh added 20110318 end

         ! Overwrite with new QC
!         DO i = 1, 4
!            varobs_qcn(i,n) = qc_new (i)
!         ENDDO

!      ENDDO

!  ENDIF
! FCV additions end
!end if  !end if cobmask

!!!#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!!! Put new message passing here??? 
!!! NOTE: OK to use same mp routine as for Kalman gain.
!!! Now message pass new qc values
!!!  CALL get_full_kalman_gain_vector(nsta, nndgv, niobf,            &
!!!                                   mp_local_cobmask, varobs_qc)
!!!#endif

!!  call flush(littlem)
!!  call flush(littleo)
  call flush(lpair)

  RETURN

  END SUBROUTINE write_everything


  SUBROUTINE sd2uv(map_proj,missing_r,rad_per_deg,CONE,  &
              cen_lat,cen_lon,YLON,U,V,Ue,Ve,spde,dire)

!------------------------------------------------------------------------------
!  PURPOSE: Convert wind (U-V in m/s) components into wind speed (spd in m/s)
!           and direction (dir in degree 0-360)
!------------------------------------------------------------------------------

   implicit none
   Real,    intent(in)  ::rad_per_deg
   Integer, intent (in) :: map_proj
   real,    intent (in) :: u, v
   real,    intent (in) :: missing_r
   real,    intent (in) :: cone,ylon,cen_lat,cen_lon
   Real,intent(out) ::Ue,Ve,dire,spde

   real :: aearth,xlonrt, ang

   IF(abs(u)+abs(v).LE.250.)THEN
      spde = sqrt(U*U + V*V)

      XLONRT = CEN_LON-YLON
      if (XLONRT .GT. 180.0) XLONRT=XLONRT-360.0
      if (XLONRT .LT.-180.0) XLONRT=XLONRT+360.0
      ANG=XLONRT*CONE*rad_per_deg

!     FOR MERCATOR PROJECTION, THE WinDS ARE AS in EARTH COORDinATES

      if (map_proj .EQ.  3) ANG = 0.0
      if (cen_lat  .LT. 0.0) ANG = -ANG
      Ue = U*COS(ANG) - V*Sin(ANG)
      ve = U*Sin(ANG) + V*COS(ANG)

      if (spde .EQ. 0.0) then
            dire = 0.0
            return
      end if

      if (ve .EQ. 0.0) then
        if (Ue .GT. 0.0) dire = 270.0
        if (Ue .LT. 0.0) dire =  90.0
      else
        AEARTH = ATAN (Ue/Ve)/rad_per_deg
        if (Ue .LE. 0.0 .AND. Ve .LE. 0.0) dire = AEARTH
        if (Ue .LE. 0.0 .AND. Ve .GE. 0.0) dire = AEARTH + 180.0
        if (Ue .GE. 0.0 .AND. Ve .GE. 0.0) dire = AEARTH + 180.0
        if (Ue .GE. 0.0 .AND. Ve .LE. 0.0) dire = AEARTH + 360.0
      end if
!     write(413,1413)ylon,u,v,ue,ve,spde,dire
! 1413 Format(F8.3,6(1x,F12.4))
   ELSE
      Ue   = missing_r
      Ve   = missing_r
      spde = missing_r
      dire = missing_r
   Endif
   END SUBROUTINE sd2uv

!*************************************************************************
  SUBROUTINE r2rhw_real(r,t,p,rh,iice)
!*************************************************************************

! convert mixing ratio to rh
! if iice=1, use saturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
      Implicit None
      REAL, intent(out)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(in)  :: r
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
      if(r .gt.-0.01)then
       if(iice.eq.1.and.t.le.t0)then
        esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
       else
        esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
       endif
       rsat=eps*esat/(p-esat)
       rh=r*100./rsat
       rh=MIN(rh,100.)
      else
       rh=-99999.
      endif
      return
  END SUBROUTINE r2rhw_real

!*************************************************************************
  SUBROUTINE r2rhw(r,t,p,rh,iice)
!*************************************************************************

! convert mixing ratio to rh
! if iice=1, use saturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
      Implicit None
      REAL, intent(out)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(in)  :: r
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
      if(r .gt.-0.01)then
!       if(iice.eq.1.and.t.le.t0)then
!        esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
!       else
!        esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
!       endif
!       rsat=eps*esat/(p-esat)
      ESAT = 6.112 * EXP (17.67*(T-273.15)/(T-29.60))
      RSAT = 0.622 * ESAT /(P-ESAT)

       rh=r*100./rsat
       rh=MIN(rh,100.)
      else
       rh=-99999.
      endif
      return
  END SUBROUTINE r2rhw

  SUBROUTINE errf_from_Vr(errf, varmodel, vobs, vobs_qc, isvr,                  &
                          niobf, vdim, nndgv, nerrf,                            &
                          nsta, xhour, ktau, ktaur,                             &
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
                          MP_LOCAL_COBMASK,                                     &
                          MP_LOCAL_UOBMASK,MP_LOCAL_VOBMASK,                    &
#endif
                          ob_var_type, ob_ndg_type, ndg_term_indxs,             &
                          gridx_u, gridy_u, gridx_v, gridy_v, gridx_t, gridy_t, &
                          timeob, rio, rjo, rko, ub, vb, wb, tb, th2,           &
                          tratx, t0, pbase, pp, qr, missing_r, stnid, elevob,   &
                          dx, dy, ide, jde, k_end, its,ite, jts,jte,            &
                          ims,ime, jms,jme, kms,kme, kde)

!------------------------------------------------------------------------------
!     PURPOSE: THIS SUBROUTINE CALCULATES THE DIFFERENCE BETWEEN THE OBSERVED
!     VALUES AND THE FORECAST VALUES OF U AND V FROM THE RADIAL WIND Vr, AT THE
!     OBSERVATION POINTS.
!
!------------------------------------------------------------------------------
  USE module_fddaobs_utilities,ONLY: calc_winddir,U,V,Vr,Prs,Hgt,SPu,SPv,MNP
  USE module_model_constants,ONLY: degrad

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: vdim                    ! Dim for vobs, vobs_qc 
  REAL,    INTENT(INOUT) :: errf(nerrf, niobf)      ! Model errors
  REAL,    INTENT(INOUT) :: vobs(vdim,niobf)     ! Observational variables
  REAL,    INTENT(INOUT) :: vobs_qc(vdim,niobf)  ! Obs var quality control
  INTEGER, INTENT(IN) :: isvr           ! radial wind: (=0 off, =-1 U,V, =+1 Vr )
  INTEGER, INTENT(IN) :: niobf                   ! Number of observations
  INTEGER, INTENT(IN) :: nndgv                   ! Number of nudge variables
  INTEGER, INTENT(IN) :: nerrf                   ! Number of error fields
  INTEGER, INTENT(IN) :: nsta                    ! # stations held for use
  REAL,    INTENT(IN) :: xhour                   ! Current model time in hours
  INTEGER, INTENT(IN) :: ktau                    ! Forecast timestep index
  INTEGER, INTENT(IN) :: ktaur                   ! Restart timestep index
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  LOGICAL, INTENT(IN) :: MP_LOCAL_COBMASK(NIOBF) ! Cross-point mask
!!! wuyh added 20151211
  LOGICAL, INTENT(IN) :: MP_LOCAL_UOBMASK(NIOBF) ! Cross-point mask
  LOGICAL, INTENT(IN) :: MP_LOCAL_VOBMASK(NIOBF) ! Cross-point mask
!!! wuyh added 20151211
#endif
  INTEGER, INTENT(IN) :: ob_var_type(niobf)      ! ob variable type (U,V,T,etc)
  INTEGER, INTENT(IN) :: ob_ndg_type(niobf)     ! Nudge-term type (# term in ndg_term_indxs)
  INTEGER, INTENT(IN) :: ndg_term_indxs(MNP,2)  ! indices for nudging terms
  REAL,    INTENT(IN) :: gridx_u                 ! U-point x stagger
  REAL,    INTENT(IN) :: gridy_u                 ! U-point y stagger
  REAL,    INTENT(IN) :: gridx_v                 ! V-point x stagger
  REAL,    INTENT(IN) :: gridy_v                 ! V-point y stagger
  REAL,    INTENT(IN) :: gridx_t                 ! T-point x stagger
  REAL,    INTENT(IN) :: gridy_t                 ! T-point y stagger
  REAL,    INTENT(IN) :: timeob(niobf)           ! Obs time (hrs)
  REAL,    INTENT(IN) :: rio(niobf)              ! Obs west-east coordinate (non-stag grid)
  REAL,    INTENT(IN) :: rjo(niobf)              ! Obs south-north coordinate (non-stag grid)
  REAL,    INTENT(IN) :: rko(niobf)              ! Obs bottom-top coordinate

  REAL,    INTENT(IN) :: ub(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: vb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: wb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: tb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: th2(ims:ime,jms:jme)    ! 2m Potential temperature
  REAL,    INTENT(IN) :: tratx(ims:ime,jms:jme)  ! T to TH2 ratio on mass points
  REAL,    INTENT(IN) :: t0                      ! Reference temperature
  REAL,    INTENT(IN) :: pbase(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN) :: pp(ims:ime,kms:kme,jms:jme) ! perturbation pres (Pa)
  REAL,    INTENT(IN) :: qr(ims:ime,kms:kme,jms:jme) ! 
  REAL,    INTENT(IN) :: missing_r                  ! missing value
  INTEGER, INTENT(IN) :: stnid(40,niobf)            ! used for radial winds radar location
  REAL,    INTENT(IN) :: elevob(niobf)              ! obs elevation (meters)
  REAL,    INTENT(IN) :: dx                         ! x-grid resolution in meters
  REAL,    INTENT(IN) :: dy                         ! y-grid resolution in meters
  INTEGER, INTENT(IN) :: ide, jde, k_end            ! domain dims.
  INTEGER, INTENT(IN) :: its,ite, jts,jte           ! tile   dims.
  INTEGER, INTENT(IN) :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, INTENT(IN) :: kde                        ! domain dim.
  Real, INTENT(INOUT) :: varmodel(niobf)

! Local variables
  integer :: n
  integer :: ia(niobf)               ! west-east coordinate of ob (integer)
  integer :: ib(niobf)               ! south-north coordinate of ob (integer)
  integer :: ic(niobf)               ! vertical coordinate of ob (integer)
  real :: ra(niobf)
  real :: rb(niobf)
  real :: rc(niobf)
  real :: dxob,dyob,dzob
  integer :: iob,job,kob
  integer :: iobm,jobm,iobp,jobp,kobp
  real :: pug1,pug2,pvg1,pvg2

  real :: Vr_model
  real :: xr,yr,zr
  real :: wind_scale
  real :: x_rad, y_rad, rrr           ! for Vr innovation

  real :: winddir_wrf                 ! model wind direction for turbine data calc
  real :: windsp_wrf                  ! observed wind speed for turbine data calc
  real :: umod_tmp, vmod_tmp          ! temp for model wind for turbine data calc
  real :: obs_pottemp                 ! Potential temperature at observation

  Real::uumodel,vvmodel,tmodel,wwmodel,Vtm
  real,parameter:: eps=.000001
!!! wuyh added 20160605
  real,parameter:: wuvr = 8.      !!!8.  20. 30.
!!! wuyh added end 20160605

!!! wuyh added for saving Vr_model
  Vtm = -999998.
!!! wuyh added for saving Vr_model end

  DO N=1,NSTA

   if(ob_var_type(N).eq.Vr) then

    if(timeob(n) .ge. xhour .OR. ktau .eq. 0 .OR. ktau .eq. ktaur) then

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      IF(MP_LOCAL_UOBMASK(N))THEN     ! Do if obs on this processor
#endif

        IF(rko(n) .gt. 0.9) then

! Calculate horizontal model wind at obs point if data is radial wind or turbine
          RA(N)=RIO(N)-gridx_u
          RB(N)=RJO(N)-gridy_u
          IA(N)=RA(N)
          IB(N)=RB(N)
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          DXOB=RA(N)-FLOAT(IA(N))
          DYOB=RB(N)-FLOAT(IB(N))

          RC(N)=RKO(N)
          IC(N)=RC(N)
          KOB=MIN0(k_end,IC(N))
          KOBP=MIN0(KOB+1,K_END)
          DZOB=RC(N)-FLOAT(IC(N))

!         Store model surface pressure (not the error!) at U point.
          PUG1 = .5*( pbase(IOBM,1,JOB) + pbase(IOB,1,JOB) )
          PUG2 = .5*( pbase(IOB,1,JOB) + pbase(IOBP,1,JOB) )
          ERRF(SPu,N)=.001*( (1.-DXOB)*PUG1 + DXOB*PUG2 )

          uumodel =  (1.-DZOB)*((1.-DyOB)*((1.-DxOB)*UB(IOB ,KOB ,JOB )     &
                                              +DxOB *UB(IOBP,KOB ,JOB ))    &
                                   +DyOB* ((1.-DxOB)*UB(IOB ,KOB ,JOBP)     &
                                              +DxOB *UB(IOBP,KOB ,JOBP)))   &
                  +      DZOB *((1.-DyOB)*((1.-DxOB)*UB(IOB ,KOBP,JOB )     &
                                              +DxOB *UB(IOBP,KOBP,JOB ))    &
                                   +DyOB *((1.-DxOB)*UB(IOB ,KOBP,JOBP)     &
                                              +DxOB *UB(IOBP,KOBP,JOBP)))
        ELSE
          uumodel = -888888.
        ENDIF
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      ENDIF       ! end IF( MP_LOCAL_UOBMASK(N) )
#endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      IF(MP_LOCAL_VOBMASK(N))THEN     ! Do if obs on this processor
#endif
        IF(rko(n) .gt. 0.9) then
          RA(N)=RIO(N)-gridx_v
          RB(N)=RJO(N)-gridy_v
          IA(N)=RA(N)
          IB(N)=RB(N)
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          DXOB=RA(N)-FLOAT(IA(N))
          DYOB=RB(N)-FLOAT(IB(N))

!         Store model surface pressure (not the error!) at V point.
          PVG1 = .5*( pbase(IOBM,1,JOB) + pbase(IOB,1,JOB) )
          PVG2 = .5*( pbase(IOB,1,JOB) + pbase(IOBP,1,JOB) )
          ERRF(SPv,N)=.001*( (1.-DXOB)*PVG1 + DXOB*PVG2 )

          vvmodel    = (1.-DZOB)*((1.-DyOB)*((1.-DxOB)*VB(IOB ,KOB ,JOB )  &
                                                +DxOB *VB(IOBP,KOB ,JOB )) &
                                     +DyOB *((1.-DxOB)*VB(IOB ,KOB ,JOBP)  &
                                                +DxOB *VB(IOBP,KOB ,JOBP)))&
                          +DZOB* ((1.-DyOB)*((1.-DxOB)*VB(IOB ,KOBP,JOB )  &
                                                +DxOB *VB(IOBP,KOBP,JOB )) &
                                     +DyOB *((1.-DxOB)*VB(IOB ,KOBP,JOBP)  &
                                                +DxOB *VB(IOBP,KOBP,JOBP)))
        ELSE
          vvmodel = -888888.
        ENDIF

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      ENDIF       ! end IF( MP_LOCAL_COBMASK(N) )
#endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      IF(MP_LOCAL_COBMASK(N))THEN     ! Do if obs on this processor
#endif
!!      if(vobs_qc(1,n).le.4000000.) then
        If(vobs_qc(1,n).ge.600000.) then   !vertical wind
          RA(N)=RIO(N)-gridx_t
          RB(N)=RJO(N)-gridy_t
          IA(N)=RA(N)
          IB(N)=RB(N)
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBP=MIN0(jde-1,JOB+1)
          DXOB=RA(N)-FLOAT(IA(N))
          DYOB=RB(N)-FLOAT(IB(N))
  
          RC(N)=RKO(N)+0.5
          IC(N)=RC(N)
          KOB=MIN0(k_end,IC(N))
          KOBP=MIN0(KOB+1,K_END)
          DZOB=RC(N)-FLOAT(IC(N))
          if(rko(n) .le. 0.5) then
            wwmodel    = (1.-DyOB)*((1.-DxOB)*WB(IOB ,1,JOB)              &
                                       +DxOB *WB(IOBP,1,JOB))             &
                            +DyOB *((1.-DxOB)*WB(IOB ,1,JOBP)             &
                                       +DxOB* WB(IOBP,1,JOBP))
          else
            wwmodel    = (1.-DZOB)*((1.-DyOB)*((1.-DxOB)*WB(IOB ,KOB ,JOB )    &
                                                  +DxOB *WB(IOBP,KOB ,JOB ))   &
                                       +DyOB *((1.-DxOB)*WB(IOB ,KOB ,JOBP)    &
                                                  +DxOB *WB(IOBP,KOB ,JOBP)))  &
                            +DZOB *((1.-DyOB)*((1.-DxOB)*WB(IOB ,KOBP,JOB )    &
                                                  +DxOB *WB(IOBP,KOBP,JOB ))   &
                                       +DyOB *((1.-DxOB)*WB(IOB ,KOBP,JOBP)    &
                                                  +DxOB *WB(IOBP,KOBP,JOBP)))
          endif
        endif

!!      if(vobs_qc(1,n).ge.4000000.) then ! wind turbine
        if(vobs_qc(1,n).le.510000.) then ! wind turbine
          umod_tmp = uumodel
          vmod_tmp = vvmodel
          if ( (umod_tmp.ne.0.).and.(vmod_tmp.ne.0.) ) then
            if ( (umod_tmp.gt.-800000.).and.(vmod_tmp.gt.-800000.) ) then
              CALL calc_winddir(umod_tmp, vmod_tmp, winddir_wrf)
            endif
          endif

          ! ====== re-calculate u, v from obs using model wind dir
          windsp_wrf = vobs(1,n)
          vobs_qc(1,N) = 1.
          if(ndg_term_indxs(ob_ndg_type(n),1).eq.U) then
            vobs(1,n) = -windsp_wrf*sin(4*atan(1.0)*winddir_wrf/180.)   ! U component
            ERRF(1,N)=ERRF(1,N)+vobs(1,n)-uumodel    !Need uratiob?
          else
            vobs(1,n) = -windsp_wrf*cos(4*atan(1.0)*winddir_wrf/180.)   ! V component
            ERRF(1,N)=ERRF(1,N)+vobs(1,n)-vvmodel    !Need uratiob?
          endif
          !write(6,*) 'wind-turbine ob: N = ',n,' ERRF(1,N) = ',ERRF(1,N)
          !write(6,*) 'wind-turbine ob: N = ',n,' ERRF(1,N) = ',ERRF(1,N)

        else ! radial wind
          CALL calc_Vtm(n, niobf, vdim, nndgv, rko, tb, th2, vobs,     &
                        tratx, t0, pbase, pp, qr,                      &
                        iob, job, kob, kobp, dxob, dyob, dzob,         &
                        ims,ime, jms,jme, kms,kme, kde, Vtm)
          If(abs(uumodel)+abs(vvmodel)+abs(wwmodel)+abs(vtm) .le. 200.) then
            X_rad = .01*float(stnid(1,n))
            Y_rad = .01*float(stnid(2,n))
            xr = (RIO(n)-X_rad)*dx
            yr = (RJO(n)-Y_rad)*dy
            zr = (vobs(Hgt,n)-elevob(n))
!!! wuyh modified for osse 20150505
!!              rrr = SQRT(xr**2+yr**2+zr**2)+0.1
!!              Vr_model = (xr*uumodel(n) + yr*vvmodel(n) +
!!                          zr*(wwmodel(n)-Vtm(n))) / rrr
            rrr = SQRT(xr**2+yr**2)+1.E-3
            Vr_model = (xr*uumodel + yr*vvmodel) / rrr
!!! wuyh modified for osse 20150505
            varmodel(n) = Vr_model    !! wuyh use Vtm to save Vr_model

! Do appropriate radial wind scheme
            IF(ISVR.EQ.-1) THEN   ! DO LIMITED Vr --> UV SCHEME
    
              if(abs(Vr_model).ge.eps) then
                wind_scale = vobs(1,n)/Vr_model 
              else
                wind_scale = 1.0
              endif
!!              vobs_qc(1,N) = 1.

              if(ndg_term_indxs(ob_ndg_type(n),2).eq.U) then
                ERRF(1,N)=ERRF(1,N)+(wind_scale-1.)*uumodel
              elseif(ndg_term_indxs(ob_ndg_type(n),2).eq.V) then
                ERRF(1,N)=ERRF(1,N)+(wind_scale-1.)*vvmodel
              else
                write(6,*) "Wrong in obs_ndg_type in errf_from_Vr"
                stop 1111
              endif
  
            ELSE    ! DO FULL Vr SCHEME

              ERRF(1,N)=ERRF(1,N)+vobs(1,N)-varmodel(n)
!!!             write(215,1215) n,vobs(1,N),Vr_model,vobs_qc(1,N)
1215          Format(I7,2(1x,F12.4),1x,F10.2)
!!! wuyh added 20150322
              if(abs(ERRF(1,N)) .LE. wuvr)then
                vobs_qc(1,N) = 600001.
              elseif(abs(ERRF(1,N)) .LE. 14.)then
                vobs_qc(1,N) = 600001.
                ERRF(1,N)= wuvr*ERRF(1,N)/abs(ERRF(1,N))
              elseif(abs(ERRF(1,N)) .LE. 20.)then
                vobs_qc(1,N) = 600001.
                ERRF(1,N)= -888886.
              else
                ERRF(1,N) = -888885.
                vobs(1,N) = -888885.
                vobs_qc(1,N) = -888885.
              endif
!!! wuyh added 20150322
            ENDIF
          Else
            IF(isvr .EQ. -1 ) then
              vobs(1,N) = -888882.
              vobs_qc(1,N) = float(0)
              ERRF(1,N) = -888888.
            ELSEIF(isvr .EQ. 1)then
              ERRF(1,N) = -888888.
              vobs(1,N)    = -888882.
              vobs_qc(1,N) = float(0)
            ENDIF
          Endif
        endif    ! radial turbine or radial wind calc

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      ENDIF       ! end IF( MP_LOCAL_COBMASK(N) )
#endif
    endif   !timeob(n) judge

   endif  ! f(ob_var_type(N).eq.Vr)

  ENDDO     ! END OBS loop

  END SUBROUTINE errf_from_Vr

  SUBROUTINE calc_Vtm( n, niobf, vdim, nndgv, rko, tb, th2, vobs,       &
                       tratx, t0, pbase, pp, qr,                        &
                       iob, job, kob, kobp, dxob, dyob, dzob,           &
                       ims,ime, jms,jme, kms,kme, kde, Vtm )
!------------------------------------------------------------------------------
!     PURPOSE: THIS SUBROUTINE CALCULATES VTM AT OBSERVATION POINTS.
!------------------------------------------------------------------------------
  USE module_model_constants, ONLY : rcp
  USE module_fddaobs_utilities, ONLY : Prs, missing_r

  IMPLICIT NONE

  INTEGER, INTENT(IN)    :: n
  INTEGER, INTENT(IN)    :: niobf                       ! Maximum number of observations
  INTEGER, INTENT(IN)    :: vdim                        ! Dimension for array vobs
  INTEGER, INTENT(IN)    :: nndgv                       ! Number of nudge variables
  REAL,    INTENT(IN)    :: rko(niobf)                  ! Obs bottom-top coordinate
  REAL,    INTENT(IN)    :: tb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN)    :: th2(ims:ime,jms:jme)        ! 2m Potential temperature
  REAL,    INTENT(IN)    :: vobs(vdim, niobf)           ! Observational variables
  REAL,    INTENT(IN)    :: tratx(ims:ime,jms:jme)      ! T to TH2 ratio on mass points
  REAL,    INTENT(IN)    :: t0                          ! Reference temperature
  REAL,    INTENT(IN)    :: pbase(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN)    :: pp(ims:ime,kms:kme,jms:jme) ! perturbation pres (Pa)
  REAL,    INTENT(IN)    :: qr(ims:ime,kms:kme,jms:jme)
  INTEGER, INTENT(IN)    :: iob
  INTEGER, INTENT(IN)    :: job
  INTEGER, INTENT(IN)    :: kob
  INTEGER, INTENT(IN)    :: kobp
  REAL,    INTENT(IN)    :: dxob
  REAL,    INTENT(IN)    :: dyob
  REAL,    INTENT(IN)    :: dzob
  INTEGER, INTENT(IN)    :: ims,ime, jms,jme, kms,kme   ! memory dims.
  INTEGER, INTENT(IN)    :: kde                         ! domain dim.
  REAL,    INTENT(INOUT) :: Vtm

! Declare local variables
  integer :: ikk, iwu, jwu, kwu
  real    :: Qrmodel
  real    :: tmodel
  real    :: pmod(kde-1)
  real    :: th2ob, obs_pottemp
  real    :: pswu(4), qzh(4)
! real    :: tmodwu, tmodwt
  real    :: ptwu, qzz, aa
  integer :: kmod
  real    :: DZmod
  real    :: pppa, pppb, ttlow, ttupp
  real, parameter :: TRWU = 1000./287.05
  real, parameter :: tiny = 1.E-4

  if(rko(n).eq.1.0.and.(th2(IOB,JOB).gt.200.and.th2(IOB,JOB).lt.500))then
       th2ob=((1.-DYOB)*((1.-DXOB)*th2(IOB,JOB)              &
                         +DXOB *th2(IOB+1,JOB))              &
                         +DYOB *((1.-DXOB)*th2(IOB,JOB+1)    &
                         +DXOB *th2(IOB+1,JOB+1)))

       if(abs(th2(IOB+1,  JOB)) .le. tiny .or.               &
          abs(th2(IOB  ,JOB+1)) .le. tiny .or.               &
          abs(th2(IOB+1,JOB+1)) .le. tiny )then
                th2ob=th2(IOB,JOB)
       endif

  else
       th2ob= ((1.-DZOB)*((1.-DyOB)*((1.-DxOB)*TB(IOB,KOB,JOB)        &
                                         +DxOB*TB(IOB+1,KOB,JOB))     &
                             +DyOB *((1.-DxOB)*TB(IOB,KOB,JOB+1)      &
                                         +DxOB *TB(IOB+1,KOB,JOB+1))) &
                  + DZOB*((1.-DyOB)*((1.-DxOB)*TB(IOB,KOBP,JOB)       &
                                        +DxOB *TB(IOB+1,KOBP,JOB))    &
                             +DyOB *((1.-DxOB)*TB(IOB,KOBP,JOB+1)     &
                                        +DxOB *TB(IOB+1,KOBP,JOB+1))))+t0
  endif

  ikk =0
  qzh(1)=(1.-DyOB)*(1.-DxOB)
  qzh(2)=DyOB *(1.-DxOB)
  qzh(3)=(1.-DyOB)*DxOB
  qzh(4)=DyOB*DxOB
  qzz = float(0)
  tmodel = float(0)
  ptwu = float(0)


  Do iwu=IOB,IOB+1
  Do jwu=JOB,JOB+1
    ikk=ikk+1
    Do kwu=1,kde-1
      pmod(kwu)=(pbase(iwu,kwu,jwu)+pp( iwu,kwu,jwu ))/1000.
    Enddo
    pswu(ikk)=pmod(1)
    ptwu=ptwu+pswu(ikk)*qzh(ikk)
    Call interpwu(kde-1,vobs(Prs,n),pmod,kmod,DZmod)
    if(kmod .ge. 1) then
      qzz=qzz+qzh(ikk)
      pppa=(pbase(iwu,Kmod,jwu)+pp(iwu,Kmod,jwu))  !!/1000.
      ttlow=(TB(iwu,  Kmod,jwu)+t0)/(100000./pppa)**RCP
      pppb=(pbase(iwu,Kmod-1,jwu)+pp(iwu,Kmod-1,jwu)) !!/1000.
      ttupp=(TB(iwu,Kmod-1,jwu)+t0)/(100000./pppb)**RCP
      tmodel=tmodel+                                &
      (ttlow*(float(1)-DZmod)+ttupp*DZmod)*qzh(ikk)
    endif

  Enddo
  Enddo

  if(rko(n) .le. 1.) then
           Qrmodel = (1.-DyOB)*((1.-DxOB)*Qr(IOB,1,JOB)          &
                                  +DxOB *Qr(IOB+1,1,JOB))        &
                        +DyOB *((1.-DxOB)*Qr(IOB,1,JOB+1)        &
                                   +DxOB* Qr(IOB+1,KOB,JOB+1))

  else
           Qrmodel =((1.-DZOB)*                                 &
                    ((1.-DyOB)*((1.-DxOB)*Qr(IOB,KOB,JOB)       &
                                   +DxOB *Qr(IOB+1,KOB,JOB))    &
                         +DyOB*((1.-DxOB) *Qr(IOB,KOB,JOB+1)    &
                                  +DxOB  *Qr(IOB+1,KOB,JOB+1))) &
                  + DZOB*                                       &
                    ((1.-DyOB)*((1.-DxOB)*Qr(IOB,KOBP,JOB)      &
                                   +DxOB *Qr(IOB+1,KOBP,JOB))   &
                        +DyOB*((1.-DxOB) *Qr(IOB,KOBP,JOB+1)    &
                                  +DxOB  *Qr(IOB+1,KOBP,JOB+1))))

  endif
  Qrmodel = Qrmodel*1000.
  if(qzz .gt. 1.E-3)then
    tmodel=tmodel/qzz
    ptwu = ptwu/qzz
    If(Qrmodel .gt. 1.E-4)Then
      aa = (ptwu/vobs(Prs,N))**0.4
      Vtm = 5.4*aa*(vobs(Prs,N)*TRWU/tmodel*Qrmodel)**0.125
    Else
      Vtm = 0.
    Endif
  else
    ptwu     = missing_r
    tmodel   = missing_r
    Vtm      = missing_r
  endif
!!  write(555,'(a,1x,I6,4(1x,E15.8))')"n,Qrmod,vtm,Pobs=",n,Qrmodel,vtm(n),vobs(Prs,N),tmodel

  END SUBROUTINE calc_Vtm

  SUBROUTINE calc_pottemp_innov( rko, tb, th2, tratx,               &
                       vobs_P, vobs_T, t0,                          &
                       iob, job, kob, kobp, dxob, dyob, dzob,       &
                       ims,ime, jms,jme, kms,kme, kde, missing_r,   &
                       pottemp)

!------------------------------------------------------------------------------
!     PURPOSE: THIS SUBROUTINE CALCULATES THE INNOVATION FOR POTENTIAL
!              TEMPERATURE.
!------------------------------------------------------------------------------
  USE module_model_constants, ONLY : rcp

  IMPLICIT NONE

  REAL,    INTENT(IN)    :: rko                         ! Obs bottom-top coordinate
  REAL,    INTENT(IN)    :: tb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN)    :: th2(ims:ime,jms:jme)        ! 2m Potential temperature
  REAL,    INTENT(IN)    :: tratx(ims:ime,jms:jme)      ! T to TH2 ratio on mass points
  REAL,    INTENT(IN)    :: vobs_P                      ! Observational pressure
  REAL,    INTENT(IN)    :: vobs_T                      ! Observational temperature
  REAL,    INTENT(IN)    :: t0                          ! Reference temperature
  INTEGER, INTENT(IN)    :: iob
  INTEGER, INTENT(IN)    :: job
  INTEGER, INTENT(IN)    :: kob
  INTEGER, INTENT(IN)    :: kobp
  REAL,    INTENT(IN)    :: dxob
  REAL,    INTENT(IN)    :: dyob
  REAL,    INTENT(IN)    :: dzob
  INTEGER, INTENT(IN)    :: ims,ime, jms,jme, kms,kme   ! memory dims.
  INTEGER, INTENT(IN)    :: kde                         ! domain dim.
  REAL,    INTENT(IN)    :: missing_r                   ! missing value
  REAL,    INTENT(OUT)   :: pottemp                     ! Potential temperature

! Declare local variables
  real    :: pmod(kde-1)
  real    :: tratxob, th2ob
  real    :: qzh(4)
  integer :: kmod
  real    :: DZmod
  real, parameter :: tiny = 1.E-4

  if(rko.eq.1.0.and.(th2(IOB,JOB).gt.200.and.th2(IOB,JOB).lt.500))then

     tratxob = ((1.-DYOB)*((1.-DXOB)*tratx(IOB,JOB)          &
                              +DXOB *tratx(IOB+1,JOB))       &
                   +DYOB *((1.-DXOB)*tratx(IOB,JOB+1)        &
                              +DXOB *tratx(IOB+1,JOB+1)))

     th2ob   = ((1.-DYOB)*((1.-DXOB)*th2(IOB,JOB)            &
                              +DXOB *th2(IOB+1,JOB))         &
                   +DYOB *((1.-DXOB)*th2(IOB,JOB+1)          &
                             +DXOB *th2(IOB+1,JOB+1)))

     if(abs(th2(IOB+1,  JOB)) .le. tiny .or.                 &
        abs(th2(IOB  ,JOB+1)) .le. tiny .or.                 &
        abs(th2(IOB+1,JOB+1)) .le. tiny )then
                 th2ob=th2(IOB,JOB)
     endif

  else
     tratxob = 1.

     th2ob   = ((1.-DZOB)*((1.-DyOB)*((1.-DxOB)*TB(IOB,KOB,JOB) &
                              +DxOB *TB(IOB+1,KOB,JOB))         &
                   +DyOB *((1.-DxOB)*TB(IOB,KOB,JOB+1)          &
                              +DxOB *TB(IOB+1,KOB,JOB+1)))      &
                   +DZOB*((1.-DyOB)*((1.-DxOB)*TB(IOB,KOBP,JOB) &
                              +DxOB *TB(IOB+1,KOBP,JOB))        &
                   +DyOB *((1.-DxOB)*TB(IOB,KOBP,JOB+1)         &
                              +DxOB *TB(IOB+1,KOBP,JOB+1))))+t0
  endif

  if(tratxob .lt. 1.0E-3) tratxob=1.

!We must convert temperature to potential temperature
  pottemp = missing_r
  if(vobs_T .gt. 100. .and. vobs_T .le. 550. .and.  vobs_P .gt. 0.1) then
       pottemp=(vobs_T*(100./vobs_P)**RCP-th2ob)*tratxob
  endif

  END SUBROUTINE calc_pottemp_innov

  SUBROUTINE calc_tmodel( tb, vobs_P, t0, pbase, pp,                &
                          iob, job, dxob, dyob,                       &
                          ims,ime, jms,jme, kms,kme, kde, missing_r,  &
                          tmodel )

!------------------------------------------------------------------------------
!     PURPOSE: THIS SUBROUTINE CALCULATES THE INNOVATION FOR POTENTIAL
!              TEMPERATURE.
!------------------------------------------------------------------------------
  USE module_model_constants, ONLY : rcp

  IMPLICIT NONE

  REAL,    INTENT(IN)    :: tb(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN)    :: vobs_P                    ! Observational pressure
  REAL,    INTENT(IN)    :: t0                          ! Reference temperature
  REAL,    INTENT(IN)    :: pbase(ims:ime,kms:kme,jms:jme)
  REAL,    INTENT(IN)    :: pp( ims:ime,kms:kme,jms:jme )  ! perturbation pres (Pa)
  INTEGER, INTENT(IN)    :: iob
  INTEGER, INTENT(IN)    :: job
  REAL,    INTENT(IN)    :: dxob
  REAL,    INTENT(IN)    :: dyob
  INTEGER, INTENT(IN)    :: ims,ime, jms,jme, kms,kme   ! memory dims.
  INTEGER, INTENT(IN)    :: kde                         ! domain dim.
  REAL,    INTENT(IN)    :: missing_r                   ! missing value
  REAL,    INTENT(OUT)   :: tmodel                      ! model temperature 

  integer :: ikk, iwu, jwu, kwu
  integer :: kmod
  real    :: pppa, pppb, ttlow, ttupp
  real    :: qzz, qzh(4)
  real    :: pmod(kde-1)
  real    :: DZmod

  ikk =0
  qzh(1)=(1.-DyOB)*(1.-DxOB)
  qzh(2)=DyOB *(1.-DxOB)
  qzh(3)=(1.-DyOB)*DxOB
  qzh(4)=DyOB*DxOB
  qzz = float(0)
  tmodel = float(0)

  Do iwu=IOB,IOB+1
  Do jwu=JOB,JOB+1
    ikk=ikk+1
    Do kwu=1,kde-1
      pmod(kwu)=(pbase(iwu,kwu,jwu)+pp( iwu,kwu,jwu ))/1000.
    Enddo
    Call interpwu(kde-1,vobs_P,pmod,kmod,DZmod)
    if(kmod .ge. 1) then
      qzz=qzz+qzh(ikk)
      pppa=(pbase(iwu,Kmod,jwu)+pp(iwu,Kmod,jwu))  !!/1000.
      ttlow=(TB(iwu,  Kmod,jwu)+t0)/(100000./pppa)**RCP
      pppb=(pbase(iwu,Kmod-1,jwu)+pp(iwu,Kmod-1,jwu)) !!/1000.
      ttupp=(TB(iwu,Kmod-1,jwu)+t0)/(100000./pppb)**RCP
      tmodel=tmodel+(ttlow*(float(1)-DZmod)+ttupp*DZmod)*qzh(ikk)
    endif
  Enddo
  Enddo

  if(qzz .gt. 1.E-3)then
    tmodel=tmodel/qzz
  else
    tmodel= missing_r
  endif

  END SUBROUTINE calc_tmodel

  SUBROUTINE place_Obs_in_Mesh( inest, ityp, nsta, niobf, vdim, nndgv,      &
                         elevob, rio, rjo, t0, rcp, pbase, pp, tb, z,       &
                         gridx, gridy, gridx_t, gridy_t, missing_r,         &
                         ims,ime, jms,jme, kms,kme, its,ite, jts,jte,       &
                         ide, jde, kds,kde, k_start, k_end, vobs, vobs_qc,  & 
                         iobmg, jobmg, dxobmg, dyobmg, ra, rb, rc, rko )

!-------------------------------------------------------------------------------
! PURPOSE: THIS SUBROUTINE CALCULATES THE VERTICAL MODEL COORDINATE (RKO)
!          FOR ALL OBSERVATION POINTS, BASED ON THE (ADJUSTED) OBS PRESSURE.
!          ALSO CALCULATED AND OUTPUT ARE:
!
!           - OBS MASS GRID COORD AND INTERP FRACTION: IOBMG,JOBMG,DXOBMG,DYOBMG
!           - OBS VARIABLES AND OBS QC: VOBS,VOBS_QC
!-------------------------------------------------------------------------------
  USE module_fddaobs_utilities,ONLY: Prs, Hgt, min_ht, ht_to_p, ht_to_psur
  USE module_dm,ONLY: wrf_dm_sum_real

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: inest                            ! nest level
  INTEGER, INTENT(IN) :: ityp                             ! variable type
  INTEGER, INTENT(IN) :: nsta                             ! # stations held for use
  INTEGER, INTENT(IN) :: niobf                            ! max number of observations
  INTEGER, INTENT(IN) :: vdim                   ! Dim for vobs, vobs_qc 
  INTEGER, INTENT(IN) :: nndgv                  ! Number of nudge variables
  REAL,    INTENT(IN) :: elevob(niobf)          ! Obs elevation (meters)
  REAL,    INTENT(IN) :: rio(niobf)             ! Obs west-east coordinate (non-stag grid).
  REAL,    INTENT(IN) :: rjo(niobf)             ! Obs south-north coordinate (non-stag grid).
  REAL,    INTENT(IN) :: t0                               ! reference temperature
  REAL,    INTENT(IN) :: rcp                              ! model constant
  REAL,    INTENT(IN) :: pbase(ims:ime, kms:kme, jms:jme)
  REAL,    INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme )  ! perturbation pres (Pa)
  REAL,    INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )  ! temperature
  REAL,    INTENT(IN) :: z ( ims:ime, kms:kme, jms:jme )  ! ht(m) above sl on half-levels
  REAL,    INTENT(IN) :: gridx                            ! x-coord stagger for variable ityp
  REAL,    INTENT(IN) :: gridy                            ! y-coord stagger for variable ityp
  REAL,    INTENT(IN) :: gridx_t                          ! x-coord stagger for T
  REAL,    INTENT(IN) :: gridy_t                          ! y-coord stagger for T
  REAL,    INTENT(IN) :: missing_r                        ! missing value
  INTEGER, INTENT(IN) :: ims,ime, jms,jme, kms,kme        ! memory dims.
  INTEGER, INTENT(IN) :: its,ite, jts,jte                 ! tile   dims.
  INTEGER, INTENT(IN) :: ide, jde, kds,kde                ! domain dims. (partial)
  INTEGER, INTENT(IN) :: k_start, k_end                   ! k range for tile
  REAL,    INTENT(INOUT) :: vobs(vdim, niobf)          ! observational variables, 1-ob, 1-var
  REAL,    INTENT(INOUT) :: vobs_qc(vdim, niobf)       ! obs var quality control, 1-ob, 1-var
  INTEGER, INTENT(OUT) :: iobmg(niobf)                     ! Obs i-coord on mass grid
  INTEGER, INTENT(OUT) :: jobmg(niobf)                     ! Obs j-coord on mass grid
  REAL,    INTENT(OUT) :: dxobmg(niobf)          ! Interp. fraction (x dir) referenced to mass-grid
  REAL,    INTENT(OUT) :: dyobmg(niobf)          ! Interp. fraction (y dir) referenced to mass-grid
  REAL,    INTENT(OUT) :: ra(niobf)                    ! Obs i-coord on stagger for variable ityp
  REAL,    INTENT(OUT) :: rb(niobf)                    ! Obs j-coord on stagger for variable ityp
  REAL,    INTENT(OUT) :: rc(niobf)                    ! Obs k-coord
  REAL,    INTENT(INOUT) :: rko(niobf)                    ! Obs bottom-top coordinate

! Local variables
  integer :: n                 ! loop index 
  integer :: ia
  integer :: ib
  integer :: ic
  real :: pbbo(kms:kme)        ! column base pressure (cb) at obs loc.
  real :: ppbo(kms:kme)        ! column pressure perturbation (cb) at obs loc.
  real :: pob,zlo              ! temp for holding ob pressure
  real :: pob_qc               ! temp for holding ob pressure qc
  real :: hob                  ! temp for holding ob height
  Real :: ttlow, ttupp
  real :: pbhi, pphi
  integer :: k,kbot
  integer :: iob,job,iob_ms,job_ms
  real :: dxob,dyob,dxob_ms,dyob_ms
  logical :: MP_LOCAL_DUMMASK  ! Mask for work to be done on this processor

! This is tricky: Initialize pob to zero in all procs. Only one proc actually
! calculates pob. If this is an obs to be converted from height-to-pressure, then
! by definition, vobs(Prs,n) will initially have the missing value -888888. After
! the pob calculation, pob will be zero in all procs except the one that calculated
! it, and so pob is dm_summed over all procs and stored into vobs(Prs,n). So on
! subsequent passes, the dm_sum will not occur because vobs(Prs,n) will not have a
! missing value.

! Loop over observations
  DO N=1,NSTA

! See note above
    pob = float(0)

    RA(N)=RIO(N)-GRIDX
    RB(N)=RJO(N)-GRIDY
    IA   =RA(N)
    IB   =RB(N)
    IOB=MAX0(1,IA)
    IOB=MIN0(IOB,ide-1)
    JOB=MAX0(1,IB)
    JOB=MIN0(JOB,jde-1)
    DXOB=RA(N)-FLOAT(IA)
    DYOB=RB(N)-FLOAT(IB)

!   Save mass-point arrays for computing rko for all var types
    if(ityp.eq.1) then
      iobmg(n) = MIN0(MAX0(1,int(RIO(n)-gridx_t)),ide-1)
      jobmg(n) = MIN0(MAX0(1,int(RJO(n)-gridy_t)),jde-1)
      dxobmg(n) = RIO(N)-gridx_t-FLOAT(int(RIO(N)-gridx_t))
      dyobmg(n) = RJO(N)-gridy_t-FLOAT(int(RJO(N)-gridy_t))
    endif
    iob_ms = iobmg(n)
    job_ms = jobmg(n)
    dxob_ms = dxobmg(n)
    dyob_ms = dyobmg(n)

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )

! Set mask for obs to be handled by this processor
    MP_LOCAL_DUMMASK = TILE_MASK(IOB, JOB, its, ite, jts, jte)

    IF ( MP_LOCAL_DUMMASK ) THEN
#endif

! Interpolate pressure to obs location column and convert from Pa to cb.
          do k = kms, kme
            pbbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS) +     &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS) ) + &
                   DYOB_MS* ( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS+1) +   &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS+1) ) )
            ppbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS) +        &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS) ) +    &
                   DYOB_MS* ( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS+1) +      &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS+1) ) )
!     write(6,'(a,i2,2(a,f9.3)') ' k= ',k,' pbbo= ',pbbo(k),' ppbo= ',ppbo(k)
          enddo
!!! wuyh added 20140316 
          ttupp = (1.-DYOB_MS)*((1.-DXOB_MS)*TB(IOB_MS  ,1,  JOB_MS)  + &
                                    DXOB_MS *TB(IOB_MS+1,1,  JOB_MS)) + &
                      DYOB_MS *((1.-DXOB_MS)*TB(IOB_MS  ,1,JOB_MS+1) +  &
                                    DXOB_MS *TB(IOB_MS+1,1,JOB_MS+1))
!!! wuyh added 20140316

!ajb      20040119: Note based on bugfix for dot/cross points split across processors,
!ajb                which was actually a serial code fix: The ityp=2 (v-points) and
!ajb                itype=3 (mass-points) cases should not use the ityp=1 (u-points)
!ajb                case rko! This is necessary for bit-for-bit reproducability
!ajb                with the parallel run.

!*******************************************************************************
! BEGIN SECTION FOR UPPER AIR DATA
!*******************************************************************************
          If(ABS(rko(n)+99).lt.1.)then
            pob = vobs(Prs,n)
            pob_qc = vobs_qc(Prs,n)
            hob = vobs(Hgt,n)

! Compute pressure from height
            IF(pob .le. -1. .and. hob .gt. min_ht) then

!!! wuyh added 20140316 
!!               pob = ht_to_p( hob, pbbo, ppbo, z, iob_ms, job_ms,       &
!!                     dxob_ms, dyob_ms, k_start, k_end, kds,kde,         &
!!                     ims,ime, jms,jme, kms,kme )
               ttlow=(ttupp+t0)/(100./( pbbo(1)+ppbo(1) ))**RCP
               pob = ht_to_p( hob, pbbo, ppbo, z, iob_ms, job_ms,        &
                          dxob_ms, dyob_ms, ttlow, k_start, k_end, kds,kde,   &
                          ims,ime, jms,jme, kms,kme )
!!! wuyh added 20140316

            ENDIF

            IF(pob .gt. -1.)then
              do k=k_end-1,1,-1
                kbot = k
                if(pob .le. pbbo(k)+ppbo(k)) then
                  goto 199
                endif
              enddo
!!! wuyh added 20130627
              kbot = 1
!!! wuyh added 20130627
 199          continue
              pphi = ppbo(kbot+1)
              pbhi = pbbo(kbot+1)
!!! wuyh modified to make  the interpolation consistent 20130411
               rko(n) = real(kbot+1)-                                    &
                 LOG(pob/(pbhi+pphi))/LOG((pbbo(kbot)+ppbo(kbot))/(pbhi+pphi))
               rko(n)=max(rko(n),1.)

            ELSE
              write(6,*)"error in errob get rko  n=",n,hob,pob
!!! wuyh added 20140225
              rko(n) = -999.
              vobs(:,n) = missing_r
              vobs_qc(:,n) = -666665.
!!! wuyh added 20140225 end
            ENDIF
          Endif   !end abs(rko(n)+99 .le. 1.

!-------------------------------------------------------------------------------
! END SECTION FOR UPPER AIR DATA
!-------------------------------------------------------------------------------

!*******************************************************************************
! BEGIN SECTION FOR SURFACE DATA
!*******************************************************************************

! Calculate or modify ob pressure if needed.
          If(ABS(rko(n)-1) .le. 0.001 .and. vobs(Prs,n) .le. -1. .and.                          &
             vobs(Hgt,n) .gt. min_ht .and. vobs(Hgt,n) .le. 9000 .AND. elevob(n) .gt. -500.)Then

               ttlow=(ttupp+t0)/(100./( pbbo(1)+ppbo(1) ))**RCP
               pob = ht_to_p( vobs(Hgt,n), pbbo, ppbo, z, iob_ms, job_ms,   &
                          dxob_ms, dyob_ms, ttlow, k_start, k_end, kds,kde,      &
                          ims,ime, jms,jme, kms,kme )
          Endif
!-------------------------------------------------------------------------------
! END SECTION FOR SURFACE DATA
!-------------------------------------------------------------------------------

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
    ENDIF       !end IF( MP_LOCAL_DUMMASK )                                 !ajb
#endif

! ajb 20090423: If obs-in-height, vobs(Prs,n) is sum of pob (see note above).
    if(vobs(Prs,n) .le. -1. .and. vobs(Hgt,n) .gt. min_ht) then
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
       vobs(Prs,n) = wrf_dm_sum_real ( pob )
       if(vobs(Prs,n) .LE. 1.0)then
         vobs(Prs,n)=missing_r
       endif
#else
       vobs(Prs,n) = pob
       if(vobs(Prs,n) .LE. 1.0)then
         vobs(Prs,n)=missing_r
       endif
#endif
    endif

    RC(N)=RKO(N)

  ENDDO      ! END MESH LOOP OVER NSTA 

  END SUBROUTINE place_Obs_in_Mesh

#endif

END MODULE module_fddaobs_rtfdda
