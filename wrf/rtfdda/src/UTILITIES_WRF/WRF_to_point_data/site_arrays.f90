  MODULE site_arrays

  USE wrf_kinds

  implicit none

  real(R8), allocatable :: clw(:,:,:,:)     ! cloud water mixing ratio
  real(R8), allocatable :: rnw(:,:,:,:)     ! rain water mixing ratio
  real(R8), allocatable :: t4d(:,:,:,:)     ! Temperature 4D
  real(R8), allocatable :: q4d(:,:,:,:)     ! Mixing ratio 4D
  real(R8), allocatable :: pb4d(:,:,:,:)    ! Base pressure 4D (Pa)
  real(R8), allocatable :: pt4d(:,:,:,:)    ! Perturbation pressure 4D (Pa)
  real(R8), allocatable :: p4d(:,:,:,:)     ! Pressure 4D (Pa)
  real(R8), allocatable :: u4d_stag(:,:,:,:) ! U-component wind 4D (m/s) stagger
  real(R8), allocatable :: v4d_stag(:,:,:,:) ! V-component wind 4D (m/s) stagger
  real(R8), allocatable :: u3d(:,:,:)       ! U-component wind 3D (m/s)
  real(R8), allocatable :: v3d(:,:,:)       ! V-component wind 3D (m/s)
  real(R8), allocatable :: tslb(:,:,:,:)    ! soil temperature (K)
  real(R8), allocatable :: tz(:,:)
  real(R8), allocatable :: pz(:,:)
  real(R8), allocatable :: qz(:,:)
  real(R8), allocatable :: uz(:,:)
  real(R8), allocatable :: vz(:,:)
  real(R8), allocatable :: tslbz(:,:)
  real(R8), allocatable :: cldfr2d(:,:)
  real(R8), allocatable :: cldfr(:)
  real(R8), allocatable :: cldfrh1d(:)
  real(R8), allocatable :: cldfrm1d(:)
  real(R8), allocatable :: cldfrl1d(:)
  real(R8), allocatable :: hlcy(:)
  real(R8), allocatable :: sm_u(:)
  real(R8), allocatable :: sm_v(:)
  real(R8), allocatable :: cdbp(:)
  real(R8), allocatable :: snowr(:)
  real(R8), allocatable :: terrain_hgt(:)   ! Terrain height(m)
  real(R8), allocatable :: Psfc(:)          ! Surface pressure (Pa)
  real(R8), allocatable :: Pmsl(:)          ! Surface pressure (Pa)
  real(R8), allocatable :: T2m(:),Q2m(:)    ! Temp and mixing ratio at 2m (K),(k g/kg)
  real(R8), allocatable :: u10m(:),v10m(:)  ! Wind speed at 10 m (m/s)
  real(R8), allocatable :: cumulus_prec(:)  ! accum. total cumulus precip (mm)
  real(R8), allocatable :: gdscale_prec(:)  ! accum. total grid scale precip (mm

  real(R8), allocatable :: pblHeight(:)     ! PBL height (m)
  real(R8), allocatable :: swDownFlux(:)    ! downward short-wave flux at sfc
  real(R8), allocatable :: visMax(:)        ! max visibility among 4 directions
  real(R8), allocatable :: visMin(:)        ! min visibility among 4 directions
  real(R8), allocatable :: tsk(:)           ! surface skin temperature (K)
  real(R8), allocatable :: snowc(:)         ! flag indicating snow coverage
  real(R8), allocatable :: snow(:)          ! snow water equivalent (kg/m^2)
  real(R8), allocatable :: snowh(:)         ! physical snow depth (m)
  real(R8), allocatable :: sfroff(:)        ! surface runoff (mm)
  real(R8), allocatable :: udroff(:)        ! surface runoff (mm)
  real(R8), allocatable :: evp(:)           ! accumulated potential evaporation
  real(R8), allocatable :: alt_density(:)   ! density altitude
  real(R8), allocatable :: alt_pressure(:)  ! pressure altitude
  real(R8), allocatable :: wspd10max(:)     ! maximum wind gust

  ! default model fields, can be overridden by model_fields namelist

  logical :: terrain = .TRUE.
  logical :: t_2m    = .TRUE.
  logical :: p_sfc   = .TRUE.
  logical :: p_msl   = .TRUE.
! logical :: q_2m    = .TRUE.
  logical :: td_2m   = .TRUE.
  logical :: rh_2m   = .TRUE.
  logical :: pbl     = .TRUE.
  logical :: wind_10m= .TRUE.
  logical :: max_gust= .TRUE.
! logical :: u_10m   = .TRUE.
! logical :: v_10m   = .TRUE.
! logical :: rain_c  = .TRUE.
! logical :: rain_nc = .TRUE.
  logical :: rain = .TRUE.
  logical :: swdown  = .FALSE.

  ! derived fields, can be specified by derived_fields namelist, no default

  logical :: vis     = .FALSE.
  logical :: alt_den = .TRUE.
  logical :: alt_pres= .TRUE.

  integer :: iterrain, ip_sfc, it_2m, iq_2m, iu_10m, iv_10m, &
             irain_c, irain_nc, ipbl, iswdown, imax_gust

  integer :: op_sfc, ot_2m, orh_2m, otd_2m, owind_10m, orain, opbl, oswdown, &
             op_msl, ovis, oalt_den, oalt_pres, omax_gust

  integer :: ivis, ialt_den, ialt_pres

  CONTAINS

  subroutine field_track(nlat,nWE,nSN,nBT,nfld_out)

  integer, intent(IN) :: nlat, nWE, nSN, nBT
  integer :: nfld_out

  !---------------------------------------------------------------------------
  !--- Keep track of which variables will be output (i* set to 1) and allocate
  !--- arrays accordingly
  !---------------------------------------------------------------------------

  if (terrain) then
     iterrain = 1
     allocate(terrain_hgt(nlat))
  else
     iterrain = 0
  end if

  if (p_sfc) then
     ip_sfc = 1
     op_sfc = 1
     allocate(Psfc(nlat))
  else
     ip_sfc = 0
     op_sfc = 0
  end if

  if (t_2m) then
     it_2m = 1
     ot_2m = 1
     allocate(T2m(nlat))
  else
     it_2m = 0
     ot_2m = 0
  end if

  if (rh_2m .and. (.not. p_sfc)) then
     print*,'Humidity variables depend on surface pressure. Set p_sfc to true!'
     stop
  end if

  if (rh_2m) then
     iq_2m = 1
     it_2m = 1
     ip_sfc = 1
     orh_2m = 1
     if (.not. allocated(Q2m)) allocate(Q2m(nlat))
     if (.not. allocated(T2m)) allocate(T2m(nlat))
     if (.not. allocated(Psfc)) allocate(Psfc(nlat))
  else
     orh_2m = 0
  end if

  if (td_2m) then
     iq_2m = 1
     it_2m = 1
     ip_sfc = 1
     otd_2m = 1
     if (.not. allocated(Q2m)) allocate(Q2m(nlat))
     if (.not. allocated(T2m)) allocate(T2m(nlat))
     if (.not. allocated(Psfc)) allocate(Psfc(nlat))
  else
     otd_2m = 0
  end if

  if (wind_10m) then
     iu_10m = 1
     iv_10m = 1
     owind_10m = 2  ! account for wdir and wspd
     allocate(u10m(nlat))
     allocate(v10m(nlat))
  else
     iu_10m = 0
     iv_10m = 0
     owind_10m = 0
  end if

  if (rain) then
     irain_c = 1
     irain_nc = 1
     orain = 1
     allocate(cumulus_prec(nlat))
     allocate(gdscale_prec(nlat))
     allocate(snow(nlat))
     allocate(snowh(nlat))
     allocate(sfroff(nlat))
     allocate(udroff(nlat))
     allocate(evp(nlat))
  else
     irain_c = 0
     irain_nc = 0
     orain = 0
  end if

  if (pbl) then
     ipbl = 1
     opbl = 1
     allocate(pblHeight(nlat))
  else
     ipbl = 0
     opbl = 0
  end if

  if (max_gust) then
     imax_gust = 1
     omax_gust = 1
     allocate(wspd10max(nlat))
  else
     imax_gust = 0
     omax_gust = 0
  end if

  if (swdown) then
     iswdown = 1
     oswdown = 1
     allocate(swDownFlux(nlat))
  else
     iswdown = 0
     oswdown = 0
  end if

  if (p_msl) then
     op_msl = 1
     ip_sfc = 1
     allocate(Pmsl(nlat))
     if (.not. allocated(Psfc)) allocate(Psfc(nlat))
  else
     op_msl = 0
  end if

  if (alt_den) then
     oalt_den = 1
     ip_sfc = 1
     if (.not. allocated(Psfc)) allocate(Psfc(nlat))
  else
     oalt_den = 0
  end if

  if (alt_pres) then
     oalt_pres = 1
     ip_sfc = 1
     if (.not. allocated(Psfc)) allocate(Psfc(nlat))
  else
     oalt_pres = 0
  end if

  if (vis) then
     it_2m = 1
     ip_sfc = 1
     ovis = 2  ! account for vismax and vismin
     allocate(clw(nWE, nSN, nBT, 1))
     allocate(rnw(nWE, nSN, nBT, 1))
     allocate(visMax(nlat))
     allocate(visMin(nlat))
  else
     ovis = 0
  end if

  allocate(tsk(nlat))
  allocate(snowc(nlat))


  ! Do not add iterrain

  nfld_out = op_sfc + op_msl + ot_2m + orh_2m + otd_2m + owind_10m + &
             orain + opbl + oswdown + ovis + oalt_den + oalt_pres + omax_gust


  return

  end subroutine field_track

  end MODULE site_arrays
