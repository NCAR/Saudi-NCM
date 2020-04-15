!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! main_analog_code
!
! Written by:
! Luca Delle Monache, NCAR - lucadm@ucar.edu  (April 2010)
!
! Updated by:
! Luca Delle Monache, NCAR - lucadm@ucar.edu  (June 2011)
! Badrinath Nagarajan, NCAR - badri@ucar.edu  (June 2011)
! Will Cheng, NCAR - chengw@ucar.edu (May - Aug 2011)
!
! 2012-apr-20	main_analog_code.m:
!		Matlab version obtained from Luca Delle Monache, NCAR
! 2013-jul-09	Final modified version for CMAQ PM2.5 demonstration
!		By Irina Djalalova, NOAA/ESRL/PSD
!
! 2014-mar-25	main_analog.f90:
!		Convert Matlab version to Fortran 90
!		By Dave Allured, NOAA/ESRL/PSD/CIRES
!               Advanced Fortran features in play:
!               * Allocatable arrays in derived types
!               * Allocate on assignment
!               * Reallocate on assignment
!               By Dave Allured, NOAA/ESRL/PSD/CIRES
!
! 2014-Oct-01   Code updates:
!               * Added search for optimal analog predictor weights
!               * Improved I/O 
!               * Fixed bugs
!               By Stefano Alessandrini (ste), NCAR
!
! 2014-Dec-01   Code updates:
!               * Added NetCDF output capability
!               * Added namelist input file                 
!               By Luca Delle Monache (LDM), NCAR
!
! 2015-Aug-14   Code updates:
!               * fixed bug for code crash using wind direction as predictor
!                 due to zero or even negative standard deviation with
!                 Yamartino estimator     
!               * fixed bug when standard deviation is zero
!               * added capabilities for
!                  a) KFAS (formerly ANKF in 2011 MWR, Vol 139, 3554-3570): 
!                     KF in analog space
!                  b) KFAN: KF applied to AnEnMEAN
!                  c) AnEnMEAN: weighted mean of AnEn
!-----------------------------------------------------------------------------

program main_analog

  use config, only : dp
  use find__analog, only : fpar_type
!ste  use kf__analog
  use kf__luca, only : kpar_type
  use read__point_data
  use only__analog
  use netcdf  ! LDM
  use netcdf_sup  ! LDM
  use permute_mod  !ste
  implicit none
!ste MPI libraries
#ifdef MPI
  include 'mpif.h'
#endif

  integer count_substrings 		! external function def.

!-------------------------------------------------
! Local variables.
!-------------------------------------------------

  type (apar_type) :: apar
  type (fpar_type) :: fpar
  type (kpar_type) :: kpar

  character(200) pred_file, obs_file
  character(200) text_dir, out_template, outname
  character(200) out_file_ascii, out_file_netcdf, target_var_unit  ! LDM
  character(40)  target_var,upper_limit_target_var,lower_limit_target_var
  character(200) analog_predictors, lower_limits, upper_limits, is_circular
  real    upper_limit_target,lower_limit_target
  character suffix*1
  real, allocatable::weight_comb(:,:),ret(:),seq(:)
  real, allocatable ::  ws(:)
  integer, allocatable ::  process(:)
  real    rmse2,rmse1,rmse,nd
  integer nattempt,cont,icomb,ndimax,iproc
  character(40), allocatable :: vcodes(:), strs(:)
  real, allocatable :: upper(:),lower(:)  !ste 
  integer j, n, di, hi, si, vi,k,inp,st,prova
  integer ndays, nhours, nvars, nstations, num_lead_times, num_analogs
  integer opt_metric_endday, window, trend
  integer diag, iobs_var, ihour, nday_start, hour_start, forecast_freq
  integer day_range(2),iiday,jjtime,kkstaz,i,mmem
  integer outfile, iday, outday1, outday2, point_lead, point_stat, point_days
  integer ncid,  days_dimid, times_dimid, stats_dimid, nummem_dimid  ! LDM
  integer varid, days_varid, times_varid, stats_varid, nummem_varid  ! LDM
  integer dimids(4), anen_varid
  ! ===== WC: 2015-06-20 =========
  integer dim3d(3)
  ! ==============================

  integer short_period, write_ascii,write_netcdf,find_weights				! 1/0 switches
  integer forecast_model, forecast_obs_every	! 1/0 switches

  real(dp) vmiss  ! missing value code, common between all vars
  real(dp) ratio, num,start,finish

  logical write_ascii_files,write_netcdf_files,find_weight_file
  ! ==== added by WC: 2015-07-07 =======
  integer :: method, atec_file_format

  ! ====== arrays from HSoh for plotting ============
  ! added by WC: 2015-07-07 
  character(40) station_id
  ! ====== WC: 2015-09-30   ============
  ! make this work for gfortran
  !character(*), dimension(3),parameter :: dcodes = (/ 'site', 'gen_date','lead_time' /)
  character(len=15), dimension(3) :: dcodes
  ! ====================================
  integer int_value, str_index, stn_id_len
  integer site_len_dimid, site_varid
  ! ===================================

! Data arrays.

  real(dp), allocatable :: ens_data(:,:,:,:)	! multi var model forecast data
  real(dp), allocatable :: obs_data(:,:,:,:)	! observational data for 1 var

  real(dp), allocatable :: ens(:,:,:)		! subsets for current station
  real(dp), allocatable :: obs(:,:)		! obs subset is single var only
  real, allocatable :: obs_w(:,:)		! obs subset is single var only

  ! ====== arrays from HSoh for plotting ============
  ! added by WC: 2015-07-07 
  integer, allocatable :: in_dates(:)           ! 
  integer, allocatable :: in_lead_hours(:)      ! 
  character, allocatable :: in_stations(:,:)    ! observational data for 1 var
  ! ===================================

! Result arrays from analog filter.  See subroutine docs.

  real(dp), allocatable :: kfas(:,:,:)	! KFAS result
  ! ====== WC: 2015-07-06 ===========
  real(dp), allocatable :: kfas_mpi(:,:,:)   ! KFAS result: MPI
  real, allocatable :: kfas_out(:,:,:)       ! array to output KFAS, not real*8

  real(dp), allocatable :: kfan(:,:,:)     ! KFAN result
  real(dp), allocatable :: kfan_mpi(:,:,:) ! KFAN result: MPI
  real, allocatable :: kfan_out(:,:,:)     ! array to output KFAN, not real*8
    
  real(dp), allocatable :: anen_mean(:,:,:)	! bias corrected KFAN result
  real(dp), allocatable :: anen_mean_mpi(:,:,:)
  real, allocatable :: anen_mean_out(:,:,:)
  ! ==================================
  integer,  allocatable :: Ianalog(:,:,:)	! indices of found analogs
  real(dp), allocatable :: analog_in_an(:,:,:,:)	! nearest analogs found
  real(dp), allocatable :: analog_in_an_mpi(:,:,:,:)	! nearest analogs found
!ste introduced for mpi
  real, allocatable :: analog_in_an_w(:,:,:,:)	! nearest analogs found
  real, allocatable :: analog_out(:,:,:,:)	! matrix for output file compatible with verification libraries, not real*8
  real, allocatable :: weight(:,:) !ste weights for different predictors and stations         ! ste 
  real, allocatable :: weight_mpi(:,:) !ste weights for different predictors and stations         ! ste 

  ! ====== arrays from HSoh for plotting ============
  ! added by WC: 2015-07-07 
  real(dp), allocatable :: dim_lead_hours(:)
  real(dp), allocatable :: dim_stations(:)
  integer,  allocatable :: dim_days(:)
  real(dp), allocatable :: dim_members(:)
  ! ==========================================
! Fixed program parameters.


  real(dp), parameter :: celsius_to_kelvin = 273.15	! unit conversion
!ste   mpi variables definitions
      integer ierr,numprocs,num_proc,my_rank
#ifdef MPI
      integer status(MPI_STATUS_SIZE)
#endif

! Namelists
namelist / file_names / pred_file, obs_file, out_file_ascii, out_file_netcdf
namelist / target_variable / target_var, lower_limit_target_var, upper_limit_target_var, target_var_unit
namelist / analog_pred / analog_predictors, lower_limits, upper_limits, &
           is_circular
namelist / anen_parameters / nday_start, num_lead_times, num_analogs, hour_start, forecast_freq, &
           point_lead, point_stat, point_days, write_ascii, write_netcdf, find_weights,          &
           opt_metric_endday, window, trend, method, atec_file_format
!namelist / analog_additional / opt_metric_endday, window, trend
! End of declarations.
!-------------------------------------------------
! User input parameters.
!-------------------------------------------------
      ! ====== WC: 2015-09-30   ============
      ! make this work for gfortran
      dcodes(1) = 'site'
      dcodes(2) = 'gen_date'
      dcodes(3) = 'lead_time'
      ! =====================================

      ! ====== WC: 2015-07-07 ========
      my_rank = 0
      numprocs = 1
      stn_id_len = 0
      ! ==============================
#ifdef MPI
      call  MPI_Init(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr) 
      call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
#endif

      print*,"my_rank, numprocs",my_rank,numprocs 
if(my_rank==0) print *, 'Start main analog code.'
  inp = 100
  diag =3		! set verbosity: 0 = errors only, 1 = milestones,
			! 2 = brief progress, 3 = major details,
			! 4 = short details, 5 = more details, etc.
!ste now it is provided as an option form analog_ini.txt
!ste  write_ascii_files = .true.	! true = write check files for stations

!!  input_dir = '.'		! Input directory holding FORECASTS,
!!  				! OBS (METAR), ANALYSIS data (Mandatory)

!ste  pred_file = 'data/combined_data.20140425.12z.nc'
!ste  obs_file   = 'data/combined_data.20140425.12z.nc'

  short_period = 0		! only for GEM
  forecast_model = 0		! only one model is used          (mandatory)
  forecast_obs_every = 1	! model forecast comes every hour (mandatory)




! Read input namelist (analog_input.nml)
if(my_rank==0)print*,'Start reading input namelist'

open(inp, file = "analog_input.nml", status = "old", iostat = st)
read(unit = inp, nml = file_names)
read(unit = inp, nml = target_variable)
read(unit = inp, nml = analog_pred)
read(unit = inp, nml = anen_parameters)
!read(unit = inp, nml = analog_additional)

close(inp)
if(my_rank==0)print*,'End reading input namelist'

! ===== HSoh: read dimensional variables =====
! added by WC: 2015-07-07 
if ( (my_rank==0).and.(atec_file_format /= 0) ) then
 print *, 'Read dimensions from model data.'
 call read_dimension_data (pred_file, dcodes, diag, vmiss, in_stations, &
         in_dates, in_lead_hours, stn_id_len)
 print*, "in_stations = ", in_stations
endif
! =========================================

!ste flipping the order to allow setting in analog_ini.txt the order  provided by ncdump
 if(point_lead==1)then
     point_lead=3
  elseif(point_lead==3)then
   point_lead=1
 endif
 if(point_stat==1)then
     point_stat=3
  elseif(point_stat==3)then
   point_stat=1
 endif
 if(point_days==1)then
     point_days=3
  elseif(point_days==3)then
   point_days=1
 endif
! ste setting flag for writing or not ascii and netdcf output
   write_ascii_files = .true.
   if(write_ascii==0)write_ascii_files=.false.
   write_netcdf_files= .true.
   if(write_netcdf==0)write_netcdf_files=.false.
!ste setting flag for finding optimal weight

   find_weight_file = .true.
   if(find_weights==0)find_weight_file=.false.
 
! Select variables to be used in the analog method.
! an_vars is a space separated list.
! See var tables in read_var.f90 for codes and actual Netcdf var names.

! In the limit specs, use the suffix C to indicate degrees Celsius.

! Data time range.

!  day_range = (/ 1, 365 /)		! One year of data
!  day_range = (/ 1, 276 /)		! 9 months of data
  day_range = (/ 1, 61 /)		! 2 months of data
!ste day_range is not used

!!  days = ispan (1:365)  		! One year of data
!!  days = ispan (1:276)  		! 9 months of data
!!  days = ispan (1:61)			! 2 months of data
!ste if predictions needs to be done on a lower max leadtime than found in
!netcdf
!  num_lead_times=73
  !apar%start_stat = nday_start		! starting point to compute statistics
  				! (for November or for September only)

  apar%num_an = num_analogs		! Number of best analogs to use for AN
  apar%weights = 0		! 0: Do not weight analogs
				! 1: Weight them by the inverse metric
				! 2: Weight them linearly
  apar%skipMissingAnalogs = 1	! Applies to both ANKF and AN
				! 1: Always use num_an analogs, even if
				! some of the best ones are missing
  fpar%useRealTrends = 0	! 1: Use trend, 0: use neighbours   !ste,???
				! 0: (p0 - a0)^2 + (p+ - a+)^2 + (p- - a-)^2
				! 1: (p0 - a0)^2 + (p+ - p- - a+ + a-)^2  !ste
				! more similar to my result =option 1

! Correction for speed or ozone or PM2.5.

  kpar%enforce_positive = 1	! enforce correction for values < 0

  fpar%lowerMetric = 0.00001	! Lower bound for allowed metric value

! Kalman filter parameters.

  kpar%varo = 0.005		! variance of observation variance
  kpar%varp = 1			! variance prediction variance

  ratio = 0.1			! KF method parameter (sigma_ratio)

  ! ====== WC: 2015-08-06 ======================
  ! no need to hardwire
  !kpar%update = 24		! number of time stamps per forecast
  ! 				! CMAQ has 24 forecasts per day
  if (method.eq.2) then
   kpar%update = 1 
  else
   kpar%update = num_lead_times
  endif
  ! ==================================

  kpar%start = (/ 0,  0 /)	! starting point to compute statistics
  kpar%timeZone = 0

! Analog parameters

  apar%fvar = 1
  fpar%window = window		! check analog at the exact hour per day

  ! =============================
  if (trend.le.1) then
   fpar%trend = (/ 1, 1, 1 /)	! check hour-1, hour & hour+1 for the var trend
  elseif ( (trend.gt.1).and.(trend.le.2) ) then
   fpar%trend = (/ 1, 1, 1, 1, 1 /)   ! check hour-1,eck hour-2, hour, hour+1, hour+2 for the var trend
  elseif ( (trend.gt.2).and.(trend.le.3) ) then
   fpar%trend = (/ 1, 1, 1, 1, 1, 1, 1 /)
  else
   fpar%trend = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
  endif 
  				! with equal weighting coefficients
				! MUST have odd number of elements.
				! Numbers are the weighted coefficients.
				! 2014-feb-19, MUST have exactly 3 elements.

!-------------------------------------------------
! Derived parameters.
!-------------------------------------------------

! Convert the analog var table from free format spec strings, to arrays.

  nvars = count_substrings (analog_predictors)	! free format helper

  allocate (vcodes(nvars), fpar%is_circular(nvars), strs(nvars))
  allocate (kpar%lower_limits(nvars), kpar%upper_limits(nvars))
  allocate(lower(nvars),upper(nvars))
  read (analog_predictors, *) vcodes(:)		! use fortran free format,
  read (is_circular, *) fpar%is_circular(:)	! parse out the substrings

! Handle "C" unit suffixes on limit strings.  Convert Celsius to Kelvin.
! Assume all limit strings are non-blank and valid numbers, after
! suffixes are removed.

  do n = 1, 2
    if (n == 1) read (lower_limits, *) strs(:)	! get substrings
    if (n == 2) read (upper_limits, *) strs(:)

    do vi = 1, nvars				! for each limit string...
      j = len_trim (strs(vi))			! get final character
      suffix = strs(vi)(j:j)

      if (suffix == 'C') j = j - 1		! omit suffix
      read (strs(vi)(1:j), *) num		! read number part only

      if (suffix == 'C') num = num + celsius_to_kelvin   ! convert as needed

      if (n == 1) then
       kpar%lower_limits(vi) = num	! insert value in proper array
       lower(vi)=kpar%lower_limits(vi)
      elseif (n == 2) then
       kpar%upper_limits(vi) = num
       upper(vi)=kpar%upper_limits(vi)
      endif

    end do
  end do
!ste reading limit for target variable                               
     j = len_trim (upper_limit_target_var)
     read (upper_limit_target_var(1:j), *) upper_limit_target 
     j = len_trim (lower_limit_target_var)
     read (lower_limit_target_var(1:j), *) lower_limit_target 
! Get array subscript for the target forecast variable to be corrected.

!ste this check is useless, in general model variables don't include obs
!  apar%fvar = 0
!  do vi = 1, nvars
!     print*,"sono qui ",  vcodes(vi),target_var
!    if (vcodes(vi) == target_var) then
!      apar%fvar = vi
!      exit
!    end if
!  end do
!ste this check is useless, in general model variable must not include obs
!  if (apar%fvar == 0) then
!    print *, '*** main_analog: Specified target variable is not in input list.'
!    print *, '*** Target variable           = ' // trim (target_var)
!    print '(999(1x,a))', ' *** Requested model variables =', &
!      (trim (vcodes(j)), j = 1, nvars)
!    call exit (1)
!  end if

!-----------------------------------------------------------------
! Read input array data.
!-----------------------------------------------------------------
!
! The model data are forecast time series interpolated to the
! qualified AIRNow station locations, not gridded time series.
!
! obs must be in the format:
!    <day> x <hour> x   1   x <station>
!
! ens (model data) must be in the format:
!    <day> x <hour> x <var> x <station>
!
! Note that obs originates as a simple 1-D hourly station time
! series.  Obs has been reshaped into double time dimensions
! days x hours, to conform to the array configuration of the
! model forecast data.
!
! This means replicating obs data for the second 24 hours, when
! this program's forecast period is configured to be anything
! greater than 24 hours.  Currently this would be a function of
! the program preparing the custom obs input file, and not this
! filter program.
!
! The ensemble dimension from the original Matlab version was
! removed.  This was a vestigial dimension, not in functional
! use, and not fully implemented.
!
! This version assumes that the same missing value code is used
! across all obs and forecast input data.  This could be checked,
! if needed.
!
!-----------------------------------------------------------------

! Read model data for training period through present.
! The training period is for the analog filter.

if(my_rank==0)  print *, 'Read model data, multiple variables for the analog filter.'

  call read_point_data (pred_file, vcodes, diag, ens_data,vmiss,upper(1:nvars),lower(1:nvars),point_lead,point_stat,point_days)

  do vi = 1, nvars			! test only
   if(my_rank==0)    print *, trim (vcodes(vi)) // ':'
!   if(my_rank==0)    print '(3i6, 5f10.2)', ((di, hi, vi, ens_data(di, hi, vi, 1:5), &
!    hi = 1, 3), di = 1, 3)
  end do

! Read obs data for same training period.
! Single target variable, this version.

if(my_rank==0)  print *, 'Read obs data file, single variable.'
!ste setting the range for target variable (obs)
  upper(1)=upper_limit_target
  lower(1)=lower_limit_target
  call read_point_data (obs_file, (/ target_var /), diag, obs_data,vmiss,upper,lower,point_lead,point_stat,point_days)
  iobs_var = 1
if(my_rank==0)   print*,"read!!!!!"
  do vi = iobs_var, iobs_var		! test only
    if(my_rank==0)    print *, trim (target_var) // ':'
!    if(my_rank==0)    print '(3i6, 5f10.2)', ((di, hi, vi, obs_data(di, hi, vi, 1:5), &
!    hi = 1, 3), di = 1, 3)
  end do

! Get input dimensions.  nvars is already known.

  if (diag >= 3 .and. my_rank==0) print *, 'shape (ens_data) = ', shape (ens_data)
  if (diag >= 3 .and. my_rank==0) print *, 'shape (obs_data) = ', shape (obs_data)

  ndays     = size (ens_data, 1)
  nhours    = size (ens_data, 2)
  nstations = size (ens_data, 4)
   print*,"ndays,nhours,nstations",ndays,nhours,nstations

  ! ===== WC: 2015-07-23 ========
  ! option for negative nday_start
  if (nday_start.lt.0) then
   apar%start_stat = ndays + nday_start + 1
   if (apar%start_stat.le.0) then
    print*,"negative nday_start going back too far: stop"
    stop
   endif
  else
   print*,"nday_start only accepts negative values"
   print*,"nday_start = -1, the most recent day"
   print*,"nday_start = -2, 2nd most recent day, etc: stop"
   stop
  endif

  if (apar%start_stat.le.1) then
   print*,"nday_start must not go farther than last 2 days: stop"
   stop
  endif

  ! ==============================================
  if (find_weight_file) then
   outday2 = opt_metric_endday
   if (opt_metric_endday.gt.ndays) then
    print*,"opt_metric_endday set to too many days: stop"
    stop
   endif
  else
   outday2 = ndays
  endif
  ! ==============================================

!ste
  allocate(weight(nstations,nvars))  !allocate weights for each station and predictor
  allocate(weight_mpi(nstations,nvars))  !allocate weights for each station and predictor
  allocate(process(nstations)) 
!-------------------------------------
! Initialize for main loop.
!-------------------------------------

  if (diag >= 1 .and. my_rank==0) print *, 'Allocate main arrays.'
!ste nohours->num_lead_times,useful if the leadtime is lower then that of netcdf

! Two dummy output arrays for KFAN.
! Not used in this version.  Therefore, skip the station dimension.

  allocate (ianalog(ndays, num_lead_times, apar%num_an))	! DHA
 ! ====== WC: 2015-10-01 ========
 ! make it work for gfortran
 !if(find_weight_file==.false.) then
 if (.not.find_weight_file) then
  allocate (analog_in_an(ndays, num_lead_times, nstations, apar%num_an))	! DHA
  ! ======= WC: 2015-07-06 =======
  allocate (kfas(ndays, num_lead_times, nstations))
  allocate (kfan(ndays, num_lead_times, nstations))
  allocate (anen_mean(ndays, num_lead_times, nstations)) 
  ! ========================== 
 endif

  allocate (analog_in_an_mpi(ndays, num_lead_times, nstations, apar%num_an))	!ste
  ! ======= WC: 2015-07-06 ======
  allocate (kfas_mpi(ndays, num_lead_times, nstations))
  allocate (kfan_mpi(ndays, num_lead_times, nstations))
  allocate (anen_mean_mpi(ndays, num_lead_times, nstations))
  ! =============================
!ste number of maxdimension for MPI functions 
  ndimax=ndays*num_lead_times*nstations*apar%num_an
!ste                                                                        ! LDM
 ! ======== WC: 2015-10-01 ======
 ! make it work for gfortran
 !if(find_weight_file==.false.)allocate (analog_in_an_w(ndays, num_lead_times, nstations, apar%num_an))	! DHA
 if(.not.find_weight_file) allocate (analog_in_an_w(ndays, num_lead_times, nstations, apar%num_an))

  if (diag >= 3.and. my_rank==0) print *, 'Main allocate complete.'
  
  analog_in_an(:,:,:,:)=0.
  analog_in_an_mpi(:,:,:,:)=0.
  ! ==== WC: 2015-07-06 =======
  kfas(:,:,:) = 0.
  kfas_mpi(:,:,:) = 0.

  kfan(:,:,:) = 0.
  kfan_mpi(:,:,:) = 0.

  anen_mean(:,:,:) = 0.
  anen_mean_mpi(:,:,:) = 0.
  ! ==========================

!---------------------------------------------------
! Main station loop for the KF/AN filter method.
!---------------------------------------------------

!ste if findeweight=0 then attempt to read a file weight_ini.txt previously
!written
    ! ==== WC: 2015-10-01 ======
    ! make it work for gfortran
    !if(find_weight_file==.false.)then
    if (.not.find_weight_file) then
    if(my_rank==0)  print*,'Start reading weight_ini.txt'
     open(inp, file="weight_ini.txt", status="old", iostat=st)
      if (st.ne.0) then
       if(my_rank==0)     print*, "Error opening file weight_ini.txt: all weights set to 1"      !
       weight(:,:)=1.
      else
       do 50 si=1,nstations
        read(inp,*,iostat=st)(weight(si,i),i=1,nvars)
         if(st.ne.0)then
          if(my_rank==0)   print*,"Error while reading weight_ini.txt: all weights set to 1"
          weight(:,:)=1
          exit 
         endif 
       50 continue      
      endif
    else 
      if(my_rank==0)then     
       print*,"Opening weight_ini.txt file"
      
       open(inp, file="weight_ini.txt")
      endif
!ste intitializing all the weights to 1
       weight(1:nstations,1:nvars)=0.
!     allocate(ws(nvars))


        nattempt=11 ! number of possible weights

        allocate(ret(nvars),seq(nattempt))
        allocate(weight_comb(1000000,nvars))
        ret(:)=0.
        nd=1./real((nattempt-1.))
        seq(1)=0.
        do i=2,nattempt
         seq(i)=seq(i-1)+nd
        enddo
        cont=0

        call comp_weight(1,nvars,1,seq,ret,nattempt,cont,weight_comb)

    endif
! Note, 2014-feb-17, Dave A:  Removed all methods that were in the
! original Matlab code, except for the target KF/AN code.  To recon-
! struct other methods, refer to main_analog_code.m version 2013-jul-9.
!ste only analog members are provided now
    iproc=0
   do si=1,nstations
    process(si)=iproc
    iproc=iproc+1
    if(iproc==numprocs)iproc=0
   enddo
station_loop: &
  do si = 1, nstations
!  do si = 1,2 
!!  do si = 1, 2		!********************** TEST ONLY
!!    print *, '---------------------------------------------------------------'
    if(my_rank==process(si)) then
    print *, '*** station index = ', si,'Process number=',my_rank
! Subset arrays for current station.

!ste   obs = obs_data(:, :, iobs_var, si)	! (days, hours, vars, stations)
    obs = obs_data(:, 1:num_lead_times, iobs_var, si)	! (days, hours, vars, stations)
    					! --> (days, hours)
    
!ste    ens = ens_data(:, :, :,        si)	! (days, hours, vars, stations)
        ens = ens_data(:, 1:num_lead_times, :,        si)	! (days, hours, vars, stations)
    					! --> (days, hours, vars)

      outday1 = apar%start_stat 
!ste outday2 should be put in the name list, in order to allow optimization on
! a shorter period than the whole dataset
!      outday2 = ndays
!      outday2 = 479 
!      outday2 = opt_metric_endday
! Compute Analog result.

    ! ====== comment out by WC: 2015-06-26
    ! do not need to hardwire
    !kpar%update = 1
    !
    ! ===========================
     call cpu_time(start)
!ste    call kf_analog (obs, ens, vmiss, apar, fpar, kpar, ratio, diag, &
!ste      kfas(:,:,si), anen_mean(:,:,si), Ianalog, analog_in_an)
      					! last four are outputs
!ste only analog search are called, avoid kf
    if(find_weight_file)then
!ste put obs in obs_w
      obs_w=obs  
!ste case weight must be found minimizing rmse
     rmse1=1000000000.
     nattempt=11
     nd=0.1
     if(all(obs==vmiss))then  !ste if all the ob are missing avoid the optimization
      print*,"All obs missing value for station ",si
!      allocate (analog_in_an(ndays, num_lead_times, nstations, apar%num_an))
       weight(si,1:nvars)=1.
      print*,"Best weights,rmse,station",weight(si,1:nvars),rmse1,si
!    write(inp,*)(weight(si,k),k=1,nvars)
!ste       analog_in_an(:,:,si,:)=vmiss
      cycle
     endif

!try all the combinations already computed in weight_comb
! WC: 2015-06-22: added kfas
!$omp parallel do private(ws,rmse2,analog_in_an,analog_in_an_w,kfas,anen_mean)
        do icomb=1,cont
         ! ==== WC: 2015-06-22 ======
         print*, "my_rank, icomb = ", my_rank, icomb
         ! ===========================
         allocate(ws(nvars))
         allocate (analog_in_an(ndays, num_lead_times, nstations, apar%num_an))
         allocate (analog_in_an_w(ndays, num_lead_times, nstations, apar%num_an))
         ! ==== WC: 2015-07-06 ============
         allocate (kfas(ndays, num_lead_times, nstations))
         allocate (kfan(ndays, num_lead_times, nstations))
         allocate (anen_mean(ndays, num_lead_times, nstations))
         ! ================================
         ws=weight_comb(icomb,:)
         call only_analog (obs, ens, vmiss, apar, fpar, kpar, ratio, diag, &
                  kfas(:,:,si), kfan(:,:,si), anen_mean(:,:,si), Ianalog,  & 
                  analog_in_an(:, :, si, :),ws(:),forecast_freq,hour_start, &
                  lower_limit_target,upper_limit_target,method)  ! LDM !ste
                 analog_in_an_w(:,:,si,:)=analog_in_an(:,:,si,:)
          rmse2=rmse(obs_w(outday1:outday2,1:num_lead_times),analog_in_an_w(outday1:outday2,1:num_lead_times, si,1:apar%num_an),(outday2 - outday1 + 1),num_lead_times,apar%num_an)
         print*,"weights,rmse",ws(1:nvars),rmse2
          if(rmse2<rmse1)then
           weight(si,:)=ws(:)
           rmse1=rmse2
          endif
        ! ======= WC: 2015-06-20 =================
        !deallocate(ws,analog_in_an,analog_in_an_w)
        deallocate(ws,analog_in_an,analog_in_an_w,kfas,kfan,anen_mean)
        ! ==========================================
        enddo
!$omp end parallel do
    print*,"Best weights,rmse, station",weight(si,1:nvars),rmse1,si

 
    else
!ste case weight_ini.txt already exists   
    print *, 'processor, station, weight used  ',my_rank, si, weight(si,:) 
     call only_analog (obs, ens, vmiss, apar, fpar, kpar, ratio, diag,     &
                  kfas(:,:,si), kfan(:,:,si), anen_mean(:,:,si), Ianalog,  & 
                  analog_in_an(:, :, si,:),weight(si,:),forecast_freq,     &
                  hour_start,lower_limit_target,upper_limit_target,method)  ! LDM !ste
     endif
   call cpu_time(finish)
   print '("Time = ",f6.3," seconds."," Station=",I4)',finish-start,si
!ste      print*,"analog_in_ana",analog_in_an(335,1,1:10)
!---------------------------------------------------------------------
! Diagnostic text file output for each station.  Demo context only.
!---------------------------------------------------------------------

    if (write_ascii_files) then
      write (outname, '(a,i3.3,a)') trim(out_file_ascii), si, '.txt'
      !outname = outfile // si // '.txt'
      outfile = 10

!ste why?      outday1 = apar%start_stat + 1

     open  (outfile, file=outname, action='write')
!ste      write (outfile, '(f20.15)') anen_mean(outday1:outday2, :, si)
      do iday=outday1,outday2
        do ihour=1,num_lead_times 
          write(outfile,"(10f8.3)")(analog_in_an(iday,ihour,si,k),k=1,apar%num_an)  ! LDM
        enddo
      enddo
     close (outfile)
    end if

!! print *, '*** DEBUG STOP AFTER FIRST STATION.'
!! call exit
   endif
  end do station_loop
 print*,"End Station loop"
 ! ====== WC: 2015-10-01 ==========
 ! make it work for gfortran
 !if(find_weight_file==.true.) then
 if (find_weight_file) then
  allocate (analog_in_an(ndays, num_lead_times,nstations, apar%num_an))    !ste 
  allocate (analog_in_an_w(ndays, num_lead_times,nstations, apar%num_an))    !ste 
  ! ===== WC: 2015-07-06 ========
  allocate (kfas(ndays, num_lead_times,nstations))
  allocate (kfan(ndays, num_lead_times,nstations))
  allocate (anen_mean(ndays, num_lead_times,nstations))
  ! =============================
 endif

 print*,"Collecting vectors form nodes"
#ifdef MPI
   if(find_weight_file)CALL MPI_REDUCE(weight,weight_mpi,nstations*nvars, MPI_REAL, MPI_SUM, 0,MPI_COMM_WORLD, ierr)
#else
 if(find_weight_file) weight_mpi(1:nstations,1:nvars) = weight(1:nstations,1:nvars)
#endif

  ! AnEn option
  if ( (method.eq.1).or.(method.eq.3).or.(method.eq.4) ) then
#ifdef MPI  
   CALL MPI_REDUCE(analog_in_an(1:ndays,1:num_lead_times,1:nstations,1:apar%num_an),     &
                   analog_in_an_mpi(1:ndays,1:num_lead_times,1:nstations,1:apar%num_an), &
                   ndimax , MPI_DOUBLE_PRECISION, MPI_SUM, 0,MPI_COMM_WORLD, ierr)
#else
   analog_in_an_mpi(1:ndays,1:num_lead_times,1:nstations,1:apar%num_an) =   &
   analog_in_an(1:ndays,1:num_lead_times,1:nstations,1:apar%num_an)
#endif
  endif
  ! ===== WC: 2015-06-23 =========
  ! KFAS option
  if (method.eq.2) then
#ifdef MPI
   CALL MPI_REDUCE(kfas(1:ndays,1:num_lead_times,1:nstations),      &
                   kfas_mpi(1:ndays,1:num_lead_times,1:nstations),  &
                   ndays*num_lead_times*nstations,                         &
                   MPI_DOUBLE_PRECISION, MPI_SUM, 0,MPI_COMM_WORLD, ierr)
#else
   kfas_mpi(1:ndays,1:num_lead_times,1:nstations) = &
   kfas(1:ndays,1:num_lead_times,1:nstations)
#endif
  endif

  ! KFAN option
  if (method.eq.3) then
#ifdef MPI
   CALL MPI_REDUCE(kfan(1:ndays,1:num_lead_times,1:nstations),      &
                   kfan_mpi(1:ndays,1:num_lead_times,1:nstations),  &
                   ndays*num_lead_times*nstations,                         &
                   MPI_DOUBLE_PRECISION, MPI_SUM, 0,MPI_COMM_WORLD, ierr)
#else
   kfan_mpi(1:ndays,1:num_lead_times,1:nstations) = &
   kfan(1:ndays,1:num_lead_times,1:nstations)
#endif
  endif

  !AnEnMEAN option
  if (method.eq.4) then
#ifdef MPI
   CALL MPI_REDUCE(anen_mean(1:ndays,1:num_lead_times,1:nstations),      &
                   anen_mean_mpi(1:ndays,1:num_lead_times,1:nstations),  &
                   ndays*num_lead_times*nstations,                         &
                   MPI_DOUBLE_PRECISION, MPI_SUM, 0,MPI_COMM_WORLD, ierr)
#else
   anen_mean_mpi(1:ndays,1:num_lead_times,1:nstations) = &
   anen_mean(1:ndays,1:num_lead_times,1:nstations)
#endif
  endif
   ! ==============================
  if(my_rank==0)then !only main process write the output netcdf file    
   if(find_weight_file)then
   print*,"Writing weight file"
      do si=1,nstations  
       write(inp,*)(weight_mpi(si,k),k=1,nvars)
      enddo
      close(inp)
   endif
! LDM, write AnEn output to netCDF file
   print*,"Weight file written!"

!    if (write_netcdf_files) then
!        out_file = "AnEn_output.nc"
!        target_var_unit = "meter per second"

    if (write_netcdf_files) then
! Create file
        call check ( nf90_create(out_file_netcdf, nf90_clobber, ncid) )

        call check ( nf90_put_att(ncid, NF90_GLOBAL, "NDAYS_ORIG", ndays) )

! Define the dimensions
        call check ( nf90_def_dim(ncid, "NDAYS", outday2 - outday1 + 1, days_dimid) )
        call check ( nf90_def_dim(ncid, "NLEADTIMES", num_lead_times, times_dimid) )
        call check ( nf90_def_dim(ncid, "NSTATIONS", nstations, stats_dimid) )
        call check ( nf90_def_dim(ncid, "NMEMBERS", apar%num_an, nummem_dimid) )

! Define  coordinate variables
        call check ( nf90_def_var(ncid, "NDAYS", NF90_REAL, days_dimid, days_varid) )
        call check ( nf90_def_var(ncid, "NLEADTIMES", NF90_REAL, times_dimid, times_varid) )
        call check ( nf90_def_var(ncid, "NSTATIONS", NF90_REAL, stats_dimid, stats_varid) )
        call check ( nf90_def_var(ncid, "NMEMBERS", NF90_REAL, nummem_dimid, nummem_varid) )

! Assign units
        call check ( nf90_put_att(ncid, days_varid, "units", "number of forecast days") )
        call check ( nf90_put_att(ncid, times_varid, "units", "number of forecast lead &
            times") )
        call check ( nf90_put_att(ncid, stats_varid, "units", "number of station locations") )
        call check ( nf90_put_att(ncid, nummem_varid, "units", "number of analog ensemble &
            members") )

        ! ==============================================
        ! HSoh: for plotting 
        ! added by WC: 2015-07-07
        if ( (stn_id_len .gt. 0 ).and.(atec_file_format /= 0) ) then
          call check ( nf90_def_dim(ncid, "site_len", stn_id_len, site_len_dimid) )
          call check ( nf90_def_var(ncid, "site", NF90_CHAR, &
                           (/ site_len_dimid, stats_dimid /), site_varid) )
        endif
        ! ==============================================

       ! AnEn option
       if ( (method.eq.1).or.(method.eq.3).or.(method.eq.4) ) then
        dimids = (/ stats_dimid,days_dimid, times_dimid, nummem_dimid /)
! Define the netCDF variable
        call check ( nf90_def_var(ncid, "AnEn", NF90_REAL, dimids, anen_varid) )
        ! Assign the attribute unit to the netCDF variable
        call check ( nf90_put_att(ncid, anen_varid, "units", target_var_unit) )
        ! Assign the attribute missing_value to the netCDF variable
        call check ( nf90_put_att(ncid, anen_varid, "missing_value", real(vmiss)) )
        call check ( nf90_put_att(ncid, anen_varid, "_FillValue", real(vmiss)) )
       endif
       ! KFAS option
       ! ANKF in 2011 paper
       if (method.eq.2) then
        dim3d = (/ stats_dimid,days_dimid, times_dimid /)
        call check ( nf90_def_var(ncid, "KFAS", NF90_REAL, dim3d, anen_varid) )
        call check ( nf90_put_att(ncid, anen_varid, "description", 'This is ANKF in 2011 MWR, Vol 139, 3554-3570') )
        call check ( nf90_put_att(ncid, anen_varid, "units", target_var_unit) )
        call check ( nf90_put_att(ncid, anen_varid, "missing_value", real(vmiss)) )
        call check ( nf90_put_att(ncid, anen_varid, "_FillValue", real(vmiss)) )
       endif
       ! KFAN option
       ! KF applied to AnEnMEAN
       if (method.eq.3) then
        dim3d = (/ stats_dimid,days_dimid, times_dimid /)
        call check ( nf90_def_var(ncid, "KFAN", NF90_REAL, dim3d, anen_varid) )
        call check ( nf90_put_att(ncid, anen_varid, "description", 'KF applied to AnEnMEAN') )
        call check ( nf90_put_att(ncid, anen_varid, "units", target_var_unit) )
        call check ( nf90_put_att(ncid, anen_varid, "missing_value", real(vmiss)) )
        call check ( nf90_put_att(ncid, anen_varid, "_FillValue", real(vmiss)) )
       endif
       ! AnENMEAN option 
       ! AN in 2011 paper
       if (method.eq.4) then
        dim3d = (/ stats_dimid,days_dimid, times_dimid /)
        call check ( nf90_def_var(ncid, "AnEnMEAN", NF90_REAL, dim3d, anen_varid) )
        call check ( nf90_put_att(ncid, anen_varid, "description", 'This is AN in 2011 MWR, Vol 139, 3554-3570') )
        call check ( nf90_put_att(ncid, anen_varid, "units", target_var_unit) )
        call check ( nf90_put_att(ncid, anen_varid, "missing_value", real(vmiss)) )
        call check ( nf90_put_att(ncid, anen_varid, "_FillValue", real(vmiss)) )
       endif


! End define mode
         call check ( nf90_enddef(ncid) )

! Open netCDF file, write AnEn data, and close the file
        call check ( nf90_open(out_file_netcdf, nf90_write, ncid) )

       ! ===== write out variables according to options =======
       ! AnEn option
       if ( (method.eq.1).or.(method.eq.3).or.(method.eq.4) ) then
        print*,"Writing netcdf file for AnEn...."
        call check ( nf90_inq_varid(ncid, "AnEn", varid)) 

!ste permute the dimension order for outut file
          allocate(analog_out(nstations,(outday2-outday1+1),num_lead_times,apar%num_an))
          i=0
         do iiday=outday1,outday2
           i=i+1
          do jjtime=1,num_lead_times
           do kkstaz=1,nstations
            do mmem= 1,apar%num_an
             analog_out(kkstaz,i,jjtime,mmem)= real(analog_in_an_mpi(iiday,jjtime,kkstaz,mmem))
            enddo
           enddo
          enddo
         enddo
        call check ( nf90_put_var(ncid, varid, analog_out(:,:, :, :)) )
       endif
       ! KFAS option
       if (method.eq.2) then
        print*,"Writing netcdf file for KFAS...."
        call check ( nf90_inq_varid(ncid, "KFAS", varid))

        allocate(kfas_out(nstations,(outday2-outday1+1),num_lead_times))
          i=0
         do iiday=outday1,outday2
           i=i+1
          do jjtime=1,num_lead_times
           do kkstaz=1,nstations
            kfas_out(kkstaz,i,jjtime)= real(kfas_mpi(iiday,jjtime,kkstaz))
           enddo
          enddo
         enddo

        call check ( nf90_put_var(ncid, varid,    &
                     kfas_out(1:nstations,1:outday2-outday1+1,1:num_lead_times)) )
       endif
       ! KFAN option
       if (method.eq.3) then
        print*,"Writing netcdf file for KFAN...."
        call check ( nf90_inq_varid(ncid, "KFAN", varid))

        allocate(kfan_out(nstations,(outday2-outday1+1),num_lead_times))
          i=0
         do iiday=outday1,outday2
           i=i+1
          do jjtime=1,num_lead_times
           do kkstaz=1,nstations
            kfan_out(kkstaz,i,jjtime)= real(kfan_mpi(iiday,jjtime,kkstaz))
           enddo
          enddo
         enddo

        call check ( nf90_put_var(ncid, varid,    &
                     kfan_out(1:nstations,1:outday2-outday1+1,1:num_lead_times)) )

       endif
       ! AnENMEAN option
       if (method.eq.4) then
        print*,"Writing netcdf file for AnEnMEAN...."
        call check ( nf90_inq_varid(ncid, "AnEnMEAN", varid))

        allocate(anen_mean_out(nstations,(outday2-outday1+1),num_lead_times))
          i=0
         do iiday=outday1,outday2
           i=i+1
          do jjtime=1,num_lead_times
           do kkstaz=1,nstations
            anen_mean_out(kkstaz,i,jjtime)= real(anen_mean_mpi(iiday,jjtime,kkstaz))
           enddo
          enddo
         enddo

        call check ( nf90_put_var(ncid, varid,    &
                     anen_mean_out(1:nstations,1:outday2-outday1+1,1:num_lead_times)) )
       endif

       ! ======== HSoh for plotting ======
       ! added by WC: 2015-07-07
       if (atec_file_format /= 0) then      
 
        allocate(dim_lead_hours(num_lead_times))
        allocate(dim_stations(nstations))
        allocate(dim_days((outday2-outday1+1)))
        allocate(dim_members(apar%num_an))

        do mmem= 1,apar%num_an
           dim_members(mmem)= mmem
         enddo
         do kkstaz=1,nstations
           dim_stations(kkstaz)= kkstaz
         enddo
         if (allocated (in_stations)) then
           do kkstaz=1,nstations
             station_id = trim(transfer(in_stations(:,kkstaz),station_id))
             str_index = INDEX(station_id, '_s')

             if (str_index .gt. 0 ) then
               station_id = trim(transfer(in_stations((str_index+2):,kkstaz),station_id))
               read (station_id, * ) int_value
               dim_stations(kkstaz)= int_value
             endif
           enddo
         endif
         if (allocated (in_lead_hours)) then
           dim_lead_hours(:)= in_lead_hours(:)
         else
           do jjtime=1,num_lead_times
             dim_lead_hours(jjtime)= jjtime
           enddo
         end if
         do iiday=outday1,outday2
             ! chop off first two digits in YYYYMMDD
             dim_days(iiday - outday1 + 1) = in_dates(iiday) -     &
               1000000*(in_dates(iiday)/1000000)
         enddo

         call check ( nf90_put_var(ncid, days_varid, dim_days(:)) )

         call check ( nf90_put_var(ncid, times_varid, real(dim_lead_hours(:))) )

         call check ( nf90_put_var(ncid, stats_varid, real(dim_stations(:))) )

         call check ( nf90_put_var(ncid, nummem_varid, real(dim_members(:))) )

         if (allocated (in_stations)) then

           call check ( nf90_put_var(ncid, site_varid, in_stations, &
                        count=(/ stn_id_len, nstations /)) )
         endif

       endif
       ! =================================
       call check ( nf90_close(ncid) )

print *,"Successfully writing netCDF file", out_file_netcdf
      endif ! write_netcdf_files

     
     print *, 'Main_analog:  Done'
    endif! end of if(myrank==0)

#ifdef MPI
    call MPI_FINALIZE(ierr) !stops all the mpi processes
#endif

!---------------------------------------------------------------
! Output section.
! Write AN data to text files.  Demonstration context only.
!---------------------------------------------------------------

!  text_dir = 'PM_Probability'
!  text_dir = 'PM_KFANout'
!ste  text_dir = 'text'

!ste  out_template = trim (text_dir) // '/' // 'pmtsrwind_cmaqobs_an_DDD.txt'
!ste  outfile = 10				! output unit number

!ste  outday1 = apar%start_stat + 1
!ste  outday2 = ndays

!!  outday2 = outday1 + 3	! ********* JUST A FEW DAYS OUTPUT, FOR INITIAL DEBUG
!!  print *, '*** Single day output, for now.'

!ste  print *, 'main_analog: Write test output day files.'
!ste  print *, 'Writing to ' // trim (out_template)

!ste  do iday = outday1, outday2

! Create unique file name for current day.

!ste    outname = out_template
!ste    j = index (outname, 'DDD')			! insert the decimal day number
!ste    write (outname(j:j+2), '(i3.3)') iday	! into the output file name

! Write text file for current day.

!ste    print *, '  Output: ' // trim (outname)

!ste    open  (outfile, file=outname, action='write')
!ste    write (outfile, '(f20.15)') anen_mean(iday:iday, :, :)
!ste    close (outfile)
!ste  end do


end program main_analog
real function rmse(misure,forecast,ndays,nleadtime,n_membri)
implicit none
integer :: count,i,nleadtime,n_membri,ndays,ii
real, intent(in) ::misure(ndays,nleadtime)
real :: mean
real, intent(in) ::forecast(ndays,nleadtime,n_membri)
count=0
rmse=0
do i=1, ndays
 do ii=1,nleadtime
!  if (var(i).ne.-9999.000)then
   if (misure(i,ii)>-9998.000 .and.forecast(i,ii,1) > -9998.000)then
    count=count+1
    mean=sum(forecast(i,ii,1:n_membri))/n_membri
    rmse=rmse+(misure(i,ii)-mean)*(misure(i,ii)-mean)
   !print*,"mean",mean!,!forecast(i,1:n_membri)
   endif
 enddo
enddo
 if(count>0)then
  rmse=rmse/count
  rmse=sqrt(rmse)
 else
  rmse=100000000000.
 print*,"Attention no valid obs or forecast in the test period"
 endif

end function
recursive subroutine comp_weight(par,n,pos,seq,ret,m,cont,weight)
integer n,m,cont
real,intent(out):: ret(n),weight(1000000,n) !ste only 10^6 possible combinationis allowed, increased in case more comb. wanted
real,intent(in)::   seq(m)
integer,intent(in):: par,pos
ret(par)=seq(pos)
 if(nint(sum(ret)*1000)==1000 .and. par==n)then
  cont=cont+1
  weight(cont,:)=ret
 endif
 if(par <n)call comp_weight(par+1,n,1,seq,ret,m,cont,weight)
!only 11 weights from 0 to 1 are allowed 
if(pos<m)call comp_weight(par,n,pos+1,seq,ret,m,cont,weight)

end subroutine comp_weight


