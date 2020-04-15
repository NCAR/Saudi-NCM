!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
!------------------------------------------------------------------------------
!
! read_point_data -- Read forecast and obs time series for multiple variables.
!
! This is a generic interface to read several forecast or
! observational data variables into a single multidimensional
! data array.  For use with the Kalman/Analog bias correction
! program, part of the CMAQ bias correction project.
!
! The current version reads multi-variable Netcdf files made
! with pm_create_netcdf_cmaqfile.pro.  This version reads time
! and forecast series at station locations, not gridded data.
!
! 2014-mar-05	Original version.  By Dave Allured, NOAA/CIRES/PSD.
!		This version specifically for prepared Netcdf files for the
!		  PSD3 PM2.5 experimental scenario, fixed set of stations,
!		  and fixed one-year time range.
!		This preliminary version does not include general purpose
!		  date/time indexing.
!2014-06-2 ste ale  change in tablevariablesfor xcel files
! Input:   infile = path to Netcdf input file with multiple variables.
!          var_codes = list of short codes for requested variables.
!          diag = verbosity control, 0 = errors only.  See below for more.
!
! Output:  out_data = 4-D array for time series data.
!		(forecast cycle, forecast hour, variable, station)
!	   Note, there are two time dimensions, AKA "forecast cycle" or
!		"date", and "forecast hour".
!	   vmiss = missing value code in data.
!
! Notes:
!
! In a single call, this version reads the given input file, and
! returns the data for a specified set of variables in a single
! multidimensional array.
!
! For the Kalman/Analog filter program, call this program once to
! read all the obs data, and another time to read all the model
! data.
!
! The variables to read are specified by a list of short code
! names.  Translation to the actual Netcdf var names is handled
! within this module.
!
! This version assumes a fixed dimension order in the input data.
! This is (stations, forecast_time, fcst_cycle) in Fortran order.
! The actual dimension names are not significant, only the
! ordering.
!
! Output arrays are allocated by this routine, and need not be
! pre-allocated.  Any that were previously allocated are
! deallocated on entry, then re-allocated.
!
! diag:  Set verbosity, i.e. diagnostic messaging level.  The
! messaging level is cumulative.  0 = errors only, 1 = entry point,
! 2 = short progress messages, 3 = output values, 4 = short work
! arrays, 5 = more details.
!
!------------------------------------------------------------------------------

module read__point_data

  use config, only : dp			! shared modules
  use netcdf
  use netcdf_sup
  use permute_mod
  use stdlit, only : pi
  implicit none

  private				! visibility controls
  public read_point_data 		! only the main routine is accessible
  ! ====== HSoh: for plotting ================
  ! WC: 2015-07-07
  public read_dimension_data    ! only the main routine is accessible
  ! ==========================================

  character(*), parameter :: routine_name = 'read_point_data.f90'

contains

!-----------------------------------------------------------------------------
! Public reader interface, returns multi-variable 5-D array.
!-----------------------------------------------------------------------------

subroutine read_point_data (infile, vcodes, diag,out_data,vmiss,up,low,point_hours,point_stat,point_days)
  implicit none

  character(*), intent (in ) :: infile		! input file path
  character(*), intent (in ) :: vcodes(:)	! codes for requested variables
  integer,      intent (in ) :: diag		! verbosity level, 0=errors only
  real,         intent(in)   :: up(:),low(:)   !limit of each variable !ste  
  real(dp), allocatable, intent (out) :: out_data(:,:,:,:)    ! output array
  						! (days, hours, vars, stations)
  real(dp),     intent (out) :: vmiss		! missing value code in file

! Local variables.

  character(len(vcodes)) vcode

  integer j, vi, ndays, nhours, nvars,nstations,point_hours,point_stat,point_days
  integer file_mode, ncid, len_vcodes
  real upper,lower
  real(dp), allocatable :: in_data(:,:,:)	! input array for single var

! Translation tables for var codes to Netcdf variable names.
! This table for pm_cmaqmodin.nc version 2013-jun-7, etc.

! Note, the computed variables are included here for completion only.
! These codes are intercepted as special cases, before the table is searched.

  ! ====== WC: 2015-09-29 ====================
  ! changed for gfortran
  !character(*), parameter :: vnames_model(11) = (/ &
  !  'OZ    OZONE              ', &
  !  'P_sfc    P_sfc    ', &
  !  'hpbl_sfc1   hpbl_sfc 1     ', &
  !  'hub_hgt_wind_dir1 hub_hgt_wind_dir1 ', &
  !  'hub_hgt_wind_speed1 hub_hgt_wind_speed1', &
  !  'hub_hgt_wind_speed_Obs hub_hgt_wind_speed_Obs ', &
  !  'SR    SolarRadiation     ', &
  !  'T     T  ', &
  !  'U     UGRD_10maboveground', &
  !  'V     VGRD_10maboveground', &
  !  'WDIR  (computed)         '  /)

  !character(50), parameter :: vnames_model(11) = (/                        &
  ! vnames_model(1)  =   'OZ    OZONE              ',                       &
  ! vnames_model(2)  =   'P_sfc    P_sfc    ',                              &
  ! vnames_model(3)  =   'hpbl_sfc1   hpbl_sfc 1     ',                     &
  ! vnames_model(4)  =   'hub_hgt_wind_dir1 hub_hgt_wind_dir1 ',            &
  ! vnames_model(5)  =   'hub_hgt_wind_speed1 hub_hgt_wind_speed1',         &
  ! vnames_model(6)  =   'hub_hgt_wind_speed_Obs hub_hgt_wind_speed_Obs ',  &
  ! vnames_model(7)  =   'SR    SolarRadiation     ',                       &
  ! vnames_model(8)  =   'T     T  ',                                       &
  ! vnames_model(9)  =   'U     UGRD_10maboveground',                       &
  ! vnames_model(10) =   'V     VGRD_10maboveground',                       &
  ! vnames_model(11) =   'WDIR  (computed)         '                    /)

  character(len=60), dimension(11) :: vnames_model
   vnames_model(1)  =   'OZ    OZONE              '                     
   vnames_model(2)  =   'P_sfc    P_sfc    '
   vnames_model(3)  =   'hpbl_sfc1   hpbl_sfc 1     '
   vnames_model(4)  =   'hub_hgt_wind_dir1 hub_hgt_wind_dir1 '
   vnames_model(5)  =   'hub_hgt_wind_speed1 hub_hgt_wind_speed1'
   vnames_model(6)  =   'hub_hgt_wind_speed_Obs hub_hgt_wind_speed_Obs '
   vnames_model(7)  =   'SR    SolarRadiation     '
   vnames_model(8)  =   'T     T  '           
   vnames_model(9)  =   'U     UGRD_10maboveground'
   vnames_model(10) =   'V     VGRD_10maboveground'
   vnames_model(11) =   'WDIR  (computed)         '
 
!-------------------------------------------------
! Initialize.
!-------------------------------------------------

  if (diag >= 1) print *,'*** read_point_data: Start.'

  nvars = size (vcodes)				! count requested variables

  len_vcodes = maxval (len_trim (vcodes(:)))	! for print column width

  if (diag >= 3) print *, 'Number of requested variables = ', nvars
  if (diag >= 3) print '(999(1x,a))', 'Requested var codes =', &
    (trim (vcodes(j)), j = 1, nvars)

! Open Netcdf input file.

  if (diag >= 2) print *, 'Open input file: ' // trim (infile)
  if (diag >= 3) print *, 'Netcdf library version = ' &
     // trim (nf90_inq_libvers())
  if (diag >= 3) print *

  file_mode = nf90_nowrite
  call check (nf90_open (infile, file_mode, ncid))
  if (diag >= 4) print *, 'ncid = ', ncid

!-------------------------------------------------
! Main loop to read each requested variable.
!-------------------------------------------------

read_var_loop: &
  do vi = 1, nvars

! Dispatch to a specific reader for derived variables, versus primary
! file variables.

! Either routine allocates the input data array, as needed.

    vcode = vcodes(vi)			! get current var code

    if (vcode == 'WDIR' .or. vcode == 'WSPD') then
      call compute_wind (ncid, vcode, vnames_model, len_vcodes, diag, &
        in_data, vmiss, nstations, nhours,ndays,point_hours,point_stat,point_days)
    else
      call read_var     (ncid, vcode, vnames_model, len_vcodes, diag, &
  in_data, vmiss, nstations, nhours, ndays,point_hours,point_stat,point_days)
    end if

! First time only: Allocate the main output array.

    if (vi == 1) then
      if (diag >= 3) print *, 'Allocate the main output array.'
      allocate (out_data(ndays, nhours, nvars, nstations))
    end if
!     if (vcode == 'hub_hgt_wind_speed_Obs')then
!      upper=upper_obs
!      lower=lower_obs
!     else
!       print*,'check read point',up(vi),vi
      upper=up(vi)
      lower=low(vi)
!     endif

! Copy the current variable data to the output array.
! Permute to the needed output dimension order.
! Avoid the Netcdf remapping feature, this seems more reliable.

!    call permute (in_data, out_data(:, :, vi, :), order=(/ 3, 2, 1 /))
!ste general permute o allow different input file format dimension, 
    call permute (in_data, out_data(:, :, vi, :), order=(/ point_days,point_hours, point_stat /))

!ste
   where (out_data(:,:,vi,:) < lower .or. out_data(:,:,vi,:) > upper) out_data(:,:,vi,:) = vmiss
!ste
!check if the variable is inside the limits, if not set to missing

        


  end do read_var_loop

! All done.  Close the input file.

  call check (nf90_close (ncid))

  if (diag >= 3) print *, '*** read_point_data: Return.'

end subroutine read_point_data

! ====== HSoh for plotting =================
! added by WC: 2015-07-07   

subroutine read_dimension_data (infile, dcodes, diag, vmiss, &
               out_stations, out_dates, out_lead_hours, stn_id_len)
  implicit none

  character(*), intent (in ) :: infile      ! input file path
  character(*), intent (in ) :: dcodes(:)   ! codes for requested dimensions
  integer,      intent (in ) :: diag        ! verbosity level, 0=errors only
  real(dp),     intent (out) :: vmiss       ! missing value code in file
  integer, allocatable, intent (out) :: out_dates(:)       ! output array
  integer, allocatable, intent (out) :: out_lead_hours(:)    ! output array
  character, allocatable, intent (out) :: out_stations(:,:)    ! output array
  integer, intent (out) :: stn_id_len    ! output array
  
! Local variables.

  character(len(dcodes)) dcode
  integer j, vi, nvars
  integer file_mode, ncid, len_dcode, len_dcodes
  integer ndim, stn_dim(2)
  character, allocatable :: in_data_str(:,:)	! input array for station
  
!-------------------------------------------------
! Initialize.
!-------------------------------------------------

  if (diag >= 1) print *,'*** read_dimension_data: Start.'

  nvars = size (dcodes)				! count requested variables

  len_dcodes = maxval (len_trim (dcodes(:)))	! for print column width

  if (diag >= 3) print *, 'Number of requested dimension variables = ', nvars
  if (diag >= 3) print '(999(1x,a))', 'Requested var codes =', &
    (trim (dcodes(j)), j = 1, nvars)

! Open Netcdf input file.

  if (diag >= 2) print *, 'Open input file: ' // trim (infile)
  if (diag >= 3) print *, 'Netcdf library version = ' &
     // trim (nf90_inq_libvers())
  if (diag >= 3) print *

  file_mode = nf90_nowrite
  call check (nf90_open (infile, file_mode, ncid))
  if (diag >= 4) print *, 'ncid = ', ncid
  if (diag >= 4) print *, 'dcodes = ', dcodes

!-------------------------------------------------
! Main loop to read each requested variable.
!-------------------------------------------------

read_dim_var_loop: &
  do vi = 1, nvars

! Either routine allocates the input data array, as needed.

    dcode = dcodes(vi)			! get current var code

! First time only: Allocate the main output array.

    len_dcode = len_trim (dcode)
    if (vi == 1) then
      if (diag >= 3) print *, 'Allocate the main output array.'
!      call read_dim_str_var (ncid, dcode, len_dcodes, diag, out_stations, vmiss, ndim)
      call read_dim_str_var (ncid, dcode, len_dcode, diag, in_data_str, vmiss, stn_dim)
      allocate (out_stations(stn_dim(1), stn_dim(2)))
      out_stations(:,:) = in_data_str(:,:)
      stn_id_len = stn_dim(1)
    else if (vi == 2) then
      call read_dim_int_var (ncid, dcode, len_dcode, diag, out_dates, vmiss, ndim)
    else if (vi == 3) then
      call read_dim_int_var (ncid, dcode, len_dcode, diag, out_lead_hours, vmiss, ndim)
    end if
  end do read_dim_var_loop

! All done.  Close the input file.

  call check (nf90_close (ncid))

  if (diag >= 3) print *, '*** read_dimension_data: Return.'

end subroutine read_dimension_data

! ==========================================
!-------------------------------------------------
! Included support routines.
!-------------------------------------------------

include 'compute_wind.f90'
include 'read_var.f90'

end module read__point_data
