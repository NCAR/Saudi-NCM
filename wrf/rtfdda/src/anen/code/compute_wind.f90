!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
!------------------------------------------------------------------------------
!
! compute_wind.f90 -- Read wind data from Netcdf, and compute wind direction
!		      or wind speed.
!
! This is a private support routine for read_point_data.f90.
!
! 2014-feb-11	Original version.  By Dave Allured, NOAA/CIRES/PSD.
!		Adapted from lines 365-421 in main_analog_code.m,
!		  version 2013-jul-9 (PSD3).
!
! See usage notes in main routine read_point_data.f90.
!
!------------------------------------------------------------------------------

subroutine compute_wind (ncid, var_code, var_table, len_vcodes, diag, &
    var_data, vmiss, nstations, nhours, ndays,point_hours,point_stat,point_days)
  implicit none

  integer,      intent (in ) :: ncid		! current netcdf file ID
  character(*), intent (in ) :: var_code	! short code for requested var
  character(*), intent (in ) :: var_table(:)	! lookup table for var names
  integer,      intent (in ) :: len_vcodes	! display width for var codes
  integer,      intent (in ) :: diag		! verbosity level, 0=errors only

  real(dp), allocatable, intent (inout) :: var_data(:,:,:)  ! computed var data
  real(dp),     intent (out) :: vmiss		! data code for missing value
  integer,      intent (out) :: nstations	! station dimension size
  integer,      intent (out) :: nhours		! hour dimension size
  integer,      intent (out) :: ndays		! date dimension size

! Local variables.

  character*10, parameter :: ucode ='U', vcode = 'V'
  integer :: point_hours,point_stat,point_days 
  real(dp) vmiss_u, vmiss_v

  real(dp), allocatable :: udata(:,:,:), vdata(:,:,:)	! U and V input arrays

! Read U and V data from the Netcdf input file.
! In this early version, no array cacheing.  Add later, to optimize.

  if (diag >= 5) print *, '*** compute_wind: Start.'

  call read_var (ncid, ucode, var_table, len_vcodes, diag, udata, vmiss_u, &
    nstations, nhours, ndays,point_hours,point_stat,point_days)

  call read_var (ncid, vcode, var_table, len_vcodes, diag, vdata, vmiss_v, &
    nstations, nhours, ndays,point_hours,point_stat,point_days)

  vmiss = vmiss_u			! return missing value, arbitrary pick

  if (diag >= 3) print *, 'Compute ' // trim (var_code(1:len_vcodes)) &
    // ' from U and V.'

! Allocate return array for computed var data, if needed.

  if (.not. allocated (var_data)) then
    if (diag >= 4) print *, 'Allocate computed var array.'
    allocate (var_data(nstations, nhours, ndays))
  end if

! Compute wind direction..

  if (var_code == 'WDIR') then
    
    where (udata == vmiss_u .or. vdata == vmiss_v)
      var_data = vmiss
    elsewhere
      var_data = modulo (180 / pi * atan2 (udata, vdata), 360d0)
    end where

! Compute wind speed.

  else if (var_code == 'WSPD') then

    where (udata == vmiss_u .or. vdata == vmiss_v)
      var_data = vmiss
    elsewhere
      var_data = sqrt (udata * udata + vdata * vdata)
    end where

  else
    print *, '*** compute_wind: Unknown variable code "' // trim (var_code) &
      // '".'
    print *, '*** Abort.'
    call exit (1)
  end if

! Untranslated matlab, 2014-feb-11.

!  spd_pred_max=max(max(max(spd_pred)));
!  spd_pred_min=min(min(min(spd_pred)));
!  spd_pred_thresh_min=min(min(min(spd_pred(spd_pred ~= par%mv))));
!  dir_pred_max=max(max(max(dir_pred)));
!  dir_pred_min=min(min(min(dir_pred)));
!  dir_pred_thresh_min=min(min(min(dir_pred(dir_pred ~= par%mv))));

  if (diag >= 5) print *, '*** compute_wind: Return.'

end subroutine compute_wind
