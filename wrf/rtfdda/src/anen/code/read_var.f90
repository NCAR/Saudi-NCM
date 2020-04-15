!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
!------------------------------------------------------------------------------
!
! read_var.f90 -- Read a single array variable from a Netcdf file.
!
! This is a private support routine for read_point_data.f90.
! This is the low level reader for actual file variables,
! not computed variables.
!
! 2014-feb-13	Original version.  By Dave Allured, NOAA/CIRES/PSD.
!
! See usage notes in main routine read_point_data.f90.
!
! Subroutine assumptions:
!
! * This routine is called only for hard file variables, not derivatives.
! * This routine will perform a var code lookup in the given var name table.
! * On entry, the target Netcdf file is already open.
!
!------------------------------------------------------------------------------

subroutine read_var (ncid, vcode, var_table, len_vcodes, diag, in_data, &
    vmiss, nstations, nhours, ndays,point_hours,point_stat,point_days)
  implicit none

  integer,      intent (in ) :: ncid		! current netcdf file ID
  character(*), intent (in ) :: vcode		! short code for requested var
  character(*), intent (in ) :: var_table(:)	! lookup table for var names
  integer,      intent (in ) :: len_vcodes	! display width for var codes
  integer,      intent (in ) :: diag		! verbosity level, 0=errors only

  real(dp), allocatable, intent (inout) :: in_data(:,:,:)   ! var input data
  real(dp),     intent (out) :: vmiss		! data code for missing value
  integer,      intent (out) :: nstations	! station dimension size
  integer,      intent (out) :: nhours		! hour dimension size
  integer,      intent (out) :: ndays		! date dimension size

! Local variables.

  character table_code*40, varname*200
  integer j, status, varid,point_hours,point_stat,point_days,dim_inp(3)
  logical found

! Get the file var name from the code translation table.

  if (diag >= 5) print *, '*** read_var: Start.'

  do j = 1, size (var_table)
    read (var_table(j), *) table_code, varname
    found = (vcode == table_code)
    if (found) exit
  end do
!ste avoinding this check, otherwise a specific vtable must be provided for
!different data-set
!  if (.not. found) then
!    print *, '*** read_point_data: Unknown variable code "' // trim (vcode) &
!      // '".'
!    print *, '*** Abort.'
!    call exit (1)
!  end if

  if (diag >= 3) print *, 'Read file variable: ' // vcode(1:len_vcodes+3) &
    // trim (varname)
         varname=vcode(1:len_vcodes)
! Look up this var name in the input file.

  status = (nf90_inq_varid (ncid, varname, varid))

  if (status == nf90_enotvar) then
    print *, '*** read_point_data: Requested variable not found in current' &
      // ' Netcdf file.'
    print *, '*** Requested file variable name  = ' // trim (varname)
    print *, '*** Requested short variable code = ' // trim (vcode)
    print *, '*** Abort.'
    call exit (1)

  else
    call check (status, routine_name)		! returns if status is good
  end if					! otherwise diagnostic and abort

! Read input file dimension sizes, (stations, hours, days).

  if (diag >= 4) print *, 'Read dimension sizes.'
!ste
!  nstations = getnc_dim_size (ncid, varid, 3)   ! station locations
!  nhours    = getnc_dim_size (ncid, varid, 1)   ! forecaset hours
!  ndays     = getnc_dim_size (ncid, varid, 2)   ! forecast dates or cycles
  dim_inp(1)=getnc_dim_size (ncid, varid, 1) 
  dim_inp(2)=getnc_dim_size (ncid, varid, 2) 
  dim_inp(3)=getnc_dim_size (ncid, varid, 3) 
!ste this chane allows for any order in input file dimension
!ste 
  nhours    = dim_inp(point_hours)   ! forecaset hours
  nstations = dim_inp(point_stat)   ! station locations
  ndays     = dim_inp(point_days)   ! forecast dates or cycles
  if (diag >= 4) then
    print *, '  nstations = ', nstations
    print *, '  nhours    = ', nhours
    print *, '  ndays     = ', ndays
  end if

! Allocate input array, if needed.

  if (.not. allocated (in_data)) then
    if (diag >= 4) print *, 'Allocate input array.'
!    allocate (in_data(nstations, nhours, ndays))
    allocate (in_data(dim_inp(1),dim_inp(2) ,dim_inp(3) ))
 
 end if

! Read data array for the current file variable.
! Current version reads the entire time series for each file variable.

! Assumed file dimensions:  (stations, hours, days)

!!    starts = (/ 1, 1, 1 /)		! no need yet for subscripting;
!!					! get_var starts at 1,1,1 by default
  call check (nf90_get_var (ncid, varid, in_data))

! Read the missing value.
  status = nf90_get_att (ncid, varid, 'missing_value', vmiss)

  if (status == nf90_enotatt) &
     status = nf90_get_att (ncid, varid, '_FillValue', vmiss)
!
  if (status == nf90_enotatt) then
    print *, '*** read_point_data: No missing_value or _FillValue attribute' &
      // ' on data variable.'
    print *, '*** Requested file variable name  = ' // trim (varname)
    print *, '*** Requested short variable code = ' // trim (vcode)
!    print *, '*** VMISS set =-9999.0'
!!ste raw files  provided Xcel haven't missing data code
    call exit (1)
!U    vmiss=-9999.0
  else
    call check (status, routine_name)		! returns if status is good
  end if					! otherwise diagnostic and abort

  if (diag >= 5) print *, '*** read_var: Return.'

end subroutine read_var

! ===== HSoh: for plotting =========
! added by WC: 2015-07-07

subroutine read_dim_int_var (ncid, dcode, len_dcodes, diag, in_data, &
    vmiss, ndim)
  implicit none

  integer,      intent (in ) :: ncid		! current netcdf file ID
  character(*), intent (in ) :: dcode		! short code for requested var
  integer,      intent (in ) :: len_dcodes	! display width for var codes
  integer,      intent (in ) :: diag		! verbosity level, 0=errors only

  integer, allocatable, intent (inout) :: in_data(:)   ! var input data
  real(dp),     intent (out) :: vmiss		! data code for missing value
  integer,      intent (out) :: ndim		!  dimension size

! Local variables.

  character varname*200
  integer status, varid

  if (diag >= 5) print *, '*** read_dim_int_var: Start.'

  varname=dcode(1:len_dcodes)
  if (diag >= 3) print *, 'Read dimension: ' // trim (varname)

  status = (nf90_inq_varid (ncid, varname, varid))

  if (status == nf90_enotvar) then
    print *, '*** read_point_data: Requested variable not found in current' &
      // ' Netcdf file.'
    print *, '*** Requested file variable name  = ' // trim (varname)
    return

  else
    call check (status, routine_name)		! returns if status is good
  end if					! otherwise diagnostic and abort

! Read input file dimension sizes, (stations, hours, days).

  if (diag >= 4) print *, 'Read dimension sizes.'

  ndim=getnc_dim_size (ncid, varid, 1) 
  if (diag >= 4) then
    print *, '  dim = ', ndim
  end if

! Allocate input array, if needed.

  if (.not. allocated (in_data)) then
    if (diag >= 4) print *, 'Allocate int dimension array.'
    allocate (in_data(ndim))
  end if

! Read data array for the current file variable.

  call check (nf90_get_var (ncid, varid, in_data))

! Read the missing value.
  status = nf90_get_att (ncid, varid, 'missing_value', vmiss)

  if (status == nf90_enotatt) &
     status = nf90_get_att (ncid, varid, '_FillValue', vmiss)

  if (status == nf90_enotatt) then
    print *, '*** Requested file variable name  = ' // trim (varname)
  else
    call check (status, routine_name)		! returns if status is good
  end if					! otherwise diagnostic and abort

!  if (diag >= 5) print *, '*** read_dim_int_var: Return.'

end subroutine read_dim_int_var

subroutine read_dim_str_var (ncid, dcode, len_dcodes, diag, in_data, &
    vmiss, ndim)
  implicit none

  integer,      intent (in ) :: ncid		! current netcdf file ID
  character(*), intent (in ) :: dcode		! short code for requested var
  integer,      intent (in ) :: len_dcodes	! display width for var codes
  integer,      intent (in ) :: diag		! verbosity level, 0=errors only

  character, allocatable, intent (inout) :: in_data(:,:)   ! var input data
  real(dp),     intent (out) :: vmiss		! data code for missing value
  integer,      intent (out) :: ndim(2)	! dimension size

! Local variables.

  character varname*200
  integer status, varid

  if (diag >= 5) print *, '*** read_dim_str_var: Start.'
  varname=dcode(1:len_dcodes)
  if (diag >= 3) print *, 'Read dimension: ' // trim (varname)

  status = (nf90_inq_varid (ncid, varname, varid))

  if (status == nf90_enotvar) then
    print *, '*** read_point_data: Requested dimension variable' &
      // ' not found in current Netcdf file.'
    print *, '*** Requested file variable name  = ' // trim (varname)
    return
  else
    call check (status, routine_name)		! returns if status is good
  end if					! otherwise diagnostic and abort

! Read input file dimension sizes, (stations, hours, days).

  if (diag >= 4) print *, 'Read dimension sizes.'
  ndim(1)=getnc_dim_size (ncid, varid, 1) 
  ndim(2)=getnc_dim_size (ncid, varid, 2) 
  if (diag >= 4) then
    print *, '  dim(1),dim(2) = ', ndim(1), ndim(2)
  end if

! Allocate input array, if needed.

  if (.not. allocated (in_data)) then
    if (diag >= 4) print *, 'Allocate char dimension array.'
    allocate (in_data(ndim(1), ndim(2)))
  end if

! Read data array for the current file variable.

  call check (nf90_get_var (ncid, varid, in_data, count=(/ ndim(1), ndim(2) /)))
  if (diag >= 4) print *, 'after nf90_get_var: ', in_data
  
! Read the missing value.
  status = nf90_get_att (ncid, varid, 'missing_value', vmiss)

  if (status == nf90_enotatt) &
     status = nf90_get_att (ncid, varid, '_FillValue', vmiss)

!  if (diag >= 5) print *, '*** read_dim_str_var: Return.'
  
end subroutine read_dim_str_var

subroutine allocate_2d_string (data_holder, nx, ny)
  implicit none
  
  integer,      intent (in ) :: nx, ny		!
  character, dimension(:,:), allocatable, intent (out) :: data_holder		! short code for requested var

! Local variables.

  if (.not. allocated (data_holder)) then
    print *, 'Allocate 2d dimension (character) array. ny: ', ny, " nx: ", nx
    allocate (data_holder(1:ny, 1:nx))
    
  end if

end subroutine allocate_2d_string

! ===================================
