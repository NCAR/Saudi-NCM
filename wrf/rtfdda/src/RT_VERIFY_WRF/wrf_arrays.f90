  MODULE wrf_arrays

  CONTAINS

  subroutine get_carray(ncid,varid,array_name,array)
!
  use NETCDF

  integer :: ncid,istatus,length,i
  character(len=*) :: array_name
  character , pointer, dimension(:,:) :: array
  integer :: varid,xtype,ndims,natts
  character (len=31) :: vname,dname
  integer, dimension(2) :: dimids,start=(/1,1/)
  integer, allocatable, dimension(:) :: isize

  istatus=nf90_inquire_variable(ncid,varid,vname,xtype,ndims,dimids,natts)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ',array_name

  allocate(isize(ndims))

  do i=1,ndims
     istatus=nf90_inquire_dimension(ncid,dimids(i),dname,length)
     if(istatus /= nf90_noerr) print*,'error inquiring dimension ID ',dimids(i)
     isize(i)=length
  !  print*,'isize(',i,') = ',isize(i)
  end do

  allocate(array(isize(1),isize(2)))

  istatus=nf90_get_var(ncid,varid,array,start,isize)
  if(istatus /= nf90_noerr) print*,'error reading array ',trim(array_name)
  if(istatus /= nf90_noerr) print*,trim(nf90_strerror(istatus))
! print*,'array(1,1,1,1) = ',array(1,1,1,1)


  end subroutine get_carray
!
!
!
  subroutine get_array3d(ncid,varid,array_name,array)
!
  use NETCDF

  integer :: ncid,istatus,length,i
  character(len=*) :: array_name
  real, pointer, dimension(:,:,:,:) :: array
  integer :: varid,xtype,ndims,natts
  character (len=31) :: vname,dname
  integer, dimension(4) :: dimids
  integer, allocatable, dimension(:) :: isize

  istatus=nf90_inquire_variable(ncid,varid,vname,xtype,ndims,dimids,natts)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ',array_name

  allocate(isize(ndims))

  do i=1,ndims
     istatus=nf90_inquire_dimension(ncid,dimids(i),dname,length)
     if(istatus /= nf90_noerr) print*,'error inquiring dimension ID ',dimids(i)
     isize(i)=length
  !  print*,'isize(',i,') = ',isize(i)
  end do

  allocate(array(isize(1),isize(2),isize(3),isize(4)))

  istatus=nf90_get_var(ncid,varid,array)
  if(istatus /= nf90_noerr) print*,'error reading arraay ',trim(array_name)
! print*,'array(1,1,1,1) = ',array(1,1,1,1)


  end subroutine get_array3d
!!!
  subroutine get_array2d(ncid,varid,array_name,array)
!
  use NETCDF

  integer :: ncid,istatus,length,i
  character(len=*) :: array_name
  real, pointer, dimension(:,:,:) :: array
  integer :: varid,xtype,ndims,natts
  character (len=31) :: vname,dname
  integer, dimension(3) :: dimids
  integer, allocatable, dimension(:) :: isize

  istatus=nf90_inquire_variable(ncid,varid,vname,xtype,ndims,dimids,natts)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ',array_name

  allocate(isize(ndims))

  do i=1,ndims
     istatus=nf90_inquire_dimension(ncid,dimids(i),dname,length)
     if(istatus /= nf90_noerr) print*,'error inquiring dimension ID ',dimids(i)
     isize(i)=length
  !  print*,'isize(',i,') = ',isize(i)
  end do

  allocate(array(isize(1),isize(2),isize(3)))

  istatus=nf90_get_var(ncid,varid,array)
  if(istatus /= nf90_noerr) print*,'error reading arraay U'
! print*,'array(1,1,1) = ',array(1,1,1)


  end subroutine get_array2d
!
!
!
  subroutine get_array1d(ncid,varid,array_name,array)
!
  use NETCDF

  integer :: ncid,istatus,length,i
  character(len=*) :: array_name
  real, pointer, dimension(:,:) :: array
  integer :: varid,xtype,ndims,natts
  character (len=31) :: vname,dname
  integer, dimension(2) :: dimids
  integer, allocatable, dimension(:) :: isize
  integer, dimension(2) :: count
  integer, dimension(2) :: start=(/1,1/)

  istatus=nf90_inquire_variable(ncid,varid,vname,xtype,ndims,dimids,natts)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ',array_name

  allocate(isize(ndims))

  do i=1,ndims
     istatus=nf90_inquire_dimension(ncid,dimids(i),dname,length)
     if(istatus /= nf90_noerr) print*,'error inquiring dimension ID ',dimids(i)
     isize(i)=length
  !  print*,'isize(',i,') = ',isize(i)
  end do

  allocate(array(isize(1),isize(2)))

  istatus=nf90_get_var(ncid,varid,array)
  if(istatus /= nf90_noerr) print*,'error reading arraay ',trim(array_name)
! print*,'array(1,1) = ',array(1,1)


  end subroutine get_array1d
!
!
!
  subroutine get_array0d(ncid,varid,array_name,array)
!
  use NETCDF

  integer :: ncid,istatus,length,i
  character(len=*) :: array_name
  real, pointer, dimension(:) :: array
  integer :: varid,xtype,ndims,natts
  character (len=31) :: vname,dname
  integer, dimension(1) :: dimids
  integer, allocatable, dimension(:) :: isize
  integer :: count
  integer :: start=1

  istatus=nf90_inquire_variable(ncid,varid,vname,xtype,ndims,dimids,natts)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ',array_name

  allocate(isize(ndims))

  istatus=nf90_inquire_dimension(ncid,dimids(1),dname,length)
  if(istatus /= nf90_noerr) print*,'error inquiring dimension ID ',dimids(i)
  isize(1)=length
  !  print*,'isize(',i,') = ',isize(i)

  allocate(array(isize(1)))

  istatus=nf90_get_var(ncid,varid,array)
  if(istatus /= nf90_noerr) print*,'error reading arraay ',trim(array_name)
! print*,'array(1,1) = ',array(1,1)


  end subroutine get_array0d

  END MODULE wrf_arrays
