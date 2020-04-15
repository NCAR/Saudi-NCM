!-----------------------------------------------------------------------------
!
! getnc_dim_size -- Get size of a particular dimension of a Netcdf variable.
!
! This is a generic Netcdf helper routine.
!
! 2012-jun-15	Original version.  By Dave Allured.
! 2012-jul-23	Remove "use netcdf_check", assume access within module.
!
! Notes:
!
! All of the specified Netcdf components must exist: the file ID,
! the var ID, and the dimension number.  Any mismatch will cause
! the program to stop with a diagnostic message.
!
!-----------------------------------------------------------------------------

function getnc_dim_size (ncid, varid, dim_num) result (dim_size)

   use netcdf
   implicit none

   integer, intent (in) :: ncid			! netcdf file ID
   integer, intent (in) :: varid		! variable ID
   integer, intent (in) :: dim_num		! Fortran dim number (1, 2 etc.)

   integer :: dim_size				! result var: output dim size

! Local variables.

   integer ndims
   integer, allocatable :: dimids(:)

! Found coordinate variable.  Get dimension ID and dim size.

! Get the number of dimensions in the requested variable.

   call check (nf90_inquire_variable (ncid, varid, ndims=ndims))
   					! various Netcdf errors may be detected

   if (dim_num < 1 .or. dim_num > ndims) then
      print *, '*** getnc_dim_size: Abort: Invalid dimension number.'
      write (*, '(a,i0)') ' *** Number of dimensions on this var = ', ndims
      write (*, '(a,i0)') ' *** Requested dimension number       = ', dim_num
      write (*, '(a,i0)') ' *** Netcdf ID   = ', ncid
      write (*, '(a,i0)') ' *** Variable ID = ', varid
      call exit (1)
   end if

! Get the dimension ID's for this variable.  Must read all of them.

   allocate (dimids(ndims))
   call check (nf90_inquire_variable (ncid, varid, dimids=dimids))

! Now get the size of the specified dimension, and return it to caller.

   call check (nf90_inquire_dimension (ncid, dimids(dim_num), len=dim_size))

end function getnc_dim_size
