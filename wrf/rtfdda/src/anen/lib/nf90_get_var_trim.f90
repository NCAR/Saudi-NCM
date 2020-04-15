!-----------------------------------------------------------------------------
!
! nf90_get_var_trim -- Same as nf90_get_var for character variables, except
!		       remove trailing zero bytes.
!
! 1.00	2012-mar-27	Original version.  By Dave Allured.
!			Support 1-D and 2-D cases only.
! 1.01	2012-mar-28	Handle intermixed trailing zeros and blanks, as
!			  specified in Netcdf Best Practices.
!			Add remaining support for scalar through 5-D.
!
! This version is written for all Netcdf 3 and 4 versions that have
! a Fortran 90 API, currently 3.5.0 through 4.2.
!
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!
! Following are the specific rank cases for nf90_get_var_trim.
! All are for type character only.
!
! These rank cases are combined into the single generic function
! nf90_get_var_trim by the generic interface in netcdf_sup.f90.
!
!-----------------------------------------------------------------------------

function getvar_scalar_trim (ncid, varid, values, start, count, stride, map) &
   result (status)
   use netcdf
   implicit none
  
   integer,           intent (in ) :: ncid, varid
   character(*),      intent (out) :: values
   integer, optional, intent (in ) :: start(:), count(:), stride(:), map(:)
   integer                         :: status
   
   status = nf90_get_var (ncid, varid, values, start, count, stride, map)
   if (status /= nf90_noerr) return	! return any error status to caller
   
   call fix_trailing_nulls (values, 1)

end function getvar_scalar_trim

!-----------------------------------------------------------------------------

function getvar_1d_trim (ncid, varid, values, start, count, stride, map) &
   result (status)
   use netcdf
   implicit none
  
   integer,           intent (in ) :: ncid, varid
   character(*),      intent (out) :: values(:)
   integer, optional, intent (in ) :: start(:), count(:), stride(:), map(:)
   integer                         :: status
   
   status = nf90_get_var (ncid, varid, values, start, count, stride, map)
   if (status /= nf90_noerr) return
   
   call fix_trailing_nulls (values, size (values))

end function getvar_1d_trim

!-----------------------------------------------------------------------------

function getvar_2d_trim (ncid, varid, values, start, count, stride, map) &
   result (status)
   use netcdf
   implicit none
  
   integer,           intent (in ) :: ncid, varid
   character(*),      intent (out) :: values(:,:)
   integer, optional, intent (in ) :: start(:), count(:), stride(:), map(:)
   integer                         :: status
   
   status = nf90_get_var (ncid, varid, values, start, count, stride, map)
   if (status /= nf90_noerr) return
   
   call fix_trailing_nulls (values, size (values))

end function getvar_2d_trim

!-----------------------------------------------------------------------------

function getvar_3d_trim (ncid, varid, values, start, count, stride, map) &
   result (status)
   use netcdf
   implicit none
  
   integer,           intent (in ) :: ncid, varid
   character(*),      intent (out) :: values(:,:,:)
   integer, optional, intent (in ) :: start(:), count(:), stride(:), map(:)
   integer                         :: status
   
   status = nf90_get_var (ncid, varid, values, start, count, stride, map)
   if (status /= nf90_noerr) return
   
   call fix_trailing_nulls (values, size (values))

end function getvar_3d_trim

!-----------------------------------------------------------------------------

function getvar_4d_trim (ncid, varid, values, start, count, stride, map) &
   result (status)
   use netcdf
   implicit none
  
   integer,           intent (in ) :: ncid, varid
   character(*),      intent (out) :: values(:,:,:,:)
   integer, optional, intent (in ) :: start(:), count(:), stride(:), map(:)
   integer                         :: status
   
   status = nf90_get_var (ncid, varid, values, start, count, stride, map)
   if (status /= nf90_noerr) return
   
   call fix_trailing_nulls (values, size (values))

end function getvar_4d_trim

!-----------------------------------------------------------------------------

function getvar_5d_trim (ncid, varid, values, start, count, stride, map) &
   result (status)
   use netcdf
   implicit none
  
   integer,           intent (in ) :: ncid, varid
   character(*),      intent (out) :: values(:,:,:,:,:)
   integer, optional, intent (in ) :: start(:), count(:), stride(:), map(:)
   integer                         :: status
   
   status = nf90_get_var (ncid, varid, values, start, count, stride, map)
   if (status /= nf90_noerr) return
   
   call fix_trailing_nulls (values, size (values))

end function getvar_5d_trim

!-----------------------------------------------------------------------------
!
! fix_trailing_nulls -- Generic procedure to remove trailing zero bytes
!                       from character arrays of any dimensionality.
!
! This is an internal support routine to be used only by
! getvar_*_trim procedures for string processing.
!
! This routine uses an assumed size array to generically process
! character arrays of any dimensionality.
!
!-----------------------------------------------------------------------------

subroutine fix_trailing_nulls (strs, nstrings)
   implicit none

   character(*), intent (inout) :: strs(*)
   integer,      intent (in   ) :: nstrings
   
   integer i, j				     ! local vars
   
   do i = 1, nstrings			     ! process each string individually
      do j = len_trim (strs(i)), 1, -1	     ! len_trim optimizes commom cases
         if (strs(i)(j:j) == ' ') cycle	     ! skip intermixed blanks
         if (strs(i)(j:j) /= char(0)) exit   ! done if other than zero or blank
         strs(i)(j:j) = ' '		     ! overwrite each zero with blank
      end do
   end do

end subroutine fix_trailing_nulls
