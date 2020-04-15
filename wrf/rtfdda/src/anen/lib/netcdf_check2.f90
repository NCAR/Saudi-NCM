!-----------------------------------------------------------------------------
!
! check -- Universal Netcdf error checker (F90).  Abort on error.
!
! 1.00	2009-jan-26	Original module version.  By Dave Allured.
! 2.00	2012-jul-20	Non-module version to be included in netcdf_sup,
!			  or other program or wrapper module.
!			Add optional argument for location ID string.
!
! This subroutine is used with the Netcdf Fortran 90 libraries to
! handle function return codes within a single program line.
!
! Usage:
!
!    use netcdf_sup   (or other wrapper module)
!    ...
!    call check (nf90_function (args) [, location] )
!
! This routine considers all error codes to be fatal errors,
! except for nf90_noerr, which is usually zero.
!
! If the optional location ID string is given, then it is included
! in the error message.  At caller's option, the location string
! could be either the name of the calling routine, or the name of
! the invoked Netcdf library function.
!
!-----------------------------------------------------------------------------

subroutine check (status, location)
   use netcdf
   implicit none

   integer,                intent (in) :: status	! Netcdf return code
   character(*), optional, intent (in) :: location	! location ID string
   
   if (status /= nf90_noerr) then
      print *, '*** Netcdf error number ', status
      print *, '*** ', trim (nf90_strerror (status))
      
      if (present (location)) then
         print *, '*** Error detected in ' // trim (location)
      end if
      
      call exit (99)
   end if
   
end subroutine check
