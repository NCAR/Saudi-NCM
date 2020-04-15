!-----------------------------------------------------------------------------
!
! nf90_get_att_trim -- Same as nf90_get_att for string attributes, except
!		       the gratuitous trailing zero byte is stripped.
!		       Also, the output string is protected from overflow.
!
! 1.00	2008-dec-22	Original module version, file name netcdf_fix.f90.
!			By Dave Allured.
!			Prevent right hand garbage chars with Netcdf 3.6.1
!			  and earlier.
!			Also, add overflow protection for the output string.
!
! 1.01	2012-mar-27	Change file name to nf90_get_att_trim.f90.
!			Remove module container.
!			This version to be include'ed in a module container
!			  such as netcdf_sup.f90.
!			Calling programs need to change from "use netcdf_fix"
!			  to the container module such as "use netcdf_sup".
!			No code changes to the actual function.
!
! This version is written for all Netcdf 3 and 4 versions that have
! a Fortran 90 API, currently 3.5.0 through 4.2.
!
!-----------------------------------------------------------------------------
!
! This is a convenience function to allow calling programs to read
! Netcdf string attributes without having to explicitly get the
! length, allocate memory, and strip a trailing zero byte each time.
!
! Fixed length output strings may be used, but they must be long
! enough to hold the result value plus one byte.  If the output
! string is not long enough, an error status will be returned to the
! caller, and the output string will be invalid.
!
! The calling sequence is identical to the standard API function
! nf90_get_att.  See Netcdf documentation.
!
! A single trailing zero byte, the C string terminator, is stripped.
! Zero bytes are NOT stripped in either of these unusual cases:
!
! * There is more than one zero byte in the original string, and it
!   is not the last one.  This implies a complex, binary encoded, or
!   malformed string.
!
! * No zero byte is present in the original string returned by
!   nf90_get_att.
!
! Note:  Theoretically it may be possible to have a Netcdf attribute
! that contains trailing blanks or trailing nulls.  In those cases,
! use of this function will cause loss of information about the
! original string length.  If preserving this is what you really
! want, then just use the original nf90_get_att function.
!
! In practice, these are both degenerate cases, i.e. malformed
! attributes, that are not encountered in normal exchange of Netcdf
! files.  IMO these cases are bad coding practice.
!
! I suppose that this convenience function could be enhanced to
! detect these weird and rare cases, and leave the trailing zero
! intact in the presence of weirdness.  This would be layering
! perversity on perversity, so I'm not going to do it right now.
!
! This routine also compensates for an old problem.  In Netcdf 3.6.1
! and earlier, calling with an oversize output string resulted in
! residual garbage characters in the right side of the output string.
! This routine always blanks out any excess characters to the right.
!
! This routine also protects the caller from a memory fault when
! the output string is too short.  In this case, the standard Netcdf
! function would overwrite memory past the end of the output string.
!
!-----------------------------------------------------------------------------

function nf90_get_att_trim (ncid, varid, name, value) result (status)
   use netcdf
   implicit none

   integer                    :: status		! returned Netcdf status code

   integer,      intent (in ) :: ncid		! netcdf file ID
   integer,      intent (in ) :: varid		! netcdf variable ID
   character(*), intent (in ) :: name		! specify attribute name
   character(*), intent (out) :: value		! returned attrib value

   integer attlen, i				! local vars
   logical right_to_left

! Check for output string too short.

   status = nf90_inquire_attribute (ncid, varid, name, len=attlen)

   if (status /= nf90_noerr) return	! return immediately if Netcdf error
   					! (missing attribute, etc.)

!!   print *, 'att len for "', trim (name), '" = ', attlen
						! **** TEST ONLY ****

   if (attlen > len (value)) then	! return error if string too short
      status = nf90_ests		! Netcdf code = "string too short"
      return
   end if

! Length okay.  Read attribute value.

   value = ' '				! clear garbage on right, if oversize

   status = nf90_get_att (ncid, varid, name, value)   ! read string attribute
   					! returns only ATTLEN number of chars.

   if (status /= nf90_noerr) return	! return immediately if Netcdf error

   right_to_left = .true.
   i = index (value, char(0), right_to_left)   ! find *last* zero byte in string

   if (i > 0) value(i:i) = ' '		! if present: overwrite with a space,
					! add to normal blank padding
end function nf90_get_att_trim
