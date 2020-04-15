!-----------------------------------------------------------------------------
!
! permute -- Simple permute routine to reorder the dimensions of an array.
!
! Like the F90 transpose intrinsic, except for higher than two dimensions.
! Like Matlab's permute function.
!
! 2014-feb-20	Original function version.  By Dave Allured, NOAA/CIRES/PSD.
!		Only 3-D arrays are supported in this initial version.
!		Abandoned, dangerous because functional construct lacks
!		  proper bounds checking.
! 2014-feb-20	Re-cast as subroutine, not function, to obtain proper
!		  bounds checking for the output array.  Confirmed.
!
! This is a convenience routine wrapped around the F90 reshape
! function.  The purpose is to improve readability of user code,
! and reduce mistakes, by hiding some of the warty syntax and
! configuration details.
!
! The "order" argument has exactly the same meaning as the same
! argument of the original reshape function.
!
! Example pseudocode:
!
! call permute (x(a,b,c), y, (/ 3,2,1 /))
! or
! call permute (x(a,b,c), y, order=(/ 3,2,1 /))
!
! Result is y(c,b,a).
!
!-----------------------------------------------------------------------------

module permute_mod
  implicit none

  private			! standard visibility
  public permute,permute_4d 		! only the generic entry point is accessible

  integer, parameter :: dp = kind (1.d0)	! default double precision

contains

subroutine permute (in, out, order)
  implicit none

  real(dp), intent (in ) :: in(:,:,:)		! input array
  integer,  intent (in ) :: order(:)		! dimension permutation vector

  real(dp), intent (out) :: out(:,:,:)		! output array, pre-allocated

  integer old_dims(size(shape(in))), new_dims(size(shape(in)))

  old_dims = shape (in)		! dim sizes in original order
  new_dims = old_dims(order) 	! vector subscripting, permute the dim sizes

  out = reshape (in, new_dims, order=order)	! now permute the data

end subroutine permute

subroutine permute_4d (in, out, order)
  implicit none

  real(dp), intent (in ) :: in(:,:,:,:)         ! input array
  integer,  intent (in ) :: order(:)            ! dimension permutation vector

  real(dp), intent (out) :: out(:,:,:,:)                ! output array,pre-allocated

  integer old_dims(size(shape(in))), new_dims(size(shape(in)))

  old_dims = shape (in)         ! dim sizes in original order
  new_dims = old_dims(order)    ! vector subscripting, permute the dim sizes

  out = reshape (in, new_dims, order=order)     ! now permute the data

end subroutine permute_4d


end module permute_mod
