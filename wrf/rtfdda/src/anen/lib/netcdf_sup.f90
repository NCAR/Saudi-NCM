!-----------------------------------------------------------------------------
!
! netcdf_sup.f90 -- Supplemental routines for Netcdf Fortran 90 interface.
!
! By Dave Allured, NOAA/OAR/ESRL/PSD/CAB, CU/CIRES/CDC.
!
! Ver	Date		Notes
! 1.00	2012-mar-27	Original version for module container.
!			By Dave Allured, NOAA/PSD/CIRES.
!			Adapted from netcdf_fix.f90 v1.00.
! 1.01	2012-mar-28	Add remaining support for scalar through 5-D,
!			  to function nf90_get_var_trim.
! 1.02	2012-jun-15	Add getnc_dim_size.
! 1.03	2012-jul-23	Add check (netcdf_check.f90 v2.00).
!			Define transitional, temporary modules netcdf__check
!			  and netcdf_fix, and external procedure check1.
!
! 1.04	2014-apr-09	Clean code transition.  Remove external check1.f90.
!			Upgrade calling programs to fix missing check symbols.
!
! Notes:
!
! This file netcdf_sup.f90 is the module container for individual
! support routines.  The supplemental routines are combined into
! a single F90 module, using the simple include method.
!
! This module supports all Netcdf 3 and 4 versions that have
! a Fortran 90 API, currently 3.5.0 through 4.2.
!
!-----------------------------------------------------------------------------

module netcdf_sup
   implicit none

   private		! visibility controls; all private except as declared

   public check, getnc_dim_size
   public nf90_get_att_trim, nf90_get_var_trim

! Generic interfaces.

   interface nf90_get_var_trim
      module procedure getvar_scalar_trim, getvar_1d_trim, getvar_2d_trim
      module procedure getvar_3d_trim,     getvar_4d_trim, getvar_5d_trim
   end interface nf90_get_var_trim

! Include section for module routines.

contains

   include 'getnc_dim_size.f90'
   include 'netcdf_check2.f90'		! temporary name to avoid conflict
   					! with netcdf_check.f90 v1.00.
   include 'nf90_get_att_trim.f90'
   include 'nf90_get_var_trim.f90'

end module netcdf_sup

!---------------------------------------------------------------
! Transitional module definitions, for backward compatibility.
!---------------------------------------------------------------

module netcdf__check
   use netcdf_sup, only : check
end module netcdf__check

module netcdf_fix
   use netcdf_sup, only : nf90_get_att_trim
end module netcdf_fix
