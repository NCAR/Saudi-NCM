!===============================================================================
! CVS: $Id: WRF_kinds.f90,v 1.1 2008/06/09 17:51:31 deirdre Exp $
! CVS: $Source: /cvs/apps/netcdf_utils/src/WRF_to_point_data/WRF_kinds.f90,v $
! CVS: $Name:  $
!===============================================================================
!BOP ===================================================================
!
! !MODULE: WRF_kinds - defines kinds for variable declaration
!
! !DESCRIPTION:
!
!    Defines variable precision for all common data types.
!
! !REVISION HISTORY:
!
! 2005-Aug-26 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

MODULE WRF_kinds

   implicit none
   save

   integer,parameter,public :: R8 = selected_real_kind(12) ! 8 byte real
   integer,parameter,public :: R4 = selected_real_kind( 6) ! 4 byte real
   integer,parameter,public :: INT= kind(1)                ! native integer
   integer,parameter,public :: char_long = 256             ! long char
   integer,parameter,public :: char_short= 80              ! short char
   integer,parameter,public :: char_date = 19              ! date length

END MODULE WRF_kinds
