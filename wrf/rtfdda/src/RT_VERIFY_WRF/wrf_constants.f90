!===============================================================================
! CVS: $Id: wrf_constants.f90,v 1.2 2006/12/14 17:32:47 sheu Exp $
! CVS: $Source: /cvs/apps/4dwx/RTFDDA/src/RT_VERIFY_WRF/wrf_constants.f90,v $
! CVS: $Name: R1-W381-20180202 $
!===============================================================================
!BOP ===================================================================
!
! !MODULE: WRF_constants - defines constants
!
! !DESCRIPTION:
!
! Defines a set of physical and numerical constants 
!
! !REVISION HISTORY:
!
! 2005-Sep-13 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

MODULE WRF_constants

   use WRF_kinds

   implicit none

   public

   !-----------------------------------------------------------------
   ! numerical constants
   !-----------------------------------------------------------------
   real(R8),parameter :: pi      = 3.14159265358979323846_R8 
   real(R8),parameter :: c0      = 0.0_R8    
   real(R8),parameter :: half    = 0.5_R8   
   real(R8),parameter :: c1      = 1.0_R8  
   real(R8),parameter :: c2      = 2.0_R8 
   real(R8),parameter :: c4      = 4.0_R8     
   real(R8),parameter :: c8      = 8.0_R8     
   real(R8),parameter :: c10     = 10.0_R8     
   real(R8),parameter :: c100    = 100.0_R8   
   real(R8),parameter :: c180    = 180.0_R8   
   real(R8),parameter :: c360    = 360.0_R8  
   real(R8),parameter :: missing = -99.0_R8 
   real(R8),parameter :: bad_data= -9999.0_R8 

   !-----------------------------------------------------------------
   ! physical constants
   !-----------------------------------------------------------------
   real(R8),parameter :: sec_per_day = 86400.0_R8   ! seconds in calendar day
   real(R8),parameter :: pa_to_mb    = 0.01_R8      ! Convert Pa to mb
   real(R8),parameter :: g0          = 9.80665_R8   ! Acceleration due to gravity
   real(R8),parameter :: LapseRate   = 0.0065       ! deg/m
   real(R8),parameter :: Rd          = 287._R8      ! Gas constant for dry air
   real(R8),parameter :: scale_ht    = Rd*256._R8/g0! Scale height
   real(R8),parameter :: T0          = 300._R8      ! WRF reference temperature
   real(R8),parameter :: rho0        = 1.275_R8     ! Density of dry air
   real(R8),parameter :: P00         = 1000._R8     ! Reference pressure
   real(R8),parameter :: Rv          = 461.5_R8     ! Gas constant for H20 vapor
   real(R8),parameter :: cp          = 1004._R8     ! Specific heat at const p
   real(R8),parameter :: LHeat       = 2.5e6        ! Latent heat of vaporization
   real(R8),parameter :: Rmin        = 6356752.0_R8 ! Earth's polar radius (m)
   real(R8),parameter :: Rmax        = 6378137.0_R8 ! Earth's equatorial radius (m)

END MODULE WRF_constants
