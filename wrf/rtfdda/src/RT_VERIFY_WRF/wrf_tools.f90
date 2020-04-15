!=======================================================================
! CVS: $Id: wrf_tools.f90,v 1.3 2007/10/12 17:34:09 sheu Exp $
! CVS: $Source: /cvs/apps/4dwx/RTFDDA/src/RT_VERIFY_WRF/wrf_tools.f90,v $
! CVS: $Name: R1-W381-20180202 $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_tools - Collection of commonly used subroutines
!
! !DESCRIPTION:
!
! !REVISION HISTORY:
!
! 2005-Sep-9 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

module WRF_tools

! !USES:

   use WRF_kinds
   use WRF_constants
   use WRF_ncread

!EOP

   implicit none

   private    ! except

! !PUBLIC TYPES:

   ! none

! !PUBLIC MEMBER FUNCTIONS:

   public :: lltoxy_generic   ! Convert lat/lon info to float (i,j)
   public :: xytoll_generic   ! Convert float (i,j) to lat/lon
   public :: nearest_k        ! Find nearest vertical point
   public :: four_point       ! Four point interpolation
   public :: interp_1d        ! 1D vertical interpolation
   public :: rotate_wind      ! rotate wind vectors
   public :: wind_dir_speed   ! Calculate wind speed and direction
   public :: lccone
!  public :: getTimeIndex     ! Get index of desired time from netCDF
!  public :: rhcalc           ! Function to calculate relative humidity
!  public :: unstagger        ! Average staggered fields to unstaggered grid

! !PUBLIC DATA MEMBERS:

   ! none

!EOP

   logical, parameter :: debug = .false. ! Set to .true. to turn on write
                                         ! statements in this module
!===============================================================================
contains
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: nearest_k -- Return index of closest vertical point
!
! !DESCRIPTION:  
!    Given a column of vertical coordinates at obs point, and the vertical
!    location of the obs point, uses bisection to return an index k_found such
!    that zObsPt lies between VCoord(k_found) and VCoord(k_found+1).  VCoord
!    can be monotonically increasing or decreasing.  
!
! !REVISION HISTORY:
!     2005-Sep-6 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine nearest_k(VCoord, zObsPt, k_found, statusFlag, statusInfo)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   real(R8)    , intent(in) :: VCoord(:)     ! Column at the obs point
   real(R8)    , intent(in) :: zObsPt        ! Obs vertical coordinate
   integer(INT),intent(out) :: k_found       ! k index closest to obs point
   integer(INT),intent(out) :: statusFlag    ! Status: 0=good, all other bad
   character(*),intent(out) :: statusInfo    ! Status information

!EOP

   !----- local -----
   integer(INT) :: k               ! loop indices
   integer(INT) :: nk_model        ! number of points in vertical dimension
   integer(INT) :: k_lower         ! lower limit
   integer(INT) :: k_upper         ! upper limit
   integer(INT) :: k_midpt         ! midpointa between upper and lower

   !----- formats -----
   character(*),parameter :: subName = "(nearest_k)"
   character(*),parameter :: F00     = "('(nearest_k) ',2a)"
   character(*),parameter :: F01     = "('(nearest_k) ',a,i6)"
   character(*),parameter :: F02     = "('(nearest_k) ',a,2f14.5)"

   nk_model = size(VCoord)      ! # of vertical indices
   k_found = 0
   statusFlag = 0

   k_lower = 0                  ! Initialize lower and upper limits
   k_upper = nk_model+1

10 if (k_upper - k_lower > 1) then
      k_midpt = (k_upper + k_lower)/2
      if ((VCoord(nk_model) >= VCoord(1)).eqv.(zObsPt >= VCoord(k_midpt))) then
         k_lower = k_midpt
      else
         k_upper = k_midpt
      endif
      goto 10
   endif

   if (zObsPt == VCoord(1)) then
      k_found = 1
   else if (zObsPt == VCoord(nk_model)) then
      k_found = nk_model - 1
   else
      k_found = k_lower
   endif

   if (debug) then
      write(6,F01)'k_found=',k_found 
      write(6,F02)'zObsPt =',zObsPt
      write(6,F02)'VCoord(k_found)  =', VCoord(k_found),VCoord(k_found+1)
   endif

   !---------------------------------------------------------------------------
   ! Check to see if point is outside of model domain
   !---------------------------------------------------------------------------
   if (k_found == 0 .or. k_found == nk_model) then
      statusFlag = 1
      statusInfo = subName// " Point is not in the *VERTICAL* model domain" 
      return
   endif

end subroutine nearest_k
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: getTimeIndex -- Find time in netCDF file and return index
!
! !DESCRIPTION:  
!   Given netCDF file name and desired date/time character string, return
!   index of matching time in file.
!
! !REVISION HISTORY:
!     2005-Sep-29 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

!subroutine getTimeIndex(file_name, date_time, nTimeInd, statusFlag, statusInfo)

!  implicit none

! !INPUT/OUTPUT PARAMETERS:

!  character(*), intent(in) :: file_name  ! netCDF path and file name
!  character(*), intent(in) :: date_time  ! string with date/time info
!  integer(INT), intent(out):: nTImeInd   ! Index of time variable
!  integer(INT), intent(out):: statusFlag ! Status: 0=good, all other bad
!  character(*), intent(out):: statusInfo ! Status information

!EOP

   !----- local -----
!  integer(INT) :: i                ! loop index
!  integer(INT) :: nTimes          ! value of netCDF Time dimension
!  character(char_date),allocatable :: file_times(:)   ! times read from netCDF file

   !----- formats -----
!  character(*),parameter :: F00 =   "('(getTimeIndex) ',20a)"

!  call get_dimension(trim(file_name), "Time", nTimes)

   !---------------------------------------------------------------------------
   !--- Check time character string variable ---
   !---------------------------------------------------------------------------
!  allocate(file_times(nTimes))
!  call getCharTimes(trim(file_name), "Times", file_times)

   !---------------------------------------------------------------------------
   !--- Verify that desired time is in this file, if so, save dimension ---
   !---------------------------------------------------------------------------
!  statusFlag = 1

!  do i = 1,nTimes
!     if (trim(adjustl(file_times(i))) == trim(adjustl(date_time))) then
!        statusFlag = 0
! nTimeInd = i
!     endif
!  enddo

!  if (statusFlag == 0) then
!     write(6,F00) 'date_time matches time in file ', date_time
!  else
!     statusFlag = 1
!     statusInfo = 'ERROR date_time not found in file '//date_time
!  endif

!  deallocate(file_times)

!end subroutine getTimeIndex

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: interp_1d -- One dimensional interpolation
!
! !DESCRIPTION:  
!    Given the 2 surrounding z coordinates and the WRF values, and the index just below the
!    observed value, returns a point at zObsPt.
!
! !REVISION HISTORY:
!     2005-Sep-23 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine interp_1d(zValue, zObsPt, column, ptValue)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   real(R8)    ,intent(in) :: zValue(2)  ! Column of height coordinates
   real(R8)    ,intent(in) :: zObsPt     ! Vertical location of obs point
   real(R8)    ,intent(in) :: column(2)  ! WRF data at zValues
   real(R8)    ,intent(out):: ptValue    ! Interpolated value returned

!EOP

   !----- local -----
   real(R8) :: dz           ! Fraction of vertical grid cell
   real(R8) :: dzm          ! 1-dz

   dz =(zValue(2) - zObsPt)/(zValue(2) - zValue(1))
   dzm = c1 - dz

   ptValue = dz*column(1) + dzm*column(2)

end subroutine interp_1d

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: rotate_wind -- Rotate wind vector from model grid to N-S grid
!
! !DESCRIPTION:  
!    Rotate wind vector from model grid to regular N-S grid
!
! !REVISION HISTORY:
!     2005-Sep-12 - J. Schramm - based on module_wrf_to_grads_util.F
!
! !INTERFACE: ------------------------------------------------------------------

subroutine rotate_wind (u, v, map_proj, center_lon, lat, lon,       &
&                       truelat1, truelat2, u_rot, v_rot)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   real(R8)    ,intent(in) :: u, v               ! wind components
   integer(INT),intent(in) :: map_proj           ! integer for type of map proj
   real(R8)    ,intent(in) :: center_lon         ! Center longitude of map proj
   real(R8)    ,intent(in) :: lat, lon           ! Location of u, v
   real(R8)    ,intent(in) :: truelat1,truelat2  ! Global attributes
   real(R8)    ,intent(out):: u_rot, v_rot       ! rotated wind components

!EOP

   !----- local -----
   real(R8)            :: cone                      ! cone factor
   real(R8)            :: diff                      ! center_lon - angle
   real(R8)            :: angle
   real(R8), parameter :: pii = 3.14159265_R8
   real(R8), parameter :: radians_per_degree = pii/180._R8

   cone = c1                                          !  PS
   if( map_proj .eq. 1) then                          !  Lambert Conformal mapping
      IF (dabs(truelat1-truelat2) > 0.1_R8) then
         cone=(dlog(dcos(truelat1*radians_per_degree))-            &
               dlog(dcos(truelat2*radians_per_degree))) /          &
              (dlog(dtan((90._R8-dabs(truelat1))*radians_per_degree*0.5_R8))- &
               dlog(dtan((90._R8-dabs(truelat2))*radians_per_degree*0.5_R8)) )
      else
         cone = dsin(dabs(truelat1)*radians_per_degree )
      endif
   endif

   diff = center_lon - lon

   if (diff >  180.) diff = diff - 360.
   if (diff < -180.) diff = diff + 360.

   if(lat < 0.) then
       angle = - diff * cone * radians_per_degree
   else
       angle =   diff * cone * radians_per_degree
   end if

   u_rot = -v*sin(angle) + u*cos(angle)
   v_rot =  v*cos(angle) + u*sin(angle)

end subroutine rotate_wind
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: wind_dir_speed -- Calculate wind direction and speed
!
! !DESCRIPTION:  
!    Calculate wind direction and speed.  Note:  If v is < 0.0001 it will
!    be returned as zero.
!
! !REVISION HISTORY:
!     2005-Sep-12 - J. Schramm - from uv2ddff.f
!
! !INTERFACE: ------------------------------------------------------------------

subroutine wind_dir_speed(u, v, dir, speed)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   real(R8), intent(in)    :: u        ! Wind component
   real(R8), intent(inout) :: v        ! Wind component
   real(R8), intent(out)   :: dir      ! Wind direction
   real(R8), intent(out)   :: speed    ! Wind speed

!EOP

   !----- local -----

   if (dabs(u) > 300._R8 .or. dabs(v) > 300._R8) then
      speed = missing
      dir   = missing
   else
      speed=dsqrt(u*u + v*v)
      if (dabs(v) > .0001_R8) then
         dir =datan(u/v)
      else
	 v = c0
	 if (u > c0) then
	    dir = -pi/c2
	 else
            dir = pi/c2
	 endif
      endif
      dir = dir*c360/(c2*pi)
      if (v   > c0) dir = dir + c180
      if (dir < c0) dir = dir + c360
   endif

end subroutine wind_dir_speed

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: rhcalc -- Calculate relative humidity
!
! !DESCRIPTION:  
!    Calculate relative humidity given mixing ratio, temperature and pressure. 
!
! !REVISION HISTORY:
!     2005-Sep-12 - J. Schramm - based on rhcalc.F from RT_SITES/src_new
!
! !INTERFACE: ------------------------------------------------------------------
!function rhcalc(t,w,p)

!  implicit none

! !INPUT/OUTPUT PARAMETERS:

!  real(R8), intent(in) :: t    ! Temperataure (K)
!  real(R8), intent(in) :: w    ! Mixing ratio (kg/kg)
!  real(R8), intent(in) :: p    ! Pressure     (mb)

!EOP

   !----- local -----
!  real(R8) :: es         ! Saturation vapor pressure (mb)
!  real(R8) :: e          ! Vapor pressure (mb)
!  real(R8) :: rhcalc     ! Relative humidity 

!  es=10**(-2937.4/t-4.9283*log10(t)+23.5518)  ! in mb
!  if(t < 233. ) es=10**(10.5553-2667./t)      ! saturation over ice
!  e=p*w/(0.622+w)                             ! vapor pressure, e
!  rhcalc=e/es
!  if(rhcalc > c1) rhcalc = c1

!end function rhcalc

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: unstagger -- Unstagger 3d horizontal or vertical WRF fields
!
! !DESCRIPTION:  
!    Move 3d staggered fields to non-staggered grid
!
! !REVISION HISTORY:
!     2005-Sep-22 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------
!subroutine unstagger(path_and_file, staggered, unstaggered)

!  implicit none

! !INPUT/OUTPUT PARAMETERS:

!  character(char_long),intent(in) :: path_and_file       ! path//filename
!  real(R8)            ,intent(in) :: staggered(:,:,:)    ! staggered field(nx,ny,nz)
!  real(R8)            ,intent(out):: unstaggered(:,:,:)  ! unstaggered field

!EOP

   !----- local -----
!  integer(INT) :: nx, ny, nz        ! dimensions of staggered array
!  integer(INT) :: nVert_stag        ! value of netCDF bottom_top_stag dimension
!  integer(INT) :: nWE_stag,nSN_stag ! value of netCDF west_east,south_north stag dimension

!  call get_dimension(path_and_file, "bottom_top_stag" , nVert_stag)
!  call get_dimension(path_and_file, "south_north_stag", nSN_stag)
!  call get_dimension(path_and_file, "west_east_stag"  , nWE_stag)

!  nx = size(staggered(:,1,1))
!  ny = size(staggered(1,:,1))
!  nz = size(staggered(1,1,:))

!  if (nx == nWE_stag) then  
!     unstaggered = half*(staggered(1:nx,:,:) + staggered(2:nx+1,:,:))
!  endif

!  if (ny == nSN_stag) then  
!     unstaggered = half*(staggered(:,1:ny,:) + staggered(:,2:ny+1,:))
!  endif

!  if (nz == nVert_stag) then  
!     unstaggered = half*(staggered(:,:,1:nz) + staggered(:,:,2:nz+1))
!  endif
   
!end subroutine unstagger

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: lltoxy_generic -- Calculate floating indices of a point 
!
! !DESCRIPTION:  
!    Calculates the floating indices of a point given the map projection info
!    This code in in CVS under pat/HRLDAS/HRLDAS_COLLECT_DATA/lib/llxy_generic.F
!    Notes    - Modeled after XYTOLL in the plots.o library  (HRLDAS)
!    JLS  This subroutine returns x and y in units of floating index 
!    JLS  x and y can be passed into function four_point to interpolate
!    JLS  Multiply by DX or DY (WRF global attributes to get distance)
!
! !REVISION HISTORY:
!                   Kevin Manning - first version
!     2006-Mar-23 - J. Schramm 
!
! !INTERFACE: ------------------------------------------------------------------
subroutine lltoxy_generic (xlat, xlon, x, y, project, dskm, reflat, reflon, &
   &                       refx,refy, cenlon, truelat1, truelat2)

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   real(R8), intent(in) :: xlat       ! latitude of the point of interest.
   real(R8), intent(in) :: xlon       ! longitude of the point of interest.
   character(LEN=2),intent(in) :: project    ! projection indicator ("ME", "CE", "LC", "ST")
   real(R8), intent(in) :: dskm       ! grid distance in km
   real(R8), intent(in) :: reflat
   real(R8), intent(in) :: reflon
   real(R8), intent(in) :: refx
   real(R8), intent(in) :: refy
   real(R8), intent(in) :: cenlon     ! Grid ratio with respect to MOAD
   real(R8), intent(in) :: truelat1   ! True latitude 1, closest to equator
   real(R8), intent(in) :: truelat2   ! True latitude 2, closest to pole
   real(R8), intent(out) :: x         ! x location of the given (lat,lon) point
   real(R8), intent(out) :: y         ! y location of the given (lat,lon) point

!EOP
   !----- local -----

   real(R8), parameter :: pi = 3.141592653589793   ! you know!  pi = 180 degrees
   real(R8), parameter :: piovr2 = pi/2.
   real(R8), parameter :: re = 6371.2       ! the radius of the earth in km
   real(R8), parameter :: ce = 2.*pi*re     ! the circumference of the earth in km
   real(R8), parameter :: degrad = pi/180.
   real(R8) :: flat1
   real(R8) :: confac     ! Cone factor
   real(R8) :: rcln              ! center longitude in radians  (local)
   real(R8) :: dj                ! distance from pole to point  (local)
   real(R8) :: di                ! distance from the central
                                 !  meridian to the point        (local)
   real(R8) :: bm                ! calculation variable         (local)
   real(R8) :: rflt, rfln, rlat, rlon, diovrdj, disdjs, ri, rj
   real(R8) :: ct1, st1, tt1, drp
   real(R8) :: pi2
   real(R8) :: londiff
   integer(INT):: isn

  rlat =  xlat * degrad
  rlon =  xlon * degrad
  rcln = cenlon * degrad
  flat1 = truelat1 * degrad
  rflt = reflat * degrad
  rfln = reflon * degrad

  if ((xlon - cenlon) < -180) then
     londiff = ((xlon-cenlon)+360.)*degrad
  else if ((xlon - cenlon) > 180) then
     londiff = ((xlon-cenlon)-360.)*degrad
  else
     londiff = (xlon-cenlon)*degrad
  endif
     

  if (project(1:2) .eq. 'ME') then

     ct1 = re*cos(flat1)
     dj = ct1 * log(tan (0.5*(rlat + piovr2)))
     di = ct1 * (rlon - rfln)
     y = refy +(dj + ct1 * log(cos(rflt)/(1 + sin(rflt))))/dskm
     x = refx + di/dskm

  else if (project(1:2) .eq. 'CE') then

!KWM     dj = re*(rlat-rflt)
!KWM     di = re*(rlon-rfln)
!KWM     y = refy + dj/dskm
!KWM     x = refx + di/dskm
     
     ! NOTE:  Cylindrical Equidistant grid increment expressed in terms 
     ! of thousandths of degrees!
     !  Calculate the distance from the horizontal axis to (J,I)
     dj = xlat-reflat
     di = xlon-reflon
     y = refy + (dj/dskm)
     x = refx + (di/dskm)

  else if (project(1:2) .eq. 'LC') then

     isn = dsign(c1, truelat2)
     pi2 = piovr2*isn

     call lccone(truelat1,truelat2,idint(dsign(c1, truelat2)),confac)
     tt1 = tan((pi2 - flat1)*0.5) ! Tangent Term 1.
     st1 = sin (pi2 - flat1) * re/(confac*dskm)     ! Sine Term 1.
     bm =  tan((pi2 - rlat)*0.5)
     if ((rlon-rcln) > pi) then
        diovrdj = -tan(((rlon-rcln)-2.*pi)*confac)
     elseif ((rlon-rcln) < -pi) then
        diovrdj = -tan(((rlon-rcln)+2.*pi)*confac)
     else
        diovrdj = -tan((rlon-rcln)*confac)
     endif
     disdjs = ( (bm/tt1)**confac * st1)**2
     ! Dj: y distance (km) from pole to given x/y point.
     dj = -sqrt(disdjs/(1+diovrdj*diovrdj))
     ! Di: x distance (km) from central longitude to given x/y point.
     di = dj * diovrdj

     bm = tan((pi2-rflt)*0.5)
     if ((rfln-rcln) > pi) then
        diovrdj = -tan(((rfln-rcln)-2.*pi)*confac)
     else if ((rfln-rcln) < -pi) then
        diovrdj = -tan(((rfln-rcln)+2.*pi)*confac)
     else
        diovrdj = -tan((rfln - rcln)*confac)
     endif
     disdjs = ( (bm/tt1)**confac * st1)**2
     ! Rj: y distance (km) from pole to reference point.
     rj = -sqrt(disdjs/(1+diovrdj*diovrdj))
     ! Ri: x distance (km) from central longitude to reference x/y point.
     ri = rj * diovrdj
     y = refy + isn*(dj - rj)
     x = refx + (di - ri)

  else if (project(1:2) .eq. 'ST') then

     isn = dsign(c1, truelat1)
     pi2 = piovr2*isn

     diovrdj = -tan(rfln-rcln)
     bm = (re/dskm)*tan(0.5*(pi2-rflt))
     disdjs = (bm*(1.0 + cos(pi2-flat1)))**2
     ! RJ:  Distance from pole to reference latitude along the center lon
     rj = -sign(sqrt(disdjs/(1+diovrdj**2)),cos(rfln-rcln))
     ! RI:  Distance from center reference to requested point rlat, rlon.
     ri = rj * diovrdj
     diovrdj = -tan(rlon-rcln)
     bm = (re/dskm)*tan(0.5*(pi2-rlat))
     disdjs = (bm*(1.0 + cos(pi2-flat1)))**2
     dj = -sign(sqrt(disdjs/(1+diovrdj**2)),cos(rlon-rcln))
     di = dj * diovrdj
     y = refy + isn*(dj-rj)
     x = refx + (di-ri)
  endif

end subroutine lltoxy_generic

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: xytoll_generic -- Calculate lat/lon from point indices 
!
! !DESCRIPTION:  
!    Transforms  mesoscale grid point coordinates (x,y) into latitude and
!    longitude coordinates.              
!    This code in in CVS under pat/HRLDAS/HRLDAS_COLLECT_DATA/lib/llxy_generic.F
!
!  On entry - X  and  Y are an ordered pair representing a grid point in  the 
!             mesoscale grid.                                                 
!                                                                             
!  On exit  - XLAT, XLON contain  the latitude and longitude respectively     
!             that resulted from the transformation.                          
!
! !REVISION HISTORY:
!                   Kevin Manning - first version
!     2006-Apr-25 - J. Schramm 
!
! !INTERFACE: ------------------------------------------------------------------

subroutine xytoll_generic (x,y,xlat,xlon,project,dskm,reflat,reflon, &
   &                       refx,refy, cenlon, truelat1,truelat2)

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   real(R8), intent(in) :: x         ! x location of the given (lat,lon) point
   real(R8), intent(in) :: y         ! y location of the given (lat,lon) point
   character(LEN=2),intent(in) :: project    ! projection indicator ("ME", "CE", "LC", "ST")
   real(R8), intent(in) :: dskm       ! grid distance in km
   real(R8), intent(in) :: reflat
   real(R8), intent(in) :: reflon
   real(R8), intent(in) :: refx
   real(R8), intent(in) :: refy
   real(R8), intent(in) :: cenlon     ! Grid ratio with respect to MOAD
   real(R8), intent(in) :: truelat1   ! True latitude 1, closest to equator
   real(R8), intent(in) :: truelat2   ! True latitude 2, closest to pole
   real(R8), intent(out) :: xlat       ! latitude of point (x,y)
   real(R8), intent(out) :: xlon       ! longitude of point (x,y)
!EOP

!--------local----------

! Parameters

   real(R8), parameter :: pi = 3.141592653589793   ! you know!  pi = 180 degrees
   real(R8), parameter :: piovr2 = pi/2.
   real(R8), parameter :: twopi  = pi*2.
   real(R8), parameter :: re = 6371.2       ! the radius of the earth in km
   real(R8), parameter :: degrad = pi/180.

!  Real variables

  real(R8) ::            confac           ! cone factor
  real(R8) ::            rfln             ! reference longitude in radians  (local)
  real(R8) ::            rflt             ! reference latitude in radians   (local)
  real(R8) ::            rcln             ! center longitude in radians  (local)
  real(R8) ::            dj, drp, djj     ! distance from the central
!                                       meridian to the point        (local)
  real(R8) ::            di,dii           ! distance from pole to point  (local)
  real(R8) ::            bm               ! calculation variable         (local)
  real(R8) ::            flat1
  real(R8) ::            ct1, tt1, tt2, pi2
  integer :: isn
!****************************  subroutine begin  *****************************!

  rflt = reflat * degrad
  rfln = reflon * degrad
  rcln = cenlon * degrad
  flat1 = truelat1 * degrad

!  If the projection is mercator ('ME') then ...

  if (project(1:2) .eq. 'ME') then
     di = (x-refx) * dskm
     !  Calculate the distance the point in question is from the pole
     dj = -re * cos(flat1)*log(cos(rflt)/(1 + sin(rflt))) + &
          (y - refy) * dskm
     !  Calculate the latitude desired in radians
     xlat = 2.0 * atan(exp(dj/(re*cos(flat1)))) - piovr2
     !  Calculate the longitude desired in radians
     xlon = rfln + di/(re*cos(flat1))

     !  Convert the calculated lat,lon pair into degrees
     xlat = xlat * 180.0/pi
     xlon = xlon * 180.0/pi


! If the projection is cylindrical equidistant ('CE') then ...
  else if (project(1:2) .eq. 'CE') then
     ! NOTE:  Cylindrical Equidistant grid increment expressed in terms 
     ! of thousandths of degrees!
     di = (x-refx) * dskm
     !  Calculate the distance from the horizontal axis to (J,I)
     dj = (y-refy) * dskm
     !  Determine the shift north-south
     xlat = reflat + dj
     !  Determine the shift east-west
     xlon = reflon + di

! If the projection is lambert conic conformal ('LC') then ...
  else if (project(1:2) .eq. 'LC') then

     isn = dsign(c1, truelat2)
     pi2 = piovr2*isn

     call lccone(truelat1,truelat2,isn,confac)

     tt1 = tan((pi2 - flat1)*0.5)    ! Tangent Term 1.
     tt2 = tan((pi2 - rflt  )*0.5)    ! Tangent Term 2.
     ct1 = -cos(flat1) * re/confac   ! cosine Term 1.

     ! Calculate the (projected) distance from the pole to 
     ! the reference lat/lon.
     drp = ct1 * (tt2/tt1)**confac
     ! Now from the pole to the reference y along the center lon.
     djj = drp * cos((rcln-rfln)*confac)

     ! Now from the pole to the requested y along the center lon.
     dj = djj + isn * ((y-refy)*dskm)
     ! Now the (projected) distance from center longitude to reference x
     dii = drp*sin((rcln-rfln)*confac)
     ! And now from center longitude to requested X
     di = dii + ((x-refx)*dskm)
     !  Calculate the Big Messy equation
     bm = tt1 * (sqrt(di**2+dj**2) /  abs(ct1))**(1.0/confac)
     !  Calculate the desired latitude in radians
     xlat = pi2 - 2.0*atan(bm)
     !  Calculate the desired longitude in radians
     xlon = rcln + (1.0/confac) * atan2(di,-dj)
     !  Convert the calculated lat,lon pair into degrees
     xlat = xlat * 180.0/pi
     xlon = xlon * 180.0/pi


!  If the projection is polar stereographic ('ST') then ...

  else if (project(1:2) .eq. 'ST') then

     isn = dsign(c1, truelat1)
     pi2 = piovr2*isn


!  Calculate the (projected) y-distance I,J lies from the pole

     drp = -re*cos(rflt) * (1.0 + cos(pi2-flat1)) / (1.0 + cos(pi2-rflt))
     djj = drp * cos(rcln-rfln)
     dj = djj + isn*( (y-refy) * dskm )
     dii = drp * sin(rcln-rfln)
     di = dii + ((x-refx)*dskm)
     ! Calculate the Big Messy quantity as would be done for LC
     ! projections.  This quantity is different in value, same 
     ! in purpose of BM above
     bm = (1.0/re) * sqrt(di*di + dj*dj) / (1.0 + cos(pi2-flat1))
     !  Calculate the desired latitude in radians
     xlat = pi2 - isn*2.0 * atan(bm)
     !  Calculate the desired longitude in radians
     xlon = rcln + atan2(di,-dj)

     !  Convert the calculated lat,lon pair into degrees
     xlat = xlat * 180.0/pi
     xlon = xlon * 180.0/pi

  else
     print*, 'Unrecognized project:  ', project
     stop
  end if

!  Make sure no values are greater than 180 degrees and none
!  are less than -180 degrees

  if (xlon .gt. 180.0)  xlon = xlon - 360.0
  if (xlon .lt. -180.0) xlon = xlon + 360.0

end subroutine xytoll_generic


!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: lccone -- Calculate cone factor
!
! !DESCRIPTION:  
!   Calculate cone factor for Lambert Conformal projection
!
! !REVISION HISTORY:
!                   Kevin Manning - first version
!
! !INTERFACE: ------------------------------------------------------------------
subroutine lccone (fsplat,ssplat,sign1,confac)

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(INT), intent(in)  :: sign1      ! plus or minus one
   real(R8)    , intent(in)  :: fsplat    ! truelat1
   real(R8)    , intent(in)  :: ssplat    ! truelat2
   real(R8)    , intent(out) :: confac    ! Cone factor

!EOP

   !----- local -----

   real(R8), parameter :: conv=0.01745329251994

   if (dabs(fsplat-ssplat).lt.1.D-2) then
      confac = sin(fsplat*conv)
   else
      confac = log10(cos(fsplat*conv))-log10(cos(ssplat*conv))
      confac = confac/(log10(tan((45.-float(sign1)*fsplat/2.)*conv))-&
                       log10(tan((45.-float(sign1)*ssplat/2.)*conv)))
   endif

end subroutine lccone

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: four_point -- Four-point interpolation
!
! !DESCRIPTION:  
! Performs a 4-point interpolation to a given (x,y) coordinate in an array.
! The X coordinate corresponds to the first dimension of array ARRAY.   
! The Y coordinate corresponds to the second dimension of array ARRAY. 
! For points outside the domain, the return value is -1.E33.          
! JLS  x and y are in units of floating index                         
! JLS  ix and jx are the total number of points in the domain.       
! JLS  This function checks to verify that x and y are in the domain 
!
! !REVISION HISTORY:
!                   Kevin Manning - first version
!
! !INTERFACE: ------------------------------------------------------------------
real function four_point(array, ix, jx, x, y)

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(INT), intent(in) :: ix, jx        ! Dimensions of array
   real(R8)    , intent(in) :: array(ix,jx)  ! 2d array
   real(R8)    , intent(in) :: x, y          ! Real (i,j) of desired point

!EOP

    integer :: i, j, ip, jp
    real :: dx, dy, rx, ry

    if (((x-ix)>1.E-4) .or. ((y-jx)>1.E-4) .or. (x < 0.99999) .or. (y < 0.99999)) then
       if((x-ix)>1.E-5)  print*, 'x, ix = ', x, ix, ((x-ix)>1.E-5), (x-ix)
       if ( y < 0.99999) print*, 'y = ', y
       four_point = -1.E33
    else
       i = dint(x+1.E-5)
       j = dint(y+1.E-5)

       ! The following MAX test should be safe, since we've already checked
       ! that we're not too much less than 1.0
       i = max(i, 1)
       j = max(j, 1)

       dx = x-i
       dy = y-j
       if (dx < 1.E-4) dx = 0.0
       if (dx > 0.9999) dx = 1.0
       if (dy < 1.E-4) dy = 0.0
       if (dy > 0.9999) dy = 1.0
       rx = 1.0 - dx
       ry = 1.0 - dy

       ! The following MIN test should be safe, since we've already
       ip = min(i+1, ix)
       jp = min(j+1, jx)

!KWM       print*, 'ip, jp = ', ip, jp
!KWM       print*, 'i, j = ', i, j
!KWM       print*, 'rx, ry, dx, dy = ', rx, ry, dx, dy

       four_point= array(i ,j )*RY*RX + &
                   array(ip,j )*RY*DX + &
                   array(i ,jp)*DY*RX + &
                   array(ip,jp)*DY*DX
    endif

end function four_point

!=======================================================================

end module WRF_tools

!=======================================================================

