!=======================================================================
! CVS: $Id: WRF_ptData.f90,v 1.3 2018/08/31 20:16:59 hsoh Exp $
! CVS: $Source: /cvs/apps/netcdf_utils/src/WRF_to_point_data/WRF_ptData.f90,v $
! CVS: $Name:  $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_ptData - Get WRF data at arbitrary point(s) for tabular data
!
! !DESCRIPTION:
!    Get WRF data at any number of arbitrary points for tabular data
!
! !REVISION HISTORY:
!
! 2005-Sep-9 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

module WRF_ptData

! !USES:

   use WRF_kinds
   use WRF_ncread
   use wrf_tools
   use WRF_constants
   use WRF_input

!EOP

   implicit none

   private    ! except

! !PUBLIC TYPES:

   ! none

! !PUBLIC MEMBER FUNCTIONS:

   public :: any_point   ! Get WRF point data at a site

! !PUBLIC DATA MEMBERS:

   ! none

!EOP
!===============================================================================
contains
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: any_point -- retrieve WRF data at an arbitrary point
!
! !DESCRIPTION:  
!
!  ASSUMPTIONS: Input (x,y,z) locations are one of two cases:
!    (1) One height value, multiple lat/lon points
!    (2) Multiple height values, multiple lat/lon points,
!        numbers of each must be equal.
! !REVISION HISTORY:
!     2005-Sep-23 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine any_point(zObsPt, zCoord, out_unit, statusFlag, statusInfo)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   real(R8)    ,intent(in)         :: zObsPt(:)       ! Vert coordinate value
   character(*),intent(in)         :: zCoord          ! Vertical coordinate type
   integer(INT),intent(in)         :: out_unit        ! Unit specifier for met output
   integer(INT),intent(out)        :: statusFlag      ! Status: 0=good, all other bad
   character(*),intent(out)        :: statusInfo      ! Status information

!EOP

   !----- local -----
   integer(INT) :: i,iz,j,k          ! loop indices
   integer(INT) :: ndims             ! number of dimensions in a field
   integer(INT) :: nx,ny,nz          ! dimensions of netCDF fields
   integer(INT) :: i_lleft,j_lleft   ! indices of lower left box surrounding pt
   integer(INT) :: nlat              ! number of obs points
   integer(INT) :: nzObs             ! number of height points
   integer(INT) :: nVert             ! value of netCDF bottom_top dimension
   integer(INT) :: nVert_stag        ! value of netCDF bottom_top_stag dimension
   integer(INT) :: nWE_stag,nSN_stag ! value of netCDF west_east,south_north stag dimension
   real(R8)     :: dx, dy            ! fraction of grid cell for interp
   real(R8)     :: u_rot, v_rot      ! Rotated wind vectors

   real(R8),allocatable :: work4d(:,:,:,:)    ! 4d work array
   real(R8),allocatable :: work3d(:,:,:)      ! 3d work array
   real(R8),allocatable :: work1d(:)          ! 1d work array
   real(R8),allocatable :: zValues(:,:)       ! Vert. values from netCDF file
   real(R8),allocatable :: znw(:),znu(:)      ! eta values, full, half levels
   real(R8),allocatable :: znWeight(:)        ! eta weights
   real(R8),allocatable :: geoExp(:,:)        ! Exponential height variable
   real(R8),allocatable :: terrain_hgt(:)     ! Model elevation

   real(R8) ,dimension(:,:),allocatable :: Pbase,Ppert,Zbase,Zpert   ! Height dependent
   real(R8) ,dimension(:,:),allocatable :: Qmix,pertT,Uz,Vz,Pz,T2m   ! fields from WRF
   real(R8) :: zValues2(2)                    ! Vert. values for interpolation
   real(R8) :: zTemp2(2)                      ! Temp. values for interpolation

   real(R8)             :: Pzpt,pertTpt, Qmixpt, Uzpt, Vzpt  ! point values from WRF
   real(R8)             :: Tz,RHz,dirz,speedz                ! Calculated values
   character(char_long) :: path_and_file                     ! path//filename
   character(len=4)     :: cyear,chour   ! character year and hour of date_time
   character(len=2)     :: cmonth,cday   ! character month and day of date_time

   integer(INT),parameter :: nfld_in=9                 ! # of fields to read in
   character(len=6)       :: field_in(nfld_in) = &         ! Names of fields to read
   &    (/'PB    ', 'P     ', 'PH    ', 'PHB   ', &
   &      'T     ', 'QVAPOR', 'U     ', 'V     ', 'T2    '/)

   !----- formats -----
   character(*),parameter :: subName =   "(any_point)"
   character(*),parameter ::     F00 = "('(any_point) ',20a)"
   character(*),parameter ::     F03 = "('(any_point) ',2(a,f20.3))"
   character(*),parameter ::     F04 =   '(f7.3,f9.3,f7.1,1x,a4,1x,a2,1x,a2,1x,a4,1x,'// &
    &                                     'f6.1,1x,f5.1,1x,f4.0,1x,f4.0,1x,f4.0)'

   if (trim(zCoord) == "P" .or. trim(zCoord) == "p") then
   !
   elseif (trim(zCoord) == "H" .or. trim(zCoord) == "h") then
   !
   else
      statusFlag = 1
      statusInfo = 'ERROR zCoord not recognized'
      return
   endif

   path_and_file = trim(wrf_path_name)//"/"//trim(wrf_file_name)
   !---------------------------------------------------------------------------
   !--- Get numbers of number of point/sounding locations and heights
   !---------------------------------------------------------------------------
   nlat    = size(lat)
   nzObs = size(zObsPt)
   
   if (nzObs == 1) then
   ! One height, multiple locations
   elseif (nzObs == nlat) then
   ! Multpile heights and locations, must be equal numbers
   else
      statusFlag = 1
      statusInfo = 'ERROR nlat not equal to nzObs, and nzObs is not 1'
      return
   endif

   !---------------------------------------------------------------------------
   !--- Get dimensions 
   !---------------------------------------------------------------------------
   call get_dimension(path_and_file, "bottom_top", nVert)
   call get_dimension(path_and_file, "bottom_top_stag", nVert_stag)
   call get_dimension(path_and_file, "south_north_stag", nSN_stag)
   call get_dimension(path_and_file, "west_east_stag"  , nWE_stag)

   !---------------------------------------------------------------------------
   !--- Read in the vertical "eta" coordinate values of both the full (w) and
   !--- half (mass) levels
   !---------------------------------------------------------------------------
   if (trim(zCoord) == "H" .or. trim(zCoord) == "h") then

      allocate(work4d(nWE,nSN,1,1))
      allocate(terrain_hgt(nlat))
      call ncread_field4dG(path_and_file, 'HGT', rfld=work4d, dim3i=nTimeInd)

      do i = 1,nlat          ! Get elevation for each point
         terrain_hgt(i) = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))
      enddo
      deallocate(work4d)

      allocate(work4d(nVert_stag,1,1,1))
      call ncread_field4dG(path_and_file, 'ZNW', rfld=work4d, dim2i=nTimeInd)
      allocate(znw(nVert_stag))
      znw = work4d(:,1,1,1)
      deallocate(work4d)

      allocate(work4d(nVert,1,1,1))
      call ncread_field4dG(path_and_file, 'ZNU', rfld=work4d, dim2i=nTimeInd)
      allocate(znu(nVert))
      znu = work4d(:,1,1,1)
      deallocate(work4d)

      allocate(znWeight(nVert))
      do k=1,nVert
         znWeight(k)=(znu(k)-znw(k))/(znw(k+1)-znw(k))
      enddo
      deallocate(znu, znw)
   endif

   !---------------------------------------------------------------------
   !--- Get integer year, month, day, hour from date
   !---------------------------------------------------------------------
   cyear  = date_time(1:4)
   cmonth = date_time(6:7)
   cday = date_time(9:10)
   chour = date_time(12:13)//date_time(15:16)

   !---------------------------------------------------------------------------
   !--- Allocate arrays for columns of point data
   !---------------------------------------------------------------------------
   allocate(Pbase(nlat,nVert), Ppert(nlat,nVert), Pz(nlat,nVert))
   allocate(Zbase(nlat,nVert_stag), Uz(nlat,nVert)   , Vz(nlat,nVert))
   allocate(Qmix(nlat,nVert) , pertT(nlat,nVert), Zpert(nlat,nVert_stag))
   allocate(T2m(nlat,1))

   !---------------------------------------------------------------------------
   !--- Read in the (x,y,z) fields for sounding data.
   !---------------------------------------------------------------------------
   do j = 1,nfld_in          ! Loop through the vertical fields to read in

      !------------------------------------------------------------------------
      ! Get number of dimensions and size of nx, ny, nz
      !------------------------------------------------------------------------
      call ncread_varDimNum(trim(path_and_file),trim(field_in(j)),ndims)
      if (trim(field_in(j)) == 'T2') then
         nz = 1
         call ncread_varDimSizes(trim(path_and_file),trim(field_in(j)),nx,ny)
      else
         call ncread_varDimSizes(trim(path_and_file),trim(field_in(j)),nx,ny,nz)
      endif

      allocate(work4d(nx, ny, nz, 1))
      allocate(work3d(nWE,nSN,nz))  ! work3d in non-staggered dimensions
!      allocate(work1d(nVert))
      allocate(work1d(nz))

      call ncread_field4dG(trim(path_and_file), trim(field_in(j)), &
      &                    rfld=work4d, dim4i=nTimeInd)

      !------------------------------------------------------------------------
      ! Check for horizontally staggered fields, averaged to non-staggered
      !------------------------------------------------------------------------
      if ( nx == nWE_stag .or. ny == nSN_stag ) then 
         call unstagger(path_and_file, work4d(:,:,:,1), work3d)
      else
         work3d = work4d(:,:,:,1)
      endif
      
      do i = 1,nlat          ! Loop through the obs points, saving fields

         !---------------------------------------------------------------------
         !--- Horizontally interpolate each point in sounding to lat,lon        
         !---------------------------------------------------------------------
         do k = 1,nz
            work1d(k) = four_point(work3d(:,:,k),nWE,nSN,x_obs(i),y_obs(i))
         enddo
         if (trim(field_in(j)) == 'PB'    ) Pbase(i,:) = work1d(:)*pa_to_mb
         if (trim(field_in(j)) == 'P'     ) Ppert(i,:) = work1d(:)*pa_to_mb
         if (trim(field_in(j)) == 'PHB'   ) Zbase(i,:) = work1d(:)
         if (trim(field_in(j)) == 'PH'    ) Zpert(i,:) = work1d(:)
         if (trim(field_in(j)) == 'QVAPOR')  Qmix(i,:) = work1d(:)
         if (trim(field_in(j)) == 'T'     ) pertT(i,:) = work1d(:)
         if (trim(field_in(j)) == 'U'     )    Uz(i,:) = work1d(:)
         if (trim(field_in(j)) == 'V'     )    Vz(i,:) = work1d(:)           
         if (trim(field_in(j)) == 'T2'    )   T2m(i,1) = work1d(1)
      enddo
      deallocate(work4d)
      deallocate(work3d)
      deallocate(work1d)
   enddo                     ! nfld_in

   !---------------------------------------------------------------------
   !--- Calculate values for point(s)
   !--- For the geopotential: the WRF vertical coordinate is mass based.
   !--- (1) Convert the geopotential to an exponential height variable,
   !---     exp(-z/H), where H is a scale height.
   !--- (2) Linearly interpolate in WRF vert. coord. ("eta") from full
   !---     (w) levels to "mass levels", and then convert back to
   !---     geopotential height.
   !--- (3) Interpolate exp(-z/H) to "mass levels", convert to height
   !---------------------------------------------------------------------
   allocate(zValues(nlat,nVert),geoExp(nlat,nVert_stag))
   iz = 1
   do i = 1,nlat      
      Pz(i,:) = Pbase(i,:) + Ppert(i,:)  
      if (trim(zCoord) == "P" .or. trim(zCoord) == "p") then
         zValues(i,:) = Pz(i,:)                ! Column of vert coord
      elseif (trim(zCoord) == "H" .or. trim(zCoord) == "h") then
         geoExp(i,:) = dexp(-(Zbase(i,:) + Zpert(i,:))/(g0*scale_ht))  
         ! OK Zbase(i,:) + Zpert(i,:)
         do k = 1,nVert
            zValues(i,k) = znWeight(k)*geoExp(i,k+1) + (c1 - znWeight(k))*geoExp(i,k)
            zValues(i,k) = -scale_ht*dlog(zValues(i,k))  ! Height above sea level
            zValues(i,k) = zValues(i,k) - terrain_hgt(i) ! Height AGL
         enddo
      endif

      !-------------------------------------------------------------------
      !--- Interpolate to get point in the vertical
      !-------------------------------------------------------------------
      call nearest_k(zValues(i,:), zObsPt(iz), k, statusFlag, statusInfo)

      !-------------------------------------------------------------------
      !--- If point is outside of vertical domain, return missing values
      !-------------------------------------------------------------------
      Pzpt = missing ; Tz     = missing 
      RHz  = missing ; RHz    = missing
      dirz = missing ; speedz = missing
      if (statusFlag == 1) then
         if(zObsPt(iz) >= 2.) then
            zValues2(1) = 2.                    ! Vert. values for interpolation
            zValues2(2) = zValues(i,1)
            zTemp2(1) = T2m(i,1)                ! Temp. values for interpolation
            zTemp2(2) = (pertT(i,1) + T0)*((Pz(i,1)/P00)**(Rd/cp))  ! Convert potential temp
            call interp_1d(zValues2, zObsPt(iz), zTemp2, Tz)
            Tz = Tz - 273.15
         endif
         statusFlag = 0
      else
         call interp_1d(zValues(i,k:k+1), zObsPt(iz), Pz(i,k:k+1),    Pzpt)
         call interp_1d(zValues(i,k:k+1), zObsPt(iz), Qmix(i,k:k+1),  Qmixpt)
         call interp_1d(zValues(i,k:k+1), zObsPt(iz), pertT(i,k:k+1), pertTpt)
         call interp_1d(zValues(i,k:k+1), zObsPt(iz), Uz(i,k:k+1),    Uzpt)
         call interp_1d(zValues(i,k:k+1), zObsPt(iz), Vz(i,k:k+1),    Vzpt)
         
         Tz  = (pertTpt + T0)*((Pzpt/P00)**(Rd/cp))     ! Convert potential temp
         
         RHz = rhcalc(Tz, Qmixpt, Pzpt)*c100   ! RH in %
         call rotate_wind (Uzpt, Vzpt, map_proj, stand_lon, lat(i), &
         &                 lon(i), truelat1, truelat2, u_rot, v_rot)
         call wind_dir_speed(u_rot, v_rot, dirz, speedz)
         Tz = Tz - 273.15
      endif
      write (out_unit,F04) lat(i), lon(i), zObsPt(iz), cyear, cmonth, cday, chour, &
      &             Pzpt, Tz, RHz, dirz, speedz
      if (nzObs /= 1) iz = iz + 1
   enddo

   deallocate(zValues, geoExp)
   if (trim(zCoord) == "H" .or. trim(zCoord) == "h") deallocate (terrain_hgt,znWeight)
   deallocate(Pbase, Ppert, Pz)
   deallocate(Zbase, Uz   , Vz)
   deallocate(Qmix , pertT, Zpert)

end subroutine any_point

!=======================================================================

end module WRF_ptData

!=======================================================================

