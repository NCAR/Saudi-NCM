!=======================================================================
! CVS: $Id: WRF_lltoxy.f90,v 1.7 2014/12/04 16:07:11 hsoh Exp $
! CVS: $Source: /cvs/apps/netcdf_utils/src/WRF_to_point_data/WRF_lltoxy.f90,v $
! CVS: $Name:  $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_lltoxy - driver routine to convert a lat/lon point to i/j
!
! !DESCRIPTION:
!    This is a stand-alone program that converts a lat/lon points to 
!    model grid-points.  It uses command line options for input.
!
! Usage: WRF_lltoxy.exe [-h] -inp_file WRF_file.nc -lat lat -lon lon 
! -----
!
! With:
!  inp_file : Name of WRF netCDF input file to read
!  lat      : Latitude of point 
!  lon      : Longitude of point 
!  h        : Print this help message and exit.
!
! Enter WRF_lltoxy.exe -h for online help.
!
! !REVISION HISTORY:
!
! 2006-Sept-8 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

program WRF_lltoxy

! !USES:

   use WRF_kinds
   use WRF_ncread
   use WRF_tools
   use WRF_input
   use WRF_constants
   use WRF_cmdLine, ONLY : lltoxy_args

!EOP

implicit none

   integer(INT) :: numarg        ! Number of arguments in command line
   integer(INT) :: rCode         ! error code
   integer(INT) :: i             ! loop index
   integer(INT) :: out_unit      ! Unit specifier for meteorological output
   integer(INT) :: fid           ! netCDF file ID
   real(R8),allocatable :: work4d(:,:,:,:)  ! 4d work array
   real(R8),allocatable :: XLAT(:,:)        ! Lat read from netCDF file
   real(R8),allocatable :: XLONG(:,:)       ! Lat read from netCDF file
   real(R8)             :: xout, yout       ! Real (i,j) indices of obs point
   logical              :: use_min_inside          ! Flag to apply a security factor of 5 grid points inside domain.
   logical              :: use_geo_file     ! Flag to indicate input file is a geo_em_d0*.nc file


   !----- formats -----
   character(*),parameter :: subName = "(WRF_lltoxy)"
  

   numarg = command_argument_count()

!------------------------------------------------------------------------------!
!--------------  There are no arguments, print message and exit ---------------!
!------------------------------------------------------------------------------!
   if (numarg == 0) then 
      write (6,*)
      write (6,*) "ERROR: Command line arguments are missing"
      write (6,*) "Usage: "
      write (6,*) "WRF_lltoxy.exe [-h] -inp_file WRF_file.nc -lat lat -lon lon [-useMinInside] [-useGeoFile] "
      write (6,*)
      STOP
   else
!------------------------------------------------------------------------------!
!------------------- Parse command line arguments -----------------------------!
!------------------------------------------------------------------------------!
      nlat = 1
      allocate(lat_in(nlat),lon_in(nlat))
      
      CALL lltoxy_args(path_and_file,lat_in(1), lon_in(1), use_min_inside, use_geo_file, out_unit)

      call ncread_open(path_and_file, fid)
      
      !------------------------------------------------------------------------
      !--- Get domain dimensions, global attributes (map info), time
      !------------------------------------------------------------------------
      call getBasicInfo(geofile=use_geo_file)

      !------------------------------------------------------------------------
      !--- Read in model latitude and longitude: XLAT, XLONG at the desired time
      !------------------------------------------------------------------------
      allocate(work4d(nWE, nSN, 1,1))    ! ncread_field4dG uses a 4d array
      if (use_geo_file) then
        call ncread_field4dG(path_and_file, 'XLAT_M', rfld=work4d, dim3i=1)
      else
        call ncread_field4dG(path_and_file, 'XLAT', rfld=work4d, dim3i=nTimeInd)
      endif
      allocate(XLAT(nWE, nSN))
      XLAT = work4d(:,:,1,1)

      if (use_geo_file) then
        call ncread_field4dG(path_and_file, 'XLONG_M', rfld=work4d, dim3i=1)
      else
        call ncread_field4dG(path_and_file, 'XLONG', rfld=work4d, dim3i=nTimeInd)
      endif
      allocate(XLONG(nWE, nSN))
      XLONG = work4d(:,:,1,1)
      deallocate(work4d)

      !------------------------------------------------------------------------
      !--- Read in XLAT, XLONG
      !------------------------------------------------------------------------

      call lltoxy_generic(lat_in(1), lon_in(1), xout, yout, projection, &
      &                   Dx, XLAT(1,1), XLONG(1,1), c1, c1, stand_lon,   &
      &                   truelat1, truelat2)

      !------------------------------------------------------------------------
      !--- Check to see if xout an your are in model domain
      !------------------------------------------------------------------------
      if(.not. use_min_inside .and. yout <= float(nSN) .and. yout >= c1 .and. xout <= float(nWE) &
         &                  .and. xout >= c1) then
         write (6,'(1X,I5,1X,F5.1,1X,F5.1,1X,I5,1X,I5,1X,F5.1,1X,I5)') 0, xout, yout, nWE, nSN, Dx, grid_id
      else if(use_min_inside .and. yout <= float(nSN-5) .and. yout >= (c1+5) .and. xout <= float(nWE-5) &
         &                  .and. xout >= (c1+5)) then
         write (6,'(1X,I5,1X,F5.1,1X,F5.1,1X,I5,1X,I5,1X,F5.1,1X,I5)') 0, xout, yout, nWE, nSN, Dx, grid_id                         
      else
!        Uncomment write statement below for debugging
        write (6,'(1X,I5,1X,F5.1,1X,F5.1,1X,I5,1X,I5,1X,F5.1,1X,I5)') 1, xout, yout, nWE, nSN, Dx, grid_id
       ! write (6,*) 'point',lat_in(1), lon_in(1), yout, xout,'is *not* in domain'
      endif
     
 
      deallocate (lat_in, lon_in)
      rCode = ncread_close(fid, subName)   

   endif 
 
!=======================================================================

end program WRF_lltoxy

!=======================================================================

