!=======================================================================
! CVS: $Id: WRF_main.f90,v 1.21 2019/01/07 20:50:52 sheu Exp $
! CVS: $Source: /cvs/apps/netcdf_utils/src/WRF_to_point_data/WRF_main.f90,v $
! CVS: $Name:  $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_main - driver routine to provide input for subroutines
!
! !DESCRIPTION:
!    This module provides input parameters for the subroutines that
!    extract point and sounding data from WRF netCDF output files.
!
!    Given any number of lat, lon and height values at  a single time,
!    and a WRF netCDF output file, this subroutine can retrieve a set
!    of meteorological data in three different ways:
!
!    1)  A virtual sounding
!    2)  Point data at an arbitrary point
!    3)  Surface site information at a point
!
! !REVISION HISTORY:
!
! 2005-Aug-19 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

program WRF_main

! !USES:

   use WRF_kinds
   use WRF_ncread
   use WRF_site
   use WRF_sounding
   use WRF_constants
   use WRF_ptData
   use WRF_tools
   use WRF_cmdLine
   use WRF_input

!EOP

implicit none

   integer(INT) :: numarg        ! Number of arguments in command line
   integer(INT) :: rCode         ! error code
   integer(INT) :: i             ! loop index
   integer(INT) :: out_unit      ! Unit specifier for meteorological output
   integer,allocatable :: izObsPt(:)        ! Height of obs point

   character(char_short) :: output_flag   ! Output data type: sounding, sfc pt, any pt
   character(char_short) :: rnge = ""     ! Range (Test Center) name
   character(char_long)  :: location_file ! File with lat, lon, elevation, info
   character(char_long)  :: height_file   ! File with height info
   character(char_long)  :: gatt_list     ! Global attributes to retrieve
   character(char_long)  :: dims_list     ! Dimension size to retrieve
   character(char_short) :: zCoord        ! Vertical coordinate type
   character(char_short) :: outputFiles   ! Single or multiple output files
   character(char_short) :: output_type   ! "fcst", "preli", or "final

   real(R8),allocatable :: ptHeight(:)      ! Height of arb point read in
   real(R8) :: zObsPt_cmd, x_obs_cmd, y_obs_cmd         ! Command line info
   real(R8) :: dz                         ! ertical interval (m) for snd
   real(R8) :: topz                       ! Max height (m) for snd
   integer :: im

   logical :: exists    ! Does location file exist?
   logical :: writeBufkit
   logical :: writeYPG2
   logical :: writeREDI
   logical :: writeIAF  ! coupled with output_type='metcm'
   logical :: use_gbc_file
   integer(INT) :: fid             ! netCDF file ID

   !----- formats -----
   character(*),parameter :: subName = "(WRF_main)"
   character(*),parameter :: F04 = '(f7.3,1x,f8.3,1x,i4,1x,a3,1x,a30)'


   namelist/pobs_input/output_flag, path_and_file, location_file, height_file, &
                       nlat, nheight, zCoord, outputFiles, out_unit, &
                       output_type, writeBufkit, rnge

   numarg = command_argument_count()

!----------------------------------------------------------------------
!  Set the default namelist values, set here, since it is written out
!  for all site data
!----------------------------------------------------------------------
   output_type = "final"

!------------------------------------------------------------------------------!
!-----------------  There are no arguments, use the namelist ------------------!
!------------------------------------------------------------------------------!
   if (numarg == 0) then 

      open(15, file = "pobs_input.nml", status="old", action="read")
      read(15, nml=pobs_input, iostat=rCode)
      close(unit=15)
      if (rCode > 0) then
         write(6,*) 'ERROR: reading input namelist, iostat=',rCode
         STOP
      end if
      useLatLon = .true.

      allocate(lat_in(nlat),lon_in(nlat), zObsPt_in(nlat))
      allocate(station_name_in(nlat), station_id_in(nlat))
      allocate(izObsPt(nlat))

      if (trim(outputFiles) == "multiple") out_unit = 61

      !----------------------------------------------------------------------
      !  Read lat, lon elevation of points from ascii file.  
      !----------------------------------------------------------------------

      inquire(file=location_file, exist=exists)
      if (.not.exists) then
         write(6,*) 'ERROR: File with lat,lon, height info does not exist:',location_file
         STOP
      endif
      open(20, file = location_file, status="old", action="read")
      write (6,*) "nlat = ",nlat
      do i = 1,nlat
         read (20,*) lat_in(i),lon_in(i), izObsPt(i), station_id_in(i), station_name_in(i)
!        write (6,F04) lat_in(i),lon_in(i), izObsPt(i), station_id_in(i), station_name_in(i)
         zObsPt_in(i) = float(izObsPt(i))
      enddo
      close(unit=20)

      !----------------------------------------------------------------------
      !  For *arbitrary points*, read height of obs points from ascii file.
      !----------------------------------------------------------------------

      if (output_flag == "arb_point") then
         allocate(ptHeight(nheight))
         open(25, file = height_file, status="old", action="read")
         do i = 1,nheight
            read (25,*) ptHeight(i)
         end do
      endif

      gatt_list=""
      dims_list=""
   else
!------------------------------------------------------------------------------!
!------------------- Parse command line arguments -----------------------------!
!------------------------------------------------------------------------------!

      nlat = 1
      nheight = 1

      allocate(lat_in(nlat),lon_in(nlat), zObsPt_in(nlat))
      allocate(station_name_in(nlat), station_id_in(nlat))

      CALL arguments(output_flag, path_and_file, station_name_in(1),    &
                     lat_in(1), lon_in(1), zObsPt_cmd, zCoord, x_obs_cmd, &
                     y_obs_cmd,dz,topz, im, useLatLon, useEqual, writeBufkit, &
                     writeYPG2, writeREDI, writeIAF, rnge, use_gbc_file, &
                     out_unit, output_type, gatt_list, dims_list)

      !----------------------------------------------------------------------
      !  Assign lat, lon and elevation of points from command line.  
      !----------------------------------------------------------------------
      if (output_flag == "arb_point") then
         allocate(ptHeight(nheight))
         ptHeight(1) = zObsPt_cmd
      else
         zObsPt_in(1) = zObsPt_cmd
      endif
      station_id_in(1) = " "
      outputFiles = "single"

   endif 

   call ncread_open(path_and_file, fid)

   if (len_trim(gatt_list) > 0) then
      call get_gatt(gatt_list,fid)
      stop
   endif

   if (len_trim(dims_list) > 0) then
      call get_dims(dims_list,fid)
      stop
   endif

   !---------------------------------------------------------------------------
   !--- Get domain dimensions, global attributes (map info)
   !---------------------------------------------------------------------------
   if (use_gbc_file) then
      call getBasicInfo(gbcfile=use_gbc_file)
   else
      call getBasicInfo()
   end if
   !---------------------------------------------------------------------------
   !--- Get list of points in model domain
   !---------------------------------------------------------------------------
   call chkInDomain(x_obs_cmd, y_obs_cmd)

   !---------------------------------------------------------------------------
   !--- Determine if output is sfc site data, arbitrary point, or sounding
   !---------------------------------------------------------------------------
   if (output_flag == "sfc_site") then

      if (use_gbc_file) then
         call site_gbc_data(outputFiles, out_unit, output_type, statusFlag, statusInfo)
      else
         call site_data(outputFiles, out_unit, output_type, writeBufkit, statusFlag, statusInfo)
      end if

   elseif (output_flag == "arb_point ") then

      call any_point(ptHeight, zCoord, out_unit, statusFlag, statusInfo)


   elseif (output_flag == "sounding") then

      call sounding_data(out_unit, zCoord, dz, im, topz, writeBufkit, writeYPG2, writeREDI, rnge, statusFlag, statusInfo)

   elseif (output_flag == "metcm") then

      if (upper(trim(rnge)) == 'IAF') then
         writeIAF = .TRUE.
      else
         writeIAF = .FALSE.
      endif

      call metcm(writeIAF,statusFlag,statusInfo)

   else
      statusFlag = 1
      statusInfo = 'ERROR in WRF_main, check value of output_flag in namelist'
   endif

   if (statusFlag /= 0) then
      write (6,*) trim(statusInfo)
   endif

 ! deallocate(x_obs, y_obs, lat, lon)
 ! deallocate(station_id, station_name, zObsPt)
   if (output_flag == "arb_point") deallocate(ptHeight)

   rCode = ncread_close(fid, subName)   
   
!=======================================================================

end program WRF_main

!=======================================================================

