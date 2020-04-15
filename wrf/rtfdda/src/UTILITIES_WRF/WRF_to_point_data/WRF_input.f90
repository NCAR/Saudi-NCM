!=======================================================================
! CVS: $Id: WRF_input.f90,v 1.13 2019/01/07 20:50:52 sheu Exp $
! CVS: $Source: /cvs/apps/netcdf_utils/src/WRF_to_point_data/WRF_input.f90,v $
! CVS: $Name:  $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_input - Process input information
!
! !DESCRIPTION:
!
!  Read map info from global attributes and determine what points of interest
!  are in the model domain.
!
! !REVISION HISTORY:
!
! 2006-Apr-24 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

module WRF_input

! !USES:

   use WRF_kinds
   use WRF_ncread
   use WRF_constants
   use WRF_tools
   use netcdf

!EOP

   implicit none

   private    ! except

! !PUBLIC TYPES:

   ! none

! !PUBLIC MEMBER FUNCTIONS:

   public :: getBasicInfo ! Get domain size, map info
   public :: chkInDomain  ! Read and parse cmd line arguments
   public :: get_gatt     ! Get global attributes specified on command-line
   public :: get_dims     ! Get global attributes specified on command-line

! !PUBLIC DATA MEMBERS:

   character(char_long), public :: wrf_path_name ! WRF NetCDF file path
   character(char_long), public :: wrf_file_name ! WRF NetCDF file name
   character(char_long), public :: path_and_file ! path appended to file name
   character(char_date), public :: date_time     ! CCYY-MM-DD_HH:MM:SS
   character(len=2),     public :: projection    ! Map projection
   character(char_long), public :: statusInfo    ! Status information

   integer(INT), public :: nSN, nWE,nBT! north_south, west_east, bottom_top dimensions
   integer(INT), public :: nSL_stag    ! soil layers dimension
   integer(INT), public :: ibeg_file   ! Integer position for beginning of filename
   integer(INT), public :: iend_file   ! Integer position for end of filename
   integer(INT), public :: nTimeInd    ! index of matching Times variable
   integer(INT), public :: map_proj    ! Integer for type of map proj
   integer(INT), public :: statusFlag  ! Status: 0=good, all other bad
   integer(INT), public :: grid_id     ! Integer domain number

   real(R8), public :: stand_lon                 ! Standard longitude of map proj
   real(R8), public :: cen_lon                   ! Center longitude of map proj
   real(R8), public :: Dx,truelat1,truelat2      ! Global attributes

! Public input for chkInDomain
   integer(INT),public :: nlat               ! number of lat for obs points
   integer(INT),public :: nheight            ! Number obs height coord points
   logical     ,public :: useLatLon          ! true for lat/lon input, false for i/j input
   logical     ,public :: useEqual           ! sfc output uses FIELD=value
   real(R8)    ,public,allocatable :: lat_in(:)          ! Latitude  read in
   real(R8)    ,public,allocatable :: lon_in(:)          ! Longitude read in
   real(R8)    ,public,allocatable :: zObsPt_in(:)       ! Elevation of obs point read in
   character(len=30),public,allocatable :: station_name_in(:) ! Name of station read in
   character(len=3), public,allocatable :: station_id_in(:)   ! Station ID read in

! Public output for chkInDomain
   real(R8)    ,public,allocatable :: x_obs(:)        ! Real x (i) location 
   real(R8)    ,public,allocatable :: y_obs(:)        ! Real y (j) location 
   real(R8)    ,public,allocatable :: lat(:)          ! Latitude (degrees_north)
   real(R8)    ,public,allocatable :: lon(:)          ! Longitude (degrees_east)
   real(R8)    ,public,allocatable :: zObsPt(:)       ! Elevation of obs point
   character(len=30),public,allocatable :: station_name(:) ! Name of station 
   character(len=3),public,allocatable :: station_id(:)   ! Station ID

!===============================================================================
contains
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: getBasicInfo -- Get domain size, map info
!
! !DESCRIPTION:
!  Get domain size, read some global attributes for map info
!
! !REVISION HISTORY:
!     2006-Apr-21 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine getBasicInfo(geofile,gbcfile)

   implicit none

!INPUT/OUTPUT PARAMETERS:

!EOP

!----- local -----
   logical, intent(in), optional :: geofile
   logical, intent(in), optional :: gbcfile
   logical          :: exists             ! Does WRF output file exist?

   !---------------------------------------------------------------------------
   !--- Check existence of path/file
   !---------------------------------------------------------------------------
   inquire(file=path_and_file, exist=exists)
   if (.not.exists) then
      write(6,*) 'ERROR: WRF output file does not exist:',path_and_file
      STOP
   endif

   !---------------------------------------------------------------------------!
   !--------- Get path name, file name and date from path_and_file ------------!
   !---------------------------------------------------------------------------!
   ibeg_file = index (path_and_file, '/', .true.)
   iend_file = len_trim(path_and_file)

   if (ibeg_file /= 0) then                ! Substring was found
      wrf_path_name = path_and_file(1:ibeg_file-1)
      wrf_file_name = path_and_file(ibeg_file+1:iend_file)
   else                                    ! / was not in path_and_file name
      wrf_path_name = "./"
      wrf_file_name = trim(path_and_file)
   endif

   date_time = wrf_file_name(12:30)
!  write (6,*) "path_name = ",trim(wrf_path_name)
!  write (6,*) "file_name = ",trim(wrf_file_name)
!  write (6,*) "date_time = ",trim(date_time)

   !---------------------------------------------------------------------------
   !--- Get dimensions from netCDF file
   !---------------------------------------------------------------------------
   call get_dimension(path_and_file, "south_north", nSN)
   call get_dimension(path_and_file, "west_east"  , nWE)
   if((.not. present(geofile)) .and. (.not. present(gbcfile))) then
     call get_dimension(path_and_file, "bottom_top" , nBT)
     call get_dimension(path_and_file, "soil_layers_stag" , nSL_stag)
   end if

   !---------------------------------------------------------------------------
   !--- Find matching time in netCDF file
   !---------------------------------------------------------------------------
   call getTimeIndex(path_and_file,date_time,nTimeInd,statusFlag,statusInfo)

   !---------------------------------------------------------------------------
   !--- Get some global attributes
   !---------------------------------------------------------------------------
   call get_gl_int_att (path_and_file, 'MAP_PROJ' , map_proj)
   call get_gl_real_att(path_and_file, 'STAND_LON', stand_lon)
   call get_gl_real_att(path_and_file, 'CEN_LON'  , cen_lon)
   call get_gl_real_att(path_and_file, 'TRUELAT1' , truelat1)
   call get_gl_real_att(path_and_file, 'TRUELAT2' , truelat2)
   call get_gl_real_att(path_and_file, 'DX'       , Dx)

   if (present(geofile)) then
      if (geofile) then
         call get_gl_int_att(path_and_file, 'grid_id'  , grid_id)
      else
         call get_gl_int_att(path_and_file, 'GRID_ID'  , grid_id)
      end if
   else
      call get_gl_int_att(path_and_file, 'GRID_ID'  , grid_id)
   endif

   Dx = Dx*0.001
   if (map_proj == 0 ) projection = "CE"
   if (map_proj == 1 ) projection = "LC"
   if (map_proj == 2 ) projection = "ST"
   if (map_proj == 3 ) projection = "ME"

END subroutine getBasicInfo

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: chkInDomain -- Collect points in model domain
!
! !DESCRIPTION:
!  Collect points in the WRF model domain
!
! !REVISION HISTORY:
!     2006-Apr-21 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

 subroutine chkInDomain(x_obs_cmd, y_obs_cmd)

   implicit none

!INPUT/OUTPUT PARAMETERS:

   real(R8),intent(in) :: x_obs_cmd  ! Command line x index
   real(R8),intent(in) :: y_obs_cmd  ! Command line y index

!EOP

!----- local -----
   integer(INT) :: i             ! loop index
   real(R8),allocatable :: work4d(:,:,:,:)  ! 4d work array
   real(R8),allocatable :: XLAT(:,:)        ! Lat read from netCDF file
   real(R8),allocatable :: XLONG(:,:)       ! Lat read from netCDF file
   real(R8),allocatable :: x_obs_tmp(:)     ! Collect real x (i) locations
   real(R8),allocatable :: y_obs_tmp(:)     ! Collect real y (j) locations
   real(R8),allocatable :: lon_tmp(:)       ! Collecting longitudes in domain 
   real(R8),allocatable :: lat_tmp(:)       ! Collecting latitudes  in domain
   real(R8),allocatable :: zObsPt_tmp(:)    ! Collect elevations of obs point
   real(R8)             :: xout, yout       ! Real indices of obs point
   character(len=30), allocatable :: station_name_tmp(:) ! Collect station names
   character(len=3) , allocatable :: station_id_tmp(:)   ! Collect Station ID's
   integer(INT) :: in_pts        ! total number of obs points in domain

   !----- formats -----
   character(*),parameter :: F03 = "('(WRF_input) ',2(a,f20.3))"

   !---------------------------------------------------------------------------
   !--- Read in model latitude and longitude: XLAT, XLONG at the desired time
   !---------------------------------------------------------------------------
   allocate(work4d(nWE, nSN, 1,1))    ! ncread_field4dG uses a 4d array
   call ncread_field4dG(path_and_file, 'XLAT', rfld=work4d, dim3i=nTimeInd)
   allocate(XLAT(nWE, nSN)) 
   XLAT = work4d(:,:,1,1)
   write (lstdout,F03) "Min XLAT= ", minval(XLAT), " Max XLAT= ", maxval(XLAT)

   call ncread_field4dG(path_and_file, 'XLONG', rfld=work4d, dim3i=nTimeInd)
   allocate(XLONG(nWE, nSN)) 
   XLONG = work4d(:,:,1,1)
   deallocate(work4d)
   write (lstdout,F03) "Min XLONG= ", minval(XLONG), " Max XLONG= ", maxval(XLONG)

   allocate(lat_tmp(nlat), lon_tmp(nlat), zObsPt_tmp(nlat))
   allocate(station_name_tmp(nlat), station_id_tmp(nlat))
   allocate(x_obs_tmp(nlat), y_obs_tmp(nlat))

   !------------------------------------------------------------
   ! Collect lat/lon points and their info if they are in domain
   !------------------------------------------------------------
   in_pts = 0
   do i = 1,nlat
      if (useLatLon) then
         call lltoxy_generic(lat_in(i), lon_in(i), xout, yout, projection, &
         &                   Dx, XLAT(1,1), XLONG(1,1), c1, c1, stand_lon, &
         &                   truelat1, truelat2)
      else
         xout = x_obs_cmd
         yout = y_obs_cmd
         call xytoll_generic(xout, yout, lat_in(i), lon_in(1), projection, &
         &                   Dx, XLAT(1,1), XLONG(1,1), c1, c1, stand_lon, &
         &                   truelat1, truelat2)
      endif

      if(yout <= float(nSN) .and. yout >= c1 .and. xout <= float(nWE) &
         &                  .and. xout >= c1) then
         in_pts = in_pts + 1
         lat_tmp(in_pts) = lat_in(i)
         lon_tmp(in_pts) = lon_in(i)
         zObsPt_tmp(in_pts) = zObsPt_in(i)
         station_id_tmp(in_pts) = station_id_in(i)
         station_name_tmp(in_pts) = station_name_in(i)
         x_obs_tmp(in_pts) = xout               ! Save these points for 
         y_obs_tmp(in_pts) = yout               ! interpolation later
      else
!        Uncomment write statement below for debugging
!        write (6,*) 'point',station_name_in(i),yout,xout,'is *not* in domain'
      endif

   end do

   deallocate(lat_in, lon_in, zObsPt_in, station_id_in, station_name_in)
   deallocate(XLAT, XLONG)

   allocate (lat(in_pts), lon(in_pts), zObsPt(in_pts))
   allocate (station_id(in_pts), station_name(in_pts))
   allocate (x_obs(in_pts), y_obs(in_pts))

   lat          = lat_tmp(1:in_pts)     ! This is the input data that is
   lon          = lon_tmp(1:in_pts)     ! in the domain
   zObsPt       = zObsPt_tmp(1:in_pts)
   station_id   = station_id_tmp(1:in_pts)
   station_name = station_name_tmp(1:in_pts)
   x_obs        = x_obs_tmp(1:in_pts)
   y_obs        = y_obs_tmp(1:in_pts)

   deallocate(lat_tmp, lon_tmp, zObsPt_tmp, station_id_tmp, station_name_tmp)
   deallocate(x_obs_tmp, y_obs_tmp)
 
 END subroutine chkInDomain

!=======================================================================

subroutine get_gatt(gatt_list,fid)

   character(char_long), intent(in) :: gatt_list
   integer(INT), intent(in) :: fid
   integer :: istatus, xtype, ilen, i
   integer, allocatable, dimension(:) :: iarr
   real, allocatable, dimension(:) :: rarr
   character(len=80) :: cstring 

   istatus = nf90_inquire_attribute(fid,NF90_GLOBAL,trim(gatt_list), &
             xtype, ilen)
   if (xtype == NF90_INT) then
      allocate(iarr(ilen))
      istatus = nf90_get_att(fid,NF90_GLOBAL,trim(gatt_list),iarr)
      do i = 1,ilen
         write(6,*) iarr(i)
      enddo
      deallocate(iarr)
   else if (xtype == NF90_FLOAT) then
      allocate(rarr(ilen))
      istatus = nf90_get_att(fid,NF90_GLOBAL,trim(gatt_list),rarr)
      do i = 1,ilen
         write(6,*) rarr(i)
      enddo
      deallocate(rarr)
   else if (xtype == NF90_CHAR) then
      istatus = nf90_get_att(fid,NF90_GLOBAL,trim(gatt_list),cstring)
      write(6,*) trim(cstring)
   else
      print*,'Variable type ', xtype, ' not implemented yet'
      stop
   endif

END subroutine get_gatt

!=======================================================================

subroutine get_dims(dims_list,fid)

!! Print dimension size if '-dims <dimension_name>' is supplied on the
!! command-line argument list. <dimension_name> can contain just on dimension
!! or multiple dimensions separated by comma. If multiple dimensions are
!! provided, the result is the product of all the dimension sizes of these
!! dimensions.

   character(char_long), intent(in) :: dims_list
   integer(INT), intent(in) :: fid
   integer :: istatus, n, i, j, k, cbeg, did, idims, total
   character(*),parameter :: subName = "(get_dims)"
   character(*),parameter :: F00 = "('(get_dims) ',a,i6)"

   integer, allocatable :: delim_pos(:)

   character(len=20), allocatable :: dim_names(:)

   n = 0
   do i = 1,len_trim(dims_list)
      if (dims_list(i:i) == ',') then
         n = n+1 
      end if
   end do

   if (n > 0) then
      allocate(delim_pos(n))
      allocate(dim_names(n+1))
      j = 0
      do i = 1,len_trim(dims_list)
         if (dims_list(i:i) == ',') then
            j = j+1
            delim_pos(j) = i
         end if
      end do

      cbeg = 1
      do k = 1,n+1
         if (k < (n+1)) then
            dim_names(k) = dims_list(cbeg:delim_pos(k)-1)
            cbeg = delim_pos(k)+1
         else
            dim_names(k) = dims_list(cbeg:len_trim(dims_list))
         end if
      end do

      total = 1
      do k = 1,n+1
         istatus = nf90_inq_dimid(fid,trim(dim_names(k)),did)
         call handleErr(istatus, subName//" ERROR finding dim "//trim(dim_names(k))//" in data file")
         istatus = nf90_inquire_dimension(fid,did,len=idims)
         call handleErr(istatus, subName//" ERROR getting "//trim(dim_names(k))//" dim in data file")
         total = total * idims
      end do

      write(6,*) total
   else
      istatus = nf90_inq_dimid(fid,trim(dims_list),did)
      call handleErr(istatus, subName//" ERROR finding dim "//trim(dims_list)//" in data file")
      istatus = nf90_inquire_dimension(fid,did,len=idims)
      call handleErr(istatus, subName//" ERROR getting "//trim(dims_list)//" dim in data file")
      write(6,*) idims
   end if

END subroutine get_dims
!=======================================================================

end module WRF_input

!=======================================================================

