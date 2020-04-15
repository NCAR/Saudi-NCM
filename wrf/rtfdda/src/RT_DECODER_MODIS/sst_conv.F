!234567
      program sst_conv
         character (len=256):: fileIn
	 character (len=24) :: hdate    ! Valid date for data YYYY:MM:DD_HH:00:00
         character (len=256):: tmpStr
         character (len=256):: inDate
         real :: xfcst
      
         call getarg (1, fileIn) 
         call getarg (2, inDate) 
         call getarg (3, tmpStr) 
         read(tmpStr,*) xfcst
        
         write(hdate,'(A4,":",A2,":",A2,"_",A2,":00:00")')   &
             inDate(1:4),inDate(5:6),inDate(7:8),inDate(9:10) 
         write(6,*) "input date YYYY:MM:DD_HH:00:00", hdate
         call sst2interm(fileIn,hdate,xfcst)
      stop
      end
      
      subroutine sst2interm (fileIn,hdate,xfcst)
        implicit none
#include <netcdf.inc>
        integer :: ounit
        character (len=256):: fileIn
        integer :: version             ! Format version (must =5 for WPS format)
        integer :: nx, ny              ! x- and y-dimensions of 2-d array
	integer :: iproj               ! Code for projection of data in array:

                               !       0 = cylindrical equidistant

                               !       1 = Mercator

                               !       3 = Lambert conformal conic

                               !       4 = Gaussian (global only!)

                               !       5 = Polar stereographic

	real :: nlats                  ! Number of latitudes north of equator

                               !       (for Gaussian grids)

	real :: xfcst                  ! Forecast hour of data

	real :: xlvl                   ! Vertical level of data in 2-d array

	real :: startlat, startlon     ! Lat/lon of point in array indicated by

                               !       startloc string

	real :: deltalat, deltalon     ! Grid spacing, degrees

	real :: dx, dy                 ! Grid spacing, km

	real :: xlonc                  ! Standard longitude of projection

	real :: truelat1, truelat2     ! True latitudes of projection

	real :: earth_radius           ! Earth radius, km

	real, allocatable,dimension(:,:) :: slab ! The 2-d array holding the data
	real, allocatable,dimension(:) :: lat, lon

	logical :: is_wind_grid_rel    ! Flag indicating whether winds are                                        
                               !       relative to source grid (TRUE) or

                               !       relative to earth (FALSE)

	character (len=8)  :: startloc ! Which point in array is given by

                               !       startlat/startlon; set either                                       
                               !       to 'SWCORNER' or 'CENTER  '

	character (len=9)  :: field    ! Name of the field

	character (len=24) :: hdate    ! Valid date for data YYYY:MM:DD_HH:00:00

	character (len=25) :: units    ! Units of data

	character (len=32) :: map_source  !  Source model / originating center

	character (len=46) :: desc     ! Short description of data
	character (len=80) :: outFile     ! Short description of data

!!! read teh netcdf data
        integer  iret,ncid, varid, dims(10)
      
        field = "sst_data" 
    ! get the array size nx,ny  
        iret = nf_open(trim(fileIn), NF_NOWRITE, ncid)
        if (iret .ne. 0) stop " open file error"
        iret = nf_inq_varid(ncid,trim(field),varid)
        iret = nf_inq_vardimid (ncid, varid, dims)
        iret = nf_inq_dimlen(ncid, dims(1), nx)
        iret = nf_inq_dimlen(ncid, dims(2), ny)
        allocate(slab (nx,ny))
        allocate(lat (ny))
        allocate(lon (nx))
        write(6,*) "sst size(nx,ny) : ",nx,ny
        ! read the sst data
        iret = nf_inq_varid(ncid, trim(field),  varid)
        iret = nf_get_var_real(ncid, varid, slab)
        slab = slab + 273.5
        write(6,*) "read data status: ", iret
        
        iret = NF_GET_ATT_TEXT(ncid,NF_GLOBAL,"Sensor_Name",map_source);
        iret = NF_GET_ATT_TEXT(ncid,NF_GLOBAL,"Units",units);
        iret = NF_GET_ATT_TEXT(ncid,NF_GLOBAL,"Parameter",desc);

!       
     iret = nf_inq_varid(ncid, "latitude",  varid)
     iret = nf_get_vara_real(ncid,varid,(/1/),(/ny/),lat)

     iret = nf_inq_varid(ncid, "longitude",  varid)
     iret = nf_get_vara_real(ncid,varid,(/1/),(/nx/),lon)

     startlat = lat(1)
     startlon = lon(1)
     deltalat = (lat(ny) - lat(1))/ny
     deltalon = (lon(nx) - lon(1))/nx
 
        write(6,*) "field ", field
        write(6,*) "lat(1), lon(1) : ", lat(1), lon(1)
        write(6,*) "lat(ny), lon(nx) : ", lat(ny), lon(nx)

        field = "SST"
        version = 5 
        iproj = 0
        xlvl = 200100
        ounit = 33
        startloc = "SWCORNER"
        earth_radius = 6371.229492   ! m
        is_wind_grid_rel = .false.
        desc = "Sea Surface Temperature"
        units = "K"
       write(6,*) "hdate: "
       write(6,*) hdate(1:4)
       write(6,*) hdate(6:7)
       write(6,*) hdate(7:8), hdate(9:10)
       write(outFile,'("SST:",A4,"-",A2,"-",A2,"_",A2)') &
            hdate(1:4), hdate(6:7),hdate(9:10),hdate(12:13)
       write(6,*) "outFile=",trim(outFile)
       open(ounit,file=trim(outFile),form="unformatted") 

!  1) WRITE FORMAT VERSION

      write(unit=ounit) version
    
      write(6,*) "version =",version

!  2) WRITE METADATA

! Cylindrical equidistant
!     read(11) version
!     write(6,*) "version =", version
!     call flush(6) 

!     read(11) version, hdate, xfcst, map_source, field, &
!                       units, desc, xlvl, nx, ny, iproj

!    write(6,*) hdate, xfcst, map_source, field, &
!                       units, desc, xlvl, nx, ny, iproj
!     write(6,*) "finish read SST"



	if (iproj == 0) then

      write(unit=ounit) hdate, xfcst, map_source, field, &
                        units, desc, xlvl, nx, ny, iproj

      write(unit=ounit) startloc, startlat, startlon, &
                        deltalat, deltalon, earth_radius

! Mercator

	else if (iproj == 1) then

      write(unit=ounit) hdate, xfcst, map_source, field, &

                        units, desc, xlvl, nx, ny, iproj

      write(unit=ounit) startloc, startlat, startlon, dx, dy, &

                        truelat1, earth_radius

! Lambert conformal

	else if (iproj == 3) then

      write(unit=ounit) hdate, xfcst, map_source, field, &

                        units, desc, xlvl, nx, ny, iproj

      write(unit=ounit) startloc, startlat, startlon, dx, dy, &

                        xlonc, truelat1, truelat2, earth_radius

! Gaussian

	else if (iproj == 4) then

      write(unit=ounit) hdate, xfcst, map_source, field, &

                        units, desc, xlvl, nx, ny, iproj

      write(unit=ounit) startloc, startlat, startlon, &

                               nlats, deltalon, earth_radius
! Polar stereographic

	else if (iproj == 5) then

      write(unit=ounit) hdate, xfcst, map_source, field, &

                        units, desc, xlvl, nx, ny, iproj

      write(unit=ounit) startloc, startlat, startlon, dx, dy, &

                        xlonc, truelat1, earth_radius

	end if

!  3) WRITE WIND ROTATION FLAG
      write(unit=ounit) is_wind_grid_rel

!  4) WRITE 2-D ARRAY OF DATA
      write(6,*) "data(1,1)", slab(1,1)
      write(unit=ounit) slab

      end subroutine sst2interm
