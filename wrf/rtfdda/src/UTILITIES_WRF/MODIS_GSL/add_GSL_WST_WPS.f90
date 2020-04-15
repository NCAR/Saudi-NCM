program sample_read
! Fortran 90 version.
 USE funcs_subrs

!   This is a simple program to read data in the WPS intermediate
!   format.

  IMPLICIT NONE

! Declarations:

  INTEGER, PARAMETER :: IUNIT = 10
  INTEGER, PARAMETER :: OUNIT = 11
  INTEGER, PARAMETER :: OUNIT_temp = 12
  INTEGER :: IERR
  INTEGER :: IFV
  INTEGER :: ix, iy
  CHARACTER(LEN=256) :: fname
  CHARACTER(LEN=24) :: HDATE
  REAL :: XFCST
  CHARACTER(LEN=9) :: FIELD
  CHARACTER(LEN=25) :: UNITS
  CHARACTER(LEN=46) :: DESC
  CHARACTER(LEN=32) :: MAP_SOURCE
  REAL :: XLVL
  INTEGER :: NX
  INTEGER :: NY
  INTEGER :: IPROJ
  CHARACTER(len=8) :: STARTLOC
  REAL :: STARTLAT
  REAL :: STARTLON
  REAL :: DELTALAT
  REAL :: DELTALON
  REAL :: DX
  REAL :: DY
  REAL :: XLONC
  REAL :: TRUELAT1
  REAL :: TRUELAT2
  REAL :: NLATS
  REAL :: EARTH_RADIUS = 6367470. * .001
  LOGICAL :: IS_WIND_EARTH_REL = .FALSE.
  REAL, ALLOCATABLE, DIMENSION(:,:) :: SLAB
  REAL, ALLOCATABLE, DIMENSION(:)   :: lats
  REAL, ALLOCATABLE, DIMENSION(:)   :: lons
  REAL :: min_lat,max_lat,min_lon,max_lon,latlon_inc
  INTEGER :: lat_ind, lon_ind, year, month, day, DOY, mon_day
  CHARACTER(LEN=3) :: DOY_str
  CHARACTER(LEN=4) :: year_str
  CHARACTER(LEN=2) :: mon_str, day_str
  CHARACTER(LEN=128) :: param_file, param_name, param_char, GSL_file, WPS_path, WPS_file, GSL_path
  INTEGER :: nlines, n_GSL_lats, n_GSL_lons, min_lat_ind, max_lat_ind, min_lon_ind, max_lon_ind
  REAL, ALLOCATABLE, DIMENSION(:) :: GSL_lats, GSL_lons
  REAL, ALLOCATABLE, DIMENSION(:,:) :: GSL_LST
  LOGICAL :: file_exists
  INTEGER :: mm, dd

  PRINT *,'Begin program add_GSL_WST_WPS.exe'
  CALL getarg(1,WPS_path)
  CALL getarg(2,WPS_file)
  CALL getarg(3,GSL_path)
  fname = TRIM(WPS_path) // '/' // TRIM(WPS_file)
  PRINT *,'Checking to see if WPS intermediate SST file exists: ' // TRIM(fname)
  INQUIRE(FILE=fname, EXIST=file_exists)
  IF (file_exists.EQV..FALSE.) THEN
   PRINT *, 'File does not exist: ' // TRIM(fname) 
   PRINT *, '  ... checking to see if file exists from previous day'
   READ(WPS_file(5:8),*) year
   READ(WPS_file(10:11),*) month
   READ(WPS_file(13:14),*) day
   DOY = day_of_year(year,month,day) - 1
   IF (DOY.NE.0) THEN
    mon_day = month_day(year,DOY)
   ELSE
    mon_day = 1231
    year = year - 1
   END IF
   mm = mon_day/100
   dd = MOD(mon_day,100)
   WRITE(year_str,'(i4.4)') year
   WRITE(mon_str,'(i2.2)') mm
   WRITE(day_str,'(i2.2)') dd
   fname = TRIM(WPS_path) // '/' // 'SST:' // year_str // '-' // mon_str // '-' // day_str // '_00'
   PRINT *,'Checking to see if WPS intermediate SST file exists: ' // TRIM(fname)
   INQUIRE(FILE=fname, EXIST=file_exists)
   IF (file_exists.EQV..FALSE.) THEN
    STOP 'WARNING: File from previous day: could not be found either ... exiting add_GSL_WST_WPS.exe' 
   END IF
  END IF
  PRINT *,'Reading in WPS intermediate SST file: ' // TRIM(fname)
  OPEN(IUNIT,FILE=TRIM(fname),FORM="UNFORMATTED")

  ! READ the first record, which gives the file version (should be 5 for WPS)
  READ (IUNIT) IFV
  !PRINT *,'IFV=',IFV

  ! READ the second record, common to all projections:
  READ (IUNIT) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
  !PRINT *,'HDATE=',HDATE
  !PRINT *,'XFCST=',XFCST
  !PRINT *,'MAPSOURCE=',MAP_SOURCE
  !PRINT *,'FIELD=',FIELD
  !PRINT *,'UNITS=',UNITS
  !PRINT *,'DESC=',DESC
  !PRINT *,'XLVL=',XLVL
  !PRINT *,'NX=',NX
  !PRINT *,'NY=',NY
  !PRINT *,'IPROJ=',IPROJ

  ! READ the third record, which depends on the projection:
  IF (IPROJ == 0) THEN
  !  This is the Cylindrical Equidistant (lat/lon) projection:
     READ (IUNIT) STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS
     !PRINT *,'Projection is Cylindrical Equidistant (lat/lon)'
     !PRINT *,'STARTLOC=',STARTLOC
     !PRINT *,'STARTLAT=',STARTLAT
     !PRINT *,'STARTLON=',STARTLON
     !PRINT *,'DELTALAT=',DELTALAT
     !PRINT *,'DELTALON=',DELTALON
     !PRINT *,'EARTH_RADIUS=',EARTH_RADIUS
  ELSEIF (IPROJ == 1) THEN
  ! This is the Mercator projection:
     READ (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, TRUELAT1, EARTH_RADIUS
     !PRINT *,'Projection is Mercator'
     !PRINT *,STARTLOC, STARTLAT, STARTLON, DX, DY, TRUELAT1, EARTH_RADIUS
  ELSEIF (IPROJ == 3) THEN
  ! This is the Lambert Conformal projection:
     READ (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, TRUELAT2, EARTH_RADIUS
     !PRINT *,'Projection is Lambert Conformal'
     !PRINT *,STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, TRUELAT2, EARTH_RADIUS
  ELSEIF (IPROJ == 4) THEN
  ! Gaussian projection
     READ (IUNIT) STARTLOC, STARTLAT, STARTLON, NLATS, DELTALON, EARTH_RADIUS
     !PRINT *,'Projection is Gaussian'
     !PRINT *,STARTLOC, STARTLAT, STARTLON, NLATS, DELTALON, EARTH_RADIUS
  ELSEIF (IPROJ == 5) THEN
  ! This is the Polar Stereographic projection:
     READ (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, EARTH_RADIUS
     !PRINT *,'Projection is Polar Stereographic'
     !PRINT *,STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, EARTH_RADIUS
  ENDIF

  ! READ the fourth record
  READ (IUNIT) IS_WIND_EARTH_REL
  !PRINT *,'IS_WIND_EARTH_REL=',IS_WIND_EARTH_REL

  ! Read in the field
  ALLOCATE(slab(NX,NY))
  READ (IUNIT) slab
  !PRINT *,MAXVAL(slab),MINVAL(slab)

  CLOSE(IUNIT)

!!!!!!!!!!!!!!!!!!!!
! READ IN GSL WSTs !
!!!!!!!!!!!!!!!!!!!!
! Read in GSL param file
! Read in parameters from param file
 !param_file = '/home/grim/process_MODIS_GSL/MODIS_GSL_namelist.asc'
 param_file = '/home/atecuser/modis_sst/gsl/MODIS_GSL_namelist.asc'
 PRINT *,'Reading in MODIS GSL param file: ' // TRIM(param_file)
 nlines = get_file_size_asc(LEN_TRIM(param_file),TRIM(param_file))
 param_name = 'max_lat'
 param_char = get_param_val(nlines,param_file,param_name)
 READ (param_char,*) max_lat
 param_name = 'min_lat'
 param_char = get_param_val(nlines,param_file,param_name)
 READ (param_char,*) min_lat
 param_name = 'max_lon'
 param_char = get_param_val(nlines,param_file,param_name)
 READ (param_char,*) max_lon
 param_name = 'min_lon'
 param_char = get_param_val(nlines,param_file,param_name)
 READ (param_char,*) min_lon
 param_name = 'latlon_inc'
 param_char = get_param_val(nlines,param_file,param_name)
 READ (param_char,*) latlon_inc

! Assemble GSL WST lat/lon arrays
 n_GSL_lats = INT(FLOOR((max_lat-min_lat)/latlon_inc))+1
 n_GSL_lons = INT(CEILING((max_lon-min_lon)/latlon_inc))+1
 ALLOCATE(GSL_lats(n_GSL_lats),GSL_lons(n_GSL_lons))
 DO iy=1,n_GSL_lats
  GSL_lats(iy) = min_lat+REAL(iy-1)*latlon_inc
 END DO
 DO ix=1,n_GSL_lons
  GSL_lons(ix) = min_lon+REAL(ix-1)*latlon_inc
 END DO

! Read in GSL WSTs
 ALLOCATE(GSL_LST(n_GSL_lons,n_GSL_lats))
 READ(WPS_file(5:8),*) year
 READ(WPS_file(10:11),*) month
 READ(WPS_file(13:14),*) day
 DOY = day_of_year(year,month,day)
 WRITE(DOY_str,'(i3.3)') DOY
 WRITE(year_str,'(i4.4)') year
 GSL_file = TRIM(GSL_path) // '/GSL_LST_final_' // year_str // DOY_str // '.bin'
 PRINT *,'Checking to see if GSL WST file exists: ' // TRIM(GSL_file)
 INQUIRE(FILE=GSL_file, EXIST=file_exists)
 IF (file_exists.EQV..FALSE.) THEN
  PRINT *, 'File does not exist: ' // TRIM(GSL_file)
  PRINT *, '  ... checking to see if file exists from previous day'
  DOY = DOY - 1
  IF (DOY.NE.0) THEN
   WRITE(DOY_str,'(i3.3)') DOY
  ELSE
   year = year - 1
   IF (MOD(year,4).NE.0) THEN
    DOY_str = '365'
   ELSE
    DOY_str = '366'
   END IF
   WRITE(year_str,'(i4.4)') year
  END IF
  GSL_file = TRIM(GSL_path) // '/GSL_LST_final_' // year_str // DOY_str // '.bin'
  PRINT *,'Checking to see if GSL WST file exists: ' // TRIM(GSL_file)
  INQUIRE(FILE=GSL_file, EXIST=file_exists)
  IF (file_exists.EQV..FALSE.) THEN
   PRINT *, 'WARNING: Unable to find Great Salt Lake WST file: ' // TRIM(GSL_file) 
   STOP '  ...  unchanged'
  END IF
 END IF
 PRINT *,'Reading in Great Salt Lake WSTs from: ' // TRIM(GSL_file)
 OPEN (OUNIT,FILE=TRIM(GSL_file), ACCESS='direct', FORM='unformatted',recl=4*n_GSL_lons*n_GSL_lats)
 READ (OUNIT,rec=1) GSL_LST
 CLOSE(OUNIT)
 GSL_LST = GSL_LST + 273.15
 !print *,MINVAL(GSL_LST),MAXVAL(GSL_LST)

! Assemble MODIS lat/lon arrays
 ALLOCATE(lats(NY),lons(NX))
 DO iy=1,NY
  lats(iy) = STARTLAT+REAL(iy)*DELTALAT
 END DO
 DO ix=1,NX
  lons(ix) = STARTLON+REAL(ix)*DELTALON
 END DO

! Find the nearest GSL lat/lon index for each MODIS index within min/max lat/lon
!  and fill in the MODIS SST array with the GSL LST values
 min_lat_ind = ind_nearest_coord(lats,NY,min_lat)
 max_lat_ind = ind_nearest_coord(lats,NY,max_lat)
 min_lon_ind = ind_nearest_coord(lons,NX,min_lon)
 max_lon_ind = ind_nearest_coord(lons,NX,max_lon)
 !print *,min_lat_ind,max_lat_ind,min_lon_ind,max_lon_ind
 PRINT *,'Inserting Great Salt Lake WSTs into MODIS SST field'
 DO iy=min_lat_ind,max_lat_ind
  lat_ind = ind_nearest_coord(GSL_lats,n_GSL_lats,lats(iy))
  DO ix=min_lon_ind,max_lon_ind
   lon_ind = ind_nearest_coord(GSL_lons,n_GSL_lons,lons(ix))
   slab(ix,iy) = GSL_LST(lon_ind,lat_ind)
  END DO
 END DO

 !lon_inds = index_true(NX,n_lon_inds,lons.GE.min_lon.AND.lons.LE.max_lon)
 !slab(min_lon_ind:max_lon_ind,min_lat_ind:max_lat_ind) = 270.
 
! Write out altered file in ascii format for easier plotting
! COMMENT OUT THIS LITTLE SECTION LATER, AS IT IS FOR TESTING ONLY
   !OPEN(OUNIT_temp, FILE='test.asc')
   !do iy = 1,NY
   ! WRITE(OUNIT_temp,'(3036E12.4)') (slab(ix,iy), ix=1,NX)
   !end do
   !CLOSE(OUNIT_temp)

!!!Write WPS intermediate file back out
  !fname = 'SST:2013-04-21_00'  ! remove this later to write back out the same file that was read in
  PRINT *,'Writing out edited WPS intermediate file: ' // TRIM(fname)
  OPEN(IUNIT,FILE=TRIM(fname),FORM="UNFORMATTED")
     write (IUNIT, IOSTAT=IERR) IFV

     ! WRITE the second record, common to all projections:

     write (IUNIT) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
     !print*, HDATE//"  ", XLVL, FIELD

     ! WRITE the third record, which depends on the projection:

     if (IPROJ == 0) then 

        !  This is the Cylindrical Equidistant (lat/lon) projection:
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS

     elseif (IPROJ == 1) then 

        ! This is the Mercator projection:
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, TRUELAT1, EARTH_RADIUS

     elseif (IPROJ == 3) then

        ! This is the Lambert Conformal projection:
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, TRUELAT2, EARTH_RADIUS
        

     elseif (IPROJ == 4) then

        ! Gaussian projection                         
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, NLATS, DELTALON, EARTH_RADIUS
        
     elseif (IPROJ == 5) then

        ! This is the Polar Stereographic projection:
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, EARTH_RADIUS

     endif

     
     WRITE (IUNIT) IS_WIND_EARTH_REL


     WRITE (IUNIT) slab

     CLOSE (IUNIT)

  DEALLOCATE(slab)
  PRINT *,'Program add_GSL_WST_WPS.exe complete'

end program sample_read
