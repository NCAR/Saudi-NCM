!=======================================================================
! CVS: $Id: WRF_cmdLine.f90,v 1.21 2019/01/07 20:50:52 sheu Exp $
! CVS: $Source: /cvs/apps/netcdf_utils/src/WRF_to_point_data/WRF_cmdLine.f90,v $
! CVS: $Name:  $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_cmdLine - Get input information from the command line
!
! !DESCRIPTION:  This module contains the subroutines for the command
!                line arguments for readWRF.exe (calculates soundings
!                and point data) and WRF_lltoxy.exe (converts lat/lon
!                to model grid (i,j).
!
! !REVISION HISTORY:
!
! 2006-Apr-24 - J. Schramm - first version, to use with operational WRF, 
!                            command line for readWRF.exe
! 2006-Sep-8  - J. Schramm - includes command line for WRF_lltoxy.exe
!
! !INTERFACE: ----------------------------------------------------------

module WRF_cmdLine

! !USES:

   use WRF_kinds
   use WRF_constants

!EOP

   implicit none

   private    ! except

! !PUBLIC TYPES:

   ! none

! !PUBLIC MEMBER FUNCTIONS:

   public :: arguments   ! Read and parse cmd line arguments for readWRF.exe
   public :: lltoxy_args ! Read and parse cmd line arguments for WRF_lltoxy.exe

! !PUBLIC DATA MEMBERS:

   ! none

!===============================================================================
contains
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: arguments -- Read and parse command line arguments from readWRF.exe
!
! !DESCRIPTION:
!    Read in the command line arguments and check for any missing or
!    inconsistent input.  Set default values.
!
! NOTE:  If no command line options are entered, the namelist is used.
!
! Usage: cmd [-h] -type output_flag -f inp_file [-s "site_name"] [-cycle_type "cycle_tag"]\
!             -lat lat -lon lon [-x xindex -y yindex] [-alt alt -coord zCoord] \!             [-writeStdout] [-equal] [-bufkit] [-ypg2] [-redi] \
!             [-range <range>] [-iaf] [-gatt GLOBAL_ATTRIBUTE] \
!             [-dims DIMENSION_NAME]
! -----
!
! With:
!  type     : Type of data to output (sfc_site, sounding or arb_point)
!  inp_file : Name of WRF netCDF input file to read 
!  site_name: Name of station to be written out with point data or sounding
!  lat      : Latitude of point data or sounding
!  lon      : Longitude of point data or sounding
!  x        : x index of point data or sounding (instead of -lat option)
!  y        : y index of point data or sounding (instead of -lon option)
!  alt      : Station elevation in meters (only used for sfc_site data)
!             Height of point data (AGL)  (only used for arb_point data)
!  coord    : Units of height coordinate (only used for arb_point data)
!                                        (h for meters above ground level)
!  cycle_type  : Cycle tag info for the data
!                                        (p for neights in hPa)
!  h        : Print this help message and exit.
!  writeStdout : flag for results to go to standard output
!  equal    : optional flag to indicate sfc output in FIELD=value
!  dz       : Soundings are interpolated into this fixed height interval (m)
!  topz     : Maximum height (m) of the sounding output
!  bufkit   : Output soundings in BUFKIT format
!  ypg2     : Output soundings in YPG format (AMSL) with extra header
!  redi     : Output soundings in REDI format
!  range    : Followed by range name to indicate REDI format for a particular
!             test range
!  iaf      : Output metcm format for IAF application when output_flag='metcm'
!
! Enter test -h for online help.
!
! !REVISION HISTORY:
!     2006-Apr-21 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine arguments(output_flag, path_and_file, station_name, lat, lon, &
                     zObsPt, zCoord, xindex, yindex, dz, topz, im, useLatLon, &
                     useEqual, writeBufkit, writeYPG2, writeREDI, writeIAF, &
                     rnge, use_gbc_file, lunit, output_type, gatt_list, dims_list)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(*),intent(out) :: output_flag    ! Type of output required
   character(*),intent(out) :: path_and_file  ! Path and name of input file
   character(*),intent(out) :: station_name   ! Station name
   character(*),intent(out) :: output_type    ! "fcst", "preli", or "final
   character(*),intent(out) :: rnge           ! Range (Test Center) name
   real(R8)    ,intent(out) :: lat            ! Latitude of data to get
   real(R8)    ,intent(out) :: lon            ! Latitude of data to get
   real(R8)    ,intent(out) :: zObsPt         ! Obs vertical coordinate
   character(*),intent(out) :: zCoord         ! Units of z coordinate
   real(R8)    ,intent(out) :: xindex         ! x (i) index
   real(R8)    ,intent(out) :: yindex         ! y (j) index
   real(R8)    ,intent(out) :: dz             ! vertical interval (m) for snd
   real(R8)    ,intent(out) :: topz           ! Max height (m) for snd
   integer(INT),intent(out) :: lunit          ! Unit specifier for output
   logical     ,intent(out) :: useLatLon      ! true for lat/lon
   logical     ,intent(out) :: useEqual       ! sfc output uses FIELD=value
   logical     ,intent(out) :: writeBufkit    ! Soundings output in BUFKIT
   logical     ,intent(out) :: writeYPG2      ! Soundings output in YPG2
   logical     ,intent(out) :: writeREDI      ! Soundings output in REDI
   logical     ,intent(out) :: writeIAF       ! METCM Soundings for the IAF
   logical     ,intent(out) :: use_gbc_file   ! Flag to indicaate input file
                                              ! is GBC trimmed-down WRF file
   character(*),intent(out) :: gatt_list
   character(*),intent(out) :: dims_list

!EOP

   !----- local -----

   INTEGER, EXTERNAL :: ABORT

   character(len=200) :: harg,adz,aval,atopz

   integer :: ierr, i, numarg
   logical :: present_type   = .FALSE.
   logical :: present_inp    = .FALSE.
   logical :: present_site   = .FALSE.
   logical :: present_lat    = .FALSE.
   logical :: present_lon    = .FALSE.
   logical :: present_hgt    = .FALSE.
   logical :: present_zCoord = .FALSE.
   logical :: present_xind   = .FALSE.
   logical :: present_yind   = .FALSE.
   logical :: present_dz     = .FALSE.
   logical :: present_topz   = .FALSE.
   logical :: present_help   = .FALSE.
   logical :: ldebug
   logical :: present_writeStdout   = .FALSE.
   logical :: present_equal  = .FALSE.
   logical :: present_bufkit = .FALSE.
   logical :: present_ypg2   = .FALSE.
   logical :: present_redi   = .FALSE.
   logical :: present_iaf    = .FALSE.
   logical :: present_range  = .FALSE.
   logical :: present_gbc    = .FALSE.
   logical :: present_gatt   = .FALSE.
   logical :: present_dims   = .FALSE.
   character (len=200) :: cmd
   integer :: iab, ibeg_file,iend_file, ift, im

!------------------------------------------------------------------------------!

   CALL getarg(0, cmd)

   numarg = command_argument_count()

   i = 1
   ldebug = .false.
   lunit = 30              ! Default for command line use - write sounding
   lstdout = 6             ! Default for command line use - write messages
   topz = 0.
   gatt_list = ""
   dims_list = ""

   DO while ( i <= numarg)

      CALL getarg(i, harg)

      IF ((harg (1:2) == "-h") .OR. (harg (1:4) == "-man")) THEN
          present_help = .TRUE.
      ELSEIF (harg (1:5) == "-type") THEN
         i = i + 1
         CALL getarg(i, harg)
         output_flag = TRIM (harg)
         present_type = .TRUE.
      ELSEIF (harg (1:2) == "-f") THEN
         i = i + 1
         CALL getarg(i, harg)
         path_and_file = TRIM (harg)
         present_inp = .TRUE.
      ELSEIF (harg (1:2) == "-s") THEN
         i = i + 1
         CALL getarg(i, harg)
         station_name = TRIM (harg)   
         present_site = .TRUE.
      ELSEIF (harg (1:4) == "-lat") THEN
         i = i + 1
         CALL getarg(i, harg)
         read (harg,*) lat                 ! Convert char string to real
         present_lat = .TRUE.
      ELSEIF (harg (1:4) == "-lon") THEN
         i = i + 1
         CALL getarg(i, harg)
         read (harg,*) lon
         present_lon = .TRUE.
      ELSEIF (harg (1:4) == "-alt") THEN
         i = i + 1
         CALL getarg(i, harg)
         read (harg,*) zObsPt
         present_hgt = .TRUE.
      ELSEIF (harg (1:6) == "-coord") THEN
         i = i + 1
         CALL getarg(i, harg)
         zCoord = TRIM (harg)
         present_zCoord = .TRUE.
      ELSEIF (harg (1:2) == "-x") THEN
         i = i + 1
         CALL getarg(i, harg)
         read (harg,*) xindex
         present_xind = .TRUE.
      ELSEIF (trim(harg) == "-y") THEN
         i = i + 1
         CALL getarg(i, harg)
         read (harg,*) yindex
         present_yind = .TRUE.
      ELSEIF (harg (1:11) == "-cycle_type") THEN
         i = i + 1
         CALL getarg(i, harg)
         output_type = TRIM (harg)
      ELSEIF (harg (1:12) == "-writeStdout") THEN
         present_writeStdout = .TRUE.
      ELSEIF (harg (1:7) == "-bufkit") THEN
         present_bufkit = .TRUE.
      ELSEIF (harg (1:5) == "-ypg2") THEN
         present_ypg2 = .TRUE.
      ELSEIF (harg (1:5) == "-redi") THEN
         present_redi = .TRUE.
      ELSEIF (harg (1:4) == "-iaf") THEN
         present_iaf = .TRUE.
      ELSEIF (harg (1:6) == "-range") THEN
         present_range = .TRUE.
         i = i + 1
         call getarg(i, harg)
         rnge = TRIM(harg)
      ELSEIF (harg (1:4) == "-gbc") THEN
         present_gbc = .TRUE.
      ELSEIF (harg(1:3) == '-dz') THEN
         i = i + 1
         CALL getarg(i, adz)
         ift = index(adz,'ft')
         im  = index(adz,'m')
         if (ift > 0) then
            aval=adz(1:ift-1)
            read(aval,*) dz
            dz = dz*ft2m
         else if(im > 0) then
            aval=adz(1:im-1)
            read(aval,*) dz
         else
            print*,'Wrong unit specified for dz; attach m or ft to value!'
            stop
         end if
         present_dz = .TRUE.
      ELSEIF (harg(1:5) == '-topz') THEN
         i = i + 1
         CALL getarg(i, atopz)
         ift = index(atopz,'ft')
         im  = index(atopz,'m')
         if (ift > 0) then
            aval=atopz(1:ift-1)
            read(aval,*) topz
            topz = topz*ft2m
         else if(im > 0) then
            aval=atopz(1:im-1)
            read(aval,*) topz
         else
            print*,'Wrong unit specified for topz; attach m or ft to value!'
            stop
         end if
         present_topz = .TRUE.
      ELSEIF (harg(1:5) == '-gatt') THEN
         i = i + 1
         CALL getarg(i, gatt_list)
         present_gatt = .TRUE.
      ELSEIF (harg(1:5) == '-dims') THEN
         i = i + 1
         CALL getarg(i, dims_list)
         present_dims = .TRUE.
      ELSEIF (harg(1:6) == '-equal') then
         present_equal = .TRUE.
      ELSE
         path_and_file = TRIM (harg)
         present_inp = .TRUE.
      ENDIF

      i = i + 1

   ENDDO

! print help and exit

    IF (present_help) THEN
         CALL prelog (lstdout,0)
         CALL help   (lstdout)
    ENDIF

!------------------------------------------------------------------------------!
!-------------------- Check mandatory arguments -------------------------------!
!------------------------------------------------------------------------------!

    IF (.NOT. present_inp) THEN
          CALL PRELOG (lunit,1)
          WRITE (lunit,'(A)') "ERROR: missing input file name"
          WRITE (lunit,'(A)') "   Add -f /path/file_name to command line"
          WRITE (lunit,'(/,3A)') "Use: ", trim (cmd)," -h for help"
          STOP 'Aborting....'
    ENDIF

    if (present_gatt .or. present_dims) then
       output_flag = " "
       station_name = " "
       lat = 0.
       lon = 0.
       zObsPt = 0.
       zCoord = " "
       xindex = 0.
       yindex = 0.
       dz = 0.
       topz = 0.
       im = 0
       useLatLon = .FALSE.
       useEqual = .FALSE.
       writeBufkit = .FALSE.
       writeYPG2 = .FALSE.
       writeREDI = .FALSE.
       writeIAF = .FALSE.
       use_gbc_file = .FALSE.
       output_type = " "
    else
       IF (.NOT. present_type) THEN
             CALL PRELOG (lunit,1)
             WRITE (lunit,'(A)') "ERROR: missing data type to output"
             WRITE (lunit,'(A)') "       Add -type to command line and set to"
             WRITE (lunit,'(A)') "       sfc_site sounding arb_point metcm"
             WRITE (lunit,'(/,3A)') "Use: ", trim (cmd)," -h for help"
             STOP 'Aborting....'
       ENDIF
       IF (.NOT. present_site) THEN          ! Set to default space
               station_name = " "
       ENDIF
       IF ((present_lon  .or. present_lat) .and. &
           (present_xind .or. present_yind)) THEN
             CALL PRELOG (lunit,1)
             WRITE (lunit,'(A)') "ERROR: Need either (lat,lon) or (x index, y index)"
             WRITE (lunit,'(/,3A)') "Use: ", trim (cmd)," -h for help"
             STOP 'Aborting....'
       ENDIF
       IF ((present_lon .and. .NOT. present_lat) .or. &
           (present_lat .and. .NOT. present_lon)) THEN
             CALL PRELOG (lunit,1)
             WRITE (lunit,'(A)') "ERROR: Need BOTH latitude and longitude"
             WRITE (lunit,'(/,3A)') "Use: ", trim (cmd)," -h for help"
             STOP 'Aborting....'
       ENDIF
       IF ((present_xind .and. .NOT. present_yind) .or. &
           (present_yind .and. .NOT. present_xind)) THEN
             CALL PRELOG (lunit,1)
             WRITE (lunit,'(A)') "ERROR: Need BOTH x-index and y-index"
             WRITE (lunit,'(/,3A)') "Use: ", trim (cmd)," -h for help"
             STOP 'Aborting....'
       ENDIF
       IF (trim(output_flag) .eq. "arb_point" ) THEN
          IF (.NOT. present_hgt .or. .NOT. present_zCoord) THEN
             CALL PRELOG (lunit,1)
             WRITE (lunit,'(A)') "ERROR: Need BOTH alitiude and units"
             WRITE (lunit,'(A)') "ERROR: for arb_point case"
             WRITE (lunit,'(/,3A)') "Use: ", trim (cmd)," -h for help"
             STOP 'Aborting....'
          ENDIF
       ENDIF
       IF (trim(output_flag) .eq. "sfc_site" .and. .NOT. present_hgt) THEN
             zObspt = zmissing
       ENDIF
       IF (present_lon .and. present_lat) useLatLon = .true.
       IF (present_xind .and. present_yind) useLatLon = .false.
       IF (present_writeStdout) THEN
             lunit = 6              ! write sounding to stdout
             lstdout = 30             ! write messages to file
       ENDIF
       If (present_equal) then
          useEqual = .true.
       else
          useEqual = .false.
       end if

       if (present_bufkit) then
          writeBufkit = .true.
       else
          writeBufkit = .false.
       end if

       if (present_ypg2) then
          writeYPG2 = .true.
       else
          writeYPG2 = .false.
       end if

       if (present_redi) then
          writeREDI = .true.
          if(.not. present_range) then
            print*,'REDI format also requires range name, "-range <range>"'
            stop
          endif
       else
          writeREDI = .false.
       end if

       if (present_iaf) then
          writeIAF = .true.
       else
          writeIAF = .false.
       end if

       if (present_gbc) then
          use_gbc_file = .true.
       else
          use_gbc_file = .false.
       end if

!------------------------------------------------------------------------------!
!-------------------------- Print out arguments -------------------------------!
!------------------------------------------------------------------------------!
       WRITE (lstdout,'(/,A)') "The options you have chosen are:"
             WRITE (lstdout,'(/,a8,A)')   "-type : ", trim(output_flag)
             WRITE (lstdout,'(a8,A)')     "-f    : ", trim(path_and_file)
       if (present_site  ) WRITE (lstdout,'(a8,A)')     "-s    : ", trim(station_name)
       if (present_lat   ) WRITE (lstdout,'(a8,f10.4)') "-lat  : ", lat
       if (present_lon   ) WRITE (lstdout,'(a8,f10.4)') "-lon  : ", lon
       if (present_xind  ) WRITE (lstdout,'(a8,f10.4)') "-x    : ", xindex
       if (present_yind  ) WRITE (lstdout,'(a8,f10.4)') "-y    : ", yindex
       if (present_hgt   ) WRITE (lstdout,'(a8,f10.4)') "-alt  : ", zObsPt
       if (present_zCoord) WRITE (lstdout,'(a8,A)')     "-coord: ", trim(zCoord)
       if (present_dz)     WRITE (lstdout,'(a8,A)')     "-dz   : ", trim(adz)
       if (present_topz)   WRITE (lstdout,'(a8,A)')     "-topz : ", trim(atopz)
       WRITE (lstdout,'(A)') ""

    end if ! close if (present_gatt)

END subroutine arguments

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: lltoxy_args -- Read and parse command line arguments from 
!                           WRF_lltoxy.exe
!
! !DESCRIPTION:
!    Read in the command line arguments and check for any missing or
!    inconsistent input.  
!
! Usage: WRF_lltoxy.exe [-h] inp_file -lat lat -lon lon 
! -----
!
! With:
!  inp_file : Name of WRF netCDF input file to read 
!  lat      : Latitude of point data or sounding
!  lon      : Longitude of point data or sounding
!  useMinInside : Flag indicating to apply margin close to boundaries
!  h        : Print this help message and exit.
!
! Enter test -h for online help.
!
! !REVISION HISTORY:
!     2006-Sep-8 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine lltoxy_args(path_and_file, lat, lon, use_min_inside, use_geo_file, lunit)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(*),intent(out) :: path_and_file  ! Path and name of input file
   real(R8)    ,intent(out) :: lat            ! Latitude of data to get
   real(R8)    ,intent(out) :: lon            ! Latitude of data to get
   integer(INT),intent(out) :: lunit          ! Unit specifier for output
   logical     ,intent(out) :: use_min_inside ! Flag to apply a security factor of 5 grid points inside domain.
   logical     ,intent(out) :: use_geo_file   ! Flag to indicate input file is a geo_em_d0*.nc file

!EOP

   !----- local -----

   INTEGER, EXTERNAL :: ABORT

   character(len=200) :: harg

   integer :: ierr, i, numarg
   logical :: present_inp    = .FALSE.
   logical :: present_lat    = .FALSE.
   logical :: present_lon    = .FALSE.
   logical :: present_help   = .FALSE.
   character (len=200) :: cmd
   integer :: iab, ibeg_file,iend_file
  
!------------------------------------------------------------------------------!

   CALL getarg(0, cmd)

   numarg = command_argument_count()

   i = 1
   lunit = 6              ! Default for command line use

   use_min_inside = .FALSE.

   use_geo_file = .FALSE.

   DO while ( i <= numarg)

      CALL getarg(i, harg)

      IF ((harg (1:2) == "-h") .OR. (harg (1:4) == "-man")) THEN
          present_help = .TRUE.
      ELSEIF (harg (1:2) == "-f") THEN
         i = i + 1
         CALL getarg(i, harg)
         path_and_file = TRIM (harg)
         present_inp = .TRUE.
      ELSEIF (harg (1:4) == "-lat") THEN
         i = i + 1
         CALL getarg(i, harg)
         read (harg,*) lat                 ! Convert char string to real
         present_lat = .TRUE.
      ELSEIF (harg (1:4) == "-lon") THEN
         i = i + 1
         CALL getarg(i, harg)
         read (harg,*) lon
         present_lon = .TRUE.
      ELSEIF (harg (1:13) == "-useMinInside") THEN
         i = i + 1
         use_min_inside = .TRUE.
      ELSEIF (harg (1:13) == "-useGeoFile") THEN
         i = i + 1
         use_geo_file = .TRUE.
      ELSE
         path_and_file = TRIM (harg)
         present_inp = .TRUE.
      ENDIF

      i = i + 1

   ENDDO

! print help and exit

    IF (present_help) THEN
         CALL prelog (lunit,0)
         CALL lltoxy_help   (lunit)
    ENDIF

!------------------------------------------------------------------------------!
!-------------------- Check mandatory arguments -------------------------------!
!------------------------------------------------------------------------------!

    IF (.NOT. present_inp) THEN
          CALL PRELOG (lunit,1)
          WRITE (lunit,'(A)') "ERROR: missing input file name"
          WRITE (lunit,'(A)') "   Add -f /path/file_name to command line"
          WRITE (lunit,'(/,3A)') "Use: ", trim (cmd)," -h for help"
          STOP 'Aborting....'
    ENDIF
    IF ((present_lon .and. .NOT. present_lat) .or. &
        (present_lat .and. .NOT. present_lon)) THEN
          CALL PRELOG (lunit,1)
          WRITE (lunit,'(A)') "ERROR: Need BOTH latitude and longitude"
          WRITE (lunit,'(/,3A)') "Use: ", trim (cmd)," -h for help"
          STOP 'Aborting....'
    ENDIF

!------------------------------------------------------------------------------!
!-------------------------- Print out arguments -------------------------------!
!------------------------------------------------------------------------------!
!   WRITE (lunit,'(/,A)') "The options you have chosen are:"
!                         WRITE (lunit,'(a8,A)')     "-f    : ", trim(path_and_file)
!   if (present_lat   ) WRITE (lunit,'(a8,f10.4)') "-lat  : ", lat
!   if (present_lon   ) WRITE (lunit,'(a8,f10.4)') "-lon  : ", lon
!   WRITE (lunit,'(A)') ""

END subroutine lltoxy_args

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: help -- Print help info
!
! !DESCRIPTION:
!     Print out help infomation about command line options
!
! !REVISION HISTORY:
!     2006-Apr-21 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

 subroutine help (lunit)
!------------------------------------------------------------------------------!
     IMPLICIT none
     INTEGER :: lunit
     INTEGER :: iab
     INTEGER, EXTERNAL :: ABORT
     CHARACTER (len=200) :: cmd
!------------------------------------------------------------------------------!

     CALL getarg(0, cmd)

     IF (lunit > 0) THEN
         CALL prelog (lunit,0)
         WRITE (lunit,'(A)') "Help message:"
     ENDIF

     WRITE (lunit,*) "Usage: ",trim (cmd), &
     " [-h] -type output_flag -f inp_file [-s site_name] [-writeStdout] \\"
     WRITE (lunit,'(A)') "  -lat lat -lon lon [-x xindex -y yindex] \\"
     WRITE (lunit,'(A)') " [-alt alt -coord zCoord] [-equal] [-bufkit] \\"
     WRITE (lunit,'(A)') " [-ypg2] [-redi] [-range <range>] [-iaf] \\"
     WRITE (lunit,'(A)') " [-gatt GLOBAL_ATTRIBUTE]"
     WRITE (lunit,'(A)') " [-dims DIMENSION_NAME]"
     WRITE (lunit,'(A)') "-----"


     WRITE (lunit,'(7X,2A)') "To extract point data from WRF output files."
     WRITE (lunit,'(7X,2A)') "Arguments in [ ] brackets are optional."

     WRITE (lunit,'(A)')"With:"
     WRITE (lunit,'(A)')  "----"
     WRITE (lunit,'(1X,A)')"type     : Type of data to output (sfc_site, sounding metcm or arb_point)"
     WRITE (lunit,'(1X,A)')"inp_file : Name of WRF netCDF input file to read"
     WRITE (lunit,'(1X,A)')"site_name: Name of station to be written out with "// &
                                       "point data or sounding"
     WRITE (lunit,'(1X,A)')"writeStdout  : flag for results to go to standard output"
     WRITE (lunit,'(1X,A)')"equal    : optional flag to indicate sfc output in FIELD=value"
     WRITE (lunit,'(1X,A)')"bufkit   : optional flag to indicate soundings output in BUFKET format"
     WRITE (lunit,'(1X,A)')"ypg2     : optional flag to indicate soundings output in YPG required format (AMSL) with extra header"
     WRITE (lunit,'(1X,A)')"redi     : optional flag to indicate soundings output in REDI format"
     WRITE (lunit,'(1X,A)')"range    : optional argement copuled with redi to indicate REDI format for a partcular test range"
     WRITE (lunit,'(1X,A)')"iaf      : optional flag to indicate METCM format for the IAF application"
     WRITE (lunit,'(1X,A)')"lat      : Latitude of point data or sounding"
     WRITE (lunit,'(1X,A)')"lon      : Longitude of point data or sounding"
     WRITE (lunit,'(1X,A)')"x        : x index of point data or sounding "// &
                                       "(instead of -lat option)"
     WRITE (lunit,'(1X,A)')"y        : y index of point data or sounding "// &
                                       "(instead of -lon option)"
     WRITE (lunit,'(1X,A)')"alt      : Height of arbitrary point data AGL "// &
                                       "(arb_point) or station elevation "// &
                                       "(sfc_site)"
     WRITE (lunit,'(1X,A)')"coord    : Units of height coordinate "// &
                                       "(only used for arb_point data)"
     WRITE (lunit,'(8x,A)')"    (h for meters above ground level)"
     WRITE (lunit,'(8x,A)')"    (p for neights in hPa)"
     WRITE (lunit,'(1x,A)')"dz       : veritical interval (m) for soundings"
     WRITE (lunit,'(1x,A)')"topz     : Max height (m) for soundings"
     WRITE (lunit,'(1X,A)')"gatt     : Print value of a gloabal attribute"
     WRITE (lunit,'(1X,A)')"dims     : Print value of a dimension"
     WRITE (lunit,'(1X,A)')"h        : Print this help message and exit."
     WRITE (lunit,'(A)') ""
!------------------------------------------------------------------------------!
!------------- Print out some examples-----------------------------------------!
!------------------------------------------------------------------------------!

     WRITE (lunit,'(A)')"EXAMPLES:"
     WRITE (lunit,'(A)')  "----"
     WRITE (lunit,'(1X,A)')"Sounding:"
     WRITE (lunit,'(4x,3A)') trim (cmd)," -type sounding -f "// &
           "/path/file_name -s ""SAMS 1"" -lat 40.1 -lon -112.9"
     WRITE (lunit,'(4x,3A)') trim (cmd)," -type sounding -f "// &
           "/path/file_name -s ""SAMS 1"" -x 44.5 -y 41.5"

     WRITE (lunit,'(A)') ""
     WRITE (lunit,'(1X,A)')"Surface Site Data:"
     WRITE (lunit,'(4x,3A)') trim (cmd)," -type sfc_site -f "// &
           "/path/file_name -s ""GUADALUPE PASS, TX"" -alt 1661 -lat 30.831 -lon -104.809"

     WRITE (lunit,'(/,1X,A)')"Arbitrary Point Data:"
     WRITE (lunit,'(4x,3A)') trim (cmd)," -type arb_point -f "// &
           "/path/file_name -lat 40.1 -lon -112.9 -alt 500. -coord p"
     WRITE (lunit,'(A)') ""

     WRITE (lunit,'(/,1X,A)')"Print a global attribute:"
     WRITE (lunit,'(4x,3A)') trim (cmd)," -f /path/file_name "// &
           "-gatt BOTTOM-TOP_GRID_DIMENSION"
     WRITE (lunit,'(A)') ""

     WRITE (lunit,'(/,1X,A)')"Print a dimensio size:"
     WRITE (lunit,'(4x,3A)') trim (cmd)," -f /path/file_name "// &
           "-dims east_west"
     WRITE (lunit,'(A)') ""

     CALL prelog (lunit,0)
     STOP 'Aborting....'

 END subroutine help

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: lltoxy_help -- Print help info for WRF_lltoxy.exe
!
! !DESCRIPTION:
!     Print out help infomation about command line options
!
! !REVISION HISTORY:
!     2006-Sep-8 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

 subroutine lltoxy_help (lunit)
!------------------------------------------------------------------------------!
     IMPLICIT none
     INTEGER :: lunit
     INTEGER :: iab
     INTEGER, EXTERNAL :: ABORT
     CHARACTER (len=200) :: cmd
!------------------------------------------------------------------------------!

     CALL getarg(0, cmd)

     IF (lunit > 0) THEN
         CALL prelog (lunit,0)
         WRITE (lunit,'(A)') "Help message:"
     ENDIF

     WRITE (lunit,*) "Usage: ",trim (cmd), &
     " [-h] -f inp_file  -lat lat -lon lon [-stdout]"
     WRITE (lunit,'(A)') "-----"


     WRITE (lunit,'(7X,2A)') "To convert lat/lon to WRF model grid (i,j):"
     WRITE (lunit,'(7X,2A)') "Arguments in [ ] brackets are optional."

     WRITE (lunit,'(A)')"With:"
     WRITE (lunit,'(A)')  "----"
     WRITE (lunit,'(1X,A)')"inp_file : Name of WRF netCDF input file to read"
     WRITE (lunit,'(1X,A)')"writeStdout  : Results go to standard output"
     WRITE (lunit,'(1X,A)')"lat      : Latitude of point data or sounding"
     WRITE (lunit,'(1X,A)')"lon      : Longitude of point data or sounding"
     WRITE (lunit,'(1X,A)')"h        : Print this help message and exit."
     WRITE (lunit,'(A)') ""
!------------------------------------------------------------------------------!
!------------- Print out an example--------------------------------------------!
!------------------------------------------------------------------------------!

     WRITE (lunit,'(A)')"EXAMPLE:"
     WRITE (lunit,'(A)')  "----"
     WRITE (lunit,'(4x,3A)') trim (cmd)," -f "// &
           "/path/file_name -lat 40.1 -lon -112.9"
     WRITE (lunit,'(A)') ""

     CALL prelog (lunit,0)
     STOP 'Aborting....'

 END subroutine lltoxy_help

!------------------------------------------------------------------------------!
SUBROUTINE prelog (iunit,skipped_lines)

      IMPLICIT NONE

      INTEGER, INTENT (in) :: iunit    ! Logical Unit to write in
      INTEGER, INTENT (in) :: skipped_lines ! Skip line before writing
      INTEGER              :: l,ierr
      INTEGER              :: iab
      INTEGER, EXTERNAL    :: ABORT
      CHARACTER (LEN=24)   :: sdate

      DO l = 1, skipped_lines
         WRITE (UNIT = iunit, FMT = '(A)') ""
      ENDDO

      RETURN

      CALL DATE_AND_TIME (sdate (1:8),sdate (9:18))

      WRITE (UNIT = iunit, FMT = '(6A)',ADVANCE = 'no',IOSTAT=ierr) &
            sdate (9:10),":",sdate (11:12),":",sdate (13:14)," "

      IF (ierr /= 0) THEN
          WRITE (UNIT = iunit, FMT = '(A,I6)') &
         "ERROR in prelog, iostat = ",ierr
          WRITE (iunit,'(A)') "Abort"
          STOP 'Aborting....'
      ENDIF

END SUBROUTINE prelog

!=======================================================================

end module WRF_cmdLine

!=======================================================================
