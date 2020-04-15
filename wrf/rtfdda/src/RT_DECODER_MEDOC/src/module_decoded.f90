 MODULE module_decoded
!------------------------------------------------------------------------------!
! Module defining the ASCII formats for observation ingestion in MM5 
! RT-FDDA/3D-VAR programs.
!
! Francois VANDENBERGHE, September 2004
! vandenb@ucar.edu
! Copyright UCAR [RAP] 1996 - 2004. All Rights Reserved.
!------------------------------------------------------------------------------!
!  Starting date for julian seconds

   CHARACTER (LEN =  19), PARAMETER :: decoded_date_start= '1970-01-01_00:00:00'

   INTEGER , PARAMETER                            ::  undefined1    =  999999
   REAL    , PARAMETER                            ::  undefined1_r  =  999999.
   INTEGER , PARAMETER                            ::  undefined2    = -999999
   REAL    , PARAMETER                            ::  undefined2_r  = -999999.
   INTEGER , PARAMETER                            ::  missing       = -888888
   REAL    , PARAMETER                            ::  missing_r     = -888888.
   INTEGER , PARAMETER                            ::  end_data      = -777777
   REAL    , PARAMETER                            ::  end_data_r    = -777777.

   !  Define error return codes used by 'read_measurements' routine.

   INTEGER , PARAMETER                            ::  ok       = 0 , &
                                                      eof_err  = 1 , &
                                                      no_data  = 2 , &
                                                      read_err = 3

!  Output format ground info (1 record at the beginning of sounding)
!  Format has been appended for iwv (Integrated water vapor)

   CHARACTER ( LEN = 120 ), PARAMETER :: rpt_format =  &
  '(2F20.5,2A40,2A40,1F20.5,5I10,3L10,2I10,A20,13(F13.5,I7),1(:,F13.5,I7))'

!  Output format for sounding level (1 record per level)
!  Format has been appended for rf (atmospheric refractivity)

   CHARACTER ( LEN = 120 ), PARAMETER :: meas_format = '(10(F13.5,I7),1(:,F13.5,I7))'

!  Output format for end of sounding (1 record at the end of sounding)

   CHARACTER ( LEN = 120 ), PARAMETER :: end_format = '(3(I7))'

!  Format for RIP stationlist file

   CHARACTER (LEN=80)  :: fmt_rip
   PARAMETER (fmt_rip = '(A21,1X,A5,1X,A3,1X,F6.2,1X,F7.2,1X,I5,1X,A5)')

!------------------------------------------------------------------------------!

   TYPE location_type

      !  The fields in this record uniquely identify the source of the 
      !  data, so that duplicates of data can be merged or discarded.
      !  The horizontal location of this report (assumed constant, even
      !  for balloon ascent) is geven by the lat/lon of the site.

!  Records read in the input file (defined by the decoder

      REAL                   :: latitude  , &   ! latitude (+ degrees east)
                                longitude       ! longitude (+ degrees north)

      CHARACTER ( LEN = 40 ) :: id , &          ! 5 digit identifier, 
                                                ! consisting of 2 digit block
                                                ! number and a 3 digit
                                                ! identifier (for soundings)
                                                ! for WMO sttn; non digit
                                                ! for other sources
                                name            ! The name corresponds to
                                                ! the id (is obtained from id
                                                ! in the program that is 
                                                ! source of data

   END TYPE location_type


!------------------------------------------------------------------------------!

   TYPE source_info

!  Records read in the input file (defined by the decoder):

      CHARACTER ( LEN = 40 ) :: platform , &    ! description of the
                                                ! measurement device
                                source          ! GTS data, NCAR ADP files,
                                                ! bogus information, etc
      REAL                   :: elevation       ! station elevation

      !  During the decoding process, how many errors (non conforming
      !  codes) were encountered, and how many warnings (this is a subjective
      !  call based on repeated incorrect -- but probably not garbled --
      !  decoded codes).  If a bulletin is completely garbled, the logical
      !  flag to not consider this report is set.

      INTEGER              :: num_vld_fld , & ! number of valid fields in the
                                              ! entire report; used as the
                                              ! first tie-breaker in deciding
                                              ! which conflicting data items
                                              ! to keep if have duplicate rpts
                              num_error , &   ! number of errors 
                                              ! encountered during the
                                              ! decoding process
                              num_warning , & ! number of warnings
                                              ! encountered during the
                                              ! decoding process
                              seq_num , &     ! sequence numbers that tell
                                              ! which of 2 reports is more
                                              ! recent.
                              num_dups        ! number of duplicates found of
                                              ! this observation 
      LOGICAL              :: is_sound        ! is-a-sounding tells whether
                                              ! the observation possibly has
                                              ! multiple levels vs having
                                              ! only one level for srfc ob.
      LOGICAL              :: bogus           ! T/F if this is a bogus 
                                              ! observation
      LOGICAL              :: discard         ! Tells whether this observation
                                              ! has been found to be a dup
                                              ! AND has been discarded or
   END TYPE source_info

!------------------------------------------------------------------------------!

   TYPE field

      !  Defines a data type consisting of a paired data value (real) with a
      !  quality control flag that holds a binary-coded combination of error
      !  codes; the codes  identify possible problems with the data.

!  Records read in the input file (defined by the decoder):

      REAL                   :: data            ! Observation value y^o
      INTEGER                :: qc              !  Quality control flags
                                                !  that are 0 if data is
                                                !  good, or different

   END TYPE field

!------------------------------------------------------------------------------!

   TYPE terrestrial

      !  The data that will occur, at most, once during a report is
      !  listed here.  These are typically terrestrial measured values.  The
      !  surface met fields are stored in a separate TYPE, to allow a
      !  POINTER to the next level (if one exists).  This needs to be a
      !  separate TYPE so as to allow a POINTER to it

!  Records read in the input file (defined by the decoder):

      TYPE ( field )         :: slp       , &   ! sea level pressure
                                ref_pres  , &   ! reference pres level for
                                                ! the thickness
                                ground_t  , &   ! ground temperature
                                sst       , &   ! sea surface temperature
                                psfc      , &   ! surface pressure
                                precip    , &   ! precipitation accumulation
                                t_max     , &   ! daily temperature max
                                t_min     , &   ! daily temperature min
                                t_min_night,&   ! min overnight temperature
                                p_tend03  , &   ! pressure tendency in 3hr
                                p_tend24  , &   ! pressure tendency in 24hr
                                cloud_cvr , &   ! total cloud cover (oktas)
                                ceiling         ! height of lowest cloud base

   END TYPE terrestrial

!------------------------------------------------------------------------------!

   TYPE time_info

      !  Report time: the valid time of the report.
      !  The largest INTEGER values require only 8 digits,
      !  so that this should function properly with 32-bit INTEGERS.

!  Records read in the input file (defined by the decoder):

      INTEGER                :: sut      , &    ! number seconds since 1 Jan
                                                ! 0000 UTC 1970
                                julian          ! Julian day
      CHARACTER ( LEN = 14 )    date_char       ! CCYYMMDDHHmmss date

   END TYPE time_info

!------------------------------------------------------------------------------!

   TYPE meas_data

      !  The met data involved with this program is defined in this TYPE.  The
      !  standard state variables (wind, temperature, moisture, with pressure
      !  and/or height to fix the vertical location) are stored here.  For
      !  single level observations, only one of these records is used per
      !  observation.   For multi-level reports, a linked list of these
      !  measurement TYPEs is generated.

!  Records read in the input file (defined by the decoder):

      TYPE ( field )         :: pressure    , & ! pressure of observation
                                height      , & ! height (above sea level)
                                temperature , & ! 
                                dew_point   , & !
                                speed       , & !
                                direction   , & !
                                u           , & ! u and v components of wind
                                v           , & ! are derived from spd and dir
                                rh          , & !
                                thickness

   END TYPE meas_data

!------------------------------------------------------------------------------!

   TYPE measurement

      TYPE ( meas_data )               :: meas  ! contains data and qc code
      TYPE ( measurement ) ,  POINTER  :: next  ! the met info is handled
                                                ! as a linked list of the
                                                ! measurement type

   END TYPE measurement

!------------------------------------------------------------------------------!

   TYPE report
                                               ! this is the entire report
      TYPE ( location_type ) :: location       ! for a single time, from a
      TYPE ( source_info )   :: info           ! single reporting platform,
      TYPE ( time_info )     :: valid_time     ! a sounding, surface, buoy,
      TYPE ( terrestrial )   :: ground         ! aircraft or ship report
      TYPE ( measurement ) , &
               POINTER       :: surface        ! Link list
      TYPE ( meas_data ), DIMENSION (:), POINTER :: each

   END TYPE report

!------------------------------------------------------------------------------!

CONTAINS
!------------------------------------------------------------------------------!

  SUBROUTINE fill_decoded (obs, medoc, i, j, nlevels, name, ierr)
!------------------------------------------------------------------------------!
      USE module_medoc
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      INTEGER, INTENT (in)            :: i, j, nlevels
      INTEGER, INTENT (out)           :: ierr

      TYPE (medoc_type), INTENT (in)  :: medoc
      TYPE (report),     INTENT (out) :: obs

      CHARACTER (LEN=40), INTENT (in) :: name

      INTEGER :: k, n, nu, nv, isb
!----------------------------------------------------------------------------!

! 1.  FILL GROUND DATA STRUCTURE
! ==============================

      isb = SCAN (medoc % filename, "/", .TRUE.) 

! 1.1 Reset ground information data structure
!     ----------------------------------------

      obs%location%latitude  = missing_r
      obs%location%longitude = missing_r
      obs%location%id        = "                                        "
      obs%location%name      = "                                        "
      obs%info%platform      = "                                        "
      obs%info%source        = "                                        "
      obs%info%elevation     = missing_r

      obs%info%num_vld_fld     = nlevels
      obs%info%num_error       = 0
      obs%info%num_warning     = 0
      obs%info%seq_num         = 0
      obs%info%num_dups        = 0
      obs%info%is_sound        = .TRUE.
      obs%info%bogus           = .FALSE.
      obs%info%discard         = .FALSE.
      obs%valid_time%sut       = missing
      obs%valid_time%julian    = missing
      obs%valid_time%date_char = "19700101000000"

      obs%ground%slp%data      = missing_r
      obs%ground%slp%qc        = missing
      obs%ground%ref_pres%data = missing_r
      obs%ground%ref_pres%qc   = missing_r
      obs%ground%ground_t%data = missing_r
      obs%ground%ground_t%qc   = missing
      obs%ground%sst%data      = missing_r
      obs%ground%sst%qc        = missing
      obs%ground%psfc%data     = missing_r
      obs%ground%psfc%qc       = missing
      obs%ground%precip%data   = missing_r
      obs%ground%precip%qc     = missing
      obs%ground%t_max%data    = missing_r
      obs%ground%t_max%qc      = missing
      obs%ground%t_min%data    = missing_r
      obs%ground%t_min%qc      = missing
      obs%ground%t_min_night%data = missing_r
      obs%ground%t_min_night%qc   = missing
      obs%ground%p_tend03%data    = missing_r
      obs%ground%p_tend03%qc      = missing
      obs%ground%p_tend24%data    = missing_r
      obs%ground%p_tend24%qc      = missing_r
      obs%ground%cloud_cvr%data   = missing_r
      obs%ground%cloud_cvr%qc     = missing
      obs%ground%ceiling%data     = missing_r
      obs%ground%ceiling%qc       = missing_r
!     obs%ground%iwv%data         = missing_r
!     obs%ground%iwv%qc           = missing_r

! 1.2 Copy input into data strucuture
!     -------------------------------

      obs%location%latitude    = medoc % lat + i*medoc % dx
      obs%location%longitude   = medoc % lon + j*medoc % dy
      WRITE (obs%location%id,'(I5.5)') i*j
      obs%location%name        = medoc % filename (isb+1:)
      obs%info%platform        = "FM-32 PILOT"
      obs%info%source          = "PENTAGON SHIELD"
      obs%info%elevation       = medoc % terrain_msl_m (i,j) ! Terrain Elevation in m

      IF (medoc % iyear > 50) THEN
      WRITE (obs%valid_time%date_char, '(I4.4,I2.2,I2.2,I2.2,I2.2,I2.2)') &
        1900+medoc % iyear, medoc % imonth, medoc % iday, &
             medoc % ihour, medoc % imin,   medoc % isec
      ELSE
      WRITE (obs%valid_time%date_char, '(I4.4,I2.2,I2.2,I2.2,I2.2,I2.2)') &
        2000+medoc % iyear, medoc % imonth, medoc % iday, &
             medoc % ihour, medoc % imin,   medoc % isec
      ENDIF


! 2.  FILL UPPER LEVELS
! ======================

! 2.1 Find info location
!     ------------------

      nu = 0
      nv = 0

      DO n = 1, medoc % nvar3d
         IF (medoc % nam3d (n) == "U") nu = n
         IF (medoc % nam3d (n) == "V") nv = n
      ENDDO

      ierr = 0
      IF (nu == 0 .OR. nv == 0) THEN
          ierr = 1
          RETURN
      ENDIF


! 2.2 Fill upper-levels
!     -----------------

      DO k = 1, nlevels

! 2.3 Reset data structure
!     --------------------

      obs % each (k) % pressure % data = missing_r
      obs % each (k) % height % data = missing_r
      obs % each (k) % temperature % data = missing_r
      obs % each (k) % dew_point % data = missing_r
      obs % each (k) % speed % data = missing_r
      obs % each (k) % direction % data = missing_r
      obs % each (k) % u % data = missing_r
      obs % each (k) % v % data = missing_r
      obs % each (k) % rh % data = missing_r
      obs % each (k) % thickness % data = missing_r

      obs % each (k) % pressure % qc = missing
      obs % each (k) % height % qc = missing
      obs % each (k) % temperature % qc = missing
      obs % each (k) % dew_point % qc = missing
      obs % each (k) % speed % qc = missing
      obs % each (k) % direction % qc = missing
      obs % each (k) % u % qc = missing
      obs % each (k) % v % qc = missing
      obs % each (k) % rh % qc = missing
      obs % each (k) % thickness % qc = missing

! 2.4 Write height
!     ------------

      obs % each (k) % height   % data    = NINT (medoc % height_msl_m (i,j,k)) ! Elevation in m 
      obs % each (k) % height   % qc      = 0      ! Observed

! 2.4 Write wind components
!     ---------------------

      obs % each (k) % u % data = medoc % var3d (i,j,k,nu)
      obs % each (k) % u % qc   = 0
      obs % each (k) % v % data = medoc % var3d (i,j,k,nv)
      obs % each (k) % v % qc   = 0

! 2.4 Compute wind speed and direction (assuming u an v are Earth projected)
!     --------------------------------

      CALL ffdduv (obs % each (k) % speed % data,      &
                   obs % each (k) % direction % data,  &
                   obs % each (k) % u % data,          &
                   obs % each (k) % v % data, -1)

      obs % each (k) % speed     % qc   = 0
      obs % each (k) % direction % qc   = 0


      ENDDO

  END SUBROUTINE fill_decoded
!------------------------------------------------------------------------------!

  SUBROUTINE write_decoded (iunit1, iunit2, medoc) 
!------------------------------------------------------------------------------!
      USE module_medoc
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      CHARACTER (LEN=80)  :: filename
      INTEGER             :: iunit1, iunit2
      TYPE (medoc_type)   :: medoc
      INTEGER             :: i, j, nlevels

      CHARACTER (LEN=80)  :: fileou
      CHARACTER (LEN=40)  :: name

      TYPE (report) :: obs
      INTEGER       :: l, iost, ierr
      INTEGER       :: isb, isf
!----------------------------------------------------------------------------!

! 1.  INITIALIZATION
! ==================

! 1.2 Expected number of vertical levels
!     ----------------------------------

      nlevels =  medoc % kmax

! 1.3 Allocate the data structure
!     ---------------------------    

      ALLOCATE (obs % each (nlevels))

! 1.4 Loop over profiles
!     ------------------

      DO j = 1, medoc % jmax
         DO i = 1, medoc % imax


! 2.  FILL THE DATA STRUCTURE
! ===========================

! 2.2 Fill the data structure
!     -----------------------

      CALL fill_decoded (obs, medoc, i, j, nlevels, name, ierr)

      IF (ierr /= 0) THEN
          DEALLOCATE (obs % each)
          WRITE (*,'(A,I5,A)') "Skip Profile #",i*j,": no valid data!"
          CYCLE
      ENDIF

! 3.  WRITE DATA STRUCTURE AT DECODED FORMAT
! ==========================================

! 3.1 Write ground info data structure
!     --------------------------------

      WRITE (UNIT = iunit1, FMT = rpt_format )                  &
      obs % location % latitude ,  obs % location % longitude, &
      obs % location % id,         obs % location % name,      &
      obs % info % platform,       obs % info % source,        &
      obs % info % elevation,      obs % info % num_vld_fld,   &
      obs % info % num_error,      obs % info % num_warning,   &
      obs % info % seq_num,        obs % info % num_dups,      &
      obs % info % is_sound,       obs % info % bogus,         &
      obs % info % discard,                                    &
      obs % valid_time % sut,      obs % valid_time % julian,  &
      obs % valid_time % date_char,                            &
      obs % ground % slp,          obs % ground % ref_pres,    &
      obs % ground % ground_t,     obs % ground % sst,         &
      obs % ground % psfc,         obs % ground % precip,      &
      obs % ground % t_max,        obs % ground % t_min,       &
      obs % ground % t_min_night,  obs % ground % p_tend03,    &
      obs % ground % p_tend24,     obs % ground % cloud_cvr,   &
      obs % ground % ceiling


! 3.2 Write upper levels
!     ------------------

      DO l = 1, nlevels

         WRITE (UNIT = iunit1,   FMT = meas_format )                  &
         obs % each (l) % pressure,    obs % each (l) % height,      &
         obs % each (l) % temperature, obs % each (l) % dew_point,   &
         obs % each (l) % speed,       obs % each (l) % direction,   &
         obs % each (l) % u,           obs % each (l) % v,           &
         obs % each (l) % rh,          obs % each (l) % thickness

      ENDDO


! 3.3 Write upper-level ending record
!     -------------------------------

      WRITE (UNIT = iunit1,  FMT = meas_format ) &
             end_data_r,     0, end_data_r, 0,  &
	     REAL (nlevels), 0, missing_r,  0,  &
             missing_r,      0, missing_r,  0,  &
             missing_r,      0, missing_r,  0,  &
             missing_r,      0, missing_r,  0,  &
             missing_r,  0

! 3.5 Write report ending record
!     --------------------------

      WRITE (UNIT = iunit1 , FMT = end_format ) nlevels, 0, 0


! 4.  WRITE AT RIP NAMELIST FORMAT
!     ----------------------------

      IF (INDEX (medoc % filename,"VLAS") > 0   .OR.        &
          INDEX (medoc % filename,"vlas") > 0) THEN ! Red o for VLAS
      WRITE (iunit2, '(1X,3A,1(A,2(F10.5,A)),A)')           &
           "feld=bullet; titl=","o","; ptyp=hb; ", "crsa=", &
            obs % location % latitude, "lat,",              & 
            obs % location % longitude,"lon; ",             &
           "colr=red; tslb=.02"
      ELSE IF (INDEX (medoc % filename,"VDRAS") > 0  .OR.   &
               INDEX (medoc % filename,"vdras") > 0) THEN ! Blue + for VDRAS
      WRITE (iunit2, '(1X,3A,1(A,2(F10.5,A)),A)')           &
           "feld=bullet; titl=","+","; ptyp=hb; ", "crsa=", &
            obs % location % latitude, "lat,",              & 
            obs % location % longitude,"lon; ",             &
           "colr=blue; tslb=.02"
      ELSE ! Unknow source in Green x
      WRITE (iunit2, '(1X,3A,1(A,2(F10.5,A)),A)')           &
           "feld=bullet; titl=","+","; ptyp=hb; ", "crsa=", &
            obs % location % latitude, "lat,",              & 
            obs % location % longitude,"lon; ",             &
           "colr=green; tslb=.02"
      ENDIF

      ENDDO
      ENDDO

! 5.  DEALLOCATE DATRA STRUCTURE
! ==============================

      DEALLOCATE (obs % each)

  END SUBROUTINE write_decoded
!------------------------------------------------------------------------------!
 END MODULE module_decoded
