!------------------------------------------------------------------------------!
! Module defining the "decoded" format, observation input format of MM5 RAWINS 
! and LITTLE_R analysis programs.
! 
! Francois VANDENBERGHE, December 2003
! vandenb@ucar.edu
!------------------------------------------------------------------------------!

!  Starting date for julian seconds

   CHARACTER (LEN =  19), PARAMETER :: decoded_date_start= '1970-01-01_00:00:00'

!  Missing value flags

!  INTEGER, PARAMETER               ::  missing    = -888888
!  REAL,    PARAMETER               ::  missing_r  = -888888.

!  Ending value flags

!  INTEGER, PARAMETER               ::  end_data   = -777777
!  REAL,    PARAMETER               ::  end_data_r = -777777.

   !  Define error return codes used by 'read_measurements' routine.

   INTEGER , PARAMETER                            ::  ok       = 0 , &
                                                      eof_err  = 1 , &
                                                      no_data  = 2 , &
                                                      read_err = 3

!  Output format ground info (1 record at the beginning of sounding)
!  Format has been appended for PW

   CHARACTER ( LEN = 120 ), PARAMETER :: rpt_format =  &
  '(2F20.5,2A40,2A40,1F20.5,5I10,3L10,2I10,A20,13(F13.5,I7),2(:,F13.5,I7))'

!  Output format for sounding level (1 record per level)

   CHARACTER ( LEN = 120 ), PARAMETER :: meas_format = '(10(F13.5,I7),2(:,F13.5,I7))'

!  Output format for end of sounding (1 record at the end of soundig)

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

   TYPE end_of_report

      !  Defines the 3 integers that signal the end of the reports.
      !  Normally, the first integer is the number of vertical levels,
      !  others are set to 0.

      INTEGER                :: end_report1     !  End of report 1
      INTEGER                :: end_report2     !  End of report 2
      INTEGER                :: end_report3     !  End of report 3

  END TYPE end_of_report

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
!  This structure is used only by the error_handler routine.  These utility
!  routines and the error data structure allow the program to record
!  errors, retrieve the error history, and exit the program when a
!  fatal error is detected.

   TYPE error_info
      INTEGER                                 :: num
      CHARACTER ( LEN=132 )                   :: msg
      TYPE ( error_info ) , POINTER           :: next
   END TYPE error_info

!------------------------------------------------------------------------------!
