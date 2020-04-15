program QCtoNC
  use netcdf
  implicit none
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

   INTEGER, external   :: iargc
   INTEGER             :: file_num
   CHARACTER (LEN=80)  :: cause
   CHARACTER (LEN=80)  :: exename,FileName
   CHARACTER (LEN=100) :: error_string
   CHARACTER (LEN=80)  :: QCFileName
   CHARACTER (LEN=10)  :: date10
   LOGICAL             :: drop_data, sounding
   LOGICAL             :: UnitExists,FileExists
   LOGICAL             :: connected, go, file_ok
   LOGICAL             :: bad_data   ! read, but do not store
   INTEGER             :: iunit = 22, j
   INTEGER             :: num_line, num_line_ground
   INTEGER             :: error_ret 
   INTEGER             :: io_error
   INTEGER             :: obs_num, meas_count
   INTEGER             :: error      ! err and type
   INTEGER, parameter  :: max_number_of_levels = 4050   ! Like MADIS V3
   INTEGER, parameter  :: total_number_of_obs  = 200000 ! Like MADIS V3
   TYPE (report) , DIMENSION(total_number_of_obs) :: obs
   TYPE (measurement), POINTER    :: current
   INTEGER, PARAMETER  ::  end_data   = -777777
   REAL,    PARAMETER  ::  end_data_r = -777777.
   INTEGER , PARAMETER ::  ok       = 0 , &
                           eof_err  = 1 , &
                           no_data  = 2 , &
                           read_err = 3
   INTEGER             :: iret
   INTEGER             :: numRec_sgl = 0  ! single-level record number 
   INTEGER             :: numRec_mpl = 0  ! multiple-level record number 
   INTEGER             :: numRec_lev      ! level record

   CHARACTER (LEN = 32), PARAMETER :: proc_name = 'read_decoded '
   CHARACTER (LEN = 132) :: error_message
   CHARACTER ( LEN = 120 ), PARAMETER :: rpt_format =  &
        '(2F20.5,2A40,2A40,1F20.5,5I10,3L10,2I10,A20,13(F13.5,I7),2(:,F13.5,I7))'
   CHARACTER ( LEN = 120 ), PARAMETER :: meas_format = &
        '(10(F13.5,I7),2(:,F13.5,I7))'
   CHARACTER ( LEN = 120 ), PARAMETER :: end_format = '(3(I7))'

! NETCDF-related variables

   INTEGER :: ncid_sgl,ncid_mpl        ! File IDs
   INTEGER :: is_sfc(1)

   j = iargc()
   if (j < 1) then
      call getarg(0,exename)
      print*,'Usage: ',TRIM(exename),' QC_FileName <start_date>'
      print*,'With  start_date = CCYYMMDDHH'
      CALL ABORT
   endif

   call getarg(1,QCFileName)

   print *, 'QC FileName = ',trim(QCFileName)

   if (j > 1) then
      call getarg(2,date10)
   else
      date10 = QCFileName(1:10)
   endif

   if (LEN_TRIM (date10) <= 0) then
       print*,'Usage: ',TRIM(exename),' QC_FileName <start_date>'
       print*,'With  start_date = CCYYMMDDHH'
       stop   'bad start_date'
    endif
    print *,"Start Date = ",date10

   INQUIRE (UNIT = iunit, EXIST = UnitExists)
   IF (UnitExists) CLOSE (UNIT = iunit)

   INQUIRE (FILE=trim(QCFileName), EXIST=FileExists)
   if (.not.FileExists) stop "File not found... stop"

   OPEN (UNIT = iunit , FILE = QCFileName , FORM = 'FORMATTED', &
        ACTION = 'READ' , STATUS= 'old', IOSTAT = io_error)
 
   obs_num = 1

   call open_netcdf_sgl(QCFileName, date10, ncid_sgl)

   call open_netcdf_mpl(QCFileName, date10, ncid_mpl, max_number_of_levels)

   read_obs : DO

      sounding = .FALSE.
      cause = "no data"
      error_string = "no data"

      IF ((obs_num > total_number_of_obs) ) THEN

         error_message = proc_name // &
              "Too many obs for the NAMELIST value of max_number_of_obs_nml."
         print *,error_message," ",obs_num
         stop
      end IF
!---------------------------- READ GROUND INFORMATION -------------------------!

      !  The first read is the "once only" type of information.

      num_line = num_line + 1
      num_line_ground = num_line


      READ (iunit , IOSTAT = io_error , FMT = rpt_format) &
             obs(obs_num)%location ,   &
             obs(obs_num)%info ,       &
             obs(obs_num)%valid_time , &
             obs(obs_num)%ground

!      if (obs(obs_num)%info%is_sound) &
!      WRITE  (6,'(A,I6,A,A5,1X,A11,1X,A,F6.2,A,F7.2,A,4A,L1)')    &
!           "line ",num_line ,": id=",                             &
!           obs(obs_num)%location%id,                              &
!           obs(obs_num)%info%platform,     "at",                  &
!           obs(obs_num)%location%latitude, "N,",                  &
!           obs(obs_num)%location%longitude,"E on ",               &
!           obs(obs_num)%valid_time%date_char (1:8),".",           &
!           obs(obs_num)%valid_time%date_char (9:14)," Sound? ",   &
!           obs(obs_num)%info%is_sound

!      if (obs(obs_num)%info%is_sound) &
!           WRITE(6,'(4(2x,a40))') obs(obs_num)%location%id,obs(obs_num)%location%name, &
!           obs(obs_num)%info%platform,obs(obs_num)%info%source

!---------------------------- ERROR READING GROUND DATA -----------------------!

      !  If there are troubles with the "once only" type of data, we keep trying
      !  until we either come to the end of this report (and cycle) or we come
      !  to the end of all of the data (exit).

      IF (io_error .GT. 0) THEN

          CALL error_read (iunit, io_error, obs (obs_num), go)

          IF (go) THEN
               CYCLE read_obs
          ELSE
               EXIT read_obs
          ENDIF

!--------------------------- HIT END OF FILE ----------------------------------!

      ELSE IF (io_error .LT. 0) THEN

!        WRITE (6,'(A,I3)') " Have reached end of unit: ", iunit
         EXIT read_obs

      ENDIF

!--------------------------- SUCCESFUL READ OF GROUND DATA --------------------!
      !  No errors.  This is the intended way to find the end of data mark.

      NULLIFY (obs(obs_num)%surface)

      CALL read_measurements(iunit, obs(obs_num)%surface, &
           obs(obs_num)%location, obs(obs_num)%info, drop_data,   &
           error_ret, error_string,                               &
           obs(obs_num)%info%elevation, meas_count, num_line)

      num_line = num_line + 1
      
      READ (iunit, IOSTAT = io_error , FMT = end_format) &
           obs(obs_num)%info%num_vld_fld, &
           obs(obs_num)%info%num_error,   &
           obs(obs_num)%info%num_warning
!       print *,obs(obs_num)%info%num_vld_fld, &
!            obs(obs_num)%info%num_error,   &
!            obs(obs_num)%info%num_warning

! Stop if observation has more levels than those defined for arrays!!!

      if (obs(obs_num)%info%num_vld_fld > max_number_of_levels) then
         print *,'Too few levels in record ',obs(obs_num)%info%num_vld_fld, &
              max_number_of_levels 
         print *,'Increase max_number_of_levels parameter and recompile!!'
         stop
      end if
      
      current => obs (obs_num) % surface 

! Write single level fields 
! CISL ADP surface data come with the is_sound flag not set
      if ((obs(obs_num)%info%num_vld_fld .le.1) .OR. &
          (.NOT. obs(obs_num)%info%is_sound)) then
         !WRITE (6,*) current%meas%pressure, current%meas%temperature
            
         numRec_sgl = numRec_sgl + 1
            
         call put_var_char(ncid_sgl,numRec_sgl, "platform" , &
              obs(obs_num)%info%platform)
         call put_var_char(ncid_sgl,numRec_sgl, "source" , &
              obs(obs_num)%info%source)
         call put_var_char(ncid_sgl,numRec_sgl, "id" , &
              obs(obs_num)%location%id)
         call put_var_char(ncid_sgl,numRec_sgl, "name" , &
              obs(obs_num)%location%name)
         call put_var_char(ncid_sgl,numRec_sgl, "date" , &
              obs(obs_num)%valid_time%date_char)
         
         call put_var_1d(ncid_sgl,numRec_sgl, "latitude" , &
              obs(obs_num)%location%latitude)
         call put_var_1d(ncid_sgl,numRec_sgl, "longitude" , &
              obs(obs_num)%location%longitude)
         call put_var_1d(ncid_sgl,numRec_sgl, "elevation" , &
              obs(obs_num)%info%elevation)
         
         is_sfc(1) = 1
         if (obs(obs_num)%info%is_sound) is_sfc(1) = 0
         call put_var_1d_int(ncid_sgl,numRec_sgl, "is_surface", &
              is_sfc(1))
         
         call put_var_1d(ncid_sgl,numRec_sgl, "pressure" , &
              current%meas%pressure%data)
         call put_var_1d_int(ncid_sgl,numRec_sgl, "pressure_qc" , &
              current%meas%pressure%qc)
         
         call put_var_1d(ncid_sgl,numRec_sgl, "temperature" , &
              current%meas%temperature%data)
         call put_var_1d_int(ncid_sgl,numRec_sgl, "temperature_qc" , &
              current%meas%temperature%qc)
         
         call put_var_1d(ncid_sgl,numRec_sgl, "dew_point" , &
              current%meas%dew_point%data)
         call put_var_1d_int(ncid_sgl,numRec_sgl, "dew_point_qc" , &
              current%meas%dew_point%qc)
         
         call put_var_1d(ncid_sgl,numRec_sgl, "wind_speed" , &
              current%meas%speed%data)
         call put_var_1d_int(ncid_sgl,numRec_sgl, "wind_speed_qc" , &
              current%meas%speed%qc)
         
         call put_var_1d(ncid_sgl,numRec_sgl, "wind_direction" , &
              current%meas%direction%data)
         call put_var_1d_int(ncid_sgl,numRec_sgl, "wind_direction_qc" , &
              current%meas%direction%qc)
         
         call put_var_1d(ncid_sgl,numRec_sgl, "u_wind" , &
              current%meas%u%data)
         call put_var_1d_int(ncid_sgl,numRec_sgl, "u_wind_qc" , &
              current%meas%u%qc)
         
         call put_var_1d(ncid_sgl,numRec_sgl, "v_wind" , &
              current%meas%v%data)
         call put_var_1d_int(ncid_sgl,numRec_sgl, "v_wind_qc" , &
              current%meas%v%qc)
         
         call put_var_1d(ncid_sgl,numRec_sgl, "rh" , &
              current%meas%rh%data)
         call put_var_1d_int(ncid_sgl,numRec_sgl, "rh_qc" , &
              current%meas%rh%qc)
            
         ! write multi-level fields
      else

         numRec_mpl = numRec_mpl + 1

      !if (obs(obs_num)%info%is_sound) &
      !     WRITE(6,'(4(2x,a40))') obs(obs_num)%location%id,obs(obs_num)%location%name, &
      !     obs(obs_num)%info%platform,obs(obs_num)%info%source

         call put_var_char(ncid_mpl,numRec_mpl, "platform" , &
              trim(obs(obs_num)%info%platform))
         call put_var_char(ncid_mpl,numRec_mpl, "source" , &
              trim(obs(obs_num)%info%source))
         call put_var_char(ncid_mpl,numRec_mpl, "id" , &
              trim(obs(obs_num)%location%id))
         call put_var_char(ncid_mpl,numRec_mpl, "name" , &
              trim(obs(obs_num)%location%name))
         call put_var_char(ncid_mpl,numRec_mpl, "date" , &
              obs(obs_num)%valid_time%date_char)
         
         call put_var_1d(ncid_mpl,numRec_mpl, "latitude" , &
              obs(obs_num)%location%latitude)
         call put_var_1d(ncid_mpl,numRec_mpl, "longitude" , &
              obs(obs_num)%location%longitude)
         call put_var_1d(ncid_mpl,numRec_mpl, "elevation" , &
              obs(obs_num)%info%elevation)
         call put_var_1d_int(ncid_mpl,numRec_mpl, "numlevs", &
              obs(obs_num)%info%num_vld_fld)
         
         numRec_lev = 0

         DO WHILE (ASSOCIATED (current))
            !WRITE (6,*) current%meas%pressure, current%meas%temperature

            numRec_lev = numRec_lev + 1

            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/), "pressure" , &
                 current%meas%pressure%data)
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), "pressure_qc" , &
                 current%meas%pressure%qc)

            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/), "temperature" , &
                 current%meas%temperature%data)
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), "temperature_qc" , &
                 current%meas%temperature%qc)
 
            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/), "dew_point" , &
                 current%meas%dew_point%data)
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), "dew_point_qc" , &
                 current%meas%dew_point%qc)

            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/), "wind_speed" , &
                 current%meas%speed%data)
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), "wind_speed_qc" , &
                 current%meas%speed%qc)
 
            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/),"wind_direction" , &
                 current%meas%direction%data) 
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), &
                 "wind_direction_qc" , current%meas%direction%qc)
 
            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/), "u_wind" , &
                 current%meas%u%data)
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), "u_wind_qc" , &
                 current%meas%u%qc)

            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/), "v_wind" , &
                 current%meas%v%data)
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), "v_wind_qc" , &
                 current%meas%v%qc)

            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/), "rh" , &
                 current%meas%rh%data)
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), "rh_qc" , &
                 current%meas%rh%qc)

            call put_var_2d(ncid_mpl,(/numRec_lev,numRec_mpl/), "height", &
                 current%meas%height%data)
            call put_var_2d_int(ncid_mpl,(/numRec_lev,numRec_mpl/), "height_qc", &
                 current%meas%height%qc)
         
            current => current % next
         end DO
      endif

      if (associated(current)) DEALLOCATE (current)
      obs_num = obs_num + 1

   end DO read_obs
 
! Close netcdf files

   iret = nf90_close(ncid_sgl)
   iret = nf90_close(ncid_mpl)

   print *,'Total number of observations: ',obs_num
   print *,'    single-level records    : ',numRec_sgl
   print *,'    multi-level records     : ',numRec_mpl


! -------------------------------------------------------------------------
!                            FUNCTIONS
! ---------------------------------------------------------------------------
contains

 SUBROUTINE read_measurements (file_num, surface, location, info, &
                               bad_data, error, error_string,     & 
                               elevation, meas_count, num_line)

   IMPLICIT NONE 

   INTEGER , INTENT (IN)                  :: file_num   ! file unit to read  
   TYPE (measurement) ,   POINTER         :: surface    ! ptr to 1st msmt
   TYPE (location_type) , INTENT (IN)     :: location   ! 5 digit ID, name, etc
   TYPE (source_info) ,   INTENT (IN)     :: info       ! Station info
   LOGICAL , INTENT (IN)                  :: bad_data   ! read, but do not store
   INTEGER , INTENT (OUT)                 :: error      ! err and type 
   CHARACTER (LEN =100),  INTENT (out)    :: error_string
   INTEGER , INTENT (INOUT)               :: num_line      
   REAL ,    INTENT(IN)                   :: elevation

   CHARACTER (LEN = 32) , PARAMETER       :: proc_name = 'read_measurements'
   INTEGER                                :: meas_count
   INTEGER                                :: io_error
   TYPE (measurement) , POINTER           :: current
   CHARACTER (LEN = 40)                   :: location_id, location_name

   INTEGER                                :: error_number
   CHARACTER ( LEN = 132 )                :: error_message
   LOGICAL                                :: fatal, listing

   ALLOCATE (current)
   NULLIFY (current%next)
   NULLIFY (surface)
   error = ok
   meas_count = 0
   location_id   = TRIM (location%id)
   location_name = TRIM (location%name)

   read_meas: DO
      !  Currently, this read puts in 12 pairs of data, a real observation
      !  value and the accompanying QC flag.

      num_line = num_line + 1
      !call fill_meas (current%meas)
      READ  (iunit, IOSTAT = io_error , FMT = meas_format) current%meas
         
      IF (io_error .GT. 0) THEN
         error = read_err
         error_string = "read error"
         EXIT read_meas
      ELSE IF (io_error .LT. 0) THEN
         error = eof_err
         EXIT read_meas
      ENDIF
      
      IF ((eps_equal (current%meas%pressure%data , end_data_r , 1.)) .OR. &
           (eps_equal (current%meas%height%data   , end_data_r , 1.))) THEN
         call set_meas (current%meas)
         error = ok
         EXIT read_meas
      ENDIF

      meas_count = meas_count + 1
      CALL insert_at_end (surface , current)

      !  Allocate space for another measurement, so we can go try and read 
      !  another level in this routine.  Initialize it to pointing to nothing.

      ALLOCATE (current)
      NULLIFY  (current%next)

   
   end DO read_meas

 end SUBROUTINE read_measurements
! ---------------------------------------------------------------------------

 LOGICAL FUNCTION eps_equal (a , b , eps)

!  Compare two real numbers a and b, and return TRUE if they are within
!  parameter 'eps' of one another.

    IMPLICIT NONE
    
    REAL , INTENT (IN)                     :: a , b , eps
    
    IF (ABS (a - b) .LT. eps) THEN
       eps_equal = .TRUE.
    ELSE
       eps_equal = .FALSE.
    ENDIF
    
  END FUNCTION eps_equal
! ---------------------------------------------------------------------------

  SUBROUTINE error_read (file_num, io_error, obs, go)

    IMPLICIT NONE

    INTEGER,            INTENT (in)    :: file_num
    INTEGER,            INTENT (inout) :: io_error
    TYPE (report),      INTENT (inout) :: obs
    LOGICAL,            INTENT (out)   :: go

    TYPE (meas_data)   :: dummy_middle
    INTEGER            :: bad_count

    go = .FALSE.

    !  Keep track of how many loops we are taking so this is not infinite.

    bad_count = 0

    DO WHILE (io_error .GE. 0)

       bad_count = bad_count + 1

       IF (bad_count .LT. 1000) THEN

           READ (UNIT=file_num , IOSTAT = io_error , FMT = meas_format)  &
                 dummy_middle
           IF (eps_equal (dummy_middle%pressure%data, end_data_r , 1.)) THEN
               go = .TRUE.
               RETURN

           ENDIF

       ELSE
           go = .FALSE.
           RETURN

       ENDIF
    END DO

 END SUBROUTINE error_read
! ---------------------------------------------------------------------------

 SUBROUTINE set_meas (meas)

   IMPLICIT NONE

   TYPE (meas_data) :: meas

          meas%pressure%data    = end_data_r
          meas%height%data      = end_data_r
          meas%temperature%data = end_data_r
          meas%dew_point%data   = end_data_r
          meas%speed%data       = end_data_r
          meas%direction%data   = end_data_r
          meas%u%data           = end_data_r
          meas%v%data           = end_data_r
          meas%rh%data          = end_data_r
          meas%thickness%data   = end_data_r
          meas%pressure%qc      = end_data
          meas%height%qc        = end_data
          meas%temperature%qc   = end_data
          meas%dew_point%qc     = end_data
          meas%speed%qc         = end_data
          meas%direction%qc     = end_data
          meas%u%qc             = end_data
          meas%v%qc             = end_data
          meas%rh%qc            = end_data
          meas%thickness%qc     = end_data

 END SUBROUTINE set_meas
! ---------------------------------------------------------------------------

 SUBROUTINE fill_meas (meas)

   IMPLICIT NONE

   TYPE (meas_data) :: meas
   real, parameter    :: r_missing = -888888.
   integer, parameter :: i_missing = -888888

          meas%pressure%data    = r_missing
          meas%height%data      = r_missing
          meas%temperature%data = r_missing
          meas%dew_point%data   = r_missing
          meas%speed%data       = r_missing
          meas%direction%data   = r_missing
          meas%u%data           = r_missing
          meas%v%data           = r_missing
          meas%rh%data          = r_missing
          meas%thickness%data   = r_missing
          meas%pressure%qc      = i_missing
          meas%height%qc        = i_missing
          meas%temperature%qc   = i_missing
          meas%dew_point%qc     = i_missing
          meas%speed%qc         = i_missing
          meas%direction%qc     = i_missing
          meas%u%qc             = i_missing
          meas%v%qc             = i_missing
          meas%rh%qc            = i_missing
          meas%thickness%qc     = i_missing

 END SUBROUTINE fill_meas
! ---------------------------------------------------------------------------



 SUBROUTINE open_netcdf_sgl(QCFileName, date10, ncid)

   implicit none
   character(len=*),intent(in) :: QCFileName   
   INTEGER, intent(out)        :: ncid

   integer :: recdim, strdim, platdim, iddim, datedim
   integer :: varid
   character(len=256)   :: output_flnm
!  character(len=19)  :: date19
   character(len=10)  :: date10
   integer :: iret
   integer :: indx
   integer, parameter :: maxStaNamLen = 40
   integer, parameter :: DateLen = 14

! Construct NetCDF file name from input filename...

!   indx = index(trim(QCFileName), "qc_out_")
!   write(output_flnm, '(A20,"_sgl.nc")') QCFileName(indx:indx+19)

   output_flnm = trim(QCFileName)//"_sgl.nc"
   print *, 'Output Filename = "'//trim(output_flnm)//'"'
   iret = nf90_create(trim(output_flnm), NF90_CLOBBER, ncid)
   if (iret /= 0) stop "Problem nf_create"

!  date19 = QCFileName(indx+8:indx+27)
!  date10 = QCFileName(1:10)
!  print *,"Start Date = ",date10
   iret = nf90_put_att(ncid, NF90_GLOBAL, "start_date", date10)

! Create unlimited dimension for the station record

   iret = nf90_def_dim(ncid, "recNum", NF90_UNLIMITED, recdim)
   iret = nf90_def_dim(ncid, "maxStaNamLen", maxStaNamLen, strdim)
   iret = nf90_def_dim(ncid, "DateLen", DateLen, datedim)

   call make_var_char(ncid, (/strdim,recdim/),"platform","platform","name")
   call make_var_char(ncid, (/strdim,recdim/),"source","source","name")
   call make_var_char(ncid, (/strdim,recdim/),"name","name","name")
   call make_var_char(ncid, (/strdim,recdim/),"id","station id","name")
   call make_var_char(ncid, (/datedim,recdim/),"date","date_char", &
        "CCYYMMDDHHmmss")
  
   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "latitude", "latitude" , &
        "degree_north" )
   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "longitude", "longitude" , &
        "degree_east" )
   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "elevation", "elevation" , &
        "meters" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "is_surface", "is surface flag", &
        "0=no 1=yes")

   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "temperature", "temperature" , "K" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "temperature_qc", &
        "temperature_qc" , "flag" )

   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "pressure", "pressure" , "Pa" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "pressure_qc", &
        "pressure_qc" , "flag" )

   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "dew_point", &
        "dew point temperature" , "K" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "dew_point_qc", &
        "dew_point_qc" , "flag" )

   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "wind_speed", "wind speed" , "m/s" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "wind_speed_qc", &
        "wind_speed_qc" , "flag" )

   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "wind_direction", &
        "wind direction" , "degrees" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "wind_direction_qc", &
        "wind_direction_qc" , "flag" )

   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "u_wind", "zonal wind" , "m/s" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "u_wind_qc", &
        "u_wind_qc" , "flag" )

   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "v_wind", "meridional wind" , "m/s" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "v_wind_qc", &
        "v_wind_qc" , "flag" )

   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "rh", "relative humidity" , "%" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "rh_qc", &
        "rh_qc" , "flag" )

   iret = nf90_enddef(ncid)

 end SUBROUTINE open_netcdf_sgl
! ---------------------------------------------------------------------------

 SUBROUTINE open_netcdf_mpl(QCFileName, date10, ncid, LevLen)

   implicit none
   character(len=*),intent(in) :: QCFileName 
   INTEGER, intent(in)         :: LevLen  ! max_number_of_levels
   INTEGER, intent(out)        :: ncid
   integer :: recdim, strdim, datedim, levdim
   integer :: varid
   character(len=256)   :: output_flnm
!  character(len=19)  :: date19
   character(len=10)  :: date10
   integer :: iret
   integer :: indx
   integer, parameter :: maxStaNamLen = 40
   integer, parameter :: DateLen = 14

! Construct NetCDF file name from input filename...

!   indx = index(trim(QCFileName), "qc_out_")
!   write(output_flnm, '(A20,"_mpl.nc")') QCFileName(indx:indx+19)
   output_flnm = trim(QCFileName)//"_mpl.nc"

   print *, 'Output Filename = "'//trim(output_flnm)//'"'
   iret = nf90_create(trim(output_flnm), NF90_CLOBBER, ncid)
   if (iret /= 0) stop "Problem nf_create"

!  date19 = QCFileName(indx+8:indx+27)
!  date10 = QCFileName(1:10)
!  print *,"Start Date = ",date10
   iret = nf90_put_att(ncid, NF90_GLOBAL, "start_date", date10)

! Create unlimited dimension for the station record

   iret = nf90_def_dim(ncid, "recNum", NF90_UNLIMITED, recdim)
   iret = nf90_def_dim(ncid, "level", LevLen, levdim)
   iret = nf90_def_dim(ncid, "maxStaNamLen", maxStaNamLen, strdim)
   iret = nf90_def_dim(ncid, "DateLen", DateLen, datedim)

   call make_var_char(ncid, (/strdim,recdim/),"platform","platform","name")
   call make_var_char(ncid, (/strdim,recdim/),"source","source","name")
   call make_var_char(ncid, (/strdim,recdim/),"name","name","name")
   call make_var_char(ncid, (/strdim,recdim/),"id","station id","name")
   call make_var_char(ncid, (/datedim,recdim/),"date","date_char", &
        "CCYYMMDDHHmmss")
  
   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "latitude", "latitude" , &
        "degree_north" )
   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "longitude", "longitude" , &
        "degree_east" )
   call make_var_att_1d(ncid, recdim, NF90_FLOAT, "elevation", "elevation" , &
        "meters" )
   call make_var_att_1d(ncid, recdim, NF90_INT, "numlevs", &
        "number of vertical levels", "number")
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "temperature", "temperature" , "K" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "temperature_qc", "temperature_qc" , "flag" )
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "pressure", "pressure" , "Pa" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "pressure_qc", "pressure_qc" , "flag" )
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "dew_point", "dew point temperature" , "K" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "dew_point_qc", "dew_point_qc" , "flag" )
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "wind_speed", "wind speed" , "m/s" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "wind_speed_qc", "wind_speed_qc" , "flag" )
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "wind_direction", "wind direction" , "degrees" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "wind_direction_qc", "wind_direction_qc" , "flag" )
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "u_wind", "zonal wind" , "m/s" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "u_wind_qc", "u_wind_qc" , "flag" )
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "v_wind", "meridional wind" , "m/s" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "v_wind_qc", "v_wind_qc" , "flag" )
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "rh", "relative humidity" , "%" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "rh_qc", "rh_qc" , "flag" )
   
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_FLOAT, &
        "height", "geopotential height" , "m" )
   call make_var_att_2d(ncid, (/levdim,recdim/), NF90_INT, &
        "height_qc", "height_qc" , "flag" )

   iret = nf90_enddef(ncid)
 end SUBROUTINE open_netcdf_mpl
! ---------------------------------------------------------------------------

 SUBROUTINE make_var_att_1d(ncid, recdim, itype, varname, vardesc, varunits)

   implicit none
   integer,          intent(in) :: ncid, recdim
   character(len=*), intent(in) :: varname
   character(len=*), intent(in) :: vardesc
   character(len=*), intent(in) :: varunits
   integer,          intent(in) :: itype
   integer :: iret
   integer :: varid
   real, parameter :: r_missing    = -888888.
   integer, parameter :: i_missing = -888888
   integer, parameter :: s_missing = -99

   iret = nf90_def_var(ncid, varname, itype, recdim, varid)
   if (iret /= 0) then
      print*, 'varname = ', varname
      stop "Failure defining variable."
   endif

   if (itype == NF90_FLOAT) then
      iret = nf90_put_att(ncid, varid, "missing_value", r_missing)
      iret = nf90_put_att(ncid, varid, "_FillValue", r_missing)
 !  else if (itype == NF90_SHORT .or. itype == NF90_INT) then
   else if (itype == NF90_SHORT) then
      iret = nf90_put_att(ncid, varid, "missing_value", i_missing)
      !iret = nf90_put_att(ncid, varid, "_FillValue", s_missing)
   else if (itype == NF90_INT) then
      iret = nf90_put_att(ncid, varid, "missing_value", i_missing)
      iret = nf90_put_att(ncid, varid, "_FillValue", i_missing)

   end if

   iret = nf90_put_att(ncid, varid, "description", vardesc)
   if (iret /= 0) then
      print*,'varname = "'//varname//'"'
      print*,'vardesc = "'//vardesc//'"'
      stop "Failure adding description attribute"
   endif

   iret = nf90_put_att(ncid, varid, "units", varunits)
   if (iret /= 0) then
      print*,'varname = "'//varname//'"'
      print*,'vardesc = "'//vardesc//'"'
      print*,'varunits = "'//varunits//'"'
      stop "Failure adding units attribute"
   endif

 end subroutine make_var_att_1d
! ---------------------------------------------------------------------------

 SUBROUTINE make_var_att_2d(ncid, dims, itype, varname, vardesc, varunits)

   implicit none
   integer,          intent(in) :: ncid
   integer,          intent(in) :: dims(2)
   character(len=*), intent(in) :: varname
   character(len=*), intent(in) :: vardesc
   character(len=*), intent(in) :: varunits
   integer,          intent(in) :: itype
   integer :: iret
   integer :: varid
   real, parameter :: r_missing = -888888.
   integer, parameter :: i_missing = -888888

   iret = nf90_def_var(ncid, varname, itype, dims, varid)
   if (iret /= 0) then
      print*, 'varname = ', varname
      stop "Failure defining variable."
   endif

   if (itype == NF90_FLOAT) then
      iret = nf90_put_att(ncid, varid, "missing_value", r_missing)
      iret = nf90_put_att(ncid, varid, "_FillValue", r_missing)
   else if (itype == NF90_INT) then
      iret = nf90_put_att(ncid, varid, "missing_value", i_missing)
      iret = nf90_put_att(ncid, varid, "_FillValue", i_missing)
   end if

   iret = nf90_put_att(ncid, varid, "description", vardesc)
   if (iret /= 0) then
      print*,'varname = "'//varname//'"'
      print*,'vardesc = "'//vardesc//'"'
      stop "Failure adding description attribute"
   endif

   iret = nf90_put_att(ncid, varid, "units", varunits)
   if (iret /= 0) then
      print*,'varname = "'//varname//'"'
      print*,'vardesc = "'//vardesc//'"'
      print*,'varunits = "'//varunits//'"'
      stop "Failure adding units attribute"
   endif

 end subroutine make_var_att_2d
! ---------------------------------------------------------------------------

 SUBROUTINE make_var_char(ncid, dims, varname, vardesc, varunits)

   implicit none
   integer,          intent(in) :: ncid
   integer, dimension(2), intent(in) :: dims
   character(len=*), intent(in) :: varname
   character(len=*), intent(in) :: vardesc
   character(len=*), intent(in) :: varunits
   integer :: iret
   integer :: varid

   iret = nf90_def_var(ncid,  varname,  NF90_CHAR, dims, varid)
   if (iret /= 0) then
      print*, 'varname = ', varname
      stop "Failure defining variable."
   endif

   iret = nf90_put_att(ncid, varid, "description", vardesc)
   if (iret /= 0) then
      print*,'varname = "'//varname//'"'
      print*,'vardesc = "'//vardesc//'"'
      stop "Failure adding description attribute"
   endif

   iret = nf90_put_att(ncid, varid, "units", varunits)
   if (iret /= 0) then
      print*,'varname = "'//varname//'"'
      print*,'vardesc = "'//vardesc//'"'
      print*,'varunits = "'//varunits//'"'
      stop "Failure adding units attribute"
   endif

 end subroutine make_var_char
! ---------------------------------------------------------------------------

 SUBROUTINE put_var_1d(ncid, NumRec, varname, vardata)

   implicit none
   integer,                   intent(in) :: ncid
   integer,                   intent(in) :: NumRec
   character(len=*),          intent(in) :: varname
   real,                      intent(in) :: vardata

   integer                               :: iret
   integer                               :: varid
   real, parameter :: missing = -888888.
   real, dimension(1) :: xdum

   xdum(1) = vardata
   if (xdum(1) < -888800.0) xdum(1) = missing

   iret = nf90_inq_varid(ncid,  varname, varid)
   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_1D:  Problem finding variable id."
   endif

   iret = nf90_put_var(ncid, varid, xdum, start = (/NumRec/))
   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_1D flt:  Problem putting variable to NetCDF file."
   endif

 end SUBROUTINE put_var_1d

 SUBROUTINE put_var_1d_int(ncid, NumRec, varname, vardata)

   implicit none
   integer,                   intent(in) :: ncid
   integer,                   intent(in) :: NumRec
   character(len=*),          intent(in) :: varname
   integer,                   intent(in) :: vardata

   integer                               :: iret
   integer                               :: varid
   integer, parameter :: missing = -888888
   integer, dimension(1) :: xdum

   xdum(1) = vardata
   if (xdum(1) < -8888) xdum(1) = missing

   iret = nf90_inq_varid(ncid,  varname, varid)
   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_1D:  Problem finding variable id."
   endif

   iret = nf90_put_var(ncid, varid, xdum, start = (/numRec/))
   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_1D Int:  Problem putting variable to NetCDF file."
   endif

 end SUBROUTINE put_var_1d_int
! ---------------------------------------------------------------------------

 SUBROUTINE put_var_2d_int(ncid, recs, varname, vardata)

   implicit none
   integer,                   intent(in) :: ncid
   integer, dimension(2),     intent(in) :: recs
   character(len=*),          intent(in) :: varname
   integer,                   intent(in) :: vardata

   integer                               :: iret
   integer                               :: varid
   integer, parameter :: missing = -888888
   integer, dimension(1) :: xdum

   xdum(1) = vardata
   if (xdum(1) < -888800.0) xdum(1) = missing

   iret = nf90_inq_varid(ncid,  varname, varid)
   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_2D:  Problem finding variable id."
   endif

   iret = nf90_put_var(ncid, varid, xdum, start = recs)
   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_2D int:  Problem putting variable to NetCDF file."
   endif

 end SUBROUTINE put_var_2d_int
! ---------------------------------------------------------------------------

 SUBROUTINE put_var_char(ncid, NumRec, varname, chardata)

   implicit none
   integer,                   intent(in) :: ncid
   integer,                   intent(in) :: NumRec
   character(len=*),          intent(in) :: varname
   character(len=*),          intent(in) :: chardata

   integer                               :: iret
   integer                               :: varid

   iret = nf90_inq_varid(ncid,  varname, varid)
   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_CHAR:  Problem finding variable id."
   endif

   !if (varname == 'name') print *,'PUT_VAR_CHAR: ',chardata

   iret = nf90_put_var(ncid, varid, chardata, start = (/1,numRec/), &
        count = (/len(chardata),1/))

   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      print *, NF90_STRERROR(iret)
      stop "Subroutine PUT_VAR_1D char:  Problem putting variable to NetCDF file."
   endif

 end SUBROUTINE put_var_char
! ---------------------------------------------------------------------------

 SUBROUTINE put_var_2d(ncid, recs, varname, vardata)

   implicit none
   integer,                   intent(in) :: ncid
   integer,dimension(2),      intent(in) :: recs
   character(len=*),          intent(in) :: varname
   real,                      intent(in) :: vardata

   integer                               :: iret
   integer                               :: varid
   real, parameter :: missing = -888888.
   real, dimension(1) :: xdum

   xdum(1) = vardata
   if (xdum(1) < -888800.0) xdum(1) = missing

   iret = nf90_inq_varid(ncid,  varname, varid)
   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_1D:  Problem finding variable id."
   endif

   iret = nf90_put_var(ncid, varid, xdum, start = recs)

   if (iret /= 0) then
      print *, 'varname = "'//varname//'"'
      stop "Subroutine PUT_VAR_1D:  Problem putting variable to NetCDF file."
   endif

 end SUBROUTINE put_var_2d
! ---------------------------------------------------------------------------

 SUBROUTINE insert_at_end (surface , new)

!  This takes a new measurement (new) and inserts it in a linked list
!  of measurements (surface points to first in list) in decreasing order of
!  pressure value.  If two levels' pressures are 'eps_equal', the levels
!  are merged instead of being linked.

   IMPLICIT NONE

   TYPE (measurement) ,  POINTER         :: surface , new

   TYPE (measurement) , POINTER          :: current , previous
   CHARACTER (LEN = 32) , PARAMETER      :: proc_name = 'insert_at'

   INTEGER                 :: error_number
   CHARACTER ( LEN = 132 ) :: error_message
   LOGICAL                 :: fatal , listing

   !  Initialize the variable to test the pressure and the place where the
   !  to-be-inserted measurement points.

   NULLIFY (new%next)

   !  The first check is to see if we are at the head of the linked list.  This
   !  drops us through to exit the routine.

   IF (.NOT. ASSOCIATED (surface)) THEN

      surface => new

   !  We are either between a couple of values, after a last value, 
   !  or we could need to be merged with a level.  All those tests are handled 
   !   in this else block.
   ELSE

      !  Initialize some dummy pointers to traverse to where we need to be.

      previous => surface 
      current => surface

      !  Loop to find correct location to link in 'new'.  
      !  The pressure is monotonically decreasing, so as soon as we find one 
      !  where the current pressure is less than the new pressure, 
      !  the new pressure goes just before it (or we run out of data looking!).
      !  Additionally, if both of the heights are equal AND the heights are
      !  the same as the input elevation of the station, then these need to 
      !  be merged surface observations.

      still_some_data : DO WHILE (ASSOCIATED (current))
         previous => current
         current => current%next
      END DO still_some_data

      ! We are now at the end of the list, insert new values

      previous%next => new

   end IF
 END SUBROUTINE insert_at_end

end program QCtoNC
