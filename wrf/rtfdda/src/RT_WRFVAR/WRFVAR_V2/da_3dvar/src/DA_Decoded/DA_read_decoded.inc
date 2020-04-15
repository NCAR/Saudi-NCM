
 SUBROUTINE read_decoded (file_num, total_number_of_obs, &
                          fatal_exceed, obs, n_obs, missing_flag, grid, &
#include <em_dummy_args.inc>
) 
!------------------------------------------------------------------------------!
!  This routine reads all observations, and measurements at all levels, 
!  from the file unit file_num (must be preably opened) and fill the 'obs' 
!  observation array.
!  For each observation, calls read_measurements to read all measurements for
!  that one observation.
!------------------------------------------------------------------------------!

   IMPLICIT NONE
   INTRINSIC MAX

   INTEGER,            INTENT (IN)   :: file_num 
   INTEGER,            INTENT (IN)   :: total_number_of_obs
   LOGICAL,            INTENT (IN)   :: fatal_exceed
   TYPE (report),      INTENT (OUT), &
                       DIMENSION (total_number_of_obs) :: obs
   INTEGER,            INTENT (OUT)  :: n_obs
   REAL,               INTENT (OUT)  :: missing_flag
   TYPE(domain),       INTENT(INOUT) :: grid

   INTEGER             :: io_error
   INTEGER             :: obs_num, meas_count
   INTEGER             :: error_ret 

   CHARACTER (LEN= 80) :: cause
   CHARACTER (LEN= 19) :: date_test
   INTEGER             :: num_empty , num_outside , num_bad_read, num_bad
   LOGICAL             :: drop_data, sounding
   LOGICAL             :: connected, go, file_ok
   LOGICAL             :: date_ok, is_sound
   INTEGER             :: final_levels, its_1, ifm, fm
   INTEGER             :: num_line, num_line_ground
   REAL                :: h_sfc, p_sfc, p_slp
   REAL                :: hdiff_below, pdiff_below
   REAL                :: hdiff_above, pdiff_above
   REAL                :: hdiff

   INTEGER             :: error_number
   CHARACTER (LEN=132) :: error_message
   LOGICAL             :: fatal, listing
   CHARACTER (LEN=100) :: error_string

   CHARACTER (LEN = 32), PARAMETER :: proc_name = 'read_decoded '

!  REAL, ALLOCATABLE :: psad(:,:), tmp2d(:,:)

   REAL, DIMENSION (:), ALLOCATABLE :: v_h, v_p
   REAL, DIMENSION (:), ALLOCATABLE :: v_t, v_q
   TYPE (measurement) , POINTER     :: current

!  Use of 3D-VAR interpolation package:

   TYPE (multi_level_type)      :: platform
   LOGICAL :: inside_halo, outside
   REAL    :: x0, dx0, dx1
   REAL    :: y0, dy0, dy1
   INTEGER :: i0, j0,k
  
#include <em_dummy_decl.inc>  
!------------------------------------------------------------------------------!

   CALL prelog
   WRITE  (log_unit,'(A)')  &
  "-----------------------------------------------------------------------"
   CALL prelog
   WRITE  (log_unit,'(A)') "Read observations:"

!---------------------------- LOOP THROUGH THE INPUT FILE ---------------------!

   !  Initialize a couple of counters for how many observations are either
   !  empty or outside of the domain.

   num_empty   = 0
   num_outside = 0
   num_bad_read= 0
   num_bad     = 0
   num_line    = 0

   num_line_ground = 0

   !  While we are not at the end of the observation file, keep reading data.

   obs_num = 1

!  ALLOCATE( tmp2d( xb%its : xb%ite, xb%jts : xb%jte) )

   read_obs : DO                                 

      sounding = .FALSE.
      cause = "no data"
      error_string = "no data"

      !  This is an array that we are filling.  Are we beyond that limit yet?

      IF ((obs_num .GT. total_number_of_obs) .AND. (fatal_exceed)) THEN

           error_message = proc_name // &
          "Too many obs for the NAMELIST value of max_number_of_obs_nml."

           CALL error_handler (log_unit, total_number_of_obs, &
                               error_message, .TRUE., .TRUE.)

      ELSE IF ((obs_num .GT. total_number_of_obs) .AND. (.NOT. fatal_exceed)) &
      THEN

           CALL prelog; WRITE (log_unit,'(A)') &
          "Have reached max_number_of_obs_nml (in NAMELIST)."
           CALL prelog; WRITE (log_unit,'(A)') &
          "finish reading and proceed."

!          CLOSE (file_num) 

           EXIT read_obs

      ENDIF

!---------------------------- READ GROUND INFORMATION -------------------------!

      !  The first read is the "once only" type of information.

      num_line = num_line + 1
      num_line_ground = num_line

      READ (file_num , IOSTAT = io_error , FMT = rpt_format) &
             obs(obs_num)%location ,   &
             obs(obs_num)%info ,       &
             obs(obs_num)%valid_time , &
             obs(obs_num)%ground 

!---------------------------- ERROR READING GROUND DATA -----------------------!

      !  If there are troubles with the "once only" type of data, we keep trying
      !  until we either come to the end of this report (and cycle) or we come
      !  to the end of all of the data (exit).

      IF (io_error .GT. 0) THEN

          CALL error_read (file_num, io_error, obs (obs_num), go)

          IF (go) THEN
               CYCLE read_obs
          ELSE
               EXIT read_obs
          ENDIF


!--------------------------- HIT END OF FILE ----------------------------------!

      ELSE IF (io_error .LT. 0) THEN


         !  While trying to find a good read, we came to the end of the file. 
         !  It happens to the best of us.

         CALL prelog
         WRITE  (log_unit,'(A,I3)') "Have reached end of unit: ", file_num

!        CLOSE (file_num) 

         EXIT read_obs

!--------------------------- SUCCESFUL READ OF GROUND DATA --------------------!

      ENDIF
!     ELSE IF (io_error .EQ. 0) THEN 

         !  No errors.  This is the intended way to find the end of data mark.

         IF (print_detail >= 2) THEN
         CALL prelog
         WRITE  (log_unit,'(A,I6,A,A5,1X,A11,1X,A,F6.2,A,F7.2,A,4A)',ADVANCE='no') &
                 "line ",num_line ,": id=",                                 &
                  obs(obs_num)%location%id,                                 &
                  obs(obs_num)%info%platform,     "at",                     &
                  obs(obs_num)%location%latitude, "N,",                     &
                  obs(obs_num)%location%longitude,"E on ",                  &
                  obs(obs_num)%valid_time%date_char (1:8),".",              &
                  obs(obs_num)%valid_time%date_char (9:14)
         ENDIF

         !  The previous read ("once only" data) was valid. If any of the data 
         !  is suspicious, the easiest place to clean it up is as soon as 
         !  we read it in, so we do not have to track through the array or 
         !  accidently hit undefined values that  are not exactly  "our" 
         !  undefined values.

         CALL check_ground (obs(obs_num)%ground)

!--------------------------- LOOK FOR BAD REPORT ------------------------------!

         !  Some data format errors are unrecoverable, throw the bad data away

         drop_data = .FALSE.

         !  Drop data with unreadable date string

         IF (.NOT. drop_data) THEN

             WRITE (date_test,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
                    obs (obs_num)%valid_time%date_char ( 1: 4), & 
                    obs (obs_num)%valid_time%date_char ( 5: 6), & 
                    obs (obs_num)%valid_time%date_char ( 7: 8), & 
                    obs (obs_num)%valid_time%date_char ( 9:10), & 
                    obs (obs_num)%valid_time%date_char (11:12), & 
                    obs (obs_num)%valid_time%date_char (13:14)

             IF (print_detail >= 1) THEN
                 date_ok = check_date (date_test, .FALSE., log_unit)
             ELSE
                 date_ok = check_date (date_test, .TRUE., log_unit)
             ENDIF


             IF (.NOT. date_ok) THEN
                 drop_data = .TRUE.
                 cause     = "bad date"
                 num_bad   = num_bad + 1
             ENDIF

         ENDIF

         !  Drop station with non-numeric WMO code

         IF (.NOT. drop_data) THEN

             READ (UNIT=obs(obs_num)%info%platform (4:6), FMT='(I3)', &
                   IOSTAT=ifm) fm

             IF (ifm /= 0) THEN
                 READ (UNIT=obs(obs_num)%info%platform (4:5), FMT='(I2)', &
                       IOSTAT=ifm) fm
             ENDIF

             IF (ifm > 0) THEN
                 drop_data = .TRUE.
                 cause     = "unknown WMO code in platform: "//&
                              TRIM (obs(obs_num)%info%platform)
                 num_bad   = num_bad + 1
             ENDIF

         ENDIF

         !  Drop stations without elevation, 
         !  Elevation is needed in surface_first, but missing elevation
         !  is OK if the missing flag is negative, so that any height 
         !  information for upper levels is greater than the station
         !  elevation.

!        IF (.NOT. drop_data) THEN

!            IF (eps_equal (obs(obs_num)%info%elevation, missing_r, 1.)) THEN 
!                drop_data = .TRUE.
!                cause = "missing elevation"
!                num_bad   = num_bad + 1
!            ENDIF

!        ENDIF

!------------- FILL THE 3D-VAR OBS STRUCTURE FOR CONVENIENCE ------------------!

         platform%info%platform = obs(obs_num)%info%platform
         WRITE (platform%info%date_char, &
              '(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')  &
                obs (obs_num)%valid_time%date_char ( 1: 4), & 
                obs (obs_num)%valid_time%date_char ( 5: 6), & 
                obs (obs_num)%valid_time%date_char ( 7: 8), & 
                obs (obs_num)%valid_time%date_char ( 9:10), & 
                obs (obs_num)%valid_time%date_char (11:12), & 
                obs (obs_num)%valid_time%date_char (13:14)
         platform%info%name     = obs(obs_num)%location%name
         platform%info%levels   = missing
         platform%info%lat      = obs(obs_num)%location%latitude
         platform%info%lon      = obs(obs_num)%location%longitude
         platform%info%elv      = obs(obs_num)%info%elevation
         platform%info%id       = obs(obs_num)%location%id

         platform%loc%slp%inv   = obs(obs_num)%ground%slp%data
         platform%loc%slp%qc    = obs(obs_num)%ground%slp%qc
         platform%loc%pw%inv    = missing_r
         platform%loc%pw%qc     = missing_r


!---------------------------- FILTER STATIONS ---------------------------------!

         !  The observations will be used in conjonction with MM5. Stations
         !  outside MM5 domains will not be used and are a memory burden,
         !  we toss out those observations.

         IF (.NOT. drop_data) THEN

             !  Compute x and y coordinates on unstagerred grid, in this
             !  context x refers to the first index and j to the second
             !  the coordinates are valid at the N-E p point of WRF A grid.
             !  This is cross points in MM5.

             CALL DA_ll_to_xy (platform%info, platform%loc,   &
                               xp, outside, inside_halo )

             !  The observations must be in the geographic domain
             !  Keep track of station location

             x0  = platform%loc%x
             y0  = platform%loc%y
             i0  = platform%loc%i
             j0  = platform%loc%j
             dx0 = platform%loc%dx
             dy0 = platform%loc%dy
             dx1 = platform%loc%dxm
             dy1 = platform%loc%dym

             !  Drop data outside domain

             IF (outside) THEN

                 drop_data   = .TRUE.

                 WRITE (cause,'(A,1X,2(A,F5.1,A,I3))') "outside domain:", &
                        "  y = ",y0,"/",xb % jte - xb % jts + 1, &
                        ", x = ",x0,"/",xb % ite - xb % its + 1

                 num_outside = num_outside + 1

             ELSE

             !  Compute the model height and full pressure profile 
             !  at station location on hlaf sigma level.
             !  See da_transfer_wrftoxb.inc for the definition of xb%h and xb%p

             IF (.NOT. ALLOCATED (v_h)) &
             ALLOCATE (v_h  (xp%kts:xp%kte))
             IF (.NOT. ALLOCATED (v_p)) &
             ALLOCATE (v_p  (xp%kts:xp%kte))
             IF (.NOT. ALLOCATED (v_t)) &
             ALLOCATE (v_t  (xp%kts:xp%kte))
             IF (.NOT. ALLOCATED (v_q)) &
             ALLOCATE (v_q  (xp%kts:xp%kte))

             DO k = xp%kts, xp%kte
                v_h (k) = dy1*(dx1*xb%h(i0,j0  ,k) + dx0*xb%h(i0+1,j0  ,k)) &
                        + dy0*(dx1*xb%h(i0,j0+1,k) + dx0*xb%h(i0+1,j0+1,k))
                v_p (k) = dy1*(dx1*xb%p(i0,j0  ,k) + dx0*xb%p(i0+1,j0  ,k)) &
                        + dy0*(dx1*xb%p(i0,j0+1,k) + dx0*xb%p(i0+1,j0+1,k))
                v_t (k) = dy1*(dx1*xb%t(i0,j0  ,k) + dx0*xb%t(i0+1,j0  ,k)) &
                        + dy0*(dx1*xb%t(i0,j0+1,k) + dx0*xb%t(i0+1,j0+1,k))
                ! Beware xb%q is the specific humidity, see routine da_transfer_wrftoxb
                v_q (k) = dy1*(dx1*xb%q(i0,j0  ,k) + dx0*xb%q(i0+1,j0  ,k)) &
                        + dy0*(dx1*xb%q(i0,j0+1,k) + dx0*xb%q(i0+1,j0+1,k))
             ENDDO

             ! Compute water vapor mixing ratio from specific humidity
             WHERE (v_q >= 1.)  v_q = 0.9999
             v_q  = v_q / (1. - v_q)

             ! Model terrain height at station location 
             h_sfc = dy1*(dx1*xb%terr (i0,j0)   + dx0*xb%terr (i0+1,j0))     &
                   + dy0*(dx1*xb%terr (i0,j0+1) + dx0*xb%terr (i0+1,j0+1))

             ! Model full pressure on model surface at station location 
             p_sfc = dy1*(dx1*xb%psfc (i0,j0  ) + dx0*xb%psfc  (i0+1,j0))     &
                   + dy0*(dx1*xb%psfc (i0,j0+1) + dx0*xb%psfc  (i0+1,j0+1))

             ! Model sea level pressure at station location 
             p_slp = dy1*(dx1*xb%slp  (i0,j0  ) + dx0*xb%slp   (i0+1,j0))     &
                   + dy0*(dx1*xb%slp  (i0,j0+1) + dx0*xb%slp   (i0+1,j0+1))

!            CALL Interp_lin_2D (xb % terr, xp%ims, xp%ime, xp%jms, xp%jme, &
!                                ii, jj, dx0, dy0, dx1, dy1, hsm )

             ENDIF

         ENDIF

         !  The observations must be valid around the analysis time,
         !  throw away data outside the time window defined in namelist

         IF (.NOT. drop_data) THEN

             CALL geth_idts (date_test, analysis_date, its_1)

             IF (.NOT. date_ok) THEN
                  its_1 = time_after
             ENDIF

             !  Keep track of station outside time window
             !  Time window is defined as
             !  [time_analysis-time_before, time_analysis+time_after]

             IF ((its_1 .LT. -time_before*60) .OR. (its_1 .GT. time_after*60)) THEN
                  drop_data   = .TRUE.
                  cause       = "outside time window"
                  num_outside = num_outside + 1
              ENDIF

         ENDIF

!---------------------------- READ UPPER-AIR DATA -----------------------------!

         !  Since no I/O errors, read 1 or more measurements.
         !  Note that obs(obs_num)%surface is pointer to first node in linked 
         !  list, so it is initially not pointing to anything.  

         NULLIFY (obs(obs_num)%surface)

         CALL read_measurements (file_num, obs(obs_num)%surface, &
         obs(obs_num)%location, obs(obs_num)%info, drop_data,    &
         error_ret, error_string,                                &
         obs(obs_num)%info%elevation, meas_count, num_line,      &
         x0, y0, h_sfc, p_sfc, p_slp,                            &
         xp%kts, xp%kte, v_h, v_p, v_t, v_q,                     &
         grid,                                                   &
#include <em_dummy_args.inc> 
)

!------------------- PROCESS UNEXPECTED END OF FILE IN READ_MEASUREMENT -------!

         IF (error_ret .EQ. eof_err) THEN

             IF (print_detail >= 2) THEN
                 WRITE (log_unit,'(1X,A)') "uncomplete (EOF encountered)"
             ENDIF

             IF (drop_data) THEN
                 num_bad      = num_bad - 1
                 num_bad_read = num_bad_read + 1
             ENDIF

             IF (ASSOCIATED (obs(obs_num)%surface)) THEN
                 !  dealloc entire linked list if it exists
                 CALL dealloc_meas (obs(obs_num)%surface)
             ENDIF

!            CLOSE (UNIT=file_num)
             EXIT read_obs

!------------------- PROCESS READ ERRORS IN READ_MEASUREMENT ------------------!

         ELSE IF (error_ret .EQ. read_err) THEN

         !  An error in the measurements read is handled in a couple of ways.  
         !  A flat out error in the read requires the process to start again 
         !  (cycle to read_obs).  If there was no data, we need to clean up 
         !  a bit of stuff, and read the famous last three integers that have 
         !  some QC information.

             IF (drop_data) THEN
                  num_bad      = num_bad      - 1
                  num_bad_read = num_bad_read + 1
             ENDIF

             drop_data = .TRUE.
!            cause = "read error"
             cause = TRIM (error_string)

             CALL error_read (file_num, io_error, obs (obs_num), go)
 
             IF (.NOT. go) THEN

                 IF (ASSOCIATED (obs(obs_num)%surface)) THEN
                     !  dealloc entire linked list if it exists
                     CALL dealloc_meas (obs(obs_num)%surface)
                 ENDIF

                 IF (print_detail >= 2) THEN
                     WRITE (log_unit,'(1X,A)') "read error, unrecoverable,"
                 ENDIF

                 EXIT read_obs

             ENDIF

!---------------------------- PROCESS EMPTY REPORTS ---------------------------!

         ELSE IF (error_ret .EQ. no_data) THEN

!            cause = "no data"
             cause = TRIM (error_string)

             IF (drop_data) THEN
                 num_bad = num_bad - 1
             ENDIF

             IF (TRIM (cause) .EQ. "no data") THEN
                 num_empty = num_empty + 1
             ELSE
                 num_bad = num_bad + 1
             ENDIF

             IF (.NOT. drop_data) THEN
                 drop_data = .TRUE.
             ENDIF

         ENDIF

!-------------- READ END OF REPORT --------------------------------------------!

         !  We have now read ground info and soundings, what follows in the
         !  standard format are three integers describing information gleaned
         !  from the program that generated the observational data.

         num_line = num_line + 1

         READ (file_num , IOSTAT = io_error , FMT = end_format) &
               obs(obs_num)%info%num_vld_fld, &
               obs(obs_num)%info%num_error,   &  
               obs(obs_num)%info%num_warning    

         !  Once again, after a read, was it successful. If not toss the whole 
         !  thing out (this is handled through the dealloc_meas routine if any 
         !  upper-air data was encountered). Discarding all of the ingested data
         !  may be a bit much, which is why the error print-out is provided.  
         !  After the  error is processed, the reading process is re-started.

         IF (io_error /= 0) THEN

             IF (drop_data) THEN
                  num_bad = num_bad - 1
                  num_bad_read = num_bad_read + 1
             ENDIF

             drop_data = .TRUE.
             cause = "read error"

         ENDIF

!---------------------------- DROP BAD REPORT ---------------------------------!

         IF (drop_data) THEN

             IF (print_detail >= 2) THEN
                 WRITE  (log_unit,'(1X,A)') TRIM (cause)
             ENDIF

            IF (ASSOCIATED (obs(obs_num)%surface)) THEN
               !  dealloc entire linked list if it exists
               CALL dealloc_meas (obs(obs_num)%surface)
            ENDIF

            CYCLE read_obs

         ELSE

           IF (print_detail >= 2) THEN
!              IF (meas_count > 1) THEN
               IF (obs(obs_num)%info%is_sound) THEN
                   WRITE (log_unit,'(I3,A)',ADVANCE='no') meas_count," upper-air level(s),"
               ELSE
                   WRITE (log_unit,'(I3,A)',ADVANCE='no') meas_count," surface level,"
               ENDIF
           ENDIF

         ENDIF

!--------------------- SORT VERTICAL LEVEL BY DECREASING PRESSURE -------------!
   
      !  Before we leave this loop, we make sure the surface level is the first
      !  level in the sounding. This test might results in removing levels

      !  Missing elevation is OK if the missing flag is negative, 
      !  so that any height information for upper levels is greater than 
      !  the station

!     IF (.NOT. eps_equal (obs(obs_num)%info%elevation, missing_r, 1.)) THEN

          CALL surf_first (obs(obs_num)%surface, obs(obs_num)%info%elevation,&
                           final_levels )

          !  Store the number of vertical levels in record num_vld_fld

          obs(obs_num)%info%num_vld_fld = final_levels

          !  Store the line number in input file in record seq_num

          obs(obs_num)%info%seq_num = num_line_ground

!     ENDIF

!--------------------- FLAG PATHOLOGIC SURFACE OBSERVATIONS -------------------!

      IF (.NOT. obs(obs_num)%info%discard) THEN

      ! Surface observations below model surface or above first level
      ! get flagged

      IF (.NOT. obs(obs_num)%info%is_sound) THEN

      ! Some stations came with 0 height, when they obviously are not at 
      ! sea level (as tells the model)

      IF (obs(obs_num)%surface%meas%pressure%qc .NE. missing .AND. &
          obs(obs_num)%surface%meas%height%qc   .NE. missing .AND. &
          obs(obs_num)%surface%meas%height%data .LE. 0.      .AND. &
          h_sfc .GE. 1.) THEN ! should be 0, but use 1m for round off
          obs(obs_num)%surface%meas%height%data = h_sfc
          obs(obs_num)%surface%meas%height%qc   = first_guess
      ENDIF

      ! Those quantities are > 0 when obs is below model surface

      hdiff_below = h_sfc - obs(obs_num)%surface%meas%height%data
      pdiff_below = obs(obs_num)%surface%meas%pressure%data - p_sfc

      IF (hdiff_below > 0. .OR. pdiff_below > 0.) THEN

          IF (obs(obs_num)%surface%meas%pressure%qc .NE. missing) THEN
              obs(obs_num)%surface%meas%pressure%qc = &
              obs(obs_num)%surface%meas%pressure%qc + surface_below
          ENDIF

          IF (obs(obs_num)%surface%meas%height%qc .NE. missing) THEN
              obs(obs_num)%surface%meas%height%qc = &
              obs(obs_num)%surface%meas%height%qc + surface_below
          ENDIF

          IF (print_detail >= 2) THEN
              WRITE (log_unit, '(1X,3(A,I5),A,A,I3,A)',ADVANCE='no')      &
             "station below surface:   Hm - Ho = ",                       &
              NINT (obs(obs_num)%surface%meas%height%data)," -",          &
              NINT (h_sfc),                         " =",                 &
              NINT (hdiff_below),"m ", "Dpres= ",                         &
              NINT (pdiff_below/100),"hPa"
          ENDIF

      ENDIF

      ! Those quantities are > 0 when obs is above the lowest half level 

      hdiff_above = obs(obs_num)%surface%meas%height%data - v_h (xp%kts)
      pdiff_above = v_p (xp%kts) - obs(obs_num)%surface%meas%pressure%data

      IF (hdiff_above > 0. .OR. pdiff_above > 0.) THEN

          IF (obs(obs_num)%surface%meas%pressure%qc .NE. missing) THEN
              obs(obs_num)%surface%meas%pressure%qc = &
              obs(obs_num)%surface%meas%pressure%qc + surface_above
          ENDIF

          IF (obs(obs_num)%surface%meas%height%qc .NE. missing) THEN
              obs(obs_num)%surface%meas%height%qc = &
              obs(obs_num)%surface%meas%height%qc + surface_above
          ENDIF

          IF (print_detail >= 2) THEN
              WRITE (log_unit, '(1X,3(A,I5),A,A,I3,A)',ADVANCE='no')     &
             "station above 1stlevel:  Ho - Hm = ",                       &
              NINT (obs(obs_num)%surface%meas%height%data)," -",          &
              NINT (v_h (xp%kts)),                  " =",                 &
              NINT (hdiff_above),"m ", "Dpres= ",                         &
              NINT (pdiff_above/100),"hPa"
          ENDIF

      ENDIF

!--------------------- FLAG PATHOLOGIC SOUNDINGS ------------------------------!

      ELSE IF (obs(obs_num)%info%num_vld_fld > 1) THEN

      ! Those quantities are > 0 when obs is below model surface

      hdiff_below = h_sfc - obs(obs_num)%surface%meas%height%data
      pdiff_below = obs(obs_num)%surface%meas%pressure%data - p_sfc

      ! When sounding starts below model surface

      IF (hdiff_below > 0. .OR. pdiff_below > 0.) THEN

      ! Flag all the sounding levles that are below model surface

          current => obs(obs_num)%surface
          hdiff   =  current%meas%height%data &
                  -  obs (obs_num)%surface%meas%height%data

          IF (print_detail >= 2) THEN
              WRITE (log_unit, '(1X,3(A,I5),A,A,I3,A)',ADVANCE='no')      &
             "base below surface: Hm - Ho = ",                            &
              NINT (obs(obs_num)%surface%meas%height%data)," -",          &
              NINT (h_sfc),                         " =",                 &
              NINT (hdiff_below),"m ", "Dpres= ",                         &
              NINT (pdiff_below/100),"hPa"
          ENDIF

	  DO WHILE (hdiff <= hdiff_below)  

             IF (current%meas%pressure%qc .NE. missing) THEN
                 current%meas%pressure%qc = &
                 current%meas%pressure%qc + surface_below
             ENDIF
             IF (current%meas%height%qc   .NE. missing) THEN
                 current%meas%height%qc = &
                 current%meas%height%qc + surface_below
              ENDIF

             current => current%next

             IF (.NOT. ASSOCIATED (current)) EXIT

             hdiff = current%meas%height%data &
                   - obs (obs_num)%surface%meas%height%data

          ENDDO


      ENDIF

      ENDIF ! end is_sound

     ENDIF ! end discard
!--------------------- GO TO NEXT OBSERVATIONS --------------------------------!


      !  Have read observation and its measurement(s) without error
      !  so continue to next observation.

      IF (print_detail >= 2) WRITE (log_unit,'(A)') ""

      obs_num = obs_num + 1

   END DO read_obs

!---------------------------- FREE MEMORY -------------------------------------!

   DEALLOCATE (v_h)
   DEALLOCATE (v_p)
   DEALLOCATE (v_t)
   DEALLOCATE (v_q)
!  DEALLOCATE( tmp2d )

!---------------------------- PRINT OUT AND EXIT ------------------------------!

   CLOSE (file_num)

   !  The end of the observation file has been reached. Decrement the counter to
   !  get the total number of observations successfully read by the program.  
   !  Output this information to the outside world.  We can also provide the 
   !  information  on the observations that are NOT included in the analysis.

   obs_num = obs_num - 1

   !  Print out

   CALL prelog
   WRITE  (log_unit, &
         '("Number of observations found in the file:        ",i6,".")') &
            obs_num+num_empty+num_bad_read+num_bad+num_outside
   CALL prelog
   WRITE  (log_unit, &
         '("Number of unreadable observations discarded      ",i6,".")') &
            num_bad_read
   CALL prelog
   WRITE  (log_unit, &
         '("Number of empty observations discarded:          ",i6,".")') &
            num_empty
   CALL prelog
   WRITE  (log_unit, &
         '("Number of bad observations discarded:            ",i6,".")') &
            num_bad
   CALL prelog
   WRITE  (log_unit, &
         '("Number of observations outside domain discarded: ",i6,".")') &
            num_outside
   CALL prelog
   WRITE  (log_unit,  &
         '("Number of observations successfully ingested:    ",i6,".")') &
            obs_num

   !  Output 

   n_obs = obs_num
   missing_flag = missing_r

 END SUBROUTINE read_decoded
!------------------------------------------------------------------------------!

 SUBROUTINE read_measurements (file_num, surface, location, info, &
                               bad_data, error, error_string,     & 
                               elevation, meas_count, num_line,   &
                               x0, y0, h_sfc, p_sfc, p_slp,       &
                               kts, kte,                          &
                               height1d, pressure1d,              &
                               temperature1d, watermixing1d,      &
                               grid,                              &
#include <em_dummy_args.inc> 
)
!------------------------------------------------------------------------------!
!  This routine reads in 'measurements' at as many levels as there are in
!  the report, then stops and returns when an end-of-measurements flag is
!  found.  If any reads produce error, return error code which causes entire
!  observation to be discarded (ob is not discarded on eof error).
!------------------------------------------------------------------------------!

   IMPLICIT NONE
   INTRINSIC MAX

   INTEGER , INTENT (IN)                  :: file_num   ! file unit to read  
   TYPE (measurement) ,   POINTER         :: surface    ! ptr to 1st msmt
   TYPE (location_type) , INTENT (IN)     :: location   ! 5 digit ID, name, etc
   TYPE (source_info) ,   INTENT (IN)     :: info       ! Station info
   LOGICAL , INTENT (IN)                  :: bad_data   ! read, but do not store
   INTEGER , INTENT (OUT)                 :: error      ! err and type 
   CHARACTER (LEN =100),  INTENT (out)    :: error_string
   INTEGER , INTENT (INOUT)               :: num_line      
   REAL ,    INTENT (IN)                  :: elevation
   INTEGER,  INTENT (OUT)                 :: meas_count
   REAL,     INTENT (IN)                  :: x0, y0
   REAL,     INTENT (IN)                  :: h_sfc, p_sfc, p_slp
   INTEGER,  INTENT (IN)                  :: kts, kte
   REAL, DIMENSION (kts:kte), INTENT (in) :: height1d, pressure1d
   REAL, DIMENSION (kts:kte), INTENT (in) :: temperature1d, watermixing1d
   TYPE(domain), intent(inout) :: grid

   CHARACTER (LEN = 32) , PARAMETER       :: proc_name = 'read_measurements'
   INTEGER                                :: io_error
   TYPE (measurement) , POINTER           :: current
   CHARACTER (LEN = 40)                   :: location_id, location_name

   INTEGER                                :: error_number
   CHARACTER ( LEN = 132 )                :: error_message
   LOGICAL                                :: fatal, listing

   INTEGER :: i0, j0, k
   REAL    :: dx0, dy0, dx1, dy1
   REAL    :: dh

#include <em_dummy_decl.inc>  
!------------------------------------------------------------------------------!

   !  Initialize dummy pointers and counters and observation names, and such.

   ALLOCATE (current)
   NULLIFY (current%next)
   NULLIFY (surface)
   error = ok
   meas_count = 0
   location_id   = TRIM (location%id)
   location_name = TRIM (location%name)

   !  This loop continues until either an error occurs, or until the end of
   !  the measurement tag is found (the graceful exit).

   read_meas: DO 

    IF (print_detail > 3) THEN
        WRITE (log_unit,'(2A)') " error_string = ",error_string
    ENDIF
!------------- READ ONE LEVEL -------------------------------------------------!

      !  Currently, this read puts in 12 pairs of data, a real observation
      !  value and the accompanying QC flag. 

      num_line = num_line + 1
      READ  (file_num , IOSTAT = io_error , FMT = meas_format)  current%meas

!------------- PROCESS ERROR --------------------------------------------------!

      !  An error < 0 means the end of the file (usually), and an error > 0
      !  is just a broken read.  Describe the read error so that the calling
      !  routine knows what happened, then exit this loop (which is exiting
      !  this routine, basically).

      IF (io_error .GT. 0) THEN
         error = read_err
         error_string = "read error"
!        CLOSE (file_num) 
         EXIT read_meas
      ELSE IF (io_error .LT. 0) THEN
         error = eof_err
!        error_string = "end of file"
!        CLOSE (file_num) 
         EXIT read_meas
      ENDIF

!------------- CHECK LEVEL VALIDITY -------------------------------------------!

      !  If we know a priori that this data is bad, no tests are necessary on
      !  the various flags values.

      bad_loop_1 : IF (.NOT. bad_data) THEN
   
         !  A successful read, yahoo!  As the data may not have the flags set up
         !  the  way we want, go through directly after this read and make sure
         !  that any special values are all set to missing.
   
            CALL check_meas (current%meas)

      ENDIF bad_loop_1

!------------- PROCESS END OF REPORT ------------------------------------------!

      !  The data we just read in could have been the flag for the end of 
      !  the measurement.
      !  This is the graceful way to exit this routine.  If this is the end of 
      !  the measurement section for this observation, set all of the data to 
      !  the same end of measurement value, just in case there were some stray 
      !  unset values in the generating program.

      IF ((eps_equal (current%meas%pressure%data , end_data_r , 1.)) .OR. &
          (eps_equal (current%meas%height%data   , end_data_r , 1.))) THEN
           call set_meas (current%meas)
           error = ok
           EXIT read_meas
      ENDIF

      !  If this is bad data, we needed to make sure that the ending measurement
      !  is the famous end_data flag so that we hit a correct exit from this
      !  loop and routine.  We can just cycle the read loop again.

      IF (bad_data) THEN
         error_string = "read error"
         CYCLE read_meas
      ENDIF

!------------- DISCARD REPORTS WITHOUT DATA -----------------------------------!

      !  One level should at least contain one of those fields: p, T, Td, RH or
      !  Speed and Direction.

      IF (.NOT. level_complete (current%meas)) THEN
          WRITE (error_string, '(A)') "no data"
!         IF (print_detail >= 2) THEN
!             CALL prelog; WRITE (log_unit,'(A)',ADVANCE='no') &
!             "no data"
!         ENDIF
          CYCLE read_meas
      ENDIF

!------------- RESET HEIGHT OF AMDAR, TAMDAR AND ACARS DATA -------------------!
! IAF ACARS come only with height, do not reset height for them

      if (location%name(1:25) .NE. " ACARS FROM IMS/IAF AMDAR") THEN

      IF (index (location%name,'ACARS')  > 0 .OR. &
          index (location%name,'AMDAR')  > 0) THEN
          IF (eps_not (current%meas%height%data   , missing_r , 1.)) THEN
              IF (print_detail >= 2) THEN
              CALL prelog; WRITE (log_unit,'(3A,I7)',ADVANCE='no')  &
             "Height for ",TRIM  (location%name)," is ",&
              NINT (current % meas % height % data)
              ENDIF
              current % meas % height % data = missing_r
              current % meas % height % qc   = missing
          ENDIF
      ENDIF

      ENDIF

!------------- SOME REPORTS INDICATE 101301 INSTEAD OF STATION PRESSURE -------!

      IF (eps_equal (current%meas%pressure%data, 101301. , 1.) .AND. &
          eps_not   (current%meas%height%data, missing_r , 1.)) THEN
          IF (current%meas%height%data > 0) THEN
              IF (print_detail >= 2) THEN
                  CALL prelog; WRITE (log_unit,'(2(3A,I7))',ADVANCE='no')  &
                 "Pressure for ",TRIM  (location%name)," is ",&
                  NINT (current % meas % pressure % data), " while"," ", &
                 "Height is ", NINT (current % meas % height % data)
              ENDIF
              current % meas % pressure % data = missing_r
              current % meas % height % qc   = missing
          ENDIF
      ENDIF

!------------- RESET QC BETWEEN 0 and 10 FOR NONE MISSING DATA ----------------!

      CALL reset_qc (current%meas)

!------------- RECOVER PRESSURE OR HEIGHT FROM MODEL NON HYDROSTATIC STATE ----!


      IF ((eps_equal (current % meas % pressure % data, missing_r, 1.)) .OR. &
          (eps_equal (current % meas % height   % data, missing_r, 1.))) THEN

           !  Recover height or pressure

           CALL recoverhp (current%meas%height, current%meas%pressure, &
                           xp, kts, kte, &
                           height1d, pressure1d, temperature1d, watermixing1d,&
                           h_sfc, p_sfc, p_slp)

      ENDIF

!------------- MAKE SURE PRESSURE AND HEIGHT QC ARE CONSISTENT WITH DATA ------!

      IF (eps_equal (current%meas%pressure%data, missing_r , 1.)) THEN
          current%meas%pressure%qc = missing
      ENDIF

      IF (eps_equal (current%meas%height%data, missing_r , 1.)) THEN
          current%meas%height%qc = missing
      ENDIF

!------------- DISCARD REPORTS WITH MISSING PRESSURE AND HEIGHT ---------------!

      !  If the pressure is still missing after trying our darnedest to get 
      !  something from the height and temperature, we admit failure, 
      !  and discard this measurement (cycle to the next measurement level).

      IF ((eps_equal (current%meas%pressure%data, missing_r , 1.))  .OR. &
          (eps_equal (current%meas%height  %data, missing_r , 1.))) THEN
           current%meas%pressure%data = missing_r
           current%meas%pressure%qc   = missing
           current%meas%height  %data = missing_r
           current%meas%height  %qc   = missing
           error_string = "missing pressure or height"
           CYCLE read_meas
      ENDIF

!------------- DISCARD REPORTS WITH UNREALISTIC HEIGHT OR PRESSURE ------------!

      !  If the pressure is null or negative, it's certainly an error 
      !  discard measurement.

      IF (current%meas%pressure%data <= 0.) THEN
          WRITE (error_string, '(A,F9.0)') &
         "negative pressure P = ",current%meas%pressure%data 
          CYCLE read_meas
      ENDIF 

      !  If the height is negative, the pressure was certainly SLP,
      !  discard measurement.

      IF (current%meas%height%data < 0.) THEN
          WRITE (error_string, '(A,F9.0)') &
         "negative height H = ",current%meas%height%data 
          CYCLE read_meas
      ENDIF 

!------------- DISCARD ANY OBS ABOVE MODEL TOP (HALF) LAYER -------------------!

      IF (current % meas % height   % data >= height1d   (xp%kte) .OR. &
          current % meas % pressure % data <= pressure1d (xp%kte)) THEN
          WRITE (error_string, '(2(A,I5,A))')                                  &
          "above model top H = ", NINT (current % meas % height % data),"m, ", &
          "pres = ",              NINT (current % meas % pressure % data/100), &
          "hPa"
          CYCLE read_meas
      ENDIF

!--- DISCARD ANY OBS BELOW MODEL SURFACE MORE THAN hsfc_below_diff_max meters -!

      dh = h_sfc - current % meas % height % data

      IF (dh > hsfc_below_diff_max) THEN
          WRITE (error_string, '(3(A,I5,A))')                   &
          "More than ",NINT (hsfc_below_diff_max)," m below model sfc:",  &
          "hm = ", NINT (h_sfc),"m, ",                          &
          "ho = ", NINT (current % meas % height % data),"m"
          CYCLE read_meas
      ENDIF

!------------- DIAGNOSTIC QUANTITIES ------------------------------------------!

      !  Compute diagnostics: TD from RH or RH from TD and Wind components 
      !  from speed and direction

      CALL diagnostics_moist (current%meas)
      CALL diagnostics_wind  (current%meas, location%longitude)

!------------- SORT LEVELS FROM SURFACE TO TOP --------------------------------!

      !  Increment count of measurements correctly read in 
      !  (these are the levels).

      meas_count = meas_count + 1

      !  Since it seems that everything went ok, insert this measurement ordered
      !  by pressure.

      CALL insert_at (surface , current , elevation)

      !  Allocate space for another measurement, so we can go try and read 
      !  another level in this routine.  Initialize it to pointing to nothing.

      ALLOCATE (current)
      NULLIFY  (current%next)

   END DO read_meas  
 
   !  The last allocated measurement is not used (no matter how loop is exited)
   !  so deallocate space.

   if(associated(current))DEALLOCATE (current)

!------------- PROCESS ERRORS  ------------------------------------------------!

   !  If unable to read in at least one measurement, return error so that
   !  entire observation is discarded.  If this was bad data, we forced it
   !  to skip over the observations without storing any data.  That will be
   !  handled in the calling routine.

   IF ((meas_count .LT. 1 ) .AND. &
       (error      .EQ. ok) .AND. &
       (.NOT. bad_data    )) THEN
        error = no_data
   ENDIF 

   IF (print_detail > 3) THEN
       WRITE (log_unit,'(2A)') " error_string = ",error_string
   ENDIF

 END SUBROUTINE read_measurements
!------------------------------------------------------------------------------!

 SUBROUTINE check_ground (ground)
!------------------------------------------------------------------------------!
   IMPLICIT NONE

   TYPE (terrestrial)   :: ground
!------------------------------------------------------------------------------!

         !  These tests are for the ground type data.  Missing data is OK, 
         !  but sometimes it comes in as undefined, which meant bad data.  
         !  Set it all to missing.

         IF ((ground%slp%data         .GT. (undefined1_r - 1.))  .OR. &
             (ground%slp%data         .LT. (undefined2_r + 1.))) THEN
              ground%slp%data         = missing_r
         ENDIF
         IF ((ground%ref_pres%data    .GT. (undefined1_r - 1.))  .OR. &
             (ground%ref_pres%data    .LT. (undefined2_r + 1.))) THEN
              ground%ref_pres%data    = missing_r
         ENDIF
         IF ((ground%ground_t%data    .GT. (undefined1_r - 1.))  .OR. &
             (ground%ground_t%data    .LT. (undefined2_r + 1.))) THEN
              ground%ground_t%data    = missing_r
         ENDIF
         IF ((ground%sst%data         .GT. (undefined1_r - 1.))  .OR. &
             (ground%sst%data         .LT. (undefined2_r + 1.))) THEN
              ground%sst%data         = missing_r
         ENDIF
         IF ((ground%psfc%data        .GT. (undefined1_r - 1.))  .OR. &
             (ground%psfc%data        .LT. (undefined2_r + 1.))) THEN
              ground%psfc%data        = missing_r
         ENDIF
         IF ((ground%precip%data      .GT. (undefined1_r - 1.))  .OR. &
             (ground%precip%data      .LT. (undefined2_r + 1.))) THEN
              ground%precip%data      = missing_r
         ENDIF
         IF ((ground%t_max%data       .GT. (undefined1_r - 1.))  .OR. &
             (ground%t_max%data       .LT. (undefined2_r + 1.))) THEN
              ground%t_max%data       = missing_r
         ENDIF
         IF ((ground%t_min%data       .GT. (undefined1_r - 1.))  .OR. &
             (ground%t_min%data       .LT. (undefined2_r + 1.))) THEN
              ground%t_min%data       = missing_r
         ENDIF
         IF ((ground%t_min_night%data .GT. (undefined1_r - 1.))  .OR. &
             (ground%t_min_night%data .LT. (undefined2_r + 1.))) THEN
              ground%t_min_night%data = missing_r
         ENDIF
         IF ((ground%p_tend03%data    .GT. (undefined1_r - 1.))  .OR. &
             (ground%p_tend03%data    .LT. (undefined2_r + 1.))) THEN
              ground%p_tend03%data    = missing_r
         ENDIF
         IF ((ground%p_tend24%data    .GT. (undefined1_r - 1.))  .OR. &
             (ground%p_tend24%data    .LT. (undefined2_r + 1.))) THEN
              ground%p_tend24%data    = missing_r
         ENDIF
         IF ((ground%cloud_cvr%data   .GT. (undefined1_r - 1.))  .OR. &
             (ground%cloud_cvr%data   .LT. (undefined2_r + 1.))) THEN
              ground%cloud_cvr%data   = missing_r
         ENDIF
         IF ((ground%ceiling%data     .GT. (undefined1_r - 1.))  .OR. &
             (ground%ceiling%data     .LT. (undefined2_r + 1.))) THEN
              ground%ceiling%data     = missing_r
         ENDIF

         !  Make sure QC is turned to missing when data is missing

         IF (eps_equal (ground%slp%data, missing_r, 1.)) THEN
              ground%slp%data         = missing_r
              ground%slp%qc           = missing_r
         ENDIF
         IF (eps_equal (ground%ref_pres%data, missing_r, 1.)) THEN
              ground%ref_pres%data    = missing_r
              ground%ref_pres%qc      = missing_r
         ENDIF
         IF (eps_equal (ground%ground_t%data, missing_r, 1.)) THEN
              ground%ground_t%data    = missing_r
              ground%ground_t%qc      = missing_r
         ENDIF
         IF (eps_equal (ground%sst%data, missing_r, 1.)) THEN
              ground%sst%data         = missing_r
              ground%sst%qc           = missing_r
         ENDIF
         IF (eps_equal (ground%psfc%data, missing_r, 1.)) THEN
              ground%psfc%data        = missing_r
              ground%psfc%qc          = missing_r
         ENDIF
         IF (eps_equal (ground%precip%data, missing_r, 1.)) THEN
              ground%precip%data      = missing_r
              ground%precip%qc   = missing_r
         ENDIF
         IF (eps_equal (ground%t_max%data, missing_r, 1.)) THEN
              ground%t_max%data       = missing_r
              ground%t_max%qc         = missing_r
         ENDIF
         IF (eps_equal (ground%t_min%data, missing_r, 1.)) THEN
              ground%t_min%data       = missing_r
              ground%t_min%qc         = missing_r
         ENDIF
         IF (eps_equal (ground%t_min_night%data, missing_r, 1.)) THEN
              ground%t_min_night%data = missing_r
              ground%t_min_night%qc   = missing_r
         ENDIF
         IF (eps_equal (ground%p_tend03%data, missing_r, 1.)) THEN
              ground%p_tend03%data    = missing_r
              ground%p_tend03%qc      = missing_r
         ENDIF
         IF (eps_equal (ground%p_tend24%data, missing_r, 1.)) THEN
              ground%p_tend24%data    = missing_r
              ground%p_tend24%qc      = missing_r
         ENDIF
         IF (eps_equal (ground%cloud_cvr%data, missing_r, 1.)) THEN
              ground%cloud_cvr%data   = missing_r
              ground%cloud_cvr%qc     = missing_r
         ENDIF
         IF (eps_equal (ground%ceiling%data, missing_r, 1.)) THEN
              ground%ceiling%data     = missing_r
              ground%ceiling%qc       = missing_r
         ENDIF

 END SUBROUTINE check_ground
!------------------------------------------------------------------------------!

 SUBROUTINE check_meas (meas)
!------------------------------------------------------------------------------!
   IMPLICIT NONE
 
   TYPE (meas_data) :: meas
!------------------------------------------------------------------------------!


         !  Look for undefined value

         IF ((meas%height%data      .GT. (undefined1_r - 1.))  .OR. &
             (meas%height%data      .LT. (undefined2_r + 1.))) THEN
              meas%height%data      = missing_r
              meas%height%qc        = missing_r
         ENDIF
         IF ((meas%temperature%data .GT. (undefined1_r - 1.))  .OR. &
             (meas%temperature%data .LT. (undefined2_r + 1.))) THEN
              meas%temperature%data = missing_r
              meas%temperature%qc   = missing_r
         ENDIF
         IF ((meas%dew_point%data   .GT. (undefined1_r - 1.))  .OR. &
             (meas%dew_point%data   .LT. (undefined2_r + 1.))) THEN
              meas%dew_point%data   = missing_r
              meas%dew_point%qc     = missing_r
         ENDIF
         IF ((meas%speed%data       .GT. (undefined1_r - 1.))  .OR. &
             (meas%speed%data       .LT. (undefined2_r + 1.))) THEN
              meas%speed%data       = missing_r
              meas%speed%qc         = missing_r
         ENDIF
         IF ((meas%direction%data   .GT. (undefined1_r - 1.))  .OR. &
             (meas%direction%data   .LT. (undefined2_r + 1.))) THEN
              meas%direction%data   = missing_r
              meas%direction%qc     = missing_r
         ENDIF
         IF ((meas%u%data           .GT. (undefined1_r - 1.))  .OR. &
             (meas%u%data           .LT. (undefined2_r + 1.))) THEN
              meas%u%data           = missing_r
              meas%u%qc             = missing_r
         ENDIF
         IF ((meas%v%data           .GT. (undefined1_r - 1.))  .OR. &
             (meas%v%data           .LT. (undefined2_r + 1.))) THEN
              meas%v%data           = missing_r
              meas%v%qc             = missing_r
         ENDIF
         IF ((meas%pressure%data    .GT. (undefined1_r - 1.))  .OR. &
             (meas%pressure%data    .LT. (undefined2_r + 1.))) THEN
              meas%pressure%data    = missing_r
              meas%pressure%qc      = missing_r
         ENDIF
         IF ((meas%thickness%data   .GT. (undefined1_r - 1.))  .OR. &
             (meas%thickness%data   .LT. (undefined2_r + 1.))) THEN
              meas%thickness%data   = missing_r
              meas%thickness%qc     = missing_r
         ENDIF

         !  Missing temperature sometimes come as 99999.0

         IF (meas%temperature%data .GT. (   99999.0   - 1.))   THEN
             meas%temperature%data = missing_r
             meas%temperature%qc   = missing_r
         ENDIF

         !  Missing height sometimes comes as ABS (missing)

         IF (eps_equal (meas%height%data, ABS (missing_r), 1.)) THEN
              meas%height%data    = missing_r
              meas%height%qc      = missing
         ENDIF

         !  Make sure that QC is turned to missing when data is missing

         IF (eps_equal (meas%temperature%data, missing_r, 1.)) THEN
              meas%temperature%data = missing_r
              meas%temperature%qc   = missing_r
         ENDIF
         IF (eps_equal (meas%dew_point%data, missing_r, 1.)) THEN
              meas%dew_point%data   = missing_r
              meas%dew_point%qc     = missing_r
         ENDIF
         IF (eps_equal (meas%speed%data, missing_r, 1.)) THEN
              meas%speed%data       = missing_r
              meas%speed%qc         = missing_r
         ENDIF
         IF (eps_equal (meas%direction%data, missing_r, 1.)) THEN
              meas%direction%data   = missing_r
              meas%direction%qc     = missing_r
         ENDIF
         IF (eps_equal (meas%u%data, missing_r, 1.)) THEN
              meas%u%data           = missing_r
              meas%u%qc             = missing_r
         ENDIF
         IF (eps_equal (meas%v%data, missing_r, 1.)) THEN
              meas%v%data           = missing_r
              meas%v%qc             = missing_r
         ENDIF
         IF (eps_equal (meas%pressure%data, missing_r, 1.)) THEN
              meas%pressure%data    = missing_r
              meas%pressure%qc      = missing_r
         ENDIF
         IF (eps_equal (meas%thickness%data, missing_r, 1.)) THEN
              meas%thickness%data   = missing_r
              meas%thickness%qc     = missing_r
         ENDIF


 END SUBROUTINE check_meas
!------------------------------------------------------------------------------!

 SUBROUTINE set_meas (meas)
!------------------------------------------------------------------------------!
   IMPLICIT NONE

   TYPE (meas_data) :: meas
!------------------------------------------------------------------------------!

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
!------------------------------------------------------------------------------!

 SUBROUTINE reset_qc (meas)
!------------------------------------------------------------------------------!
   IMPLICIT NONE
 
   TYPE (meas_data) :: meas
!------------------------------------------------------------------------------!

         !  Set to 0 any none missing QC flag greater negative or 
         !  greater than 10

         IF (eps_not (meas%pressure%data, missing_r, 1.)) THEN
             IF (meas%pressure%qc < 0 .OR. meas%pressure%qc > 10) &
                 meas%pressure%qc = 0
         ENDIF
         IF (eps_not (meas%height%data, missing_r, 1.)) THEN
             IF (meas%height%qc < 0 .OR. meas%height%qc > 10) &
                 meas%height%qc = 0
         ENDIF
         IF (eps_not (meas%temperature%data, missing_r, 1.)) THEN
             IF (meas%temperature%qc < 0 .OR. meas%temperature%qc > 10) &
                 meas%temperature%qc = 0
         ENDIF
         IF (eps_not (meas%dew_point%data, missing_r, 1.)) THEN
             IF (meas%dew_point%qc < 0 .OR. meas%dew_point%qc > 10) &
                 meas%dew_point%qc = 0
         ENDIF
         IF (eps_not (meas%speed%data, missing_r, 1.)) THEN
             IF (meas%speed%qc < 0 .OR. meas%speed%qc > 10) &
                 meas%speed%qc = 0
         ENDIF
         IF (eps_not (meas%direction%data, missing_r, 1.)) THEN
             IF (meas%direction%qc < 0 .OR. meas%direction%qc > 10) &
                 meas%direction%qc = 0
         ENDIF
         IF (eps_not (meas%u%data, missing_r, 1.)) THEN
             IF (meas%u%qc < 0 .OR. meas%u%qc > 10) &
                 meas%u%qc = 0
         ENDIF
         IF (eps_not (meas%v%data, missing_r, 1.)) THEN
             IF (meas%v%qc < 0 .OR. meas%v%qc > 10) &
                 meas%v%qc = 0
         ENDIF
         IF (eps_not (meas%rh%data, missing_r, 1.)) THEN
             IF (meas%rh%qc < 0 .OR. meas%rh%qc > 10) &
                 meas%rh%qc = 0
         ENDIF
         IF (eps_not (meas%thickness%data, missing_r, 1.)) THEN
             IF (meas%thickness%qc < 0 .OR. meas%thickness%qc > 10) &
                 meas%thickness%qc = 0
         ENDIF


 END SUBROUTINE reset_qc
!------------------------------------------------------------------------------!

 FUNCTION level_complete (meas)
!------------------------------------------------------------------------------!
   IMPLICIT NONE

   TYPE (meas_data) :: meas
   LOGICAL          :: level_complete
!------------------------------------------------------------------------------!

    !  Check that at least one entry is valid

    IF (eps_not (meas%pressure   %data, missing_r, 1.)  .OR. &
        eps_not (meas%temperature%data, missing_r, 1.)  .OR. &
        eps_not (meas%dew_point  %data, missing_r, 1.)  .OR. &
        eps_not (meas%rh         %data, missing_r, 1.)  .OR. &
        eps_not (meas%speed      %data, missing_r, 1.)) THEN
!FV 20100226 Allow speed without direction
!        eps_not (meas%direction  %data, missing_r, 1.))) THEN
         level_complete = .TRUE.
    ELSE
         level_complete = .FALSE.
    ENDIF
 
 END FUNCTION level_complete
!------------------------------------------------------------------------------!
 SUBROUTINE error_read (file_num, io_error, obs, go)

!------------------------------------------------------------------------------!
    IMPLICIT NONE

    INTEGER,            INTENT (in)    :: file_num
    INTEGER,            INTENT (inout) :: io_error
    TYPE (report),      INTENT (inout) :: obs
    LOGICAL,            INTENT (out)   :: go

    TYPE (meas_data)   :: dummy_middle
    INTEGER            :: bad_count
!------------------------------------------------------------------------------!

    go = .FALSE.
         
!   CALL prelog
!   WRITE  (log_unit, '(A,A,A,A)') "Troubles with measurement lines ",&
!                                   TRIM (obs%location%id) , ' ',     &
!                                   TRIM (obs%location%name) 

    !  Keep track of how many loops we are taking so this is not infinite.

    bad_count = 0

    DO WHILE (io_error .GE. 0)

       bad_count = bad_count + 1

       IF (bad_count .LT. 1000) THEN

           READ (UNIT=file_num , IOSTAT = io_error , FMT = meas_format)  &
                 dummy_middle

           IF (eps_equal (dummy_middle%pressure%data, end_data_r , 1.)) THEN

!              READ (UNIT=file_num , IOSTAT = io_error , FMT = end_format) &
!                    obs%info%num_vld_fld , &
!                    obs%info%num_error ,   &  
!                    obs%info%num_warning    
                 
!              CALL prelog
!              WRITE  (log_unit,'(A)') "Read recovered"
!!              CYCLE read_obs
               go = .TRUE.
               RETURN

           ENDIF

       ELSE

!          CALL prelog
!          WRITE (log_unit,'(A)') &
!         "Too many attempts to read the data correctly. Exiting read loop."
!!          EXIT read_obs
           go = .FALSE.
           RETURN

       ENDIF
    END DO 

 END SUBROUTINE error_read
!------------------------------------------------------------------------------!
