!------------------------------------------------------------------------------!
! Sort observations by location and time,
! Merge space duplicate stations (same type, same location, same time),
! Remove time duplicate stations in time (same type & location, different time),
!
! D. GILL,         April 1998
! F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!

 SUBROUTINE check_duplicate_loc (num_obs, obs, index, &
                                 total_merge, time_analysis)

!  Checks array of reports (obs), which has a sorted index to the reports,
!  to determine if any reports are for the same time/location.  If so,
!  and the data is duplicated exactly in all fields, one is discarded.  If
!  they are from same time/location and data is not identical, data from
!  two reports is merged:  'missing' is replaced by known values; data at
!  different levels is merged into one linked list.

   IMPLICIT NONE

   TYPE ( report ) , INTENT ( INOUT ) , DIMENSION ( : ) :: obs
   INTEGER         , INTENT ( IN )    , DIMENSION ( : ) :: index
   INTEGER         , INTENT ( IN )                      :: num_obs 

   INTEGER                                :: current , &
                                             next    , & 
                                             first   , &
                                             second
   INTEGER         , INTENT ( OUT )       :: total_merge
!  INTEGER         , INTENT ( IN  )       :: date    , &
!                                            time
   CHARACTER (LEN = 19)                   :: time_analysis
   CHARACTER (LEN = 24)                   :: time24
   INTEGER                                :: total_valid
   INTEGER                                :: total_dups, total_left

   INTEGER :: century_year, month, day
   INTEGER :: hour, minute, seconds
   INTEGER :: ten_thousandth
   INTEGER :: date, time
   INTEGER :: io_error

   CHARACTER (LEN =  80):: filename
   CHARACTER (LEN =  32):: proc_name = "check_duplicate_loc "
   CHARACTER (LEN = 160):: error_message
   LOGICAL              :: fatal, connected

!------------------------------------------------------------------------------!

      CALL prelog 
      WRITE  (log_unit,'(A)')  &
     "-----------------------------------------------------------------------"
      CALL prelog; 
      WRITE  (log_unit,'(A)') &
      "Remove duplicate stations by location:"

   !  Count obs per type before merging

count_before:&
   DO current = 1 , num_obs

      first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF ( obs(first)%info%discard ) THEN
         CYCLE count_before
      END IF

   ENDDO count_before

   !  Break analysis time into ccyymmdd and hhmnss

   
   time24 = time_analysis//".0000"
   CALL split_date_char (time24, &
                         century_year, month, day, hour, minute, seconds, &
                         ten_thousandth)

   date = century_year * 10000 + month  * 100 + day
   time = hour         * 10000 + minute * 100 + seconds

   !  Count the total number of duplicate reports.

   total_dups  = 0
   total_merge = 0
   total_valid = 0

   !  Merge obs

obsloop:&
   DO current = 1 , num_obs - 1

      first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF ( obs(first)%info%discard ) THEN
         CYCLE obsloop
      END IF

      total_valid = total_valid + 1

      !  Get second obs to compare with first; compare first obs to second obs 
      !  until next obs does not match.

      compare: DO next = current + 1 , num_obs

         second = index(next)

         ! Sorted by location, so if locations NE, then no chance of any
         ! more matches with first.

! foo
!        IF (.NOT. (obs(first)%location .EQ. obs(second)%location )) THEN
         IF (.NOT. loc_eq (obs(first), obs(second))) THEN
            CYCLE obsloop
         END IF

         !  If this obs has been merged with another obs or discarded, skip it.

         IF (obs(second)%info%discard) THEN
            CYCLE compare
         END IF

         !  If time fields are not completely identical, go to next observation.
         !  Sort is by location ONLY, not by time; so next+1 may be identical
         !  even though next has different time.

! This statements modifies the obs date and time
!        IF (.NOT. time_eq (obs(first)%valid_time, obs(second)%valid_time, &
!            date, time)) THEN

         IF (.NOT. time_eq_old (obs(first)%valid_time, obs(second)%valid_time))&
         THEN

            IF (print_detail >= 1) THEN

                CALL prelog 
                WRITE  (log_unit,'(A,A5,1X,A,F6.2,A,F7.2,5A,I6)')             &
                       "Multiple reports id=",                                &
                        obs(first)%location%id,       "at",                   &
                        obs(first)%location%latitude, "N, ",                  &
                        obs(first)%location%longitude,"E on ",                &
                        obs(first)%valid_time%date_char (1:8),".",            &
                        obs(first)%valid_time%date_char (9:14)," line: ",     &
                        obs(first) %info%seq_num
                CALL prelog 
                WRITE  (log_unit,'(44X,5A,I6)')                               &
                        "& on ",                                              &
                        obs(second)%valid_time%date_char (1:8),".",           &
                        obs(second)%valid_time%date_char (9:14)," line: ",    &
                        obs(second)%info%seq_num

            ENDIF

            CYCLE compare

         END IF

         total_dups = total_dups + 1

         !  Observations are from same location and time, so merge them.

         IF (print_detail >= 1) THEN
             CALL prelog
!            WRITE  (log_unit,'(A,A5,1X,A,F6.2,A,F7.2,4A,2(A,I6))')        &
!                   "Merge ids ",                                          &
!                    obs(first)%location%id,       "at",                   &
!                    obs(first)%location%latitude, "N, ",                  &
!                    obs(first)%location%longitude,"E on ",                &
!                    obs(first)%valid_time%date_char (1:8),".",            &
!                    obs(first)%valid_time%date_char (9:14)," lines: ",    &
!                    obs(first) %info%seq_num, " & ",                      &
!                    obs(second)%info%seq_num
                WRITE  (log_unit,'(A,A5,1X,A,F6.2,A,F7.2,5A,I6)')             &
                       "Combine  reports id=",                                &
                        obs(first)%location%id,       "at",                   &
                        obs(first)%location%latitude, "N, ",                  &
                        obs(first)%location%longitude,"E on ",                &
                        obs(first)%valid_time%date_char (1:8),".",            &
                        obs(first)%valid_time%date_char (9:14)," line: ",     &
                        obs(first) %info%seq_num
                CALL prelog 
                WRITE  (log_unit,'(44X,5A,I6)')                               &
                        "& on ",                                              &
                        obs(second)%valid_time%date_char (1:8),".",           &
                        obs(second)%valid_time%date_char (9:14)," line: ",    &
                        obs(second)%info%seq_num

         ENDIF

         CALL merge_obs ( obs(first) , obs(second))

         !  Mark second of pair as discarded; data is put in 'first'.  
         !  Note that a duplicate has been found by incrementing the counter.

         obs(second)%info%discard  = .true.
         obs(first)%info%num_dups  = obs(first)%info%num_dups + 1

         total_merge = total_merge + 1

         !  Free up the space for the observation report that is discarded.
         !  Unfortunately, OR NOT!  

! foo
!        CALL dealloc_meas ( obs(second)%surface ) 
         NULLIFY ( obs(second)%surface ) 

!        obs (second)%info%discard = .TRUE.

      END DO compare

   END DO obsloop

   total_valid = total_valid + 1

   !  Count obs per type after merging

   total_left = 0

count_after:&
   DO current = 1 , num_obs

       first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF (obs(first)%info%discard ) THEN
          CYCLE count_after
      END IF

      total_left = total_left + 1

   ENDDO count_after

   CALL prelog
   WRITE  (log_unit,'(A,I4,1X,A)') &
  "Found  ",total_dups,"duplicate stations (same location, same time)."
   CALL prelog
   WRITE  (log_unit,'(A,I4,1X,A,I5,1X,A)' ) &
  "Merge  ",total_merge,"stations, leaving ",total_left, "valid station(s)."

 END SUBROUTINE check_duplicate_loc

!
! -----------------------------------------------------------------------

 SUBROUTINE check_duplicate_time (num_obs, obs, index, &
                                  total_merge, time_analysis)

!  Checks array of reports (obs), which has a sorted index to the reports,
!  to determine if any reports are for the same location but different time.
!  -If both observations are soundings, then the data closest to the analysis 
!   time is kept
!  -If time differences are equal (obs before and after the analysis time),
!   then the obs valid after the analysis time is kept. 
!  -If one is a sounding and the other a surface observation, then the sounding
!   is kept whatever the time differences are. 
!  
!   USE module_date
!   USE module_decoded

   IMPLICIT NONE

   TYPE (report),        INTENT (INOUT), DIMENSION (:) :: obs   
   INTEGER,              INTENT (IN),    DIMENSION (:) :: index 
   INTEGER,              INTENT (IN)                   :: num_obs 
   CHARACTER (LEN = 19), INTENT (INOUT)                :: time_analysis
   INTEGER,              INTENT (OUT)                  :: total_merge

   INTEGER                                             :: total_valid
   INTEGER                                             :: total_dups, &
                                                          total_left

   INTEGER :: current, next, first, second
   CHARACTER (LEN = 19) :: time_first, time_second
   INTEGER :: itfirst, itsecond
   LOGICAL :: llfirst, llsecond

   TYPE (report)               :: obs_tmp
   TYPE (measurement), POINTER :: current_tmp
   LOGICAL                     :: remove_duplicate = .TRUE.

   CHARACTER (LEN = 80)        :: filename
   CHARACTER (LEN = 32 ), PARAMETER :: proc_name = 'check_duplicate_time '
   LOGICAL                     :: connected
   INTEGER                     :: io_error
   CHARACTER (LEN = 160):: error_message

!------------------------------------------------------------------------------!

      CALL prelog
      WRITE (log_unit,'(A)')  &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE (log_unit,'(A)') "Remove duplicate stations by time:"

   !  Count the total number of duplicate reports.

   total_valid = 0
   total_dups  = 0

   obsloop: DO current = 1 , num_obs - 1

      first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF ( obs(first)%info%discard ) THEN
         CYCLE obsloop
      END IF

      total_valid = total_valid + 1

      !  Get second obs to compare with first; compare first obs to second obs 
      !  until next obs does not match.

      compare: DO next = current + 1 , num_obs

         second = index(next)

         ! Sorted by location, so if locations NE, then no chance of any
         ! more matches with first.

         IF ( .NOT. loc_eq ( obs(first) , obs(second) ) ) THEN
            CYCLE obsloop
         END IF

         !  If this obs has been merged with another obs or discarded, skip it.

         IF ( obs(second)%info%discard ) THEN
            CYCLE compare
         END IF

         !  If time fields are not completely identical, they are duplicated

time_difference: &
         IF (.NOT. time_eq_old (obs(first)%valid_time, obs(second)%valid_time))&
         THEN

         total_dups = total_dups + 1
         llfirst  = .FALSE.
         llsecond = .FALSE.

         IF (print_detail >= 1) THEN

             CALL prelog 
             WRITE (log_unit,'(A)') 'Found duplicated stations:'

             CALL prelog 
             WRITE (log_unit,'(A,A5,A,A23,2F9.3,A,L1)') &
            'Station 1 name and ID = ' ,                &
             TRIM (obs(first)%location%id ) , ' ' ,     &
             TRIM (obs(first)%location%name ) ,         &
                   obs(first)%location%latitude ,       &
                   obs(first)%location%longitude, ' ',  &
                   obs (first)%info%is_sound

             CALL prelog 
             WRITE (log_unit,'(A,A5,A,A23,2F9.3,A,L1)') &
            'Station 2 name and ID = ' ,                &
             TRIM (obs(second)%location%id ) , ' ' ,    &
             TRIM (obs(second)%location%name ) ,        &
                   obs(second)%location%latitude ,      &
                   obs(second)%location%longitude,' ',  &
                   obs(second)%info%is_sound

         ENDIF

         ! First we check the nature of the observation: sounding or surface

is_sound:IF (      obs (first)  % info % is_sound .AND.  &
              .NOT. obs (second) % info % is_sound) THEN

             llfirst  = .TRUE.
             llsecond = .FALSE.

         ELSE IF (.NOT. obs (first)  % info % is_sound .AND. & 
                        obs (second) % info % is_sound) THEN

             llfirst  = .FALSE.
             llsecond = .TRUE.

         ELSE is_sound

         ! Second we test the time difference between the analysis time and 
         ! the observations time 
         ! (negative for before analysis time, positive for after analysis time)

           WRITE (time_first, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
            obs (first) % valid_time % date_char ( 1: 4), &
            obs (first) % valid_time % date_char ( 5: 6), &
            obs (first) % valid_time % date_char ( 7: 8), &
            obs (first) % valid_time % date_char ( 9:10), &
            obs (first) % valid_time % date_char (11:12), &
            obs (first) % valid_time % date_char (13:14)

           WRITE (time_second, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
            obs (second) % valid_time % date_char ( 1: 4), &
            obs (second) % valid_time % date_char ( 5: 6), &
            obs (second) % valid_time % date_char ( 7: 8), &
            obs (second) % valid_time % date_char ( 9:10), &
            obs (second) % valid_time % date_char (11:12), &
            obs (second) % valid_time % date_char (13:14)

            CALL GETH_IDTS (time_first,  time_analysis, itfirst)
            CALL GETH_IDTS (time_second, time_analysis, itsecond)

            IF (print_detail >= 1) THEN

                CALL prelog
                WRITE  (log_unit,'(2A)') 'Analysis  time = ',time_analysis

                IF (itfirst .GE. 0) THEN
                    CALL prelog
                    WRITE  (log_unit,'(3A,I8,A)') 'Station 1 time = ',&
                            time_first,' = ta + ',itfirst/60,'mn'
                ELSE
                    CALL prelog
                    WRITE  (log_unit,'(3A,I8,A)') 'Station 1 time = ',&
                            time_first,' = ta - ',ABS (itfirst/60),'mn'
                ENDIF

                IF (itsecond .GE. 0) THEN
                    CALL prelog
                    WRITE  (log_unit,'(3A,I8,A)') 'Station 2 time = ',&
                            time_second, ' = ta + ',itsecond/60,'mn'
                ELSE
                    CALL prelog
                    WRITE  (log_unit,'(3A,I8,A)') 'Station 2 time = ',&
                            time_second,' = ta - ',ABS (itsecond/60),'mn'
                ENDIF

            ENDIF

            ! Time difference must be different

time_equal: IF (itfirst .EQ. itsecond) THEN

                IF (print_detail >= 1) THEN
                    CALL prelog
                    WRITE  (log_unit,'(2A)') "First_time  = ",time_first
                    CALL prelog
                    WRITE  (log_unit,'(2A)') "Second_time = ",time_second

                    error_message =  proc_name // 'Internal error.'
                    CALL error_handler (log_unit, 443002, error_message, &
                                        .TRUE., .TRUE.)
                ENDIF

            ENDIF time_equal

time_different: IF (itfirst*itsecond .GT. 0) THEN 

                !  if both observations are either before or after 
                !  the analysis time they can easily be compared

                IF (itfirst .LT. itsecond) THEN
                    llfirst  = .TRUE.
                    llsecond = .FALSE.
                ELSE IF (itfirst .GT. itsecond) THEN
                    llfirst  = .FALSE.
                    llsecond = .TRUE.
                ENDIF
                
            ELSE IF (itfirst*itsecond .LT. 0) THEN 

            ! if one observations is before and one after, keep the one before

                IF (itfirst .GT. 0) THEN
                    llfirst  = .FALSE.
                    llsecond = .TRUE.
                ELSE IF (itfirst .LT. 0) THEN
                    llfirst  = .TRUE.
                    llsecond = .FALSE.
                ENDIF

            ELSE IF (itfirst*itsecond .EQ. 0) THEN 

            ! if one observations is at analysis time, keep this one

                IF (itfirst .EQ. 0) THEN
                    llfirst  = .TRUE.
                    llsecond = .FALSE.
                ELSE IF (itsecond .EQ. 0) THEN
                    llfirst  = .FALSE.
                    llsecond = .TRUE.
                ENDIF

            ENDIF time_different

         END IF is_sound

         !  Remove duplicate sounding

         IF (remove_duplicate) THEN

         IF (llfirst) THEN

             IF (print_detail >= 1) THEN
                 CALL prelog 
                 WRITE  (log_unit,'(A)') 'Keep station 1 and reject station 2.'
             ENDIF

             obs (second)%info%discard  = .true.
             obs (first)%info%num_dups  = obs (first)%info%num_dups + 1

             NULLIFY (obs(second)%surface ) 

             total_merge = total_merge + 1

             CYCLE compare


         ELSE IF (llsecond) THEN

             IF (print_detail >= 1) THEN
                 CALL prelog 
                 WRITE (log_unit, FMT = '(A)') &
                'Keep station 2 and reject station 1.'
             ENDIF

             obs (first)%info%discard    = .true.
             obs (second)%info%num_dups  = obs (second)%info%num_dups + 1

             NULLIFY ( obs(first)%surface ) 

             total_merge = total_merge + 1

             CYCLE obsloop

         ENDIF

         ELSE

         !  Order duplicate soundings by time incresing

         IF (llfirst) THEN

         !  If first observation is before second, do nothing

         ELSE IF (llsecond) THEN

         !  If second observation is before first, swap

              obs_tmp      = obs (second)
              obs (second) = obs (first)
              obs (first)  = obs_tmp

         ENDIF

         ENDIF

      ENDIF time_difference

       !  Free up the space for the observation report that is discarded.
       !  Unfortunately, OR NOT!  
! foo
!        CALL dealloc_meas ( obs(second)%surface ) 
!        NULLIFY ( obs(second)%surface ) 


      END DO compare

   END DO obsloop

   total_valid = total_valid + 1

   !  Count left observations

   total_left = 0

count_after:&
   DO current = 1 , num_obs

       first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF (obs(first)%info%discard ) THEN
          CYCLE count_after
      END IF

      total_left = total_left + 1

   ENDDO count_after

   CALL prelog
   WRITE  (log_unit,'(A,I4,1X,A)') &
  "Found  ",total_dups,"duplicate stations (same location, different time)."
   CALL prelog
   WRITE  (log_unit,'(A,I4,1X,A,I5,1X,A)') &
  "Remove ",total_merge,"stations, leaving ",total_left,"valid station(s)."

 END SUBROUTINE check_duplicate_time
!------------------------------------------------------------------------------!

