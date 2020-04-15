! -------------------------------------------------------------------------
!                            FUNCTIONS
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

LOGICAL FUNCTION eps_not (a , b , eps)

!  Compare two real numbers a and b, and return FALSE if they are within
!  parameter 'eps' of one another.  

   IMPLICIT NONE 

   REAL , INTENT (IN)                     :: a , b , eps

   IF (ABS (a - b) .LT. eps) THEN
      eps_not = .FALSE.
   ELSE
      eps_not = .TRUE.
   ENDIF

END FUNCTION eps_not
 
!
! ------------------------------------------------------------------------
!                            ROUTINES
! -------------------------------------------------------------------------

SUBROUTINE dealloc_meas (head)

!  This deallocates all nodes in a linked list of measurements.

   IMPLICIT NONE 

   TYPE (measurement) , POINTER           :: head     ! head of linked list

   TYPE (measurement) , POINTER           :: previous &
                                             , temp
   INTEGER                                  :: status

   !  Start at the head, kill everything that is pointed to.  After no longer 
   !  associated, deallocate the head.

   IF (ASSOCIATED (head)) THEN

      previous => head
      list_loop : DO WHILE (ASSOCIATED (previous%next))
         temp => previous
         previous => previous%next
         DEALLOCATE (temp , STAT = status) 
         IF (status .NE. 0) THEN
            PRINT *,'Error in DEALLOCATE, continuing by stopping DEALLOCATE on this list.'
            EXIT list_loop
         ENDIF
      END DO list_loop

   ENDIF

!  NULLIFY (head) 
  
END SUBROUTINE dealloc_meas

!
!------------------------------------------------------------------------------
SUBROUTINE link_levels (first, new)
!-------------------------------------------------------------------------------
! Link a new level to the previous one.
!-------------------------------------------------------------------------------
   IMPLICIT NONE

   TYPE (measurement), POINTER :: first, new
   TYPE (measurement), POINTER :: current, previous, oldptr

   INTEGER :: iab
!  INTEGER, EXTERNAL :: ABORT
!------------------------------------------------------------------------------!

   NULLIFY ( new%next )

   !  The first check is to see if we are at the head of the linked list.
   !  This drops us through to exit the routine.

   IF (.NOT. ASSOCIATED (first)) THEN   ! We are at the first level

      first => new

      ELSE !  We are after a last value,

      !  Initialize some dummy pointers to traverse to where we need to be.

      previous => first
      current  => first

      still_associated_level : DO WHILE ( ASSOCIATED ( current ) )

         previous => current
         current  => current%next

      END DO still_associated_level

      ! We ran out of data finding where to insert level: add it to the end

      IF ( .NOT. ASSOCIATED ( current ) ) THEN

         previous%next => new

      ELSE

         !  One of those "should never get here" logic errors, fatal.

            WRITE (log_unit,'(2A)') &
          ' ERROR  ASSOCIATED ( current ) = ',ASSOCIATED ( current )
            call ABORT ()

      END IF

    END IF

    RETURN

END SUBROUTINE link_levels
!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------

SUBROUTINE insert_at (surface , new , elevation)

!  This takes a new measurement (new) and inserts it in a linked list
!  of measurements (surface points to first in list) in decreasing order of
!  pressure value.  If two levels' pressures are 'eps_equal', the levels
!  are merged instead of being linked.

   IMPLICIT NONE

   TYPE (measurement) ,  POINTER         :: surface , new
   REAL , INTENT(IN)                     :: elevation

   TYPE (measurement) , POINTER          :: current , previous , oldptr
   REAL                                  :: new_pres , new_height
   CHARACTER (LEN = 32) , PARAMETER      :: proc_name = 'insert_at'

   INTEGER                 :: error_number
   CHARACTER ( LEN = 132 ) :: error_message
   LOGICAL                 :: fatal , listing

   !  Initialize the variable to test the pressure and the place where the
   !  to-be-inserted measurement points.

   new_pres = new%meas%pressure%data
   new_height = new%meas%pressure%data
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
         IF (current%meas%pressure%data .LT. new_pres) EXIT still_some_data
         previous => current
         current => current%next
      END DO still_some_data 

      !  There are several cases:
      !  1) the new value has the same pressure as the previous value, or
      !     both heights are equal to the station elevation: merge them
      !  2) ran out of data finding where to insert level: add it to the end
      !  3) the new value has the same pressure as the current pressure value, or
      !     both heights are equal to the station elevation: merge them
      !  4) new pressure is < the previous value: stick it at end of previous
      !  5) new pressure > than previous: put at head of list
      !  ***** THE ORDER OF THE TESTS IS IMPORTANT *****

      IF ((eps_equal (previous%meas%pressure%data , new_pres   , 1.)) .OR. &
         ((eps_equal (previous%meas%height%data   , new_height , 1.)) .AND. &
          (eps_equal (previous%meas%height%data   , elevation  , 1.)))) THEN

         DEALLOCATE (new)

      ELSE IF (.NOT. ASSOCIATED (current)) THEN

         previous%next => new

      ELSE IF ((eps_equal (current%meas%pressure%data, new_pres   , 1.)) .OR. &
              ((eps_equal (current%meas%height%data  , new_height , 1.)) .AND. &
               (eps_equal (current%meas%height%data ,  elevation  , 1.)))) THEN

         DEALLOCATE (new)

      ELSE IF (previous%meas%pressure%data .GT. new_pres) THEN

         oldptr => previous%next
         previous%next => new
         new%next => oldptr

      ELSE IF (previous%meas%pressure%data .LT. new_pres) THEN

         ! If we aren't at head of list, have some internal (fatal) error.

         IF (.NOT. ASSOCIATED (previous , surface)) THEN
            error_message = TRIM (proc_name) // "Logic error in IF"
            CALL error_handler (log_unit, 33341, error_message, .TRUE., .TRUE.)
         ELSE
            oldptr => surface
            surface => new
            new%next => oldptr
         ENDIF 

      ELSE

         !  One of those "should never get here" logic errors, fatal.

         error_message = TRIM (proc_name) // &
        "Logic error in IF test for where to put the new observation level."
         CALL error_handler (log_unit, 33342, error_message, .TRUE., .TRUE.)

      ENDIF

   ENDIF

END SUBROUTINE insert_at
!
! ----------------------------------------------------------------------------
SUBROUTINE surf_first ( surface , elevation, final_levels )


!  This routine takes the sounding and makes sure that if a surface
!  level exists, that it is the first level.

   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface  
   REAL , INTENT(IN)                       :: elevation
   INTEGER,  INTENT(out)                   :: final_levels

   TYPE ( measurement ) , POINTER          :: current

   !  Um, is there any data at all?

   IF ( ASSOCIATED ( surface ) ) THEN

      !  Alrighty, we have data, so loop through the sounding to see if their is
      !  a surface observation. We can't very well have the surface be the first
      !  level if we don't have one.  Also, start looking at location #2
      !  (surface%next) so that we don't "fix" the ones that aren't broken.

      current  => surface%next

      find_sfc : DO WHILE ( ASSOCIATED ( current ) )

         IF ( eps_equal ( current%meas%height%data , elevation , 1. ) ) THEN
            surface => current
            EXIT find_sfc
         END IF

         current => current%next

      END DO find_sfc

      !  Count the number of upper-air levels left for the sounding

      current => surface
      final_levels = 0

      count_levels : DO WHILE ( ASSOCIATED ( current ) )

         final_levels = final_levels + 1
         current => current%next

      END DO count_levels

   END IF

END SUBROUTINE surf_first
!
! ----------------------------------------------------------------------------
SUBROUTINE revert_qc ( surface , qc_in, qc_out, value_out)


!  This routine takes the sounding and makes sure that if a surface
!  level exists, that it is the first level.

   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface  
   INTEGER                                 :: qc_in, qc_out
   REAL                                    :: value_out

   TYPE ( measurement ) , POINTER          :: current

   !  Loop over upper_level

      current  => surface

      DO WHILE ( ASSOCIATED ( current ) )

         IF (current % meas % height % qc   == qc_in) THEN
             current % meas % height % qc    = qc_out
             current % meas % height % data  = value_out
         ENDIF

         IF (current % meas % pressure % qc   == qc_in) THEN
             current % meas % pressure % qc    = qc_out
             current % meas % pressure % data  = value_out
         ENDIF

         current  => current % next
         
      END DO

END SUBROUTINE revert_qc
!
! ----------------------------------------------------------------------------
FUNCTION compare_loc ( a , b, flag ) RESULT (compare)

!  This defines the comparison operator '.LT.' for use with the 'report'
!  data type.  NOTE that the other operators LE, GE, GT are NOT
!  defined at all for the 'report' data type.

   IMPLICIT NONE

   INTEGER         , INTENT ( IN )     :: flag
   TYPE ( report ) , INTENT ( IN )     :: a  ! the first data item compared
   TYPE ( report ) , INTENT ( IN )     :: b  ! the second data item compared
   LOGICAL                             :: compare

   integer           :: a1, b1
   integer           :: iosta, iostb

   compare = .FALSE.

   READ (a%info%platform (4:6),'(I3)',IOSTAT=iosta) a1

   IF (iosta /= 0) THEN
       READ (a%info%platform (4:5),'(I2)',IOSTAT=iosta) a1
   ENDIF
       
   READ (b%info%platform (4:6),'(I3)',IOSTAT=iostb) b1

   IF (iostb /= 0) THEN
       READ (b%info%platform (4:5),'(I2)',IOSTAT=iostb) b1
   ENDIF

   IF ( a1 < b1 .OR. iosta /= 0 .OR. iostb /= 0) THEN
      compare = .TRUE.
   ELSE IF ( a%info%platform (4:6) .EQ. b%info%platform (4:6) ) THEN
     IF ( a%location%longitude .LT. b%location%longitude ) THEN
        compare = .TRUE.
     ELSE IF ( a%location%longitude .eq. b%location%longitude ) THEN
       IF ( a%location%latitude .LT. b%location%latitude ) THEN
          compare = .TRUE.
       ELSE IF ( a%location%latitude .EQ. b%location%latitude ) THEN
          IF ( LLT ( a%location%id , b%location%id ) ) THEN
            compare = .TRUE.
          ELSE IF ( a%location%id .EQ. b%location%id ) THEN
            IF ( LLT ( a%location%name , b%location%name ) ) THEN
               compare = .TRUE.
            END IF
          END IF
       END IF
     END IF
   ENDIF

   IF (flag == 1) then

   IF ( a%info%platform (4:6) .EQ. b%info%platform (4:6) .and. &
        a%location%longitude  .EQ. b%location%longitude  .and. &
        a%location%latitude   .EQ. b%location%latitude   .and. &
        a%location%id         .EQ. b%location%id         .and. &
        a%location%name       .EQ. b%location%name        ) THEN
        compare = .TRUE.
   ENDIF

   ENDIF

END FUNCTION compare_loc

!^L
! ---------------------------------------------------------------------------

!LOGICAL FUNCTION compare ( a , b, flag )
FUNCTION compare_tim ( a , b, flag ) RESULT (compare)

!  This defines the comparison operator '.LT.' for use with the 'report'
!  data type.  NOTE that the other operators LE, GE, GT are NOT
!  defined at all for the 'report' data type.

!   USE module_date

   IMPLICIT NONE

   INTEGER         , INTENT ( IN )     :: flag
   TYPE ( report ) , INTENT ( IN )     :: a  ! the first data item compared
   TYPE ( report ) , INTENT ( IN )     :: b  ! the second data item compared
   LOGICAL                             :: compare
   LOGICAL                             :: pass

   CHARACTER (LEN = 19)                :: time_a, time_b
   CHARACTER (LEN = 80)                :: stmp
   INTEGER                             :: its, fma, fmb
   INTEGER                             :: iosta, iostb

   !  Useless Statement needed on LINUX for unknown reasons

   WRITE (stmp,*) TRIM (a % info % platform)

   !  Get time of a in MM5 string date format CCYY-MM-DD_HH:MN:SS

   WRITE (time_a, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
          a % valid_time % date_char ( 1: 4), &
          a % valid_time % date_char ( 5: 6), &
          a % valid_time % date_char ( 7: 8), &
          a % valid_time % date_char ( 9:10), &
          a % valid_time % date_char (11:12), &
          a % valid_time % date_char (13:14)

   !  Get time of second in MM5 string date format CCYY-MM-DD_HH:MN:SS

   WRITE (time_b, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
          b % valid_time % date_char ( 1: 4), &
          b % valid_time % date_char ( 5: 6), &
          b % valid_time % date_char ( 7: 8), &
          b % valid_time % date_char ( 9:10), &
          b % valid_time % date_char (11:12), &
          b % valid_time % date_char (13:14)

   !  Get the difference in s between first and second

   CALL GETH_IDTS (time_a,  time_b, its)

   !  Negative difference indicates that a is prior to b

   compare = .FALSE.

   !  Negative difference indicates that a is prior to b

   IF (its .LT.  0) THEN

      compare = .TRUE.

   ELSE IF (its .EQ.  0) THEN

   !  Same time are sorted upon obs type

        READ (a%info%platform (4:6),'(I3)',IOSTAT=iosta) fma

        IF (iosta /= 0) THEN
            READ (a%info%platform (4:5),'(I2)',IOSTAT=iosta) fma
        ENDIF

        READ (b%info%platform (4:6),'(I3)',IOSTAT=iostb) fmb

        IF (iostb /= 0) THEN
            READ (b%info%platform (4:5),'(I2)',IOSTAT=iostb) fmb
        ENDIF
       
        IF (fma .LT. fmb .OR. iosta /= 0 .OR. iostb /= 0) THEN

             compare = .TRUE.

        ELSE IF (fma .EQ. fmb) THEN

             !  Same time and same type are sorted upon location

             IF (compare_loc (a, b, flag)) THEN

                 compare = .TRUE.

             ENDIF

        ENDIF

   END IF

END FUNCTION compare_tim

!^L
! -------------------------------------------------------------------------

LOGICAL FUNCTION ground_eq ( a , b )

! This defines operator .EQ. for 'terrestrial' data type

   IMPLICIT NONE

   TYPE ( terrestrial ) , INTENT ( IN )    :: a , b

   IF ( eps_equal ( a%slp%data         , b%slp%data         , .01 ) .AND. &
        eps_equal ( a%ref_pres%data    , b%ref_pres%data    , .01 ) .AND. &
        eps_equal ( a%ground_t%data    , b%ground_t%data    , .01 ) .AND. &
        eps_equal ( a%sst%data         , b%sst%data         , .01 ) .AND. &
        eps_equal ( a%psfc%data        , b%psfc%data        , .01 ) .AND. &
        eps_equal ( a%precip%data      , b%precip%data      , .01 ) .AND. &
        eps_equal ( a%t_max%data       , b%t_max%data       , .01 ) .AND. &
        eps_equal ( a%t_min%data       , b%t_min%data       , .01 ) .AND. &
        eps_equal ( a%t_min_night%data , b%t_min_night%data , .01 ) .AND. &
        eps_equal ( a%p_tend03%data    , b%p_tend03%data    , .01 ) .AND. &
        eps_equal ( a%p_tend24%data    , b%p_tend24%data    , .01 ) .AND. &
        eps_equal ( a%cloud_cvr%data   , b%cloud_cvr%data   , .01 ) .AND. &
        eps_equal ( a%ceiling%data     , b%ceiling%data     , .01 ) .AND. &
        a%slp%qc         .EQ. b%slp%qc         .AND. &
        a%ref_pres%qc    .EQ. b%ref_pres%qc    .AND. &
        a%ground_t%qc    .EQ. b%ground_t%qc    .AND. &
        a%sst%qc         .EQ. b%sst%qc         .AND. &
        a%psfc%qc        .EQ. b%psfc%qc        .AND. &
        a%precip%qc      .EQ. b%precip%qc      .AND. &
        a%t_max%qc       .EQ. b%t_max%qc       .AND. &
        a%t_min%qc       .EQ. b%t_min%qc       .AND. &
        a%t_min_night%qc .EQ. b%t_min_night%qc .AND. &
        a%p_tend03%qc    .EQ. b%p_tend03%qc    .AND. &
        a%p_tend24%qc    .EQ. b%p_tend24%qc    .AND. &
        a%cloud_cvr%qc   .EQ. b%cloud_cvr%qc   .AND. &
        a%ceiling%qc     .EQ. b%ceiling%qc ) THEN
        ground_eq = .TRUE.
   ELSE
        ground_eq = .FALSE.
   END IF
 
END FUNCTION ground_eq
! -------------------------------------------------------------------------

LOGICAL FUNCTION loc_eq ( a , b )

! This defines operator .EQ. for 'location' data type

   IMPLICIT NONE

   TYPE ( report ) , INTENT ( IN )     :: a  ! the first data item compared
   TYPE ( report ) , INTENT ( IN )     :: b  ! the second data item compared
  
   IF ( eps_equal(a%location%latitude ,b%location%latitude , .001) .AND. &
        eps_equal(a%location%longitude,b%location%longitude, .001) .AND. &
        a%location%id   .EQ. b%location%id   .AND. &
        a%location%name .EQ. b%location%name .AND. &
        a%info%platform (4:6) .EQ. b%info%platform (4:6) ) THEN
      loc_eq = .TRUE.
   ELSE
      loc_eq = .FALSE.
   END IF
 
END FUNCTION loc_eq

!^L
! ------------------------------------------------------------------------ 

LOGICAL FUNCTION field_eq ( a , b )

! This defines operator .EQ. for 'field' data type

   IMPLICIT NONE

   TYPE ( field ) , INTENT ( IN )                :: a , b

   IF ( a%data .EQ. b%data .AND. a%qc .EQ. b%qc ) THEN
      field_eq = .TRUE.
   ELSE
      field_eq = .FALSE.
   END IF

END FUNCTION field_eq

!^L
! -------------------------------------------------------------------------

LOGICAL FUNCTION time_eq_old ( a , b )

! This defines operator .EQ. for 'time_info' data type

   IMPLICIT NONE

   TYPE ( time_info ) , INTENT ( IN )           :: a , b
  
   IF ( ( a%sut       .EQ. b%sut       ) .AND. &
        ( a%julian    .EQ. b%julian    ) .AND. &
        ( a%date_char .EQ. b%date_char ) ) THEN
      time_eq_old = .TRUE.
   ELSE
      time_eq_old = .FALSE.
   END IF
 
END FUNCTION time_eq_old

! -------------------------------------------------------------------------
 FUNCTION info_levels (surface) RESULT (levels)


!  This routine takes the sounding and makes sure that if a surface
!  level exists, that it is the first level.

   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface
   INTEGER                                 :: levels

   TYPE ( measurement ) , POINTER          :: current

   !  Um, is there any data at all?

   levels = 0

   IF ( ASSOCIATED ( surface ) ) THEN

      levels = levels + 1

      current  => surface%next

      DO WHILE ( ASSOCIATED ( current ) )

         levels = levels + 1
         current => current%next

      END DO

   END IF

 END FUNCTION info_levels
! -------------------------------------------------------------------------
