      SUBROUTINE count_stations (number_obs, obs, message)
!------------------------------------------------------------------------------!
! Count valid stations per platform types. 
! 
! Francois VANDENBERGHE, December 2002
!------------------------------------------------------------------------------!
      IMPLICIT NONE
!------------------------------------------------------------------------------!
! ARGUMENTS

      INTEGER, INTENT (in) :: number_obs

      TYPE (report), DIMENSION (number_obs), INTENT (inout) :: obs

      CHARACTER (LEN=80), INTENT (in) :: message

!------------------------------------------------------------------------------!
! LOCAL VARIABLES

      INTEGER :: obs_num
      INTEGER :: fm, ier
!------------------------------------------------------------------------------!

! 1.  RESET STATION COUNTERS
! ===========================

      CALL fm_code (reset=.TRUE.)

! 2.  COUNT STATION PER TYPE AND VERTICAL LEVELS PER STATION
! ==========================================================

      DO obs_num = 1, number_obs

! 2.1 Skip invalid obs 
!     ----------------

         IF (obs (obs_num) % info % discard) CYCLE

! 2.2 Increment station counters
!     --------------------------

         READ  (obs (obs_num) % info % platform (4:6),'(I3)',IOSTAT=ier) fm

         IF (ier /= 0) THEN
             READ  (obs (obs_num) % info % platform (4:5),'(I2)',IOSTAT=ier) fm
         ENDIF

         IF (ier /= 0) fm = missing

         CALL fm_code (fm=fm)

      ENDDO

! 3.  PRINT OUT WITH MESSAGE
! ==========================

      RETURN

      CALL prelog
      WRITE  (log_unit,'(A)') &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE  (log_unit,'(A)') TRIM (message)

      CALL fm_code (print_out=.TRUE.)


 END SUBROUTINE count_stations
!------------------------------------------------------------------------------!
