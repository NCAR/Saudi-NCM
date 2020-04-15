!SUBROUTINE prelog (iunit,skipped_lines)
SUBROUTINE prelog (skipped_lines)

      IMPLICIT NONE

!     INTEGER, INTENT (in) :: iunit    ! Logical Unit to write in
      INTEGER, INTENT (in), OPTIONAL :: skipped_lines ! Skip line before writing
      INTEGER              :: l,ierr, iab
!     INTEGER, EXTERNAL    :: ABORT
      CHARACTER (LEN=24)   :: sdate

      IF (PRESENT (skipped_lines)) THEN
          DO l = 1, skipped_lines
            WRITE (UNIT = log_unit, FMT = '(A)') ""
          ENDDO
      ENDIF

      IF (log_time) THEN

           CALL date_and_time (sdate (1:8),sdate (9:18))

           WRITE (UNIT = log_unit, FMT = '(6A)',ADVANCE = 'no',IOSTAT=ierr) &
           sdate (9:10),":",sdate (11:12),":",sdate (13:14)," "

      ELSE

           WRITE (UNIT = log_unit, FMT = '(A)',ADVANCE = 'no',IOSTAT=ierr) " "

      ENDIF

      IF (ierr /= 0) THEN
          WRITE (UNIT = log_unit, FMT = '(/,A,I6,/)') &
         "Error in prelog, iostat = ",ierr
          call ABORT ()
      ENDIF

END SUBROUTINE prelog

