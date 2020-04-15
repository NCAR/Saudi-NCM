SUBROUTINE prelog (iunit,skipped_lines)

      IMPLICIT NONE

      INTEGER, INTENT (in) :: iunit    ! Logical Unit to write in
      INTEGER, INTENT (in) :: skipped_lines ! Skip line before writing
      INTEGER              :: l,ierr
      INTEGER              :: iab
!     INTEGER, EXTERNAL    :: ABORT
      CHARACTER (LEN=24)   :: sdate

      CALL DATE_AND_TIME (sdate (1:8),sdate (9:18))

      DO l = 1, skipped_lines
         WRITE (UNIT = iunit, FMT = '(A)') ""
      ENDDO

      WRITE (UNIT = iunit, FMT = '(6A)',ADVANCE = 'no',IOSTAT=ierr) &
            sdate (9:10),":",sdate (11:12),":",sdate (13:14)," "

      IF (ierr /= 0) THEN
          WRITE (UNIT = iunit, FMT = '(/,A,I6,/)') &
         "Error in prelog, iostat = ",ierr
          WRITE (iunit,'(A)') "Abort"
          call ABORT ()
      ENDIF

END SUBROUTINE prelog

