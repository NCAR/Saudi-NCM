      SUBROUTINE openin_unit (filename, iunit, proc_name, formatted)

!------------------------------------------------------------------------------!
      IMPLICIT NONE
!------------------------------------------------------------------------------!
      CHARACTER (len=80), INTENT (inout) :: filename
      INTEGER,            INTENT (in)    :: iunit
      CHARACTER (len=32), INTENT (in)    :: proc_name
      LOGICAL,            INTENT (in)    :: formatted

      CHARACTER (len=100) :: error_message
      LOGICAL :: file_exist, unit_exist
      INTEGER :: io_error
!------------------------------------------------------------------------------!

      CALL prelog ()
      WRITE  (log_unit,'(A)')  &
     "-----------------------------------------------------------------------"

! 4.1 Look for file with file name (if requested in arguments)
!     -------------------------------------------------------

      INQUIRE (FILE = filename, EXIST = file_exist)

      IF (file_exist) THEN

! 4.2 Close logical unit if already opened
!     ------------------------------------

          INQUIRE (UNIT = iunit, EXIST = unit_exist)

          IF (unit_exist) CLOSE (UNIT = iunit)

! 4.3 Open input file with file name
!     ------------------------------

          CALL prelog ()
          WRITE  (log_unit,'(2A)') "Open input file: ", TRIM  (filename)

          IF (formatted) THEN
              OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED', &
                    ACTION = 'READ' , STATUS= 'old', IOSTAT = io_error)
          ELSE
              OPEN (UNIT = iunit , FILE = filename , FORM = 'UNFORMATTED', &
                    ACTION = 'READ' , STATUS= 'old', IOSTAT = io_error)
          ENDIF

          IF (io_error .NE. 0) THEN
              error_message = "Unable to open file: "// TRIM (filename)
              CALL error_handler (log_unit, iunit, &
                   proc_name(1:32) // error_message, .TRUE., .TRUE.)
          ENDIF

      ELSE

! 4.4 Use file fort.iunit otherwise
!     -----------------------------

          IF (iunit > 0 .AND. iunit < 10) THEN
              WRITE (filename,'(A,I1.1)') "fort.",iunit
          ELSE IF (iunit > 0 .AND. iunit < 100) THEN
              WRITE (filename,'(A,I2.2)') "fort.",iunit
          ELSE IF (iunit > 0 .AND. iunit < 1000) THEN
              WRITE (filename,'(A,I3.3)') "fort.",iunit
          ELSE
              WRITE (error_message,'(A,I5)') "Unable to open unit: ", iunit
              CALL   error_handler (log_unit, 331001, &
                     proc_name(1:32) // error_message, .TRUE., .TRUE.)
          ENDIF

          CALL prelog ()
          WRITE  (log_unit,'(2A)')"No or bad input file specified, will use: ",&
          TRIM  (filename)

          IF (formatted) THEN
              OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED', &
                    ACTION = 'READ' , STATUS= 'old', IOSTAT = io_error)
          ELSE
              OPEN (UNIT = iunit , FILE = filename , FORM = 'UNFORMATTED', &
                    ACTION = 'READ' , STATUS= 'old', IOSTAT = io_error)
          ENDIF

          IF (io_error .NE. 0) THEN
              error_message = "Unable to open file: "// TRIM (filename)
              CALL error_handler (log_unit, iunit, &
                   proc_name(1:32) // error_message, .TRUE., .TRUE.)
          ENDIF

      ENDIF

      END SUBROUTINE openin_unit
!------------------------------------------------------------------------------!
      SUBROUTINE openou_unit (filename, iunit, proc_name, formatted)

!------------------------------------------------------------------------------!
      IMPLICIT NONE
!------------------------------------------------------------------------------!
      CHARACTER (len=80), INTENT (inout) :: filename
      INTEGER,            INTENT (in)    :: iunit
      CHARACTER (len=32), INTENT (in)    :: proc_name
      LOGICAL,            INTENT (in)    :: formatted

      CHARACTER (len=100) :: error_message
      LOGICAL :: file_exist, unit_exist
      INTEGER :: io_error
!------------------------------------------------------------------------------!

      CALL prelog ()
      WRITE  (log_unit,'(A)')  &
     "-----------------------------------------------------------------------"

! 4.1 Look for file with file name (if requested in arguments)
!     -------------------------------------------------------

      INQUIRE (FILE = filename, EXIST = file_exist)

      IF (file_exist) THEN

! 4.2 Close logical unit if already opened
!     ------------------------------------

          INQUIRE (UNIT = iunit, EXIST = unit_exist)

          IF (unit_exist) CLOSE (UNIT = iunit)

! 4.3 Open input file with file name
!     ------------------------------

          CALL prelog ()
          WRITE  (log_unit,'(2A)') "Open output file: ", TRIM  (filename)

          IF (formatted) THEN
              OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED', &
                    ACTION = 'WRITE', STATUS = 'replace', IOSTAT = io_error)
          ELSE
              OPEN (UNIT = iunit , FILE = filename , FORM = 'UNFORMATTED', &
                    ACTION = 'WRITE', STATUS = 'replace', IOSTAT = io_error)
          ENDIF

          IF (io_error .NE. 0) THEN
              error_message = "Unable to open file: "// TRIM (filename)
              CALL error_handler (log_unit, iunit, &
                   proc_name(1:32) // error_message, .TRUE., .TRUE.)
          ENDIF

      ELSE

! 4.4 Use file fort.iunit otherwise
!     -----------------------------

          IF (iunit > 0 .AND. iunit < 10) THEN
              WRITE (filename,'(A,I1.1)') "fort.",iunit
          ELSE IF (iunit > 0 .AND. iunit < 100) THEN
              WRITE (filename,'(A,I2.2)') "fort.",iunit
          ELSE IF (iunit > 0 .AND. iunit < 1000) THEN
              WRITE (filename,'(A,I3.3)') "fort.",iunit
          ELSE
              WRITE (error_message,'(A,I5)') "Unable to open unit: ", iunit
              CALL   error_handler (log_unit, iunit, &
                     proc_name(1:32) // error_message, .TRUE., .TRUE.)
          ENDIF

          CALL prelog
          WRITE  (log_unit,'(2A)')"No or bad input file specified, will use: ",&
          TRIM  (filename)

          IF (formatted) THEN
              OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED', &
                    ACTION = 'WRITE', STATUS = 'replace', IOSTAT = io_error)
          ELSE
              OPEN (UNIT = iunit , FILE = filename , FORM = 'UNFORMATTED', &
                    ACTION = 'WRITE', STATUS = 'replace', IOSTAT = io_error)
          ENDIF

          IF (io_error .NE. 0) THEN
              error_message = "Unable to open file: "// TRIM (filename)
              CALL error_handler (log_unit, iunit, &
                   proc_name(1:32) // error_message, .TRUE., .TRUE.)
          ENDIF

      ENDIF

      END SUBROUTINE openou_unit
!------------------------------------------------------------------------------!
