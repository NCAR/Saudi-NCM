  SUBROUTINE OPENIN (file_name, iunitin, formatted)
!------------------------------------------------------------------------------!
! Open a file and check status
!
! Francois VANDENBERGHE, September 2001
! vandenb@ucar.edu
! Copyright UCAR [RAP] 1996 - 2004. All Rights Reserved.
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      CHARACTER (LEN =  *), INTENT (in) :: file_name
      INTEGER,              INTENT (in) :: iunitin
      LOGICAL,              INTENT (in) :: formatted

      CHARACTER (LEN = 80)  :: proc_name = "openin.f90"
      INTEGER               :: iost
      LOGICAL               :: connected
!------------------------------------------------------------------------------!

      WRITE (UNIT = 0, FMT = '(A,A)') "About to open input file:  ",&
      TRIM  (file_name)

      INQUIRE (UNIT = iunitin, OPENED = connected )

      IF (connected) CLOSE (iunitin)

      IF (formatted) THEN

         OPEN (UNIT   =  iunitin,    FILE   = file_name,&
               FORM   = 'formatted', ACCESS = 'sequentiaL', &
               ACTION = 'read',      STATUS = 'old', IOSTAT =  iost)

      ELSE

          OPEN (UNIT   =  iunitin,    FILE   = file_name,&
                FORM   = 'unformatted', ACCESS = 'sequentiaL', &
                ACTION = 'read',      STATUS = 'old', IOSTAT =  iost)


      ENDIF
     
      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
             'Problem opening input file ', file_name, .TRUE.)
      ENDIF

      END SUBROUTINE OPENIN
!------------------------------------------------------------------------------!

      SUBROUTINE OPENOUT (file_name, iunitou, formatted)
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      CHARACTER (LEN = 80), INTENT (in) :: file_name
      INTEGER ,             INTENT (in) :: iunitou
      LOGICAL,              INTENT (in) :: formatted

      CHARACTER (LEN = 80)  :: proc_name = "openout.f90"
      INTEGER               :: iost
      LOGICAL               :: connected
!------------------------------------------------------------------------------!


      WRITE (UNIT = 0, FMT = '(A,A)') "About to open output file: ",&
      TRIM  (file_name)

      INQUIRE (UNIT = iunitou, OPENED = connected )

      IF (connected) CLOSE (iunitou)

      IF (formatted) THEN

      OPEN (UNIT   = iunitou,     FILE   = file_name,     &
            FORM   = 'formatted', ACCESS = 'sequential', &
            ACTION = 'write',     STATUS = 'replace',    IOSTAT = iost)


      ELSE

      OPEN (UNIT   = iunitou,     FILE   = file_name,     &
            FORM   = 'unformatted', ACCESS = 'sequential', &
            ACTION = 'write',     STATUS = 'replace',    IOSTAT = iost)

      ENDIF


      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
             ' Problem opening output file ', file_name, .TRUE.)
      ENDIF

      END SUBROUTINE OPENOUT
!------------------------------------------------------------------------------!

      SUBROUTINE OPENAPP (file_name, iunitap, formatted)
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      CHARACTER (LEN = 80), INTENT (in) :: file_name
      INTEGER ,             INTENT (in) :: iunitap
      LOGICAL,              INTENT (in) :: formatted

      CHARACTER (LEN = 80)  :: proc_name = "openapp.f90"
      INTEGER               :: iost
      LOGICAL               :: connected
!------------------------------------------------------------------------------!


      WRITE (UNIT = 0, FMT = '(A,A)') "About to open output file: ",&
      TRIM  (file_name)

      INQUIRE (UNIT = iunitap, OPENED = connected )

      IF (connected) CLOSE (iunitap)

      IF (formatted) THEN

      OPEN (UNIT   = iunitap,     FILE   = file_name,     POSITION = 'append',&
            FORM   = 'formatted', ACCESS = 'sequential',  &
            ACTION = 'write',     STATUS = 'unknown',    IOSTAT = iost)


      ELSE

      OPEN (UNIT   = iunitap,     FILE   = file_name,    POSITION = 'append', &
            FORM   = 'unformatted', ACCESS = 'sequential', &
            ACTION = 'write',     STATUS = 'unknown',    IOSTAT = iost)

      ENDIF


      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
             ' Problem opening output file ', file_name, .TRUE.)
      ENDIF

      END SUBROUTINE OPENAPP

!------------------------------------------------------------------------------!
      SUBROUTINE OPENFILES (time_analysis, &
                            iunit_wvr_t, iunit_wvr_q, iunit_wvr_clw, &
                            iunit_mdl_t, iunit_mdl_q, iunit_mdl_clw)
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      CHARACTER (LEN = 80),    INTENT (in) :: time_analysis
      INTEGER , INTENT (in) :: iunit_wvr_t, iunit_wvr_q, iunit_wvr_clw, &
                               iunit_mdl_t, iunit_mdl_q, iunit_mdl_clw
      CHARACTER (LEN = 80)  :: file_name
      CHARACTER (LEN = 80)  :: proc_name = "openfiles.f90"
      INTEGER :: iost
      LOGICAL :: connected
!------------------------------------------------------------------------------!

! 1.  DATA OUTPUT FILES
! =====================

! 1.1 Open output file (WVR temperature profiles)
!      -----------------------------------------

      WRITE (file_name,'(A,A4,A2,A2,A2)')  'tem_wvr.', &
             time_analysis ( 1: 4), time_analysis ( 6: 7),&
             time_analysis ( 9:10), time_analysis (12:14)

      WRITE (UNIT = 0, FMT = '(A,A)') "About to open output file: ", &
      TRIM  (file_name)

      INQUIRE (UNIT = iunit_wvr_t, OPENED = connected )

      IF (connected) CLOSE (iunit_wvr_t)

      OPEN (UNIT   =  iunit_wvr_t, FILE   =  file_name,    &
            FORM   = 'formatted',  ACCESS = 'sequential', &
            ACTION = 'write',      STATUS = 'replace',    IOSTAT = iost)

      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
             ' Problem opening output file ', TRIM (file_name), .TRUE.)
      ENDIF

! 1.2 Open output file (WVR water vapor profiles)
!      ------------------------------------------

      WRITE (file_name,'(A,A4,A2,A2,A2)')  'qvp_wvr.', &
             time_analysis ( 1: 4), time_analysis ( 6: 7),&
             time_analysis ( 9:10), time_analysis (12:14)

      WRITE (UNIT = 0, FMT = '(A,A)') "About to open output file: ", &
      TRIM  (file_name)

      INQUIRE (UNIT = iunit_wvr_q, OPENED = connected )

      IF (connected) CLOSE (iunit_wvr_q)

      OPEN (UNIT    = iunit_wvr_q, FILE   = file_name,    &
            FORM   = 'formattEd', ACCESS = 'sequential', &
            ACTION = 'write',     STATUS = 'replace',      IOSTAT = iost)

      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
             ' Problem opening output file ', TRIM (file_name), .TRUE.)
      ENDIF

! 1.3 Open output file (liquid water temperature profiles)
!      ---------------------------------------------------

      WRITE (file_name,'(A,A4,A2,A2,A2)')  'clw_wvr.', &
             time_analysis ( 1: 4), time_analysis ( 6: 7),&
             time_analysis ( 9:10), time_analysis (12:14)

      WRITE (UNIT = 0, FMT = '(A,A)') "About to open output file: ", &
      TRIM  (file_name)

      INQUIRE (UNIT = iunit_wvr_clw, OPENED = connected )

      IF (connected) CLOSE (iunit_wvr_clw)

      OPEN (UNIT   =  iunit_wvr_clw, FILE   = file_name,     &
            FORM   = 'formatted',    ACCESS = 'sequential', &
            ACTION = 'write',        STATUS = 'replace',    IOSTAT = iost)

      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
        ' Problem opening output file ', file_name, .TRUE.)
      ENDIF


! 2.  MODEL DATA
! ==============

! 2.1 Open output file (model temperature profiles)
!      -----------------------------------------

      WRITE (file_name,'(A,A4,A2,A2,A2)')  'tem_mdl.', &
             time_analysis ( 1: 4), time_analysis ( 6: 7),&
             time_analysis ( 9:10), time_analysis (12:14)

      WRITE (UNIT = 0, FMT = '(A,A)') "About to open output file: ", &
      TRIM  (file_name)

      INQUIRE (UNIT = iunit_mdl_t, OPENED = connected )

      IF (connected) CLOSE (iunit_mdl_t)

      OPEN (UNIT   =  iunit_mdl_t, FILE   = file_name,     &
            FORM   = 'formatted',  ACCESS = 'sequential', &
            ACTION = 'write',      STATUS = 'replace',    IOSTAT = iost)

      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
             ' Problem opening output file ', file_name, .TRUE.)
      ENDIF

! 2.2 Open output file (WVR water vapor profiles)
!      ------------------------------------------

      WRITE (file_name,'(A,A4,A2,A2,A2)')  'qvp_mdl.', &
             time_analysis ( 1: 4), time_analysis ( 6: 7),&
             time_analysis ( 9:10), time_analysis (12:14)

      WRITE (UNIT = 0, FMT = '(A,A)') "About to open output file: ", &
      TRIM  (file_name)

      INQUIRE (UNIT = iunit_mdl_q, OPENED = connected )

      IF (connected) CLOSE (iunit_mdl_q)

      OPEN (UNIT   =  iunit_mdl_q, FILE   = file_name,     &
            FORM   = 'formatted',  ACCESS = 'sequential', &
            ACTION = 'write',      STATUS = 'replace',    IOSTAT = iost)

      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
             ' Problem opening output file ', file_name, .TRUE.)
      ENDIF

! 2.3 Open output file (liquid water temperature profiles)
!      ---------------------------------------------------

      WRITE (file_name,'(A,A4,A2,A2,A2)')  'clw_mdl.', &
             time_analysis ( 1: 4), time_analysis ( 6: 7),&
             time_analysis ( 9:10), time_analysis (12:14)

      WRITE (UNIT = 0, FMT = '(A,A)') "About to open output file: ", &
      TRIM  (file_name)

      INQUIRE (UNIT = iunit_mdl_clw, OPENED = connected )

      IF (connected) CLOSE (iunit_mdl_clw)

      OPEN (UNIT   =  iunit_mdl_clw, FILE   = file_name,     &
            FORM   = 'FORMATTED',    ACCESS = 'SEQUENTIAL', &
            ACTION = 'WRITE',        STATUS = 'REPLACE',    IOSTAT = iost)

      IF (iost .ne. 0) THEN
          CALL error_handler (proc_name, &
             ' Problem opening output file ', file_name, .TRUE.)
      ENDIF

      END SUBROUTINE OPENFILES
