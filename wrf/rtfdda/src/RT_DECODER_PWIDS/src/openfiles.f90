      SUBROUTINE OPENIN (file_name, iunitin, formatted)
!------------------------------------------------------------------------------!
! Open a file and check status
!
! Copyright UCAR (c) 1992 - 2004.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Program (RAP),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
! Francois Vandenberghe, vandenb@ucar.edu, April 2004.
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      CHARACTER (LEN =200), INTENT (in) :: file_name
      INTEGER,              INTENT (in) :: iunitin
      LOGICAL,              INTENT (in) :: formatted

      CHARACTER (LEN = 80)  :: proc_name = "openin.F90"
      INTEGER               :: iost, iab
      INTEGER, EXTERNAL     :: ABORT
      LOGICAL               :: connected
!------------------------------------------------------------------------------!

      WRITE (UNIT = *, FMT = '(A,A)') "About to open input  file: ",&
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
          WRITE (UNIT = *, FMT = '(2A,/)') &
        " Problem opening input file ", TRIM (file_name)
          iab = ABORT ()
      ENDIF

      END SUBROUTINE OPENIN

      SUBROUTINE OPENOUT (file_name, iunitou, formatted)
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      CHARACTER (LEN =200), INTENT (in) :: file_name
      INTEGER ,             INTENT (in) :: iunitou
      LOGICAL,              INTENT (in) :: formatted

      CHARACTER (LEN = 80)  :: proc_name = "openout.F90"
      INTEGER               :: iost, iab
      INTEGER, EXTERNAL     :: ABORT
      LOGICAL               :: connected
!------------------------------------------------------------------------------!


      WRITE (UNIT = *, FMT = '(A,A)') "About to open output file: ",&
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
          WRITE (UNIT = *, FMT = '(2A,/)') &
        " Problem opening output file ", TRIM (file_name)
          iab = ABORT ()
      ENDIF

      END SUBROUTINE OPENOUT
!------------------------------------------------------------------------------!
