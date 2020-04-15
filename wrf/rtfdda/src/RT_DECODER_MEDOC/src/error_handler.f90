SUBROUTINE error_handler (calling_routine,message1,message2,fatal)

!  This subroutine handles all error exits and warnings from
!  the program via the calling arguments to this routine.
!  This routine will terminate the program with a fatal error
!  condition if the fatal variable is .TRUE. on input.  For an 
!  uncorrectable error, the program should exit the program from 
!  this routine, otherwise it returns to the calling routine 
!  with the info/warning/error noted in the standard print output.
!
!                               Francois VANDENBERGHE
!                               September 2001

   IMPLICIT NONE

   CHARACTER (LEN=*) , INTENT ( IN )          :: calling_routine
   CHARACTER (LEN=*) , INTENT ( IN )          :: message1
   CHARACTER (LEN=*) , INTENT ( IN )          :: message2
   LOGICAL , INTENT ( IN )                    :: fatal 

   WRITE (UNIT = 0, FMT = '(/,A,1X,A)') TRIM (message1),TRIM (message2)

   IF (fatal) THEN
       WRITE (UNIT = 0, FMT = '(/,A,A,/)') "STOP in ",TRIM (calling_routine)
       CALL ABORT
   ENDIF

END SUBROUTINE error_handler
