!------------------------------------------------------------------------------!
  SUBROUTINE error_handler (lunit, error_number, error_message, fatal, listing)

!  This subroutine handles all error exits and warnings from
!  the program.  The error number and message are passed
!  via the calling arguments to this routine, directly
!  from the error producing portion of code.  The calling
!  routine name is contained in the first 31 characters of
!  the error message.  The actual error message starts at
!  character 32 in the error_message variable.

!  This routine will terminate the program with a fatal error
!  condition if the fatal variable is .TRUE. on input.  For an 
!  uncorrectable error, the program should exit the program from 
!  this routine, otherwise it returns to the calling routine 
!  with the info/warning/error noted in the standard print output.

   IMPLICIT NONE

   INTEGER , INTENT ( IN )                    :: lunit 
   INTEGER , INTENT ( IN )                    :: error_number 
   CHARACTER (LEN=132) , INTENT ( IN )        :: error_message
   LOGICAL , INTENT ( IN )                    :: fatal 
   LOGICAL , INTENT ( IN )                    :: listing
!  INTEGER , EXTERNAL                         :: ABORT
   INTEGER                                    :: iab

   LOGICAL                                    :: any_errors    = .false.
   TYPE ( error_info ) , POINTER              :: current, head
   SAVE current , head , any_errors
!------------------------------------------------------------------------------!

      !   Modifications, call only for fatal error

      CALL prelog
      WRITE (UNIT=lunit, FMT='(A)' )  &
     "-----------------------------------------------------------------------"
 
      CALL prelog
      WRITE (UNIT=lunit, FMT='(A)' )  'FATAL ERROR, EXIT'
      CALL prelog
      WRITE (UNIT=lunit, FMT='("Error identifier: ",I8,",")', &
      ADVANCE = 'YES' ) error_number
      CALL prelog
      WRITE (UNIT=lunit, FMT='("Calling Routine:  ",A)',&
      ADVANCE = 'YES' ) TRIM (error_message(1:32))
      CALL prelog
      WRITE (UNIT=lunit, FMT='("Error message:    ",A)',&
      ADVANCE = 'YES' ) TRIM(error_message(33:)) 

      CALL prelog
      WRITE (UNIT=lunit, FMT='(A)' )  &
     "-----------------------------------------------------------------------"
      call ABORT ()

      RETURN

   !  They may just want a listing of the errors thus far.  If this
   !  is a fatal error, we are going to get a listing anyways, so 
   !  no need to do it twice.


   listing_only_1 : IF (  listing .AND. ( .NOT. fatal ) ) THEN

      !  We should have been in here at least once for this to work.

      IF ( .NOT. any_errors ) THEN
         CALL prelog
         WRITE (UNIT=lunit, FMT='("NO INFORMATION/WARNINGS/ERRORS RECORDED IN PROGRAM")' )
      ELSE
         CALL prelog
         WRITE (UNIT=lunit, FMT='("Error Listing")' )
         CALL print_all_error (lunit, head ) 
      ENDIF

   ENDIF listing_only_1

   !  Write error information to the standard output.  If this is a
   !  fatal error, note that before exit.

   IF ( fatal ) THEN
      CALL prelog
      WRITE (UNIT=lunit, FMT='(A)' )  &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE (UNIT=lunit, FMT='(A)' )  'FATAL ERROR, EXIT'
   ELSE IF( error_number .GT. 0 ) THEN
      CALL prelog
      WRITE (UNIT=lunit, FMT='(A)' )  &
     "-----------------------------------------------------------------------"
      CALL prelog
      WRITE (UNIT=lunit, FMT='(A)' )  'ERROR DETECTION'
   ENDIF
      
   !  Check to see that we should be processing some error, else we can
   !  assume that this was just a sanity check for an error listing.
   !  If the error number <= 0, this is not an error, so we can skip
   !  the rest of the routine.
   
   listing_only_2 : IF ( error_number .GT. 0 ) THEN
      CALL prelog
      WRITE (UNIT=lunit, FMT='("Error identifier ",i8,",")', &
      ADVANCE = 'YES' ) error_number
      CALL prelog
      WRITE (UNIT=lunit, FMT='("Calling Routine: ",A)',&
      ADVANCE = 'YES' ) TRIM (error_message(1:31))
      CALL prelog
      WRITE (UNIT=lunit, FMT='("Error message: ",A)',&
      ADVANCE = 'YES' ) TRIM(error_message(32:)) 

      !  Initialize the linked list during the first time inside.
   
      IF ( .NOT. any_errors ) THEN
         CALL initialize_error ( head , current ) 
      ENDIF
   
      !  Store this error information every time we are in this routine.
   
      CALL store_error ( current , error_number , error_message ) 
   
      IF ( fatal ) THEN
         
         !  If this is a fatal error, print out all messages, then exit.
   
!        CALL prelog
!        WRITE (UNIT=lunit, FMT='(a)' ) &
!      "-----------------------------------------------------------------------"
         CALL prelog
         WRITE (UNIT=lunit , &
         FMT = '("WE STOP, INFO/WARNING/ERROR history follows:")' )
         CALL print_all_error (lunit, head ) 
         call ABORT ()
!        STOP 'ERROR_EXIT in error_handler'

      ELSE
   
         !  Non-fatal error, so we just return to the calling routine.  We
         !  must keep track that we have scored a hit on an error for the
         !  error history.
   
         CALL prelog
         WRITE (UNIT=lunit, FMT='(a,a,a)' ) &
         'Returning from error_handler to ', &
         TRIM ( error_message(1:LEN_TRIM(error_message(1:31)))),'.'
         CALL prelog
         WRITE (UNIT=lunit, FMT='(a)' ) &
       "-----------------------------------------------------------------------"
         any_errors    = .true.
   
      ENDIF
   ENDIF listing_only_2
 
 END SUBROUTINE error_handler
!------------------------------------------------------------------------------!
!  This routines are used only by the error_handler routine.  These utility
!  routines and the error data structure allow the program to record 
!  errors, retrieve the error history, and exit the program when a 
!  fatal error is detected.
!------------------------------------------------------------------------------!
   
   SUBROUTINE initialize_error ( head , current ) 
   
   !  Initialize the linked list of errors reported from the calling
   !  routines.  Send back out the two required pointers to remember
   !  where the beginning is and the current pointer location.
   
      IMPLICIT NONE
      
      TYPE ( error_info ) , POINTER                  :: head , &
                                                        current
      TYPE ( error_info ) , POINTER                  :: error_report
   
      ALLOCATE ( error_report ) 
      error_report%num = -1
      error_report%msg = '                                Head of List'
      NULLIFY ( error_report%next )
      head         => error_report
      current      => error_report
   
   END SUBROUTINE initialize_error
   
!------------------------------------------------------------------------------!
   
   SUBROUTINE print_all_error ( lunit, head ) 
   
   !  Print all of the stored error messages.  Head is where the
   !  linked list begins, current is what is used to traverse.
   
      IMPLICIT NONE
   
      TYPE ( error_info ) , POINTER                 :: head 
      TYPE ( error_info ) , POINTER                 :: temp
   
      INTEGER                                       :: lunit
      INTEGER                                       :: loop
      
      loop = 0
      temp => head
      print_loop : DO 
         IF ( .NOT. ASSOCIATED ( temp ) ) EXIT print_loop

         IF (loop > 0) THEN
         CALL prelog  ; WRITE (UNIT=lunit, FMT='(a,i8)' ) &
        'Error Number : '   , loop
         CALL prelog  ; WRITE (UNIT=lunit, FMT='(a,i8)' ) &
        'Error Identifier: ', temp%num
         CALL prelog  ; WRITE (UNIT=lunit, FMT='(a,a)' ) &
        'Error Issued By: ' , temp%msg(1:31)
         CALL prelog  ; WRITE (UNIT=lunit, FMT='(a,a)' ) &
        'Error Message: '   , temp%msg(32:LEN_TRIM(temp%msg))
         ENDIF

         temp => temp%next
         loop = loop + 1

      END DO print_loop

      CALL prelog ; WRITE (UNIT=lunit, FMT='(a)' ) &
     "END INFO/WARNING/ERROR list."
      CALL prelog ; WRITE (UNIT=lunit, FMT='(a)' ) &
     "-----------------------------------------------------------------------"

   END SUBROUTINE print_all_error
   
!------------------------------------------------------------------------------!
   
   SUBROUTINE print_single_error ( lunit, current )
   
   !  Print a single stored error message.  Current is where the
   !  data is located to provide information.  This is useful only
   !  for debugging.
   
      IMPLICIT NONE
   
      INTEGER                        :: lunit
      TYPE ( error_info ) , POINTER  :: current
      
      IF ( .NOT. ASSOCIATED ( current ) ) THEN
         CALL prelog
         WRITE (UNIT=lunit, FMT='(A)') 'No error data at this location'
      ELSE
         CALL prelog
         WRITE (UNIT=lunit, FMT='(28x,a20,i8,a20,a31,a20)' ) &
         ADJUSTL ( 'Error Identifier: ' ) , current%num , &
         ADJUSTL ( 'Error Issued By: ' ) , ADJUSTL ( current%msg(1:31) )
         CALL prelog; WRITE (UNIT=lunit, FMT='(a101)') & 
         ADJUSTL ( 'Error Message: ' )
         CALL prelog; WRITE (UNIT=lunit, FMT='(A)')  &
       "-----------------------------------------------------------------------"
      ENDIF
   END SUBROUTINE print_single_error
   
!-----------------------------------------------------------------------------
   
   SUBROUTINE store_error ( current , error_number , error_message ) 
   
   !  Store this information in the already initialized linked list.
   !  Return the new pointer location.
   
      IMPLICIT NONE
      
      TYPE ( error_info ) , POINTER           :: current 
   
      INTEGER                                 :: error_number
      CHARACTER (LEN=132)                     :: error_message
   
      TYPE ( error_info ) , POINTER           :: error_report
   
      ALLOCATE ( error_report )
      error_report%num = error_number
      error_report%msg = error_message
      NULLIFY ( error_report%next )
      current%next => error_report
      current => error_report
   
   END SUBROUTINE store_error
!-----------------------------------------------------------------------------
