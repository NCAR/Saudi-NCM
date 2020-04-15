!INCLUDE 'bulletin.module'

SUBROUTINE read_a_bulletin ( inunit )

   ! Routine to read in a bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   IMPLICIT NONE

   INCLUDE 'inc.traceflow'
   INCLUDE 'inc.special_symbols'

   INTEGER , INTENT ( IN )  :: inunit
   CHARACTER ( LEN = llen ) :: line , dummy

   IF ( TRACE_MOST ) PRINT  * , 'read_a_bulletin : in'

   CALL bulletin_initialize

   read_loop : DO
      READ ( inunit , '(A)' , END = 90 , ERR = 99 ) line
      write(dummy,'(a)') line(1:1)  ! This dummy statement allows the program to run on the O2000
      CALL parse_line ( adjustl ( line ) , LINE_DELIMITORS , line , dummy )
      IF ( SCAN ( line , ETX ) > 0 ) THEN
         CALL bulletin_append ( ETX )
         EXIT read_loop
      ELSEIF ( SCAN ( line , SOH ) > 0 ) THEN
         IF ( bulletin_nlines == 0 ) then
            ! Skipping header SOH
         ELSE
            ! Warning : ETX is missing in end of message, add one
            CALL bulletin_append ( ETX )
            EXIT read_loop
         ENDIF
      ELSEIF ( bulletin_nlines .GE. bulletin_maxlines ) THEN
         PRINT *,'Number of lines in current bulletin >= ', bulletin_maxlines 
         PRINT *,'========== Skipping overflow lines ==========='
         skip_overflow : DO
            bulletin_nlines = bulletin_nlines + 1
            READ ( inunit , '(A20)', END = 90 , ERR = 99 ) line
            IF ( SCAN ( line(1:20) , ETX//SOH ) > 0 ) THEN
               CALL bulletin_append (ETX )
               EXIT read_loop
            ENDIF
         ENDDO skip_overflow
      ELSE
         CALL bulletin_append ( line )
      ENDIF
   ENDDO read_loop

   IF ( bulletin_nlines .GE. bulletin_maxlines ) THEN
      PRINT *,'Number of lines in overflow bulletin = ', bulletin_nlines
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'read_a_bulletin : out'
   RETURN

90 CALL bulletin_append ( ETX )
   IF ( bulletin_nlines .EQ. 1 ) bulletin_nlines = -1
   PRINT  * , ' read_a_bulletin : ***  End of   input unit : ' , inunit
   RETURN

99 CALL bulletin_append ( ETX )
   IF ( bulletin_nlines .EQ. 1 ) bulletin_nlines = -1
   ! bulletin_nlines = -1
   PRINT  * , ' read_a_bulletin : ***  Error in input unit : ' , inunit
   RETURN

ENDSUBROUTINE read_a_bulletin
