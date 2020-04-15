!INCLUDE 'record.module'
!INCLUDE 'bulletin.module'

!----------------------------------------------------------------------

SUBROUTINE parse_line ( string , delims , first_word , rest )

   ! SUBROUTINE to parse line into 2 parts, given a set of delimitors
   !
   ! note : the routine is written in a way such that it is possible to CALL
   !        parse_line ( line , first_word , line ) or ( line , line , rest )
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN )  :: string ,      & ! String to parse
                                             delims          ! Word delimitor set
   CHARACTER ( LEN = * ) , INTENT ( OUT ) :: first_word ,  & ! First word in string
                                             rest            ! Rest of string

   CHARACTER ( LEN = len ( string ) )     :: in_str
   INTEGER                                :: slen

   in_str = string
   slen = SCAN ( in_str , delims )
   IF ( slen .EQ. 0 ) THEN
      first_word = in_str
      rest = ''
   ELSE
      first_word = in_str(:slen-1)
      find_next_char : DO WHILE ( slen <= len ( in_str ) )
         IF ( SCAN ( delims , in_str(slen:slen) ) .EQ. 0 ) THEN
            EXIT find_next_char
         ELSE
            slen = slen + 1
         ENDIF
      ENDDO find_next_char
      rest = in_str(slen:)
   ENDIF

ENDSUBROUTINE parse_line

! ----------------------------------------------------------------------

SUBROUTINE cset_msg_time ( ddhhmm )

   USE record_def
   IMPLICIT NONE
   CHARACTER ( LEN = 6 ) , INTENT ( IN ) :: ddhhmm
   INTEGER                               :: dhm

   dhm = str2int ( ddhhmm )
   CALL iset_msg_time ( dhm )

   ! Output the message time for reference (one of those lines)
   ! PRINT *, ' cset_msg_time : out ', msg_yymmdd, msg_hhmmss, dhm, ddhhmm

ENDSUBROUTINE cset_msg_time

!----------------------------------------------------------------------

SUBROUTINE iset_msg_time ( dhm )

   USE record_def
   IMPLICIT NONE
   INTEGER , INTENT ( IN ) :: dhm

   IF ( dhm < 0 ) THEN
      CALL code_error ( 'set_msg_time' , 'invalid ddhhmm' , int2str(dhm) )
      msg_yymmdd = UNDEFINED
      msg_hhmmss = UNDEFINED
      RETURN
   ENDIF

   msg_year   = utc_year
   msg_month  = utc_month
   msg_day    = INT ( dhm / 10000 )
   IF ( msg_day > utc_day ) THEN
      msg_month = msg_month - 1
      IF ( msg_month .EQ. 0 ) THEN
         msg_month = 12
         msg_year  = msg_year - 1
      ENDIF
   ELSE IF ( msg_day .eq. 0 ) THEN
      msg_day = utc_day
   ENDIF
   msg_yymmdd = msg_year * 10000 + msg_month * 100 + msg_day

   msg_hhmmss = MOD ( dhm , 10000 ) * 100
   msg_hour   = INT ( msg_hhmmss / 10000 )
   msg_minute = MOD ( dhm , 100 )
   msg_second = 0

ENDSUBROUTINE iset_msg_time

!----------------------------------------------------------------------

SUBROUTINE decrease_date ( yymmdd , new_ymd )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: yymmdd
   INTEGER , INTENT ( OUT ) :: new_ymd
   INTEGER                  :: year , month , day
   INTEGER , PARAMETER , DIMENSION (12) :: mdays =  &
      (/ 31 , 28 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 /)

   year  = yymmdd / 10000
   month = INT ( MOD ( yymmdd , 10000 ) / 100 )
   day   = MOD ( yymmdd , 100 )

   day = day - 1
   IF ( day .EQ. 0 ) THEN
      month = month - 1
      IF ( month .EQ. 0 ) THEN
         month = 12
         year  = year - 1
      ENDIF
      day = mdays(month)
      IF ( month .EQ. 2 ) THEN
         IF ( MOD(year,4) .EQ. 0 ) THEN
            IF ( MOD(year,100) .NE. 0 .OR. MOD(year,400) .EQ. 0 ) day = 29
         ENDIF
      ENDIF
   ENDIF
   new_ymd = year * 10000 + month * 100 + day

ENDSUBROUTINE decrease_date

!----------------------------------------------------------------------

FUNCTION set_century ( yy )

   IMPLICIT NONE
   INTEGER , INTENT ( IN ) :: yy
   INTEGER                 :: set_century

   IF ( yy > 90 ) THEN
      set_century = 1900 + yy
   ELSEIF ( yy >= 0 ) THEN
      set_century = 2000 + yy
   ENDIF

ENDFUNCTION

!----------------------------------------------------------------------

SUBROUTINE print_statistics ( iout )

   USE bulletin_def
   USE record_def
   IMPLICIT NONE

   INTEGER , INTENT ( IN ) :: iout
   INTEGER                 :: i , fm

   IF ( TRACE_NEW_MOST ) PRINT  * , 'print_statistics : in'

   bul_stat ( 100 , : ) = sum ( bul_stat , 1 )
   msg_stat ( 100 , : ) = sum ( msg_stat , 1 )

   WRITE ( iout, '(A)' ) ' ====================================================== '
   DO fm = 1, 100
      IF ( bul_stat ( fm , number ) > 0 ) &
         WRITE ( iout ,'(1x,a8,1x,4I10)' ) 'bulletin', fm, (bul_stat(fm,i), i=1,3)
   ENDDO
   WRITE ( iout, '(A)' ) ' ====================================================== '
   DO fm = 1, 100
      IF ( msg_stat ( fm , number ) > 0 ) &
         WRITE ( iout ,'(1x,a8,1x,4I10)' ) 'message ', fm, (msg_stat(fm,i), i=1,3)
   ENDDO
   WRITE ( iout, '(A)' ) ' ====================================================== '
   WRITE ( iout, '(A20,2I15)' ) ' Summary statistics ', utc_yymmdd, utc_hhmmss

   IF ( TRACE_NEW_MOST ) PRINT  * , 'print_statistics : out'

ENDSUBROUTINE print_statistics

