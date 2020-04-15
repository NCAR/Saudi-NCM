!INCLUDE 'record.module'
!INCLUDE 'bulletin.module'

SUBROUTINE decode_bulletin_header ( ddhhmm )

   ! Routine to decoder message header (this part of the bulletin does
   !    not appear to be part of the WMO code form specification
   !
   ! After passing through identify code form, it is guaranteed that
   !    there is at least 4 lines in bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE record_def
   USE bulletin_def
   IMPLICIT NONE
   INTEGER , INTENT ( OUT ) :: ddhhmm
!  INTEGER                  :: seqnum
   CHARACTER ( LEN = llen ) :: msghdr , cval , rest

   IF ( TRACE_ALL ) PRINT  * , 'decode_bulletin_header : in'

   current_line => bulletin

   ! First line of bulletin ( a three digit sequence number )
   current_line   => current_line%next_line
   seqnum         =  str2int ( TRIM ( current_line%text ) )
   IF ( seqnum < 0 ) THEN
      CALL code_error ( 'bulletin_header' , 'seqnum' , current_line%text )
      ddhhmm = UNDEFINED
      CALL bulletin_skip ( ETX )
      NULLIFY ( current_line )
      RETURN
   ENDIF

   ! Second line of bulletin ( reporting center id, time of report )
   current_line   => current_line%next_line
   msghdr         =  current_line%text
   msg_header     = TRIM ( msghdr )

   CALL parse_line ( msghdr , WORD_DELIMITORS , cval , rest ) ! message ID
   CALL parse_line ( rest   , WORD_DELIMITORS , cval , rest ) ! center ID
   CALL parse_line ( rest   , WORD_DELIMITORS , cval , rest ) ! Reporting time
   CALL cset_msg_time ( TRIM ( cval ) )
   IF ( msg_yymmdd < 0 .OR. msg_hhmmss < 0 ) THEN
      ddhhmm = UNDEFINED
      CALL bulletin_skip ( ETX )
      NULLIFY ( current_line )
      RETURN
   ENDIF
   bul_year = msg_year ; bul_month = msg_month ; bul_day = msg_day
   bul_hour = msg_hour ; bul_minute= msg_minute; bul_second = msg_second
   bul_yymmdd = msg_yymmdd ; bul_hhmmss = msg_hhmmss

   IF ( LEN_TRIM ( rest ) .EQ. 0 ) THEN
      cval = '0002:BHDR :Bulletin hdr DDHHMM ' // TRIM ( msghdr )
   ELSE
      cval = '-002:BHDR :Bulletin hdr DDHHMM ' // TRIM ( msghdr )
   ENDIF
   ddhhmm = bul_day*10000 + bul_hour*100 + bul_minute
   CALL record_appendj ( seqnum , ddhhmm , cval )

   current_line   => current_line%next_line
   remaining_text =  current_line%text

   IF ( TRACE_MOST ) PRINT  * , 'decode_bulletin_header : out ', ddhhmm

ENDSUBROUTINE decode_bulletin_header
