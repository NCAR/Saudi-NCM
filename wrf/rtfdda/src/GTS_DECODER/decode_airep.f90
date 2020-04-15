!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

SUBROUTINE decode_airep

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival , ddhhmm
   REAL                     :: rval
   CHARACTER ( LEN = llen ) :: cval, arg

   IF ( TRACE_MOST ) PRINT  * , 'decode_airep : in '
   CALL assign_outfile ( 23 )

   ! decode part of the bulletin that is not part of the CODE-FORM
   CALL decode_bulletin_header ( ddhhmm )
   IF ( .NOT. ASSOCIATED ( current_line ) ) RETURN  ! header error

   ! next argument should be AIREP
 
   CALL get_next_word_in_mesg ( arg )
   cval = '....:MiMj :CODE HEADER ' // arg
   CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )

   IF ( arg(1:5) .NE. 'AIREP' ) THEN
      CALL code_error ( 'airep' , 'ERROR in header' , arg )
      PRINT  * , '***  ERROR in header , skipping entire bulletin ', arg
      record_error = record_error + 1
      CALL bulletin_skip ( ETX )
   ENDIF

   ! repeated groups

   loop_mesgs : DO WHILE ( ASSOCIATED ( current_line ) )

      CALL new_section ( 1 )

      loop_words : DO
      
         CALL get_next_word_in_mesg ( arg )       ! get a word from the bulletin

         IF ( arg .EQ. ETX ) THEN                 ! check end of bulletin
            EXIT loop_mesgs

         ELSEIF ( LEN_TRIM ( arg ) .EQ. 0 ) THEN  ! check end of message
            EXIT loop_words

         ELSE
            CALL decode_airep_words ( arg )  ! decode the word
            IF ( section_id < 0 ) THEN
               ! serious error, STOP decoding this mesage
               CALL bulletin_skip ( MESG_DELIMITORS )
               EXIT loop_words
            ENDIF
         ENDIF

      ENDDO loop_words

      IF ( TRACE_ALL ) PRINT * , 'exit loop_mesgs'

   ENDDO loop_mesgs

   IF ( TRACE_ALL ) PRINT  * , 'decode_airep : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE decode_airep_words ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_airep_words: in ', &
      section_id, section_argnum, ' ', TRIM(arg)

   IF ( arg .EQ. 'NIL' .OR. arg .EQ. 'nil' .OR. arg .EQ. 'MIS' ) RETURN

   ! Mandatory groups are sequence indicated (oriented), must be decoded
   !   before numerial indicated groupts
   !   
   ! [ARP|ARS] position time flight level temp wind ...

   IF ( arg .eq. 'ARP' .or. arg .eq. 'ARS' ) then
      CALL new_section ( 1 ) ! new AIREP
   ENDIF

   CALL decode_airep_sec1 ( arg )

   IF ( TRACE_MOST ) PRINT *,'decode_airep_words: out ', &
      TRIM(arg), ' ', section_id, section_argnum

ENDSUBROUTINE decode_airep_words

!----------------------------------------------------------------------

SUBROUTINE decode_airep_sec1 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget
   CHARACTER ( LEN = rlen ) , SAVE       :: arg2

   IF ( TRACE_MOST ) PRINT *,'decode_airep_sec1: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

   CASE ( 1 )

      ! Decode [ ARP | ARS ] and aircraft identification

      IF ( arg .eq. 'ARP' .OR. arg .eq. 'ARS' ) then
         ! arg is ARP or ARS
         ! read arg2 as aircraft identification
         CALL get_next_word_in_mesg ( arg2 )
         IF ( LEN_TRIM (arg2) .EQ. 0 ) THEN
            call push_word_back_to_mesg ( ETX ) 
            RETURN
         ENDIF
      ELSE
         ! arg is aircraft identification, set to arg2
         arg2 = arg
      ENDIF
      
      msg_yymmdd = bul_yymmdd
      msg_hhmmss = bul_hhmmss
      CALL code_table_MMMM ( 'AIREP' , ival , rval , cval , 'APPEND' )
      cval = '....:AIREP:Flight Number ' // TRIM(arg2) // ' ' // TRIM(arg)
      CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )
      section_argnum = 2

   CASE ( 2 )

      ! Decode Position ( 1 or 2 words )

      CALL code_airep_ltln ( arg, ival, rval, cval, 'APPEND' )

      section_argnum = 3

   CASE ( 3 )

      ! Decode Time ( HHMM or just MM )

      IF ( LEN_TRIM ( arg ) .eq. 4 ) THEN
         call code_table_GGgg ( arg, ival , rval , cval , 'APPEND' )

      ELSE IF ( LEN_TRIM ( arg ) .eq. 2 ) THEN
         iget = MOD ( bul_hhmmss , 10000 ) + str2int ( arg )
         cget = int2str ( iget )
         CALL code_table_GGgg ( cget(1:4), ival, rval, cval, 'APPEND' )

      ELSE
         ival = MISSING
         rval = MISSING

      ENDIF

      msg_yymmdd = ival
      msg_hhmmss = NINT ( rval )

      IF ( msg_yymmdd < 0 .OR. msg_hhmmss < 0 ) THEN
         cval = arg
         CALL record_appendj ( msg_yymmdd , msg_hhmmss , cval )
         CALL code_error ( 'airep_sec1', 'invalid Obs. Time', cval )
         section_id = -1
      ELSE
         section_argnum = 4
      ENDIF

   CASE ( 4 )

      ! Decode Flight Level ( e.g. F310 or 310 as 31000 ft )
      CALL code_airep_flvl ( arg, ival, rval, cval, 'APPEND' )
      section_argnum = 5
   
   CASE ( 5 ) 

      ! Decode Temperature ( e.g. MS46 as -46C )
      CALL code_airep_temp ( arg, ival, rval, cval, 'APPEND' )
      section_argnum = 6

   CASE ( 6 )

      ! Decode Wind  ( e.g. 270/050KT as 50 knot wind at 270 deg )
      CALL code_airep_wind ( arg, ival, rval, cval, 'APPEND' )
      section_argnum = 7

   CASE DEFAULT

      ! ONLY ONE TEMPERATURE / WIND GROUP is decoded per AIREP report
      CALL code_ignore ( 'airep_sec1' , 'ignored' , arg )

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_airep_sec1: out ', section_id

ENDSUBROUTINE decode_airep_sec1

!----------------------------------------------------------------------

ENDSUBROUTINE decode_airep
