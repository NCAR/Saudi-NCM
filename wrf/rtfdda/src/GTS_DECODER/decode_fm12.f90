!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

SUBROUTINE decode_fm12

   ! Driver for decoding FM-12 format

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival, sttn_op, ro_ignore
   REAL                     :: rval, wind_unit
   CHARACTER ( LEN = llen ) :: cval, arg

   IF ( TRACE_MOST ) PRINT  * , 'decode_fm12 : in '
   CALL assign_outfile ( 23 )

   ! decode part of the bulletin that is not part of the CODE-FORM
   !
   CALL decode_bulletin_header ( ival )
   IF ( .NOT. ASSOCIATED ( current_line ) ) RETURN  ! header error

   ! Parts of FM-12 & FM-13 bulletin that appear only once, and not
   !    repeated in each message
   !
   ! next argument should be AAXX (FM-12) or BBXX (FM-13)
   !
   CALL get_next_word_in_mesg ( arg )
   cval = '....:MiMj :CODE HEADER ' // arg
   CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )
   wind_unit = MISSING
   sttn_op   = MISSING
   ro_ignore = MISSING

   ! group YYGGiw is not repeated in each message in FM 12-X SYNOP
   !
   IF ( record_fm .EQ. 12 ) THEN
      CALL get_next_ngrp_in_mesg ( arg )
      CALL code_table_YYGG ( arg(1:4) , ival , rval , cval , 'APPEND' )
      CALL code_table_1855 ( arg(5:5) , ival , wind_unit , cval , 'APPEND' )
      IF ( str2int(arg(1:4)) .EQ. UNDEFINED .OR. ival .EQ. UNDEFINED ) THEN
         CALL code_error ( 'fm12' , 'ERROR in header' , arg )
         PRINT  * , '***  ERROR in header , skipping entire bulletin ', arg
         record_error = record_error + 1
         CALL bulletin_skip ( ETX )
      ENDIF
   ENDIF

   loop_mesgs : DO WHILE ( ASSOCIATED ( current_line ) )

      ! First repeated section is 0
      CALL new_section ( 0 )

      ! First repeated argument is either 3 (FM-12) or 2 (FM-13)
      IF ( record_fm .EQ. 12 ) THEN
         ! FM 12-X SYNOP: 2 groups AAXX YYGGiw not repeated, start with IIiii
         section_argnum = 2
      ELSEIF ( record_fm .EQ. 13 ) THEN
         ! FM 13-X SHIP : 1 group  BBXX not repeated, start with D...D or 99...
         section_argnum = 1
      ELSE
         CALL code_error ( 'fm12', 'Unexpected fm : ', int2str(record_fm) )
         print *, ' === Unexpected fm, check code ===  '
      ENDIF

      loop_words : DO

         IF ( record_fm .EQ. 13 .AND. section_argnum .LE. 1 ) THEN
            CALL get_next_word_in_mesg ( arg )    ! get a word from the bulletin
         ELSE
            CALL get_next_ngrp_in_mesg ( arg )    ! get a ngrp from the bulletin
         ENDIF

         IF ( arg .EQ. ETX ) THEN                 ! check end of bulletin
            EXIT loop_mesgs

         ELSEIF ( LEN_TRIM ( arg ) .EQ. 0 ) THEN  ! check end of message
            EXIT loop_words

         ELSE
            section_argnum = section_argnum + 1   ! incr. argument count
            CALL decode_fm12_words ( arg )  ! decode the word
            IF ( section_id < 0 ) THEN
               ! serious error, STOP decoding this mesage
               CALL bulletin_skip ( MESG_DELIMITORS )
               EXIT loop_words
            ENDIF
         ENDIF

      ENDDO loop_words

      IF ( TRACE_ALL ) PRINT * , 'exit loop_mesgs'

   ENDDO loop_mesgs

   IF ( TRACE_ALL ) PRINT  * , 'decode_fm12 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE decode_fm12_words ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm12_words: in ', &
      TRIM(arg), ' ', TRIM(prev), ' ', section_id, section_argnum

   IF ( arg .EQ. 'NIL' .OR. arg .EQ. 'nil' .OR. arg .EQ. 'MIS' ) RETURN

   ! Mandatory groups are ordered by sequence, must be decoded before
   !   numerial indicated groupts
   !
   IF ( section_id .EQ. 0 ) THEN
      CALL decode_fm12_sec0 ( arg ) ! Mandatory : section 0
      prev = NULL

   ELSEIF ( section_id .EQ. 1 .AND. section_argnum <= 2) THEN
      CALL decode_fm12_sec1 ( arg ) ! Mandatory : groups IrIxhVV and Nddff
      prev = NULL
      ro_ignore = 0

   ELSEIF ( ro_ignore .GT. 0 ) then
      ! part of the MW xx.x QNT xx.x
      ro_ignore = ro_ignore - 1
      CALL code_ignore ( 'fm12_words' , 'ROHK-non-standard' , arg )

   ELSE ! Mandatory groups finished, check for new section header

      select_section_headers:SELECT CASE ( arg(1:3) )

         CASE ( '222' )
            CALL new_section ( 2 )
            CALL code_table_0700 ( arg(4:4) , ival , rval , cval , 'APPEND' )
            CALL code_table_4451 ( arg(5:5) , ival , rval , cval , 'APPEND' )
            RETURN

         CASE ( '333' , '444' , '555' )
            CALL new_section ( str2int ( arg(1:1) ) )
            RETURN

         CASE ( '800' )
            IF ( section_id .EQ. 3 .AND. arg .EQ. '80000' ) THEN
               CALL new_section ( 38 )
               RETURN
            ENDIF

         CASE DEFAULT
            IF ( arg(1:1) < prev(1:1) ) THEN
               CALL code_error ( 'fm12_words' , 'out of seq.' , &
                                 TRIM ( prev ) // ' x ' // TRIM ( arg ) )
               section_id = -1
            ENDIF

      ENDSELECT select_section_headers

      ck_sections : SELECT CASE ( section_id )

         CASE (  1 ) ; CALL decode_fm12_sec1 ( arg )

         CASE (  2 ) ; CALL decode_fm12_sec2 ( arg )

         CASE (  3 ) ; CALL decode_fm12_sec3 ( arg )

         CASE ( 38 ) ; ! skipping ( (80000) .... groups )
            IF ( TRACE_MOST ) &
               CALL code_ignore ( 'fm12_words' , 'skipped' , arg )

         CASE (  4 )
            ! Ct in group N'C'H'H'CT is ignored
            CALL code_table_2700 ( arg(1:1) , iget , rval , cval , ' ' )
            CALL code_table_0500 ( arg(2:2) , ival , rval , cget , ' ' )
            rget = multiply ( 100 , str2int ( arg(3:4) ) )
            CALL record_appendr ( iget , rget , cget )

         CASE (  5 ) ; ! skipping section 5 (nationally developed)
            IF ( TRACE_MOST ) &
               CALL code_ignore ( 'fm12_words' , 'skipped' , arg )

         CASE DEFAULT
            CALL code_error ( 'fm12_words' , 'unexpected' , arg )

      ENDSELECT ck_sections

      prev = arg

   ENDIF

   IF ( TRACE_MOST ) PRINT *,'decode_fm12_words: out ', &
      TRIM(arg), ' ', TRIM(prev), ' ', section_id, section_argnum

ENDSUBROUTINE decode_fm12_words

!----------------------------------------------------------------------

SUBROUTINE decode_fm12_sec0 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm12_sec0: in ', & 
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( record_fm )

   CASE ( 12 )

      IF ( section_argnum .EQ. 3 ) THEN
         CALL code_table_MMMM ( 'AAXX' , ival , rval , cval , 'APPEND' )
         CALL code_table_stnid ( arg(1:5), ival, rval, cval, 'APPEND' )
         IF ( ival < 0 ) THEN
            CALL code_error ( 'fm12_sec0' , 'Station Id' , arg )
            section_id = -1
         ELSE
            ! last item, change to section 1
            CALL new_section ( 1 ) ; section_argnum = 0
         ENDIF

      ELSE
         CALL code_error ( 'fm12_sec0' , 'unexpected A' , arg )
         section_id = -1
      ENDIF

   CASE ( 13 )

      IF ( section_argnum .EQ. 2 ) THEN
         CALL code_table_MMMM ( 'BBXX' , ival , rval , cval , 'APPEND' )
         wind_unit = MISSING
         SELECT CASE ( arg(1:1) )
            CASE ( '1' : '9' )
               CALL code_table_Abnnn ( arg )
            CASE DEFAULT
               CALL code_table_DDDD ( arg )
         ENDSELECT

      ELSEIF ( section_argnum .EQ. 3 ) THEN
         CALL code_table_YYGG ( arg(1:4) , ival , rval , cval , 'APPEND' )
         CALL code_table_1855 ( arg(5:5) , ival , wind_unit , cval , 'APPEND' )
         IF ( str2int(arg(1:4)) .EQ. UNDEFINED .OR. ival .EQ. UNDEFINED ) THEN
            CALL code_error ( 'fm12_sec0' , 'Invalid Date' , arg )
            section_id = -1
         ENDIF

      ELSEIF ( section_argnum .EQ. 4 ) THEN
         ! section_argnum would increase another time in code_table_LTLN
         CALL code_table_LTLN ( arg , ival , rval , cval , 'APPEND' )
         IF ( section_id .NE. -1 ) THEN
            ! last item, change to section 1
            CALL new_section ( 1 )
            section_argnum = 0
         ENDIF

      ELSE
         CALL code_error ( 'fm12_sec0' , 'unexpected B' , arg )
         section_id = -1
      ENDIF

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm12_sec0: out ', section_id

ENDSUBROUTINE decode_fm12_sec0

!----------------------------------------------------------------------

SUBROUTINE decode_fm12_sec1 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT * , 'decode_fm12_sec1: in ' , &
      section_argnum, ' ', TRIM ( arg )

   IF ( section_argnum .EQ. 1 ) THEN
      sttn_op = MISSING
      CALL code_table_1819 ( arg(1:1) , ival    , rval , cval , 'APPEND' ) ! iR
      CALL code_table_1860 ( arg(2:2) , sttn_op , rval , cval , 'APPEND' ) ! ix
      CALL code_table_1600 ( arg(3:3) , ival    , rval , cval , 'APPEND' ) ! h
      CALL code_table_4377 ( arg(4:5) , ival    , rval , cval , 'APPEND' ) ! VV

   ELSEIF ( section_argnum .EQ. 2 ) THEN

      CALL code_table_2700 ( arg(1:1) , ival , rval , cval , ' ' )
      CALL record_appendr ( ival , rval , '2700:N    :Total Cloud Cover' )

      CALL code_table_0877 ( arg(2:3) , ival , rval , cval , 'APPEND' )
      ival = str2int ( arg(4:5) )
      IF ( ival .NE. 99 ) THEN
         rval = multiply ( ival , wind_unit )
         CALL record_appendr ( ival , rval , '....:ff   :Wind speed' )
      ENDIF

   ELSE
      section_1_args : SELECT CASE ( arg(1:1) )

      CASE ( '0' )
         ival = str2int ( arg(3:5) )
         rval = multiply ( ival , wind_unit )
         CALL record_appendr ( ival , rval , '....:ff   :Wind speed' )

      CASE ( '1' )
         CALL code_table_3845 ( arg(2:2) , ival , rval , cval , ' ' )
         rval = multiply ( multiply ( 0.1 , rval ) , str2int ( arg (3:5) ) )
         CALL record_appendr ( ival , rval , '....:TTT  :Air Temperature (C) ' )

      CASE ( '2' )
         CALL code_table_3845 ( arg(2:2) , ival , rval , cval , ' ' )
         jval = str2int ( arg(3:5) )
         IF ( rval .EQ. 0 ) THEN
            CALL record_appendj ( ival , jval , &
                                  '....:UUU  :Relative Humidity (%) ' )
         ELSE
            rval = multiply ( multiply ( 0.1 , rval ) , jval )
            CALL record_appendr ( ival , rval , '....:Td   :Dew Point (C) ' )
         ENDIF

      CASE ( '3' )
         ival = str2int ( arg(2:5) )
         IF ( ival < 999 ) THEN ! maximum height for a station : 1099.9 hPa
            rval = add ( 1000 , multiply ( 0.1 , ival ) )
         ELSE
            rval = multiply ( 0.1 , ival )
         ENDIF
         CALL record_appendr ( ival , rval , '....:PPPP :Station pressure' )

      CASE ( '4' )
         IF ( arg(2:2) .EQ. '0' ) THEN
            ival = str2int ( arg(3:5) )
            rval = add ( 1000 , multiply ( 0.1 , ival ) )
            CALL record_appendr ( ival , rval , '....:PPPP :Mean SLP (hPa)' )
         ELSEIF ( arg(2:2) .EQ. '9' ) THEN
            ival = str2int ( arg(2:5) )
            rval = multiply ( 0.1 , ival )
            CALL record_appendr ( ival , rval , '....:PPPP :Mean SLP (hPa)' )
         ELSE
            CALL code_table_0264 ( arg(2:5) , ival , rval , cval , 'APPEND' )
         ENDIF

      CASE ( '5' )
         CALL code_table_0200 ( arg(2:5) , ival , rval , cval , 'APPEND' )

      CASE ( '6' )
         CALL code_table_4019 ( arg(2:5) , ival , rval , cval , 'APPEND' )

      CASE ( '7' )
         IF ( sttn_op .EQ. 4 ) THEN
            CALL code_table_4677 ( arg(2:3) , ival , rval , cval , 'APPEND' )
            CALL code_table_4561 ( arg(4:4) , ival , rval , cval , 'APPEND' )
            CALL code_table_4561 ( arg(5:5) , ival , rval , cval , 'APPEND' )
         ELSEIF ( sttn_op .NE. UNDEFINED .AND. sttn_op .NE. MISSING ) THEN
            CALL code_table_4680 ( arg(2:3) , ival , rval , cval , 'APPEND' )
            CALL code_table_4531 ( arg(4:4) , ival , rval , cval , 'APPEND' )
            CALL code_table_4531 ( arg(5:5) , ival , rval , cval , 'APPEND' )
         ELSE
            CALL code_warn ( 'fm12_sec1', 'Skipping 7wwWW for invalid sttn_op', arg )
            print *, ' === Unexpected sttn_op, skipping 7wwWW ===  '
         ENDIF

      CASE ( '8' )
         CALL code_table_2700 ( arg(2:2) , ival , rval , cval , 'APPEND' )
         CALL code_table_0513 ( arg(3:3) , ival , rval , cval , 'APPEND' )
         CALL code_table_0515 ( arg(4:4) , ival , rval , cval , 'APPEND' )
         CALL code_table_0509 ( arg(5:5) , ival , rval , cval , 'APPEND' )

      CASE ( '9' )
         CALL code_table_GGgg ( arg(2:5) , ival , rval , cval , 'APPEND' )

      CASE DEFAULT
         CALL code_error ( 'fm12_sec1' , 'unexpected' , arg )

      ENDSELECT section_1_args

   ENDIF

   IF ( TRACE_ALL ) PRINT *,'decode_fm12_sec1: out ', section_id

ENDSUBROUTINE decode_fm12_sec1

!----------------------------------------------------------------------

SUBROUTINE decode_fm12_sec2 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT * , 'decode_fm12_sec2: in ' , TRIM ( arg )

   section_2_args : SELECT CASE ( arg(1:1) )

      CASE ( '0' )
         CALL code_table_3850 ( arg(2:2) , ival , rval , cval , ' ' )
         rval = multiply ( multiply ( 0.1 , rval ) , str2int ( arg(3:5) ) )
         CALL record_appendr ( ival , rval , &
            '3850:SST  :Sea Surface Temperature (C) ' )

      CASE ( '1' )
         rval = multiply ( 0.5 , str2int ( arg(4:5) ) )
         CALL record_appendr ( str2int(arg(2:3)) , rval , &
           '....:PHwa :Period(s) and height(m) of wave obtained by instruments')

      CASE ( '2' )
         rval = multiply ( 0.5 , str2int ( arg(4:5) ) )
         CALL record_appendr ( str2int(arg(2:3)) , rval , &
           '....:PHw  :Period(s) and height(m) of wind wave' )

      CASE ( '3' )
         CALL code_table_0877 ( arg(2:3) , ival , rval , cval , ' ' )
         IF ( rval < UNDEFINED ) &
           CALL record_appendr ( ival, rval, '0877:dw1  :Swell wave direction' )
         CALL code_table_0877 ( arg(4:5) , ival , rval , cval , ' ' )
         IF ( rval < UNDEFINED ) &
           CALL record_appendr ( ival, rval, '0877:dw2  :Swell wave direction' )

      CASE ( '4' , '5' )
         rval = multiply ( 0.5 , str2int ( arg(4:5) ) )
         CALL record_appendr ( str2int(arg(2:3)) , rval , &
           '....:PHw  :Period(s) and height(m) of swell wave' )

      CASE ( '7' )
         ival = str2int ( arg(2:5) ) ; rval = multiply ( 0.1 , ival )
         CALL record_appendr ( ival , rval , '....:Hwa  :Measured Wave height(m)' )

      CASE ( '8' )
         CALL code_table_3850 ( arg(2:2) , ival , rval , cval , ' ' )
         rval = multiply ( multiply ( 0.1 , rval ) , str2int ( arg(3:5) ) )
         CALL record_appendr ( ival , rval , &
                            '3850:Tb   :Wet bulb temperature (C) ' )

      CASE DEFAULT
         ! the icing group ( 6xxxx + plain language ) is ignored
         CALL code_ignore ( 'fm12_sec2' , 'ignored' , arg )

   ENDSELECT section_2_args

   IF ( TRACE_ALL ) PRINT *,'decode_fm12_sec2: out ', section_id

ENDSUBROUTINE decode_fm12_sec2

!----------------------------------------------------------------------

SUBROUTINE decode_fm12_sec3 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm12_sec3: in ', TRIM ( arg )

   section_3_args : SELECT CASE ( arg(1:1) )

      CASE ( '0' )
         CALL code_table_0901 ( arg(2:2) , ival , rval , cval , 'APPEND' )
         CALL code_table_3845 ( arg(3:3) , ival , rval , cval , ' ' )
         rval = multiply ( rval, str2int ( arg(4:5) ) )
         CALL record_appendr ( ival , rval, &
                            'ASIA:TgTg :Ground / Grass Temperature (ASIA) ' )

      CASE ( '1' )
         CALL code_table_3845 ( arg(2:2) , ival , rval , cval , ' ' )
         rval = multiply ( multiply ( 0.1 , rval ) , str2int ( arg(3:5) ) )
         CALL record_appendr ( ival , rval , &
                            '....:TxTx :Max Temperature (C) ' )

      CASE ( '2' )
         CALL code_table_3845 ( arg(2:2) , ival , rval , cval , ' ' )
         rval = multiply ( multiply ( 0.1 , rval ) , str2int ( arg(3:5) ) )
         CALL record_appendr ( ival , rval , &
                            '....:TnTn :Min Temperature (C) ' )

      CASE ( '3' )
         CALL code_table_0901 ( arg(2:2) , ival , rval , cval , 'APPEND' )
         CALL code_table_3845 ( arg(3:3) , ival , rval , cval , ' ' )
         rval = multiply ( rval, str2int ( arg(4:5) ) )
         CALL record_appendr ( ival , rval, &
                            'ASIA:TgTg :Nght Tmin (ASIA)' )

      CASE ( '4' )
         CALL code_table_0975 ( arg(2:2) , ival , rval , cval , 'APPEND' )
         CALL code_table_3889 ( arg(3:5) , ival , rval , cval , 'APPEND' )

      CASE ( '5' )
         CALL decode_fm12_sec3_5 ( arg )

      CASE ( '6' )
         CALL code_table_4019 ( arg(2:5) , ival , rval , cval , 'APPEND' )

      CASE ( '7' )
         CALL code_table_R24  ( arg(2:5) , ival , rval , cval , 'APPEND' )

      CASE ( '8' )
         CALL code_table_2700 ( arg(2:2) , iget , rval , cval , ' ' )
         CALL code_table_0500 ( arg(3:3) , ival , rval , cget , ' ' )
         CALL code_table_1677 ( arg(4:5) , ival , rget , cval , ' ' )
         CALL record_appendr ( iget , rget , cget )

      CASE ( '9' )
         ! skipping the 9SpSpspsp GROUP (Code tabe 3778)
         ! Supplementary information about phenomena where appropiate
         IF ( TRACE_MOST ) &
            CALL code_ignore ( 'fm12_sec3' , '9SpSpspsp' , arg )

      CASE ( 'M' )
         ! Non Standard RO data in form of MW xx.x QNT xx.x
         IF ( TRIM(arg) .EQ. 'MW' ) then
            CALL code_ignore ( 'fm12_sec3' , 'ROHK-non-standard' , arg )
            ro_ignore = 3
         ENDIF

      CASE DEFAULT
         CALL code_error ( 'fm12_sec3' , 'unexpected' , arg )

   ENDSELECT section_3_args

   IF ( TRACE_ALL ) PRINT *,'decode_fm12_sec3: out ' , section_id

ENDSUBROUTINE decode_fm12_sec3

!----------------------------------------------------------------------

SUBROUTINE decode_fm12_sec3_5 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm12_sec3_5: in ', TRIM ( arg )

   section_3_5_args : SELECT CASE ( arg(2:2) )

      CASE ( '0', '1', '2', '3' )
         CALL code_table_1806 ( arg(2:5) , ival , rval , cval , 'APPEND' )

      CASE ( '4' )
         CALL code_table_0822 ( arg(3:5) , ival , rval , cval , 'APPEND' )

      CASE ( '5' )
         CALL decode_fm12_sec3_55 ( arg )

      CASE ( '6' )
         CALL code_table_0700 ( arg(3:3) , ival , rval , cval , ' ' )
         CALL record_appendr  ( ival , rval , TRIM(cval) // ' CL Moving dir.' )
         CALL code_table_0700 ( arg(4:4) , ival , rval , cval , ' ' )
         CALL record_appendr  ( ival , rval , TRIM(cval) // ' CM Moving dir.' )
         CALL code_table_0700 ( arg(5:5) , ival , rval , cval , ' ' )
         CALL record_appendr  ( ival , rval , TRIM(cval) // ' CH Moving dir.' )

      CASE ( '7' )
         CALL code_table_0500 ( arg(3:3) , ival , rval , cval , ' ' )
         CALL code_table_0700 ( arg(4:4) , iget , rget , cget , ' ' )
         CALL record_appendr ( ival , rget , TRIM ( cget ) // ' ' // cval(12:) )
         ! the 'ec' of the '57CDaec' group is not decoded

      CASE ( '8' )
         ival = str2int ( arg(3:5) ) ; rval = multiply (  0.1 , ival )
         CALL record_appendr ( ival , rval , '....:P24T :24-h Pre. Tendency' )

      CASE ( '9' )
         ival = str2int ( arg(3:5) ) ; rval = multiply ( -0.1 , ival )
         CALL record_appendr ( ival , rval , '....:P24T :24-h Pre. Tendency' )

      CASE DEFAULT
         CALL code_error ( 'fm35_sec3_5' , 'unexpected' , arg )

   ENDSELECT section_3_5_args

   IF ( TRACE_ALL ) PRINT *,'decode_fm12_sec3_5: out ' , section_id

ENDSUBROUTINE decode_fm12_sec3_5

!----------------------------------------------------------------------

SUBROUTINE decode_fm12_sec3_55 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( IN ) :: arg
   REAL                                 :: rval, rget
   INTEGER                              :: ival, iget, jval
   CHARACTER ( LEN = rlen )             :: cval, cget, arg2

   IF ( TRACE_MOST ) PRINT *,'decode_fm12_sec3_55: in ', TRIM ( arg )

   section_3_55_args : SELECT CASE ( arg(3:3) )

      CASE ( '0', '1', '2', '/' )
         IF ( arg(4:5) .NE. '//' ) THEN
            ival = str2int ( arg(3:5) ) ; rval = multiply ( 0.1 , ival )
            CALL record_appendr ( ival, rval, '....:3Sn24:Sunshine in 24 hr' )
         ENDIF
         radiation_24hr : DO
            CALL get_next_ngrp_in_mesg ( arg2 )
            IF ( LEN_TRIM ( arg2 ) .EQ. 0 ) THEN
               CALL code_error ( 'fm12_sec3_55', 'no rad. data A' , arg )
               section_id = -1
               EXIT radiation_24hr
            ENDIF
            rval = str2int ( arg2(2:5) ) ; ival = str2int ( arg2(1:1) )
            SELECT CASE ( arg2(1:1) )
               CASE ( '0' ) ; cval = 'net radiation 24 hr, J/cm2'
               CASE ( '1' ) ; cval = 'net radiation 24 hr, J/cm2'; rval = -rval
               CASE ( '2' ) ; cval = 'global solar radiation 24 hr, J/cm2'
               CASE ( '3' ) ; cval = 'diffused solar radiation 24 hr, J/cm2'
               CASE ( '4' ) ; cval = 'downward longwave radiation 24 hr, J/cm2'
               CASE ( '5' ) ; cval = 'upward longwave radiation 24 hr, J/cm2'
               CASE ( '6' ) ; cval = 'shortwave radiation 24 hr, J/cm2'
               CASE ( ' ' ) ; remaining_text = MESG_DELIMITORS(1:1)
                              EXIT radiation_24hr
               CASE DEFAULT ; CALL push_word_back_to_mesg ( arg2 )
                              EXIT radiation_24hr
            ENDSELECT
            CALL record_appendr ( ival, rval, '....:FFF24:' // cval )
         ENDDO radiation_24hr

      CASE ( '3' )
         IF ( arg(4:5) .NE. '//' ) THEN
            ival = str2int ( arg(4:5) ) ; rval = multiply ( ival , 0.1 )
            CALL record_appendr ( ival, rval, '....:3Sun :Sunshine in past hr' )
         ENDIF
         radiation_past_hr : DO
            CALL get_next_ngrp_in_mesg ( arg2 )
            IF ( LEN_TRIM ( arg2 ) .EQ. 0 ) THEN
               CALL code_error ( 'fm12_sec3_55', 'no rad. data B' , arg )
               section_id = -1
               EXIT radiation_past_hr
            ENDIF
            rval = str2int ( arg2(2:5) ) ; ival = str2int ( arg2(1:1) )
            SELECT CASE ( arg2(1:1) )
               CASE ( '0' ); cval = 'net radiation past hr, KJ/m2'
               CASE ( '1' ); cval = 'net radiation past hr, KJ/m2'; rval = -rval
               CASE ( '2' ); cval = 'global solar radiation past hr, KJ/m2'
               CASE ( '3' ); cval = 'diffused solar radiation past hr, KJ/m2'
               CASE ( '4' ); cval = 'downward longwave radiation past hr, KJ/m2'
               CASE ( '5' ); cval = 'upward longwave radiation past hr, KJ/m2'
               CASE ( '6' ); cval = 'shortwave radiation past hr, KJ/m2'
               CASE ( ' ' ); remaining_text = MESG_DELIMITORS(1:1)
                             EXIT radiation_past_hr
               CASE DEFAULT ; CALL push_word_back_to_mesg ( arg2 )
                             EXIT radiation_past_hr
            ENDSELECT
            CALL record_appendr ( ival, rval, '....:FFFpast:' // cval )
         ENDDO radiation_past_hr

      CASE ( '4' )
         CALL get_next_ngrp_in_mesg ( arg2 )
         IF ( LEN_TRIM ( arg2 ) .EQ. 0 ) THEN
            CALL code_error ( 'fm12_sec3_55', 'no rad. data C' , arg )
            section_id = -1
            RETURN
         ENDIF
         IF ( arg2(1:1) .EQ. '4' ) THEN
            rval = str2int ( arg2(2:5) ) ; ival = rval
            IF ( arg(4:5) .EQ. '07' ) THEN
               cval = '....:4NTSW:Net Short-wave in past hour, in KJ/m2'
            ELSEIF ( arg(4:5) .EQ. '08' ) THEN
               cval = '....:4DRSW:Direct Short-wave in past hour, in KJ/m2'
            ELSE
               ival = UNDEFINED
               CALL code_error ( '554x', 'Short-wave in past hour', arg )
            ENDIF
         ELSE
            ival = UNDEFINED
            CALL code_error ( arg(2:5), 'Short-wave in past hour', arg2 )
         ENDIF
         IF ( ival .NE. UNDEFINED ) CALL record_appendr ( ival, rval, cval )

      CASE ( '5' )
         CALL get_next_ngrp_in_mesg ( arg2 )
         IF ( LEN_TRIM ( arg2 ) .EQ. 0 ) THEN
            CALL code_error ( 'fm12_sec3_55', 'no rad. data D' , arg )
            section_id = -1
            RETURN
         ENDIF
         IF ( arg2(1:1) .EQ. '5' ) THEN
            rval = str2int ( arg2(2:5) ) ; ival = rval
            IF ( arg(4:5) .EQ. '07' ) THEN
               cval = '....:5NTSW:Net Short-wave in past 24 hr, in J/cm2'
            ELSEIF ( arg(4:5) .EQ. '08' ) THEN
               cval = '....:5DRSW:Direct Short-wave in past 24 hr, in J/cm2'
            ELSE
               ival = UNDEFINED
               CALL code_error ( '555x', 'Short-wave in past 24 hr', arg )
            ENDIF
         ELSE
            ival = UNDEFINED
            CALL code_error ( arg(2:5), 'Short-wave in past 24 hr', arg2 )
         ENDIF
         IF ( ival .NE. UNDEFINED ) CALL record_appendr ( ival, rval, cval )

      CASE DEFAULT
         CALL code_error ( 'fm12_sec3_55' , 'invalid arg' , arg )

   ENDSELECT section_3_55_args

   IF ( TRACE_ALL ) PRINT *,'decode_fm12_sec3_55: out ' , section_id

ENDSUBROUTINE decode_fm12_sec3_55

!----------------------------------------------------------------------

ENDSUBROUTINE decode_fm12
