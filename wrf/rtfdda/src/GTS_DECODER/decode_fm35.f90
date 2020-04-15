!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

SUBROUTINE decode_fm35

   ! Driver for decoding FM-35 format

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival, ddhhmm
   REAL                     :: rval
   CHARACTER ( LEN = llen ) :: cval, arg, previous

   CHARACTER ( LEN = 1 )    :: part
   REAL                     :: wind_unit
   INTEGER                  :: last_wind_lvl , pressure , sttn_pre

   IF ( TRACE_MOST ) PRINT * , 'decode_fm35 : in '
   CALL assign_outfile ( 23 )

   ! decode part of the bulletin that is not part of the CODE-FORM
   CALL decode_bulletin_header ( ddhhmm )
   IF ( .NOT. ASSOCIATED ( current_line ) ) RETURN  ! header error

   ! No mandatory group

   loop_mesgs : DO WHILE ( ASSOCIATED ( current_line ) )

      CALL new_section ( 1 ) ; section_argnum = 0

      loop_words : DO

         CALL get_next_ngrp_in_mesg ( arg )       ! get a word from the bulletin

         IF ( arg .EQ. ETX ) THEN                 ! check end of bulletin
            EXIT loop_mesgs

         ELSEIF ( LEN_TRIM ( arg ) .EQ. 0 ) THEN  ! check end of message
            EXIT loop_words

         ELSE
            section_argnum = section_argnum + 1
            CALL decode_fm35_words ( arg , previous )  ! decode the word
            IF ( section_id < 0 ) THEN
               ! serious error, STOP decoding this mesage
               CALL bulletin_skip ( MESG_DELIMITORS )
               EXIT loop_words
            ENDIF
         ENDIF

      ENDDO loop_words

      IF ( TRACE_ALL ) PRINT * , 'exit loop_mesgs'

   ENDDO loop_mesgs

   IF ( TRACE_ALL ) PRINT  * , 'decode_fm35 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE decode_fm35_words ( arg, previous )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   CHARACTER ( LEN = * )                 :: previous
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_words: in ', &
      TRIM(arg), ' ', section_id, section_argnum

   IF ( arg .EQ. 'NIL' .OR. arg .EQ. 'nil' .OR. arg .EQ. 'MIS' ) then
      previous = arg
      RETURN
   endif

   ! Mandatory groups are sequence indicated (oriented), must be decoded
   !   before numerial indicated groupts
   !
   IF ( section_id .EQ. 1 ) THEN
      CALL decode_fm35_sec1 ( arg ) ! Mandatory : FM-35 to 38
      previous = arg
      RETURN

   ELSEIF ( section_id .EQ. 2 ) THEN
      CALL decode_fm35_sec2 ( arg ) ! Mandatory : FM-35 FM-37
      IF ( section_subgrp .NE. 5 ) then
         previous = arg
         RETURN
      endif

   ENDIF

   ! Mandatory groups finished, check for new section header
   select_section_headers:SELECT CASE ( arg )

      CASE ( '21212' )
         IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 6 )
            previous = arg
            RETURN
         ENDIF

      CASE ( '31313' )
         IF ( part .EQ. 'B' .or. part .EQ. 'D') THEN
            CALL new_section ( 7 )
            previous = arg
            RETURN
         ELSE
            if (index(previous,'77999') > 0) CALL new_section ( 7 )
            previous = arg
            RETURN
         ENDIF

      CASE ( '41414' )
         IF ( part .EQ. 'B' ) THEN
            CALL new_section ( 8 )
            previous = arg
            RETURN
         ENDIF

      CASE ( '51515' , '52525' , '53535' , '54545' , &
             '56565' , '57575' , '58585' , '59595' )
         ! the standard 1988 edition code form specified this group
         ! should only appear in sections 'B' and 'D'. However, in
         ! practise, the '51515' group is also seen in section 'A'
         ! IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 9 )
            previous = arg
            RETURN
         ! ENDIF

      CASE ( '61616' , '62626' , '63636' , '64646' , &
             '65656' , '67676' , '68686' , '69696' )
         IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 10 )
            previous = arg
            RETURN
         ENDIF

      CASE ( 'TTAA' , 'TTBB' , 'TTCC' , 'TTDD' , &
             'UUAA' , 'UUBB' , 'UUCC' , 'UUDD' , &
             'XXAA' , 'XXBB' , 'XXCC' , 'XXDD' , &
             'IIAA' , 'IIBB' , 'IICC' , 'IIDD' )
         CALL new_section ( 1 )
         CALL decode_fm35_sec1 ( arg )
         previous = arg
         RETURN

      CASE DEFAULT

         SELECT CASE ( arg(1:2) )

         CASE ( '99' )
            IF ( part .EQ. 'A' .AND. section_id < 2 ) THEN
               CALL new_section ( 2 )
               previous = arg
               RETURN
            ENDIF

         CASE ( '88' )
            IF ( part .EQ. 'A' .OR. part .EQ. 'C' ) THEN
               pressure = str2int ( arg(3:5) )
               CALL new_section ( 3 )
               cval = '....:88PPP:Pressure (mb) at tropopause level'
               IF ( pressure .EQ. 999 ) THEN
                  ! PRINT * , 'no tropopause information'
                  CALL record_appendi ( MISSING , cval )
               ELSEIF ( part .EQ. 'A' ) THEN
                  CALL record_appendi ( pressure , cval )
               ELSEIF ( part .EQ. 'C' ) THEN
                  rval = multiply ( pressure , 0.1 )
                  pressure = divide ( pressure , 10 )
                  CALL record_appendr ( pressure , rval , cval )
               ENDIF
               previous = arg
               RETURN
            ENDIF

         CASE ( '77' , '66' )
            IF ( part .EQ. 'A' .OR. part .EQ. 'C' ) THEN
               pressure = str2int ( arg(3:5) )
               CALL new_section ( 4 )
               cval='....:'//arg(1:2)//'PPP:P @ max V level (mb)'
               IF ( pressure .EQ. 999 ) THEN
                  ! PRINT * , 'no maximum wind level information'
                  CALL record_appendi ( MISSING , cval )
               ELSEIF ( part .EQ. 'A' ) THEN
                  CALL record_appendi ( pressure , cval )
               ELSEIF ( part .EQ. 'C' ) THEN
                  rval = multiply ( pressure , 0.1 )
                  pressure = divide ( pressure , 10 )
                  CALL record_appendr ( pressure , rval , cval )
               ENDIF
               previous = arg
               RETURN
            ENDIF

         ENDSELECT

   ENDSELECT select_section_headers

   ck_sections : SELECT CASE ( section_id )

         CASE ( 2 ) ; CALL decode_fm35_sec2 ( arg )

         CASE ( 3 ) ; CALL decode_fm35_sec3 ( arg )

         CASE ( 4 ) ; CALL decode_fm35_sec4 ( arg )

         CASE ( 5 ) ; CALL decode_fm35_sec5 ( arg )

         CASE ( 6 ) ; CALL decode_fm35_sec6 ( arg )

         CASE ( 7 ) ; CALL decode_fm35_sec7 ( arg )

      CASE ( 8 )
         ! group NhCLhCMCH
         CALL code_table_1600 ( arg(3:3) , ival , rval , cval , 'APPEND' )
         CALL code_table_2700 ( arg(1:1) , ival , rval , cval , 'APPEND' )
         CALL code_table_0513 ( arg(2:2) , ival , rval , cval , 'APPEND' )
         CALL code_table_0515 ( arg(4:4) , ival , rval , cval , 'APPEND' )
         CALL code_table_0509 ( arg(5:5) , ival , rval , cval , 'APPEND' )

      CASE ( 9 , 10 ) ; ! skipping sections 9 and 10 (nationally developed)
         IF ( TRACE_MOST ) &
         CALL code_ignore ( 'fm35_words' , 'ignored' , arg )

      CASE DEFAULT
         CALL code_error ( 'fm35_words' , 'unexpected' , arg )

   ENDSELECT ck_sections

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_words: out ', &
      TRIM(arg), ' ', section_id, section_argnum

ENDSUBROUTINE decode_fm35_words

!----------------------------------------------------------------------

SUBROUTINE decode_fm35_sec1 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget , arg2

   IF ( TRACE_MOST ) PRINT * , 'decode_fm35_sec1: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

   CASE ( 1 )
      part          = '?'
      wind_unit     = MISSING
      last_wind_lvl = MISSING
      pressure      = MISSING
      sttn_pre      = MISSING
      SELECT CASE ( arg(1:2) )
      CASE ( 'TT' ) ; record_fm = 35
      CASE ( 'UU' ) ; record_fm = 36
      CASE ( 'XX' ) ; record_fm = 37
      CASE ( 'II' ) ; record_fm = 38
      CASE DEFAULT
         CALL code_error ( 'fm35_sec1' , 'TEMP code header' , arg )
         section_id = -1
         RETURN
      ENDSELECT

      part = arg(3:3)
      IF ( part .lt. 'A' .OR. part .gt. 'D' ) THEN
         CALL code_error ( 'fm35_sec1' , 'TEMP part info' , arg )
         section_id = -1
         RETURN
      ENDIF
      CALL code_table_MMMM ( arg(1:4) , ival , rval , cval , 'APPEND' )
      cval = '....:MiMj :CODE HEADER ' // arg
      CALL record_appendj ( bul_yymmdd, bul_hhmmss, arg )

      IF ( record_fm .EQ. 36 .OR. record_fm .EQ. 38 ) THEN
         CALL get_next_word_in_mesg ( arg2 )   ! note no increase in argnum
         IF ( LEN_TRIM ( arg2 ) .lt. 3 ) THEN
            CALL code_error ( 'fm35_sec1' , 'D...D < 3 letters' , arg)
            section_id = -1
         ELSE
            CALL code_table_DDDD ( arg2 )
         ENDIF
      ENDIF

   CASE ( 2 )
      CALL code_table_YYGG ( arg(1:4) , ival , wind_unit , cval , 'APPEND' )
      IF ( part .EQ. 'A' .OR. part .EQ. 'C' ) THEN
         CALL code_table_1734 ( arg(5:5),last_wind_lvl,rval,cval,'APPEND',part )
      ELSEIF ( part .EQ. 'B' ) THEN
         CALL code_table_0265 ( arg(5:5) , ival , rval , cval , 'APPEND' )
      ELSEIF ( part .EQ. 'D' ) THEN
         IF ( arg(5:5) .NE. '/' ) THEN
            CALL code_warn ( 'fm35_sec1', 'Expecting YYGG/, found', arg )
            print *, ' === "Unexpected, not YYGG/, check code ===  '
         ENDIF
      ELSE
         CALL code_error ( 'fm35_sec1', 'Unexpected Part : ', part )
         print *, ' === Unexpected part, check code ===  '
      ENDIF

   CASE ( 3 )
      IF ( record_fm .EQ. 35 ) THEN
         CALL code_table_stnid ( arg(1:5), ival, rval, cval, 'APPEND' )
         IF ( ival < 0 ) THEN
            CALL code_error ( 'fm35_sec1' , 'invalid Station ID' , arg )
            section_id = -1
         ELSE
            CALL finish_fm35_sec1
         ENDIF
      ELSE
         CALL code_table_LTLN ( arg , ival , rval , cval , 'APPEND' )
      ENDIF

   CASE ( 5 )
      ! position verifying group MMMUlaUlo , verification not applied
      ival = str2int ( arg(1:5) )
      CALL record_appendi ( ival , '2590:MMMLL:Pos. verify group - NOT USED' )
      IF ( record_fm .NE. 38 ) CALL finish_fm35_sec1

   CASE ( 6 )
      CALL code_table_hhhhim ( arg , ival , rval , cval , 'APPEND' )
      CALL finish_fm35_sec1

   CASE DEFAULT
      CALL code_error ( 'fm35_sec1' , 'unexpected' , arg )
      section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm35_sec1: out ', section_id

ENDSUBROUTINE decode_fm35_sec1

!----------------------------------------------------------------------

SUBROUTINE finish_fm35_sec1
   IMPLICIT NONE
   IF ( part .EQ. 'A' .OR. part .EQ. 'C' ) THEN
      CALL new_section ( 2 )
   ELSE
      CALL new_section ( 5 )
   ENDIF
   section_argnum = 0
   IF ( TRACE_ALL ) PRINT *,'TEMP : switching section ',record_fm,section_id
ENDSUBROUTINE finish_fm35_sec1

!----------------------------------------------------------------------

SUBROUTINE decode_fm35_sec2 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_sec2: in ', &
      section_argnum, section_subgrp, ' ', part, ' ', TRIM(arg)

   IF ( section_argnum .EQ. 1 .AND. part .EQ. 'A' ) THEN
      IF ( arg(1:2) .NE. '99' ) THEN
         CALL code_error ( 'fm35_sec2' , 'invalid 99PPP group' , arg )
         section_id = -1
      ELSE
         pressure = str2int ( arg(3:5) )
         IF ( pressure .EQ. UNDEFINED ) THEN
            CALL code_error (  'fm35_sec2', 'invalid PPP in 99PPP', arg )
            sttn_pre = -MISSING
         ELSE
            IF ( pressure < 100 ) pressure = 1000 + pressure
            sttn_pre = pressure
         ENDIF
         section_subgrp = 2
         CALL record_appendr ( sttn_pre , 0. , '99PP:99PPP:Station Pressure' )
      ENDIF
      RETURN
   ENDIF

   ck_nxtgroup : SELECT CASE ( section_subgrp )

      CASE ( 1 )
         IF ( arg(1:2) .EQ. '88' ) THEN
            section_subgrp = 5
            RETURN
         ENDIF
         CALL code_table_PPhhh ( arg,ival,rval,cval,'APPEND',pressure,part )
         IF ( pressure .EQ. UNDEFINED ) THEN
            section_subgrp = 5
         ELSE
            section_subgrp = 2
         ENDIF

      CASE ( 2 )
         IF ( arg .EQ. '/////' .AND. pressure > sttn_pre ) THEN
            ! for stations above 1000 mb, groups TTTDD and ddff for underground
            !    levels may be reported by ///// or ///// /////
            !
            CALL get_next_word_in_mesg ( cval )
            IF ( LEN_TRIM(cval) .eq. 0 ) THEN
               CALL push_word_back_to_mesg ( ETX )
               RETURN
            ELSE IF ( cval(1:5) .NE. '/////' ) THEN
               CALL push_word_back_to_mesg ( cval(1:5) )
            ELSE
               section_argnum = section_argnum + 1
            ENDIF
            section_subgrp = 1
         ELSE
            CALL code_table_TTTaDD ( arg, ival, rval, cval, 'APPEND', pressure )
            IF ( section_argnum .EQ. 2 .AND. part .EQ. 'A' ) THEN
               section_subgrp = 3
            ELSEIF ( last_wind_lvl .EQ. MISSING .OR. last_wind_lvl>pressure ) THEN
               ! it has been seen that a last_wind_lvl is code, but one continue
               ! to supply wind data as ///// above that level, in voliation of code
               ! format
               CALL get_next_word_in_mesg ( cval )
               IF ( LEN_TRIM(cval) .eq. 0 ) THEN
                  CALL push_word_back_to_mesg ( ETX )
                  RETURN
               ELSE IF ( cval(1:5) .EQ. '/////' ) THEN
                  CALL code_warn ( 'fm35_sec2', 'should have been omitted', cval(1:5) )
                  ! DO NOTHING (skipping it)
               ELSE
                  CALL push_word_back_to_mesg ( cval(1:5) )
               ENDIF
               section_subgrp = 1
            ELSE
               section_subgrp = 3
            ENDIF
         ENDIF

      CASE ( 3 )
         CALL code_table_ddfff(arg,ival,rval,cval,'APPEND',pressure,wind_unit)
         section_subgrp = 1

      CASE ( 5 )
         CALL code_warn ( 'fm35_sec2' , 'ignored' , arg )

      CASE DEFAULT
         CALL code_error ( 'fm35_sec2' , 'unexpected' , arg )

   ENDSELECT ck_nxtgroup

   IF ( TRACE_ALL ) PRINT *,'decode_fm35_sec2: out ', section_id

ENDSUBROUTINE decode_fm35_sec2

!----------------------------------------------------------------------

SUBROUTINE decode_fm35_sec3 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_sec3: in ', &
      section_argnum, section_id, TRIM(arg)

   SELECT CASE ( section_argnum )

      CASE ( 2 )
         CALL code_table_TTTaDD ( arg, ival, rval, cval, 'APPEND', pressure )

      CASE ( 3 )
         CALL code_table_ddfff(arg,ival,rval,cval,'APPEND',pressure,wind_unit)

      CASE DEFAULT
         CALL code_error ( 'fm35_sec3' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm35_sec3: out ', section_id

ENDSUBROUTINE decode_fm35_sec3

!----------------------------------------------------------------------

SUBROUTINE decode_fm35_sec4 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_sec4: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

      CASE ( 2 )
         CALL code_table_ddfff(arg,ival,rval,cval,'APPEND',pressure,wind_unit)

      CASE ( 3 )
         IF ( arg(1:1) .NE. '4' ) THEN
            CALL code_error ( 'fm35_sec4' , 'unexpected' , arg )
            section_id = -1
         ELSE
            ival = str2int ( arg(2:3) )
            IF ( ival > 0 ) THEN
               cval = '....:vava :Absolute vector wind dif. 1 km above max. wind level'
               rval = multiply ( ival , wind_unit )
               CALL record_appendr ( pressure , rval , cval )
            ENDIF
            ival = str2int ( arg(4:5) )
            IF ( ival > 0 ) THEN
               cval = '....:vbvb :Absolute vector wind dif. 1 km below max. wind level'
               rval = multiply ( ival , wind_unit )
               CALL record_appendr ( pressure , rval , cval )
            ENDIF
         ENDIF

      CASE DEFAULT
         CALL code_error ( 'fm35_sec4' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm35_sec4: out ', section_id

ENDSUBROUTINE decode_fm35_sec4

!----------------------------------------------------------------------

SUBROUTINE decode_fm35_sec5 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_sec5: in ', &
      section_subgrp, ' ', TRIM(arg)

   ck_nxtgroup : SELECT CASE ( section_subgrp )

      CASE ( 1 )
         pressure = str2int ( arg(3:5) )
         IF ( part .EQ. 'B' ) THEN
            IF ( pressure < 100 ) pressure = add ( 1000 , pressure )
            rval = pressure
         ELSEIF ( part .EQ. 'D' ) THEN
            rval = multiply ( pressure , 0.1 )
            pressure = divide ( pressure , 10 )
         ELSE
            CALL code_error ( 'fm35_sec1', 'Unexpected Part : ', part )
            print *, ' === Unexpected part, check code ===  '
         ENDIF
         CALL record_appendr ( pressure , rval , &
            arg(1:2) // '..:PTemp:Pressure at significant temperature level' )
         section_subgrp = 2

      CASE ( 2 )
         CALL code_table_TTTaDD ( arg, ival, rval, cval, 'APPEND', pressure )
         section_subgrp = 1

      CASE DEFAULT
         CALL code_error ( 'fm35_sec5' , 'unexpected' , arg )

   ENDSELECT ck_nxtgroup

   IF ( TRACE_ALL ) PRINT *,'decode_fm35_sec5: out ', section_id

ENDSUBROUTINE decode_fm35_sec5

!----------------------------------------------------------------------

SUBROUTINE decode_fm35_sec6 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_sec6: in ', &
      section_subgrp, ' ', TRIM(arg)

   ck_nxtgroup : SELECT CASE ( section_subgrp )

      CASE ( 1 )
         pressure = str2int ( arg(3:5) )
         IF ( part .EQ. 'B' ) THEN
            IF ( pressure < 100 ) pressure = add ( 1000 , pressure )
            rval = pressure
         ELSEIF ( part .EQ. 'D' ) THEN
            rval = multiply ( pressure , 0.1 )
            pressure = divide ( pressure , 10 )
         ELSE
            CALL code_error ( 'fm35_sec1', 'Unexpected Part : ', part )
            print *, ' === Unexpected part, check code ===  '
         ENDIF
         CALL record_appendr ( pressure , rval , &
            arg(1:2) // '..:PWind:Pressure at significant wind level' )
         section_subgrp = 2

      CASE ( 2 )
         CALL code_table_ddfff(arg,ival,rval,cval,'APPEND',pressure,wind_unit)
         section_subgrp = 1

      CASE DEFAULT
         CALL code_error ( 'fm35_sec6' , 'unexpected' , arg )

   ENDSELECT ck_nxtgroup

   IF ( TRACE_ALL ) PRINT *,'decode_fm35_sec6: out ', section_id

ENDSUBROUTINE decode_fm35_sec6

!----------------------------------------------------------------------

SUBROUTINE decode_fm35_sec7 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_sec7: in ', TRIM(arg)

   SELECT CASE ( arg(1:1) )

      CASE ( '0' : '7' )
!        skipping sr   : solar and infrared radiation correction
!        CALL code_table_3849 ( arg(1:1) , ival , rval , cval , ' ' ) ! sr
!        skipping rara : Radioonde/sounding system used
!        CALL code_table_3685 ( arg(2:3) , ival , rval , cval , ' ' ) ! rara
!        skipping sasa : Tracking technique / status of system used
!        CALL code_table_3872 ( arg(4:5) , ival , rval , cval , ' ' ) ! sasa

      CASE ( '8' )
         CALL code_table_GGgg ( arg(2:5) , ival , rval , cval , 'APPEND' )

      CASE ( '9' )
         CALL code_table_3845 ( arg(2:2) , ival , rval , cval , ' ' )
         rval = multiply ( 0.1 , multiply ( rval , str2int ( arg(3:5) ) ) )
         CALL record_appendr ( ival , rval , &
                            '3845:SST  :Sea Surface Temperature (C) ' )

      CASE DEFAULT
         CALL code_error ( 'fm35_sec7' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm35_sec7: out ', section_id

ENDSUBROUTINE decode_fm35_sec7

!----------------------------------------------------------------------

ENDSUBROUTINE decode_fm35
