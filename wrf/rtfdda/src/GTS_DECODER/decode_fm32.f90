!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

SUBROUTINE decode_fm32

   ! Driver for decoding FM-32 format

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival , ddhhmm
   REAL                     :: rval
   CHARACTER ( LEN = llen ) :: cval, arg

   CHARACTER ( LEN = 1 )    :: part
   REAL                     :: wind_unit
   INTEGER                  :: last_wind_lvl , num_subgrp , &
                               pressure , npre , altitude

   INTEGER , DIMENSION ( 14 ) , PARAMETER :: std_lvls = &
      (/ 850, 700, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10 /)

   IF ( TRACE_MOST ) PRINT  * , 'decode_fm32 : in '
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
            CALL decode_fm32_words ( arg )  ! decode the word
            IF ( section_id < 0 ) THEN
               ! serious error, STOP decoding this mesage
               CALL bulletin_skip ( MESG_DELIMITORS )
               EXIT loop_words
            ENDIF
         ENDIF

      ENDDO loop_words

      IF ( TRACE_ALL ) PRINT * , 'exit loop_mesgs'

   ENDDO loop_mesgs

   IF ( TRACE_ALL ) PRINT  * , 'decode_fm32 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE decode_fm32_words ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm32_words: in ', &
      TRIM(arg), ' ', section_id, section_argnum

   IF ( arg .EQ. 'NIL' .OR. arg .EQ. 'nil' .OR. arg .EQ. 'MIS' ) RETURN

   ! Mandatory groups are sequence indicated (oriented), must be decoded
   !   before numerial indicated groupts
   !
   IF ( section_id .EQ. 1 ) THEN
      CALL decode_fm32_sec1 ( arg ) ! Mandatory : FM-32 FM-33 FM-34
      RETURN

   ELSEIF ( section_id .EQ. 4 ) THEN
      CALL decode_fm32_sec4 ( arg )
      IF ( section_subgrp >= 1 ) RETURN

   ENDIF

   ! Mandatory groups finished, check for new section header
   SELECT CASE ( arg )

      CASE ( '21212' )
         IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 42 )
            RETURN
         ENDIF

      CASE ( '51515' , '52525' , '53535' , '54545' , '55555' , &
             '56565' , '57575' , '58585' , '59595' )
         IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 5 )
            RETURN
         ENDIF

      CASE ( '61616' , '62626' , '63636' , '64646' , '66666' , &
             '65656' , '67676' , '68686' , '69696' )
         IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 6 )
            RETURN
         ENDIF

      CASE ( 'PPAA' , 'PPBB' , 'PPCC' , 'PPDD' , &
             'QQAA' , 'QQBB' , 'QQCC' , 'QQDD' , &
             'EEAA' , 'EEBB' , 'EECC' , 'EEDD' )
         CALL new_section ( 1 )
         CALL decode_fm32_sec1 ( arg )
         RETURN

      CASE DEFAULT

         IF ( part .EQ. 'A' .OR. part .EQ. 'C' ) THEN

            IF ( arg(1:2) .EQ. '44' .OR. arg(1:2) .EQ. '55' ) THEN
               CALL new_section ( 2 )

            ELSEIF ( arg(1:2) .EQ. '77' .OR. arg(1:2) .EQ. '66' ) THEN
               pressure = str2int ( arg(3:5) )
               cval='....:'//arg(1:2)//'PPP:P @ max V level (mb)'
               IF ( pressure .EQ. 999 ) THEN
                  ! PRINT * , 'no maximum wind level information'
                  CALL record_appendi ( MISSING , cval )
               ELSEIF ( part .EQ. 'A' ) THEN
                  CALL record_appendi ( pressure , cval )
               ELSEIF ( part .EQ. 'C' ) THEN
                  rval = multiply ( pressure , 0.1 )
                  pressure = divide ( pressure , 10.0 )
                  CALL record_appendr ( pressure , rval , cval )
               ELSE
                  CALL code_error ( 'fm32_words', 'Unexpected Part : ', part )
                  print *, ' === Unexpected part, check code ===  '
               ENDIF
               CALL new_section ( 3 )
               RETURN

            ELSEIF ( arg(1:1) .EQ. '7' .OR. arg(1:1) .EQ. '6' ) THEN
               pressure = multiply ( 10 , str2int ( arg(2:5) ) )
               cval='....:'//arg(1:1)//'HHHH:Z @ max V (gpm)'
               CALL record_appendj ( minus(pressure) , pressure , cval )
               CALL new_section ( 3 )
               RETURN

            ENDIF

         ENDIF

   ENDSELECT

   ck_sections : SELECT CASE ( section_id )

      CASE ( 2 )     ; CALL decode_fm32_sec2  ( arg )

      CASE ( 3 )     ; CALL decode_fm32_sec3  ( arg )

      CASE ( 42 )    ; CALL decode_fm32_sec42 ( arg )

      CASE ( 5 : 6 ) ; ! skipping sections 5 and 6 (nationally developed)
         IF ( TRACE_MOST ) CALL code_ignore ( 'fm32_words' , &
            'skipping nationally developed group' , arg )

      CASE DEFAULT
         CALL code_error ( 'fm32_words' , 'unexpected' , arg )

   ENDSELECT ck_sections

   IF ( TRACE_MOST ) PRINT *,'decode_fm32_words: out ', &
      TRIM(arg), ' ', section_id, section_argnum

ENDSUBROUTINE decode_fm32_words

!----------------------------------------------------------------------

SUBROUTINE decode_fm32_sec1 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget , arg2

   IF ( TRACE_MOST ) PRINT *,'decode_fm32_sec1: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

   CASE ( 1 )
      part          = '?'
      wind_unit     = MISSING
      last_wind_lvl = MISSING
      num_subgrp    = MISSING
      pressure      = MISSING
      npre          = MISSING
      altitude      = MISSING
      SELECT CASE ( arg(1:2) )
      CASE ( 'PP' ) ; record_fm = 32
      CASE ( 'QQ' ) ; record_fm = 33
      CASE ( 'EE' ) ; record_fm = 34
      CASE DEFAULT
         CALL code_error ( 'fm32_sec1' , 'PILOT code header' , arg )
         section_id = -1
         RETURN
      ENDSELECT

      part = arg(3:3)
      IF ( part .lt. 'A' .OR. part .gt. 'D' ) THEN
         CALL code_error ( 'fm32_sec1' , 'PILOT part info' , arg )
         section_id = -1
         RETURN
      ENDIF
      CALL code_table_MMMM ( arg(1:4) , ival , rval , cval , 'APPEND' )
      cval = '....:MiMj :CODE HEADER ' // arg
      CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )

      IF ( record_fm .EQ. 33 .OR. record_fm .EQ. 34 ) THEN
         CALL get_next_word_in_mesg ( arg2 )   ! note no increase in argnum
         IF ( LEN_TRIM ( arg2 ) .lt. 3 ) THEN
            CALL code_error ( 'fm32_sec1' , 'D...D < 3 letters' , arg)
            section_id = -1
         ELSE
            CALL code_table_DDDD ( arg2 )
         ENDIF
      ENDIF

   CASE ( 2 )
      CALL code_table_YYGG ( arg(1:4) , ival , wind_unit , cval , 'APPEND' )
      CALL code_table_0265 ( arg(5:5) , ival , rval , cval , 'APPEND' )

   CASE ( 3 )
      IF ( record_fm .EQ. 32 ) THEN
         CALL code_table_stnid ( arg(1:5), ival, rval, cval, 'APPEND' )
         IF ( ival < 0 ) THEN
            CALL code_error ( 'fm32_sec1' , 'invalid Station ID' , arg )
            section_id = -1
         ELSE
            CALL finish_fm32_sec1
         ENDIF
      ELSE
         CALL code_table_LTLN ( arg , ival , rval , cval , 'APPEND' )
      ENDIF

   CASE ( 5 )
      ! position verifying group MMMUlaUlo , verification not applied
      ival = str2int ( arg(1:5) )
      CALL record_appendi ( ival , '2590:MMMLL:Pos. verify group - NOT USED' )
      IF ( record_fm .NE. 34 ) CALL finish_fm32_sec1

   CASE ( 6 )
      CALL code_table_hhhhim ( arg , ival , rval , cval , 'APPEND' )
      CALL finish_fm32_sec1

   CASE DEFAULT
      CALL code_error ( 'fm32_sec1' , 'unexpected' , arg )
      section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm32_sec1: out ', section_id

ENDSUBROUTINE decode_fm32_sec1

!----------------------------------------------------------------------

SUBROUTINE finish_fm32_sec1
   IMPLICIT NONE
   IF     ( part .EQ. 'A' .OR. part .EQ. 'C' ) THEN
      CALL new_section ( 2 )
   ELSEIF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
      CALL new_section ( 4 )
   ELSE
      CALL code_error ( 'fm32_sec1' , 'unexpected part', part )
      section_id = -1
      RETURN
   ENDIF
   section_argnum = 0
   IF ( TRACE_ALL ) PRINT *,'PILOT: switching section ',record_fm,section_id
ENDSUBROUTINE finish_fm32_sec1

!----------------------------------------------------------------------

SUBROUTINE decode_fm32_sec2 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm32_sec2: in ', &
      section_subgrp, ' ', TRIM(arg)

   ck_next_group : SELECT CASE ( section_subgrp )

      CASE ( 1 )
         IF ( arg(1:2) .EQ. '44' ) THEN
            cval = '....:44nPP:Pressure (mb) at Standard Level (Measured)'
         ELSEIF ( arg(1:2) .EQ. '55' ) THEN
            cval = '....:55nPP:Pressure (mb) at Standard Level (Estimated)'
         ELSE
            CALL code_error ( 'fm32_sec2' , 'expecting 44nPP or 55nPP' , arg )
            section_id = -1
            RETURN
         ENDIF

         num_subgrp = str2int ( arg(3:3) )
         IF ( num_subgrp < 1 .OR. num_subgrp > 3 ) THEN
            pressure = UNDEFINED
            CALL code_error ( 'fm32_sec2' , 'invalid n in nPP' , arg )
            section_id = -1
         ELSE
            IF ( part .EQ. 'A' ) THEN
               pressure = multiply ( 10 , str2int ( arg(4:5) ) )
            ELSEIF ( part .EQ. 'C' ) THEN
               pressure = str2int ( arg(4:5) )
            ELSE
               CALL code_error ( 'fm35_sec2', 'Unexpected Part : ', part )
               print *, ' === Unexpected part, check code ===  '
            ENDIF
            section_subgrp = -1
            find_std_lvl : DO npre = 1, 14
               IF ( std_lvls ( npre ) .EQ. pressure ) THEN
                  CALL record_appendi ( pressure , cval )
                  section_subgrp = 2
                  EXIT find_std_lvl
               ENDIF
            ENDDO find_std_lvl
            IF ( section_subgrp .EQ. -1 ) THEN
               CALL code_error ( 'fm32_sec2', 'invalid Std. Pre.', arg )
            ENDIF
         ENDIF

      CASE ( 2 : 4 )
         CALL code_table_ddfff(arg,ival,rval,cval,'APPEND',pressure,wind_unit)
         npre     = npre + 1
         pressure = std_lvls ( npre )
         IF ( section_subgrp .EQ. ( num_subgrp + 1 ) ) THEN
            section_subgrp = 1
         ELSE
            section_subgrp = section_subgrp + 1
         ENDIF

      CASE ( -1 )
         CALL code_warn ( 'fm32_sec2' , 'ignored' , arg )

      CASE DEFAULT
         CALL code_error ( 'fm32_sec2' , 'unexpected' , arg )

   ENDSELECT ck_next_group

   IF ( TRACE_ALL ) PRINT *,'decode_fm32_sec2: out ', section_id

ENDSUBROUTINE decode_fm32_sec2

!----------------------------------------------------------------------

SUBROUTINE decode_fm32_sec3 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm32_sec3: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

      CASE ( 2 )
         CALL code_table_ddfff(arg,ival,rval,cval,'APPEND',pressure,wind_unit)

      CASE ( 3 )
         IF ( arg(1:1) .NE. '4' ) THEN
            CALL code_error ( 'fm32_sec3' , 'unexpected' , arg )
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
         CALL code_error ( 'fm32_sec3' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm32_sec3: out ', section_id

ENDSUBROUTINE decode_fm32_sec3

!----------------------------------------------------------------------

SUBROUTINE decode_fm32_sec4 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget
   INTEGER , SAVE                        :: hmulti , hadd
   INTEGER , SAVE                        :: tn , u1 , u2 , u3

   IF ( TRACE_MOST ) PRINT *,'decode_fm32_sec4: in ', &
      section_subgrp, ' ', TRIM(arg)

   SELECT CASE ( section_subgrp )

      CASE ( 1 )
         IF ( arg(1:1) .EQ. '9' ) THEN
            hmulti = 300 ; hadd = 0
         ELSEIF ( arg(1:1) .EQ. '8' ) THEN
            hmulti = 500 ; hadd = 0
         ELSEIF ( arg(1:1) .EQ. '1' .AND. part .EQ. 'D' ) THEN
            hmulti = 300 ; hadd = 30000
         ELSE
            ! May not be section 4 afterall
            section_subgrp = -1
            RETURN
         ENDIF
         tn = str2int ( arg(2:2) )
         u1 = str2int ( arg(3:3) )
         u2 = str2int ( arg(4:4) )
         u3 = str2int ( arg(5:5) )
         section_subgrp = 2

      CASE ( 2 )
         IF ( u1 .EQ. MISSING ) THEN
            altitude = cur_sttn_alt
            cget = '....:tnuuu:Height (gpm) at Station level'
         ELSE
            altitude = add ( hadd , &
                    multiply ( hmulti , add ( u1 , multiply ( 10 , tn ) ) ) )
            cget = '....:tnuuu:Height (gpm) of significant level'
         ENDIF
         CALL record_appendj ( minus ( altitude ) , altitude , cget )
         CALL code_table_ddfff ( arg , ival , rval , cval , 'APPEND' , &
                                 minus ( altitude ) , wind_unit )
         IF ( u2 .EQ. MISSING ) THEN
            section_subgrp = 1
         ELSE
            section_subgrp = 3
         ENDIF

      CASE ( 3 )
         cget = '....:tnuuu:Height (gpm) of significant level'
         altitude = add ( hadd , &
                    multiply ( hmulti , add ( u2 , multiply ( 10 , tn ) ) ) )
         CALL record_appendj ( minus ( altitude ) , altitude , cget )
         CALL code_table_ddfff ( arg , ival , rval , cval , 'APPEND' , &
                                 minus ( altitude ) , wind_unit )
         IF ( u3 .EQ. MISSING ) THEN
            section_subgrp = 1
         ELSE
            section_subgrp = 4
         ENDIF

      CASE ( 4 )
         cget = '....:tnuuu:Height (gpm) of significant level'
         altitude = add ( hadd , &
                    multiply ( hmulti , add ( u3 , multiply ( 10 , tn ) ) ) )
         CALL record_appendj ( minus ( altitude ) , altitude , cget )
         CALL code_table_ddfff ( arg , ival , rval , cval , 'APPEND' , &
                                 minus ( altitude ) , wind_unit )
         section_subgrp = 1

      CASE DEFAULT
         CALL code_error ( 'fm32_sec4' , 'unexpected' , arg )

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm32_sec4: out ', section_id

ENDSUBROUTINE decode_fm32_sec4

!----------------------------------------------------------------------

SUBROUTINE decode_fm32_sec42 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm32_sec42: in ', &
      section_subgrp, ' ', TRIM(arg)

   ck_next_group : SELECT CASE ( section_subgrp )

      CASE ( 1 )
         pressure = str2int ( arg(3:5) )
         IF ( part .EQ. 'B' ) THEN
            IF ( pressure < 100 ) pressure = add ( pressure , 1000 )
            rval = pressure
         ELSEIF ( part .EQ. 'D' ) THEN
            rval = multiply ( pressure , 0.1 )
            pressure = divide ( pressure , 10.0 )
         ELSE
            CALL code_error ( 'fm32_sec42' , 'unexpected part' , part )
            section_id = -1
            RETURN
         ENDIF
         CALL record_appendr ( pressure , rval , &
            arg(1:2) // '..:PWind:Pressure at significant wind level' )
         section_subgrp = 2

      CASE ( 2 )
         CALL code_table_ddfff(arg,ival,rval,cval,'APPEND',pressure,wind_unit)
         section_subgrp = 1

      CASE DEFAULT
         CALL code_error ( 'fm32_sec42' , 'unexpected' , arg )

   ENDSELECT ck_next_group

   IF ( TRACE_ALL ) PRINT *,'decode_fm32_sec42: out ', section_id

ENDSUBROUTINE decode_fm32_sec42

!----------------------------------------------------------------------

ENDSUBROUTINE decode_fm32
