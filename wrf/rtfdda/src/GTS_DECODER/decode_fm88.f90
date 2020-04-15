!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

SUBROUTINE decode_fm88

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival , ddhhmm
   REAL                     :: rval
   CHARACTER ( LEN = llen ) :: cval, arg

   REAL                     :: wind_unit
   INTEGER                  :: npts , pressure , la0 , ln0 , lat_sgn , lng_sgn

   IF ( TRACE_MOST ) PRINT  * , 'decode_fm88 : in '
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
            CALL decode_fm88_words ( arg )  ! decode the word
            IF ( section_id < 0 ) THEN
               ! serious error, STOP decoding this mesage
               CALL bulletin_skip ( MESG_DELIMITORS )
               EXIT loop_words
            ENDIF
         ENDIF

      ENDDO loop_words

      IF ( TRACE_ALL ) PRINT * , 'exit loop_mesgs'

   ENDDO loop_mesgs

   IF ( TRACE_ALL ) PRINT  * , 'decode_fm88 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE decode_fm88_words ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm88_words: in ', &
      section_id, section_argnum, ' ', TRIM(arg)

   IF ( arg .EQ. 'NIL' .OR. arg .EQ. 'nil' .OR. arg .EQ. 'MIS' ) RETURN

   ! Mandatory groups are sequence indicated (oriented), must be decoded
   !   before numerial indicated groupts
   !
   IF ( section_id .EQ. 1 ) THEN
      CALL decode_fm88_sec1 ( arg )
      RETURN
   ENDIF

   ! Mandatory groups finished, check for new section header
   select_section_headers:SELECT CASE ( arg )

      CASE ( '222' )
         CALL new_section ( 2 )
         section_subgrp = 2
         RETURN

      CASE ( '333' , '444' , '555' , '666' , '777' , '888' )
         CALL new_section ( str2int ( arg(1:1) ) )
         RETURN

      CASE ( 'YYXX' )
         CALL new_section ( 1 )
         CALL decode_fm88_sec1 ( arg ) ! New SATOB report
         RETURN

      CASE DEFAULT
         IF ( section_argnum .EQ. 1 ) THEN
            CALL code_ignore ( 'fm88_words' , 'ignored' , arg )
            RETURN
         ENDIF

   ENDSELECT select_section_headers

   ck_sections : SELECT CASE ( section_id )

         CASE ( 2 ) ; CALL decode_fm88_sec2 ( arg )
         CASE ( 3 ) ; CALL decode_fm88_sec3 ( arg )
         CASE ( 4 ) ; CALL decode_fm88_sec4 ( arg )
         CASE ( 5 ) ; CALL decode_fm88_sec5 ( arg )
!        CASE ( 6 ) ; CALL decode_fm88_sec6 ( arg ) ! never seen sctn 6 yet
!        CASE ( 7 ) ; CALL decode_fm88_sec7 ( arg ) ! never seen sctn 7 yet
!        CASE ( 8 ) ; CALL decode_fm88_sec8 ( arg ) ! never seen sctn 8 yet
         CASE (6:8) ; CALL code_error ( 'fm88_words', 'to code' , arg )
            CALL code_error ( 'fm88_words', 'New to code new section : ', arg )
            print *, ' === Unexpected fm88 section, check code ===  '

      CASE DEFAULT
         CALL code_error ( 'fm88_words' , 'unexpected' , arg )

   ENDSELECT ck_sections

   IF ( TRACE_MOST ) PRINT *,'decode_fm88_words: out ', &
      TRIM(arg), ' ', section_id, section_argnum

ENDSUBROUTINE decode_fm88_words

!----------------------------------------------------------------------

SUBROUTINE decode_fm88_sec1 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget
   CHARACTER ( LEN = rlen ) , SAVE       :: arg2

   IF ( TRACE_MOST ) PRINT *,'decode_fm88_sec1: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

   CASE ( 1 )
      wind_unit = MISSING
      npts      = MISSING
      pressure  = MISSING
      la0       = MISSING
      ln0       = MISSING
      lat_sgn   = MISSING
      lng_sgn   = MISSING
      CALL code_table_MMMM ( arg(1:4) , ival , rval , cval , 'APPEND' )
      cval = '....:MiMj :CODE HEADER ' // arg
      CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )

   CASE ( 2 )

      ! Decode the YYMMJ and GGggwi group together
      CALL get_next_ngrp_in_mesg ( arg2 )

      IF ( LEN_TRIM (arg2) .EQ. 0 ) THEN
         call push_word_back_to_mesg ( ETX ) 

      ELSE IF ( arg2 .EQ. '238//' ) THEN
         ! this deviation from standard FM-88 is to taken into account
         ! of the SATOB reports of stallite 238// in the form of
         !   YYXX  29221 238//
         ! I am assuming a notation of
         !   YYXX  YYGG? I1I2I2//
         ! until more information is provided

         CALL code_table_YYGG ( arg(1:4) , ival , rval , cval , 'APPEND' )
         IF ( ival < 0 .OR. rval < 0 ) THEN
            CALL code_error ( 'fm88_sec1', 'invalid Obs. Time 238//', arg )
         ENDIF
         CALL code_table_SATID ( arg2 , ival, rval , cval , 'APPEND' )
         CALL new_section ( 4 ) ! always 444 follows
         section_argnum = 4

      ELSE
         ! standard FM-88
         ival = str2int ( arg(1:4) )
         IF ( ival > 5000 ) THEN
            wind_unit = KNOT
            CALL record_appendr ( ival , wind_unit , &
                                  '....:YYMMJ:Wind originally in knots' )
            ival = ival - 5000
         ELSE
            wind_unit = 1.0
         ENDIF
         ival = 10000 * ( INT(bul_year/10)*10 + str2int(arg(5:5)) ) &
                      + MOD(ival,100)*100 + INT(ival/100)
         IF ( ival > bul_yymmdd ) ival = ival - 10000

         IF ( arg2(1:4) .EQ. '////' ) THEN
            ! information relevant for the whole day
            CALL iset_msg_time ( 2359 )
         ELSE
            CALL iset_msg_time ( str2int(arg2(1:4)) )
         ENDIF

         ! make sure observation time not later than bullletin report time
         ! IF ( msg_hhmmss > bul_hhmmss .AND. ival >= bul_yymmdd ) THEN
         !    CALL decrease_date ( ival , msg_yymmdd )
         ! ELSE
         !    msg_yymmdd = ival
         ! ENDIF
         !
         ! The year month and day are specified by the message, no change
           msg_yymmdd = ival

         cval = 'SAT.:YMDHM:Observation Time (UTC) YYMMDD + HHMMSS'
         CALL record_appendj ( msg_yymmdd , msg_hhmmss , cval )
         IF ( msg_yymmdd < 0 .OR. msg_hhmmss < 0 ) THEN
            cval = arg // arg2
            CALL code_error ( 'fm88_sec1', 'invalid Obs. Time', cval )
            section_id = -1
         ENDIF

         CALL code_table_4639 ( arg2(5:5) , ival , rval , cval , 'APPEND' )
         section_argnum = 3

      ENDIF

   CASE ( 4 )
      CALL code_table_SATID ( arg , ival , rval , cval , 'APPEND' )
      CALL new_section ( 2 )
      section_argnum = 0

   CASE DEFAULT
      CALL code_error ( 'fm88_sec1' , 'unexpected' , arg )
      section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm88_sec1: out ', section_id

ENDSUBROUTINE decode_fm88_sec1

!----------------------------------------------------------------------

SUBROUTINE decode_fm88_sec2 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval
   INTEGER                               :: ival , iget , jval, lat, lng
   CHARACTER ( LEN = rlen )              :: cval , cget
   CHARACTER ( LEN = 5 ) , SAVE          :: arg2

   IF ( TRACE_MOST ) PRINT *,'decode_fm88_sec2: in ', &
      section_subgrp, ' ', TRIM(arg)

   SELECT CASE ( section_subgrp )

      CASE ( 2 ) ! BBBnn
         CALL code_table_BBB ( arg(1:3), ival, rval, cval, 'APPEND', &
                               la0, ln0, lat_sgn, lng_sgn )
         npts = str2int ( arg(4:5) )
         cval = 'SAT.:nn   :Number of pts to follow'
         CALL record_appendi ( npts , cval )
         IF ( npts < 0 .OR. abs(la0) > 90 .OR. abs(ln0) > 180 ) THEN
            CALL code_error ( 'fm88_sec2' , 'invalid BBBnn' , arg )
            CALL new_section ( 9 )
         ELSE
            section_subgrp = 3
         ENDIF

      CASE ( 3 ) ! UlaUloUlaUlo/
         arg2 = arg(1:5)
         section_subgrp = 4

      CASE ( 4 ) ! PcPcTcTcTa
         CALL code_satob_latlng ( arg2(1:2),la0,ln0,lat,lng,lat_sgn,lng_sgn )
         pressure = multiply ( 10 , str2int ( arg(1:2) ) )
         rval     = multiply ( 0.1, str2int ( arg(3:5) ) )
         IF ( SCAN ( '13579' , arg(5:5) ) > 0 ) rval = minus(rval)
         cval     = 'SAT.:Tc   :Temperature (C) of Cloud at specific pre. sfc'
         CALL record_appendr ( pressure , rval , cval )
         section_subgrp = 5

      CASE ( 5 ) ! ddfff
         CALL code_table_ddfff(arg,ival,rval,cval,'APPEND',pressure,wind_unit)
         npts = npts - 1
         IF ( npts .NE. 0 ) THEN
            IF ( arg2(3:3) .NE. '/' ) THEN
               arg2     = arg2(3:5) // '//'
               section_subgrp = 4
            ELSE
               section_subgrp = 3
            ENDIF
         ELSE
            section_subgrp = 2
         ENDIF

      CASE DEFAULT
         CALL code_error ( 'fm88_sec2' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm88_sec2: out ', section_id

ENDSUBROUTINE decode_fm88_sec2

!----------------------------------------------------------------------

SUBROUTINE decode_fm88_sec3 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval
   INTEGER                               :: ival , iget , jval, lat, lng
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm88_sec3: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

      CASE ( 2 )
         CALL code_table_BBB ( arg(1:3), ival, rval, cval, 'APPEND', &
                               la0, ln0, lat_sgn, lng_sgn )
         npts = str2int ( arg(4:5) )
         cval = 'SAT.:nn   :Number of pts to follow'
         CALL record_appendi ( npts , cval )
         IF ( npts < 0 .OR. abs(la0) > 90 .OR. abs(ln0) > 180 ) THEN
            CALL code_error ( 'fm88_sec3' , 'invalid BBBnn' , arg )
            CALL new_section ( 9 )
         ENDIF

      CASE ( 3: )
         IF ( section_argnum > (npts*2)+2 ) THEN
            CALL code_error ( 'fm88_sec3' , 'unexpected' , arg )
            CALL new_section ( 9 )
         ELSE
            IF ( mod ( section_argnum , 2 ) .EQ. 1 ) THEN
               CALL code_satob_latlng ( arg(1:2),la0,ln0,lat,lng,lat_sgn,lng_sgn )
               pressure = multiply ( 10 , str2int ( arg(3:4) ) )
            ELSE
               CALL code_table_ddfff ( arg , ival , rval , cval , 'APPEND' , &
                   pressure , wind_unit )
            ENDIF
            IF ( section_argnum .EQ. (npts*2)+2 ) THEN
               section_argnum = 1
            ENDIF
         ENDIF

      CASE DEFAULT
         CALL code_error ( 'fm88_sec3' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm88_sec3: out ', section_id

ENDSUBROUTINE decode_fm88_sec3

!----------------------------------------------------------------------

SUBROUTINE decode_fm88_sec4 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval
   INTEGER                               :: ival , iget , jval, lat, lng
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm88_sec4: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

      CASE ( 2 )
         CALL code_table_BBB ( arg(1:3), ival, rval, cval, 'APPEND', &
                               la0, ln0, lat_sgn, lng_sgn )
         npts = str2int ( arg(4:5) )
         cval = 'SAT.:nn   :Number of pts to follow'
         CALL record_appendi ( npts , cval )
         IF ( npts < 0 .OR. abs(la0) > 90 .OR. abs(ln0) > 180 ) THEN
            CALL code_error ( 'fm88_sec4' , 'invalid BBBnn' , arg )
            CALL new_section ( 9 )
         ENDIF

      CASE ( 3: )
         IF ( section_argnum > npts+2 ) THEN
            CALL code_error ( 'fm88_sec4' , 'unexpected' , arg )
            CALL new_section ( 9 )

         ELSE
            CALL code_satob_latlng ( arg(1:2),la0,ln0,lat,lng,lat_sgn,lng_sgn )

            rval = multiply ( 0.1 , str2int ( arg(3:5) ) )
            IF ( SCAN ( '13579' , arg(5:5) ) > 0 ) rval = minus(rval)
            cval = 'SAT.:Tsfc :Surface Temperature (C)'
            CALL record_appendr ( str2int(arg(1:5)) , rval , cval )

            IF ( section_argnum .EQ. npts+2 ) THEN
               section_argnum = 1
            ENDIF
         ENDIF

      CASE DEFAULT
         CALL code_error ( 'fm88_sec4' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm88_sec4: out ', section_id

ENDSUBROUTINE decode_fm88_sec4

!----------------------------------------------------------------------

SUBROUTINE decode_fm88_sec5 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval
   INTEGER                               :: ival , iget , jval, lat, lng
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm88_sec5: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

      CASE ( 2 )
         CALL code_table_BBB ( arg(1:3), ival, rval, cval, 'APPEND', &
                               la0, ln0, lat_sgn, lng_sgn )
         npts = str2int ( arg(4:5) )
         cval = 'SAT.:nn   :Number of pts to follow'
         CALL record_appendi ( npts , cval )
         IF ( npts < 0 .OR. abs(la0) > 90 .OR. abs(ln0) > 180 ) THEN
            CALL code_error ( 'fm88_sec5' , 'invalid BBBnn' , arg )
            CALL new_section ( 9 )
         ENDIF

      CASE ( 3: )
         IF ( section_argnum > (npts*2)+2 ) THEN
            CALL code_error ( 'fm88_sec5' , 'unexpected' , arg )
            CALL new_section ( 9 )
         ELSE
            IF ( mod ( section_argnum , 2 ) .EQ. 1 ) THEN
               CALL code_satob_latlng ( arg(1:2),la0,ln0,lat,lng,lat_sgn,lng_sgn )
               pressure = multiply ( 10 , str2int ( arg(3:4) ) )
            ELSE
               rval = str2int ( arg(1:2) )
               cval = 'SAT.:PP NN:Cloud Cover (%) at specific pre. sfc'
               CALL record_appendr ( pressure , rval , cval )
               rval = multiply ( 0.1 , str2int ( arg(3:5) ) )
               IF ( SCAN ( '13579' , arg(5:5) ) > 0 ) rval = minus(rval)
               cval = 'SAT.:PPTTT:Temperature (C) at specific pre. sfc'
               cval = 'SAT.:PPTTT:Cloud Top Temperature (C)'
               CALL record_appendr ( pressure , rval , cval )
            ENDIF
            IF ( section_argnum .EQ. (npts*2)+2 ) THEN
               section_argnum = 1
            ENDIF
         ENDIF

      CASE DEFAULT
         CALL code_error ( 'fm88_sec5' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm88_sec5: out ', section_id

ENDSUBROUTINE decode_fm88_sec5

!----------------------------------------------------------------------

ENDSUBROUTINE decode_fm88
