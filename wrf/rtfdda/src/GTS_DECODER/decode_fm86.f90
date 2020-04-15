!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

SUBROUTINE decode_fm86

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER , EXTERNAL       :: approx_height
   INTEGER                  :: ival , ddhhmm
   REAL                     :: rval
   CHARACTER ( LEN = llen ) :: cval, arg

   CHARACTER ( LEN = 1 )    :: part
   REAL                     :: ref_pre
   INTEGER                  :: nlayers , pressure

   IF ( TRACE_MOST ) PRINT  * , 'decode_fm86 : in '
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
            CALL decode_fm86_words ( arg )  ! decode the word
            IF ( section_id < 0 ) THEN
               ! serious error, STOP decoding this mesage
               CALL bulletin_skip ( MESG_DELIMITORS )
               EXIT loop_words
            ENDIF
         ENDIF

      ENDDO loop_words

      IF ( TRACE_ALL ) PRINT * , 'exit loop_mesgs'

   ENDDO loop_mesgs

   IF ( TRACE_ALL ) PRINT  * , 'decode_fm86 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE decode_fm86_words ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm86_words: in ', &
      TRIM(arg), ' ', section_id, section_argnum

   IF ( arg .EQ. 'NIL' .OR. arg .EQ. 'nil' .OR. arg .EQ. 'MIS' ) RETURN

   ! Mandatory groups are sequence indicated (oriented), must be decoded
   !   before numerial indicated groupts
   !
   IF ( section_id .EQ. 1 ) THEN
      CALL decode_fm86_sec1 ( arg )
      RETURN
   ENDIF

   ! Mandatory groups finished, check for new section header
   select_section_headers:SELECT CASE ( arg )

      CASE ( '222' , '333' , '444' , '555' , '666' , '777' )
         CALL new_section ( str2int ( arg(1:1) ) )
         RETURN

      CASE ( 'VVAA' , 'VVBB' , 'VVCC' , 'VVDD' )
         CALL new_section ( 1 )
         CALL decode_fm86_sec1 ( arg ) ! New SATEM report
         RETURN

   ENDSELECT select_section_headers

   ck_sections : SELECT CASE ( section_id )

         CASE ( 2 ) ; CALL decode_fm86_sec2 ( arg )
         CASE ( 3 ) ; CALL decode_fm86_sec3 ( arg )
         CASE ( 4 ) ; CALL decode_fm86_sec4 ( arg )
         CASE ( 5 ) ; CALL decode_fm86_sec5 ( arg )
         CASE ( 6 ) ; CALL decode_fm86_sec6 ( arg )
         CASE ( 7 ) ; CALL decode_fm86_sec7 ( arg )

      CASE DEFAULT
         CALL code_error ( 'fm86_words' , 'unexpected' , arg )

   ENDSELECT ck_sections

   IF ( TRACE_MOST ) PRINT *,'decode_fm86_words: out ', &
      TRIM(arg), ' ', section_id, section_argnum

ENDSUBROUTINE decode_fm86_words

!----------------------------------------------------------------------

SUBROUTINE decode_fm86_sec1 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget , arg2

   IF ( TRACE_MOST ) PRINT * , 'decode_fm86_sec1: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )

   CASE ( 1 )
      part     = '?'
      ref_pre  = MISSING
      nlayers  = MISSING
      pressure = MISSING
      SELECT CASE ( arg(1:2) )
      CASE ( 'VV' ) ; record_fm = 86
      CASE DEFAULT
         CALL code_error ( 'fm86_sec1' , 'SATEM code header' , arg )
         section_id = -1
         RETURN
      ENDSELECT

      part = arg(3:3)
      IF ( part .lt. 'A' .OR. part .gt. 'D' ) THEN
         CALL code_error ( 'fm86_sec1', 'Unexpected Part : ', arg )
         print *, ' === Unexpected part, check code ===  '
         section_id = -1
         RETURN
      ENDIF
      CALL code_table_MMMM ( arg(1:4) , ival , rval , cval , 'APPEND' )
      cval = '....:MiMj :CODE HEADER ' // arg
      CALL record_appendj ( bul_yymmdd, bul_hhmmss, cval )

   CASE ( 2 )
      CALL code_table_YYGG ( arg(1:4) , ival , rval , cval , 'APPEND' )

   CASE ( 3 )
      CALL code_table_SATID ( arg , ival , rval , cval , 'APPEND' )
      CALL new_section ( 2 ) ; section_argnum = 0

   CASE DEFAULT
      CALL code_error ( 'fm86_sec1' , 'unexpected' , arg )
      section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm86_sec1: out ', section_id

ENDSUBROUTINE decode_fm86_sec1

!----------------------------------------------------------------------

SUBROUTINE decode_fm86_sec2 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT * , 'decode_fm86_sec2: in ', &
      section_argnum, ' ', TRIM(arg)

   SELECT CASE ( section_argnum )
      CASE ( 2 )
         CALL code_table_QLaLo ( arg , ival , rval , cval , 'APPEND' )

      CASE ( 3 )
         IF ( part .EQ. 'A' .OR. part .EQ. 'B' )  THEN
            CALL code_table_NNPPP ( arg , ival , rval , cval , 'APPEND' )
         ELSE
            CALL code_error ( 'fm86_sec2', 'Unexpected Part : ', arg )
            print *, ' === Unexpected part, check code ===  '
         ENDIF

      CASE DEFAULT
         CALL code_error ( 'fm86_sec2' , 'unexpected' , arg )

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm86_sec2: out ', section_id

ENDSUBROUTINE decode_fm86_sec2

!----------------------------------------------------------------------

SUBROUTINE decode_fm86_sec3 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , pmulti
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT * , 'decode_fm86_sec3: in ', &
      section_argnum, ' ', TRIM(arg)

   IF     ( part .EQ. 'A' ) THEN
      pmulti = 10.
   ELSEIF ( part .EQ. 'C' ) THEN
      pmulti = 0.1
   ELSE
      CALL code_error ( 'fm86_sec2', 'Unexpected Part : ', arg )
      print *, ' === Unexpected part, check code ===  '
      section_id = -1
   ENDIF

   SELECT CASE ( section_argnum )

      CASE ( 2 ) ! PAPAnLnLq
         nlayers = str2int ( arg(3:4) )
         rval = multiply ( str2int ( arg(5:5) ) , 10. )
         cval = '....:nnq  :Number of layers and Confidence of data (%)'
         CALL record_appendr ( nlayers , rval , cval )

         CALL code_table_PP ( arg(1:2) , ival , ref_pre , cval , ' ' )
         ref_pre  = multiply ( ref_pre , pmulti )
         pressure = nint ( ref_pre )
         cval     = 'SAT0:PP000:Ref. Pressure (hPa) for SATEM observations'
         CALL record_appendr ( minus(approx_height(pressure)), ref_pre, cval )
         IF ( nlayers < 0 .OR. ref_pre < 0 ) section_id = -1

      CASE ( 3: )
         IF ( section_argnum > nlayers+2 ) THEN
            CALL code_error ( 'fm86_sec3' , 'unexpected' , arg )
            print *, ' === Unexpected section_argnum, check code ===  '
            section_id = -1
         ENDIF
         CALL code_table_PP ( arg(1:2) , ival , rval , cval , ' ' )
         rval = multiply ( pmulti , rval )
         CALL record_appendr ( section_argnum - 2 , rval , cval )

         iget = multiply ( 10. , str2int ( arg(3:5) ) )
         CALL modify_thickness ( iget, pressure, nint(rval), 10000 )
!        IF ( pressure .EQ. 1000 ) THEN
!           CALL modify_height_by_stdpre ( nint(rval) , iget , 10000 )
!        ELSE
!           ! *******************************************************
!           ! modification required ( thousands digits omitted )
!           ! *******************************************************
!           CALL code_warn ( 'fm86_sec3' , 'ref pre <> 1000' , arg )
!        ENDIF
         cval = 'SAT.:TL   :Thickness (gpm) between std. pre. sfcs'
         CALL record_appendr ( iget , rval , cval )

      CASE DEFAULT
         CALL code_error ( 'fm86_sec3' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm86_sec3: out ', section_id

ENDSUBROUTINE decode_fm86_sec3

!----------------------------------------------------------------------

SUBROUTINE decode_fm86_sec4 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , pmulti
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT * , 'decode_fm86_sec4: in ', &
      section_argnum, ' ', TRIM(arg)

   IF ( part .EQ. 'A' ) THEN
      pmulti = 10.
   ELSE
      CALL code_error ( 'fm86_sec4', 'Unexpected Part : ', arg )
      print *, ' === Unexpected part, check code ===  '
      section_id = -1
   ENDIF

   SELECT CASE ( section_argnum )

      CASE ( 2 )
         nlayers = str2int ( arg(3:4) )
         rval = multiply ( 10. , str2int ( arg(5:5) ) )
         cval = '....:nnq  :Number of layers and Confidence of data (%)'
         CALL record_appendr ( nlayers , rval , cval )

         CALL code_table_PP ( arg(1:2) , ival , ref_pre , cval , ' ' )
         ref_pre       = multiply ( pmulti , ref_pre )
         pressure      = nint ( ref_pre )
         cval ( 1:10 ) = 'SAT0:PP000'
         CALL record_appendr ( nlayers, ref_pre, TRIM(cval) // ' Ref. level' )
         IF ( nlayers < 0 .OR. ref_pre < 0 ) section_id = -1

      CASE ( 3: )
         IF ( section_argnum > nlayers+2 ) THEN
            CALL code_error ( 'fm86_sec4' , 'unexpected' , arg )
            print *, ' === Unexpected section_argnum, check code ===  '
            section_id = -1
         ENDIF
         CALL code_table_PP ( arg(1:2) , ival , rval , cval , ' ' )
         rval = multiply ( pmulti , rval )
         CALL record_appendr ( section_argnum - 2 , rval , cval )

         iget = str2int ( arg(3:5) )
         cval = 'SAT.:PWatr:Precip. Water (mm) between std. pre. sfcs'
         CALL record_appendr ( iget , rval , cval )

      CASE DEFAULT
         CALL code_error ( 'fm86_sec4' , 'unexpected' , arg )
         section_id = -1

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm86_sec4: out ', section_id

ENDSUBROUTINE decode_fm86_sec4

!----------------------------------------------------------------------

SUBROUTINE decode_fm86_sec5 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT * , 'decode_fm86_sec5: in ', &
      section_argnum, ' ', TRIM(arg)

   IF ( part .NE. 'A' .AND. part .NE. 'B' ) THEN
      CALL code_error ( 'fm86_sec5', 'Unexpected Part : ', arg )
      print *, ' === Unexpected part, check code ===  '
   ENDIF

   SELECT CASE ( section_argnum )
      CASE ( 2 )
         IF ( arg(1:3) .NE. '///' ) THEN
            CALL code_table_3845 ( arg(1:1) , ival , rget , cval , ' ' )
            rval = multiply ( rget , str2int ( arg(2:3) ) )
            cval = 'SAT.:SFC_T:Surface Temperature (C)'
            CALL record_appendr ( ival , rval , cval )
         ENDIF
         IF ( arg(4:5) .NE. '00' ) THEN
            ! Tropopause temperature should be negative
            ival = minus ( str2int ( arg(4:5) ) )
            cval = 'SAT.:SFC_T:Tropopause Temperature (C)'
            CALL record_appendi ( ival , cval )
         ENDIF

      CASE ( 3 )
         ival = str2int ( arg(1:3) )
         rval = ival
         IF ( rval > 500. ) THEN
            rval = multiply ( 0.1 , rval )
         ELSEIF ( rval < 20. ) THEN
            rval = multiply ( 10. , rval )
         ENDIF
         cval = 'SAT.:PTrop:Tropopause Pressure (mb)'
         CALL record_appendr ( ival , rval , cval )

         ival = str2int ( arg(4:4) )
         IF ( ival > 0 ) THEN
            rval = multiply ( 10 , ival )
         ELSE
            rval = ival
         ENDIF
         cval = 'SAT.:ETrop:Standard Error of Tropopause Pressure (mb)'
         CALL record_appendr ( ival , rval , cval )

      CASE DEFAULT
         CALL code_error ( 'fm86_sec5' , 'unexpected' , arg )

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm86_sec5: out ', section_id

ENDSUBROUTINE decode_fm86_sec5

!----------------------------------------------------------------------

SUBROUTINE decode_fm86_sec6 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT * , 'decode_fm86_sec6: in ', &
      section_argnum, ' ', TRIM(arg)

   IF ( part .NE. 'B' .AND. part .NE. 'D' ) THEN
      CALL code_error ( 'fm86_sec6', 'Unexpected Part : ', arg )
      print *, ' === Unexpected part, check code ===  '
   ENDIF

   SELECT CASE ( section_argnum )
      CASE ( 2 )
         cval = 'skipping mean temp. between non standard layers'
         CALL code_error ( 'fm86_sec6' , cval , arg )

      CASE DEFAULT
         cval = 'skipping mean temp. between non standard layers'
         CALL code_error ( 'fm86_sec6' , cval , arg )

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm86_sec6: out ', section_id

ENDSUBROUTINE decode_fm86_sec6

!----------------------------------------------------------------------

SUBROUTINE decode_fm86_sec7 ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget , jval
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT * , 'decode_fm86_sec7: in ', &
      section_argnum, ' ', TRIM(arg)

   IF ( part .NE. 'B' ) THEN
      CALL code_error ( 'fm86_sec7', 'Unexpected Part : ', arg )
      print *, ' === Unexpected part, check code ===  '
   ENDIF

   SELECT CASE ( section_argnum )
      CASE ( 2 )
         cval = 'skipping mean precip. water between non standard layers'
         CALL code_error ( 'fm86_sec7' , cval , arg )

      CASE DEFAULT
         cval = 'skipping mean precip. water between non standard layers'
         CALL code_error ( 'fm86_sec7' , cval , arg )

   ENDSELECT

   IF ( TRACE_ALL ) PRINT *,'decode_fm86_sec7: out ', section_id

ENDSUBROUTINE decode_fm86_sec7

!----------------------------------------------------------------------

ENDSUBROUTINE decode_fm86
