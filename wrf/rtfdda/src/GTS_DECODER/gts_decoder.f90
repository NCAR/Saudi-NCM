MODULE bulletin_def

   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   INCLUDE 'inc.bulletin'

   INTEGER , PARAMETER         :: llen = 80
   TYPE lines
      CHARACTER ( LEN = llen ) :: text
      TYPE ( lines ) , POINTER :: next_line
   ENDTYPE lines

   ! The whole PROGRAM builds on the idea of a 'current' bulletin,
   !   identified by the global TARGET <bulletin>, the end of which is
   !   identified by the POINTER <bulletin_tail>. The number of lines
   !   in the bulletin is saved in <bulletin_nlines>
   !
   TYPE ( lines ) , TARGET     :: bulletin
   TYPE ( lines ) , POINTER    :: bulletin_tail
   INTEGER                     :: bulletin_nlines
   INTEGER                     :: bulletin_allocate = 0
   INTEGER , PARAMETER         :: bulletin_maxlines = 500

   INTEGER                     :: bulletin_error , bulletin_warn  
   INTEGER , PARAMETER         :: number = 1 , error = 2 , warning = 3
   INTEGER , DIMENSION (100,3) :: bul_stat = 0

   ! During decoding, <current_line> points to the line in the bulletin
   !    that is currently being looked at. Normally, the word that is
   !    being examined is saved in a local variable <arg>, while the
   !    text in the <current_line> still remained to be decoded is saved
   !    in the global variable <remaining_text>.
   !
   TYPE ( lines ) , POINTER    :: current_line
   CHARACTER ( LEN = llen )    :: remaining_text
   !
   INTEGER :: bul_year , bul_month , bul_day   , bul_yymmdd
   INTEGER :: bul_hour , bul_minute, bul_second, bul_hhmmss

! Modified Mar 16 2000 - SLN
!     Changed the ROHK identifier to ATEC
! Modified Aug 12 2010 - FCV
!     Changed the ATEC identifier to NOAA

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE bulletin_initialize

   ! Routine to create a new bulletin. ROHK convention, each new
   !   bulletin is headed by a 'SOH' CHARACTER (and ended with
   !   a 'ETX' character, see also bulletin_append.)
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE

   IF ( TRACE_ALL ) PRINT  * , 'bulletin_initialize : in'

   NULLIFY ( bulletin%next_line )

   bulletin_tail => bulletin
   bulletin_nlines = 0

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_initialize : out'

ENDSUBROUTINE bulletin_initialize

!----------------------------------------------------------------------

SUBROUTINE bulletin_deallocate

   ! Routine to deallocate space used by the current bulletin
   !
   ! Created : Dec 29, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   TYPE ( lines ) , POINTER :: bprev, btmp

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_deallocate : in  ', bulletin_allocate

   IF ( ASSOCIATED ( bulletin%next_line ) ) THEN
      bprev => bulletin%next_line
      DO WHILE ( ASSOCIATED ( bprev%next_line ) )
         btmp  => bprev
         bprev => bprev%next_line
         DEALLOCATE ( btmp ) 
         bulletin_allocate = bulletin_allocate - 1
      END DO
      DEALLOCATE ( bprev )
      bulletin_allocate = bulletin_allocate - 1
   ENDIF

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_deallocate : out ', bulletin_allocate

ENDSUBROUTINE bulletin_deallocate

!----------------------------------------------------------------------

SUBROUTINE bulletin_append ( line )

   ! SUBROUTINE to append a <line> to the current bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: line
   TYPE ( lines ) , POINTER              :: btmp
   CHARACTER ( LEN = llen )              :: firstpart, remaining

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_append : in ', TRIM(line)

   remaining = line  ! make a copy of the input line
                 ! remaining will be broken up into parts subsequently

   DO WHILE ( LEN_TRIM ( remaining ) > 0 )

      ! make sure every bulletin ends with a mesg_separator
      IF ( TRIM(line) .EQ. ETX ) THEN
         IF ( bulletin_nlines .NE. 0 ) THEN
            bulletin_tail%text = TRIM(bulletin_tail%text) // MESG_SEPARATORS(1:1)
         ENDIF
      ENDIF

      ! check for mesg_separator; if found, break line into two parts
      CALL parse_line ( remaining , MESG_SEPARATORS , firstpart , remaining )
      IF ( LEN_TRIM ( remaining ) > 0 ) THEN
         firstpart = TRIM ( firstpart ) // MESG_SEPARATORS(1:1)
         remaining = ADJUSTL ( remaining )
      ENDIF

      ! append the first part to the bulletin
      ALLOCATE ( btmp )
      bulletin_allocate = bulletin_allocate + 1

      btmp%text = firstpart
      NULLIFY ( btmp%next_line )

      bulletin_tail%next_line => btmp
      bulletin_tail           => btmp

      bulletin_nlines   = bulletin_nlines + 1

   ENDDO

   IF ( TRACE_ALL ) PRINT  * , 'bulletin_append : out'

ENDSUBROUTINE bulletin_append

!----------------------------------------------------------------------

SUBROUTINE bulletin_print ( iounit )

   ! routine to PRINT out the current bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: iounit
   TYPE ( lines ) , POINTER :: btmp

   IF ( TRACE_ALL ) PRINT  * , 'bulletin_print :  in ', iounit

   IF ( iounit < 1 ) THEN
      PRINT *, 'BULLETIN PRINT IOUNIT < 0, IGNORE WRITE ', iounit
      RETURN
   ENDIF

   btmp => bulletin

   ! PRINT every line including the end of bulletin ETX symbol
   DO WHILE ( ASSOCIATED ( btmp ) )
      IF ( ASSOCIATED ( btmp%next_line ) ) THEN
         WRITE ( iounit , '(a)' ) TRIM (btmp%text)//CR//CR
      ELSE
         ! alexis gill =============================================
         !   would be nice to have the following, but appears not to
         !   work on some implementations
         ! alexis gill =============================================
         ! WRITE ( iounit , '(a)' , ADVANCE='NO' ) TRIM (btmp%text)
         WRITE ( iounit , '(a)' ) TRIM (btmp%text)
      ENDIF
      btmp => btmp%next_line
   ENDDO

   ! flush output during error diagnostic / testing runs
!  IF ( TRACE_LEAST ) CALL flush ( iounit )

   IF ( TRACE_ALL ) PRINT  * , 'bulletin_print : out'

ENDSUBROUTINE bulletin_print

!----------------------------------------------------------------------

SUBROUTINE bulletin_skip ( delimitors )

   ! Routine to skip a number of line in the current bulletin.
   !
   ! Typical Usage:
   !
   ! (1) When the decoder encounter an error condition
   !    in the current bulletin, it will USE <bulletin_skip> to
   !    skip forward to the beginning of the next message in the
   !    current bulletin. For this, the <delimitors> supplied
   !    should be a message delimitor ('=$').
   !
   ! (2) This routine can also be used to skip the entire bulletin
   !    when the error is so bad that we cannot CONTINUE anymore.
   !    For this, the <delimitors> should be <ETX>.
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   CHARACTER ( LEN = * )    :: delimitors

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_skip : in ', current_line%text

   find_delimitors : DO WHILE ( ASSOCIATED ( current_line ) )

      IF ( TRACE_MOST ) THEN
         IF ( delimitors .EQ. ETX ) THEN
            PRINT * , ' SKIPPING ENTIRE BULLETIN : ', TRIM ( current_line%text )
         ELSE
            PRINT * , ' SKIPPING A MESSAGE : ',       TRIM ( current_line%text )
         ENDIF
      ENDIF

      IF ( SCAN ( current_line%text , delimitors // ETX ) > 0 ) THEN
         IF ( TRIM ( current_line%text ) .NE. ETX ) THEN
            current_line => current_line%next_line
         ENDIF
         EXIT find_delimitors
      ELSE
         current_line => current_line%next_line
      ENDIF

   ENDDO find_delimitors

   remaining_text = current_line%text

   IF ( TRACE_MOST ) PRINT  * , 'bulletin_skip : out ', current_line%text

ENDSUBROUTINE bulletin_skip

!----------------------------------------------------------------------

RECURSIVE SUBROUTINE get_next_word_in_mesg ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( OUT ) :: arg

   IF ( TRACE_ALL ) PRINT  *,'get_next_word_in_mesg : in ',TRIM(remaining_text)

   CALL parse_line ( remaining_text , SPACE // MESG_DELIMITORS , &
                     arg , remaining_text )

   IF ( LEN_TRIM(arg) .EQ. 0 ) THEN
      ! argument length = 0 , check end of message, if not, advance line
      IF ( SCAN ( current_line%text , MESG_DELIMITORS ) > 0 ) THEN
         current_line   => current_line%next_line
         remaining_text =  current_line%text
         RETURN
      ELSE
         current_line   => current_line%next_line
         remaining_text =  current_line%text
         CALL get_next_word_in_mesg ( arg )
      ENDIF

   ! **** begin non-standard ****
   ELSEIF ( LEN_TRIM(arg) .GE. 10 ) THEN
      ! handle some case where two words got merged into one
      remaining_text = TRIM ( arg(6:) ) // ' ' // remaining_text
      arg            = arg(1: 5)
   ! ****  end  non-standard ****

   ENDIF

   IF ( TRACE_MOST ) PRINT  *,'get_next_word_in_mesg : out ',TRIM(arg), &
                              ' <<<>>> ',TRIM(remaining_text)

ENDSUBROUTINE get_next_word_in_mesg

!----------------------------------------------------------------------

RECURSIVE SUBROUTINE get_next_ngrp_in_mesg ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( OUT ) :: arg
   INTEGER                               :: narg

   IF ( TRACE_MOST ) PRINT  * , 'get_next_ngrp_in_mesg : in'

   CALL get_next_word_in_mesg ( arg ) 

   IF ( VERIFY ( TRIM(arg) , NUMBER_SET ) .EQ. 0 ) RETURN
   IF ( TRIM(arg) .EQ. 'NIL' )                     RETURN
   IF ( LEN_TRIM(arg) .EQ. 4 ) THEN
      IF ( arg(3:4) .EQ. 'AA' ) RETURN
      IF ( arg(3:4) .EQ. 'BB' ) RETURN
      IF ( arg(3:4) .EQ. 'CC' ) RETURN
      IF ( arg(3:4) .EQ. 'DD' ) RETURN
      IF ( arg(1:4) .EQ. 'YYXX' ) RETURN
   ENDIF

   CALL code_ignore ( 'get_next_ngrp' , 'non-numeric ****' , arg )
   IF ( SCAN ( ERROR_INDICATOR , arg(LEN_TRIM(arg):LEN_TRIM(arg)) ) .EQ. 0 ) THEN
      IF ( VERIFY ( TRIM(arg) , ALPHANUMERIC_SET ) .NE. 0 ) THEN
         PRINT * , '*** : Non-alphanumeric group found >>> ', TRIM(arg), ' <<< kept'
      ENDIF
      PRINT * , 'Warning : Non-numeric group found >>> ', TRIM(arg), ' <<< kept'
   ELSE
      PRINT * , 'Warning : Non-numeric group found >>> ', TRIM(arg), ' <<< skip'
      CALL get_next_ngrp_in_mesg ( arg )
   ENDIF
   
   IF ( TRACE_MOST ) PRINT  * , 'get_next_ngrp_in_mesg : out ' , TRIM(arg)

ENDSUBROUTINE get_next_ngrp_in_mesg

!---------------------------------------------------------------------- 

SUBROUTINE push_word_back_to_mesg ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg

   IF ( TRACE_ALL ) PRINT  * , 'push_word_back_to_mesg : in ' , TRIM(arg)

   remaining_text = TRIM(arg) // ' ' // remaining_text

   IF ( TRACE_MOST ) PRINT  * , 'push_word_back_to_mesg : out ' , TRIM(remaining_text)

ENDSUBROUTINE push_word_back_to_mesg

!----------------------------------------------------------------------

ENDMODULE bulletin_def

!======================================================================


MODULE nan_arithmetic

   INTERFACE remainder
      FUNCTION remainder_ii ( val1 , val2 ) RESULT ( val )
         INTEGER , INTENT(in)  :: val1
         INTEGER , INTENT(in)  :: val2
         INTEGER               :: val
      ENDFUNCTION
      FUNCTION remainder_rr ( val1 , val2 ) RESULT ( val )
         REAL , INTENT(in)     :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: val
      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE minus
      FUNCTION minus_i ( val1 ) RESULT ( val )
         INTEGER , INTENT(in)  :: val1
         INTEGER               :: val
      ENDFUNCTION
      FUNCTION minus_r ( val1 ) RESULT ( val )
         REAL , INTENT(in)     :: val1
         REAL                  :: val
      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE add
      FUNCTION add_ii ( val1 , val2 ) RESULT ( val )
         INTEGER , INTENT(in)  :: val1
         INTEGER , INTENT(in)  :: val2
         INTEGER               :: val
      ENDFUNCTION
      FUNCTION add_ir ( val1 , val2 ) RESULT ( val )
         INTEGER , INTENT(in)  :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: val
      ENDFUNCTION
      FUNCTION add_ri ( val1 , val2 ) RESULT ( val )
         REAL , INTENT(in)     :: val1
         INTEGER , INTENT(in)  :: val2
         REAL                  :: val
      ENDFUNCTION
      FUNCTION add_rr ( val1 , val2 ) RESULT ( val )
         REAL , INTENT(in)     :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: val
      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE subtract
      FUNCTION subtract_ii ( val1 , val2 ) RESULT ( val )
         INTEGER , INTENT(in)  :: val1
         INTEGER , INTENT(in)  :: val2
         INTEGER               :: val
      ENDFUNCTION
      FUNCTION subtract_ir ( val1 , val2 ) RESULT ( val )
         INTEGER , INTENT(in)  :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: val
      ENDFUNCTION
      FUNCTION subtract_ri ( val1 , val2 ) RESULT ( val )
         REAL , INTENT(in)     :: val1
         INTEGER , INTENT(in)  :: val2
         REAL                  :: val
      ENDFUNCTION
      FUNCTION subtract_rr ( val1 , val2 ) RESULT ( val )
         REAL , INTENT(in)     :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: val
      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE multiply
      FUNCTION multiply_ii ( val1 , val2 ) RESULT ( prod )
         INTEGER , INTENT(in)  :: val1
         INTEGER , INTENT(in)  :: val2
         INTEGER               :: prod
      ENDFUNCTION
      FUNCTION multiply_ir ( val1 , val2 ) RESULT ( prod )
         INTEGER , INTENT(in)  :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: prod
      ENDFUNCTION
      FUNCTION multiply_ri ( val1 , val2 ) RESULT ( prod )
         REAL , INTENT(in)     :: val1
         INTEGER , INTENT(in)  :: val2
         REAL                  :: prod
      ENDFUNCTION
      FUNCTION multiply_rr ( val1 , val2 ) RESULT ( prod )
         REAL , INTENT(in)     :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: prod
      ENDFUNCTION
   ENDINTERFACE

   !----------------------------------------------------------------------

   INTERFACE divide
      FUNCTION divide_ii ( val1 , val2 ) RESULT ( prod )
         INTEGER , INTENT(in)  :: val1
         INTEGER , INTENT(in)  :: val2
         INTEGER               :: prod
      ENDFUNCTION
      FUNCTION divide_ir ( val1 , val2 ) RESULT ( prod )
         INTEGER , INTENT(in)  :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: prod
      ENDFUNCTION
      FUNCTION divide_ri ( val1 , val2 ) RESULT ( prod )
         REAL , INTENT(in)     :: val1
         INTEGER , INTENT(in)  :: val2
         REAL                  :: prod
      ENDFUNCTION
      FUNCTION divide_rr ( val1 , val2 ) RESULT ( prod )
         REAL , INTENT(in)     :: val1
         REAL , INTENT(in)     :: val2
         REAL                  :: prod
      ENDFUNCTION
   ENDINTERFACE

!----------------------------------------------------------------------

ENDMODULE nan_arithmetic

!======================================================================


MODULE record_def

   USE nan_arithmetic

   IMPLICIT NONE

   INCLUDE 'inc.traceflow'
   INCLUDE 'inc.special_symbols'

! ----------------------------------------------------------------------

   INTEGER , PARAMETER         :: rlen = 80

   INTEGER                     :: outfiles(1000) = UNDEFINED
   INTEGER                     :: current_unit   = 10

   TYPE stack
      INTEGER                  :: field_ival
      REAL                     :: field_rval
      CHARACTER ( LEN = rlen ) :: field_cval
      TYPE ( stack ) , POINTER :: next_record
   ENDTYPE stack

   TYPE ( stack ) , TARGET     :: record
   TYPE ( stack ) , POINTER    :: record_tail

   INTEGER                     :: record_allocate = 0
   INTEGER                     :: record_error  , ierr  , ierr0  = 3
   INTEGER                     :: record_warn   , iwarn , iwarn0 = 4
   INTEGER                     :: record_fm      ! current code form number
   INTEGER                     :: section_id     ! current section number
   INTEGER                     :: section_argnum ! current argument number
   INTEGER                     :: section_subgrp ! current subgroup number

   INTEGER                     :: cur_sttn_id    ! current station id
   INTEGER                     :: cur_sttn_alt   ! current station height

   INTEGER , PARAMETER         :: number = 1 , error = 2 , warning = 3
   INTEGER , DIMENSION (100,3) :: msg_stat = 0
   INTEGER                     :: seqnum   = -1

   CHARACTER ( LEN = rlen )    :: msg_header
   CHARACTER ( LEN = rlen )    :: prev
   !
   ! For simplicity , all decoded value is pushed onto a stack with
   !   appropriate comment. The first record in the stack will be
   !   the code form identification number ( fm ) .
   !   Routines can subsequently be developed to output the decoded
   !   values in manners suitable for specific applications. This
   !   stack implementation effectively separates the decoding part
   !   of the PROGRAM ( the core ) from its output routines.

   INTEGER , PARAMETER          :: stn_data = 10
   INTEGER , PARAMETER          :: stn_llen = 100
   CHARACTER ( LEN = 6 )        :: stn_fmt = '(A100)'
   CHARACTER ( LEN = stn_llen ) :: stn_dscrptn
   CHARACTER ( LEN = rlen )     :: RECFMT = ' ( i8 , 1x , f12.3 , 1x , a ) '

   ! Current Date and Time ( in file 'current_time' )
   !
   INTEGER                      :: utc_year , utc_month , utc_day
   INTEGER                      :: utc_hour , utc_minute, utc_second
   INTEGER                      :: utc_yymmdd , utc_hhmmss
   !
   INTEGER                      :: msg_year , msg_month , msg_day
   INTEGER                      :: msg_hour , msg_minute, msg_second
   INTEGER                      :: msg_yymmdd , msg_hhmmss

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE record_initialize ( fm )

   IMPLICIT NONE
   INTEGER , INTENT ( IN ) :: fm

   IF ( TRACE_MOST ) PRINT  * , 'record_initialize : in ' , fm

   record%field_cval = '0000:fm   :CODE FORM'
   record%field_ival = fm
   record%field_rval = fm
   NULLIFY  ( record%next_record )

   record_tail    => record

!  msg_stat ( fm , number ) = msg_stat ( fm , number ) + 1
   record_error   =  0
   record_warn    =  0

   record_fm      =  fm
   ierr           =  outfiles ( fm+200 )
   iwarn          =  outfiles ( fm+300 )
   section_id     =  0
   section_argnum =  0
   section_subgrp =  1
   prev           =  NULL

   IF ( TRACE_ALL ) PRINT  * , 'record_initialize : out', fm, ierr, iwarn

ENDSUBROUTINE record_initialize

!----------------------------------------------------------------------

SUBROUTINE record_deallocate

   ! Routine to deallocate space used by the current record
   !
   ! Created : Dec 29, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   TYPE ( stack ) , POINTER :: rprev, rtmp

   IF ( TRACE_MOST ) PRINT  * , 'record_deallocate   : in  ', record_allocate

   IF ( ASSOCIATED ( record%next_record ) ) THEN
      rprev => record%next_record
      DO WHILE ( ASSOCIATED ( rprev%next_record ) )
         rtmp  => rprev
         rprev => rprev%next_record
         DEALLOCATE ( rtmp ) 
         record_allocate = record_allocate - 1
      END DO
      DEALLOCATE ( rprev )
      record_allocate = record_allocate - 1
   ENDIF

   IF ( TRACE_MOST ) PRINT  * , 'record_deallocate   : out ', record_allocate

ENDSUBROUTINE record_deallocate

!----------------------------------------------------------------------

SUBROUTINE record_appendr ( ival , rval , cval )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )               :: ival
   REAL , INTENT ( IN )                  :: rval
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: cval
   TYPE ( stack ) , POINTER              :: rtmp

   IF ( TRACE_MOST ) PRINT  *, 'record_append : in ', ival, rval, TRIM(cval)

   ALLOCATE ( rtmp )
   record_allocate = record_allocate + 1
   rtmp%field_ival = ival
   rtmp%field_rval = rval
   rtmp%field_cval = cval
   NULLIFY ( rtmp%next_record )

   record_tail%next_record => rtmp
   record_tail             => rtmp

   IF ( TRACE_ALL ) PRINT  * , 'record_append : out'

ENDSUBROUTINE record_appendr

! ----------------------------------------------------------------------

SUBROUTINE record_appendi ( ival , cval )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )               :: ival
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: cval

   CALL record_appendr ( ival , REAL ( ival ) , cval )

ENDSUBROUTINE record_appendi

! ----------------------------------------------------------------------

SUBROUTINE record_appendj ( ival , jval , cval )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )               :: ival
   INTEGER , INTENT ( IN )               :: jval
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: cval

   CALL record_appendr ( ival , REAL ( jval ) , cval )

ENDSUBROUTINE record_appendj

!----------------------------------------------------------------------

SUBROUTINE record_print ( iounit )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: iounit
   INTEGER :: ival, rval
   LOGICAL :: large_ival, large_rval
   TYPE ( stack ) , POINTER :: rtmp

   IF ( TRACE_ALL ) PRINT  * , 'record_print : in', iounit
   IF ( iounit < 1 ) THEN
      PRINT *, 'RECORD PRINT IOUNT < 0, IGNORE WRITE ', iounit
      RETURN
   ENDIF

   rtmp => record

   WRITE ( iounit , ' ( a ) ' ) ' ============================================ '

   DO WHILE ( ASSOCIATED ( rtmp ) )
      WRITE ( iounit , RECFMT ) &
            rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )

      ival = rtmp%field_ival 
      rval = nint(rtmp%field_rval)
      large_rval = ( ABS(rval)*10 .GE. abs(MISSING) )
      large_ival = ( ABS(ival)*10 .GE. abs(MISSING) )

      IF ( ( large_ival .AND. ival.NE.MISSING .AND. ival.NE.UNDEFINED ) .OR. &
           ( large_rval .AND. rval.NE.MISSING .AND. rval.NE.UNDEFINED ) ) THEN
      IF ( ival.LE.19950000 .OR. ival.GE.21000000 ) THEN
      IF ( rtmp%field_cval(2:9) .NE. '002:BHDR' ) THEN
      IF ( rtmp%field_cval(2:9) .NE. '...:Abnn' ) THEN
         PRINT *, ival, rtmp%field_rval, ' ===> ', TRIM(rtmp%field_cval)
      ENDIF
      ENDIF
      ENDIF
      ENDIF

      rtmp => rtmp%next_record
   ENDDO

!  IF ( TRACE_LEAST ) CALL flush ( iounit )

   IF ( TRACE_ALL ) PRINT  * , 'record_print : out'

ENDSUBROUTINE record_print

!----------------------------------------------------------------------

SUBROUTINE assign_outfile ( unit_prefix )

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: unit_prefix
   INTEGER             :: temp_prefix

   IF ( TRACE_ALL ) PRINT  * , 'assign_outfile : in ', unit_prefix, record_fm
   temp_prefix = unit_prefix

   if ( temp_prefix .EQ. 0 ) then
      CALL open_unit ( record_fm )
   ENDIF

   DO WHILE ( temp_prefix .GT. 0 )
      CALL open_unit ( record_fm+100*MOD(temp_prefix,10) )
      temp_prefix = temp_prefix / 10
   ENDDO

ENDSUBROUTINE assign_outfile

!----------------------------------------------------------------------

SUBROUTINE open_unit ( unitnum )

   IMPLICIT NONE
   INTEGER , PARAMETER     :: MAXRECL = 1024
   INTEGER , INTENT ( IN ) :: unitnum
   CHARACTER               :: outfile_name*80
   LOGICAL                 :: existed

   IF ( outfiles ( unitnum ) .GT. 0 ) THEN
      ! unit already opened
      RETURN

   ELSE IF ( outfiles ( unitnum ) .EQ. 0 ) THEN
      ! unit number in error
      CALL prog_abort ( 'Calling OPEN_UNIT with 0 as unitnum' )

   ELSE IF ( outfiles ( unitnum ) .EQ. UNDEFINED ) THEN
      ! unit number is new
      current_unit      = current_unit + 1
      outfiles(unitnum) = current_unit

   ELSE
      ! old unit number that has been closed
      outfiles(unitnum) = -outfiles(unitnum)

   ENDIF

   WRITE ( outfile_name , 10 ) unitnum
   INQUIRE ( FILE=outfile_name, EXIST=existed )

   IF ( existed ) THEN
      OPEN ( UNIT=outfiles(unitnum), FILE=outfile_name, FORM='FORMATTED', &
             RECL=MAXRECL, ACCESS='SEQUENTIAL', STATUS='OLD', POSITION='APPEND')
   ELSE
      OPEN ( UNIT=outfiles(unitnum), FILE=outfile_name, FORM='FORMATTED', &
             RECL=MAXRECL, ACCESS='SEQUENTIAL', STATUS='NEW' )
   ENDIF

   IF     ( unitnum .GE. 200 .AND. unitnum .LE. 299 ) THEN
      ierr  = outfiles(unitnum)
   ELSEIF ( unitnum .GE. 300 .AND. unitnum .LE. 399 ) THEN
      iwarn = outfiles(unitnum)
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'open_unit : out ', &
      unitnum, current_unit, outfiles(unitnum), ' ', TRIM(outfile_name)

10 FORMAT ( 'gts_out.' , I3.3 )

ENDSUBROUTINE open_unit

!----------------------------------------------------------------------

SUBROUTINE flush_outfiles

   IMPLICIT NONE
   INTEGER :: temp_prefix, unitnum

   IF ( TRACE_ALL ) PRINT  * , 'flush_outfiles : in ', record_fm

   DO temp_prefix = 0, 900, 100
      unitnum = outfiles ( record_fm + temp_prefix )
      IF ( unitnum > 0 ) THEN
         CLOSE ( unitnum )
         outfiles ( record_fm + temp_prefix ) = -unitnum
      ENDIF
   ENDDO

ENDSUBROUTINE flush_outfiles

!----------------------------------------------------------------------

SUBROUTINE new_section ( id )

   IMPLICIT NONE
   INTEGER , INTENT ( IN ) :: id

   IF ( TRACE_ALL ) PRINT  * , 'new_section : in  ', section_id, id

   section_id     = id
   section_argnum = 1
   section_subgrp = 1
   prev           = NULL

   IF ( TRACE_ALL ) PRINT  * , 'new_section : out ', section_id, id

ENDSUBROUTINE new_section

!----------------------------------------------------------------------

FUNCTION int2str ( val )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: val
   CHARACTER ( LEN = rlen ) :: int2str
   int2str = BLANK_LINE
   WRITE ( int2str , '(I10)' ) val
   int2str = adjustl ( int2str )

ENDFUNCTION int2str

!----------------------------------------------------------------------

FUNCTION real2str ( val )

   IMPLICIT NONE
   REAL , INTENT ( IN )     :: val
   CHARACTER ( LEN = rlen ) :: real2str
   real2str = BLANK_LINE
   WRITE ( real2str , '(g16.6)' ) val
   real2str = adjustl ( real2str )

ENDFUNCTION real2str

! ----------------------------------------------------------------------

FUNCTION str2int ( string )

   ! FUNCTION to RETURN the INTEGER value of a string of numeric
   !    character.  If the string contains non numeric characters, the
   !    value RETURN by the FUNCTION is <UNDEFINED>, as defined in the
   !    include file 'inc.special_symbols'
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: string
   INTEGER                               :: str2int
   CHARACTER ( LEN = 4 )                 :: fmt
   CHARACTER ( LEN = len ( string ) )    :: tmp_string

   IF ( VERIFY ( TRIM(string) , NUMBER_SET ) .NE. 0 ) GOTO 10

   fmt = '(I' // ACHAR ( IACHAR ( '0' ) + LEN ( string ) ) // ')'
   tmp_string = ADJUSTR ( string )
   READ ( tmp_string , fmt , ERR = 10 ) str2int
   RETURN

10 CONTINUE
   IF ( string .EQ. repeat ( '/' , len ( string ) ) ) THEN
      str2int = MISSING
   ELSE
      str2int = UNDEFINED
   ENDIF

ENDFUNCTION str2int

!----------------------------------------------------------------------

ENDMODULE record_def

!======================================================================

MODULE mm5obs_def

   INCLUDE 'inc.mm5obs'     ! Part of Dave Hart's observation module

   TYPE mm5_hdr
      TYPE ( location_type ) :: location
      TYPE ( source_info   ) :: info
      TYPE ( time_info     ) :: valid_time
      TYPE ( terrestrial   ) :: ground
   END TYPE mm5_hdr

   TYPE ( mm5_hdr   )        :: empty_hdr
   TYPE ( meas_data )        :: empty_upa

   REAL , PARAMETER :: fake_stnpre = 101301.00

   INTEGER :: obs_yymmdd , obs_hhmmss , rev_yymmdd , rev_hhmmss
   INTEGER :: fm           ! FM-xx  WMO format number
   INTEGER :: nvalid       ! number of fields decoded
   INTEGER :: nlevel       ! number of vertical level written

   ! mainly used in sounding data
   INTEGER :: vert_id      ! indicate whether data is pressure
                           !    (+ve) or height (-ve) based
   LOGICAL :: wrote_hdr    ! whether header has been written out

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE init_mm5wr_emptyobs

   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'

   TYPE ( location_type ) :: empty_location
   TYPE ( source_info   ) :: empty_info
   TYPE ( time_info     ) :: empty_time
   TYPE ( field         ) :: empty_field
   TYPE ( terrestrial   ) :: empty_ground

   rev_yymmdd               = MISSING
   rev_hhmmss               = MISSING

   empty_location%latitude  = MISSING
   empty_location%longitude = MISSING
   empty_location%id        = BLANK_LINE
   empty_location%name      = BLANK_LINE

   empty_info%platform      = BLANK_LINE
   empty_info%source        = 'GTS (NOAA)'
   empty_info%elevation     = MISSING
   empty_info%num_vld_fld   = MISSING
   empty_info%num_error     = MISSING
   empty_info%num_warning   = MISSING
   empty_info%seq_num       = MISSING
   empty_info%num_dups      = MISSING

   empty_info%is_sound      = .FALSE.    ! need to discuss with Dave
   empty_info%bogus         = .FALSE.    ! need to discuss with Dave
   empty_info%discard       = .FALSE.    ! need to discuss with Dave
  
   empty_time%sut           = MISSING
   empty_time%julian        = MISSING
   empty_time%date_char     = BLANK_LINE

   empty_field%data         = MISSING
   empty_field%qc           = 0          ! need to discuss with Dave

   empty_ground%slp         = empty_field
   empty_ground%ref_pres    = empty_field
   empty_ground%ground_t    = empty_field
   empty_ground%sst         = empty_field
   empty_ground%psfc        = empty_field
   empty_ground%precip      = empty_field
   empty_ground%t_max       = empty_field
   empty_ground%t_min       = empty_field
   empty_ground%t_min_night = empty_field  
   empty_ground%p_tend03    = empty_field  
   empty_ground%p_tend24    = empty_field  
   empty_ground%cloud_cvr   = empty_field
   empty_ground%ceiling     = empty_field

   empty_upa%pressure       = empty_field
   empty_upa%height         = empty_field
   empty_upa%temperature    = empty_field
   empty_upa%dew_point      = empty_field
   empty_upa%speed          = empty_field
   empty_upa%direction      = empty_field
   empty_upa%u              = empty_field
   empty_upa%v              = empty_field
   empty_upa%rh             = empty_field
   empty_upa%thickness      = empty_field

   empty_hdr%location       = empty_location
   empty_hdr%info           = empty_info
   empty_hdr%valid_time     = empty_time
   empty_hdr%ground         = empty_ground

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE write_rpt_hdr ( iwrite , rpt_seq_num, hdr , ieor )

   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   INTEGER ,          INTENT ( IN )    :: iwrite, rpt_seq_num
   TYPE ( mm5_hdr ) , INTENT ( INOUT ) :: hdr 
   INTEGER ,          INTENT ( OUT )   :: ieor

   CHARACTER                           :: cdate*14

   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_rpt_hdr iounit < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   IF ( nvalid .LE. 0 ) THEN
      ! print *, ' **** write_rpt_hdr : NO valid field'
      ieor = 1

   ELSE IF ( rev_yymmdd .EQ. UNDEFINED .OR. rev_hhmmss .EQ. UNDEFINED ) THEN
      print *, ' **** write_rpt_hdr : UNEXPECTED rev_yymmdd IS UNDEFINED'
      ieor = 2

   ELSE IF ( rev_yymmdd .EQ. MISSING .OR. rev_hhmmss .EQ. MISSING ) THEN
      print *, ' **** write_rpt_hdr : UNEXPECTED rev_yymmdd IS MISSING'
      ieor = 2

   ELSE IF ( nint ( hdr%location%latitude  ) .EQ. MISSING   .OR.  &
             nint ( hdr%location%longitude ) .EQ. MISSING ) THEN
      ! print *, ' **** write_rpt_hdr : latitude or longitude IS MISSING'
      ieor = 3

   ELSE
      SELECT CASE ( fm ) 
         CASE (12) ; hdr%info%platform = 'FM-12 SYNOP'
                     hdr%info%is_sound = .FALSE.
         CASE (13) ; hdr%info%platform = 'FM-13 SHIP '
                     hdr%info%is_sound = .FALSE.
         CASE (32) ; hdr%info%platform = 'FM-32 PILOT'
                     hdr%info%is_sound = .TRUE.
         CASE (33) ; hdr%info%platform = 'FM-33 PILOT SHIP'
                     hdr%info%is_sound = .TRUE.
         CASE (34) ; hdr%info%platform = 'FM-34 PILOT MOBIL'
                     hdr%info%is_sound = .TRUE.
         CASE (35) ; hdr%info%platform = 'FM-35 TEMP'
                     hdr%info%is_sound = .TRUE.
         CASE (36) ; hdr%info%platform = 'FM-36 TEMP SHIP'
                     hdr%info%is_sound = .TRUE.
         CASE (37) ; hdr%info%platform = 'FM-37 TEMP DROP'
                     hdr%info%is_sound = .TRUE.
         CASE (38) ; hdr%info%platform = 'FM-38 TEMP MOBIL'
                     hdr%info%is_sound = .TRUE.
         CASE (86) ; hdr%info%platform = 'FM-86 SATEM'
                     hdr%info%is_sound = .TRUE.
         CASE (88) ; hdr%info%platform = 'FM-88 SATOB'
                     hdr%info%is_sound = .FALSE.
         CASE (97) ; hdr%info%platform = 'FM-97 AIREP'
                     hdr%info%is_sound = .TRUE.
      END SELECT
   
      write ( cdate , '(I8.8,I6.6)' ) rev_yymmdd, rev_hhmmss
!     rpt_seq_num = rpt_seq_num + 1
   
      hdr%info%seq_num         = rpt_seq_num
      hdr%info%num_vld_fld     = nvalid
      hdr%valid_time%date_char = cdate
   
      WRITE ( iwrite , fmt = rpt_format ) hdr
      ieor = 0
      nlevel = 0
      wrote_hdr = .TRUE.
      vert_id = MISSING

   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE write_rpt_upa ( iwrite , upa , ieor )

   IMPLICIT NONE
   INTEGER ,            INTENT ( IN ) :: iwrite , ieor
   TYPE ( meas_data ) , INTENT ( IN ) :: upa

   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_rpt_upa iounit < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   IF ( ieor .EQ. 0 ) THEN
      WRITE ( iwrite , fmt = meas_format ) upa
      nlevel = nlevel + 1
   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE write_rpt_end ( iwrite , nerror , nwarn , ieor )

   IMPLICIT NONE
   INTEGER , INTENT ( IN )  :: iwrite, nerror, nwarn, ieor
   TYPE ( meas_data )       :: upa_tmp

   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_rpt_end iounit < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   IF ( ieor .EQ. 0 ) THEN
      upa_tmp                  = empty_upa
      upa_tmp%pressure%data    = end_data
      upa_tmp%height%data      = end_data
      upa_tmp%temperature%data = nlevel
      WRITE ( iwrite , fmt = meas_format ) upa_tmp
      WRITE ( iwrite , fmt = end_format ) nvalid, nerror, nwarn
      wrote_hdr = .FALSE.
   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

ENDMODULE mm5obs_def

PROGRAM gts_decoder

   ! main PROGRAM driver
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER :: inunit = 8

   CALL prog_initialization ( inunit )

   main_loop : DO

      CALL read_a_bulletin ( inunit )
   PRINT *, '----Main:   read '

      IF ( bulletin_nlines < 0 ) THEN
         EXIT main_loop
      ELSE
        CALL identify_code_form
        CALL interpret_bulletin
      ENDIF

      CALL bulletin_deallocate
      CALL record_deallocate

   ENDDO main_loop

   CALL print_statistics ( 2 )

   PRINT *, 'SUCCESSFUL COMPLETION OF DECODER FOR ', utc_yymmdd, utc_hhmmss
   STOP 99999

ENDPROGRAM gts_decoder

!----------------------------------------------------------------------

SUBROUTINE prog_initialization ( inunit )

   USE bulletin_def
   USE record_def
   USE mm5obs_def

   IMPLICIT NONE
   INTEGER , INTENT ( IN ) :: inunit
   INTEGER , EXTERNAL      :: set_century

   OPEN ( inunit , FILE = 'gts_data' , STATUS = 'OLD' )

   READ ( 5, '(4X,5I2)', END=99, ERR=99 ) &
      utc_year, utc_month, utc_day, utc_hour, utc_minute
      utc_second = 0

   PRINT *, ' cutoff time input : ', utc_year, utc_month, utc_day, utc_hour
   utc_year   = set_century ( utc_year )
   utc_yymmdd = utc_year * 10000 + utc_month * 100 + utc_day
   utc_hhmmss = utc_hour * 10000 + utc_minute* 100 + utc_second
   PRINT *, ' cutoff time : ', utc_yymmdd, utc_hhmmss

   OPEN ( stn_data , FILE = 'gts_sttnid_final' , FORM = 'FORMATTED' , &
          ACCESS = 'DIRECT' , RECL = stn_llen )

   OPEN ( UNIT=2,      FILE='gts_out.002', FORM='FORMATTED', POSITION='APPEND' )
   OPEN ( UNIT=ierr0,  FILE='gts_out.003', FORM='FORMATTED', POSITION='APPEND' )
   OPEN ( UNIT=iwarn0, FILE='gts_out.004', FORM='FORMATTED', POSITION='APPEND' )

   ! initialize the basic bulletin contents
   !
   bulletin%text = SOH

   ! initialize empty observation for output in mm5wr
   CALL init_mm5wr_emptyobs
 
   RETURN

99 CALL prog_abort ( 'ERROR in GETTING CUTOFF TIME' )

ENDSUBROUTINE prog_initialization

!----------------------------------------------------------------------

SUBROUTINE prog_abort ( arg )

   USE bulletin_def
   USE record_def
   USE mm5obs_def

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg

   PRINT *, TRIM ( arg )
   PRINT *, '*** UNSUCCESSFUL RUN OF DECODER FOR ', utc_yymmdd, utc_hhmmss
   STOP 88888

ENDSUBROUTINE prog_abort

!----------------------------------------------------------------------

   ! Decoding Tables referenced by the decoder
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

!----------------------------------------------------------------------

MODULE bound_checks

   USE record_def
   REAL , PARAMETER :: &
      lmt_sfc_temp_1w(4) = (/ -40., -30., 50., 55. /) , &
      lmt_sfc_temp_2w(4) = (/ -90., -80., 35., 40. /) , &
      lmt_sfc_temp_1s(4) = (/ -30., -20., 50., 60. /) , &
      lmt_sfc_temp_2s(4) = (/ -40., -30., 40., 50. /) 

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE bound_check ( value , limits , suspect , error )
   IMPLICIT NONE
   REAL , INTENT ( INOUT )             :: value
   REAL , INTENT ( IN )                :: limits ( 4 )
   LOGICAL , INTENT ( OUT ) , OPTIONAL :: suspect
   LOGICAL , INTENT ( OUT ) , OPTIONAL :: error
   LOGICAL                             :: err , spct

   err  = .FALSE.
   spct = .TRUE.
   IF ( value < limits(1) .OR. value > limits(4) ) THEN
      err   = .TRUE.
      value = UNDEFINED
   ELSEIF ( value <= limits(2) .OR. value >= limits(3) ) THEN
      spct = .TRUE.
   ENDIF

   IF ( PRESENT ( suspect ) ) suspect = spct
   IF ( PRESENT ( error   ) ) error   = err

ENDSUBROUTINE

!----------------------------------------------------------------------

ENDMODULE bound_checks

!======================================================================

SUBROUTINE code_airep_ltln ( arg , ival , rval , cval , append ) ;
   USE record_def
   USE bulletin_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'

   INTEGER                  :: iget
   CHARACTER ( len = rlen ) :: arg1, arg2 

   arg1 = arg
   iget = SCAN ( TRIM(arg), 'SN' )
   IF ( iget .EQ. 0 ) THEN
      CALL code_error ( 'airep_ltln', 'missing S or N', arg )
      section_id = -1
      RETURN
   ELSE IF ( iget .EQ. LEN_TRIM ( arg ) ) THEN
      CALL get_next_word_in_mesg ( arg2 )
      IF ( LEN_TRIM (arg2) .EQ. 0 ) THEN
         call push_word_back_to_mesg ( ETX ) 
         CALL code_error ( 'airep_ltln', 'missing Longitute', arg )
         section_id = -1
         RETURN
      ENDIF
   ELSE
      arg2 = arg ( iget+1: )
      arg1 = arg ( :iget   )
   ENDIF
      
   iget = len_trim(arg1)
   IF ( SCAN ( 'NSns' , arg1(iget:iget) ) .EQ. 0 ) THEN
      ival = UNDEFINED
      section_id = -1
   ELSE 
      ival = str2int ( arg1 ( :iget-1 ) )
      if ( ival .LT. 0 ) section_id = -1
   ENDIF
   IF ( section_id .NE. -1 ) THEN
      IF ( iget .LE. 3 ) THEN
         rval = ival
      ELSE
         rval = ( ival / 100 ) + MOD ( ival , 100 ) / 60.0
      ENDIF
      IF ( arg1(iget:iget) .EQ. 'S' .OR. arg1(iget:iget) .EQ. 's' ) THEN
         ival = minus ( ival )
         rval = minus ( rval )
      ENDIF
   ELSE
      rval = UNDEFINED
   ENDIF
   cval = 'LAT.:AIREP:Latitude (N) ' // arg1
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival, rval, cval )

   iget = len_trim(arg2)
   IF ( SCAN ( 'EWew' , arg2(iget:iget) ) .EQ. 0 ) THEN
      ival = UNDEFINED
      section_id = -1
   ELSE 
      ival = str2int ( arg2 ( :iget-1 ) )
      if ( ival .LT. 0 ) section_id = -1
   ENDIF
   IF ( section_id .NE. -1 ) THEN
      IF ( iget .LE. 4 ) THEN
         rval = ival
      ELSE
         rval = ( ival / 100 ) + MOD ( ival , 100 ) / 60.0
      ENDIF
      IF ( arg2(iget:iget) .EQ. 'W' .OR. arg2(iget:iget) .EQ. 'w' ) THEN
         ival = minus ( ival )
         rval = minus ( rval )
      ENDIF
   ELSE
      rval = UNDEFINED
   ENDIF
   cval = 'LON.:AIREP:Longitude (E) ' // arg2
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival, rval, cval )

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_airep_flvl ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   IF ( arg(1:1) .EQ. 'F' ) THEN
      ival = str2int ( TRIM(arg(2:)) )
   ELSE
      ival = str2int ( TRIM(arg) )
   ENDIF
   IF ( ival .LT. 0 ) THEN
      cval = 'invalid <' // TRIM(arg) // '>'
      CALL code_error ( 'airep_flvl', 'Invalid Flight Level', arg )
      section_id = -1
   ELSE
      cval = arg
   ENDIF
   rval = multiply ( ival, 30.48 )  ! 100 ft is 30.48 m
   cval = 'FLVL:AIREP:Flight Level (m)' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_airep_temp ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'

   ival = str2int ( TRIM(arg(3:)) )

   IF ( ival .eq. MISSING ) THEN
      CALL code_ignore ( 'airep_temp', 'Missing Temperature', arg )
      cval = 'missing <' // TRIM(arg) // '>'

   ELSE IF ( ival .LT. 0 ) THEN
      CALL code_error ( 'airep_temp', 'Invalid Temperature', arg )
      cval = 'invalid <' // TRIM(arg) // '>'

   ELSE IF ( arg(1:2) .EQ. 'MS' ) THEN
      ival = minus ( ival )
      cval = arg

   ELSE IF ( arg(1:2) .EQ. 'PS' ) THEN
      cval = arg

   ELSE
      ! Probably decoding wrong group, unexpected
      CALL code_error ( 'airep_temp', 'Invalid Temperature', arg )
      ival = UNDEFINED
      cval = 'invalid <' // TRIM(arg) // '>'
      section_id = -1

   ENDIF

   rval = ival
   cval = 'TEMP:AIREP:Temperature (C)' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_airep_wind ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER :: iget
   CHARACTER ( LEN = rlen ) :: cget

   cval = arg
   iget = SCAN ( TRIM(arg) , '/' )
   IF ( iget .EQ. 0 ) THEN
      CALL code_error ( 'airep_wind', 'Invalid wind missing /', arg )
      section_id = -1
      RETURN
   ENDIF
   cget = arg(iget+1:LEN_TRIM(arg))

   ival = str2int ( arg(:iget-1) )
   IF ( ival .LT. 0 ) THEN
      CALL code_error ( 'airep_wind', 'Invalid wind direction', arg )
      rval = UNDEFINED
      cval = 'invalid <' // TRIM(arg) // '>'
      section_id = -1
   ELSE
      rval = ival
   ENDIF
   cval = 'WDIR:AIREP:Wind direction (deg)' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

   cval = cget
   iget = SCAN ( TRIM(cget) , 'KT' )
   IF ( iget .EQ. 0 ) iget = LEN_TRIM(cget)+1

   ival = str2int ( cget(:iget-1) )
   IF ( ival .LT. 0 ) THEN
      CALL code_error ( 'airep_wind', 'Invalid wind speed', cget )
      rval = UNDEFINED
      cval = 'invalid <' // TRIM(cget) // '>'
      section_id = -1
   ELSE
      rval = multiply ( ival, KNOT )
   ENDIF
   cval = 'WSPD:AIREP:Wind speed (m/s)' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

   ! Decoding Tables referenced by the decoder
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

!----------------------------------------------------------------------

SUBROUTINE code_table_0200 ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg(1:1) )
   rval = multiply ( str2int ( arg(2:4) ) , 0.1 )
   SELECT CASE ( ival )
      CASE ( 0 ) ;
         cval = 'increasing, then decreasing; pressure >= 3 hrs. ago'
      CASE ( 1 : 3 ) ;
         cval = 'increasing, pressure > 3 hrs. ago'
      CASE ( 4 ) ;
         cval = 'steady, pressure the same as 3 hrs. ago'
      CASE ( 5 ) ;
         cval = 'decreasing, then increasing; pressure <= 3 hrs. ago'
         rval = minus(rval)
      CASE ( 6 : 8 )
         cval = 'decreasing, pressure < 3 hrs. ago'
         rval = minus(rval)
      CASE DEFAULT
         IF ( arg .EQ. '////' ) THEN
            rval = MISSING
            cval = 'missing'
         ELSE
            rval = UNDEFINED
            cval = 'invalid tendency <' // arg(:5) // '>'
            CALL code_error ( '0200', 'Pressure Tendency', arg )
         ENDIF
   ENDSELECT
   cval = '0200:appp :Pressure ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0264 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER :: altitude
   SELECT CASE ( arg(1:1) )
      CASE ( '1' ) ; cval = 'at 1000 hPa' ; ival = 1000
      CASE ( '2' ) ; cval = 'at  925 hPa' ; ival =  925
      CASE ( '5' ) ; cval = 'at  500 hPa' ; ival =  500
      CASE ( '7' ) ; cval = 'at  700 hPa' ; ival =  700
      CASE ( '8' ) ; cval = 'at  850 hPa' ; ival =  850
      CASE DEFAULT
         IF ( arg .EQ. '////' ) THEN
            ival = MISSING
            rval = MISSING
            cval = 'missing'
         ELSE
            ival = UNDEFINED
            rval = UNDEFINED
            cval = 'invalid <' // arg(:5) // '>'
            CALL code_error ( '0264', 'Geopotential on P sfc', arg )
         ENDIF
   ENDSELECT
   IF ( ival > 0 ) THEN
      altitude = str2int ( arg(2:4) )
      CALL modify_height_by_stdpre ( ival , altitude , 1000 )
      rval = altitude
   ENDIF
   cval = '0264:a3hhh:Geopotential height (gpm) ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0265 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg(1:1) )
   rval = ival
   SELECT CASE ( ival )
      CASE ( 0 ) ; cval = 'Pressure instr. ASSOCIATED wind-measuring equip.'
      CASE ( 1 ) ; cval = 'Optical theodolite'
      CASE ( 2 ) ; cval = 'Radiotheodolite'
      CASE ( 3 ) ; cval = 'Radar'
      CASE ( 4 ) ; cval = 'Pressure instr. failed during ascent'
      CASE ( 5 ) ; cval = 'VLF-Omega'
      CASE ( 6 ) ; cval = 'Loran-C'
      CASE ( 7 ) ; cval = 'Wind profiler'
      CASE ( 8 ) ; cval = 'Satellite navigation'
      CASE ( 9 ) ; cval = 'Reserved / Others'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '0265', 'Measuring Equipment used', arg )
         ENDIF
   ENDSELECT
   cval = '0265:a4   :Measuring Equipment used ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0500 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( arg )
      CASE ( '0' ) ; cval = 'Cirrus (Ci)'
      CASE ( '1' ) ; cval = 'Cirrocumulus (Cc)'
      CASE ( '2' ) ; cval = 'Cirrostratus (Cs)'
      CASE ( '3' ) ; cval = 'Altocumulus (Ac)'
      CASE ( '4' ) ; cval = 'Altostratus (As)'
      CASE ( '5' ) ; cval = 'Nimbostratus (Ns)'
      CASE ( '6' ) ; cval = 'Stratocumulus (Sc)'
      CASE ( '7' ) ; cval = 'Stratus (St)'
      CASE ( '8' ) ; cval = 'Cumulus (Cu)'
      CASE ( '9' ) ; cval = 'Cumulonimbus (Cb)'
      CASE ( '/' ) ; cval = 'Invisible owing to darkness or obstruction'
                     rval = MISSING
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'invalid <' // arg(:5) // '>'
         CALL code_error ( '0500', 'Genus of cloud', arg )
   ENDSELECT
   cval = '0500:C    :Genus of cloud ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0509 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( arg )
      CASE ( '0' ) ; cval = 'No CH clouds'
      CASE ( '1' ) ; cval = 'Cirrus in form of filaments, strands or hooks'
      CASE ( '2' ) ; cval = 'Dense Cirrus, in patches or entangled sheaves'
      CASE ( '3' ) ; cval = 'Dense Cirrus, in form of an anvil'
      CASE ( '4' ) ; cval = 'Cirrus in form of filaments, increasing'
      CASE ( '5' ) ; cval = 'Cirrus/Cirrostratus, cover < 45 deg of Horizon, increasing'
      CASE ( '6' ) ; cval = 'Circus/Cirrostratus, cover > 45 deg of Horizon, increasing'
      CASE ( '7' ) ; cval = 'Cirrostratus covering the whole sky'
      CASE ( '8' ) ; cval = 'Cirrostratus, not increasing'
      CASE ( '9' ) ; cval = 'Cirrcumulus'
      CASE ( '/' ) ; cval = 'Invisible owing to darkness or obstruction'
                     rval = MISSING
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'High Clouds invalid <' // arg(:5) // '>'
         CALL code_error ( '0509', 'High Cloud Indictor', arg )
   ENDSELECT
   cval = '0509:CH   :' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0513 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( arg )
      CASE ( '0' ) ; cval = 'No CL clouds'
      CASE ( '1' ) ; cval = 'Cumulus with little vertical extent'
      CASE ( '2' ) ; cval = 'Cumulus of moderate or strong vertical extent'
      CASE ( '3' ) ; cval = 'Cumulonimbus, the summits lack sharp outlines'
      CASE ( '4' ) ; cval = 'StratoCumulus formed by spreading of Cumulus'
      CASE ( '5' ) ; cval = 'StratoCumulus formed not by spreading of Cumulus'
      CASE ( '6' ) ; cval = 'Stratus in more or less a continuous sheet'
      CASE ( '7' ) ; cval = 'Stratus of bad weather'
      CASE ( '8' ) ; cval = 'Cumulus and StratoCumulus'
      CASE ( '9' ) ; cval = 'Cumulonimbus with clear anvil'
      CASE ( '/' ) ; cval = 'Invisible owing to darkness or obstruction'
                     rval = MISSING
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'Low clouds invalid <' // arg(:5) // '>'
         CALL code_error ( '0513', 'Low Cloud Indicator', arg )
   ENDSELECT
   cval = '0513:CL   :' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0515 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( arg )
      CASE ( '0' ) ; cval = 'No CM clouds'
      CASE ( '1' ) ; cval = 'Altostratus, most being transparent'
      CASE ( '2' ) ; cval = 'Altostratus or Nimbostratus, dense enough to block the sun'
      CASE ( '3' ) ; cval = 'Altostratus, most being semi-transparent'
      CASE ( '4' ) ; cval = 'Patches of semi-transparent Altocumulus'
      CASE ( '5' ) ; cval = 'Bands or a layer of semi-transparent Altocumulus'
      CASE ( '6' ) ; cval = 'Altocumulus resulting from spread of Cumulus'
      CASE ( '7' ) ; cval = 'Altocumulus in two or more layers'
      CASE ( '8' ) ; cval = 'Altocumulus with sproutings in small towers'
      CASE ( '9' ) ; cval = 'Altocumulus of a chaotic sky'
      CASE ( '/' ) ; cval = 'Invisible owing to darkness or obstruction'
                     rval = MISSING
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'Middle clouds invalid <' // arg(:5) // '>'
         CALL code_error ( '0515', 'Middle Cloud Indicator', arg )
   ENDSELECT
   cval = '0515:CM   :' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0700 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   CHARACTER ( LEN = 2 ), DIMENSION ( 8 ) :: cc = &
      (/ 'NE' , 'E ' , 'SE' , 'S ' , 'SW' , 'W ' , 'NW' , 'N ' /)
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   SELECT CASE ( arg )
      CASE ( '0' ) ;       rval = 0         ; cval = 'calm or stationary'
      CASE ( '1' : '8' ) ; rval = multiply(45,ival)
                           cval = cc ( ival )
      CASE ( '9' ) ;       rval = UNDEFINED ; cval = 'unknown'
      CASE ( '/' ) ;       rval = MISSING   ; cval = 'missing'
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'invalid <' // arg(:5) // '>'
         CALL code_error ( '0700', 'Compass Bearing', arg )
   ENDSELECT
   cval = '0700:D    :Dir. or Bearing >> ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0822 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   CALL code_table_3845 ( arg(2:2) , ival , rval , cval , ' ' )
   ival = str2int ( arg(1:1) )
   SELECT CASE ( arg(3:3) )
      CASE ( '0' ) ; rval = multiply ( rval , 10 )
      CASE ( '1' ) ; rval = multiply ( rval , 11 )
      CASE ( '2' ) ; rval = multiply ( rval , 12 )
      CASE ( '3' ) ; rval = multiply ( rval , 13 )
      CASE ( '4' ) ; rval = multiply ( rval , 14 )
      CASE ( '5' ) ; rval = multiply ( rval ,  5 )
      CASE ( '6' ) ; rval = multiply ( rval ,  6 )
      CASE ( '7' ) ; rval = multiply ( rval ,  7 )
      CASE ( '8' ) ; rval = multiply ( rval ,  8 )
      CASE ( '9' ) ; rval = multiply ( rval ,  9 )
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '0822', 'Temperature Change and Time', arg )
         ENDIF
   ENDSELECT
   cval = '0822:gosnd:Temperature Change (K) and Time (hr) ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0877 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   SELECT CASE ( ival )
      CASE ( 0 ) ;       rval = 0 ; cval = 'Calm , no motion or no waves'
      CASE ( 01 : 36 ) ; rval = multiply ( ival , 10 ) ; cval = '(deg.)'
      CASE ( 99 ) ;      rval = 0 ; cval = 'Variable or unknown'
      CASE DEFAULT
         IF ( arg .EQ. '//' ) THEN
            rval = MISSING
            cval = 'missing'
         ELSE
            rval = UNDEFINED
            cval = 'invalid <' // arg(:5) // '>'
            CALL code_error ( '0877', 'Wind or Wave Direction', arg )
         ENDIF
   ENDSELECT
   cval = '0877:dd   :Wind direction or wave direction ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0901 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( arg )
      CASE ( '0' ) ; cval = 'dry, no crack'
      CASE ( '1' ) ; cval = 'moist'
      CASE ( '2' ) ; cval = 'wet'
      CASE ( '3' ) ; cval = 'flooded'
      CASE ( '4' ) ; cval = 'frozen'
      CASE ( '5' ) ; cval = 'glaze'
      CASE ( '6' ) ; cval = 'loose dry dust, not covered completely'
      CASE ( '7' ) ; cval = 'thin cover of loose dry dust, cover completely'
      CASE ( '8' ) ; cval = 'moderate or thick loose dry dust, cover completely'
      CASE ( '9' ) ; cval = 'extremely dry with cracks'
      CASE ( '/' ) ; cval = 'missing' ; rval = MISSING
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'invalid <' // arg(:5) // '>'
         CALL code_error ( '0901', 'State of Ground', arg )
   ENDSELECT
   cval = '0901:E    :State of Ground ' // cval
   IF ( append .EQ. 'APPEND' ) THEN
      IF ( arg .NE. '/' ) CALL record_appendr ( ival , rval , cval )
   ENDIF
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_0975 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( arg )
      CASE ( '0' ) ; cval = 'predominantly covered by ice'
      CASE ( '1' ) ; cval = 'compact / wet snow, covering < 1/2 ground'
      CASE ( '2' ) ; cval = 'compact / wet snow, covering >= 1/2 ground'
      CASE ( '3' ) ; cval = 'compact / wet snow, even layer, cover completely'
      CASE ( '4' ) ; cval = 'compact / wet snow, uneven layer, cover completely'
      CASE ( '5' ) ; cval = 'loose dry snow, covering < 1/2 ground'
      CASE ( '6' ) ; cval = 'loose dry snow, covering >= 1/2 ground'
      CASE ( '7' ) ; cval = 'loose dry snow, even layer, cover completely'
      CASE ( '8' ) ; cval = 'loose dry snow, uneven layer, cover completely'
      CASE ( '9' ) ; cval = 'snow, cover completely; deep drifts'
      CASE ( '/' ) ; cval = 'missing' ; rval = MISSING
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'invalid <' // arg(:5) // '>'
         CALL code_error ( '0975', 'State of Ground with snow', arg )
   ENDSELECT
   cval = '0975:E    :State of Ground with snow ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_1600 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   SELECT CASE ( arg )
      CASE ( '0' ) ; cval =      '0 - 50 m ' ; rval = 25.
      CASE ( '1' ) ; cval =    '50 - 100 m ' ; rval = 75.
      CASE ( '2' ) ; cval =   '100 - 200 m ' ; rval = 150.
      CASE ( '3' ) ; cval =   '200 - 300 m ' ; rval = 250.
      CASE ( '4' ) ; cval =   '300 - 600 m ' ; rval = 450.
      CASE ( '5' ) ; cval =  '600 - 1000 m ' ; rval = 800.
      CASE ( '6' ) ; cval = '1000 - 1500 m ' ; rval = 1250.
      CASE ( '7' ) ; cval = '1500 - 2000 m ' ; rval = 1750.
      CASE ( '8' ) ; cval = '2000 - 2500 m ' ; rval = 2250.
      CASE ( '9' ) ; cval =   'over 2500 m ' ; rval = 2500.
      CASE ( '/' ) ; cval = 'Not known/base level < station'
                     rval = MISSING
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'invalid <' // arg(:5) // '>'
         CALL code_error ( '1600', 'Lowest Cloud Height', arg )
   ENDSELECT
   cval = '1600:h    :Lowest cloud height ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_1677 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   SELECT CASE ( ival )
      CASE (       0 ) ; cval = 'less than 30 m ' ; rval = 15.
      CASE (  1 : 50 ) ; rval = ival * 30.
      CASE ( 56 : 80 ) ; rval = ( ival - 56 ) *  300. + 1800.
      CASE ( 81 : 88 ) ; rval = ( ival - 81 ) * 1500. + 10500.
      CASE (      89 ) ; cval =   'over 21000 m ' ; rval = 21000.1
      CASE (      90 ) ; cval = 'less than 50 m ' ; rval = 25.
      CASE (      91 ) ; cval =     '50 - 100 m ' ; rval = 75.
      CASE (      92 ) ; cval =    '100 - 200 m ' ; rval = 150.
      CASE (      93 ) ; cval =    '200 - 300 m ' ; rval = 250.
      CASE (      94 ) ; cval =    '300 - 600 m ' ; rval = 450.
      CASE (      95 ) ; cval =   '600 - 1000 m ' ; rval = 800.
      CASE (      96 ) ; cval =  '1000 - 1500 m ' ; rval = 1250.
      CASE (      97 ) ; cval =  '1500 - 2000 m ' ; rval = 1750.
      CASE (      98 ) ; cval =  '2000 - 2500 m ' ; rval = 2250.
      CASE (      99 ) ; cval =    'over 2500 m ' ; rval = 2500.1
      CASE DEFAULT
         IF ( arg .EQ. '//' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '1677', 'Cloud or Fog height', arg )
         ENDIF
   ENDSELECT
   cval = '1677:hh   :cloud or fog height ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_1734 ( arg , ival , rval , cval , append , part )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   CHARACTER ( LEN = 1 ) , INTENT ( IN ) :: part
   IF ( arg .EQ. '/' ) THEN
      ival = MISSING
   ELSEIF ( part .EQ. 'A' ) THEN
      SELECT CASE ( arg )
         CASE ( '1':'5' , '7' ) ; ival = multiply ( str2int ( arg ) , 100 )
         CASE ( '8' )           ; ival = 850
         CASE ( '9' )           ; ival = 925
         CASE ( '0' )           ; ival = 1000
         CASE DEFAULT           ; ival = UNDEFINED
      ENDSELECT
   ELSEIF ( part .EQ. 'C' ) THEN
      IF ( SCAN ( '12357' , arg ) > 0 ) THEN
         ival = multiply ( str2int ( arg ) , 10 )
      ELSE
         ival = UNDEFINED
      ENDIF
   ENDIF
   IF ( ival .EQ. UNDEFINED ) THEN
      CALL code_error ( '1734' , 'Maximum wind level' , arg )
   ENDIF
   rval = ival
   cval = '1734:Id   :Maximum wind level'
   IF ( append .EQ. 'APPEND' ) THEN
      IF ( ival .EQ. MISSING ) THEN
         CALL record_appendr ( ival , rval , '1734:Id   :No wind on std P sfc' )
      ELSE
         CALL record_appendr ( ival , rval , cval )
      ENDIF
   ENDIF
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_1806 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg(4:4) )
   rval = multiply ( str2int ( arg(1:3) ) , 0.1 * TOMETER )
   SELECT CASE ( arg(4:4) )
      CASE ( '0' ) ; cval = 'USA open pan evaporimeter (without cover)'
      CASE ( '1' ) ; cval = 'USA open pan evaporimeter (mesh covered)'
      CASE ( '2' ) ; cval = 'GGI-3000 evaporimeter (sunken)'
      CASE ( '3' ) ; cval = '20 m2 tank'
      CASE ( '4' ) ; cval = 'others'
      CASE ( '5' ) ; cval = 'Rice (evapo-transpiration)'
      CASE ( '6' ) ; cval = 'Wheat (evapo-transpiration)'
      CASE ( '7' ) ; cval = 'Maize (evapo-transpiration)'
      CASE ( '8' ) ; cval = 'Sorghum (evapo-transpiration)'
      CASE ( '9' ) ; cval = 'Other Crops (evapo-transpiration)'
      CASE ( '/' ) ; cval = 'Unknown'
      CASE DEFAULT
         cval = 'invalid <' // arg(:5) // '>'
         CALL code_error ( '1806', 'Evaporation (past 24h, in m)', arg )
   ENDSELECT
   cval = '1806:EEEiE:Evaporation ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_1819 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( ival )
      CASE ( 0 ) ; cval = 'included in both sections 1 & 3'
      CASE ( 1 ) ; cval = 'included in section 1'
      CASE ( 2 ) ; cval = 'included in section 3'
      CASE ( 3 ) ; cval = '= 0, Group 6RRRtR omitted'
                   CALL record_appendr ( MISSING , 0. , &
                        '4019:RRRtR:Precip. amount (duration unknown)' )
      CASE ( 4 ) ; cval = 'unknown, Group 6RRRtR omitted'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '1819', 'Precip.  availability', arg )
         ENDIF
   ENDSELECT
   cval = '1819:iR   :Precip. ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_1855 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   SELECT CASE ( ival )
      CASE ( 0 ) ; rval = 1.00 ; cval = 'estimated'
      CASE ( 1 ) ; rval = 1.00 ; cval = 'obtained by anemometer'
      CASE ( 3 ) ; rval = KNOT ; cval = 'estimated'
      CASE ( 4 ) ; rval = KNOT ; cval = 'obtained by anemometer'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '1855', 'Source and Unit of Wind speed', arg )
         ENDIF
    ENDSELECT
    cval = '1855:iw   :Source and Unit of Wind speed ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_1860 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( ival )
      CASE ( 1 ) ; cval =    'Manned, Present/Past Weather included'
      CASE ( 2 ) ; cval =    'Manned, No significant phenomenon to report'
      CASE ( 3 ) ; cval =    'Manned, Present/Past Weather not available'
      CASE ( 4 ) ; cval = 'Automatic, Weather data coded :4677 and 4561'
      CASE ( 5 ) ; cval = 'Automatic, No significant phenomenon to report'
      CASE ( 6 ) ; cval = 'Automatic, Present/Past Weather not available'
      CASE ( 7 ) ; cval = 'Automatic, Weather data coded :4680 and 4531'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '1860', 'Station Type', arg )
         ENDIF
   ENDSELECT
   cval = '1860:ix   :Station TYPE ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_2700 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg ) ; rval = ival
   SELECT CASE ( ival )
      CASE ( 0 : 8 ) ; cval = arg(1:1) // ' oktas'
      CASE ( 9 ) ;     cval = 'Sky obscured' ; rval = UNDEFINED
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'indiscernible or observation not made'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '2700', 'Total Cloud Cover', arg )
         ENDIF
   ENDSELECT
   cval = '2700:Nh   :Total Cloud Cover ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_3845 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   SELECT CASE ( ival )
      CASE ( 0 ) ; rval =  1. ; cval = 'positive or zero'
      CASE ( 1 ) ; rval = -1. ; cval = 'negative'
      CASE ( 9 ) ; rval =  0. ; cval = 'Relative Humidity Follows'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
! *************************************************************************
            CALL code_error ( '3845', 'Sign of data and RH Indicator', arg )
! *************************************************************************
         ENDIF
   ENDSELECT
   cval = '3845:Sn   :Sign of data and RH indicator' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_3850 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   SELECT CASE ( ival )
      CASE ( 0 ) ; rval =  1. ; cval = 'positive or zero / Intake'
      CASE ( 2 ) ; rval =  1. ; cval = 'positive or zero / Bucket'
      CASE ( 4 ) ; rval =  1. ; cval = 'positive or zero / Hull contact sensor'
      CASE ( 6 ) ; rval =  1. ; cval = 'positive or zero / Other'
      CASE ( 1 ) ; rval = -1. ; cval = 'negative / Intake'
      CASE ( 3 ) ; rval = -1. ; cval = 'negative / Bucket'
      CASE ( 5 ) ; rval = -1. ; cval = 'negative / Hull contact sensor'
      CASE ( 7 ) ; rval = -1. ; cval = 'negative / Other'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '3850', 'Sign & TYPE of SST Measurement', arg )
         ENDIF
   ENDSELECT
   cval = '3850:Sn   :Sign and TYPE of SST Measurement ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_3889 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   cval = '(m)'
   SELECT CASE ( ival )
      CASE ( 001 : 996 ) ; rval = multiply ( 0.01 , ival )
      CASE ( 997 )       ; rval = 0.005 ;    cval = 'less than 0.5 cm'
      CASE ( 998 )       ; rval = UNDEFINED; cval = 'not continuous'
      CASE ( 999 )       ; rval = UNDEFINED; cval = 'measurement impossible'
      CASE DEFAULT
         IF ( arg .EQ. '///' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '3889', 'Depth of Snow', arg )
         ENDIF
   ENDSELECT
   cval = '3889:sss  :Depth of snow ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_4019 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INTEGER :: iget
   CHARACTER ( LEN = 2 ) , DIMENSION ( 9 ) :: cc = &
      (/ ' 6' , '12' , '18' , '24' , ' 1' , ' 2' , ' 3' , ' 9' , '15' /)
   INCLUDE 'inc.code_tables'

   iget = str2int ( arg(1:3) )
   IF ( iget > 989 ) iget = iget - 990
   rval = multiply ( iget , 0.1 * TOMETER )

   iget = str2int ( arg(4:4) )
   ival = MISSING
   SELECT CASE ( iget )
      CASE ( 0 ) ;       cval = '(dt nostandard) <' // arg(:5) // '>'
      CASE ( MISSING ) ; cval = '(dt invalid) <' // arg(:5) // '>'
      CASE ( 1 : 9 ) ;   cval = cc(iget) // ' hrs preceding obs. (m)'
                         ival = str2int(cc(iget))
      CASE DEFAULT
         cval = '(dt invalid) <' // arg(:5) // '>'
         rval = UNDEFINED
         ival = UNDEFINED
         CALL code_error ( '4019', 'Precip. & duration', arg )
   ENDSELECT
   cval = '4019:RRRtR:Precip. amount & duration ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_4377 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   cval = '(km)'
   SELECT CASE ( ival )
      CASE ( 0 )       ; rval = 0.09 ; cval = '< .1 km'
      CASE (  1 : 50 ) ; rval = ival  *  0.1
      CASE ( 56 : 80 ) ; rval = ival - 50.0
      CASE ( 81 : 88 ) ; rval = ( ival - 80.0 ) * 5 + 30.0
      CASE ( 89 )      ; rval = 70.1 ; cval = '> 70 km'
      CASE ( 90 )      ; rval = 0.03 ; cval = '< .05 km'
      CASE ( 91 )      ; rval = 0.13 ; cval = '.05 - .2 km'
      CASE ( 92 )      ; rval = 0.35 ; cval = '.2 - .5 km'
      CASE ( 93 )      ; rval = 0.75 ; cval = '.5 - 1 km'
      CASE ( 94 )      ; rval =  1.5 ; cval = '1 - 2 km'
      CASE ( 95 )      ; rval =   3. ; cval = '2 - 4 km'
      CASE ( 96 )      ; rval =   6. ; cval = '4 - 10 km'
      CASE ( 97 )      ; rval =  15. ; cval = '10 - 20 km'
      CASE ( 98 )      ; rval =  35. ; cval = '20 - 50 km'
      CASE ( 99 )      ; rval = 50.1 ; cval = '>= 50 km'
      CASE DEFAULT
         IF ( arg .EQ. '//' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
           cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
           CALL code_error ( '4377', 'Visibility', arg )
         ENDIF
   ENDSELECT
   cval = '4377:VV   :Visibility ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_4451 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   cval = ' ( km/hr ) '
   SELECT CASE ( arg )
      CASE ( '0' ) ; cval = '0 km/hr' ;       rval = 0.
      CASE ( '1' ) ; cval = '0 - 10 km/hr' ;  rval = 5.
      CASE ( '2' ) ; cval = '11 - 19 km/hr' ; rval = 15.
      CASE ( '3' ) ; cval = '20 - 28 km/hr' ; rval = 24.
      CASE ( '4' ) ; cval = '29 - 37 km/hr' ; rval = 33.
      CASE ( '5' ) ; cval = '38 - 47 km/hr' ; rval = 43.
      CASE ( '6' ) ; cval = '48 - 56 km/hr' ; rval = 54.
      CASE ( '7' ) ; cval = '57 - 65 km/hr' ; rval = 61.
      CASE ( '8' ) ; cval = '66 - 75 km/hr' ; rval = 71.
      CASE ( '9' ) ; cval = 'over 75 km/hr' ; rval = 75.1
      CASE ( '/' ) ; cval = 'missing' ; rval = MISSING
      CASE DEFAULT
         rval = UNDEFINED
         cval = 'invalid <' // arg(:5) // '>'
         CALL code_error ( '4451', 'Ship speed 3 hrs preceding observation', arg )
   ENDSELECT
   cval = '4451:V    :Ship speed 3 hrs preceding observation ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_4531 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg ) ; rval = ival
   SELECT CASE ( ival )
      CASE ( 0 ) ; cval = 'No significant weather observed'
      CASE ( 1 ) ; cval = 'Visibility reduced'
      CASE ( 2 ) ; cval = 'Blowing phenomena, visibility reduced'
      CASE ( 3 ) ; cval = 'Fog'
      CASE ( 4 ) ; cval = 'Precipitation'
      CASE ( 5 ) ; cval = 'Drizzle'
      CASE ( 6 ) ; cval = 'Rain'
      CASE ( 7 ) ; cval = 'Snow or ice pellets'
      CASE ( 8 ) ; cval = 'Showers or intermittent precipitation'
      CASE ( 9 ) ; cval = 'Thunderstorm'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '4531', 'Past Weather (auto station)', arg )
         ENDIF
   ENDSELECT
   cval = '4531:wa1wa:Past Wea. (auto sttn) ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_4561 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg ) ; rval = ival
   SELECT CASE ( ival )
      CASE ( 0 ) ; cval = 'Cloud cover <= 1/2'
      CASE ( 1 ) ; cval = 'Cloud cover variable'
      CASE ( 2 ) ; cval = 'Cloud cover > 1/2'
      CASE ( 3 ) ; cval = 'Sandstorm, duststorm or blowing snow'
      CASE ( 4 ) ; cval = 'Fog or ice fog or thick haze'
      CASE ( 5 ) ; cval = 'Drizzle'
      CASE ( 6 ) ; cval = 'Rain'
      CASE ( 7 ) ; cval = 'Snow, or rain and snow mixed'
      CASE ( 8 ) ; cval = 'Shower(s)'
      CASE ( 9 ) ; cval = 'Thunderstorm(s) with or without precip.'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '4561', 'Past Weather (manned station)', arg )
         ENDIF
   ENDSELECT
   cval = '4561:w1w2 :Past Wea. (manned sttn) ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_4639 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   rval = ival
   SELECT CASE ( ival )
      CASE ( 1 ) ; cval = 'cloud motion in infrared channel'
      CASE ( 2 ) ; cval = 'cloud motion in visible channel'
      CASE ( 3 ) ; cval = 'cloud motion in water vapour channel'
      CASE ( 4 ) ; cval = 'cloud motion in a number of channels'
      CASE DEFAULT
         IF ( arg .EQ. '/' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '4639', 'Winds derivation method', arg )
         ENDIF
   ENDSELECT
   cval = '4639:wi   :Winds derivation method ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_4677 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg ) ; rval = ival
   SELECT CASE ( ival )
      CASE (  0 : 19 ) ; cval = 'No precipitation'
      CASE ( 20 : 29 ) ; cval = 'Precip. during preceding hr, not at obs. time'
      CASE ( 30 : 39 ) ; cval = 'Duststorm, sandstorm, drifting or blowing snow'
      CASE ( 40 : 49 ) ; cval = 'Fog or ice for at obs. time'
      CASE ( 50 : 59 ) ; cval = 'Drizzle'
      CASE ( 60 : 69 ) ; cval = 'Rain'
      CASE ( 70 : 79 ) ; cval = 'Solid precip. not in showers'
      CASE ( 80 : 89 ) ; cval = 'Showery precip.'
      CASE ( 90 : 99 ) ; cval = 'Showery precip. with thunderstorm'
      CASE DEFAULT
         IF ( arg .EQ. '//' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '4677', 'Present Wea. (manned station)', arg )
         ENDIF
   ENDSELECT
   cval = '4677:ww   :Present Wea. (manned sttn) ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_4680 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg ) ; rval = ival
   SELECT CASE ( ival )
      CASE (  0 : 19 ) ; cval = 'No precipitation'
      CASE ( 20 : 26 ) ; cval = 'Precip. during preceding hr, not at obs. time'
      CASE ( 27 : 35 ) ; cval = 'Duststorm, sandstorm, drifting or blowing snow'
      CASE ( 40 : 49 ) ; cval = 'Precipitation'
      CASE ( 50 : 59 ) ; cval = 'Drizzle'
      CASE ( 60 : 69 ) ; cval = 'Rain'
      CASE ( 70 : 76 ) ; cval = 'Snow'
      CASE ( 80 : 87 ) ; cval = 'Shower(s) or intermittent precip.'
      CASE ( 90 : 96 ) ; cval = 'Thunderstorm'
      CASE (      99 ) ; cval = 'Tornado'
      CASE DEFAULT
         IF ( arg .EQ. '//' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            cval = 'invalid <' // arg(:5) // '>'
            rval = UNDEFINED
            CALL code_error ( '4680', 'Present Weather (auto station)', arg )
         ENDIF
   ENDSELECT
   cval = '4680:wawa :Present Wea. (auto sttn) ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_Abnnn ( arg )
   USE record_def
   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   CHARACTER ( LEN = rlen )              :: cval
   cval = '....:Abnnn:Platform Id >>> ' // arg
   CALL record_appendi ( str2int(arg(1:5)) , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_BBB ( arg, ival, rval, cval, append, &
                            lat,  lng, lat_sgn, lng_sgn )
   USE record_def
   USE bulletin_def
   IMPLICIT NONE
   INTEGER , INTENT ( OUT ) :: lat , lng , lat_sgn , lng_sgn
   INCLUDE 'inc.code_tables'

   ival = str2int ( arg(1:1) )
   lat  = multiply ( str2int ( arg(2:2) ) , 10 )
   lng  = multiply ( str2int ( arg(3:3) ) , 10 )

   IF ( lat < 0 .OR. lat > 90 ) THEN
      CALL code_error ( 'BBB' , 'invalid Latitude' , arg )
      section_id = -1 ; RETURN
   ELSEIF ( lng < 0 ) THEN
      CALL code_error ( 'BBB' , 'invalid Longitude' , arg )
      section_id = -1 ; RETURN
   ENDIF

   SELECT CASE ( ival )
      CASE ( 1 , 2 , 6 , 7 ) ; IF ( lng <= 80 ) lng = lng + 100
      CASE ( 0 , 3 , 5 , 8 ) ;
      CASE DEFAULT
         CALL code_error ( 'BBB  ' , 'invalid Octant of the global' , arg )
         section_id = -1 ; RETURN
   ENDSELECT

   IF ( SCAN('5678',arg(1:1)) > 0 ) THEN
      lat_sgn = -1
      lat     = -lat
   ELSE
      lat_sgn =  1
   ENDIF

   IF ( SCAN('0156',arg(1:1)) > 0 ) THEN
      lng_sgn = -1
      lng     = -lng
   ELSE
      lng_sgn =  1
   ENDIF

   IF ( append .EQ. 'APPEND' ) THEN
      ival = multiply ( lat , 100 )
      rval = multiply ( lng , 100 )
      cval = 'SAT.:BBB  :Lat(N) Long(E) '
      CALL record_appendr ( ival , rval , cval )
   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_DDDD ( arg )
   USE record_def
   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   CHARACTER ( LEN = rlen )              :: cval
   cval = '....:D...D:SHIP or TEMP MOBIL Id >>> ' // arg
   CALL record_appendi ( -1 , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_ddfff (arg, ival, rval, cval, append, pressure, wind_unit)
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER , INTENT ( IN ) :: pressure
   REAL , INTENT ( IN )    :: wind_unit
   CALL code_table_0877 ( arg(1:2) , ival , rval , cval , ' ' )
   ival = str2int ( arg(3:5) )
   IF ( ival >= 500 ) THEN
      ival = subtract ( ival , 500 )
      rval = add ( rval , 5. )
   ENDIF
   cval = TRIM ( cval ) // ' at specific P(Z) sfc'
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( pressure , rval , cval )
   rval = multiply ( ival , wind_unit )
   cval = '....:fff  :Wind speed (m/s) at specific P(Z) sfc'
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( pressure , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_GGgg ( arg , ival , rval , cval , append )
   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER :: iget
   iget = multiply ( 100 , str2int ( arg(1:4) ) )
   IF ( iget > bul_hhmmss ) THEN
      CALL decrease_date ( bul_yymmdd , ival )
   ELSEIF ( iget < 0 ) THEN
      ival = UNDEFINED
      CALL code_error ( 'GGgg' , 'Rev. Observation Time HHMM' , arg )
   ELSE
      ival = bul_yymmdd
   ENDIF
   rval = iget
   cval = '....:GGgg :Observation Time HHMM'
   IF ( append .EQ. 'APPEND' ) THEN
      IF ( ival .NE. MISSING  .AND. ival .NE. UNDEFINED )  &
         CALL record_appendr ( ival, rval, cval )
   ENDIF
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_hhhhim ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   rval = str2int ( arg(1:4) )
   ival = str2int ( arg(5:5) )
   SELECT CASE ( ival )
      CASE ( 1 : 4 ) ; cval = 'originally in m'
      CASE ( 5 : 8 ) ; cval = 'originally in ft' ; rval = multiply(rval,FEET)
      CASE DEFAULT
         cval = 'invalid <' // arg(:5) // '>'
         rval = UNDEFINED
         CALL code_error ( 'hhhhim', 'Elevation of mobile land sttn', arg)
   ENDSELECT
   cval = '....:hhim:Elevation of mobile station (m) ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

! ----------------------------------------------------------------------

SUBROUTINE code_satob_latlng ( arg, la0, ln0, lat, lng, lat_sgn, lng_sgn )
   USE record_def
   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   INTEGER , INTENT ( IN )               :: la0 , ln0, lat_sgn, lng_sgn
   INTEGER , INTENT ( OUT )              :: lat , lng

   INTEGER                  :: ival
   REAL                     :: rval
   CHARACTER ( LEN = rlen ) :: cval
   CALL code_table_MMMM ( 'YYXX satob_latlng', ival, rval, cval, 'APPEND' )

   lat = add ( la0 , multiply ( lat_sgn , str2int ( arg (1:1) ) ) )
   CALL record_appendi ( multiply ( 100, lat ) , 'SAT.:BBBLa:Latitude (N)' )
   lng = add ( ln0 , multiply ( lng_sgn , str2int ( arg (2:2) ) ) )
   CALL record_appendi ( multiply ( 100, lng ) , 'SAT.:BBBLn:Longitude (E)' )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_LTLN ( arg , ival , rval , cval , append )
   USE record_def
   USE bulletin_def
   IMPLICIT NONE
   CHARACTER ( LEN = rlen ) :: cget
   INTEGER                  :: iget
   REAL                     :: rget
   INCLUDE 'inc.code_tables'

   cval = 'QcLat and QcLng'
   IF ( arg(1:2) .NE. '99' .OR. LEN_TRIM ( arg ) .NE. 5 ) THEN
      CALL code_error ( '99LL' , 'invalid Latitude' , arg )
      section_id = -1 ; RETURN
   ELSE
      iget = str2int ( arg(3:5) )
      rget = multiply ( iget, 10 )
   ENDIF

   CALL get_next_ngrp_in_mesg ( cget )
   IF ( LEN_TRIM ( cget ) .NE. 5 ) THEN
      section_id = -1 ; RETURN
   ELSE
      ival = str2int ( cget(1:1) )
      rval = multiply ( 10, str2int ( cget(2:5) ) )
   ENDIF
   section_argnum = section_argnum + 1

   SELECT CASE ( ival )
      CASE ( 1 ) ;
      CASE ( 3 ) ; rget = minus(rget)
      CASE ( 5 ) ; rget = minus(rget) ; rval = minus(rval)
      CASE ( 7 ) ;                      rval = minus(rval)
      CASE DEFAULT
         CALL code_error ( 'Long' , 'invalid Longitude' , cget )
         section_id = -1 ; RETURN
   ENDSELECT

   IF ( append .EQ. 'APPEND' ) THEN
      CALL record_appendr ( ival , rget , '3333:QcLat:Latitude (N)' )
      CALL record_appendr ( ival , rval , '3333:QcLng:Longitude (E)' )
   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_MMMM ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = msg_yymmdd
   rval = msg_hhmmss
   cval = '    :=====:New message ' // TRIM(arg) // BLANK_LINE
   WRITE ( cval(1:4) , '(I4)' ) record_fm
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

   msg_stat ( record_fm , number ) = msg_stat ( record_fm , number ) + 1
   record_error = 0
   record_warn = 0

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_NNPPP ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg(1:2) )
   rval = str2int ( arg(3:5) )
   IF ( ival < 0 .OR. rval < 0 ) THEN
      cval = 'invalid <' // arg(:5) // '>'
   ELSE
      cval = '(% and hPa)'
   ENDIF
   cval = 'SAT.:NNPPP:Cloud Cover from Satellite ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_PP ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'

   ival = str2int ( arg(1:2) )
   rval = ival
   cval = '(mb)'
   IF ( ival .EQ. 0 ) THEN
      rval = 100.
   ELSEIF ( ival .EQ. 92 ) THEN
      rval = 92.5
   ELSEIF ( ival < 0 ) THEN
      section_id = -1
      IF ( arg(1:2) .EQ. '//' ) THEN
         cval = 'missing'
         rval = MISSING
      ELSE
         cval = 'invalid <' // arg(:5) // '>'
         rval = UNDEFINED
         CALL code_error ( 'PP', 'Pressure of isobaric sfc', arg )
      ENDIF
   ENDIF
   cval = 'SAT.:PP   :Pressure of isobaric sfc ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_PPhhh ( arg, ival, rval, cval, append, pressure, part )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER , INTENT ( OUT )              :: pressure
   CHARACTER ( LEN = 1 ) , INTENT ( IN ) :: part
   INTEGER                               :: altitude
   ival = str2int ( arg(1:2) )
   pressure = UNDEFINED
   IF ( part .EQ. 'A' ) THEN
      SELECT CASE ( ival )
         CASE ( 00 )
            pressure = 1000
         CASE ( 92 )
            pressure = 925
         CASE ( 85 , 70 , 50 , 40 , 30 , 25 , 20 , 15 , 10 )
            pressure = multiply ( ival , 10 )
      ENDSELECT
   ELSEIF ( part .EQ. 'C' ) THEN
      SELECT CASE ( ival )
         CASE ( 70 , 50 , 30 , 20 , 10 )
            pressure = ival
      ENDSELECT
   ELSE
      cval = 'Not a valid part : ' // part
      CALL code_error ( 'PPhhh', cval , arg )
   ENDIF
   IF ( pressure .EQ. UNDEFINED ) THEN
      cval = 'Not a stardard pressure level : part ' // part
      CALL code_error ( 'PPhhh' , cval , arg )
   ENDIF
   ! h1h1h1 ... hnhnhn
   IF ( pressure > 500 ) THEN
      altitude = str2int ( arg(3:5) )
      IF ( pressure >= 1000 .AND. altitude > 500 ) THEN
         ! geopotential surface under sea level
         altitude = subtract ( 500 , altitude )
      ENDIF
      CALL modify_height_by_stdpre ( pressure , altitude , 1000 )
   ELSE
      altitude = multiply ( str2int ( arg(3:5) ) , 10 )
      CALL modify_height_by_stdpre ( pressure , altitude , 10000 )
   ENDIF
   rval = altitude
   cval = '....:PPhhh:Geopotential Height (m) at specific P sfc'
   IF ( rval .EQ. UNDEFINED ) THEN
      cval = TRIM(cval) // ' invalid <' // arg(:5) // '>' // BLANK_LINE
   ELSEIF ( rval .EQ. MISSING ) THEN
      cval = TRIM(cval) // ' Missing <' // arg(:5) // '>' // BLANK_LINE
   ENDIF
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( pressure , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_QLaLo ( arg , ival , rval , cval , append )
   USE record_def
   USE bulletin_def
   IMPLICIT NONE
   REAL                     :: lat , lng
   INCLUDE 'inc.code_tables'

   ival = str2int ( arg(1:1) )
   lat  = str2int ( arg(2:3) )
   lng  = str2int ( arg(4:5) )
   cval = 'QLat and QLong'
   rval = ival

   IF ( lat < 0 .OR. lat > 90 ) THEN
      CALL code_error ( 'QLaLo' , 'invalid Latitude' , arg )
      section_id = -1 ; RETURN
   ELSEIF ( lng < 0 ) THEN
      CALL code_error ( 'QLaLo' , 'invalid Longitude' , arg )
      section_id = -1 ; RETURN
   ENDIF

   SELECT CASE ( ival )
      CASE ( 0 ) ;                                                 lng = -lng
      CASE ( 1 ) ;              IF ( lng <= 80 ) lng = lng + 100 ; lng = -lng
      CASE ( 2 ) ;              IF ( lng <= 80 ) lng = lng + 100
      CASE ( 3 ) ;
      CASE ( 5 ) ; lat = -lat ;                                    lng = -lng
      CASE ( 6 ) ; lat = -lat ; IF ( lng <= 80 ) lng = lng + 100 ; lng = -lng
      CASE ( 7 ) ; lat = -lat ; IF ( lng <= 80 ) lng = lng + 100
      CASE ( 8 ) ; lat = -lat
      CASE DEFAULT
         CALL code_error ( 'QLaLo' , 'invalid Octant of the global' , arg )
         section_id = -1 ; RETURN
   ENDSELECT

   IF ( append .EQ. 'APPEND' ) THEN
      rval = multiply ( lat , 100 )
      CALL record_appendr ( str2int(arg(1:5)), rval, '3300:QLat :Latitude (N)' )
      rval = multiply ( lng , 100 )
      CALL record_appendr ( str2int(arg(1:5)), rval, '3300:QLong:Longitude (E)' )
   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_R24 ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg )
   cval = '(m)'
   SELECT CASE ( ival )
      CASE ( 0000 : 9997 ) ; rval = TOMETER * 0.1 * ival
      CASE ( 9998 )        ; rval = TOMETER * 0.1 * ival ; cval = '>= 0.9998 m'
      CASE ( 9999 )        ; rval = 0 ; ival = 0 ; cval = 'trace'
      CASE DEFAULT
         IF ( arg .EQ. '////' ) THEN
            cval = 'missing'
            rval = MISSING
         ELSE
            rval = UNDEFINED
            cval = 'invalid <' // arg(:5) // '>'
            CALL code_error ( 'R24_', '24 hr. Precipitation', arg )
         ENDIF
   ENDSELECT
   cval = '....:R24  :Precip. amount & duration 24 hrs preceding obs. ' // cval
   ival = 24
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_SATID ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg(1:3) )
   rval = str2int ( arg(1:5) )
   SELECT CASE ( arg(1:1) )
      CASE ( '0' ) ; cval = 'European Community'
      CASE ( '1' ) ; cval = 'Japanese'
   !  CASE ( '2' ) ; cval = 'US'
      CASE ( '3' ) ; cval = 'USSR'
      CASE ( '4' ) ; cval = 'Indian'
      CASE DEFAULT ; cval = 'unknown ### <' // arg(:5) // '>'
   ENDSELECT

   ! US Satellites
   IF ( arg(1:1) .EQ. '2' ) THEN

      ! Satellite ID
      SELECT CASE ( str2int ( arg(2:3) ) )
         CASE (             0 ) ; cval = 'US not specified'
         CASE ( 11:15 , 32:35 ) ; cval = 'US NOAA-N'
         CASE ( 20:26 , 29    ) ; cval = 'US GOES-X'
         CASE ( 10, 31, 36:39 ) ; cval = 'US TIROS-N'
         CASE DEFAULT           ; cval = 'US unknown ### <' // arg(:5) // '>'
      ENDSELECT

      ! Instrument TYPE
      IF ( arg(2:3) >= '10' .AND. arg(2:3) <= '15' ) THEN
         SELECT CASE ( arg(4:4) )
            CASE ( '0' ) ; cval = TRIM(cval) // ' instrument not specified'
            CASE ( '1' ) ; cval = TRIM(cval) // ' VTPR instr. 1'
            CASE ( '2' ) ; cval = TRIM(cval) // ' VTPR instr. 2'
         ENDSELECT
      ELSEIF ( arg(2:3) >= '31' .AND. arg(2:3) <= '39' ) THEN
         SELECT CASE ( arg(4:4) )
            CASE ( '0' ) ; cval = TRIM(cval) // ' instrument not specified'
            CASE ( '1' ) ; cval = TRIM(cval) // ' HIRS+MSU+SSU'
            CASE ( '2' ) ; cval = TRIM(cval) // ' HIRS+MSU'
            CASE ( '3' ) ; cval = TRIM(cval) // ' HIRS'
            CASE ( '4' ) ; cval = TRIM(cval) // ' HIRS+SSU'
            CASE ( '5' ) ; cval = TRIM(cval) // ' MSU'
            CASE ( '6' ) ; cval = TRIM(cval) // ' MSU+SSU'
            CASE ( '7' ) ; cval = TRIM(cval) // ' SSU'
         ENDSELECT
      ELSE
         cval = TRIM(cval) // ' instrument not specified'
      ENDIF

      ! Path characteristics
      SELECT CASE ( arg(5:5) )
         CASE ( '0' ) ; cval = TRIM(cval) // ' processing not specified'
         CASE ( '1' ) ; cval = TRIM(cval) // ' clear path, regression'
         CASE ( '2' ) ; cval = TRIM(cval) // ' partly cloudy path, regression'
         CASE ( '3' ) ; cval = TRIM(cval) // ' cloudy path, regression'
         CASE ( '4' ) ; cval = TRIM(cval) // ' clear path, regression+QC'
         CASE ( '5' ) ; cval = TRIM(cval) // ' partly cloudy path, reg.+QC'
         CASE ( '6' ) ; cval = TRIM(cval) // ' cloudy path, regression+QC'
      ENDSELECT

   ELSE
      cval = TRIM(cval) // ' No. ' // arg(2:3) // BLANK_LINE

   ENDIF

   cval = 'SAT.:I1223:Satellite ' // cval
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_stnid ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER :: iget
   ival = str2int ( arg )
   rval = ival
   IF ( ival <= 0 ) THEN
      CALL code_error ( 'stnid' , 'invalid station' , arg )
      IF ( append .EQ. 'APPEND' ) &
         CALL record_appendi ( ival , '....:IIiii:Station Id' )
   ELSE
      READ ( stn_data , stn_fmt , rec = ival ) stn_dscrptn
      IF ( ival .EQ. str2int( stn_dscrptn(1:5) ) ) THEN
         cur_sttn_id = ival
         iget = str2int ( stn_dscrptn( 7:11) )
         cval = '....:' // arg(1:5) // ':Latitude (N) WMO ' // stn_dscrptn(31:)
         CALL record_appendi ( iget , cval )
         iget = str2int ( stn_dscrptn(14:19) )
         cval = '....:' // arg(1:5) // ':Longitude(E) WMO ' // stn_dscrptn(31:)
         CALL record_appendi ( iget , cval )
         iget = str2int ( stn_dscrptn(22:26) )
         cur_sttn_alt = iget
         cval = '....:' // arg(1:5) // ':Elevation(m) WMO ' // stn_dscrptn(31:)
         IF ( append .EQ. 'APPEND' ) CALL record_appendi ( iget , cval )
      ELSE
         ival = minus ( ival )
         CALL code_error ( 'stnid' , 'unknown station' , arg )
      ENDIF
   ENDIF
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_TTTaDD ( arg , ival , rval , cval , append , pressure )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER , INTENT ( IN ) :: pressure

   rval = multiply ( str2int(arg(1:3)) , 0.1 )
   IF ( SCAN ( '13579' , arg(3:3) ) > 0 ) rval = minus(rval)
   cval = '3931:TTTa :Temperature (C) at specific P(Z) sfc'
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( pressure , rval , cval )

   cval = '(C) at specific P(Z) sfc'
   SELECT CASE ( arg(4:5) )
      CASE ( '00':'50' ) ; ival = str2int ( arg(4:5) )
      CASE ( '56':'99' ) ; ival = multiply ( 10, subtract ( str2int(arg(4:5)), 50 ) )
      CASE ( '//'      ) ; ival = MISSING
      CASE DEFAULT       ; ival = UNDEFINED
              cval = 'invalid <' // arg(:5) // '>'
              CALL code_error ( 'TTTaDD', 'Dew Point Depression' , arg )
   ENDSELECT
   rval = subtract ( rval , multiply ( 0.1 , ival ) )
   cval = '0777:DnDn :Dew Point ' // cval
   IF ( append .EQ. 'APPEND' ) &
      CALL record_appendr ( pressure, rval , cval )

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_YYGG ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   ival = str2int ( arg(1:4) )
   rval = ival
   IF ( ival < 0 ) THEN
      CALL code_error ( 'YYGG' , 'Observation Day & Hour (UTC) ' , arg )
   ELSEIF (( record_fm >= 32 .AND. record_fm <= 41 ) .OR. record_fm.EQ.81 ) THEN
      IF ( ival > 5000 ) THEN
         rval = KNOT ; ival = ival - 5000
         CALL record_appendr (ival, rval, '....:YYGG :Wind originally in knots')
      ELSE
         rval = 1.
      ENDIF
   ENDIF
   CALL iset_msg_time ( multiply ( ival , 100 ) )
   cval = 'YYGG:YYGG :Observation Day & Hour ( UTC ) '
   IF ( append .EQ. 'APPEND' ) CALL record_appendj (msg_yymmdd,msg_hhmmss,cval)
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_YYGGiw ( arg , ival , rval , cval , append )
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER :: iget
   CALL code_table_YYGG ( arg(1:4) , iget , rval , cval , ' ' )
   CALL code_table_1855 ( arg(5:5) , ival , rval , cval , ' ' )
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( iget , rval , &
      '1855:YYGG :Observation Day & Hour ( UTC ) ' )
ENDSUBROUTINE

!----------------------------------------------------------------------

FUNCTION approx_height ( pre ) RESULT ( gpm )

   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT ( IN )               :: pre
   INTEGER                               :: gpm
   REAL                                  :: rgpm
   INTEGER, PARAMETER                    :: NLVL = 31
   INTEGER                               :: i
   INTEGER, DIMENSION(2*NLVL)            :: pz2 = (/ &
                       1013,     0, &
      1000,   100, &
                        950,   600, &
       925,   850, &
                        900,  1000, &
       850,  1500, &
                        800,  2000, &
                        750,  2500, &
       700,  3100, &
                        650,  3600, &
                        600,  4300, &
                        550,  4900, &
       500,  5800, &
                        450,  6400, &
       400,  7600, &
                        350,  8300, &
       300,  9500, &
       250, 10600, &
       200, 12300, &
       150, 14100, &
       100, 16600, &
        70, 18500, &
        50, 20500, &
        30, 24000, &
        20, 26500, &
        10, 31000, &
                          7, 33500, &
                          5, 36000, &
                          3, 39000, &
                          2, 42000, &
                          1, 48000 /)
   INTEGER, DIMENSION(2,NLVL) :: pz
   EQUIVALENCE ( pz , pz2 )

   IF ( pre .EQ. MISSING .OR. pre .EQ. UNDEFINED ) THEN
      gpm = pre
      RETURN
   ENDIF

   i = 2
   find_i : DO WHILE ( i < nlvl )
      IF ( pz(1,i) .EQ. pre ) THEN
         gpm = pz(2,i)
         RETURN
      ELSEIF ( pre > pz(1,i) ) THEN
         EXIT find_i
      ELSE
         i = i + 1
      ENDIF
   ENDDO find_i

   rgpm = REAL ( pre - pz(1,i) ) / REAL ( pz(1,i) - pz(1,i-1) )
   gpm = pz(2,i) * exp ( rgpm * log ( real(pz(2,i)) / pz(2,i-1) ) )

ENDFUNCTION approx_height

!----------------------------------------------------------------------

SUBROUTINE modify_thickness ( thickness, p0, p1, incr )

   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT ( IN )    :: p0, p1, incr
   INTEGER , INTENT ( INOUT ) :: thickness
   INTEGER                    :: gpm, h0, h1
   INTEGER , EXTERNAL         :: approx_height

   IF ( thickness .EQ. MISSING .OR. thickness .EQ. UNDEFINED ) RETURN

   gpm = approx_height ( p1 ) - approx_height ( p0 )
   h0 = ( gpm / incr - 2 ) * incr + thickness
   h1 = h0 + incr
   DO WHILE ( abs(h1-gpm) <= abs(h0-gpm) )
      h0 = h1
      h1 = h1 + incr
   ENDDO
   thickness = h0

ENDSUBROUTINE modify_thickness

!----------------------------------------------------------------------

SUBROUTINE modify_height_by_stdpre ( pressure , altitude , incr )

! *********** This need to be revisited ****************************
!
! Standard Pre. (hPa)  850  700  500  400  300  250  200  150  100
! Approx. Height (km)  1.5  3.1  5.8  7.6  9.5 10.6 12.3 14.1 16.6
!
! Standard Pre. (hPa)  100   70   50   30   20   10  .... 925
! Approx. Height (km) 16.6 18.5 20.5 24.0 26.5 31.0  .... .85
!
! *********** This need to be revisited ****************************

   USE record_def
   IMPLICIT NONE
   INTEGER , INTENT ( IN )             :: pressure , incr
   INTEGER , INTENT ( INOUT )          :: altitude
   INTEGER                             :: i, h0, h1

   INTEGER, PARAMETER                  :: NLVL = 16
   INTEGER, DIMENSION(NLVL), PARAMETER :: pre = (/ 1000,        &
          925,   850,   700,   500,   400,   300,   250,   200, &
          150,   100,    70,    50,    30,    20,    10 /)
   INTEGER, DIMENSION(NLVL), PARAMETER :: gpm = (/    0,        &
          850,  1500,  3100,  5800,  7600,  9500, 10600, 12300, &
        14100, 16600, 18500, 20500, 24000, 26500, 31000 /)

   IF ( altitude .EQ. MISSING .OR. altitude .EQ. UNDEFINED ) RETURN

   DO i = 1, NLVL
      IF ( pressure .EQ. pre(i) ) THEN
         h0 = ( gpm(i) / incr - 2 ) * incr + altitude
         h1 = h0 + incr
         DO WHILE ( abs(h1-gpm(i)) <= abs(h0-gpm(i)) )
            h0 = h1
            h1 = h1 + incr
         ENDDO
         altitude = h0
         RETURN
      ENDIF
   ENDDO

   CALL code_error ( 'modify_height_by_stdpre' , 'invalid Pressure' , &
                     int2str(pressure) )

ENDSUBROUTINE

! ---------------------------------------------------------------------

SUBROUTINE code_error ( routine_name , description , value )

   ! ‘sort of’ generic error handling routine 1, see also code_warn
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: routine_name ! calling routine name
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: description  ! of offending variable
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: value        ! of offending variable
   REAL                                  :: rval

   IF ( bulletin_error .EQ. 0 ) THEN
      bul_stat ( record_fm , error ) = bul_stat ( record_fm , error ) + 1
      ! WRITE ( ierr , 1000 ) bul_stat ( record_fm , number )
      WRITE ( ierr , 1000 ) seqnum
   ENDIF
   bulletin_error = bulletin_error + 1

   IF ( record_error .EQ. 0 ) THEN
      msg_stat ( record_fm , error ) = msg_stat ( record_fm , error ) + 1
   ENDIF
   record_error   = record_error + 1

   WRITE ( ierr,  1001 ) record_fm, section_id, section_argnum, &
         section_subgrp, routine_name, TRIM(description), TRIM(value)

   WRITE ( ierr0, 1001 ) record_fm, section_id, section_argnum, &
         section_subgrp, routine_name, TRIM(description), TRIM(value)

   rval = section_argnum + 0.01 * record_fm
   CALL record_appendr ( section_id , rval , '#### <' //        &
      TRIM ( value ) // '> ' // TRIM ( routine_name ) // ':' // &
      TRIM ( description ) )

1000 FORMAT ( ' >>>>>> ', I5, ' <<<<<<' )
1001 FORMAT ( ' #### INVALID (FM=',I3,',SCTN=',I3,',ARGN=',I3,',SGRP=', &
              I3,') ', A, ':', A, ' ==> ', A )

ENDSUBROUTINE code_error

! -----------------------------------------------------------------------

SUBROUTINE code_warn ( routine_name , description , value )

   ! ‘sort of’ generic error handling routine 2, see also code_error
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: routine_name ! calling routine name
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: description  ! of ignored variable
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: value        ! of ignored variable
   REAL                                  :: rval

   IF ( bulletin_warn .EQ. 0 ) THEN
      bul_stat ( record_fm , warning ) = bul_stat ( record_fm , warning ) + 1
      ! WRITE ( iwarn , 1000 ) bul_stat ( record_fm , number )
      WRITE ( ierr , 1000 ) seqnum
   ENDIF
   bulletin_warn = bulletin_warn + 1

   IF ( record_warn .EQ. 0 ) THEN
      msg_stat ( record_fm , warning ) = msg_stat ( record_fm , warning ) + 1
   ENDIF
   record_warn   = record_warn + 1

   WRITE ( iwarn,  1001 ) record_fm, section_id, section_argnum, &
         section_subgrp, routine_name, TRIM(description), TRIM(value)

   WRITE ( iwarn0, 1001 ) record_fm, section_id, section_argnum, &
         section_subgrp, routine_name, TRIM(description), TRIM(value)

   rval = section_argnum + 0.01 * record_fm
   CALL record_appendr ( section_id , rval , 'WWWW <' //        &
      TRIM ( value ) // '> ' // TRIM ( routine_name ) // ':' // &
      TRIM ( description ) )

1000 FORMAT ( ' >>>>>> ', I5, ' <<<<<<' )
1001 FORMAT ( ' WWWW WARNING (FM=',I3,',SCTN=',I3,',ARGN=',I3,',SGRP=', &
              I3,') ', A, ':', A, ' ==> ', A )

ENDSUBROUTINE code_warn

! -----------------------------------------------------------------------

SUBROUTINE code_ignore ( routine_name , description , value )

   ! ‘sort of’ generic error handling routine 2, see also code_error
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: routine_name ! calling routine name
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: description  ! of ignored variable
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: value        ! of ignored variable
   REAL                                  :: rval

   WRITE ( iwarn,  1001 ) record_fm, section_id, section_argnum, &
         section_subgrp, routine_name, TRIM(description), TRIM(value)

   WRITE ( iwarn0, 1001 ) record_fm, section_id, section_argnum, &
         section_subgrp, routine_name, TRIM(description), TRIM(value)

   rval = section_argnum + 0.01 * record_fm
   CALL record_appendr ( section_id , rval , 'IIII <' //        &
      TRIM ( value ) // '> ' // TRIM ( routine_name ) // ':' // &
      TRIM ( description ) )

1001 FORMAT ( ' IIII IGNORED (FM=',I3,',SCTN=',I3,',ARGN=',I3,',SGRP=', &
              I3,') ', A, ':', A, ' ==> ', A )

ENDSUBROUTINE code_ignore


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
      CALL bulletin_skip ( ETX )
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
      CALL bulletin_skip ( ETX )
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
FUNCTION gts_fm_num ( keyword , keytype )

   ! FUNCTION to identify the code form according to a <keyword>
   !    and a <keytype>. Valid Keytype = 'CODE_HEADER' , 'STATION_ID'.
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   CHARACTER ( LEN = *) , INTENT ( IN ) :: keyword , &
                                           keytype
   INTEGER                              :: gts_fm_num

   gts_fm_num = 99

   SELECT CASE ( keytype )

   CASE ( 'STATION_ID' )
      IF     ( keyword(1:4) .EQ. 'BMAA' ) THEN ! Adminstration Messages
         gts_fm_num = 95
      ELSEIF ( keyword(1:1) .EQ. 'W'    ) THEN ! Textual TS Warnings
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'TPPN' ) THEN ! Additional TS Warnings
         gts_fm_num = 94
      ELSEIF ( keyword(1:2) .EQ. 'AB'   ) THEN ! Additional TS Warnings
         gts_fm_num = 94
      ELSEIF ( keyword(1:2) .EQ. 'UA'   ) THEN ! other ARPs
         gts_fm_num = 96
      ELSEIF ( keyword(1:2) .EQ. 'AA'   ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:3) .EQ. 'RWK'  ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'FP'   ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'SE'   ) THEN ! Seismic activity report
         gts_fm_num = 93
      ENDIF

   CASE ( 'CODE_HEADER' )
      SELECT CASE ( keyword )

         CASE ( 'SATELLITE' ) ;                       gts_fm_num = 94
         CASE ( 'RSMC' ) ;                            gts_fm_num = 94
         CASE ( 'GEOALERT' ) ;                        gts_fm_num = 93
         CASE ( 'ARP' ) ;                             gts_fm_num = 96
         CASE ( 'AIREP' ) ;                           gts_fm_num = 97
         CASE ( 'NIL' ) ;                             gts_fm_num = 98
         CASE ( 'NIL=' ) ;                            gts_fm_num = 98

         CASE ( 'AAXX' ) ;                            gts_fm_num = 12
         CASE ( 'BBXX' ) ;                            gts_fm_num = 13
         CASE ( 'METAR' ) ;                           gts_fm_num = 15
         CASE ( 'SPECI' ) ;                           gts_fm_num = 16
         CASE ( 'ZZYY' ) ;                            gts_fm_num = 18

         CASE ( 'FFAA' , 'FFBB' , 'GGAA' , 'GGBB' ) ; gts_fm_num = 20
         CASE ( 'RADREP' ) ;                          gts_fm_num = 22

         CASE ( 'PPAA' , 'PPBB' , 'PPCC' , 'PPDD' ) ; gts_fm_num = 32
         CASE ( 'QQAA' , 'QQBB' , 'QQCC' , 'QQDD' ) ; gts_fm_num = 33
         CASE ( 'EEAA' , 'EEBB' , 'EECC' , 'EEDD' ) ; gts_fm_num = 34
         CASE ( 'TTAA' , 'TTBB' , 'TTCC' , 'TTDD' ) ; gts_fm_num = 35
         CASE ( 'UUAA' , 'UUBB' , 'UUCC' , 'UUDD' ) ; gts_fm_num = 36
         CASE ( 'XXAA' , 'XXBB' , 'XXCC' , 'XXDD' ) ; gts_fm_num = 37
         CASE ( 'IIAA' , 'IIBB' , 'IICC' , 'IIDD' ) ; gts_fm_num = 38

         CASE ( 'RRXX' ) ;                            gts_fm_num = 39

         CASE ( 'SSXX' ) ;                            gts_fm_num = 40
         CASE ( 'LLXX' ) ;                            gts_fm_num = 41
         CASE ( 'AMDAR' ) ;                           gts_fm_num = 42
         CASE ( 'ICEAN' ) ;                           gts_fm_num = 44
         CASE ( '10001' , '65556' ) ;                 gts_fm_num = 45
         CASE ( 'GRID' ) ;                            gts_fm_num = 47
         CASE ( 'GRAF' ) ;                            gts_fm_num = 49

         CASE ( 'WINTEM' ) ;                          gts_fm_num = 50
         CASE ( 'TAF' ) ;                             gts_fm_num = 51
         CASE ( 'ARFOR' ) ;                           gts_fm_num = 53
         CASE ( 'ROFOR' ) ;                           gts_fm_num = 54
         CASE ( 'RADOF' ) ;                           gts_fm_num = 57

         CASE ( 'MAFOR' ) ;                           gts_fm_num = 61
         CASE ( 'NNXX' ) ;                            gts_fm_num = 62
         CASE ( 'JJXX' ) ;                            gts_fm_num = 63
         CASE ( 'KKXX' ) ;                            gts_fm_num = 64
         CASE ( 'MMXX' ) ;                            gts_fm_num = 65
         CASE ( 'HHXX' ) ;                            gts_fm_num = 67
         CASE ( 'HYFOR' ) ;                           gts_fm_num = 68

         CASE ( 'CLIMAT' ) ;                          gts_fm_num = 71
         CASE ( 'CLINP' , 'CLISA' ) ;                 gts_fm_num = 73
         CASE ( 'INCLI' , 'NACLI' , 'SPCLI' ) ;       gts_fm_num = 73

         CASE ( 'SFLOC' ) ;                           gts_fm_num = 82
         CASE ( 'SFAZU' ) ;                           gts_fm_num = 83
         CASE ( 'CCAA' , 'CCBB' , 'DDAA' ) ;          gts_fm_num = 85
         CASE ( 'VVAA' , 'VVBB' , 'VVCC' , 'VVDD' ) ; gts_fm_num = 86
         CASE ( 'WWXX' ) ;                            gts_fm_num = 87
         CASE ( 'YYXX' ) ;                            gts_fm_num = 88

      ENDSELECT

   ENDSELECT

ENDFUNCTION gts_fm_num

SUBROUTINE identify_code_form

   ! SUBROUTINE to identify the code TYPE of the bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER , EXTERNAL       :: gts_fm_num
   CHARACTER ( LEN = llen ) :: header_lines ( 4 ) ,  &
                               rest ,                &
                               arg
   INTEGER                  :: lcount
   TYPE ( lines ) , POINTER :: btmp

   IF ( TRACE_ALL ) PRINT  * , 'identify_code_form : in'

   btmp => bulletin 

   read_4_lines : DO lcount = 1 , 4
      IF ( TRIM ( btmp%text ) .EQ. ETX ) THEN
         record_fm = 99
         RETURN
      ELSE
         header_lines ( lcount ) = btmp%text
         btmp => btmp%next_line
      ENDIF
   ENDDO read_4_lines

   CALL parse_line ( header_lines(4) , ' ' , arg , rest )
   record_fm = gts_fm_num ( TRIM ( arg ) , 'CODE_HEADER' )
   IF ( record_fm .EQ. 99 ) THEN
      record_fm = gts_fm_num ( TRIM ( header_lines(3) ) , 'STATION_ID' )
   ENDIF

   IF ( TRACE_MOST ) PRINT  * , 'identify_code_form : out ' , record_fm

ENDSUBROUTINE identify_code_form

SUBROUTINE interpret_bulletin

   ! Driver Routine for interpreting bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   LOGICAL                  :: decoded

   IF ( TRACE_MOST ) PRINT  * , 'interpret_bulletin : in ' , record_fm

   bul_stat ( record_fm, number ) = bul_stat ( record_fm, number ) + 1
   bulletin_error = 0
   bulletin_warn  = 0

   CALL record_initialize ( record_fm )
   CALL assign_outfile ( 0 )
   IF ( outfiles ( record_fm ) .LT. 0 ) THEN
      CALL prog_abort(' ===> Unexpected condition, outfiles(record_fm) < 0 ')
   ENDIF

   CALL bulletin_print ( outfiles ( record_fm ) )

   SELECT CASE ( record_fm )

      CASE ( 12 , 13 ) ; decoded = .TRUE. 
                         CALL assign_outfile ( 23  ) ; CALL decode_fm12
                         CALL assign_outfile ( 456 ) ; CALL write_fm12
                         CALL assign_outfile ( 789 ) ; CALL mm5wr_fm12
                                         
      CASE ( 32 : 34 ) ; decoded = .TRUE.
                         CALL assign_outfile ( 23  ) ; CALL decode_fm32
                         CALL assign_outfile ( 456 ) ; CALL write_fm35
                         CALL assign_outfile ( 789 ) ; CALL mm5wr_fm35
                                         
      CASE ( 35 : 38 ) ; decoded = .TRUE.
                         CALL assign_outfile ( 23  ) ; CALL decode_fm35
                         CALL assign_outfile ( 456 ) ; CALL write_fm35
                         CALL assign_outfile ( 789 ) ; CALL mm5wr_fm35
                                         
      CASE ( 86      ) ; decoded = .TRUE.
                         CALL assign_outfile ( 23  ) ; CALL decode_fm86
                         CALL assign_outfile ( 456 ) ; CALL write_fm86

      CASE ( 88      ) ; decoded = .TRUE.
                         CALL assign_outfile ( 23  ) ; CALL decode_fm88
                         CALL assign_outfile ( 456 ) ; CALL write_fm88
                         CALL assign_outfile ( 789 ) ; CALL mm5wr_fm88

      CASE ( 97      ) ; decoded = .TRUE.
                         CALL assign_outfile ( 23  ) ; CALL decode_airep
                         CALL assign_outfile ( 789 ) ; CALL mm5wr_airep

      CASE DEFAULT     ; decoded = .FALSE.

   ENDSELECT

   IF ( decoded ) THEN

      CALL assign_outfile ( 1 )

      ! input bulletin as the decoder sees it
      CALL bulletin_print ( outfiles ( record_fm+100 ) )

      ! decoded text
      CALL record_print ( outfiles ( record_fm+100 ) )

      ! error(s) or warning(s) found, PRINT output to appropriate files
      IF ( TRACE_LEAST ) THEN
         IF ( bulletin_error  > 0 ) THEN
            CALL bulletin_print ( ierr )
            CALL record_print   ( ierr )
            CALL bulletin_print ( ierr0 )
            CALL record_print   ( ierr0 )
         ENDIF
      ENDIF

      IF ( TRACE_MOST ) THEN
         IF ( bulletin_warn > 0 ) THEN
            CALL bulletin_print ( iwarn )
            CALL record_print   ( iwarn )
            CALL bulletin_print ( iwarn0 )
            CALL record_print   ( iwarn0 )
         ENDIF
      ENDIF

   ENDIF

   IF ( FLUSH_FILES ) CALL flush_outfiles

   IF ( TRACE_ALL ) PRINT  * , 'interpret_bulletin : out'

ENDSUBROUTINE interpret_bulletin

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
FUNCTION add_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_ir ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_ri ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

FUNCTION add_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 + val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION subtract_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_ir ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_ri ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

FUNCTION subtract_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 - val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION multiply_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_ir ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_ri ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

FUNCTION multiply_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = val1 * val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION divide_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0 ) THEN
      IF ( val1.EQ.0 ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_ir ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0. ) THEN
      IF ( val1.EQ.0 ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_ri ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(val2).EQ.ABS_UNDEFINED .OR. ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val2).EQ.ABS_MISSING .OR. ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0 ) THEN
      IF ( val1.EQ.0. ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

FUNCTION divide_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSEIF ( val2.EQ.0. ) THEN
      IF ( val1.EQ.0. ) THEN
         PRINT *, '0/0, maybe you should try some elementry calculus'
      ENDIF
      val = UNDEFINED
   ELSE
      val = val1 / val2
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION remainder_ii ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER , INTENT(in)  :: val2
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED .OR. ABS(val2).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING .OR. ABS(val2).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = mod ( val1 , val2 )
   ENDIF
ENDFUNCTION

FUNCTION remainder_rr ( val1 , val2 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL    , INTENT(in)  :: val2
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED .OR. ABS(NINT(val2)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING .OR. ABS(NINT(val2)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = mod ( val1 , val2 )
   ENDIF
ENDFUNCTION

!----------------------------------------------------------------------

FUNCTION minus_i ( val1 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   INTEGER , INTENT(in)  :: val1
   INTEGER               :: val
   IF ( ABS(val1).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(val1).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = -val1
   ENDIF
ENDFUNCTION

FUNCTION minus_r ( val1 ) RESULT ( val )
   INCLUDE 'inc.special_symbols'
   REAL    , INTENT(in)  :: val1
   REAL                  :: val
   IF ( ABS(NINT(val1)).EQ.ABS_UNDEFINED ) THEN
      val = UNDEFINED
   ELSEIF ( ABS(NINT(val1)).EQ.ABS_MISSING ) THEN
      val = MISSING
   ELSE
      val = -val1
   ENDIF
ENDFUNCTION


SUBROUTINE decode_fm12

   ! Driver for decoding FM-12 format

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival, sttn_op, ro_ignore
   REAL                     :: rval, wind_unit
   CHARACTER ( LEN = llen ) :: cval, arg

   IF ( TRACE_MOST ) PRINT  * , 'decode_fm12 : in '

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

MODULE surface_observations

   INTEGER :: obs_yymmdd , obs_hhmmss

   TYPE surface_obs
      CHARACTER ( LEN = 60 ) :: sttn_name
      INTEGER :: rev_yymmdd, &
                 rev_hhmmss, &
                 station_id
      REAL    :: latitude,   &
                 longitude,  &
                 elevation,  &
                 sttn_pre,   &
                 mslp,       &
                 temp,       &
                 dewpt,      &
                 speed,      &
                 direction,  &
                 precip
   ENDTYPE surface_obs

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE clear_surface_obs ( surf )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   TYPE ( surface_obs ) , INTENT ( INOUT ) :: surf

   surf%sttn_name  = BLANK_LINE
   surf%station_id = MISSING
   surf%rev_yymmdd = MISSING
   surf%rev_hhmmss = MISSING
   surf%latitude   = MISSING
   surf%longitude  = MISSING
   surf%elevation  = MISSING
   surf%sttn_pre   = MISSING
   surf%mslp       = MISSING
   surf%temp       = MISSING
   surf%dewpt      = MISSING
   surf%speed      = MISSING
   surf%direction  = MISSING
   surf%precip     = MISSING

ENDSUBROUTINE clear_surface_obs

!----------------------------------------------------------------------

SUBROUTINE write_surface_obs ( iwrite , surf )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   INCLUDE 'inc.formats'
   INTEGER , INTENT ( IN )              :: iwrite
   TYPE ( surface_obs ) , INTENT ( IN ) :: surf

   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_surface_obs IOUNIT < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   IF ( surf%rev_yymmdd .NE. MISSING ) THEN
      WRITE ( iwrite , fmt=SYNOPFMT ) surf, obs_yymmdd, obs_hhmmss
   ENDIF
ENDSUBROUTINE write_surface_obs

!----------------------------------------------------------------------

ENDMODULE surface_observations

!----------------------------------------------------------------------

SUBROUTINE write_fm12

   USE record_def
   USE surface_observations
   IMPLICIT NONE

   TYPE ( stack ) , POINTER               :: rtmp
   TYPE ( surface_obs )                   :: surf

   INTEGER                                :: iwrite , fm
   LOGICAL                                :: used

   IF ( TRACE_MOST ) PRINT  * , 'write_fm12 : in ', record_fm
   iwrite = outfiles ( record_fm+400 )

   rtmp => record

   obs_yymmdd = MISSING
   obs_hhmmss = MISSING
   CALL clear_surface_obs ( surf )

   DO WHILE ( ASSOCIATED ( rtmp ) )

      SELECT CASE ( rtmp%field_cval(12:20) )

         CASE ( 'CODE FORM' ) ; used = .TRUE.
                                fm     = rtmp%field_ival
                                iwrite = outfiles ( fm+400 )
                                IF ( record_fm .NE. fm ) THEN
                                   PRINT *,' Unexpected mixing of fm, ', &
                                      record_fm, fm
                                ENDIF

         CASE ( 'CODE HEAD' ) ; used = .TRUE.
                                obs_yymmdd      = rtmp%field_ival
                                obs_hhmmss      = INT(rtmp%field_rval)

         CASE ( 'Observati' ) ; used = .TRUE.
                                surf%rev_yymmdd = rtmp%field_ival
                                surf%rev_hhmmss = INT(rtmp%field_rval)

         CASE ( 'New messa' ) ; used = .TRUE.
                                CALL write_surface_obs ( iwrite , surf )
                                CALL clear_surface_obs ( surf )
                                surf%rev_yymmdd = rtmp%field_ival
                                surf%rev_hhmmss = INT(rtmp%field_rval)

         CASE ( 'Platform ' ) ; used = .TRUE.
            surf%station_id = -6666666
            surf%sttn_name  = rtmp%field_cval(12:)

         CASE ( 'SHIP or T' ) ; used = .TRUE.
            surf%station_id = -7777777
            surf%sttn_name  = rtmp%field_cval(12:)

         CASE ( 'Latitude ' ) ; used = .TRUE.
                                surf%latitude   = rtmp%field_rval/100

         CASE ( 'Longitude' ) ; used = .TRUE.
            surf%longitude  = rtmp%field_rval/100
            IF ( fm .EQ. 12 ) THEN
               surf%station_id = str2int(rtmp%field_cval(6:10))
               surf%sttn_name  = rtmp%field_cval(35:rlen)
            ENDIF

         CASE ( 'Elevation' ) ; used = .TRUE.
                                surf%elevation  = rtmp%field_rval

         CASE ( 'Air Tempe' ) ; used = .TRUE.
                                surf%temp  = add ( TOKELVIN , rtmp%field_rval )

         CASE ( 'Dew Point' ) ; used = .TRUE.
                                surf%dewpt = add ( TOKELVIN , rtmp%field_rval )

         CASE ( 'Wind dire' ) ; used = .TRUE.
                                surf%direction  = rtmp%field_rval

         CASE ( 'Wind spee' ) ; used = .TRUE.
                                surf%speed      = rtmp%field_rval

         CASE ( 'Mean SLP ' ) ; used = .TRUE.
            surf%mslp       = multiply ( rtmp%field_rval , TOPa )

         CASE ( 'Station p' ) ; used = .TRUE.
            surf%sttn_pre   = multiply ( rtmp%field_rval , TOPa )

         CASE ( 'Precip. a' ) ; used = .TRUE.
                                surf%precip     = rtmp%field_rval

         CASE ( 'Rev. Obse' ) ; used = .TRUE.
                                surf%rev_yymmdd = rtmp%field_ival
                                surf%rev_hhmmss = INT(rtmp%field_rval)

         CASE DEFAULT         ; used = .FALSE.

      ENDSELECT


      IF ( used ) THEN
         WRITE ( outfiles ( record_fm+600 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
      ELSE
         WRITE ( outfiles ( record_fm+500 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
      ENDIF

      rtmp => rtmp%next_record

   ENDDO

   CALL write_surface_obs ( iwrite , surf )

   IF ( TRACE_ALL ) PRINT  * , 'write_fm12 : out'

ENDSUBROUTINE write_fm12

SUBROUTINE mm5wr_fm12

   USE record_def
   USE mm5obs_def
   IMPLICIT NONE

   TYPE ( stack ) , POINTER  :: rtmp
   TYPE ( mm5_hdr )          :: hdr
   TYPE ( meas_data )        :: upa

   INTEGER  :: iwrite , ieor
   LOGICAL  :: used

   IF ( TRACE_MOST ) PRINT  * , 'mm5wr_fm12 : in '
   iwrite = outfiles ( record_fm+700 )

   rtmp => record

   obs_yymmdd = missing
   obs_hhmmss = missing

   hdr    = empty_hdr
   upa    = empty_upa
   nvalid = 0
   hdr%info%source = 'GTS (NOAA) ' // msg_header

   upa%pressure%data = fake_stnpre   ! only for fm12
 
   DO WHILE ( ASSOCIATED ( rtmp ) )

    !---------------------------------------------------------------

    SELECT CASE ( rtmp%field_cval(12:20) )

     CASE('CODE FORM'); used = .TRUE.
                        fm = rtmp%field_ival
                        iwrite = outfiles ( fm+700 )
 
     !---------------------------------------------------------------
     ! Time information
    
     CASE('CODE HEAD'); used = .TRUE.
                        obs_yymmdd = rtmp%field_ival
                        obs_hhmmss = NINT(rtmp%field_rval)
 
     CASE('Observati'); used = .TRUE.
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)
 
     CASE('New messa'); used = .TRUE.
                        CALL write_rpt_hdr ( iwrite , seqnum, hdr , ieor )
                        CALL write_rpt_upa ( iwrite , upa , ieor )
                        CALL write_rpt_end ( iwrite , record_error, record_warn , ieor )

                        hdr    = empty_hdr
                        upa    = empty_upa
                        nvalid = 0
                        hdr%info%source = 'GTS (NOAA) ' // msg_header

                        upa%pressure%data = fake_stnpre     ! only for fm12
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     CASE('Rev. Obse'); used = .TRUE.
                        nvalid = nvalid + 1
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     !---------------------------------------------------------------
     ! location information 
 
     CASE('Platform '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = '-6666'
                        hdr%location%name = rtmp%field_cval(12:)
 
     CASE('SHIP or T'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = '-7777'
                        hdr%location%name = rtmp%field_cval(12:)
 
     CASE('Latitude '); used = .TRUE.
                        hdr%location%latitude = rtmp%field_rval/100
 
     CASE('Longitude'); used = .TRUE.
                        hdr%location%longitude = rtmp%field_rval/100
                        IF ( fm .EQ. 12 ) THEN
                           hdr%location%id = rtmp%field_cval(6:10)
                           hdr%location%name = rtmp%field_cval(35:rlen)
                        ENDIF

     !---------------------------------------------------------------
     ! Source information

     CASE('Elevation'); used = .TRUE.
                        hdr%info%elevation = rtmp%field_rval
                        upa%height%data = rtmp%field_rval
 
     !---------------------------------------------------------------
     ! Terrestrial information
     !     slp*f, ref_pres*f, ground_t*f, sst*f, psfc*f,
     !     precip*f, t_max*f, t_min*f, p_tend03*f,
     !     cloud_low*f, cloud_med*f, cloud_hi*f, ceiling*f

     CASE('Mean SLP '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%slp%data = multiply ( rtmp%field_rval , TOPa )
 
     ! Decoded Ground Temperature look very strange, skipping it
   ! CASE('Ground / '); used = .TRUE.
                        nvalid = nvalid + 1
   !                    hdr%ground%ground_t%data = add ( TOKELVIN , rtmp%field_rval )

     CASE('Sea Surfa'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%sst%data = add ( TOKELVIN , rtmp%field_rval )

     CASE('Station p'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%psfc%data = multiply ( rtmp%field_rval , TOPa )
                        upa%pressure%data = multiply ( rtmp%field_rval , TOPa )

     CASE('Precip. a'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%precip%data = rtmp%field_rval

     CASE('Max Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%t_max%data = add ( TOKELVIN , rtmp%field_rval )
 
     CASE('Min Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%t_min%data = add ( TOKELVIN , rtmp%field_rval )
 
     CASE('Nght Tmin'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%t_min_night%data = add ( TOKELVIN , rtmp%field_rval )

     CASE('Pre. Tend'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%p_tend03%data = rtmp%field_rval
 
     CASE('24-h Pre.'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%p_tend24%data = rtmp%field_rval
 
     CASE('Total Clo'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%cloud_cvr%data = rtmp%field_rval
 
     CASE('Lowest cl'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%ceiling%data = rtmp%field_rval
 
     !---------------------------------------------------------------
     ! Meteorological information

     CASE('Air Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        upa%temperature%data = add ( TOKELVIN , rtmp%field_rval )
 
     CASE('Dew Point'); used = .TRUE.
                        nvalid = nvalid + 1
                        upa%dew_point%data = add ( TOKELVIN , rtmp%field_rval )
 
     CASE('Wind dire'); used = .TRUE.
                        nvalid = nvalid + 1
                        upa%direction%data = rtmp%field_rval
 
     CASE('Wind spee'); used = .TRUE.
                        nvalid = nvalid + 1
                        upa%speed%data = rtmp%field_rval

     !---------------------------------------------------------------
     ! Default action
 
     CASE DEFAULT     ; used = .FALSE.

    ENDSELECT

    !---------------------------------------------------------------

    IF ( used ) THEN
       WRITE ( outfiles ( record_fm+800 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
    ELSE
       WRITE ( outfiles ( record_fm+900 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
    ENDIF

    rtmp => rtmp%next_record

   ENDDO

   CALL write_rpt_hdr ( iwrite , seqnum, hdr , ieor )
   CALL write_rpt_upa ( iwrite , upa , ieor )
   CALL write_rpt_end ( iwrite , record_error, record_warn , ieor )

   IF ( TRACE_ALL ) PRINT  * , 'mm5wr_fm12 : out'

ENDSUBROUTINE mm5wr_fm12

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

   ! decode part of the bulletin that is not part of the CODE-FORM
   CALL decode_bulletin_header ( ddhhmm )

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
                  pressure = divide ( pressure , 10 )
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
            pressure = divide ( pressure , 10 )
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

SUBROUTINE decode_fm35

   ! Driver for decoding FM-35 format

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival, ddhhmm
   REAL                     :: rval
   CHARACTER ( LEN = llen ) :: cval, arg

   CHARACTER ( LEN = 1 )    :: part
   REAL                     :: wind_unit
   INTEGER                  :: last_wind_lvl , pressure , sttn_pre

   IF ( TRACE_MOST ) PRINT * , 'decode_fm35 : in '

   ! decode part of the bulletin that is not part of the CODE-FORM
   CALL decode_bulletin_header ( ddhhmm )

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
            CALL decode_fm35_words ( arg )  ! decode the word
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

SUBROUTINE decode_fm35_words ( arg )

   IMPLICIT NONE
   CHARACTER ( LEN = * ) , INTENT ( IN ) :: arg
   REAL                                  :: rval , rget
   INTEGER                               :: ival , iget
   CHARACTER ( LEN = rlen )              :: cval , cget

   IF ( TRACE_MOST ) PRINT *,'decode_fm35_words: in ', &
      TRIM(arg), ' ', section_id, section_argnum

   IF ( arg .EQ. 'NIL' .OR. arg .EQ. 'nil' .OR. arg .EQ. 'MIS' ) RETURN

   ! Mandatory groups are sequence indicated (oriented), must be decoded
   !   before numerial indicated groupts
   !
   IF ( section_id .EQ. 1 ) THEN
      CALL decode_fm35_sec1 ( arg ) ! Mandatory : FM-35 to 38
      RETURN

   ELSEIF ( section_id .EQ. 2 ) THEN
      CALL decode_fm35_sec2 ( arg ) ! Mandatory : FM-35 FM-37
      IF ( section_subgrp .NE. 5 ) RETURN

   ENDIF

   ! Mandatory groups finished, check for new section header
   select_section_headers:SELECT CASE ( arg )

      CASE ( '21212' )
         IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 6 )
            RETURN
         ENDIF

      CASE ( '31313' )
         IF ( part .EQ. 'B' ) THEN
            CALL new_section ( 7 )
            RETURN
         ENDIF

      CASE ( '41414' )
         IF ( part .EQ. 'B' ) THEN
            CALL new_section ( 8 )
            RETURN
         ENDIF

      CASE ( '51515' , '52525' , '53535' , '54545' , &
             '56565' , '57575' , '58585' , '59595' )
         ! the standard 1988 edition code form specified this group
         ! should only appear in sections 'B' and 'D'. However, in
         ! practise, the '51515' group is also seen in section 'A'
         ! IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 9 )
            RETURN
         ! ENDIF

      CASE ( '61616' , '62626' , '63636' , '64646' , &
             '65656' , '67676' , '68686' , '69696' )
         IF ( part .EQ. 'B' .OR. part .EQ. 'D' ) THEN
            CALL new_section ( 10 )
            RETURN
         ENDIF

      CASE ( 'TTAA' , 'TTBB' , 'TTCC' , 'TTDD' , &
             'UUAA' , 'UUBB' , 'UUCC' , 'UUDD' , &
             'XXAA' , 'XXBB' , 'XXCC' , 'XXDD' , &
             'IIAA' , 'IIBB' , 'IICC' , 'IIDD' )
         CALL new_section ( 1 )
         CALL decode_fm35_sec1 ( arg )
         RETURN

      CASE DEFAULT

         SELECT CASE ( arg(1:2) )

         CASE ( '99' )
            IF ( part .EQ. 'A' .AND. section_id < 2 ) THEN
               CALL new_section ( 2 )
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

MODULE upa_observations

   INTEGER :: obs_yymmdd , obs_hhmmss , vert_id , nlevels
   LOGICAL :: header_written = .FALSE.

   TYPE upa_var
      REAL :: pres, geop, temp, dewpt, speed, dir
   ENDTYPE

   TYPE upa_obs
      CHARACTER ( LEN = 60 ) :: sttn_name
      INTEGER          :: rev_yymmdd, rev_hhmmss, station_id
      REAL             :: latitude,   longitude , elevation
      TYPE ( upa_var ) :: obs
   ENDTYPE upa_obs

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE clear_upa_obs_var ( upaobs )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   TYPE ( upa_var ) , INTENT ( OUT ) :: upaobs
   upaobs%pres   = MISSING
   upaobs%geop   = MISSING
   upaobs%temp   = MISSING
   upaobs%dewpt  = MISSING
   upaobs%speed  = MISSING
   upaobs%dir    = MISSING
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE clear_upa_obs ( upa )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   TYPE ( upa_obs ) , INTENT ( OUT ) :: upa
   vert_id        = MISSING
   nlevels        = 0
   upa%sttn_name  = BLANK_LINE
   upa%rev_yymmdd = MISSING
   upa%rev_hhmmss = MISSING
   upa%station_id = MISSING
   upa%latitude   = MISSING
   upa%longitude  = MISSING
   upa%elevation  = MISSING
   CALL clear_upa_obs_var ( upa%obs )
ENDSUBROUTINE clear_upa_obs

!----------------------------------------------------------------------

SUBROUTINE write_upa_obs ( iwrite , upa , write_flag )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   INCLUDE 'inc.formats'
   INTEGER , INTENT ( IN )             :: iwrite
   TYPE ( upa_obs ) , INTENT ( INOUT ) :: upa
   INTEGER , INTENT ( IN )             :: write_flag

   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_upa_obs IOUNIT < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   IF ( write_flag .EQ. 0 ) THEN
      header_written = .TRUE.
      WRITE ( iwrite , FMT=HDRFMT , ADVANCE=ADV ) upa%sttn_name, &
         upa%rev_yymmdd, upa%rev_hhmmss, upa%station_id, &
         upa%latitude, upa%longitude, upa%elevation
   ELSEIF ( write_flag > 0 ) THEN
      nlevels = nlevels + 1
      WRITE ( iwrite , FMT=SNDFMT , ADVANCE=ADV ) upa%obs
      CALL clear_upa_obs_var ( upa%obs )
   ELSEIF ( write_flag < 0 ) THEN
      header_written = .FALSE.
      WRITE ( iwrite , FMT=ENDFMT ) ENDOUTPUT, ENDOUTPUT, vert_id, &
         nlevels, obs_yymmdd, obs_hhmmss
   ENDIF

ENDSUBROUTINE write_upa_obs

!----------------------------------------------------------------------

ENDMODULE upa_observations

!----------------------------------------------------------------------

SUBROUTINE write_fm35

   USE record_def
   USE upa_observations
   IMPLICIT NONE

   TYPE ( stack ) , POINTER               :: rtmp
   TYPE ( upa_obs )                       :: upa

   INTEGER                                :: iwrite , fm
   LOGICAL                                :: used

   IF ( TRACE_MOST ) PRINT  * , 'write_fm35 : in '
   iwrite = outfiles ( record_fm+400 )

   rtmp => record

   obs_yymmdd = MISSING
   obs_hhmmss = MISSING
   CALL clear_upa_obs ( upa )

   DO WHILE ( ASSOCIATED ( rtmp ) )

      used = .FALSE.

      IF ( rtmp%field_ival .NE. MISSING .OR. &
           nint(rtmp%field_rval) .NE. MISSING ) THEN

         SELECT CASE ( rtmp%field_cval(12:20) )

            CASE ( 'CODE FORM' ) ; used = .TRUE.
                                   fm     = rtmp%field_ival
                                   iwrite = outfiles ( fm+400 )
                                   IF ( record_fm .NE. fm ) THEN
                                      PRINT *,' Unexpected mixing of fm, ', &
                                         record_fm, fm
                                   ENDIF

            CASE ( 'CODE HEAD' ) ; used = .TRUE.
                                   obs_yymmdd      = rtmp%field_ival
                                   obs_hhmmss      = INT(rtmp%field_rval)

            CASE ( 'Observati' ) ; used = .TRUE.
                                   upa%rev_yymmdd = rtmp%field_ival
                                   upa%rev_hhmmss = INT(rtmp%field_rval)

            CASE ( 'New messa' ) ; used = .TRUE.
!                                  IF ( upa%latitude .NE. MISSING ) THEN
                                   IF ( header_written ) THEN
                                      CALL write_upa_obs ( iwrite , upa , -1 )
                                   ENDIF
                                   CALL clear_upa_obs ( upa )
                                   upa%rev_yymmdd = rtmp%field_ival
                                   upa%rev_hhmmss = INT(rtmp%field_rval)

            CASE ( 'Platform ' ) ; used = .TRUE.
                                   upa%station_id = -6666666
                                   upa%sttn_name  = rtmp%field_cval(12:)
                                   upa%elevation  = 0.0

            CASE ( 'SHIP or T' ) ; used = .TRUE.
                                   upa%station_id = -7777777
                                   upa%sttn_name  = rtmp%field_cval(12:)
                                   upa%elevation  = 0.0

            CASE ( 'Latitude ' ) ; used = .TRUE.
                                   upa%latitude   = rtmp%field_rval/100

            CASE ( 'Longitude' ) ; used = .TRUE.
                                   upa%longitude  = rtmp%field_rval/100
                                   IF ( fm .EQ. 35 .OR. fm .EQ. 32 ) THEN
                                      upa%station_id=str2int(rtmp%field_cval(6:10))
                                      upa%sttn_name =rtmp%field_cval(35:rlen)
                                   ELSE
                                      CALL write_upa_obs ( iwrite , upa , 0 )
                                   ENDIF

            CASE ( 'Elevation' ) ; used = .TRUE.
                                   upa%elevation  = rtmp%field_rval
                                   CALL write_upa_obs ( iwrite , upa , 0 )

            CASE ( 'Rev. Obse' ) ; used = .TRUE.
                                   upa%rev_yymmdd = rtmp%field_ival
                                   upa%rev_hhmmss = INT(rtmp%field_rval)

            CASE ( 'Station P' ) ; used = .TRUE.
                                   upa%obs%pres = multiply ( TOPa , rtmp%field_ival )
                                   upa%obs%geop = upa%elevation

            CASE ( 'Pressure ' ) ; CALL set_upa_val ( upa%obs%pres  , .FALSE. )
                                   upa%obs%pres = multiply ( TOPa , upa%obs%pres )

            CASE ( 'Geopotent' ) ; CALL set_upa_val ( upa%obs%geop  , .TRUE.  )

            CASE ( 'Height (g' ) ; CALL set_upa_val ( upa%obs%geop  , .TRUE.  )

            CASE ( 'Temperatu' ) ; CALL set_upa_val ( upa%obs%temp  , .TRUE.  )
                                   upa%obs%temp = add ( TOKELVIN, upa%obs%temp )

            CASE ( 'Dew Point' ) ; CALL set_upa_val ( upa%obs%dewpt , .TRUE.  )
                                   upa%obs%dewpt = add ( TOKELVIN, upa%obs%dewpt )

            CASE ( 'Wind dire' ) ; CALL set_upa_val ( upa%obs%dir   , .TRUE.  )

            CASE ( 'Wind spee' ) ; CALL set_upa_val ( upa%obs%speed , .TRUE.  )

!           CASE DEFAULT         ; used = .FALSE.

         ENDSELECT

      ENDIF

      IF ( used ) THEN
         WRITE ( outfiles ( record_fm+600 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
      ELSE
         WRITE ( outfiles ( record_fm+500 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
      ENDIF

      rtmp => rtmp%next_record

   ENDDO

   IF ( header_written ) THEN
      CALL write_upa_obs ( iwrite , upa ,  1 )
      CALL write_upa_obs ( iwrite , upa , -1 )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'write_fm35 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE set_upa_val ( variable , set_pz )
   IMPLICIT NONE
   REAL , INTENT ( OUT )               :: variable
   LOGICAL , INTENT ( IN )             :: set_pz
   IF ( vert_id .NE. rtmp%field_ival .AND. vert_id .NE. MISSING ) THEN
      CALL write_upa_obs ( iwrite , upa , 1 )
   ENDIF
   used     = .TRUE.
   variable = rtmp%field_rval
   vert_id  = rtmp%field_ival
   IF ( set_pz ) THEN
      IF ( vert_id > 0 ) THEN
         IF ( upa%obs%pres .EQ. MISSING ) upa%obs%pres = multiply ( TOPa, vert_id )
      ELSE
         IF ( upa%obs%geop .EQ. MISSING ) upa%obs%geop = abs ( vert_id )
      ENDIF
   ENDIF
ENDSUBROUTINE

ENDSUBROUTINE write_fm35

SUBROUTINE mm5wr_fm35

   USE record_def
   USE mm5obs_def
   IMPLICIT NONE

   TYPE ( stack ) , POINTER  :: rtmp
   TYPE ( mm5_hdr )          :: hdr
   TYPE ( meas_data )        :: upa

   INTEGER  :: iwrite , ieor
   LOGICAL  :: used

   IF ( TRACE_MOST ) PRINT  * , 'mm5wr_fm35 : in '
   iwrite = outfiles ( record_fm+700 )

   rtmp => record

   obs_yymmdd = missing
   obs_hhmmss = missing

   hdr    = empty_hdr
   upa    = empty_upa
   ieor   = 0
   nvalid = 1
   hdr%info%source = 'GTS (NOAA) ' // msg_header
   hdr%info%is_sound = .TRUE.
   wrote_hdr = .FALSE.
   vert_id = MISSING
 
   DO WHILE ( ASSOCIATED ( rtmp ) )

    !---------------------------------------------------------------

    SELECT CASE ( rtmp%field_cval(12:20) )

     CASE('CODE FORM'); used = .TRUE.
                        fm = rtmp%field_ival
                        iwrite = outfiles ( fm+700 )
 
     !---------------------------------------------------------------
     ! Time information
    
     CASE('CODE HEAD'); used = .TRUE.
                        obs_yymmdd = rtmp%field_ival
                        obs_hhmmss = NINT(rtmp%field_rval)
 
     CASE('Observati'); used = .TRUE.
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)
 
     CASE('New messa'); used = .TRUE.
                        IF ( wrote_hdr ) THEN
                           CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
                        ENDIF

                        hdr    = empty_hdr
                        upa    = empty_upa
                        nvalid = 1
                        ieor   = 0
                        hdr%info%source = 'GTS (NOAA) ' // msg_header
                        hdr%info%is_sound = .TRUE.
                        wrote_hdr = .FALSE.
                        vert_id = MISSING

                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     CASE('Rev. Obse'); used = .TRUE.
                        nvalid = nvalid + 1
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     !---------------------------------------------------------------
     ! location information 
 
     CASE('Platform '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = '-6666'
                        hdr%location%name = rtmp%field_cval(12:)
                        ! hdr%info%elevation = 0.0    ! assume 'SHIP' at elevation = 0 
                        ! upa%height%data =  0.0      ! assume 'SHIP' at elevation = 0 
 
     CASE('SHIP or T'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = '-7777'
                        hdr%location%name = rtmp%field_cval(12:)
                        ! hdr%info%elevation = 0.0    ! assume 'SHIP' at elevation = 0 
                        ! upa%height%data =  0.0      ! assume 'SHIP' at elevation = 0 

     CASE('Latitude '); used = .TRUE.
                        hdr%location%latitude = rtmp%field_rval/100
 
     CASE('Longitude'); used = .TRUE.
                        hdr%location%longitude = rtmp%field_rval/100
                        IF ( fm .EQ. 35 .OR. fm .EQ. 32 ) THEN
                           hdr%location%id = rtmp%field_cval(6:10)
                           hdr%location%name = rtmp%field_cval(35:rlen)
                        ELSE
                           CALL write_rpt_hdr ( iwrite , seqnum, hdr , ieor )
                        ENDIF

     !---------------------------------------------------------------
     ! Source information

     CASE('Elevation'); used = .TRUE.
                        hdr%info%elevation = rtmp%field_rval
                        ! upa%height%data = rtmp%field_rval 
                        ! The above is true for synop data, think about it here
                        CALL write_rpt_hdr ( iwrite , seqnum, hdr , ieor )
 
     !---------------------------------------------------------------
     ! Terrestrial and Upper air information

     CASE('Station P'); used = .TRUE.
                        hdr%ground%psfc%data = multiply ( rtmp%field_rval , TOPa )
                        upa%pressure%data = multiply ( rtmp%field_rval , TOPa )
                        upa%height%data = hdr%info%elevation
                        nvalid = nvalid + 1

     CASE('P @ max V'); IF ( rtmp%field_ival .NE. MISSING ) THEN
                           CALL set_upa_val ( upa%pressure%data , .FALSE. )
                           upa%pressure%data = multiply ( TOPa , upa%pressure%data )
                           nvalid = nvalid + 1
                        ELSE
                           used = .FALSE.
                        ENDIF

     CASE('Pressure '); CALL set_upa_val ( upa%pressure%data , .FALSE. )
                        upa%pressure%data = multiply ( TOPa , upa%pressure%data )
                        nvalid = nvalid + 1

     CASE('Geopotent'); CALL set_upa_val ( upa%height%data , .TRUE. )
                        nvalid = nvalid + 1

     CASE('Z @ max V'); IF ( rtmp%field_ival .NE. MISSING ) THEN
                           CALL set_upa_val ( upa%height%data , .FALSE. )
                           nvalid = nvalid + 1
                        ELSE
                           used = .FALSE.
                        ENDIF

     CASE('Height (g'); CALL set_upa_val ( upa%height%data , .TRUE. )
                        nvalid = nvalid + 1

     CASE('Temperatu'); CALL set_upa_val ( upa%temperature%data , .TRUE. )
                        upa%temperature%data = add ( TOKELVIN, upa%temperature%data )
                        nvalid = nvalid + 1

     CASE('Dew Point'); CALL set_upa_val ( upa%dew_point%data , .TRUE. )
                        upa%dew_point%data = add ( TOKELVIN, upa%dew_point%data )
                        nvalid = nvalid + 1

     CASE('Wind dire'); CALL set_upa_val ( upa%direction%data , .TRUE. )
                        nvalid = nvalid + 1

     CASE('Wind spee'); CALL set_upa_val ( upa%speed%data , .TRUE. )
                        nvalid = nvalid + 1

     CASE('Sea Surfa'); used = .TRUE.
                        hdr%ground%sst%data = add ( TOKELVIN , rtmp%field_rval )
                        nvalid = nvalid + 1

     CASE('Total Clo'); used = .TRUE.
                        hdr%ground%cloud_cvr%data = rtmp%field_rval
                        nvalid = nvalid + 1
 
     CASE('Lowest cl'); used = .TRUE.
                        hdr%ground%ceiling%data = rtmp%field_rval
                        nvalid = nvalid + 1
 
     !---------------------------------------------------------------
     ! Default action
 
     CASE DEFAULT     ; used = .FALSE.

    ENDSELECT

    !---------------------------------------------------------------

    IF ( used ) THEN
       WRITE ( outfiles ( record_fm+800 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
    ELSE
       WRITE ( outfiles ( record_fm+900 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
    ENDIF

    rtmp => rtmp%next_record

   ENDDO

   IF ( wrote_hdr ) THEN
      CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
      CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'mm5wr_fm35 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE set_upa_val ( variable , set_pz )

   IMPLICIT NONE

   REAL , INTENT ( OUT )               :: variable
   LOGICAL , INTENT ( IN )             :: set_pz

   used     = .TRUE.

   IF ( vert_id .NE. rtmp%field_ival .AND. vert_id .NE. MISSING ) THEN
      CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
   ENDIF

   variable = rtmp%field_rval
   vert_id  = rtmp%field_ival

   IF ( set_pz ) THEN
      IF ( vert_id > 0 ) THEN
   !     IF ( upa%pressure%data .EQ. MISSING ) & 
              upa%pressure%data = multiply ( TOPa, vert_id )
      ELSE
   !      IF ( upa%height%data .EQ. MISSING ) &
              upa%height%data = abs ( vert_id )
      ENDIF
   ENDIF

ENDSUBROUTINE

ENDSUBROUTINE mm5wr_fm35

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

   ! decode part of the bulletin that is not part of the CODE-FORM
   CALL decode_bulletin_header ( ddhhmm )

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

MODULE stm_observations

   INTEGER :: obs_yymmdd , obs_hhmmss , nlevels , &
              ref_pre , ref_height
   LOGICAL :: header_written = .FALSE.

   TYPE stm_var
      REAL :: pres, geop, temp, dewpt, speed, dir
   ENDTYPE

   TYPE stm_obs
      CHARACTER ( LEN = 60 ) :: sttn_name
      INTEGER          :: rev_yymmdd, rev_hhmmss, station_id
      REAL             :: latitude,   longitude , elevation
      TYPE ( stm_var ) :: obs
   ENDTYPE stm_obs

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE clear_stm_obs_var ( stmobs )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   TYPE ( stm_var ) , INTENT ( OUT ) :: stmobs
   stmobs%pres   = MISSING
   stmobs%geop   = MISSING
   stmobs%temp   = MISSING
   stmobs%dewpt  = MISSING
   stmobs%speed  = MISSING
   stmobs%dir    = MISSING
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE clear_stm_obs ( stm )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   TYPE ( stm_obs ) , INTENT ( OUT ) :: stm
   nlevels        = 0
   stm%sttn_name  = BLANK_LINE
   stm%rev_yymmdd = MISSING
   stm%rev_hhmmss = MISSING
   stm%station_id = MISSING
   stm%latitude   = MISSING
   stm%longitude  = MISSING
   stm%elevation  = MISSING
   CALL clear_stm_obs_var ( stm%obs )
ENDSUBROUTINE clear_stm_obs

!----------------------------------------------------------------------

SUBROUTINE write_stm_obs ( iwrite , stm , write_flag )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   INCLUDE 'inc.formats'
   INTEGER , INTENT ( IN )             :: iwrite
   TYPE ( stm_obs ) , INTENT ( INOUT ) :: stm
   INTEGER , INTENT ( IN )             :: write_flag

   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_stm_obs IOUNIT < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   IF ( write_flag .EQ. 0 ) THEN
      WRITE ( iwrite , FMT=HDRFMT , ADVANCE=ADV ) stm%sttn_name, &
         stm%rev_yymmdd, stm%rev_hhmmss, stm%station_id, &
         stm%latitude, stm%longitude, stm%elevation
      ref_pre    = MISSING
      ref_height = MISSING
      header_written = .TRUE.
   ELSEIF ( write_flag > 0 ) THEN
      nlevels = nlevels + 1
      stm%obs%temp  = ref_pre
      stm%obs%dewpt = ref_height
      WRITE ( iwrite , FMT=SNDFMT , ADVANCE=ADV ) stm%obs
      CALL clear_stm_obs_var ( stm%obs )
   ELSEIF ( write_flag < 0 ) THEN
      WRITE ( iwrite , FMT=ENDFMT ) ENDOUTPUT, ENDOUTPUT, ref_pre, &
         nlevels, obs_yymmdd, obs_hhmmss
      ref_pre    = MISSING
      ref_height = MISSING
      header_written = .FALSE.
   ENDIF

ENDSUBROUTINE write_stm_obs

!----------------------------------------------------------------------

ENDMODULE stm_observations

!----------------------------------------------------------------------

SUBROUTINE write_fm86

   USE record_def
   USE stm_observations
   IMPLICIT NONE

   TYPE ( stack ) , POINTER               :: rtmp
   TYPE ( stm_obs )                       :: stm

   INTEGER                                :: iwrite , fm
   LOGICAL                                :: used

   IF ( TRACE_MOST ) PRINT  * , 'write_fm86 : in '
   iwrite = outfiles ( record_fm+400 )

   rtmp => record

   obs_yymmdd = MISSING
   obs_hhmmss = MISSING
   CALL clear_stm_obs ( stm )

   DO WHILE ( ASSOCIATED ( rtmp ) )

         SELECT CASE ( rtmp%field_cval(12:20) )

            CASE ( 'CODE FORM' ) ; used = .TRUE.
                                   fm     = rtmp%field_ival
                                   iwrite = outfiles ( fm+400 )
                                   IF ( record_fm .NE. fm ) THEN
                                      PRINT *,' Unexpected mixing of fm, ', &
                                         record_fm, fm
                                   ENDIF

            CASE ( 'CODE HEAD' ) ; used = .TRUE.
                                   obs_yymmdd      = rtmp%field_ival
                                   obs_hhmmss      = INT(rtmp%field_rval)

            CASE ( 'Observati' ) ; used = .TRUE.
                                   stm%rev_yymmdd = rtmp%field_ival
                                   stm%rev_hhmmss = INT(rtmp%field_rval)

            CASE ( 'New messa' ) ; used = .TRUE.
                                   IF ( header_written ) THEN
                                      CALL write_stm_obs ( iwrite , stm , -1 )
                                   ENDIF
                                   CALL clear_stm_obs ( stm )
                                   stm%rev_yymmdd = rtmp%field_ival
                                   stm%rev_hhmmss = INT(rtmp%field_rval)

            CASE ( 'Satellite' ) ; used = .TRUE.
                                   stm%station_id = rtmp%field_ival
                                   stm%sttn_name  = rtmp%field_cval(12:)
                                   stm%elevation  = UNDEFINED

            CASE ( 'Latitude ' ) ; used = .TRUE.
                                   stm%latitude   = rtmp%field_rval/100

            CASE ( 'Longitude' ) ; used = .TRUE.
                                   stm%longitude  = rtmp%field_rval/100
                                   CALL write_stm_obs ( iwrite , stm , 0 )

            CASE ( 'Ref. Pres' ) ; used = .TRUE.
                                   ref_pre      = multiply ( TOPa , rtmp%field_rval )
                                   ref_height   = rtmp%field_ival
                                   stm%obs%pres = ref_pre
                                   stm%obs%geop = 0

            CASE ( 'Thickness' ) ; used = .TRUE.
                                   IF ( stm%obs%geop .NE. MISSING ) THEN
                                      CALL write_stm_obs ( iwrite , stm , 1 )
                                   ENDIF
                                   stm%obs%pres = multiply ( TOPa , rtmp%field_rval )
                                   stm%obs%geop = rtmp%field_ival

            CASE DEFAULT         ; used = .FALSE.

         ENDSELECT

      IF ( used ) THEN
         WRITE ( outfiles ( record_fm+600 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
      ELSE
         WRITE ( outfiles ( record_fm+500 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
      ENDIF

      rtmp => rtmp%next_record

   ENDDO

   IF ( header_written ) THEN
      CALL write_stm_obs ( iwrite , stm ,  1 )
      CALL write_stm_obs ( iwrite , stm , -1 )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'write_fm86 : out'

ENDSUBROUTINE write_fm86

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

   ! decode part of the bulletin that is not part of the CODE-FORM
   CALL decode_bulletin_header ( ddhhmm )

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

      ELSE IF ( arg2 .NE. '238//' ) THEN
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

      ELSE
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

MODULE sat_observations

   INTEGER :: obs_yymmdd , obs_hhmmss , vert_id
   LOGICAL :: header_written = .FALSE.

   TYPE sat_var
      REAL :: pres, geop, temp, clcvr, speed, dir
   ENDTYPE

   TYPE sat_obs
      CHARACTER ( LEN = 60 ) :: sttn_name
      INTEGER          :: rev_yymmdd, rev_hhmmss, station_id
      REAL             :: latitude,   longitude , elevation
      TYPE ( sat_var ) :: obs
   ENDTYPE sat_obs

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE clear_sat_obs_var ( satobs )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   TYPE ( sat_var ) , INTENT ( OUT ) :: satobs
   satobs%pres   = MISSING
   satobs%geop   = MISSING
   satobs%temp   = MISSING
   satobs%clcvr  = MISSING
   satobs%speed  = MISSING
   satobs%dir    = MISSING
ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE clear_sat_obs ( sat )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   TYPE ( sat_obs ) , INTENT ( OUT ) :: sat
   vert_id        = MISSING
   sat%sttn_name  = BLANK_LINE
   sat%rev_yymmdd = MISSING
   sat%rev_hhmmss = MISSING
   sat%station_id = MISSING
   sat%latitude   = MISSING
   sat%longitude  = MISSING
   sat%elevation  = MISSING
   CALL clear_sat_obs_var ( sat%obs )
ENDSUBROUTINE clear_sat_obs

!----------------------------------------------------------------------

SUBROUTINE write_sat_obs ( iwrite , sat )
   IMPLICIT NONE
   INCLUDE 'inc.special_symbols'
   INCLUDE 'inc.formats'
   INTEGER , INTENT ( IN )             :: iwrite
   TYPE ( sat_obs ) , INTENT ( INOUT ) :: sat

   IF ( iwrite < 1 ) THEN
      PRINT *, 'write_sat_obs IOUNIT < 0, IGNORE WRITE ', iwrite
      RETURN
   ENDIF

   WRITE ( iwrite , FMT=HDRFMT , ADVANCE=ADV ) sat%sttn_name, &
      sat%rev_yymmdd, sat%rev_hhmmss, sat%station_id, &
      sat%latitude, sat%longitude, sat%elevation
      header_written = .TRUE.
   WRITE ( iwrite , FMT=SNDFMT , ADVANCE=ADV ) sat%obs
   WRITE ( iwrite , FMT=ENDFMT , ADVANCE=ADV ) ENDOUTPUT, ENDOUTPUT, &
      MISSING, 1, obs_yymmdd, obs_hhmmss
      header_written = .FALSE.
   CALL clear_sat_obs_var ( sat%obs )

ENDSUBROUTINE write_sat_obs

!----------------------------------------------------------------------

ENDMODULE sat_observations

!----------------------------------------------------------------------

SUBROUTINE write_fm88

   USE record_def
   USE sat_observations
   IMPLICIT NONE

   TYPE ( stack ) , POINTER               :: rtmp
   TYPE ( sat_obs )                       :: sat

   INTEGER                                :: iwrite , fm
   LOGICAL                                :: used

   IF ( TRACE_MOST ) PRINT  * , 'write_fm88 : in '
   iwrite = outfiles ( record_fm+400 )

   rtmp => record

   obs_yymmdd = MISSING
   obs_hhmmss = MISSING
   CALL clear_sat_obs ( sat )

   DO WHILE ( ASSOCIATED ( rtmp ) )

      SELECT CASE ( rtmp%field_cval(12:20) )

         CASE ( 'CODE FORM' ) ; used = .TRUE.
                                fm     = rtmp%field_ival
                                iwrite = outfiles ( fm+400 )
                                IF ( record_fm .NE. fm ) THEN
                                   PRINT *,' Unexpected mixing of fm, ', &
                                      record_fm, fm
                                ENDIF

         CASE ( 'CODE HEAD' ) ; used = .TRUE.
                                obs_yymmdd      = rtmp%field_ival
                                obs_hhmmss      = INT(rtmp%field_rval)

         CASE ( 'Observati' ) ; used = .TRUE.
                                sat%rev_yymmdd  = rtmp%field_ival
                                sat%rev_hhmmss  = INT(rtmp%field_rval)

         CASE ( 'New messa' ) ; used = .TRUE.
                                IF ( sat%latitude .NE. MISSING ) THEN
                                   CALL write_sat_obs ( iwrite , sat )
                                ENDIF
                                CALL clear_sat_obs ( sat )
                                sat%rev_yymmdd = rtmp%field_ival
                                sat%rev_hhmmss = INT(rtmp%field_rval)

         CASE ( 'Satellite' ) ; used = .TRUE.
                                sat%station_id = rtmp%field_ival
                                sat%sttn_name  = rtmp%field_cval(12:)
                                sat%elevation  = UNDEFINED

         CASE ( 'Latitude ' ) ; used = .TRUE.
                                IF ( sat%latitude .NE. MISSING ) THEN
                                   CALL write_sat_obs ( iwrite , sat )
                                ENDIF
                                sat%latitude   = rtmp%field_rval/100

         CASE ( 'Longitude' ) ; used = .TRUE.
                                sat%longitude  = rtmp%field_rval/100

!        CASE ( 'Rev. Obse' ) ; used = .TRUE.
!                               sat%rev_yymmdd = rtmp%field_ival
!                               sat%rev_hhmmss = INT(rtmp%field_rval)

!        CASE ( 'Pressure ' ) ; CALL set_sat_val ( sat%obs%pres  )
!                               sat%obs%pres = multiply ( TOPa , sat%obs%pres )

!        CASE ( 'Geopotent' ) ; CALL set_sat_val ( sat%obs%geop  )

!        CASE ( 'Height (g' ) ; CALL set_sat_val ( sat%obs%geop  )

         CASE ( 'Surface T' ) ; used = .TRUE.
                                sat%obs%temp = add ( TOKELVIN, rtmp%field_rval )
                                sat%obs%geop = -1.

         CASE ( 'Temperatu' ) ; CALL set_sat_val ( sat%obs%temp  )
                                sat%obs%temp = add ( TOKELVIN, sat%obs%temp )

!        CASE ( 'Dew Point' ) ; CALL set_sat_val ( sat%obs%dewpt )
!                               sat%obs%dewpt = add ( TOKELVIN, sat%obs%dewpt )

         CASE ( 'Cloud Cov' ) ; CALL set_sat_val ( sat%obs%clcvr )
                                sat%obs%clcvr = multiply ( 0.01 , sat%obs%clcvr )

         CASE ( 'Wind dire' ) ; CALL set_sat_val ( sat%obs%dir   )

         CASE ( 'Wind spee' ) ; CALL set_sat_val ( sat%obs%speed )

         CASE DEFAULT         ; used = .FALSE.

      ENDSELECT

      IF ( used ) THEN
         WRITE ( outfiles ( record_fm+600 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
      ELSE
         WRITE ( outfiles ( record_fm+500 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
      ENDIF

      rtmp => rtmp%next_record

   ENDDO

   CALL write_sat_obs ( iwrite , sat )

   IF ( TRACE_ALL ) PRINT  * , 'write_fm88 : out'

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE set_sat_val ( variable )
   IMPLICIT NONE
   REAL , INTENT ( OUT )               :: variable
   used     = .TRUE.
   variable = rtmp%field_rval
   vert_id  = rtmp%field_ival
   IF ( vert_id > 0 ) THEN
      IF ( sat%obs%pres .EQ. MISSING ) sat%obs%pres = multiply ( TOPa, vert_id )
   ELSE
      IF ( sat%obs%geop .EQ. MISSING ) sat%obs%geop = abs ( vert_id )
   ENDIF
ENDSUBROUTINE

ENDSUBROUTINE write_fm88

SUBROUTINE mm5wr_fm88

   USE record_def
   USE mm5obs_def
   IMPLICIT NONE

   TYPE ( stack ) , POINTER  :: rtmp
   TYPE ( mm5_hdr )          :: hdr
   TYPE ( meas_data )        :: upa

   INTEGER  :: iwrite , ieor
   LOGICAL  :: used
   CHARACTER ( len = 40 ) :: satid 

   IF ( TRACE_MOST ) PRINT  * , 'mm5wr_fm88 : in '
   iwrite = outfiles ( record_fm+700 )

   rtmp => record

   obs_yymmdd = missing
   obs_hhmmss = missing

   hdr    = empty_hdr
   upa    = empty_upa
   ieor   = 0
   nvalid = 1
   hdr%info%source = 'GTS (NOAA) ' // msg_header
   hdr%info%is_sound = .TRUE.
   satid  = 'Unknown satellite'
   wrote_hdr = .FALSE.
   vert_id = MISSING
 
   DO WHILE ( ASSOCIATED ( rtmp ) )

    !---------------------------------------------------------------

    SELECT CASE ( rtmp%field_cval(12:20) )

     CASE('CODE FORM'); used = .TRUE.
                        fm = rtmp%field_ival
                        iwrite = outfiles ( fm+700 )
 
     !---------------------------------------------------------------
     ! Time information
    
     CASE('CODE HEAD'); used = .TRUE.
                        obs_yymmdd = rtmp%field_ival
                        obs_hhmmss = NINT(rtmp%field_rval)
 
     CASE('Observati'); used = .TRUE.
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)
 
     CASE('New messa'); used = .TRUE.
                        IF ( wrote_hdr ) THEN
                           CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
                           CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
                        ENDIF

                        hdr    = empty_hdr
                        upa    = empty_upa
                        nvalid = 1
                        ieor   = 0
                        hdr%info%source   = 'GTS (NOAA) ' // msg_header
                        hdr%info%is_sound = .TRUE.
                        hdr%location%id   = satid
                        wrote_hdr = .FALSE.

                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     !---------------------------------------------------------------
     ! location information 
 
     CASE('Satellite'); used = .TRUE.
                        satid = rtmp%field_cval(22:)
                        hdr%location%id = satid

     CASE('Latitude '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%latitude = rtmp%field_rval
 
     CASE('Longitude'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%longitude = rtmp%field_rval
                        CALL write_rpt_hdr ( iwrite, seqnum, hdr, ieor )

     !---------------------------------------------------------------
     ! Terrestrial and Upper air information

!    CASE('Cloud Cov'); used = .TRUE.
!                       nvalid = nvalid + 1

!    CASE('Cloud Top'); used = .TRUE.
!                       nvalid = nvalid + 1

!    CASE('Surface T'); used = .TRUE.
!                       nvalid = nvalid + 1

     CASE('Temperatu'); used = .TRUE.
                        upa%temperature%data = add ( TOKELVIN, rtmp%field_rval )
                        upa%pressure%data = multiply ( TOPa, rtmp%field_ival )
                        nvalid = nvalid + 1

     CASE('Wind dire'); used = .TRUE.
                        upa%direction%data = rtmp%field_rval
                        upa%pressure%data = multiply ( TOPa, rtmp%field_ival )
                        nvalid = nvalid + 1

     CASE('Wind spee'); used = .TRUE.
                        upa%speed%data = rtmp%field_rval
                        upa%pressure%data = multiply ( TOPa, rtmp%field_ival )
                        nvalid = nvalid + 1

     !---------------------------------------------------------------
     ! Default action
 
     CASE DEFAULT     ; used = .FALSE.

    ENDSELECT

    !---------------------------------------------------------------

    IF ( used ) THEN
       WRITE ( outfiles ( record_fm+800 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
    ELSE
       WRITE ( outfiles ( record_fm+900 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
    ENDIF

    rtmp => rtmp%next_record

   ENDDO

   IF ( wrote_hdr ) THEN
      CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
      CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'mm5wr_fm88 : out'

!----------------------------------------------------------------------

ENDSUBROUTINE mm5wr_fm88

SUBROUTINE decode_airep

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   INTEGER                  :: ival , ddhhmm
   REAL                     :: rval
   CHARACTER ( LEN = llen ) :: cval, arg

   IF ( TRACE_MOST ) PRINT  * , 'decode_airep : in '

   ! decode part of the bulletin that is not part of the CODE-FORM
   CALL decode_bulletin_header ( ddhhmm )

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

SUBROUTINE mm5wr_airep

   USE record_def
   USE mm5obs_def
   IMPLICIT NONE

   TYPE ( stack ) , POINTER  :: rtmp
   TYPE ( mm5_hdr )          :: hdr
   TYPE ( meas_data )        :: upa

   INTEGER  :: iwrite , ieor
   LOGICAL  :: used

   IF ( TRACE_MOST ) PRINT  * , 'mm5wr_airep : in '
   iwrite = outfiles ( record_fm+700 )

   rtmp => record

   obs_yymmdd = missing
   obs_hhmmss = missing

   hdr    = empty_hdr
   upa    = empty_upa
   ieor   = 0
   nvalid = 1
   hdr%info%source = 'GTS (NOAA) ' // msg_header
   hdr%info%is_sound = .TRUE.
   wrote_hdr = .FALSE.
 
   DO WHILE ( ASSOCIATED ( rtmp ) )

    !---------------------------------------------------------------

    SELECT CASE ( rtmp%field_cval(12:20) )

     CASE('CODE FORM'); used = .TRUE.
                        fm = rtmp%field_ival
                        iwrite = outfiles ( fm+700 )
 
     !---------------------------------------------------------------
     ! Time information
    
     CASE('CODE HEAD'); used = .TRUE.
                        obs_yymmdd = rtmp%field_ival
                        obs_hhmmss = NINT(rtmp%field_rval)
 
     CASE('Observati'); used = .TRUE.
                        nvalid = nvalid + 1
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)
                        CALL write_rpt_hdr ( iwrite, seqnum, hdr, ieor )
 
     CASE('New messa'); used = .TRUE.
                        IF ( wrote_hdr ) THEN
                           CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
                           CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
                        ENDIF

                        hdr    = empty_hdr
                        upa    = empty_upa
                        nvalid = 1
                        ieor   = 0
                        hdr%info%source = 'GTS (NOAA) ' // msg_header
                        hdr%info%is_sound = .TRUE.
                        wrote_hdr = .FALSE.

                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     !---------------------------------------------------------------
     ! location information 
 
     CASE('Flight Nu'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%name = rtmp%field_cval(26:)
 
     CASE('Latitude '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = rtmp%field_cval(25:)
                        hdr%location%latitude = rtmp%field_rval
 
     CASE('Longitude'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = TRIM(hdr%location%id) // rtmp%field_cval(25:)
                        hdr%location%longitude = rtmp%field_rval

     !---------------------------------------------------------------
     ! Terrestrial and Upper air information

     CASE('Flight Le'); used = .TRUE.
                        upa%height%data = rtmp%field_rval
                        nvalid = nvalid + 1

     CASE('Temperatu'); used = .TRUE.
                        upa%temperature%data = add ( TOKELVIN, rtmp%field_rval )
                        nvalid = nvalid + 1

     CASE('Wind dire'); used = .TRUE.
                        upa%direction%data = rtmp%field_rval
                        nvalid = nvalid + 1

     CASE('Wind spee'); used = .TRUE.
                        upa%speed%data = rtmp%field_rval
                        nvalid = nvalid + 1

     !---------------------------------------------------------------
     ! Default action
 
     CASE DEFAULT     ; used = .FALSE.

    ENDSELECT

    !---------------------------------------------------------------

    IF ( used ) THEN
       WRITE ( outfiles ( record_fm+800 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
    ELSE
       WRITE ( outfiles ( record_fm+900 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
    ENDIF

    rtmp => rtmp%next_record

   ENDDO

   IF ( wrote_hdr ) THEN
      CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
      CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'mm5wr_airep : out'

!----------------------------------------------------------------------

ENDSUBROUTINE mm5wr_airep

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

