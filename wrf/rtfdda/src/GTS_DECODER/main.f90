!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'
!INCLUDE 'mm5obs.module'

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

   READ ( 5, '(I4,4I2)', END=99, ERR=99 ) &
      utc_year, utc_month, utc_day, utc_hour, utc_minute
      utc_second = 0

   PRINT *, ' cutoff time input : ', utc_year, utc_month, utc_day, utc_hour
!-   use 4-digit year now
!-   utc_year   = set_century ( utc_year )
   utc_yymmdd = utc_year * 10000 + utc_month * 100 + utc_day
   utc_hhmmss = utc_hour * 10000 + utc_minute* 100 + utc_second
   PRINT *, ' cutoff time : ', utc_yymmdd, utc_hhmmss

   OPEN ( stn_data , FILE = 'gts_sttnid_final' , FORM = 'FORMATTED' , &
          ACCESS = 'DIRECT' , RECL = stn_llen )
   OPEN ( icao_data , FILE = 'gts_sttnid_final.icao' , FORM = 'FORMATTED' , &
          ACCESS = 'DIRECT' , RECL = stn_llen )

   OPEN ( UNIT=2,      FILE='gts_out.002', FORM='FORMATTED', POSITION='APPEND' )
   OPEN ( UNIT=ierr0,  FILE='gts_out.003', FORM='FORMATTED', POSITION='APPEND' )
   OPEN ( UNIT=iwarn0, FILE='gts_out.004', FORM='FORMATTED', POSITION='APPEND' )

   ! initialize the basic bulletin contents
   !
   bulletin%text = SOH

   ! initialize empty observation for output in wr_rap
   CALL init_wr_rap_emptyobs
 
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
