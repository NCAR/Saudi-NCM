!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

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
