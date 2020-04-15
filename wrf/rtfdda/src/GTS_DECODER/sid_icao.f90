PROGRAM sttnid

   IMPLICIT NONE

   INTEGER, PARAMETER :: imax = 26*36*36*36

   CHARACTER :: icao_id*4
   CHARACTER :: cntry_id*2
   CHARACTER :: city_id*3
   CHARACTER :: descrptn*60
   CHARACTER :: cntry_nam*12
   CHARACTER :: stn_lat*10, stn_1*3, stn_2*2, stn_3*2
   CHARACTER :: stn_lng*10, clat*1, clng*1
   CHARACTER :: stn_height*7

   INTEGER, PARAMETER                :: llen = 100
   CHARACTER ( LEN =  6 ), PARAMETER :: fmt = '(A100)'

   CHARACTER ( LEN = 255 )           :: line
   CHARACTER ( LEN = llen )          :: newline, blkline
   CHARACTER                         :: blk = ' '
   INTEGER                           :: ifil1=21
   INTEGER                           :: iout1=31, iout2=32
   INTEGER                           :: i, height, lat, lng, ikey
   INTEGER, EXTERNAL                 :: str2int, icao_key

   OPEN ( ifil1, FILE = 'ICAO_STATION_LOCATION'   )
   OPEN ( iout1, FILE = 'gts_sttnid_input.icao.txt' )
   OPEN ( iout2, FILE = 'gts_sttnid_final.icao', ACCESS = 'direct', &
          FORM = 'formatted', RECL = llen )

   blkline = REPEAT ( '9', llen )
!  DO i = 1, imax
!     WRITE ( iout2, fmt, rec=i ) blkline
!  ENDDO

   DO
      READ ( ifil1, '(A)', END = 99 ) line
      CALL strcut ( icao_id,    line, ';',  1 )
      CALL strcut ( cntry_id,   line, ';',  2 )
      CALL strcut ( city_id,    line, ';',  3 )
      CALL strcut ( descrptn,   line, ';',  4 )
      CALL strcut ( cntry_nam,  line, ';',  6 )
      CALL strcut ( stn_lat,    line, ';',  8 )
      CALL strcut ( stn_lng,    line, ';',  9 )
      CALL strcut ( stn_height, line, ';', 12 )
      height = str2int(stn_height)
     
      i = LEN_TRIM(STN_LAT)
      clat    = stn_lat(i:i)
      stn_lat = stn_lat(:i-1)
      stn_1 = ""
      stn_2 = ""
      stn_3 = ""
      CALL strcut ( stn_1, stn_lat, '-', 1 )
      CALL strcut ( stn_2, stn_lat, '-', 2 )
      CALL strcut ( stn_3, stn_lat, '-', 3 )
      IF ( stn_3 .EQ. "" ) THEN
         lat = str2int( stn_1 )*100. + str2int(stn_2)*100./60.
      ELSE
         lat = str2int( stn_1 )*100. + str2int(stn_2)*100./60.+ str2int(stn_3)*100./3600.
      ENDIF
      IF ( clat .EQ. 'S' ) THEN
         lat = - lat 
      ENDIF

      i = LEN_TRIM(stn_lng)
      clng    = stn_lng(i:i)
      stn_lng = stn_lng(:i-1)
      stn_1 = ""
      stn_2 = ""
      stn_3 = ""
      CALL strcut ( stn_1, stn_lng, '-', 1 )
      CALL strcut ( stn_2, stn_lng, '-', 2 )
      CALL strcut ( stn_3, stn_lng, '-', 3 )
      IF ( stn_3 .EQ. "" ) THEN
         lng = str2int( stn_1 )*100. + str2int(stn_2)*100./60.
      ELSE
         lng = str2int( stn_1 )*100. + str2int(stn_2)*100./60.+ str2int(stn_3)*100./3600.
      ENDIF
      IF ( clng .EQ. 'W' ) THEN
         lng = - lng 
      ENDIF

      write ( newline, '(A4,A2,A3,I6.5,I6.5,I7,A60,A12)' ) &
         icao_id, cntry_id, city_id, lat, lng, height, descrptn, cntry_nam

      write ( iout1, fmt ) newline
      ikey = icao_key(icao_id) 
      if ( ikey > 0 ) then
         write ( iout2, fmt, rec=icao_key(icao_id) ) newline
      endif
   ENDDO

99 STOP

66 print *, i
   STOP

ENDPROGRAM

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
   CHARACTER ( LEN = * ), INTENT ( IN ) :: string
   INTEGER                               :: str2int
   CHARACTER ( LEN = 4 )                 :: fmt
   CHARACTER ( LEN = len ( string ) )    :: tmp_string

   fmt = '(I' // ACHAR ( IACHAR ( '0' ) + LEN ( string ) ) // ')'
   tmp_string = ADJUSTR ( string )
   READ ( tmp_string, fmt, ERR = 10 ) str2int
   RETURN

10 CONTINUE
   IF ( string .EQ. repeat ( '/', len ( string ) ) ) THEN
      str2int = MISSING
   ELSE
      str2int = UNDEFINED
   ENDIF

ENDFUNCTION str2int

! ----------------------------------------------------------------------

SUBROUTINE strcut ( strout, strin, delimitor, key )

   ! equivalent to
   !    set strout = `echo $strin | cut -ddelimitor -fkey
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( OUT ) :: strout
   CHARACTER ( LEN = * ), INTENT ( IN  ) :: strin
   CHARACTER ( LEN = 1 ), INTENT ( IN  ) :: delimitor
   INTEGER                               :: key, i, j
   CHARACTER ( LEN = len ( strin ) )     :: strtmp

   strtmp = strin
   i = 0
   strout = ""
   DO WHILE ( i < key ) 
      i = i + 1
      j = scan ( strtmp, delimitor )
      IF ( j > 0 ) THEN
         strout = strtmp(:j-1)
         strtmp = strtmp(j+1:)
      ELSE 
         if ( i .eq. 1 ) then
           strout = strin
         else if ( i .eq. key ) then
           strout = strtmp
         else
           strout = ""
         endif
         EXIT
      ENDIF
   ENDDO

ENDSUBROUTINE strcut

! ----------------------------------------------------------------------

FUNCTION icao_key ( id )

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( IN ) :: id
   INTEGER                              :: icao_key, ia
   INTEGER, PARAMETER     :: imax = 26*36*36*36
   CHARACTER ( LEN = 36 ) :: cset = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

   ia = VERIFY ( id, cset )
   if ( ia .NE. 0 ) THEN
      icao_key = 0
   else
      icao_key = (SCAN(cset,id(1:1))-1)*26*36*36 + &
                 (SCAN(cset,id(2:2))-1)   *36*36 + &
                 (SCAN(cset,id(3:3))-1)      *36 + &
                 (SCAN(cset,id(4:4))-1)          + 1
   ENDIF
   
   if ( icao_key .GT. imax ) icao_key = 0

ENDFUNCTION icao_key

