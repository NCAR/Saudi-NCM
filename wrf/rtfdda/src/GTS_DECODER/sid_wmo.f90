PROGRAM sttnid

   IMPLICIT NONE

   CHARACTER :: country*5, country_long*60
   CHARACTER :: id*5, region*1
   CHARACTER :: call_letter*5
   CHARACTER :: latitude*6, clat*2
   CHARACTER :: longitude*7, clng*2
   CHARACTER :: altitude*6
   CHARACTER :: name*60

   INTEGER, PARAMETER                :: llen = 100
   CHARACTER ( LEN =  6 ), PARAMETER :: fmt = '(A100)'
   CHARACTER ( LEN = 20 ), PARAMETER :: cfmt = '(I4,1X,A60)'

   CHARACTER ( LEN = 132 )           :: line
   CHARACTER ( LEN = llen )          :: newline, blkline
   CHARACTER                         :: blk = ' ', cur_cname*60
   INTEGER                           :: ifil1=21, ifil2=22
   INTEGER                           :: iout1=31, iout2=32, iout3=33
   INTEGER                           :: i, cur_cid, stn_cntryid, ikey      !linux
   INTEGER, EXTERNAL                 :: str2int

   OPEN ( ifil1, FILE = 'gts_sttnid_input.wmo'    )
   OPEN ( ifil2, FILE = 'gts_sttnid_input.wmo.cc' )
   OPEN ( iout1, FILE = 'gts_sttnid_input.wmo.txt' )
   OPEN ( iout2, FILE = 'gts_sttnid_cmpct.wmo'    )
   OPEN ( iout3, FILE = 'gts_sttnid_final.wmo', ACCESS = 'direct', &
          FORM = 'formatted', RECL = llen )

   blkline = REPEAT ( '9', llen )
   DO i = 1, 99999
      WRITE ( iout3, fmt, rec=i ) blkline
   ENDDO

   READ ( ifil2, cfmt ) cur_cid, cur_cname

   DO
      READ ( ifil1, '(A)', END = 99 ) line
      IF ( line(1:2) .EQ. '02' ) THEN

         stn_cntryid = str2int(line(3:6))

         get_cntry_name : DO WHILE ( stn_cntryid > cur_cid )
            READ ( ifil2, cfmt, ERR=88 ) cur_cid, cur_cname
               GOTO 89
            88 cur_cid = 99999
            89 CONTINUE
         ENDDO get_cntry_name
         IF ( stn_cntryid .EQ. cur_cid ) THEN
            country_long = cur_cname
         ELSE
            country_long = 'UNEXPECTED'
         ENDIF

         name = TRIM(line(14:61))
         i    = INDEX(TRIM(name),'  ')
         DO WHILE ( i .NE. 0 )
            name = name(1:i) // name(i+2:)
            i    = INDEX(TRIM(name),'  ')
         ENDDO
         i = INDEX(TRIM(name),'- ')
         IF ( i .ne. 0 ) name = name(1:i-1)//name(i+2:)
         name = TRIM(name) // ' / ' // TRIM(country_long)

         region      = line( 3: 3)
         id          = line( 7:11)
         call_letter = '_WMO_'

         i = str2int(line(64:65))*100/60
         IF ( i > 99 ) i = str2int(line(63:64))
         WRITE ( clat , '(I2.2)' ) i
         latitude = blk // line(62:63) // clat // 'N'
         IF ( line(66:66) .EQ. 'S' ) latitude(1:1) = '-'

         i = str2int(line(70:71))*100/60
         IF ( i > 99 ) i = str2int(line(70:71))
         WRITE ( clng , '(I2.2)' ) i
         longitude = blk // line(67:69) // clng // 'E'
         IF ( line(72:72) .EQ. 'W' ) longitude(1:1) = '-'

         IF ( line(77:80) .EQ. '    ' ) line(80:80) = '0'
         IF ( line(75:75) .EQ. '-' ) THEN
            altitude = line(77:80)
         ELSEIF ( line(73:76) .EQ. '    ' ) THEN
            altitude = line(77:80)
         ELSE
            altitude = line(73:76)
         ENDIF
         read  ( altitude(1:4) , '(I4)' , ERR=77 ) i
         goto 78
      77 read  ( altitude(1:3) , '(I3)' , ERR=66 ) i
         if ( i < 0 ) then
             i =  10 * i - INDEX('ABCDEFGHI',altitude(4:4))
         else
             i =  10 * i + INDEX('ABCDEFGHI',altitude(4:4))
         endif
      78 continue
         write ( altitude(1:5) , '(I5.4)' ) i
         altitude(6:6) = 'm'

         newline = id // blk // latitude// blk // longitude// blk //      &
                   altitude// blk // region// blk // call_letter// blk // &
                   name
         WRITE ( iout1, fmt ) newline
         WRITE ( iout2, * ) newline(1:80)
	 ikey = str2int(id)
print *, fmt, id, str2int(id)
         WRITE ( iout3, fmt, rec = ikey ) newline                          !linux
      ENDIF
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
