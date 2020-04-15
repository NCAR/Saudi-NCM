INCLUDE 'bulletin.module'
INCLUDE 'record.module'

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

SUBROUTINE code_fm15_tdtd ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'

   ival = str2int(arg(2:4))
   IF ( ival .eq. MISSING ) THEN
      CALL code_ignore ( 'fm15_tdtd', 'Missing Dew Point', arg )
      cval = 'missing <' // TRIM(arg) // '>'

   ELSE IF ( ival < 0 ) THEN
      ! Probably decoding wrong group, unexpected
      CALL code_error ( 'fm15_tdtd', 'Invalid Dew Point', arg )
      ival = UNDEFINED
      cval = 'invalid <' // TRIM(arg) // '>'
      section_id = -1

   ELSE
      rval = multiply ( ival, 0.1 )
      ! only add dew point temperature from remark if successful
      IF ( arg(1:1) .EQ. '1' ) THEN
         ival = minus ( ival )
      ENDIF
      cval = arg
      cval = 'TEMP:METAR:Dew Point (C) RMK ' // TRIM(cval)
      IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_fm15_tttt ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'

   ival = str2int(arg(2:4))
   IF ( ival .eq. MISSING ) THEN
      CALL code_ignore ( 'fm15_tttt', 'Missing Temperature', arg )
      cval = 'missing <' // TRIM(arg) // '>'

   ELSE IF ( ival < 0 ) THEN
      ! Probably decoding wrong group, unexpected
      CALL code_error ( 'fm15_tttt', 'Invalid Temperature', arg )
      ival = UNDEFINED
      cval = 'invalid <' // TRIM(arg) // '>'
      section_id = -1

   ELSE
      ! only add temperature from remark if successful
      IF ( arg(1:1) .EQ. '1' ) THEN
         ival = minus ( ival )
      ENDIF
      rval = multiply ( ival, 0.1 )        
      cval = arg
      cval = 'TEMP:METAR:Temperature (C) RMK ' // TRIM(cval)
      IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_fm15_tt ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'

   if ( arg(1:1) .eq. 'M' ) THEN
      ival = str2int( TRIM(arg(2:)) )
   else
      ival = str2int( TRIM(arg) ) 
   endif

   IF ( ival .eq. MISSING ) THEN
      CALL code_ignore ( 'fm15_tt', 'Missing Temperature', arg )
      cval = 'missing <' // TRIM(arg) // '>'

   ELSE IF ( ival < 0 ) THEN
      ! Probably decoding wrong group, unexpected
      CALL code_error ( 'fm15_tt', 'Invalid Temperature', arg )
      ival = UNDEFINED
      cval = 'invalid <' // TRIM(arg) // '>'
      section_id = -1

   ELSE
      IF ( arg(1:1) .EQ. 'M' ) THEN
         ival = minus ( ival )
      ENDIF
      cval = arg

   ENDIF

   rval = ival
   cval = 'TEMP:METAR:Temperature (C)' // ' ' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_fm15_td ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'

   if ( arg(1:1) .eq. 'M' ) THEN
      ival = str2int( TRIM(arg(2:)) )
   else
      ival = str2int( TRIM(arg) ) 
   endif

   IF ( ival .eq. MISSING ) THEN
      CALL code_ignore ( 'fm15_td', 'Missing Dew Point', arg )
      cval = 'missing <' // TRIM(arg) // '>'

   ELSE IF ( ival < 0 ) THEN
      ! Probably decoding wrong group, unexpected
      CALL code_error ( 'fm15_td', 'Invalid Dew Point', arg )
      ival = UNDEFINED
      cval = 'invalid <' // TRIM(arg) // '>'
      section_id = -1

   ELSE
      IF ( arg(1:1) .EQ. 'M' ) THEN
         ival = minus ( ival )
      ENDIF
      cval = arg

   ENDIF

   rval = ival
   cval = 'TDTD:METAR:Dew Point (C)' // ' ' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_fm15_dir ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'

   cval = arg
   ival = str2int ( arg )
   IF ( arg .EQ. 'VRB' ) THEN
      ival = MISSING
      rval = MISSING
   ELSE IF ( ival .LT. 0 ) THEN
      CALL code_error ( 'fm15_dir', 'Invalid wind direction', arg )
      rval = UNDEFINED
      cval = 'invalid <' // TRIM(arg) // '>'
      section_id = -1
   ELSE
      rval = ival
   ENDIF
   cval = 'WDIR:METAR:Wind direction (deg)' // ' ' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_fm15_ff ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   INTEGER :: iget
   REAL    :: rget
   CHARACTER ( LEN = rlen ) :: cget

   cval = arg
   cget = arg
   iget = LEN_TRIM(arg)
   IF      ( arg(iget-2:iget) .EQ. 'KPH' ) THEN
      cget = arg(1:iget-3)
      rget = KMPH
   ELSE IF ( arg(iget-2:iget) .EQ. 'MPS' ) THEN
      cget = arg(1:iget-3)
      rget = 1.0
   ELSE IF ( arg(iget-1:iget) .EQ. 'KT'  ) THEN
      cget = arg(1:iget-2)
      rget = KNOT
   ELSE
      CALL code_error ( 'fm15_ff', 'Invalid wind speed', arg )
      section_id = -1
      RETURN
   ENDIF

   iget = SCAN ( TRIM(cget), 'G' )
   IF ( iget .NE. 0 ) THEN
      cget = cget(1:iget-1)
   ENDIF

   ival = str2int( TRIM(cget) ) 
   IF ( ival .LT. 0 ) THEN
      CALL code_error ( 'fm15_ff', 'Invalid wind speed', cget )
      rval = UNDEFINED
      cval = 'invalid <' // TRIM(cget) // '>'
      section_id = -1
   ELSE
      rval = multiply ( ival, rget )
   ENDIF
   cval = 'WSPD:METAR:Wind speed (m/s)' // ' ' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_PHPHPHPH ( arg , ival , rval , cval , append ) ;
   USE record_def
   IMPLICIT NONE
   INCLUDE 'inc.code_tables'
   REAL    :: rget

   cval = arg
   IF ( arg(1:1) .EQ. 'Q' ) THEN
      rget = 1.0
   ELSE IF ( arg(1:1) .EQ. 'A' ) THEN
      rget = 0.01 * INCHHG
   ELSE
      CALL code_error ( 'fm15_ff', 'Invalid QNH value', arg )
      section_id = -1
      RETURN
   ENDIF

   ival = str2int( TRIM(arg(2:)) ) 
   IF ( ival .LT. 0 ) THEN
      CALL code_error ( 'fm15_ff', 'Invalid QNH value', cval )
      rval = UNDEFINED
      cval = 'invalid <' // TRIM(cval) // '>'
      section_id = -1
   ELSE
      rval = multiply ( ival, rget )
   ENDIF
   cval = 'QNH :METAR:QNH value (hPa) ' // TRIM(cval)
   IF ( append .EQ. 'APPEND' ) CALL record_appendr ( ival , rval , cval )

ENDSUBROUTINE

!----------------------------------------------------------------------

SUBROUTINE code_table_icao ( arg , ival , rval , cval , append )
   USE record_def
   USE bulletin_def
   IMPLICIT NONE
!  INCLUDE 'inc.code_tables'
! replace the INCLUDE inc.code_tables with the following lines
! to allow arg to be INOUT
   CHARACTER ( LEN = * ) , INTENT ( INOUT )  :: arg
   INTEGER , INTENT ( OUT )               :: ival
   REAL , INTENT ( OUT )                  :: rval
   CHARACTER ( LEN = * ) , INTENT ( OUT ) :: cval
   CHARACTER ( LEN = * ) , INTENT ( IN )  :: append
!
   INTEGER :: iget
   INTEGER, EXTERNAL :: icao_key
   CHARACTER ( len = rlen ) :: arg1, arg2 
   CHARACTER :: shrt_dscrptn*100

   ival = icao_key ( arg(1:4) )
   IF ( ival <= 0 ) THEN
      CALL code_error ( 'icao' , 'invalid station' , arg )
      ! read one more item from message (to skip over redundant date)
      CALL get_next_word_in_mesg ( arg )
      ival = icao_key ( arg(1:4) )
      if ( ival <= 0 ) THEN
         CALL code_error ( 'icao' , 'invalid station' , arg )
         IF ( append .EQ. 'APPEND' ) &
            CALL record_appendi ( ival , '....:CCCC :ICAO Station Id' )
         RETURN
      ENDIF
   ENDIF
   rval = ival

   READ ( icao_data , stn_fmt , rec=ival ) stn_dscrptn
   IF ( arg(1:4) .EQ. stn_dscrptn(1:4) ) THEN
      cur_sttn_id = str2int(stn_dscrptn(5:9))
      iget = str2int ( stn_dscrptn(10:15) )
      shrt_dscrptn = TRIM(stn_dscrptn(:9)) // " " // TRIM(stn_dscrptn(29:88)) &
                                           // " " // TRIM(stn_dscrptn(89:)) 
      cval = '....:' // arg(1:5) // ':Latitude (N) ICAO ' // shrt_dscrptn
      CALL record_appendi ( iget , cval )
      iget = str2int ( stn_dscrptn(16:21) )
      cval = '....:' // arg(1:5) // ':Longitude(E) ICAO ' // shrt_dscrptn
      CALL record_appendi ( iget , cval )
      iget = str2int ( stn_dscrptn(22:28) )
      cur_sttn_alt = iget
      cval = '....:' // arg(1:5) // ':Elevation(m) ICAO ' // shrt_dscrptn
      IF ( append .EQ. 'APPEND' ) CALL record_appendi ( iget , cval )
   ELSE
      ival = minus ( ival )
      CALL code_error ( 'icao' , 'unknown station' , arg )
   ENDIF

ENDSUBROUTINE

!----------------------------------------------------------------------

FUNCTION icao_key ( id )

   IMPLICIT NONE
   CHARACTER ( LEN = * ), INTENT ( IN ) :: id
   INTEGER                              :: icao_key, ia
!  INTEGER, PARAMETER     :: imax = 26*36*36*36

!  INTEGER, PARAMETER     :: imax = 943499       ! last station 10AK
   INTEGER, PARAMETER     :: imax = 874212       ! last station 10AK
   ! imax also corresponds to number of 100-byte record in gts_sttnid_final.icao
 
   CHARACTER ( LEN = 36 ) :: cset = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

!  print *,' id = ',id,' imax = ', imax

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

!  print *,' icao_key = ',icao_key

ENDFUNCTION icao_key

