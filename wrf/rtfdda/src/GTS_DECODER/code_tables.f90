!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

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
!  IF ( iget > bul_hhmmss ) THEN
! Make the time window one hour long 
   IF ( (iget - bul_hhmmss) .GT.  10000. ) THEN
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

