!INCLUDE 'record.module'

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

SUBROUTINE wr_rawins_fm88

   USE record_def
   USE sat_observations
   IMPLICIT NONE

   TYPE ( stack ) , POINTER               :: rtmp
   TYPE ( sat_obs )                       :: sat

   INTEGER                                :: iwrite , fm
   LOGICAL                                :: used

   IF ( .NOT. FOR_RAWINS ) return

   IF ( TRACE_MOST ) PRINT  * , 'wr_rawins_fm88 : in ', record_fm

   IF ( FOR_DEBUG ) THEN
      CALL assign_outfile ( 456 )
   ELSE
      CALL assign_outfile ( 4 )
   ENDIF

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

      IF ( FOR_DEBUG ) THEN
         IF ( used ) THEN
            WRITE ( outfiles ( record_fm+600 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
         ELSE
            WRITE ( outfiles ( record_fm+500 ) , RECFMT ) &
                 rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
         ENDIF
      ENDIF

      rtmp => rtmp%next_record

   ENDDO

   CALL write_sat_obs ( iwrite , sat )

   IF ( TRACE_ALL ) PRINT  * , 'wr_rawins_fm88 : out'

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

ENDSUBROUTINE wr_rawins_fm88
