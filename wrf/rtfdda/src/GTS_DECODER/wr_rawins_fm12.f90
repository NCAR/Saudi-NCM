!INCLUDE 'record.module'

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

SUBROUTINE wr_rawins_fm12

   USE record_def
   USE surface_observations
   IMPLICIT NONE

   TYPE ( stack ) , POINTER               :: rtmp
   TYPE ( surface_obs )                   :: surf

   INTEGER                                :: iwrite , fm
   LOGICAL                                :: used

   IF ( .NOT. FOR_RAWINS ) return

   IF ( TRACE_MOST ) PRINT  * , 'wr_rawins_fm12 : in ', record_fm

   IF ( FOR_DEBUG ) THEN
      CALL assign_outfile ( 456 )
   ELSE
      CALL assign_outfile ( 4 )
   ENDIF

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

   CALL write_surface_obs ( iwrite , surf )

   IF ( TRACE_ALL ) PRINT  * , 'wr_rawins_fm12 : out'

ENDSUBROUTINE wr_rawins_fm12
