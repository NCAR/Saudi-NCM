!INCLUDE 'record.module'

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

SUBROUTINE wr_rawins_fm35

   USE record_def
   USE upa_observations
   IMPLICIT NONE

   TYPE ( stack ) , POINTER               :: rtmp
   TYPE ( upa_obs )                       :: upa

   INTEGER                                :: iwrite , fm
   LOGICAL                                :: used

   IF ( .NOT. FOR_RAWINS ) return

   IF ( TRACE_MOST ) PRINT  * , 'wr_rawins_fm35 : in ', record_fm

   IF ( FOR_DEBUG ) THEN
      CALL assign_outfile ( 456 )
   ELSE
      CALL assign_outfile ( 4 )
   ENDIF

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

   IF ( header_written ) THEN
      CALL write_upa_obs ( iwrite , upa ,  1 )
      CALL write_upa_obs ( iwrite , upa , -1 )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'wr_rawins_fm35 : out'

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

ENDSUBROUTINE wr_rawins_fm35
