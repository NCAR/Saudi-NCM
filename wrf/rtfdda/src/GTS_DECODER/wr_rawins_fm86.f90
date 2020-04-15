!INCLUDE 'record.module'

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

SUBROUTINE wr_rawins_fm86

   USE record_def
   USE stm_observations
   IMPLICIT NONE

   TYPE ( stack ) , POINTER               :: rtmp
   TYPE ( stm_obs )                       :: stm

   INTEGER                                :: iwrite , fm
   LOGICAL                                :: used

   IF ( .NOT. FOR_RAWINS ) return

   IF ( TRACE_MOST ) PRINT  * , 'wr_rawins_fm86 : in ', record_fm

   IF ( FOR_DEBUG ) THEN
      CALL assign_outfile ( 456 )
   ELSE
      CALL assign_outfile ( 4 )
   ENDIF

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
      CALL write_stm_obs ( iwrite , stm ,  1 )
      CALL write_stm_obs ( iwrite , stm , -1 )
   ENDIF

   IF ( TRACE_ALL ) PRINT  * , 'wr_rawins_fm86 : out'

ENDSUBROUTINE wr_rawins_fm86
