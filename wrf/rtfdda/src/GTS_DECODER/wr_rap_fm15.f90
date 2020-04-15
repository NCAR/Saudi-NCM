!INCLUDE 'record.module'
!INCLUDE 'mm5obs.module'

SUBROUTINE wr_rap_fm15

   USE record_def
   USE mm5obs_def
   IMPLICIT NONE

   TYPE ( stack ) , POINTER  :: rtmp
   TYPE ( mm5_hdr )          :: hdr
   TYPE ( meas_data )        :: upa

   INTEGER  :: iwrite , ieor
   LOGICAL  :: used

   IF ( .NOT. FOR_RAP ) return

   IF ( TRACE_MOST ) PRINT  * , 'wr_rap_fm15 : in '

   IF ( FOR_DEBUG ) THEN
      CALL assign_outfile ( 789 )
   ELSE
      CALL assign_outfile ( 7 )
   ENDIF

   iwrite = outfiles ( record_fm+700 )

   rtmp => record

   obs_yymmdd = missing
   obs_hhmmss = missing

   hdr    = empty_hdr
   upa    = empty_upa
   ieor   = 0
   nvalid = 1
   hdr%info%source = 'GTS (NOAA) ' // msg_header
   hdr%info%is_sound = .TRUE.
   wrote_hdr = .FALSE.
 
! Modified Mar 16 2000 - SLN
!       Changed ROHK identifier to ATEC
!       add the QNH value for METARS as ground info
! Modified Aug 12 2010 - FCV
!     Changed the ATEC identifier to NOAA
 
   DO WHILE ( ASSOCIATED ( rtmp ) )

! print *, ' checked ', rtmp%field_cval

    !---------------------------------------------------------------

    SELECT CASE ( rtmp%field_cval(12:20) )

     CASE('CODE FORM'); used = .TRUE.
                        fm = rtmp%field_ival
                        iwrite = outfiles ( fm+700 )
 
     !---------------------------------------------------------------
     ! Time information
    
     CASE('CODE HEAD'); used = .TRUE.
                        obs_yymmdd = rtmp%field_ival
                        obs_hhmmss = NINT(rtmp%field_rval)
 
     CASE('Observati'); used = .TRUE.
                        nvalid = nvalid + 1
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)
 
     CASE('New messa'); used = .TRUE.
                        CALL write_rpt_hdr ( iwrite, seqnum, hdr, ieor )
                        CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
                        CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )

                        hdr    = empty_hdr
                        upa    = empty_upa
                        nvalid = 1
                        ieor   = 0
                        hdr%info%source = 'GTS (NOAA) ' // msg_header
                        hdr%info%is_sound = .TRUE.
                        wrote_hdr = .FALSE.

                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     !---------------------------------------------------------------
     ! location information 
 
     CASE('Latitude '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = rtmp%field_cval(34:38)
                        hdr%location%name = rtmp%field_cval(25:33) // TRIM(rtmp%field_cval(39:))
                        hdr%info%platform = rtmp%field_cval(25:33)
                        hdr%location%latitude = rtmp%field_rval/100.
 
     CASE('Longitude'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%longitude = rtmp%field_rval/100.

     CASE('Elevation'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%info%elevation = rtmp%field_rval
                        upa%height%data = rtmp%field_rval

     !---------------------------------------------------------------
     ! Terrestrial and Upper air information

     CASE('Mean SLP '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%slp%data = multiply ( rtmp%field_rval , TOPa )
 
     CASE('Temperatu'); used = .TRUE.
                        upa%temperature%data = add ( TOKELVIN, rtmp%field_rval )
                        nvalid = nvalid + 1

     CASE('Dew Point'); used = .TRUE.
                        upa%dew_point%data = add ( TOKELVIN, rtmp%field_rval )
                        nvalid = nvalid + 1

     CASE('Wind dire'); used = .TRUE.
                        upa%direction%data = rtmp%field_rval
                        nvalid = nvalid + 1

     CASE('Wind spee'); used = .TRUE.
                        upa%speed%data = rtmp%field_rval
                        nvalid = nvalid + 1

     CASE('QNH value'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%psfc%data = multiply ( rtmp%field_rval , TOPa )
     !                    upa%pressure%data = multiply ( rtmp%field_rval , TOPa )


     !---------------------------------------------------------------
     ! Default action
 
     CASE DEFAULT     ; used = .FALSE.

    ENDSELECT

    !---------------------------------------------------------------

    IF ( FOR_DEBUG ) THEN
       IF ( used ) THEN
          WRITE ( outfiles ( record_fm+800 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
       ELSE
          WRITE ( outfiles ( record_fm+900 ) , RECFMT ) &
               rtmp%field_ival , rtmp%field_rval , TRIM ( rtmp%field_cval )
       ENDIF
    ENDIF

    rtmp => rtmp%next_record

   ENDDO

   CALL write_rpt_hdr ( iwrite, seqnum, hdr, ieor )
   CALL write_rpt_upa ( iwrite , upa , ieor ) ; upa = empty_upa
   CALL write_rpt_end ( iwrite , record_error , record_warn , ieor )

   IF ( TRACE_ALL ) PRINT  * , 'wr_rap_fm15 : out'

!----------------------------------------------------------------------

ENDSUBROUTINE wr_rap_fm15
