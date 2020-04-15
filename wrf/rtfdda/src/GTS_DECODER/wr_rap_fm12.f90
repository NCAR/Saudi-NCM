!INCLUDE 'record.module'
INCLUDE 'mm5obs.module'

SUBROUTINE wr_rap_fm12

   USE record_def
   USE mm5obs_def
   IMPLICIT NONE

   TYPE ( stack ) , POINTER  :: rtmp
   TYPE ( mm5_hdr )          :: hdr
   TYPE ( meas_data )        :: upa

   INTEGER  :: iwrite , ieor
   LOGICAL  :: used

! Modified Mar 16 2000 - SLN
!    Changed the ROHK identifier to ATEC
! Modified Aug 12 2010 - FCV
!     Changed the ATEC identifier to NOAA
  
   IF ( .NOT. FOR_RAP ) return

   IF ( TRACE_MOST ) PRINT  * , 'wr_rap_fm12 : in '

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
   nvalid = 0
   hdr%info%source = 'GTS (NOAA) ' // msg_header

   upa%pressure%data = fake_stnpre   ! only for fm12
 
   DO WHILE ( ASSOCIATED ( rtmp ) )

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
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)
 
     CASE('New messa'); used = .TRUE.
                        CALL write_rpt_hdr ( iwrite , seqnum, hdr , ieor )
                        CALL write_rpt_upa ( iwrite , upa , ieor )
                        CALL write_rpt_end ( iwrite , record_error, record_warn , ieor )

                        hdr    = empty_hdr
                        upa    = empty_upa
                        nvalid = 0
                        hdr%info%source = 'GTS (NOAA) ' // msg_header

                        upa%pressure%data = fake_stnpre     ! only for fm12
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     CASE('Rev. Obse'); used = .TRUE.
                        nvalid = nvalid + 1
                        rev_yymmdd = rtmp%field_ival
                        rev_hhmmss = NINT(rtmp%field_rval)

     !---------------------------------------------------------------
     ! location information 
 
     CASE('Platform '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = '-6666'
                        hdr%location%name = rtmp%field_cval(12:)
 
     CASE('SHIP or T'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%location%id = '-7777'
                        hdr%location%name = rtmp%field_cval(12:)
 
     CASE('Latitude '); used = .TRUE.
                        hdr%location%latitude = rtmp%field_rval/100
 
     CASE('Longitude'); used = .TRUE.
                        hdr%location%longitude = rtmp%field_rval/100
                        IF ( fm .EQ. 12 ) THEN
                           hdr%location%id = rtmp%field_cval(6:10)
                           hdr%location%name = rtmp%field_cval(35:rlen)
                        ENDIF

     !---------------------------------------------------------------
     ! Source information

     CASE('Elevation'); used = .TRUE.
                        hdr%info%elevation = rtmp%field_rval
                        upa%height%data = rtmp%field_rval
 
     !---------------------------------------------------------------
     ! Terrestrial information
     !     slp*f, ref_pres*f, ground_t*f, sst*f, psfc*f,
     !     precip*f, t_max*f, t_min*f, p_tend03*f,
     !     cloud_low*f, cloud_med*f, cloud_hi*f, ceiling*f

     CASE('Mean SLP '); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%slp%data = multiply ( rtmp%field_rval , TOPa )
 
     ! Decoded Ground Temperature look very strange, skipping it
   ! CASE('Ground / '); used = .TRUE.
                        nvalid = nvalid + 1
   !                    hdr%ground%ground_t%data = add ( TOKELVIN , rtmp%field_rval )

     CASE('Sea Surfa'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%sst%data = add ( TOKELVIN , rtmp%field_rval )

     CASE('Station p'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%psfc%data = multiply ( rtmp%field_rval , TOPa )
                        upa%pressure%data = multiply ( rtmp%field_rval , TOPa )

     CASE('Precip. a'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%precip%data = rtmp%field_rval

     CASE('Max Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%t_max%data = add ( TOKELVIN , rtmp%field_rval )
 
     CASE('Min Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%t_min%data = add ( TOKELVIN , rtmp%field_rval )
 
     CASE('Nght Tmin'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%t_min_night%data = add ( TOKELVIN , rtmp%field_rval )

     CASE('Pre. Tend'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%p_tend03%data = rtmp%field_rval
 
     CASE('24-h Pre.'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%p_tend24%data = rtmp%field_rval
 
     CASE('Total Clo'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%cloud_cvr%data = rtmp%field_rval
 
     CASE('Lowest cl'); used = .TRUE.
                        nvalid = nvalid + 1
                        hdr%ground%ceiling%data = rtmp%field_rval
 
     !---------------------------------------------------------------
     ! Meteorological information

     CASE('Air Tempe'); used = .TRUE.
                        nvalid = nvalid + 1
                        upa%temperature%data = add ( TOKELVIN , rtmp%field_rval )
 
     CASE('Dew Point'); used = .TRUE.
                        nvalid = nvalid + 1
                        upa%dew_point%data = add ( TOKELVIN , rtmp%field_rval )
 
     CASE('Wind dire'); used = .TRUE.
                        nvalid = nvalid + 1
                        upa%direction%data = rtmp%field_rval
 
     CASE('Wind spee'); used = .TRUE.
                        nvalid = nvalid + 1
                        upa%speed%data = rtmp%field_rval

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

   CALL write_rpt_hdr ( iwrite , seqnum, hdr , ieor )
   CALL write_rpt_upa ( iwrite , upa , ieor )
   CALL write_rpt_end ( iwrite , record_error, record_warn , ieor )

   IF ( TRACE_ALL ) PRINT  * , 'wr_rap_fm12 : out'

ENDSUBROUTINE wr_rap_fm12
