!INCLUDE 'bulletin.module'
!INCLUDE 'record.module'

SUBROUTINE interpret_bulletin

   ! Driver Routine for interpreting bulletin
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   USE bulletin_def
   USE record_def
   IMPLICIT NONE
   LOGICAL                  :: decoded

   IF ( TRACE_MOST ) PRINT  * , 'interpret_bulletin : in ' , record_fm

   bul_stat ( record_fm, number ) = bul_stat ( record_fm, number ) + 1
   bulletin_error = 0
   bulletin_warn  = 0

   CALL record_initialize ( record_fm )
   CALL assign_outfile ( 0 )
   IF ( outfiles ( record_fm ) .LT. 0 ) THEN
      CALL prog_abort(' ===> Unexpected condition, outfiles(record_fm) < 0 ')
   ENDIF

   CALL bulletin_print ( outfiles ( record_fm ) )

   SELECT CASE ( record_fm )

      CASE ( 12 , 13 ) ; decoded = .TRUE. 
                         CALL decode_fm12
                         CALL wr_rawins_fm12
                         CALL wr_rap_fm12
                                         
      CASE ( 15 , 16 ) ; decoded = .TRUE. 
                         CALL decode_fm15
                         CALL wr_rap_fm15
                                         
      CASE ( 32 : 34 ) ; decoded = .TRUE.
                         CALL decode_fm32
                         CALL wr_rawins_fm35
                         CALL wr_rap_fm35
                                         
      CASE ( 35 : 38 ) ; decoded = .TRUE.
                         CALL decode_fm35
                         CALL wr_rawins_fm35
                         CALL wr_rap_fm35
                                         
      CASE ( 86      ) ; decoded = .TRUE.
                         CALL decode_fm86
                         CALL wr_rawins_fm86

      CASE ( 88      ) ; decoded = .TRUE.
                         CALL decode_fm88
                         CALL wr_rawins_fm88
                         CALL wr_rap_fm88

      CASE ( 97      ) ; decoded = .TRUE.
                         CALL decode_airep
                         CALL wr_rap_airep

      CASE DEFAULT     ; decoded = .FALSE.

   ENDSELECT

   IF ( decoded ) THEN

      CALL assign_outfile ( 1 )

      ! input bulletin as the decoder sees it
      CALL bulletin_print ( outfiles ( record_fm+100 ) )

      ! decoded text
      CALL record_print ( outfiles ( record_fm+100 ) )

      ! error(s) or warning(s) found, PRINT output to appropriate files
      IF ( TRACE_LEAST ) THEN
         IF ( bulletin_error  > 0 ) THEN
            CALL bulletin_print ( ierr )
            CALL record_print   ( ierr )
            CALL bulletin_print ( ierr0 )
            CALL record_print   ( ierr0 )
         ENDIF
      ENDIF

      IF ( TRACE_MOST ) THEN
         IF ( bulletin_warn > 0 ) THEN
            CALL bulletin_print ( iwarn )
            CALL record_print   ( iwarn )
            CALL bulletin_print ( iwarn0 )
            CALL record_print   ( iwarn0 )
         ENDIF
      ENDIF

   ENDIF

   IF ( FLUSH_FILES ) CALL flush_outfiles

   IF ( TRACE_ALL ) PRINT  * , 'interpret_bulletin : out'

ENDSUBROUTINE interpret_bulletin
