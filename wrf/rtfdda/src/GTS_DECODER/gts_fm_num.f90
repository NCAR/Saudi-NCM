FUNCTION gts_fm_num ( keyword , keytype )

   ! FUNCTION to identify the code form according to a <keyword>
   !    and a <keytype>. Valid Keytype = 'CODE_HEADER' , 'STATION_ID'.
   !
   ! Created : May 22, 1995    Alexis Lau (HKUST/NCAR)

   CHARACTER ( LEN = *) , INTENT ( IN ) :: keyword , &
                                           keytype
   INTEGER                              :: gts_fm_num

   gts_fm_num = 99

   SELECT CASE ( keytype )

   CASE ( 'STATION_ID' )
      IF     ( keyword(1:4) .EQ. 'BMAA' ) THEN ! Adminstration Messages
         gts_fm_num = 93
      ELSEIF ( keyword(1:4) .EQ. 'WWJP' ) THEN ! Japanese TS warnings
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'TPPN' ) THEN ! Satellite TS Fixes
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'WTPN' ) THEN ! Guam TS Warning
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'WDPN' ) THEN ! Guam TS Diagnostics
         gts_fm_num = 94
      ELSEIF ( keyword(1:4) .EQ. 'WTPQ' ) THEN ! HK0 TS Warning
         gts_fm_num = 94
!     ELSEIF ( keyword(1:4) .EQ. 'WSCI' ) THEN ! Taipei TS Diagnostics
!        gts_fm_num = 94
      ELSEIF ( keyword(1:1) .EQ. 'W'    ) THEN ! Textual TS Warnings
         gts_fm_num = 95
      ELSEIF ( keyword(1:2) .EQ. 'AB'   ) THEN ! General Weather Bulletins
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'UA'   ) THEN ! other ARPs
         gts_fm_num = 96
      ELSEIF ( keyword(1:2) .EQ. 'AA'   ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:3) .EQ. 'RWK'  ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'FP'   ) THEN ! Textual Sounding & Reports
         gts_fm_num = 93
      ELSEIF ( keyword(1:2) .EQ. 'SE'   ) THEN ! Seismic activity report
         gts_fm_num = 93
      ENDIF

   CASE ( 'CODE_HEADER' )
      SELECT CASE ( keyword )

         CASE ( 'SATELLITE' ) ;                       gts_fm_num = 95
         CASE ( 'RSMC' ) ;                            gts_fm_num = 95
         CASE ( 'GEOALERT' ) ;                        gts_fm_num = 93
         CASE ( 'ARP' ) ;                             gts_fm_num = 96
         CASE ( 'AIREP' ) ;                           gts_fm_num = 97
         CASE ( 'NIL' ) ;                             gts_fm_num = 98
         CASE ( 'NIL=' ) ;                            gts_fm_num = 98

         CASE ( 'AAXX' ) ;                            gts_fm_num = 12
         CASE ( 'BBXX' ) ;                            gts_fm_num = 13
         CASE ( 'METAR' ) ;                           gts_fm_num = 15
         CASE ( 'SPECI' ) ;                           gts_fm_num = 16
         CASE ( 'ZZYY' ) ;                            gts_fm_num = 18

         CASE ( 'FFAA' , 'FFBB' , 'GGAA' , 'GGBB' ) ; gts_fm_num = 20
         CASE ( 'RADREP' ) ;                          gts_fm_num = 22

         CASE ( 'PPAA' , 'PPBB' , 'PPCC' , 'PPDD' ) ; gts_fm_num = 32
         CASE ( 'QQAA' , 'QQBB' , 'QQCC' , 'QQDD' ) ; gts_fm_num = 33
         CASE ( 'EEAA' , 'EEBB' , 'EECC' , 'EEDD' ) ; gts_fm_num = 34
         CASE ( 'TTAA' , 'TTBB' , 'TTCC' , 'TTDD' ) ; gts_fm_num = 35
         CASE ( 'UUAA' , 'UUBB' , 'UUCC' , 'UUDD' ) ; gts_fm_num = 36
         CASE ( 'XXAA' , 'XXBB' , 'XXCC' , 'XXDD' ) ; gts_fm_num = 37
         CASE ( 'IIAA' , 'IIBB' , 'IICC' , 'IIDD' ) ; gts_fm_num = 38

         CASE ( 'RRXX' ) ;                            gts_fm_num = 39

         CASE ( 'SSXX' ) ;                            gts_fm_num = 40
         CASE ( 'LLXX' ) ;                            gts_fm_num = 41
         CASE ( 'AMDAR' ) ;                           gts_fm_num = 42
         CASE ( 'ICEAN' ) ;                           gts_fm_num = 44
         CASE ( '10001' , '65556' ) ;                 gts_fm_num = 45
         CASE ( 'GRID' ) ;                            gts_fm_num = 47
         CASE ( 'GRAF' ) ;                            gts_fm_num = 49

         CASE ( 'WINTEM' ) ;                          gts_fm_num = 50
         CASE ( 'TAF' ) ;                             gts_fm_num = 51
         CASE ( 'ARFOR' ) ;                           gts_fm_num = 53
         CASE ( 'ROFOR' ) ;                           gts_fm_num = 54
         CASE ( 'RADOF' ) ;                           gts_fm_num = 57

         CASE ( 'MAFOR' ) ;                           gts_fm_num = 61
         CASE ( 'NNXX' ) ;                            gts_fm_num = 62
         CASE ( 'JJXX' ) ;                            gts_fm_num = 63
         CASE ( 'KKXX' ) ;                            gts_fm_num = 64
         CASE ( 'MMXX' ) ;                            gts_fm_num = 65
         CASE ( 'HHXX' ) ;                            gts_fm_num = 67
         CASE ( 'HYFOR' ) ;                           gts_fm_num = 68

         CASE ( 'CLIMAT' ) ;                          gts_fm_num = 71
         CASE ( 'CLINP' , 'CLISA' ) ;                 gts_fm_num = 73
         CASE ( 'INCLI' , 'NACLI' , 'SPCLI' ) ;       gts_fm_num = 73

         CASE ( 'SFLOC' ) ;                           gts_fm_num = 82
         CASE ( 'SFAZU' ) ;                           gts_fm_num = 83
         CASE ( 'CCAA' , 'CCBB' , 'DDAA' ) ;          gts_fm_num = 85
         CASE ( 'VVAA' , 'VVBB' , 'VVCC' , 'VVDD' ) ; gts_fm_num = 86
         CASE ( 'WWXX' ) ;                            gts_fm_num = 87
         CASE ( 'YYXX' ) ;                            gts_fm_num = 88

      ENDSELECT

   ENDSELECT

ENDFUNCTION gts_fm_num
