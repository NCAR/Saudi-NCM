
  FUNCTION oberr_list (iunit) RESULT (platform)
!------------------------------------------------------------------------------!
! Associate NCEP error file id with the corresponding platform string.
!
! See description at http://lnx21.wwb.noaa.gov/oberr/reanl-obs.html
!
! F. Vandenberghe, January 2003
!------------------------------------------------------------------------------!
!
!                TABLE OF IDENTIFICATION OF REPORT TYPES USED
!
!                          CDAS/REANALYSIS SYSTEMS
!
!                                  X = Used
!
! Click on Report ID to view table of observation error profiles
! follow this link to the Forecast Error profiles
!
!        WINDS                           MASS
!
!  202    currently not used       102    SSM/I 7-CHANNEL BRIGHTNESS
!                                         TEMPERATURE
!  210    SYNTHETIC TROPICAL       110    UPPER-AIR HEIGHT BOGUS
!         CYCLONE WINDS
!  220  X RAWINSONDE               120  X RAWINSONDE
!  221  X PIBAL WINDS              121    currently not used
!  222    CLASS SOUNDING           122    CLASS SOUNDING
!  223    PROFILER WINDS           123    currently not used
!  224    VAD (NEXRAD) WINDS       124    currently not used
!  225    NEXRAD RADIAL WINDS      125    currently not used
!  230  X AIREP/PIREP AIRCRAFT     130  X AIREP/PIREP AIRCRAFT FLIGHT-LEVEL
!         FLIGHT-LEVEL
!  231  X ASDAR AIRCRAFT           131  X ASDAR AIRCRAFT FLIGHT-LEVEL
!         FLIGHT-LEVEL
!  232  X RECONNAISSANCE:          132  X RECONNAISSANCE: DROPWINSONDES
!         DROPWINSONDES
!  233  X ACARS AIRCRAFT           133  X ACARS AIRCRAFT FLIGHT-LEVEL
!         FLIGHT-LEVEL
!  240  X NESDIS/NWS: GOES-ODD     140    NESDIS/NWS:GOES-ODD SAT
!         CLD-DRFT LOW-LVL                CLOUD-DRIFT
!  241    INDIA:INSAT CLOUD - DRFT 141    INDIA:INSAT SATELLITE CLOUD-DRIFT
!         L0W-LVL
!  242  X JAPAN: GMS SAT           142    JAPAN: GMS SATELLITE CLOUD-DRIFT
!         CLOUD-DRIFT LOW-LVL
!  243  X ESA: METEOSAT SAT        143    ESA: METEOSAT SATELLITE
!         CLD-DRIFT LOW-LVL               CLOUD-DRIFT
!  244  X NESDIS/NWS: GOES-EVN     144    NESDIS/NWS:GOES-EVEN SAT
!         CLD-DRFT LOW-LVL                CLOUD-DRIFT
!  245    NESDIS: METEOSAT SAT     145    NESDIS:METEOSAT SATELLITE
!         CLD-DRIFT LOW-LVL               CLOUD-DRIFT
!  246    NESDIS: GOES SAT VAS     146    NESDIS: GOES SATELLITE VAS
!         GRADIENT LOW-LVL                GRADIENT
!  248    NESDIS: METEOSAT WATER   148    NESDIS:METEOSAT SATELLITE WATER
!         VAPOR LOW-LVL                   VAPOR
!  249    NESDIS: GOES SAT WATER   149    NESDIS: GOES SATELLITE WATER VAPOR
!         VAPOR LOW-LVL
!  250  X NESDIS/NWS: GOES-ODD     150    SATELLITE MOISTURE BOGUS
!         CLD-DRIFT HI-LVL
!  251    IND1A INSAT  CLOUD-DRIFT 151    RAOB PRECIPITABLE
!         HI-LVL                          WATER(CALCULATED)
!  252  X JAPAN GMS SAT CLOUD -    152    SSM/I PRECIPITABLEWATER RETRIEVAL
!         DRIFT HI-LEVEL
!  253  X ESA : METEOSAT           153    TOVS (NMCEDS) PRECIP. WATER
!         CLOUD-DRFT HI-LVL               RETRIEVAL
!  254  X NESDIS/NWS : GOES-EVN    154    currently not used
!         CLD-DRIFT HI-LVL
!  255    NESDIS: METEOSAT         155    currentlynot used
!         CLOUD-DRIFT HI-LEVEL
!  256    NESDIS: GOES SAT VAS     156    LAND GOES-1 PREC. WATER
!         GRADIENT HIGH-LVL               RETRIEVAL-CLEAR
!  257    ** currently not used    157    LAND GOES-1 PREC. WATER
!         **                              RETRIEVAL-CLDY
!  258    NESDIS: METEOSAT WATER   158    OCEAN GOES-1 PREC. WATER
!         VAPOR HI-LEVEL                  RETRIEVAL-CLEAR
!  259    NESDIS: GOES SAT WATER   159    OCEAN GOES-1 PREC. WATER
!         VAPOR HIGH-LVL                  RETRIEVAL-CLDY
!  260    currently not used       160    LAND TOVS SOUNDING TYPE UNKNOWN
!  261    currently not used       161  X LAND TOVS SOUNDING CLEAR
!  262    currently not used       162  X LAND TOVS SOUNDING NSTAR
!  263    currently not used       163  X LAND TOVS SOUNDING CLOUDY
!  264    currently not used       164    LAND GOES-1 SOUNDINGS CLEAR
!  265    currently not used       165    LAND GOES-1 SOUNDINGS CLOUDY
!  269    currently not used       169    LAND DHSP SOUNDING
!  270  X CONSTANT-LEVEL BALLOON   170    OCEAN TOVS SOUNDING TYPE UNKNOWN
!         (PSEUDO)
!  271    currently not used       171  X OCEAN TOVS SOUNDING CLEAR
!  272    currently not used       172  X OCEAN TOVS SOUNDING NSTAR
!  273    currently not used       173  X OCEAN TOVS SOUNDING CLOUDY
!  274    currently not used       174    OCEAN GOES-1 SOUNDINGS CLEAR
!  275    currently not used       175    OCEAN GOES-1 SOUNDINGS CLOUDY
!  279    currently not used       179    OCEAN DMSP SOUNDING
!  280  X SURFACE MARINE WITH      180  X SURFACE MARINE WITH VALID STATION
!         VALID STATION PRS.              PRESSURE
!  281    SURFACE LAND WITH VALID  181  X SURFACE LAND WITH VALID STATION
!         STATION PRS.                    PRESSURE
!  282  X ATLAS BUOYS(SFC MARINE:  182  X SPLASH-LVL
!         MISSING STN. P)                 DROPWINSONDE/RECCON/EST. MSLP
!  283    SURFACE SSM/I WIND SPEED 183    SFC WITH MISSING STN PRESS & ELEV
!         (DIRECTION=0)                   >7.5 M
!  284    SFC W/MISSING STN PRESS. 184    currently not used
!         & ELEV > 7.5M
!  286    SURFACE MARINE           186    currently not used
!         SCATTEROMETER (ERS1)
!  290    currently not used       190    OPC/NOS POINT MEAN SEA-LVL
!  291    currently not used       191  X AUSTRALIAN PAOBS (MSLP BOGUS)
!  293    currently not used       193    DIGITIZED MEAN SEA-LEVEL PRESSURE
!                                         BOGUS
!------------------------------------------------------------------------------!
  IMPLICIT NONE

  INTEGER            :: iunit
  CHARACTER (len=40) :: platform

!------------------------------------------------------------------------------!

  SELECT CASE (iunit)

    CASE (202); platform = "currently not used" 
    CASE (102); platform = "SSM/I 7-CHANNEL BRIGHTNESS TEMPERATURE"
    CASE (210); platform = "SYNTHETIC TROPICAL CYCLONE WIND"
    CASE (110); platform = "UPPER-AIR HEIGHT BOGUS"
    CASE (220); platform = "RAWINSONDE WIND"
    CASE (120); platform = "RAWINSONDE MASS"
    CASE (221); platform = "PIBAL WIND"
    CASE (121); platform = "currently not used"
    CASE (222); platform = "CLASS SOUNDING WIND" 
    CASE (122); platform = "CLASS SOUNDING MASS"
    CASE (223); platform = "PROFILER WIND"
    CASE (123); platform = "currently not used"
    CASE (224); platform = "VAD (NEXRAD) WIND"      
    CASE (124); platform = "currently not used"
    CASE (225); platform = "NEXRAD RADIAL WIND"      
    CASE (125); platform = "currently not used"
    CASE (230); platform = "AIREP/PIREP AIRCRAFT FLIGHT-LEVEL WIND"
    CASE (130); platform = "AIREP/PIREP AIRCRAFT FLIGHT-LEVEL MASS"
    CASE (231); platform = "ASDAR AIRCRAFT FLIGHT-LEVEL WIND"
    CASE (131); platform = "ASDAR AIRCRAFT FLIGHT-LEVEL MASS"
    CASE (232); platform = "RECONNAISSANCE: DROPWINSONDES WIND"
    CASE (132); platform = "RECONNAISSANCE: DROPWINSONDES MASS"
    CASE (233); platform = "ACARS AIRCRAFT FLIGHT-LEVEL WIND"
    CASE (133); platform = "ACARS AIRCRAFT FLIGHT-LEVEL MASS"
    CASE (240); platform = "NESDIS/NWS: GOES-ODD CLD-DRFT LOW-LVL"
    CASE (140); platform = "NESDIS/NWS:GOES-ODD SAT CLOUD-DRIFT"
    CASE (241); platform = "INDIA:INSAT CLOUD - DRFT L0W-LVL"
    CASE (141); platform = "INDIA:INSAT SATELLITE CLOUD-DRIFT"
    CASE (242); platform = "JAPAN: GMS SAT CLOUD-DRIFT LOW-LVL"
    CASE (142); platform = "JAPAN: GMS SATELLITE CLOUD-DRIFT"
    CASE (243); platform = "ESA: METEOSAT SAT CLD-DRIFT LOW-LVL"               
    CASE (143); platform = "ESA: METEOSAT SATELLITE CLOUD-DRIFT"
    CASE (244); platform = "NESDIS/NWS: GOES-EVN CLD-DRFT LOW-LVL"
    CASE (144); platform = "NESDIS/NWS:GOES-EVEN SAT CLOUD-DRIFT"
    CASE (245); platform = "NESDIS: METEOSAT SAT CLD-DRIFT LOW-LVL"
    CASE (145); platform = "NESDIS:METEOSAT SATELLITE CLOUD-DRIFT"
    CASE (246); platform = "NESDIS: GOES SAT VASGRADIENT LOW-LVL"
    CASE (146); platform = "NESDIS: GOES SATELLITE VAS GRADIENT"
    CASE (248); platform = "NESDIS: METEOSAT WATER VAPOR LOW-LVL"
    CASE (148); platform = "NESDIS:METEOSAT SATELLITE WATER VAPOR"
    CASE (249); platform = "NESDIS: GOES SAT WATER VAPOR LOW-LVL"
    CASE (149); platform = "NESDIS: GOES SATELLITE WATER VAPOR"
    CASE (250); platform = "NESDIS/NWS: GOES-ODD CLD-DRIFT HI-LVL"
    CASE (150); platform = "SATELLITE MOISTURE BOGUS"
    CASE (251); platform = "IND1A INSAT CLOUD-DRIFT HI-LVL"
    CASE (151); platform = "RAOB PRECIPITABLE WATER(CALCULATED)"
    CASE (252); platform = "JAPAN GMS SAT CLOUD - DRIFT HI-LEVEL"
    CASE (152); platform = "SSM/I PRECIPITABLE WATER RETRIEVAL"
    CASE (253); platform = "ESA : METEOSAT CLOUD-DRFT HI-LVL"               
    CASE (153); platform = "TOVS (NMCEDS) PRECIP. WATER RETRIEVAL"
    CASE (254); platform = "NESDIS/NWS : GOES-EVN CLD-DRIFT HI-LVL"
    CASE (154); platform = "currently not used"
    CASE (255); platform = "NESDIS: METEOSAT CLOUD-DRIFT HI-LEVEL"
    CASE (155); platform = "currently not used"
    CASE (256); platform = "NESDIS: GOES SAT VAS GRADIENT HIGH-LVL"
    CASE (156); platform = "LAND GOES-1 PREC. WATER RETRIEVAL-CLEAR"
    CASE (257); platform = "** currently not used **"
    CASE (157); platform = "LAND GOES-1 PREC. WATER RETRIEVAL-CLDY"
    CASE (258); platform = "NESDIS: METEOSAT WATER VAPOR HI-LEVEL"
    CASE (158); platform = "OCEAN GOES-1 PREC. WATER RETRIEVAL-CLEAR"
    CASE (259); platform = "NESDIS: GOES SAT WATER VAPOR HIGH-LVL"
    CASE (159); platform = " OCEAN GOES-1 PREC. WATER RETRIEVAL-CLDY"
    CASE (260); platform = "currently not used"      
    CASE (160); platform = "LAND TOVS SOUNDING TYPE UNKNOWN"
    CASE (261); platform = "currently not used"
    CASE (161); platform = "LAND TOVS SOUNDING CLEAR"
    CASE (262); platform = "currently not used"
    CASE (162); platform = "LAND TOVS SOUNDING NSTAR MASS"
    CASE (263); platform = "currently not used"
    CASE (163); platform = "LAND TOVS SOUNDING CLOUDY MASS"
    CASE (264); platform = "currently not used"
    CASE (164); platform = "LAND GOES-1 SOUNDINGS CLEAR MASS"
    CASE (265); platform = "currently not used"
    CASE (165); platform = "LAND GOES-1 SOUNDINGS CLOUDY MASS"
    CASE (269); platform = "currently not used"
    CASE (169); platform = "LAND DHSP SOUNDING MASS"
    CASE (270); platform = "CONSTANT-LEVEL BALLOON (PSEUDO) WIND"
    CASE (170); platform = "OCEAN TOVS SOUNDING TYPE UNKNOWN MASS"
    CASE (271); platform = "currently not used"
    CASE (171); platform = "OCEAN TOVS SOUNDING CLEAR MASS"
    CASE (272); platform = "currently not used WIND"
    CASE (172); platform = "OCEAN TOVS SOUNDING NSTAR MASS"
    CASE (273); platform = "currently not used"
    CASE (173); platform = "OCEAN TOVS SOUNDING CLOUDY MASS"
    CASE (274); platform = "currently not used"
    CASE (174); platform = "OCEAN GOES-1 SOUNDINGS CLEAR MASS"
    CASE (275); platform = "currently not used"
    CASE (175); platform = "OCEAN GOES-1 SOUNDINGS CLOUDY MASS"
    CASE (279); platform = "currently not used"
    CASE (179); platform = "OCEAN DMSP SOUNDING MASS"
    CASE (280); platform = "SURFACE MARINE WITH VALID STATION PRS. WIND"
    CASE (180); platform = "SURFACE MARINE WITH VALID STATION PRESSURE MASS"
    CASE (281); platform = "SURFACE LAND WITH VALID STATION PRS. WIND"
    CASE (181); platform = "SURFACE LAND WITH VALID STATION PRESSURE MASS"
    CASE (282); platform = "ATLAS BUOYS(SFC MARINE: MISSING STN. P) WIND"
    CASE (182); platform = "SPLASH-LVL DROPWINSONDE/RECCON/EST.MSLP MASS"
    CASE (283); platform = "SURFACE SSM/I WIND SPEED (DIRECTION=0)"
    CASE (183); platform = "SFC WITH MISSING STN PRESS & ELEV > 7.5M MASS)"
    CASE (284); platform = "SFC W/MISSING STN PRESS. & ELEV > 7.5M WINDS)"
    CASE (184); platform = "currently not used"
    CASE (286); platform = "SURFACE MARINE SCATTEROMETER (ERS1)"
    CASE (186); platform = "currently not used"
    CASE (290); platform = "currently not used"      
    CASE (190); platform = "OPC/NOS POINT MEAN SEA-LVL MASS"
    CASE (291); platform = "currently not used"
    CASE (191); platform = "AUSTRALIAN PAOBS (MSLP BOGUS) MASS"
    CASE (293); platform = "currently not used"
    CASE (193); platform = "DIGITIZED MEAN SEA-LEVEL PRESSURE BOGUS MASS"

    CASE DEFAULT

                platform = "unknown"

    END SELECT

   END FUNCTION oberr_list
!------------------------------------------------------------------------------!
