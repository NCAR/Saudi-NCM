
 SUBROUTINE iserr_wmo_upr (missing_out, Coord_mb, Temp_K, Sp_Hum_gkg, uv_ms, &
                           P_mb, H_m, RH_water)
!------------------------------------------------------------------------------!
! Fill instrumental error profiles with default values for upper-air levels.
!
! Default values are based on WMO sounding requirements from the
!"Guide to meteorological instruments and methods of observation."
! WMO No 8, 1996 Edition, Supplement No. 1 (XII, 1997).
!
! Wind speed error has been divided by sqrt(2) to apply to components.
! Original moisture error is given in % of humidity, conversion to specific
! humidity is obtained by use of ICAO standard atmosphere, after linearization.
!
! Geopotential height instrumental error is of the order of 1% (WMO values).
! but only absolute values can be used in this routine, therefore,the
! ECMWF radiosonde "observational" (not instrumental) error is provided here.
! Original ECMWF data were given on 15 pressure levels (Courtier et al. 1999, 
! QJR Metero. Soc., 123). They have been logarithmically interpolated on
! the NCEP 33 levels.
!
! F. Vandenberghe, February 2004
!------------------------------------------------------------------------------!
  IMPLICIT NONE

! ARGUMENTS

  REAL,    INTENT (in) :: missing_out
  REAL,    INTENT (in) :: Coord_mb
  REAL,    INTENT (out), OPTIONAL :: Temp_K, Sp_Hum_gkg, &
                                     uv_ms, P_mb, H_m, RH_water

! OTHER VARIABLES

  REAL    :: Temp_K_ou, Sp_Hum_gkg_ou, uv_ms_ou, P_mb_ou, H_m_ou, RH_water_ou
!------------------------------------------------------------------------------!

! 1.  RESET ARRAYS
! ================

      H_m_ou        = missing_out
      P_mb_ou       = missing_out
      uv_ms_ou      = missing_out
      Temp_K_ou     = missing_out
      RH_water_ou   = missing_out
      Sp_Hum_gkg_ou = missing_out


! 2.  SET WMO SOUNDING ACCURACY REQUIREMENTS AS DEFAULT FOR INSTRUMENTAL ERROR
! ============================================================================

  IF (Coord_mb <=   1.) THEN
       Temp_K_ou = 1.0; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 1.4; H_m_ou = 40.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >    1. .AND. Coord_mb <=   2.) THEN 
       Temp_K_ou = 1.0; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 1.4; H_m_ou = 40.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >    2. .AND. Coord_mb <=   3.) THEN 
       Temp_K_ou = 1.0; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 1.4; H_m_ou = 40.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >    3. .AND. Coord_mb <=   4.) THEN 
       Temp_K_ou = 1.0; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 1.4; H_m_ou = 40.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >    4. .AND. Coord_mb <=   5.) THEN 
       Temp_K_ou = 1.0; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 1.4; H_m_ou = 40.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >    5. .AND. Coord_mb <=  10.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 40.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >   10. .AND. Coord_mb <=  20.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 40.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >   20. .AND. Coord_mb <=  30.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 32.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >   30. .AND. Coord_mb <=  40.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 25.0; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >   40. .AND. Coord_mb <=  50.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 23.7; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >   50. .AND. Coord_mb <=  75.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 22.5; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >   75. .AND. Coord_mb <= 100.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 19.3; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  100. .AND. Coord_mb <= 150.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 18.1; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  150. .AND. Coord_mb <= 200.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 15.2; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  200. .AND. Coord_mb <= 250.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 13.2; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  250. .AND. Coord_mb <= 300.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 11.8; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  300. .AND. Coord_mb <= 350.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 10.7; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  350. .AND. Coord_mb <= 400.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou = 10.2; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  400. .AND. Coord_mb <= 450.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  9.8; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  450. .AND. Coord_mb <= 500.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  9.1; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  500. .AND. Coord_mb <= 550.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  8.4; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  550. .AND. Coord_mb <= 600.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  7.5; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  600. .AND. Coord_mb <= 650.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  6.6; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  650. .AND. Coord_mb <= 700.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  5.9; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  700. .AND. Coord_mb <= 750.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  5.2; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  750. .AND. Coord_mb <= 800.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  4.9; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  800. .AND. Coord_mb <= 850.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  4.7; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  850. .AND. Coord_mb <= 900.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  4.4; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  900. .AND. Coord_mb <= 950.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  4.4; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  950. .AND. Coord_mb <= 1000.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  4.3; RH_water_ou = 5.0;

  ELSE IF (Coord_mb > 1000. .AND. Coord_mb <= 1050.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  4.3; RH_water_ou = 5.0;

  ELSE IF (Coord_mb > 1050. .AND. Coord_mb <= 1100.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  4.3; RH_water_ou = 5.0;

  ELSE IF (Coord_mb >  1100.) THEN 
       Temp_K_ou = 0.5; Sp_Hum_gkg_ou =  2.0 ; P_mb_ou =  0.01; 
       uv_ms_ou  = 0.7; H_m_ou =  4.3; RH_water_ou = 5.0;

  ENDIF

  IF (PRESENT (H_m))        H_m      = H_m_ou
  IF (PRESENT (P_mb))       P_mb     = P_mb_ou
  IF (PRESENT (uv_ms))      uv_ms    = uv_ms_ou
  IF (PRESENT (Temp_K))     Temp_K   = Temp_K_ou
  IF (PRESENT (RH_water))   RH_water = RH_water_ou
  IF (PRESENT (Sp_Hum_gkg)) Sp_Hum_gkg = Sp_Hum_gkg_ou

 END SUBROUTINE iserr_wmo_upr
!------------------------------------------------------------------------------!

 SUBROUTINE iserr_wmo_sfc (missing_out, Coord_mb, Temp_K, Sp_Hum_gkg, uv_ms, &
                           P_mb, H_m, RH_water)
!------------------------------------------------------------------------------!
! Fill instrumental error profiles with default values for upper-air levels.
!
! Default values are based on WMO sounding requirements from the
!"Guide to meteorological instruments and methods of observation."
! WMO No 8, 1996 Edition, Supplement No. 1 (XII, 1997).
!
! Wind speed error has been divided by sqrt(2) to apply to components.
! Original moisture error is given in % of humidity, conversion to specific
! humidity is obtained by use of ICAO standard atmosphere, after linearization.
!
! Geopotential height instrumental error is of the order of 1% (WMO values).
! but only absolute values can be used in this routine, therefore,the
! ECMWF radiosonde "observational" (not instrumental) error is provided here.
! Original ECMWF data were given on 15 pressure levels (Courtier et al. 1999, 
! QJR Metero. Soc., 123) They habe been logarithmically interpolated on
! the NCEP 33 levels.
!
! The lowest level values from iserr_wmo_upr are used at every level
!
! F. Vandenberghe, May 2003
!------------------------------------------------------------------------------!
  IMPLICIT NONE

! ARGUMENTS

  REAL,    INTENT (in) :: missing_out
  REAL,    INTENT (in) :: Coord_mb
  REAL,    INTENT (out), OPTIONAL :: Temp_K, Sp_Hum_gkg, &
                                     uv_ms, P_mb, H_m, RH_water

!------------------------------------------------------------------------------!

      IF (PRESENT (H_m))        H_m      = 4.3
      IF (PRESENT (P_mb))       P_mb     = 0.01
      IF (PRESENT (uv_ms))      uv_ms    = 0.7
      IF (PRESENT (Temp_K))     Temp_K   = 0.5
      IF (PRESENT (RH_water))   RH_water = 5.0
      IF (PRESENT (Sp_Hum_gkg)) Sp_Hum_gkg = 2.0

 END SUBROUTINE iserr_wmo_sfc
!------------------------------------------------------------------------------!
