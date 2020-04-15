
 SUBROUTINE FFDDUV (F,D,UEARTH,VEARTH,ID)
!------------------------------------------------------------------------------!
! When ID =  1
! Convert wind speed (F in m/s) and direction (D in degree 0-360) into 
! Earth projected wind components UEARTH and VEARTH in m/s. 
!
! When ID = -1
! Convert projected wind components UEARTH and VEARTH in m/s into wind speed 
! (F in m/s) and direction (D in degree 0-360)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL,    INTENT (inout) :: F,D
    REAL,    INTENT (inout) :: UEARTH, VEARTH
    INTEGER, INTENT (in)    :: ID

    REAL :: AEARTH

    REAL, PARAMETER :: pi   = 3.1415926535897932346
    REAL, PARAMETER :: CONV = 180.0 / pi
!------------------------------------------------------------------------------!

    SELECT CASE (ID)

      CASE (1);

!     CONVERT WIND MODULE/DIRECTION INTO U/V WIND COMPONENTS ON EARTH,
!
      AEARTH = D/CONV

      UEARTH = -F*SIN(AEARTH)
      VEARTH = -F*COS(AEARTH)

      CASE (-1);

!     CONVERT U/V WIND COMPONENTS ON EARTH INTO WIND MODULE/DIRECTION 

      F = SQRT(UEARTH*UEARTH + VEARTH*VEARTH)

      IF (F .EQ. 0.0) THEN
         D = 0.
         RETURN
      ENDIF

      IF (VEARTH .EQ. 0.) THEN

         IF (UEARTH .GT. 0.) D = 270.
         IF (UEARTH .LT. 0.) D =  90.

      ELSE

         AEARTH = ATAN (UEARTH/VEARTH)*CONV

         IF (UEARTH .LE. 0.0 .AND. VEARTH .LE. 0.0 ) D = AEARTH
         IF (UEARTH .LE. 0.0 .AND. VEARTH .GE. 0.0 ) D = AEARTH + 180.0
         IF (UEARTH .GE. 0.0 .AND. VEARTH .GE. 0.0 ) D = AEARTH + 180.0
         IF (UEARTH .GE. 0.0 .AND. VEARTH .LE. 0.0 ) D = AEARTH + 360.0

      ENDIF

      CASE DEFAULT

           WRITE (*,'(A,I2)') 'FFDDUV: UNKNOWN OPTION ',ID
           CALL ABORT

    END SELECT

 END SUBROUTINE FFDDUV
