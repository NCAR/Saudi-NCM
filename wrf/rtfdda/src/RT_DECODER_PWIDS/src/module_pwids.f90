
 MODULE module_pwids
!==============================================================================!
! Module defining the pwids data structure.
!
! Copyright UCAR (c) 1992 - 2004.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Program (RAP),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, April 2004.
!==============================================================================!
!                          DATA STRUCTURES
!==============================================================================!

   IMPLICIT NONE

!  MAXIMAL NUMBER OF PWIDS STATIONS THAT CAN BE READ FILE Instruments.txt
!  ===============================                                       
!  This is the maximal number of physical stations on the field, not the
!  number of reports read in the data file. There might be several reports
!  from the same station (ie 1 report every minute).

   INTEGER, PARAMETER  :: npwids_max = 15


!  NUMBER OF VERTICAL MEASUREMENTS: 
!  ===============================                                       
!  PWIDS are surface station, there is 1 vertical level only.

   INTEGER, PARAMETER  :: pwid_vertical_levels = 1

!  TIME ORIGIN 
!  ===========

   CHARACTER (LEN =  19), PARAMETER :: pwid_date_start= '1970-01-01_00:00:00'

!  PWIDS DATA STRUCTURE
!  ====================

!   PWIDS Network information

    TYPE pwid_station_type

     INTEGER               :: id              ! Id number
     REAL                  :: elevation       ! PWIDS elvation in m
     REAL                  :: latitude        ! PWIDS longitude in deg
     REAL                  :: longitude       ! PWIDS latitude in deg
     CHARACTER (LEN=40)    :: description     ! Description, eg Pentagon, etc. 

   END TYPE pwid_station_type

! PWIDS Station information 

   TYPE pwid_type

!   Information found in a *.dat file

    INTEGER :: logger_id
    INTEGER :: year       ! CCYY
    INTEGER :: juliand    ! Julian day 
    INTEGER :: hhmm       ! Time of Day
    INTEGER :: ss         ! Time of Day
    INTEGER :: id         !
    REAL    :: volt       !
    REAL    :: spd        ! m/s
    REAL    :: dir        ! Degrees north
    REAL    :: tem        ! Celcius
    REAL    :: rhu        !  %

!   Additional variable for handling 

    CHARACTER (LEN=19) :: date19

    INTEGER :: ntimes 

   END TYPE pwid_type

!  Missing flag

   REAL, PARAMETER  :: rhu_missing = 999.9
   REAL, PARAMETER  :: dir_missing = 999.9
   REAL, PARAMETER  :: spd_missing = 999.9
   REAL, PARAMETER  :: tem_missing = 999.9
   REAL, PARAMETER  :: hei_missing = 999.9

!==============================================================================!
!                       ROUTINES
!==============================================================================!
 CONTAINS

!------------------------------------------------------------------------------!

      SUBROUTINE reset_pwid (pwid)
!------------------------------------------------------------------------------!
!     Reset a PWID data structure to 0
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      TYPE (pwid_type), INTENT (out) :: pwid
!------------------------------------------------------------------------------!

      pwid % logger_id = 0
      pwid % year    = 0   
      pwid % juliand = 0  
      pwid % hhmm = 0
      pwid % ss   = 0
      pwid % id   = 0
      pwid % rhu = 0.
      pwid % spd = 0.
      pwid % dir = 0.
      pwid % tem = 0.

      pwid % ntimes = 0

      pwid % date19 = pwid_date_start

      END SUBROUTINE reset_pwid
!------------------------------------------------------------------------------!

      SUBROUTINE reset_pwid_met (pwid)
!------------------------------------------------------------------------------!
!     Reset the MET variables of a PWIDS data structure to 0
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      TYPE (pwid_type), INTENT (out) :: pwid
!------------------------------------------------------------------------------!

      pwid % id  = 0
      pwid % rhu = 0.
      pwid % spd = 0.
      pwid % dir = 0.
      pwid % tem = 0.

      pwid % ntimes = 0

      END SUBROUTINE reset_pwid_met
!------------------------------------------------------------------------------!

      SUBROUTINE reset_pwids (npwids, pwids)
!------------------------------------------------------------------------------!
!     Reset an array of PWID data structures to 0
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      INTEGER, INTENT (in) :: npwids
      TYPE (pwid_type), DIMENSION (npwids), INTENT (out) :: pwids
!------------------------------------------------------------------------------!

      pwids % logger_id = 0
      pwids % year    = 0   
      pwids % juliand = 0  
      pwids % hhmm = 0
      pwids % ss   = 0
      pwids % rhu = 0.
      pwids % spd = 0.
      pwids % dir = 0.
      pwids % tem = 0.

      pwids % id  = 0
      pwids % rhu = 0.
      pwids % spd = 0.
      pwids % dir = 0.
      pwids % tem = 0.

      pwids % ntimes = 0

      pwids % date19 = "0000-00-00_00:00:00"

      END SUBROUTINE reset_pwids
!------------------------------------------------------------------------------!

      SUBROUTINE reset_pwids_stations (npwids, stations)
!------------------------------------------------------------------------------!
!     Reset the PWIDS data structure to the missing flag
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      INTEGER, INTENT (in) :: npwids
      TYPE (pwid_station_type), DIMENSION (npwids), INTENT (out) :: stations
!------------------------------------------------------------------------------!

      stations % id =  0
      stations % latitude  =  hei_missing
      stations % longitude =  hei_missing
      stations % elevation =  hei_missing
      stations % description = ""

      END SUBROUTINE reset_pwids_stations
!------------------------------------------------------------------------------!
!==============================================================================!
 END MODULE module_pwids
