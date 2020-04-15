
 SUBROUTINE count_levels (number_obs, obs, index)
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      INTEGER,            INTENT (in) :: number_obs
      TYPE (report),      DIMENSION (number_obs), INTENT (inout) :: obs
      INTEGER,            DIMENSION (number_obs), INTENT (in)    :: index

      TYPE (measurement), POINTER :: current

      INTEGER :: n, nn, ns, l
!----------------------------------------------------------------------------!

! 1.  LOOP OVER STATIONS
! ======================

      ns = 0

stations:&
      DO n = 1, number_obs

! 1.3 Write data sorted by location
!     -----------------------------

      nn = index (n)
      
! 1.4 Skip discarded data
!     -------------------

      IF (obs (nn) % info % discard) CYCLE stations

      ns = ns + 1

! 2.  COUNT UPPER LEVELS
! ======================

      current => obs (nn) % surface 
      l = 0

      DO WHILE (ASSOCIATED (current))

         l = l + 1

         current => current % next

      ENDDO

! 3. SAVE NUMBER OF LEVELS IN VARIABLES obs % info % num_vld_fld
! ==============================================================

      obs (nn) % info % num_vld_fld = l

      ENDDO stations

 END SUBROUTINE count_levels
!------------------------------------------------------------------------------!
