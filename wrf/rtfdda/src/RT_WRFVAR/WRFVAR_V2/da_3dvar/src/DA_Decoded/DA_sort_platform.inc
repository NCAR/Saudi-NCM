SUBROUTINE sort_platform (nobs_max, obs, number_of_obs,  &
                          nsynops, nshipss, nmetars, npilots, nsounds, &
                          nsatems, nsatobs, naireps, ngpspws, nssmt1s, &
                          nssmt2s, nssmis,  ntovss,  namdars, ntamdars, &
                          nqscats, &
                          nprofls, nbuoyss, nothers, title)

!------------------------------------------------------------------------------
! Count observations per WMO platform
!
!------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,              INTENT (in)                         :: nobs_max
  TYPE (report),        INTENT (inout), DIMENSION (nobs_max):: obs
  INTEGER,              INTENT (in)                         :: number_of_obs
  INTEGER,              INTENT (inout)                      :: nsynops,nshipss,&
                                                               nmetars,npilots,&
                                                               nsounds,nsatems,&
                                                               nsatobs,naireps,&
                                                               ngpspws,nssmt1s,&
                                                               nssmt2s,nssmis, &
                                                               ntovss,         &
                                                               namdars,ntamdars,&
                                                               nqscats,nprofls,&
                                                               nbuoyss, nothers
  CHARACTER (LEN = 80), INTENT (in), OPTIONAL               :: title
       

  TYPE (measurement ) ,POINTER                              :: current
  INTEGER                                                   :: loop_index

  CHARACTER (LEN = 40)                                      :: platform
  INTEGER                                                   :: fm, is_sound
  INTEGER                                                   :: nvalids,nmultis,&
                                                               nsingls,nlevels
  INTEGER                                                   :: nuppers

!------------------------------------------------------------------------------!

      IF (PRESENT (title)) THEN
      CALL prelog
      WRITE (log_unit,'(A)')  &
'------------------------------------------------------------------------------'
      CALL prelog
      WRITE (UNIT = log_unit, FMT = '(A)') TRIM (title)
      ENDIF


! 0.  RESET
! =========

      nvalids = 0
      nmultis = 0
      nsingls = 0
      nlevels = 0

      nsynops = 0
      nmetars = 0
      nshipss = 0
      npilots = 0
      nsounds = 0
      namdars = 0
      ntamdars = 0
      nsatems = 0
      nsatobs = 0
      naireps = 0
      ngpspws = 0
      nssmt1s = 0
      nssmt2s = 0
      nssmis  = 0
      ntovss  = 0
      nqscats = 0
      nprofls = 0
      nbuoyss = 0
      nothers = 0


! 1. LOOP OVER STATIONS
! ====================

stations: &
      DO loop_index = 1, number_of_obs


! 1.1 Check if record is valid
!     ------------------------

stations_valid: &
      IF (obs(loop_index)%info%discard ) THEN

      CYCLE  stations

      ELSE stations_valid

      nvalids = nvalids + 1

! 1.2 WMO Platform code
!     -----------------

!      READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

       !  RT-FDDA uses different codes, bases on name and platform

       fm = fm_wmo (obs (loop_index) % location % name, &
                    obs (loop_index) % info % platform)

       IF (fm .LE. 0)  THEN
           WRITE (log_unit,'(A,A,I3,A)') obs (loop_index) % info % platform, &
                                        "FM =",fm," IS INVALID."
           CYCLE  stations
       ENDIF

! 1.3 interpret code
!     ---------------

       CALL fm_decoderd (fm, platform, &
                         synop=nsynops, ship =nshipss, metar=nmetars, &
                         pilot=npilots, sound=nsounds, satem=nsatems, &
                         satob=nsatobs, airep=naireps, gpspw=ngpspws, &
                         ssmt1=nssmt1s, ssmt2=nssmt2s, ssmi =nssmis,  &
                         tovs =ntovss,  amdar=namdars, tamdar=ntamdars, &
                         qscat=nqscats, &
                         profl=nprofls, buoy =nbuoyss, other=nothers )

! 1.4 Initialise pointer to surface level
!     -----------------------------------

      current => obs (loop_index) % surface

! 2.  LOOP ON UPPER LEVELS
! ========================

      is_sound  = -1
      nuppers   =  0

levels: DO WHILE (ASSOCIATED (current))

! 2.1 Found one level, increment
!     --------------------------

      is_sound = is_sound + 1
      nuppers  = nuppers  + 1
      nlevels  = nlevels  + 1

! 2.2 Go to next level
!     ----------------

      current => current%next

      ENDDO levels

! 2.3 Count surface obs and sounding
!     ------------------------------

      if (is_sound .gt. 0) then
          nmultis = nmultis + 1
      else
          nsingls = nsingls + 1
      endif

! 3. ASSIGN THE NUMBER OF UPPER-AIR LEVELS TO STATION
! ===================================================

!     obs (loop_index) % info % levels = nuppers


! 4.  GO TO NEXT STATION
! ======================

! 4.1 Go to next valid station
!     ------------------------

      ENDIF  stations_valid

! 4.2 Go to next station
!     ------------------
      ENDDO  stations


! 5.  PRINT OUT
! =============

      IF (print_detail >= 0) THEN
      WRITE (log_unit, '(A,I6)') ' SYNOP  reports:',nsynops
      WRITE (log_unit, '(A,I6)') ' SHIPS  reports:',nshipss
      WRITE (log_unit, '(A,I6)') ' BUOY   reports:',nbuoyss
      WRITE (log_unit, '(A,I6)') ' METAR  reports:',nmetars
      WRITE (log_unit, '(A,I6)') ' PILOT  reports:',npilots
      WRITE (log_unit, '(A,I6)') ' SOUND  reports:',nsounds
      WRITE (log_unit, '(A,I6)') ' AMDAR  reports:',namdars
      WRITE (log_unit, '(A,I6)') ' TAMDAR reports:',ntamdars
      WRITE (log_unit, '(A,I6)') ' SATEM  reports:',nsatems
      WRITE (log_unit, '(A,I6)') ' SATOB  reports:',nsatobs
      WRITE (log_unit, '(A,I6)') ' AIREP  reports:',naireps
      WRITE (log_unit, '(A,I6)') ' GPSPW  reports:',ngpspws
      WRITE (log_unit, '(A,I6)') ' SSMT1  reports:',nssmt1s
      WRITE (log_unit, '(A,I6)') ' SSMT2  reports:',nssmt2s
      WRITE (log_unit, '(A,I6)') ' SSMI   reports:',nssmis
      WRITE (log_unit, '(A,I6)') ' TOVS   reports:',ntovss
      WRITE (log_unit, '(A,I6)') ' QSCAT  reports:',nqscats
      WRITE (log_unit, '(A,I6)') ' PROFL  reports:',nprofls 
      WRITE (log_unit, '(A,I6)') ' OTHER  reports:',nothers
      WRITE (log_unit, '(A,3(I6,A))') &
                          ' Total reports: ',nsynops+nshipss+nmetars +&
                                             npilots+nsounds+nsatems +&
                                             nsatobs+naireps+ngpspws +&
                                             nssmt1s+nssmt2s+nssmis  +&
                                             ntovss +namdars+ntamdars+&
                                             nqscats                 +&
                                             nprofls+nbuoyss+nothers, &
      " = ",nsingls," single + ",nmultis," multi-level reports."
       ENDIF


! 4.  END
! =======
      RETURN

      END SUBROUTINE sort_platform
