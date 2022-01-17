! FORTRAN90 codes for python (2.7.5 or later).
! some of the forrtan codes were adapted from wrf_user.f in WRF-NCL package with
! revision for FORTRAN90 standard and for python (f2py) : 
! f2py -c -m wrffortran wrf_fortran90.f90 --fcompiler=pg
! f2py -c --help-fcompiler
!
! W. WU (wu80012@gmail.com) May 23 2013

! DCOMPUTERTK: computer temperature in kevin from potential temeprature (theta)
!              and pressure

      SUBROUTINE DCOMPUTETK(PRES,THETA, TK, DIMSZ)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: DIMSZ
      REAL(8), INTENT(IN), DIMENSION(DIMSZ) :: PRES     ! in unit of hPa (mb)
      REAL(8), INTENT(IN), DIMENSION(DIMSZ) :: THETA
      REAL(8), INTENT(OUT), DIMENSION(DIMSZ) :: TK

      REAL(8), PARAMETER :: P1000MB=1000.0, R_D=287.0, CP=7.0*R_D/2.0

      print*, P1000MB,  R_D, CP
      TK = THETA * ((PRES/P1000MB)** (R_D/CP))

      END SUBROUTINE DCOMPUTETK

! DCOMPUTERH: compute relative humidity (RH)
! NOTE: works in FORTRAN, but not in python????

      SUBROUTINE DCOMPUTERH(QV,P,T,RH,DIMSZ)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: DIMSZ
      REAL(8), INTENT(IN),  DIMENSION(DIMSZ) :: QV,P,T
      REAL(8), INTENT(OUT), DIMENSION(DIMSZ) :: RH

      REAL(8), PARAMETER :: SVP1=0.6112, SVP2=17.67, SVP3=29.65, SVPT0=273.15 
      REAL(8), PARAMETER :: R_D=287.0, R_V=461.6, EP_2=R_D/R_V, EP_3=0.622 

      REAL(8), DIMENSION(DIMSZ) :: ES, QVS

      ES = 10.0D0*SVP1*EXP(SVP2* (T-SVPT0)/(T-SVP3))
      QVS = EP_3*ES/ (0.01D0*P- (1.0D0-EP_3)*ES)
      RH = 100.0D0*DMAX1(DMIN1(QV/QVS,1.0D0),0.0D0)

      END SUBROUTINE DCOMPUTERH 

! DCOMPUTETD: computer dew point temperature (TD) 
! NOTE: does not work properly in FORTRAN and python ????
      SUBROUTINE DCOMPUTETD(QV, PRES, TD, DIMSZ)

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::  DIMSZ
      REAL(8), INTENT(IN), DIMENSION(DIMSZ) :: PRES, QV 
      REAL(8), INTENT(OUT), DIMENSION(DIMSZ) :: TD

      REAL(8), DIMENSION(DIMSZ) :: T, TDC

!     T = DMAX1(QV,0.0D0)
! vapor pressure
!     T = T*PRES/ (.622D0+T)
! avoid problems near zero
!     T = DMAX1(T,0.001D0)
!     TD = (243.5D0*LOG(T)-440.8D0)/ (19.48D0-LOG(T))
!     DO I = 1,NX
          T  = DMAX1(QV,0.D0)
! vapor pressure
          TDC =  T*PRES/ (.622D0+T)

! avoid problems near zero
          TDC = DMAX1(TDC,0.001D0)
          TD = (243.5D0*LOG(TDC)-440.8D0)/ (19.48D0-LOG(TDC))
!     END DO

      END SUBROUTINE DCOMPUTETD 

! RHandTD: computer RH, and TD from T, Q, and P
! T, P, and Q are  in units of Celsius, hPa (mb) and Kg/Kg.
! RH in % and TD in Celsius.
! W. WU (wu80012@gmail.com) May 23 2013

      SUBROUTINE RHANDTD(QV, PRES, T, RH, TD, DIMSZ)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: DIMSZ
      REAL(8), INTENT(IN),  DIMENSION(0:DIMSZ-1) :: PRES, QV, T
      REAL(8), INTENT(OUT), DIMENSION(0:DIMSZ-1) :: RH, TD

      REAL(8), DIMENSION(0:DIMSZ-1) :: VP, VPS 

      VP = DMAX1(QV,0.0D0)
      VP = VP * PRES/(0.622D0+VP)
      VPS = 6.112D0*EXP((17.67D0*T)/(T+243.5D0))
      RH = 100.0D0 * DMAX1(DMIN1(VP/VPS, 1.0D0), 0.0D0)

      VPS = LOG(VP/6.112D0)
      VPS = 17.72D0/VPS - 1.0D0
      TD  = 243.5D0/VPS

      END SUBROUTINE RHANDTD 

! RHfromTD: compute RH from T and TD in Celsius
! W. WU (wu80012@gmail.com) May 24 2013

      SUBROUTINE RHFROMTD(T, TD, RH, DIMSZ)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: DIMSZ
      REAL(8), INTENT(IN),  DIMENSION(0:DIMSZ-1) :: T, TD
      REAL(8), INTENT(OUT), DIMENSION(0:DIMSZ-1) :: RH

      REAL(8), DIMENSION(0:DIMSZ-1) :: VP, VPS 

      VP  = 6.112D0*EXP((17.67D0*TD)/(TD+243.5D0))
      VPS = 6.112D0*EXP((17.67D0*T)/(T+243.5D0))
      RH = 100.0D0*DMAX1(DMIN1(VP/VPS, 1.0D0),0.0D0)

      END SUBROUTINE RHFROMTD 
