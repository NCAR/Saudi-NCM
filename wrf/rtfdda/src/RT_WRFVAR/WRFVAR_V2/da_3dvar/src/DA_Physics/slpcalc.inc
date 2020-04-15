
      subroutine slpcalc (qvp,tmk,prs,slp,pstx,ter,sigh,ptop,miy,mjx,mkzh)
!------------------------------------------------------------------------------!
!   Create sea-level pressure.
!   Given: at lowest half sigma layer (LHSL): Tv, p
!          at surface: z, p
!          at sea level: z (=0)
!
!   First, get z at LHSL using altimeter equation between LHSL and
!   surface. Then, get p at sea level using alt. eqn. between LHSL
!   and sea level. Assume p. pert. at surface equals p. pert. at LHSL.
!------------------------------------------------------------------------------!
      IMPLICIT NONE
!------------------------------------------------------------------------------!

      INTEGER :: miy, mjx, mkzh
      REAL, dimension (miy,mjx,mkzh) :: qvp, tmk,  prs
      REAL, dimension (miy,mjx)      :: slp, pstx, ter
      REAL, dimension (mkzh)         :: sigh
      REAL :: ptop

      INTEGER :: i, j
      REAL :: expon, exponi, eps_local
      REAL :: tvlhsl, prslhsl, ghtlhsl, psurf
!------------------------------------------------------------------------------!

      eps_local=gas_constant/gas_constant_v
      expon=gas_constant*ussalr/gravity
      exponi=1./expon

      do j=1,mjx
      do i=1,miy

! Virtual Temperature
          tvlhsl=tmk(i,j,mkzh)*(eps_local+qvp(i,j,mkzh)) &
                /(eps_local*(1.+qvp(i,j,mkzh)))

! Full Pressure at Lowest Half Sigma Level 
          prslhsl=prs(i,j,mkzh)

! Surface pressure
          psurf=prs(i,j,mkzh)+(1.-sigh(mkzh))*pstx(i,j)

! Geopotential Height at Lowest Half Sigma Level
          ghtlhsl=ter(i,j)+tvlhsl/ussalr*((psurf/prslhsl)**expon - 1.)

! Sea level pressure

          slp(i,j)=prslhsl*(1.+ussalr/tvlhsl*ghtlhsl)**exponi

      enddo
      enddo

      end subroutine slpcalc
