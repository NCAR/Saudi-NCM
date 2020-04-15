
  SUBROUTINE w10t2 (u10, v10, t2,                         & 
                    uu, vv, tt, tg, land, pstarc, sigmah, & 
                    ts0, tlp, ps0, ptop, ix, jx, kx,      &
                    rough) 
!------------------------------------------------------------------------------!
!  Compute the 2m temperature and 10m wind. This routine follows the similarity 
!  theory and computes the temp and wind over land points only. 
!  The theory was developed over land only (a fancy log extrapolator). 
!  Over ocean nobody knows. So it keys on the `landuse' values.
!
! Original:
!  S. Lownam
!
! Modifications: Fortran 90, Roughness Length passed as arguments
!  F.Vandenberghe,  February 2003
!------------------------------------------------------------------------------!
      IMPLICIT NONE
!------------------------------------------------------------------------------!
! ARGUMENTS

      INTEGER, INTENT (in) :: ix, jx, kx
      REAL,    INTENT (in) :: ts0, tlp, ps0, ptop

      REAL, DIMENSION (ix,jx),    INTENT (out) :: u10, v10, t2
      REAL, DIMENSION (ix,jx,kx), INTENT (in)  :: uu, vv, tt
      REAL, DIMENSION (ix,jx),    INTENT (in)  :: tg, land, pstarc
      REAL, DIMENSION (kx),       INTENT (in)  :: sigmah

      REAL, DIMENSION (ix,jx),    INTENT (in)  :: rough
  
!------------------------------------------------------------------------------!
! LOCAL VARIABLE

      INTEGER :: i, ii, jj, icat
      REAL    :: sig1, sig2
      REAL    :: p01, p02, alnp, catlnd, flvl
      REAL    :: za, z1, z2, z2oz0, zaoz0, z10oz0
      REAL    :: z0, zl0
!------------------------------------------------------------------------------!
! DEFAULT VALUES FOR ROUGHNESS LENGTH

!     REAL, DIMENSION (54) :: sfz0

!     DATA sfz0  /50.,15., 12.,  50.,              &
!                 50., 40.,0.01,  20., 10., 10.,  5., 50., 15.,  8.,&
!                 10.,  2.,   1., 12., .01, 50., 20., 15., 50., 40.,&
!                 15.,  1.,  15., 50., 20., 12., 20., 50., 15., 50.,&
!                 12., 10.,  10., 50., 50., 50., 15., 50., 15., 10.,&
!                 50., 50.,  50., 50., 50., 12., 10., 50., 50., 50./
!------------------------------------------------------------------------------!

	  sig1 = 1.
	  sig2 = sigmah (kx)

          DO ii=1,ix
             DO jj=1,jx

! Old categories
!               catlnd = land(ii,jj)
!               icat   = nint(catlnd)     ! Land category
!	        z0 = sfz0(icat)           ! Roughness length in m

! Input from LANDUSE.TBL file

	        z0 = rough (ii,jj)*100.   ! Roughness length in m

! Pressure at surface and lowest half sigma level

                p01 = pstarc(ii,jj)*sig1 + ptop   ! top sigma Reference pres
                p02 = pstarc(ii,jj)*sig2 + ptop   ! bottom sigma reference pres 

                alnp = ALOG(p01/ps0)

                z1   = -(gas_constant*ts0*alnp+gas_constant*tlp*alnp*alnp/2.)&
		     /   gravity

                alnp = alog(p02/ps0)
	        z2   = -(gas_constant*ts0*alnp+gas_constant*tlp*alnp*alnp/2.)&
		     /   gravity

                za = (z2-z1)*100.

	        flvl = 200.

                DO WHILE (flvl .le. za)

                   z2oz0 = alog(flvl/z0)
                   zaoz0 = alog(za/z0)
                   z10oz0= alog(1000./z0)

                   t2  (ii,jj) = tg(ii,jj)+z2oz0*(tt(ii,jj,kx)-tg(ii,jj))/zaoz0
                   u10(ii,jj) = (uu(ii,jj,kx)/zaoz0)*z10oz0
                   v10(ii,jj) = (vv(ii,jj,kx)/zaoz0)*z10oz0
                   flvl = flvl + 200.

                ENDDO

             ENDDO
          ENDDO

  END SUBROUTINE w10t2
!------------------------------------------------------------------------------!
