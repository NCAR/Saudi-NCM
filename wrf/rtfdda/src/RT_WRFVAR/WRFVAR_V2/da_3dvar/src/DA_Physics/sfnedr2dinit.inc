      subroutine sfnedr2dinit(nlags)
!
! calculate universal models for velocity structure functions
! assuming 2D isotropic Lindborg model
! nlags   - maximum number of lags for best-fit structure functions

      integer   k,nlags,maxlag,i,np
      real dmodxx,dmodyy,dmodxy,dmodyx,dmodTx,pp,slope,s,dx
      parameter(maxlag=10,np=36)
      parameter(pp=4./3.,slope=2./3.,dx=30730.)
      common /sfnmodvel/ dmodxx(maxlag,np), dmodyy(maxlag,np), &
      dmodxy(maxlag,np), dmodyx(maxlag,np)
      common /sfnmodT/ dmodTx(maxlag,np)

      include 'DA_Physics/params_vel_atc1_sfn.inc'	!coefficients for velocity structure functions
      include 'DA_Physics/params_temp_atc1_sfn.inc'!coefficients for temperature structure functions

      do k=1,nlags

      do i=1,np
      s=dx*float(k)

! Longitudinal structure function in x direction (EW)
      dmodxx(k,i)=2.0*sfngasp(s,1)*(s/p2xx(i))**pp &
      /(1.+(s/p2xx(i))**pp+p3xx(i)*(s/p2xx(i))**slope)

! Longitudinal structure function in y direction (NS)
      dmodyy(k,i)=2.0*sfngasp(s,1)*(s/p2yy(i))**pp &
      /(1.+(s/p2yy(i))**pp+p3yy(i)*(s/p2yy(i))**slope)

! Transverse structure function in x direction (EW)
      dmodyx(k,i)=10.0/3.0*sfngasp(s,2)*(s/p2yx(i))**pp &
      /(1.+(s/p2yx(i))**pp+p3yx(i)*(s/p2yx(i))**slope)

! Transverse structure function in y direction (NS)
      dmodxy(k,i)=10.0/3.0*sfngasp(s,2)*(s/p2xy(i))**pp &
      /(1.+(s/p2xy(i))**pp+p3xy(i)*(s/p2xy(i))**slope)

! Temperature structure function in x direction (EW)
      dmodTx(k,i)=sfngasp(s,1)*(s/p2(i))**pp &
      /(1.+(s/p2(i))**pp+p3(i)*(s/p2(i))**slope)

      enddo	!end of pressure level i

      enddo	!end of lag k

      return
      end subroutine sfnedr2dinit

      Function sfngasp(s,mode)
! calculate 2D isotropic Lindborg model for structure functions
! s     - separation (m)
! mode  - type of calculation
! mode=1- longitudinal structure function
! mode=2- transverse structure function
      real b1,c1,b2,c2,slope,s,sfngasp
      integer mode
      parameter(b1=6.66666666e-7,c1=4.444444444e-8)	!Lindborg with r^(2/3)
      parameter(b2=3.*(3.*b1-c1)/5.,c2=9.*c1/5.)	!Lindborg with r^(2/3)
      parameter(slope=0.66666666666)			!Kolmogorov slope

! longitudinal structure function
      if(mode.eq.1) &
      sfngasp=s**slope+b1*s**2-c1*s**2*alog(s)

! transverse structure function
      if(mode.eq.2) &
      sfngasp=s**slope+b2*s**2-c2*s**2*alog(s)

      return
      end Function sfngasp

