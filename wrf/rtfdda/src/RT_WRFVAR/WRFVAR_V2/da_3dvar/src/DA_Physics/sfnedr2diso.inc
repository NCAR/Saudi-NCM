      subroutine sfnedr2d(u,v,nxmax,nymax,nzmax,icen,jcen,kcen, &
      idel,kdel,k1,nlags,edr)
!
! calculate coefficient of velocity representative error epsilon^(1/3)
! for coordinate (icen,jcen,kcen) based on best-fit structure function
! assuming 2D isotropic Lindborg model stored in common block /sfnmodvel/
! see sfnedr2dinit.inc.f90
!
! u(nxmax,nymax,nzmax) - u velocity component array of dimension nxmax,nymax,nzmax
! v(nxmax,nymax,nzmax) - v velocity component array of dimension nxmax,nymax,nzmax
! icen                 - x index of calculation
! jcen                 - y index of calculation
! kcen                 - z index of calculation (pressure level)
! idel                 - x domain of calculation is (icen-idel,icen+idel)
!                      - y domain of calculation is (jcen-idel,jcen+idel)
! kdel                 - z domain of calculation is (kcen-kdel,kcen+kdel)
! k1                   - first structure function index of best-fit (1)
! nlags                - maximum number of lags calculated for best-fit
! d                    - calculated structure function
! sfnmodvel            - common block for best-fit normalized structure functions
!                        calculated by sfnedr2dinit
! edr                  - epsilon^(1/3) from all four structure functions

      integer   nxmax,nymax,nzmax,istart,iend,jstart,jend,kstart,kend
      integer   icen,jcen,kcen,idel,kdel,nlags,maxlag,np
      integer   imv, i, j, k, k1
      real      cv2, cv2xx, cv2yy, cv2xy, cv2yx, s
      real      edr
      parameter(maxlag=10,np=36)
      real   u(nxmax,nymax,nzmax),v(nxmax,nymax,nzmax)
      real   d(maxlag)
      real dmodxx,dmodyy,dmodxy,dmodyx
      common /sfnmodvel/ dmodxx(maxlag,np), dmodyy(maxlag,np), &
      dmodxy(maxlag,np), dmodyx(maxlag,np)

      logical :: ldebug = .FALSE.

! determine calculation sub-domain

! istart               - start index in x
! iend                 - end index in x
! jstart               - start index in y
! jend                 - end index in y
! kstart               - start index in z
! kend                 - end index in z
      istart=icen-idel
      iend=icen+idel
      jstart=jcen-idel
      jend=jcen+idel
      kstart=kcen-kdel
      kend=kcen+kdel
!     k1=1
!     nlags=2*idel

! Longitudinal structure function in x direction (EW)
      call sfnxdir(u,nxmax,nymax,nzmax,istart,iend,jstart,jend, &
      kstart,kend,d)
! calculate structure function level cv2 with best-fit to universal
! structure function
      call sfnwgt(d,dmodxx(1,kcen),k1,nlags,cv2xx)

      if (ldebug) THEN
      write(log_unit,*)' duLL',(d(i),i=1,nlags)
      endif

! Longitudinal structure function in y direction (NS)
      call sfnydir(v,nxmax,nymax,nzmax,istart,iend,jstart,jend, &
      kstart,kend,d)
! calculate structure function level cv2 with best-fit to universal
! structure function
      call sfnwgt(d,dmodyy(1,kcen),k1,nlags,cv2yy)

      if (ldebug) THEN
      write(log_unit,*) 'dvLL',(d(i),i=1,nlags)
      ENDIF

! Transverse structure function in x direction (EW)
      call sfnxdir(v,nxmax,nymax,nzmax,istart,iend,jstart,jend, &
      kstart,kend,d)
! calculate structure function level cv2 with best-fit to universal
! structure function
      call sfnwgt(d,dmodyx(1,kcen),k1,nlags,cv2yx)

      if (ldebug) THEN
      write(log_unit,*)' duTT',(d(i),i=1,nlags)
      endif

! Transverse structure function in y direction (NS)
      call sfnydir(u,nxmax,nymax,nzmax,istart,iend,jstart,jend, &
      kstart,kend,d)
! calculate structure function level cv2 with best-fit to universal
! structure function
      call sfnwgt(d,dmodxy(1,kcen),k1,nlags,cv2xy)

      if (ldebug) THEN
      write(log_unit,*) 'duTT',(d(i),i=1,nlags)
      ENDIF

      cv2=(cv2xx+cv2yy+cv2xy+cv2yx)/4.0
! calculate epsilon^(1/3) from average structure function level cv2
      edr=sqrt(abs(cv2))

      return
      end subroutine sfnedr2d

      subroutine sfnct2d(T,nxmax,nymax,nzmax,icen,jcen,kcen, &
      idel,kdel,k1,nlags,ct)
!
! calculate coefficient of temperature representative error CT
! for coordinate (icen,jcen,kcen) based on best-fit structure function
! assuming 2D isotropic scalar Lindborg model stored in common block sfnmodT
!
! u(nxmax,nymax,nzmax) - u velocity component array of dimension nxmax,nymax,nzmax
! v(nxmax,nymax,nzmax) - v velocity component array of dimension nxmax,nymax,nzmax
! icen                 - x index of calculation
! jcen                 - y index of calculation
! kcen                 - z index of calculation (pressure level)
! idel                 - x domain of calculation is (icen-idel,icen+idel)
!                      - y domain of calculation is (jcen-idel,jcen+idel)
! kdel                 - z domain of calculation is (kcen-kdel,kcen+kdel)
! k1                   - first structure function index of best-fit (1)
! nlags                - maximum number of lags calculated for best-fit
! d                    - calculated structure function
! sfnmodT              - best-fit normalized structure function models
!                        calculated by sfnedr2dinit
! ct                   - temperature structure constant = ct**2

      integer   icen,jcen,kcen,idel,kdel,nlags,maxlag,np
      integer   nxmax,nymax,nzmax,istart,iend,jstart,jend,kstart,kend
      integer   imv, i, j, k, k1
      parameter(maxlag=10,np=36)
      real      cv2, s
      real   T(nxmax,nymax,nzmax)
      real   d(maxlag)
      real   ct2,ct
      real   dmodTx
      common /sfnmodT/ dmodTx(maxlag,np)

      logical :: ldebug = .FALSE.

! determine calculation sub-domain

! istart               - start index in x
! iend                 - end index in x
! jstart               - start index in y
! jend                 - end index in y
! kstart               - start index in z
! kend                 - end index in z
      istart=icen-idel
      iend=icen+idel
      jstart=jcen-idel
      jend=jcen+idel
      kstart=kcen-kdel
      kend=kcen+kdel
!     k1=1
!     nlags=2*idel

! calculate structure function in x direction
      call sfnxdir(T,nxmax,nymax,nzmax,istart,iend,jstart,jend, &
      kstart,kend,d)
      if (ldebug) THEN
      write(log_unit,*)' d ',(d(i),i=1,nlags)
      endif

! calculate structure constant ct2 with best-fit to universal
! structure function
      call sfnwgt(d,dmodTx(1,kcen),k1,nlags,ct2)

! calculate ct from structure constant ct2
      ct=sqrt(abs(ct2))

! calculate best-fit structure function
!     do k=1,nlags
!     dmod(k)=dmod(k)*ct2
!     enddo

      return
      end subroutine sfnct2d

      subroutine sfnwgt(sfn,sfnmod,k1,nlags,cv2)
! calculate best-fit structure function level using
! minimum weighted chi-square error
! sfn                  - calculated structure function
! sfnmod               - best-fit structure function model
! k1                   - first index of fit
! nlags                - last index of fit
! cv2                  - velocity structure constant

      real   sfn(*),sfnmod(*),cv2
      real   sumn,sumd
      integer   k,k1,nlags

! calculate minimum chi-square
      sumn=0.0
      sumd=0.0
      do k=k1,nlags
      sumn=sumn+(sfn(k)/sfnmod(k))**2/float(k)
      sumd=sumd+(sfn(k)/sfnmod(k))/float(k)
      enddo

      cv2=sumn/sumd

      return
      end subroutine sfnwgt

      subroutine sfnxdir(q,nxmax,nymax,nzmax,istart,iend,jstart,jend, &
      kstart,kend,d)
! calculate structure function in the x-direction
! q(nxmax,nymax,nzmax) - array of dimensions nxmax,nymax,nzmax
! istart               - start index in x 
! iend                 - end index in x 
! jstart               - start index in y 
! jend                 - end index in y 
! kstart               - start index in z 
! kend                 - end index in z 
! d                    - output structure function of dimension iend-istart

      integer   nxmax,nymax,nzmax,istart,iend,jstart,jend,kstart,kend
      real   q(nxmax,nymax,nzmax), d(nxmax)
      real   di(1000), sum
      integer   ipts,jpts,kpts,nlags
      integer   i, imv, j, k, k1

      logical :: ldebug = .TRUE.

      ipts = iend - istart + 1		! number of points in x
      jpts = jend - jstart + 1		! number of points in y
      kpts = kend - kstart + 1		! number of points in z
      nlags = iend-istart		! number of lags

! zero summing array for structure function
      do i=1,nlags
      Di(i)= 0.0
      enddo


!-- loop over k: z direction
      do 300 k=kstart,kend

!-- loop over j: y direction
      do 200 j=jstart,jend

!-- loop over i: x direction

      do 100 i = 1, nlags
        sum = 0.0
        do imv = istart, iend-i
        sum = sum + (q(imv,j,k)-q(imv+i,j,k))**2
        enddo

       Di(i) = Di(i) + sum/float(ipts-i)

 100  continue		!end loop over i:

 200  continue		!end loop over j:

 300  continue		!end loop over k:

! normalize structure functions

      do i=1,nlags
      D(i)= Di(i)/float(jpts*kpts)
      enddo

      return
      end subroutine sfnxdir

      subroutine sfnydir(q,nxmax,nymax,nzmax,istart,iend,jstart,jend, &
      kstart,kend,d)
! calculate structure function in the y-direction
! q(nxmax,nymax,nzmax) - array of dimensions nxmax,nymax,nzmax
! istart               - start index in x 
! iend                 - end index in x 
! jstart               - start index in y 
! jend                 - end index in y 
! kstart               - start index in z 
! kend                 - end index in z 
! d                    - output structure function of dimension jend-jstart

      integer   nxmax,nymax,nzmax,istart,iend,jstart,jend,kstart,kend
      real   q(nxmax,nymax,nzmax), d(nxmax)
      real   di(1000), sum
      integer   ipts,jpts,kpts,nlags
      integer   i, imv, j, k, k1

      ipts = iend - istart + 1		! number of points in x
      jpts = jend - jstart + 1		! number of points in y
      kpts = kend - kstart + 1		! number of points in z
      nlags = jend-jstart		! number of lags

! zero summing array for structure function
      do i=1,nlags
      Di(i)= 0.0
      enddo

!-- loop over k: z direction
      do 300 k=kstart,kend

!-- loop over i: x direction
      do 200 i=istart,iend

!-- loop over j: y direction

      do 100 j = 1, nlags
        sum = 0.0
        do imv = jstart, jend-j
        sum = sum + (q(i,imv,k)-q(i,imv+j,k))**2
        enddo

       Di(j) = Di(j) + sum/float(jpts-j)

 100  continue		!end loop over j:

 200  continue		!end loop over i:

 300  continue		!end loop over k:

! normalize structure functions

      do i=1,nlags
      D(i)= Di(i)/float(ipts*kpts)
      enddo

      return
      end subroutine sfnydir
