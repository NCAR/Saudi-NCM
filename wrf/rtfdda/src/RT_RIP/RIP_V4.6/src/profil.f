c                                                                     c
c*********************************************************************c
c                                                                     c
      subroutine profil (wk,maxpl,ipl,pmax,pmin,vc3d,vcor,rslcg,
     &   set1,set2,unwk,csout,icomg,ilinw,miy,mjx,mkzh)
c
      dimension wk(miy,mjx,mkzh),vc3d(miy,mjx,mkzh),rslcg(2,maxpl),
     &   th(mkzh),p2(mkzh),icomg(maxpl),ilinw(maxpl)
      character title*(80),labl*(80),csout(maxpl)*58,unwk(maxpl)*24,
     &   axtit*(80),vcor*1
c
      include 'comconst'
c
      sxgn=1.+(rslcg(2,ipl)-xjcorn)*refrat
      sygn=1.+(rslcg(1,ipl)-yicorn)*refrat
      i = nint(sygn)
      j = nint(sxgn)
      write(csout(ipl)(5:10),'(f6.2)') sxgn
      write(csout(ipl)(12:17),'(f6.2)') sygn
      write(labl,987) csout(ipl)
  987 format ('Profile at ',a58)
      title = '   '
c
c   If pmax and pmin haven't been set yet, do it now
c
      pmaxch=-99999.
      pminch= 99999.
      do  k = mkzh, 1, -1
         pmaxch = amax1(wk(i,j,k),pmaxch)
         pminch = amin1(wk(i,j,k),pminch)
      enddo
      if (pmax .eq. -99999.) pmax=pmaxch
      if (pmin .eq. 99999.) pmin=pminch
      k1 = 1
      do k = mkzh, 1, -1
         if (vcor.eq.'z'.or.vcor.eq.'f') then
            p2(k1) = -.001*sclht*alog(vc3d(i,j,k))
         else if (vcor.eq.'l') then
            p2(k1) = alog(vc3d(i,j,k))
         else if (vcor.eq.'x') then
            p2(k1) = (vc3d(i,j,k))**gamma
         else
            p2(k1) = vc3d(i,j,k)
         endif
         th(k1) = wk(i,j,k)
         k1 = k1 + 1
      enddo
      ymax = amax1(set1,set2)
      ymin = amin1(set1,set2)
c
c   Write vertical axis title
c
      if (vcor.eq.'s') then ! these are in vertical level index
         axtit='Vertical Level Index$                          '
      elseif (vcor.eq.'p') then ! these are in hPa
         axtit='Pressure (hPa)$                  '
      elseif (vcor.eq.'l') then ! b
         axtit='Pressure (hPa), log scale$       '
      elseif (vcor.eq.'x') then ! these are in hPa
         axtit='Pressure (hPa), Exner scale$     '
      elseif (vcor.eq.'z') then ! these are in km
         axtit='Height (km)$                    '
      elseif (vcor.eq.'f') then ! these are in km
         axtit='Ht AFL (km)$                    '
      elseif (vcor.eq.'t') then ! these are in K
         axtit='Theta (K)$                      '
      elseif (vcor.eq.'m') then ! these are in deg. C
         axtit='Temperature (deg C)$            '
      elseif (vcor.eq.'e') then ! these are in K
         axtit='Theta_e (K)$                    '
      elseif (vcor.eq.'q') then ! these are in PVU
         axtit='PV (PVU)$                       '
      endif
      k1 = k1 - 1
      call agsetf ('GRAPH/LEFT.',flmin)
      call agsetf ('GRAPH/RIGHT.',frmax)
      call agsetf ('GRAPH/BOTTOM.',fbmin)
      call agsetf ('GRAPH/TOP.',ftmax)
      call agsetf ('X/MINIMUM.',pmin)
      call agsetf ('X/MAXIMUM.',pmax)
      call agsetf ('Y/MINIMUM.',ymin)
      call agsetf ('Y/MAXIMUM.',ymax)
      if (vcor .eq. 'p' .or. vcor .eq. 'x' .or. vcor .eq. 'l' .or. 
     &    vcor .eq. 's' .or. vcor .eq. 'm') then
         call agsetf ('Y/ORDER.',1.)
      else
         call agsetf ('Y/ORDER.',0.)
      endif
      call agsetc ('LABEL/NAME.','L')
      call agseti ('LINE/NUMBER.',100)
      call agsetf ('LINE/CHARACTER.',.045)
      call agsetc ('LINE/TEXT.',axtit)
      call agsetc ('LABEL/NAME.','B')
      call agseti ('LINE/NUMBER.',-100)
      call agsetf ('LABEL/BASEPOINT/X.',.5)
      call agsetf ('LINE/CHARACTER.',.05)
      call agseti ('LINE/MAXIMUM.',70)
      call agsetc ('LINE/TEXT.',labl)
      call agseti ('FRAME.',2)
      call agsetf ('AXIS/BOTTOM/NUMERIC/WIDTH.',.035)
      call agsetf ('AXIS/LEFT/NUMERIC/WIDTH.',.03)
      call agezsu (2,th,p2,k1,1,k1,' ',iivx,iiex,iivy,iiey)
      call agback
      call sflush                  ! set the color
      call gqplci (ierr,ml)
      call gqtxci (ierr,nl)
      call gsplci(icomg(ipl))
      call gstxci(icomg(ipl))
      width=ilinw(ipl)            ! set line width
      call gqlwsc (ierr, oldw)
      call gslwsc(width)
      call agcurv (th,1,p2,1,k1,1)
      call sflush
      call gslwsc(oldw)
      call gsplci(ml)
      call gstxci(nl)
c
c   Draw a 0 line on the plot
c
      do k = 1, k1
         th(k) = 0.
      enddo
      call agezsu (2,th,p2,k1,1,k1,title,iivx,iiex,iivy,iiey)
      call agback
      call agcurv (th, 1, p2, 1, k1, 1)
      call sflush
c
c   Write units at end of horizontal axis, roughly lined up
c      with axis labels.
c
      call set(0.,1.,0.,1.,0.,1.,0.,1.,1)
      chsize=.01
      call plchhq (frmax-.005,fbmin+.096,unwk(ipl),chsize,0.,-1.)
c
      return
      end
