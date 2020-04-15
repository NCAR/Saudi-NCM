
      subroutine readmm5(nfile,mfile,iopen,iclose,irewind,isearch,
     :                   msearch,ni,nj,nk,buffer,iflag)
c
c-----Read processed data to obtain fields
c
c-----msearch%string: string (up to 10 chars) containing name of field to read
c
c-----for the following 0 = no and 1 = yes
c
c     iopen - open file
c     iclose - close file
c     irewind - rewind file
c     isearch - search (usually 1)
c
c
c     include 'parametr.inc'
      character*80 mbuf
      character*1 mblnk1(80)
      character*80 mblnk80
      character*80 findfld
      character*80 mfile
      character*10 msearch

      integer*2 issame80

c     type (file_name) mfile
c     type (field_name) msearch
      
      dimension buffer(ni,nj,nk)

      equivalence (mblnk80,mblnk1(1))

      data mblnk1 /80*' '/

c     dll_export readmm5

      findfld = mblnk80
      findfld = msearch

      if (iopen .eq. 1) then
c       open(nfile,file=mfile%strng,status='old',form='formatted',
c    :       err=100)
        ix=index(mfile,' ')
        if(ix .eq. 0) then
          open(nfile,file=mfile,status='old',form='formatted',
     +         err=100)
        else
          open(nfile,file=mfile(1:ix-1),status='old',form='formatted',
     +         err=100)
        endif
        go to 10
      end if

      if (irewind .eq. 1) rewind(nfile,err=200)

   10 if (isearch .eq. 0) go to 50

   20 mbuf = mblnk80
      read(nfile,'(a80)',err=300,end=400) mbuf
      if (issame80(findfld,mbuf,10) .eq. 1) go to 30
      go to 20

   30 continue

      read(nfile,*,err=300,end=400) k1,kmax
      if (nk .gt. kmax) go to 600
      if (k1 .ne. 1) go to 300

      do k=1,nk
        if (k .ne. 1) then
          read(nfile,*,err=300,end=400) k1,kmax
          if (k1 .ne. k) go to 300
        end if
c
c-----read in values and flip i and j indeces
c
        do i=1,ni
          read(nfile,*,err=300,end=400) (buffer(i,j,k),j=1,nj)
        end do
      end do

   50 if (iclose .eq. 1) then
        close(nfile,err=700)
        return
      end if


c
c-----file open error
c
  100 iflag = -1
      go to 700
c
c-----file rewind error
c
  200 iflag = -2
      close(nfile,err=700)
      go to 700
c
c-----file read error
c
  300 iflag = -3
      close(nfile,err=700)
      go to 700
c
c-----msearch (variable name) not found
c
  400 iflag = -4
      close(nfile,err=700)
      go to 700
c
c-----close error
c
  500 iflag = -5
      go to 700
c
c-----k dimension exceeds max
c
  600 iflag = -6
      close(nfile,err=700)

  700 continue
      return
      end

c==============================================================================

      integer*2 function issame80(string1,string2,maxch)
c
c-----compare string1 to string2. Null characters terminate strings
c-----and trailing blanks are ignored
c
c
c     issame = 0 - different
c            = 1 - same
c
      character*80 string1,string2

      issame80=0
      iend = min(maxch,80)

      do i=1,iend
        ich = i
        if (string1(i:i) .ne. string2(i:i)) go to 10
        if (ichar(string1(i:i)) .eq. 0.and.
     :      ichar(string2(i:i)) .eq. 0) then
          issame80 = 1
          return
        end if
      end do
      issame80=1
      return

   10 continue
      if (ichar(string1(ich:ich)) .eq. 0) then
        do i=ich,iend
          ic = ichar(string2(i:i))
          if (ic .ne. 0.and.ic .ne. 32) return
        end do
        issame80 = 1
        return
      end if

      if (ichar(string2(ich:ich)) .eq. 0) then
        do i=ich,iend
          ic = ichar(string1(i:i))
          if (ic .ne. 0.and.ic .ne. 32) return
        end do
        issame80 = 1
        return
      end if

      return

      end

c==============================================================================

      subroutine readcfg(nfile,mfile,nx,ny,nk,dx,dy,xlatc,xlonc,
     :                   iyear,imonth,iday,ihour,iminute,iflag)
c
c-----read ().cfg for some mm5/scipuff parameters. Arrays, which are
c-----also in ().cfg are read by readmm5
c
c-----note: in mm5, j corresponds to x (e-w) and i to y (n-s)
c-----In subroutine readmm5, however, these are switched to (i,j) <-> (x,y).
c
cr    include 'parametr.inc'
cr    type (file_name) mfile
      character*80 mfile

c     dll_export readcfg

      iflag = 0
cr    open(nfile,file=mfile%strng,status='old',form='formatted',
cr   :     err=100)
      open(nfile,file=mfile,status='old',form='formatted',err=100)
      read(nfile,*,err=200) nk,nx,ny
      read(nfile,*,err=200) dx,dy
      read(nfile,*,err=200) xlonc,xlatc
      read(nfile,*,err=200) iyear,imonth,iday,ihour,iminute
      close(nfile)
      return
  100 iflag=-1
      return
  200 iflag=-2
      close(nfile)
      return
      end

c==============================================================================

      subroutine newgrid(ni,nj,xlat,xlon,ylat,ylon,dlat,dlon)
c
c-----setup lat/lons for interpolating rectangular grid to lat/lon grid
c
      dimension xlat(ni,nj),xlon(ni,nj),ylat(nj),ylon(ni)
      dimension boundlat(2),boundlon(2)

c     dll_export newgrid
      
c
      call clipgrid(ni,nj,xlat,xlon,boundlat,boundlon)
c
      dlat = (boundlat(2) - boundlat(1)) / dfloat(nj-1)
      dlon = (boundlon(2) - boundlon(1)) / dfloat(ni-1)
c
      do 10 j=2,nj-1
      ylat(j) = boundlat(1) + dlat * dfloat(j-1)
   10 continue
      ylat(1) = boundlat(1)
      ylat(nj) = boundlat(2)
c
      do 20 i=2,ni-1
      ylon(i) = boundlon(1) + dlon * dfloat(i-1)
   20 continue
      ylon(1) = boundlon(1)
      ylon(ni) = boundlon(2)

c
      return
      end

c==============================================================================

      subroutine model2sph(ni,nj,nk,xlonc,xlon,u,v,utmp,vtmp,xn)
c
c-----Convert Lambert Conformal horizontal components to spherical
c-----Source: pseudo-code provided by NCAR
c
      dimension u(ni,nj,nk),v(ni,nj,nk),xlon(ni,nj)
c
      dimension utmp(ni,nj),vtmp(ni,nj)
c
      do 30 k = 1,nk
      do 10 j = 1,nj
      do 10 i = 1,ni
      arg = (xlonc-xlon(i,j)) * xn * 3.14159265 / 180.
      utmp(i,j) = u(i,j,k) * cos(arg) - v(i,j,k) * sin(arg)
      vtmp(i,j) = u(i,j,k) * sin(arg) + v(i,j,k) * cos(arg)
   10 continue
      do 20 j=1,nj
      do 20 i=1,ni
      u(i,j,k) = utmp(i,j)
      v(i,j,k) = vtmp(i,j)
   20 continue
   30 continue
      return
      end

c==============================================================================

      subroutine setztr(ni,nj,nk,zagl,terr,zadjust,ztrans)
c
c-----Compute vertical coordinate system for MEDOC file
c
      dimension zagl(ni,nj,nk),zadjust(ni,nj,nk),terr(ni,nj),ztrans(nk)
c

c     dll_export setztr

      do 10 k=1,nk
      do 10 j=1,nj
      do 10 i=1,ni
      zadjust(i,j,k) = zagl(i,j,k) + terr(i,j)
   10 continue
c
c-----find lowest elevation (MSL) of top fx level and use heights at this
c-----point for the transformed vertical coordinate.
c
      mini = 0
      minj = 0
      xmin=1.e20
      do 20 i=1,ni
      do 20 j=1,nj
      if (zadjust(i,j,nk) .lt. xmin) then
        mini = i
        minj = j
        xmin = zadjust(i,j,nk)
      end if
   20 continue

      const = zadjust(mini,minj,nk)/(zadjust(mini,minj,nk)-
     :                               terr(mini,minj))
      do 30 k = 1,nk
      ztrans(k) = const*(zadjust(mini,minj,k)-terr(mini,minj))
   30 continue
c
      return
      end

c==============================================================================

      subroutine zparams(ni,nj,nk,ztrans,zadjust,terr,slopef,kindex)
c
c-----compute vertical interpolation parameters slopef and kindex
c
      dimension zadjust(ni,nj,nk),terr(ni,nj),ztrans(nk)
      dimension slopef(ni,nj,nk),kindex(ni,nj,nk)
c

c     dll_export zparams
      
c
      depth = ztrans(nk)
c
      do 40 i=1,ni
      do 40 j=1,nj

      terloc = terr(i,j)

      do 30 k=1,nk
c
      ztarg = terloc + (ztrans(k)*(depth-terloc))/depth
      k1 = 0
      k2 = 0
c
      if (ztarg .lt. 0.) then
        ztarg = 0.
      end if
c
      if (ztarg .lt. zadjust(i,j,1)) then
        k1 = 1
        k2 = 2
        go to 20
      end if
c
      do 10 l=1,nk-1
      if(ztarg .ge. zadjust(i,j,l).and.ztarg .le. zadjust(i,j,l+1)) then
        k1 = l
        k2 = l + 1
        go to 20
      end if
   10 continue
c
      if (ztarg .gt. zadjust(i,j,nk)) then
        k1 = nk - 1
        k2 = nk
      end if
c
   20 continue
c
      slopef(i,j,k) =
     :     (ztarg-zadjust(i,j,k1))/(zadjust(i,j,k2)-zadjust(i,j,k1))
      kindex(i,j,k) = k1
c
   30 continue
   40 continue
c
      return
      end

c==============================================================================

      subroutine zinterp(ni,nj,nk,slopef,kindex,data,tmpdat)
c
c-----Interpolate input data to MEDOC vertical coordinates
c
      dimension tmpdat(nk),data(ni,nj,nk),slopef(ni,nj,nk),
     :          kindex(ni,nj,nk)

c     dll_export zinterp
      
      do 30 i=1,ni
      do 30 j=1,nj
c
      do 10 k=1,nk
      k1 = kindex(i,j,k)
      k2 = k1 + 1
      tmpdat(k) = data(i,j,k1) +
     :            slopef(i,j,k)*(data(i,j,k2)-data(i,j,k1))
   10 continue

      do 20 k=1,nk
      data(i,j,k) = tmpdat(k)
   20 continue

   30 continue
c
      return
      end

c==============================================================================

      subroutine wrtmedoc(ni,nj,nk,iyear,imon,iday,ihour,imin,isec,
     :                    ztrans,dlat,dlon,xlat0n,xlon0n,
     :                    terr,u,v,w,th,pblhgt,htflux,
     :                    nf,mfile,mode,ifxflag)
c
c-----Write Medoc file
c
c-----mode:    append existing file (0=no,1=yes) 
c-----ifxflag: write heatflux and pblhgt (0=no,1=yes)
c
cr    include 'parametr.inc'
c
      character*8 name3d,name2d,namdum
      character*80 mfile

cr    type (file_name) mfile
c
      dimension u(ni,nj,nk),v(ni,nj,nk),w(ni,nj,nk),th(ni,nj,nk),
     :          terr(ni,nj),pblhgt(ni,nj),htflux(ni,nj),ztrans(nk)
c
      dimension name3d(4),name2d(3)

      dimension idums(6)
c
c-----note "T" is potential temperature and "ZI" is pbl height
c
      data name3d/'U','V','W','T'/
      data name2d,namdum/'TOPO','ZI','HFLX','NMDUMDUM'/
      data idums /0,0,0,0,0,0/

c     dll_export wrtmedoc
      
      index_blank=index(mfile," ")

      if (mode .eq. 0) then
cr      open(nf,file=mfile%strng,status='unknown',form='formatted')
        open(nf,file=mfile(1:index_blank-1),status='unknown',
     +       form='formatted')
      else
cr      open(nf,file=mfile%strng,access='append',status='unknown',
cr   :       form='formatted' )
        open(nf,file=mfile(1:index_blank-1),access='append',
     +       status='unknown',form='formatted' )
      end if
c
c-----set for spherical (SCIPUFF recognizes these values as a
c-----flag for spherical)
c
      x0n = -999999.
      y0n = -999999.

      numi = ni
      numj = nj

      num2d = 3
      if (ifxflag .eq. 0) num2d = 1
c
      write(nf,9001)'FFFFFFFF'
      write(nf,9001)'        '
      write(nf,9002) iday,imon,iyear,ihour,imin,isec
      write(nf,9002) iday,imon,iyear,ihour,imin,isec
      write(nf,9002) numi,numj,nk,0,4,num2d
      write(nf,9002) (idums(i),i=1,6)
      write(nf,9002) (idums(i),i=1,6)
      write(nf,9003) (ztrans(n),n=1,nk),dlon,dlat,x0n,y0n,xlat0n,xlon0n,
     :              0.,0.,0.,0.,ztrans(nk)
      write(nf,9001) (name3d(n),n=1,4),(namdum,n=1,4),
     :               (name2d(n),n=1,num2d),(namdum,n=1,num2d)
      write(nf,9003)(((u(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      write(nf,9003)(((v(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      write(nf,9003)(((w(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      write(nf,9003)(((th(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      write(nf,9003) ((terr(i,j),i=1,ni),j=1,nj)
      if (num2d .eq. 3) then
        write(nf,9003) ((pblhgt(i,j),i=1,ni),j=1,nj)
        write(nf,9003) ((htflux(i,j),i=1,ni),j=1,nj)
      endif

      close(nf)
c
      return
c
 9001 format(6(a8,1x))
 9002 format(6(i12,1x))
 9003 format(6(f12.4,1x))
c
      end

c==============================================================================

      subroutine hparams(ni,nj,xlatc,xlonc,xlat0,xlon0,ylat,ylon,dx,dy,
     :                  iindex,jindex,hfact,xn)
c            
c-----compute parameters for interpolating Lamabert Conformal grid
c-----to lat/lon grid
c
      dimension ylat(nj),ylon(ni)
      dimension iindex(ni,nj),jindex(ni,nj),hfact(4,ni,nj)
c
c     dll_export hparams
c
c-----compute southwest corner x and y of Lambert Conformal Grid
c
c     call getxy(xlatc,xlonc,xlat0,xlon0,swx,swy)
c
c     The projection is polar stereographic, use getxyps
      if(xn .eq. 1.) then
        call getxyps(xlatc,xlonc,xlat0,xlon0,swx,swy)
      else
        call getxy(xlatc,xlonc,xlat0,xlon0,swx,swy)
      endif
c
      do 10 i=1,ni
      do 10 j=1,nj                                           
c
c-----get x/y location for output lon/lat
c      
      if(xn .eq. 1.) then
        call getxyps(xlatc,xlonc,ylat(j),ylon(i),xtarget,ytarget)
      else
        call getxy(xlatc,xlonc,ylat(j),ylon(i),xtarget,ytarget)
      endif
c
c-----find cell in original x/y grid
c     
      xcell = (xtarget - swx) / dx
      ycell = (ytarget - swy) / dy
      itarget = int(xcell) + 1
      jtarget = int(ycell) + 1
c
c-----convert x and y to a unit square (0 -> 1)
c       
      x = xtarget - swx - (itarget - 1) * dx
      x = x / dx
      y = ytarget - swy - (jtarget - 1) * dy
      y = y / dy                                                    
c
c
c-----adjust for precision
c      
      x = min(x,1.0)
      x = max(x,0.0)
      y = min(y,1.0)
      y = max(y,0.0)
c      
      iindex(i,j) = itarget
      jindex(i,j) = jtarget
c
c-----interpolation factors
c
c     1 - lower left
c     2 - upper left
c     3 - lower right
c     4 - upper right

      hfact(1,i,j) = (1.-x) * (1.-y)
      hfact(2,i,j) = (1.-x) * y
      hfact(3,i,j) = x * (1.-y)
      hfact(4,i,j) = x * y

   10 continue
c
      return
      end

c==============================================================================

      subroutine clipgrid(ni,nj,xlat,xlon,xlatclip,xlonclip)
c
c-----compute lat/lon limits in lambert conformal grid that could fully contain a
c-----spherical grid for interpolation without any extrapolation
c    
      dimension xlat(ni,nj),xlon(ni,nj)
      dimension xlatclip(2),xlonclip(2)
c                
      xlatclip(1) = xlat(1,1)
      xlatclip(2) = xlat(1,nj)

      do 10 i=1,ni
      xlatclip(1) = max(xlatclip(1),xlat(i,1))
      xlatclip(2) = min(xlatclip(2),xlat(i,nj))
   10 continue
c   
      xlonclip(1) = xlon(1,1)
      xlonclip(2) = xlon(ni,1)

      do 20 j=1,nj
      xlonclip(1) = max(xlonclip(1),xlon(1,j))
      xlonclip(2) = min(xlonclip(2),xlon(ni,j))
   20 continue
c      
      return
      end


c==============================================================================

      subroutine getxy(xlatc,xlonc,xlat,xlon,x,y)
c
c-----compute x,y displacement (KM) of xlat and xlon from the
c-----central lat/lon (xlatc,xlonc) of a Lambert Conformal projection
c-----True latitudes of 30 and 60 degrees are used
c-----Source: Appendix 3 in - A description of ... (MM5) NCAR/TN-398+STR
c-----(June '94)
c
      double precision cnv,xn,a,psi,psi0,psi1,xlamp,r,c2,temp

      cnv = 3.1415927d0/180.d0
      xn = .716d0
      a = 6370.d0

      psi = 90.d0 - xlat
      psi0 = 90.d0 - xlatc
      psi1 = 30.d0
      xlamp = xn*(xlon - xlonc -90.d0/xn)

      psi = psi*cnv
      psi0 = psi0*cnv
      psi1 = psi1*cnv
      xlamp = xlamp*cnv

      temp = dtan(psi/2.d0)/dtan(psi1/2.d0)
      r = (a/xn)*dsin(psi1)*(temp**xn)

      temp = dtan(psi0/2.d0)/dtan(psi1/2.d0)
      c2 = (a/xn)*dsin(psi1)*(temp**xn)

      x = r*dcos(xlamp)
      y = r*dsin(xlamp) + c2

      return
      end

c==============================================================================

      subroutine getfxtim(nfile,mfile,ihour,imin,isec,iflag )
c
c-----read forecast time
c
      character*80 mfile
cr    include 'parametr.inc'

cr    type (file_name) mfile

c     dll_export getfxtim
      
      iflag = 0

cr    open(nfile,file=mfile%strng,status='old',form='formatted',err=100)
      ix=index(mfile,' ')
      open(nfile,file=mfile(1:ix-1),status='old',form='formatted',
     +     err=100)
      read(nfile,*,err=200)
      read(nfile,*,err=200)
      read(nfile,*,err=200) ihour,imin,isec
      go to 300

  100 iflag = -1
      return
  200 iflag = -2
  300 close(nfile,err=400)
  400 return
      end

c==============================================================================

      subroutine hinterp2(ni,nj,hfact,iindex,jindex,data,buffer)
c            
c-----interpolate rectangular grid to lat/lon grid for 2-d field
c
      dimension data(ni,nj),buffer(ni,nj)
      dimension hfact(4,ni,nj),iindex(ni,nj),jindex(ni,nj)
c
c     dll_export hinterp2
c
      do 10 i=1,ni
      do 10 j=1,nj
      i1 = iindex(i,j)
      i2 = i1 + 1
      j1 = jindex(i,j)
      j2 = j1 + 1
      buffer(i,j) = data(i1,j1) * hfact(1,i,j) +
     :              data(i1,j2) * hfact(2,i,j) +
     :              data(i2,j1) * hfact(3,i,j) +
     :              data(i2,j2) * hfact(4,i,j)
   10 continue

      do 20 i=1,ni
      do 20 j=1,nj
      data(i,j) = buffer(i,j)
   20 continue
c
      return
      end

c==============================================================================

      subroutine hinterp3(ni,nj,nk,hfact,iindex,jindex,data,buffer)
c            
c-----interpolate rectangular grid to lat/lon grid for 3-d field
c
      dimension data(ni,nj,nk),buffer(ni,nj)
      dimension hfact(4,ni,nj),iindex(ni,nj),jindex(ni,nj)
c
c     dll_export hinterp3

      do 30 k=1,nk
      do 10 i=1,ni
      do 10 j=1,nj
      i1 = iindex(i,j)
      i2 = i1 + 1
      j1 = jindex(i,j)
      j2 = j1 + 1
      buffer(i,j) = data(i1,j1,k) * hfact(1,i,j) +
     :              data(i1,j2,k) * hfact(2,i,j) +
     :              data(i2,j1,k) * hfact(3,i,j) +
     :              data(i2,j2,k) * hfact(4,i,j)
   10 continue

      do 20 i=1,ni
      do 20 j=1,nj
      data(i,j,k) = buffer(i,j)
   20 continue
   30 continue
c
      return
      end
c
c
c
      subroutine getxyps(xlatc,xlonc,xlat,xlon,x,y)
c
c-----compute x,y displacement (KM) of xlat and xlon from the
c-----central lat/lon (xlatc,xlonc) of a polar stereographic projection
c-----True latitude at 60 is used
c-----Source: Appendix 3 in - A description of ... (MM5) NCAR/TN-398+STR
c-----(June '94)
c
      double precision cnv,a,phi1,psi0,psi1,xlamp,r,c2,m
 
      cnv = 3.1415927d0/180.d0
      a = 6370.d0
      xlamp = xlon - xlonc -90.d0
 
      phi1 = 60.d0                !! true at this latitude
      psi0 = 90.d0 - xlatc        !! co-latitude of center latitude
      psi1 = 90.d0 - phi1         !! Northern Hemisphere
 
      phi1 = phi1*cnv
      psi0 = psi0*cnv
      psi1 = psi1*cnv
      xlamp = xlamp*cnv

      m = (1.d0+dsin(phi1))/(1.d0+dsin(xlat*cnv))
 
      r = a*m*dcos(xlat*cnv)
 
      c2 = a*dsin(psi1)*dtan(psi0*0.5d0)/dtan(psi1*0.5d0)
 
      x = r*dcos(xlamp)
      y = r*dsin(xlamp) + c2
 
      return
      end
