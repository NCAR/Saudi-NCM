c
      subroutine arinam(map,lm)
      integer lm
      integer map(1)
      return
      end
c
      subroutine aredam(map,xlon,ylat,np,igroup,idleft,idright)
      integer np,igroup,idleft,idright 
      real    xlon(np),ylat(np)
      integer map(1)
      return
      end
c
      subroutine arscam(map,xwrk,ywrk,nwrk,iarea,igrp,ngroups,fill)
      integer nwrk,ngroups
      real    xwrk(nwrk),ywrk(nwrk)
      integer iarea(ngroups),igrp(ngroups)
      integer map(1)
      external fill
      return
      end
c
      subroutine cpclam(xy,rw,iw,map)
      real xy(1),rw(1)
      integer iw(1)
      integer map(1)
      return
      end
c
      subroutine cpcnrc(xy,n,nn,m,x,y,z,i,j,k)
      real xy(1),x,y,z
      integer n,nn,m,i,j,k
      return
      end
c
      subroutine cprect(xy,n,nn,m,rw,ll,iw,li)
      real xy(1),rw(ll)
      integer iw(li),n,nn,m
      return
      end
c
      subroutine cpsetr(cx,x)
      character*(*) cx
      real    x
      return
      end
c
      subroutine cpgeti(cx,i)
      character*(*) cx
      integer i
      return
      end
c
      subroutine cpsetc(cx,c)
      character*(*) cx,c
      return
      end
c
      subroutine cpseti(cx,i)
      character*(*) cx
      integer i
      return
      end
c
      subroutine CSIZE(a,b)
      real a,b
      return
      end             
c
      subroutine curved(x,y,n)
      real    x(n),y(n)
      integer n
      return
      end
c
      function cpux(i)
      integer i
      cpux = i
      return
      end
c
      function cpuy(j)
      integer j
      cpuy = j
      return
      end
c
      subroutine gaseti(c,j)
      character*3 c
      integer j
      return
      end
c
      subroutine gagetr(c,a)
      character*3 c
      real a
      return
      end
c
      subroutine gasetr(c,a)
      character*3 c
      real a
      return
      end
c
      subroutine GOPKS(i,j)
      integer i,j
      return
      end
c
      subroutine GOPWK(i,j,k)
      integer i,j,k
      return
      end
c
      subroutine GACWK(i)
      integer i
      return
      end
c
      subroutine gfa(n,x,y)
      integer n
      real    x(n),y(n)
      return
      end
c
      subroutine gridal(mjrx,mnrx,mjry,mnry,i,j,k,x,y)
      integer mjrx,mnrx,mjry,mnry,i,j,k
      real    x,y
      return
      end
c
      subroutine gslwsc(x)
      real    x
      return
      end
c
      subroutine GSCR(i,j,x,y,z)
      integer i,j
      real    x,y,z
      return
      end
c
      subroutine GSELNT(i)
      integer i
      return
      end
c
      subroutine gsfaci(i)
      integer i
      return
      end
c
      subroutine gsfais(i)
      integer i
      return
      end
c
      subroutine GSLN(i)
      integer i
      return
      end
c
      subroutine GSCLIP(i)
      integer i
      return
      end
c
      subroutine GSP(x,y,ipen)
      integer ipen
c     IF (ipen .EQ. 2) THEN
c        CALL VECTOR(x,y)
c     ELSE
c     print *,'in GSP calling FRSTPT: x,y,ipen=',x,y,ipen
c        CALL FRSTPT(x,y)
c     END IF
      return
      end             
c
      subroutine GSPL(I)
      integer I
      return
      end             
c
      subroutine GSPLCI(i)
      integer i
      return
      end
c
      subroutine GSPM(i,A,B,j)
      real A,B 
      integer I,j
      return
      end             
c
      subroutine GDAWK(i)
      integer i
      return
      end
c
      subroutine GCLWK(i)
      integer i
      return
      end
c
      subroutine GCLKS
      return
      end
c
      subroutine GPL(n,X,Y)
      integer n
      real    x(n),y(n)
      return
      end             
c
      subroutine GPM(I,X,Y)
      integer I
      real    X,Y
      return
      end             
c
      subroutine gsmksc(x)
      real    X
      return
      end
c
      subroutine gspmci(i)
      integer i
      return
      end
c
      subroutine gstxci(i)
      integer i
      return
      end             
c
      subroutine gstxfp(i,j)
      integer i,j
      return
      end
c
      subroutine GTX(x,y,ch)
      character*45 ch
      real cpux,cpuy
      integer is,io,ic
      is=2  ! character width in plotter coordinate system
      io=0  ! orientation in deg cc from horizontal
      ic=0  ! centering option (0=center of string)
      ipt1=int(192.8+819.2)
cnarg ux = cpux(512)
cnarg uy = cpuy(ipt1)
c     print *,'in GTX calling wtstr'
cnarg call wtstr(ux,uy,ch(1:45),is,io,ic)
      return
      end
c
      subroutine frame
      return
      end
c
      subroutine set(a,b,c,d,aa,bb,cc,dd,k)
      real    a,b,c,d,aa,bb,cc,dd
      integer k
      return
      end
c
      subroutine getset(a,b,c,d,aa,bb,cc,dd,LL)
      real    a,b,c,d,aa,bb,cc,dd
      integer LL
      aa=0.
      bb=1.
      cc=0.
      dd=1.
      return
      end
c
      subroutine setusv(c,k)
      character*2 c
      integer k
      return
      end
c
      subroutine vectd(x,y)
      real x,y
      return
      end
c
      subroutine frstd(x,y)
      real x,y
      return
      end
c
      subroutine LINE(a,b,c,d)
      real a,b,c,d
      return
      end
c
      subroutine labmod(cx,cy,i,j,k,l,m,n,nn)
      character*(*) cx,cy
      integer i,j,k,l,m,n,nn
      return
      end
c
      subroutine lblbar(ihov,xleb,xreb,ybeb,yteb,nbox,wsfb,hsfb,lfin,
     1  iftp,llbs,nlbs,lbab)
      integer ihov, nbox, lfin(*), nlbs, lbab
      real    xleb,xreb,ybeb,yteb,wsfb,hsfb
      character*(*) llbs(nlbs)
      return
      end
c
      subroutine pcseti(cx,i)
      character*(*) cx
      integer i
      return
      end
c
      subroutine perim(i,j,k,l)
      integer i,j,k,l
      return
      end
c
      subroutine periml(i,j,k,l)
      integer i,j,k,l
      return
      end
c
      subroutine points(px,py,NP,IC,IL)
      integer NP,IC,IL
      real px(NP),py(NP)
      return
      end
c
      subroutine dashdc(c,i,j)
      character*(*) c
      integer i,j
      return
      end
c
      subroutine ngwsym(c,i,x,y,z,j,k)
      character*(*) c
      integer i,j,k
      real x,y,z
      return
      end
c
      subroutine plchhq(X,Y,c,xx,yy,zz)
      character*(*) c
      real x,y,xx,yy,zz
      return
      end
c
      subroutine WTSTR(X,Y,c,i,j,k)
      character*(*) c
      integer i,j,k
      real x,y
      return
      end
c
      subroutine SFSETR(c,z)
      character*(*) c
      real z
      return
      end
c
      subroutine SFSGFA(x,y,i,z,k,l,m,n)
      integer i,j,k,l,m,n
      real z(m)
      real x,y
      return
      end
