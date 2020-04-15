      subroutine da_lmdir (stp,iter,ipoint,iflag, &
                           ndims, gr, gro, msave, s1, y, w) 

      real, intent(in)       :: stp
      integer, intent(in)    :: iter
      integer, intent(inout) :: ipoint
      integer, intent(inout) :: iflag
      integer, intent(in)    :: ndims
      real, intent(in)       :: gr(:)
      real, intent(in)       :: gro(:)
      integer, intent(in)    :: msave
      real, intent(inout)    :: s1(msave*ndims)
      real, intent(inout)    :: y(msave*ndims)
      real, intent(inout)    :: w(ndims+2*msave)

      integer i, n, m1, j, k, ii, npt
      real stpmin,stpmax,f,ys,diag,beta,yy1
! 
      integer bound,cp

      n = ndims
      m1 = msave
      bound = iter-1
!
      if (iter > m1) bound=m1
      if (iter == 1) return
!
!     ------------------------------------------------------------
!     compute the new s1 and y
!     ------------------------------------------------------------
! 
      npt = ipoint*n
!
      do i = 1, n
        s1(npt+i) = stp*s1(npt+i)
        y(npt+i) = gr(i)-gro(i)
      end do
!
      ys = da_dot(y(npt+1),s1(npt+1),n )
      yy1 = da_dot(y(npt+1),y(npt+1),n )

      ipoint = ipoint+1
      if (ipoint == m1) ipoint=0
! 
! 
!     ------------------------------------------------------------
!     compute -hg and the diagonal scaling matrix in diag
!     ------------------------------------------------------------
! 
      diag = ys/yy1
      if (diag < 0.0) go to 95
      do i = 1, n
        w(i)= -gr(i)
      end do
! 
      cp = ipoint
      if (ipoint == 0) cp = m1
!
      w(n+cp) = 1.0/ys
!
      cp= ipoint
!
      do 25 ii = 1, bound
         cp=cp-1
         if (cp == -1)cp=m1-1
         w(n+m1+cp+1)= w(n+cp+1) * da_dot(s1(cp*n+1), w, n)
         do 20 k = 1, n
  20     w(k)= w(k)-w(n+m1+cp+1)*y(cp*n+k)
  25  continue
! 
      do 30 i = 1, n
  30  w(i)=diag*w(i)
!
      do 45 ii=1,bound
         beta= w(n+cp+1) * da_dot(y(cp*n+1), w, n)
         do 40 k=1,n
  40     w(k)= w(k)+s1(cp*n+k)*(w(n+m1+cp+1)-beta)
         cp=cp+1
         if (cp == m1)cp=0
  45  continue
! 
!     ------------------------------------------------------------
!     store the new direction in s1
!     ------------------------------------------------------------
! 
      do 70 i=1,n
        s1(ipoint*n+i)= w(i)
        w(i)=gr(i)
70    continue
! 
      return
95    iflag= -7
      if (lp > 0) write(lp,135) iflag
      return
! 
 135  format(/' iflag= ',i2,/' the diagonal element of the', &
             ' inverse hessian approximation is not positive')
  
      end subroutine da_lmdir
