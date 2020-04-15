  subroutine pseudo_snd(alat,alon,ix,jx,kx,xlat,xlon,x,x_ps,iflag)

  real, dimension(ix,jx) :: xlat,xlon
  real, dimension(ix,jx,kx) :: x
  real, dimension(kx) :: x_ps

  i0=0
  j0=0

  x_ps=-8888.

  iflag=0
  J_Loop: do j=1,jx-1-1
  I_Loop: do i=1,ix-1-1
             if(alat >= xlat(i,j) .and. alon >= xlon(i,j) .and. &
                alat <= xlat(i+1,j) .and. alon <= xlon(i,j+1)) then
               dx=(alon-xlon(i,j))/(xlon(i,j+1)-xlon(i,j))
               dy=(alat-xlat(i,j))/(xlat(i+1,j)-xlat(i,j))
               dxm=1.-dx
               dym=1.-dy
               iflag=1
               i0=i
               j0=j
               exit J_Loop
             endif
          enddo I_Loop
          enddo J_Loop

  if(iflag == 1) then
    do k=1,kx
       x_ps(k)=dxm*(dym*x(i,j,k)+dy*x(i+1,j,k))+ &
               dx*(dym*x(i,j+1,k)+dy*x(i+1,j+1,k))
    enddo
  endif

  return
  end subroutine pseudo_snd
