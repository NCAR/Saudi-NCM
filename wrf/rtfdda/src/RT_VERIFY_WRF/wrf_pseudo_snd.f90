  subroutine wrf_pseudo_snd(kx,it,x,y,array,x_ps)

  integer :: kx,it,k
  real :: x,y
  real, pointer, dimension(:,:,:,:) :: array
  real, allocatable, dimension(:) :: x_ps
  real :: dx,dy,dxm,dym,elevm,dalt
  integer :: i,j
  real :: rmissing = -8888.

  i=int(x)
  j=int(y)

  dx=x-i
  dy=y-j
  dxm=1.-dx
  dym=1.-dy

  do k=1,kx
     x_ps(k)=dxm*(dym*array(i,j,k,it)+dy*array(i,j+1,k,it))+ &
             dx*(dym*array(i+1,j,k,it)+dy*array(i+1,j+1,k,it))

  enddo

  return
  end subroutine wrf_pseudo_snd
