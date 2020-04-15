  subroutine wrf_pseudo_2d(it,x,y,array,x_point)

  integer :: it
  real :: x,y
  real, pointer, dimension(:,:,:) :: array
  real :: dx,dy,dxm,dym,x_point
  integer :: i,j

  i=int(x)
  j=int(y)

  dx=x-i
  dy=y-j
  dxm=1.-dx
  dym=1.-dy

  x_point=dxm*(dym*array(i,j,it)+dy*array(i,j+1,it))+ &
          dx*(dym*array(i+1,j,it)+dy*array(i+1,j+1,it))

  return

  end subroutine wrf_pseudo_2d
