SUBROUTINE da_transform_xtoy_buoy_adj( xb, iv, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)    :: xb          ! first guess state.
   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(INOUT) :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j, kz ! Index dimension.
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !

   IF ( iv%num_buoy > 0 ) THEN

      if (sfc_assi_options == 1) then
       do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, iv%ob_numb(iv%current_ob_time)%buoy
      
       if(iv%buoy(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%buoy(n)%loc%i
         dy = iv%buoy(n)%loc%dy
         dym = iv%buoy(n)%loc%dym
         j = iv%buoy(n)%loc%j
         dx = iv%buoy(n)%loc%dx
         dxm = iv%buoy(n)%loc%dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D_adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%buoy(n)%u, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%buoy(n)%v, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % t, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%buoy(n)%t, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % q, xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%buoy(n)%q, iv%buoy(n)%zk)

         call Interp_lin_2D_adj(jo_grad_x % psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                                   i, j, dx, dy, dxm, dym, jo_grad_y%buoy(n)%p)
#else
         call Interp_Obs_lin_2D_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%buoy(n)%u, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%buoy(n)%v, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%buoy(n)%t, iv%buoy(n)%zk)
         call Interp_Obs_lin_2D_adj(jo_grad_x % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                    jo_grad_y%buoy(n)%q, iv%buoy(n)%zk)

         call Interp_lin_2D_adj(jo_grad_x % psfc(xp%ims,xp%jms), &
                                xp%ims, xp%ime, xp%jms, xp%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%buoy(n)%p)
#endif
       endif

       END DO
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%buoy + 1, iv%ob_numb(iv%current_ob_time)%buoy
            if(iv%buoy(n)%loc%proc_domain_with_halo) then
               CALL DA_Transform_XToPsfc_Adj(xb,xp,iv%buoy(n), &
                                             jo_grad_y%buoy(n),jo_grad_x)
            endif
         ENDDO
      endif
   
   END IF

END SUBROUTINE da_transform_xtoy_buoy_adj

