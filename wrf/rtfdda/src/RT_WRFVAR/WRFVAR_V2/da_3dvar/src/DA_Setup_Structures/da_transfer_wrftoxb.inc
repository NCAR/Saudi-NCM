SUBROUTINE DA_Transfer_WRFToXb( xbx, grid, &
!
#include <em_dummy_args.inc>
!
                              )

!------------------------------------------------------------------------------
!  PURPOSE: Transfers fields from WRF to first guess structure.
!
!  METHOD:  
!
!  HISTORY: 09/27/2002		Wei Huang
!
!  HISTORY: 06/28/2004 with respect to the cloud water and rain water        Jianfeng Gu
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xbx_type), INTENT(INOUT)     :: xbx          ! Header & non-gridded vars.

   TYPE(domain), TARGET               :: grid

!  Definitions of dummy arguments to solve

#include <em_dummy_decl.inc>

!---------------------------------------------------------------------------

   integer :: i, j, k, n

   integer :: is, ie, js, je, ks, ke

   real    :: theta, tmpvar, zrhom

   real    :: ssmi_psfc, ssmi_ta,  ssmi_gamma, zcld, &
              ssmi_tpw,  ssmi_alw, ssmi_dum,   ssmi_htpw 

   REAL, DIMENSION(xp%ims:xp%ime,xp%jms:xp%jme) :: rgh_fac

   character(len=19) :: current_date

!---------------------------------------------------------------------------
   real :: loc_psac_mean

   real, dimension(xp%jds:xp%jde) :: loc_latc_mean

   integer :: ierror, size2d

   real, dimension(xp%kms:xp%kme) :: DDT
!------------------------------------------------------------------------------

   REAL   :: qvf1, cvpm, cpovcv, ppb, ttb, albn, aln

!---------------------------------------------------------------------------
!--Set xb array range indices for processor subdomain.
!---------------------------------------------------------------------------

   is = xp % its
   ie = xp % ite
   js = xp % jts
   je = xp % jte
   ks = xp % kts
   ke = xp % kte

   xb % map  = grid%map_proj
!  xb % ds   = 1.0/grid%rdx
   xb % ds   = grid%dx

   xb % mix = xp % ide - xp % ids + 1
   xb % mjy = xp % jde - xp % jds + 1
   xb % mkz = xp % kde - xp % kds + 1

   xbx%big_header%bhi(5,5) = grid%start_year
   xbx%big_header%bhi(6,5) = grid%start_month
   xbx%big_header%bhi(7,5) = grid%start_day
   xbx%big_header%bhi(8,5) = grid%start_hour

!---------------------------------------------------------------------------
!--WRF-specific fields:
!---------------------------------------------------------------------------

   ptop = grid%p_top
   ps0  = 100000.0    ! Base sea level pressure
   ts0  = 300.0       ! Base potential temperature for all levels.
   tis0 = 290.0       ! Base sea level temperature
   tlp  = 50.0        ! temperature difference from 1000mb to 300mb

   xb%sigmaf(ke+1) = znw(ke+1)

   xb%znw(ke+1) = znw(ke+1)
   xb%znu(ke+1) = 0.0
 
   do k=ks,ke
     xb%sigmah(k) = znu(k)
     xb%sigmaf(k) = znw(k)

     xb%znu(k) = znu(k)
     xb%znw(k) = znw(k)
     xb%dn(k)  = dn(k)
     xb%dnw(k) = dnw(k)
   enddo

   xb % ptop = ptop
   xb % ps0  = ps0
   xb % ts0  = ts0
   xb % tlp  = tlp
      
!---------------------------------------------------------------------------
!--Convert WRF fields to xb:
!---------------------------------------------------------------------------

   if(print_detail > 0) then
     write(unit=*, fmt='(3a, i8)') &
          'file:', __FILE__, ', line:', __LINE__

     print *, 'is,ie=', is,ie
     print *, 'js,je=', js,je
     print *, 'ks,ke=', ks,ke

     if(print_detail > 10) then
     write(unit=*, fmt='(/5a/)') &
          'lvl         dnw                dn 		rdnw       rdn'

     do k=ks,ke+1
        write(unit=*, fmt='(i3,8f16.8)') k, &
              dnw(k), dn(k), rdnw(k), rdn(k)
     enddo

     write(unit=*, fmt='(/5a/)') &
          'lvl         znu                 znw		rdnw       rdn'

     do k=ks,ke+1
        write(unit=*, fmt='(i3,8f16.8)') k, &
              xb%sigmah(k), xb%sigmaf(k), rdnw(k), rdn(k)
     enddo
     endif

     if(print_detail > 100) then
     write(unit=*, fmt='(/5a/)') &
          'lvl         phb                 ph_2'

     do k=ks,ke
        write(unit=*, fmt='(i3,8e20.12)') k, &
              phb(is,js,k), ph_2(is,js,k)
     enddo

     write(unit=*, fmt='(/5a/)') ' simple varbles:'

     if(je == xp%jde) then
        print *, ' '
        
        do k=ks+5,ke,10
           do i=is,ie,10
              write(unit=*, fmt='(3(a,i5),a,e20.12)') &
                   'v_2(', i, ',', xp%jde+1, ',', k, ')=', &
                    v_2(i, xp%jde+1,k)
           enddo
           print *, ' '
        enddo
     endif

     if(ie == xp%ide) then
        print *, ' '

        do k=ks+5,ke,10
           do j=js,je,10
              write(unit=*, fmt='(3(a,i5),a,e20.12)') &
                   'u_2(', xp%ide+1, ',', j, ',', k, ')=', &
                    u_2(xp%ide+1,j,k)
           enddo
           print *, ' '
        enddo
     endif
     endif

     write(unit=*, fmt='(/5a/)') ' simple varbles:'

     print *, ' u_1(is,js,ks)=',  u_1(is,js,ks)
     print *, ' v_1(is,js,ks)=',  v_1(is,js,ks)
     print *, ' w_1(is,js,ks)=',  w_1(is,js,ks)
     print *, ' t_1(is,js,ks)=',  t_1(is,js,ks)
     print *, 'ph_1(is,js,ks)=', ph_1(is,js,ks)


     print *, ' u_2(is,je,ks)=',  u_2(is,je,ks)
     print *, ' v_2(ie,js,ks)=',  v_2(ie,js,ks)
     print *, ' w_2(is,js,ks)=',  w_2(is,js,ks)
     print *, ' t_2(is,js,ks)=',  t_2(is,js,ks)
     print *, 'ph_2(is,js,ks)=', ph_2(is,js,ks)
     print *, ' phb(is,js,ks)=',  phb(is,js,ks)

     print *, 'grid%sm31,grid%em31,grid%sm32,grid%em32,grid%sm33,grid%em33=', &
               grid%sm31,grid%em31,grid%sm32,grid%em32,grid%sm33,grid%em33

     print *, 'p_top=', grid%p_top
     print *, 'znu(ks)=', znu(ks)
     print *, 'mu0(is,js)=', mu0(is,js)
     print *, 'mub(is,js)=', mub(is,js)
     print *, 'mu_2(is,js)=', mu_2(is,js)

     print *, 'hbot(is,js)=', hbot(is,js)
     print *, 'htop(is,js)=', htop(is,js)

     print *, 'grid%p_top=', grid%p_top
     print *, 'num_moist=', num_moist
     print *, 'P_QV=', P_QV

     print *, 'moist(is,js,ks,2)=', moist(is,js,ks,2)
     print *, ' '
   endif

!---------------------------------------------------------------
!--Need this to exchange values in the halo region.
!  xa%u and xa%v are used as tempory arrays and so
!  it is easy to use the existing exchange scheme.
!
!  Note, this is needed as u_2 and v_2 has no guarantee
!  the most east column, and the most north raw are
!  properly initailized for each tile.
!                             Wei Huang, 03/06/03
!---------------------------------------------------------------

   do j=js,je
   do k=ks,ke
   do i=is,ie+1
      xa%u(i,j,k) = u_2(i,j,k)
   enddo
   enddo
   enddo

   do j=js,je+1
   do k=ks,ke
   do i=is,ie
      xa%v(i,j,k) = v_2(i,j,k)
   enddo
   enddo
   enddo

!--Fill the halo region for u and v.

   call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id3)

!---------------------------------------------------------------

   if(print_detail > 0) then
   print *, 'is,ie,js,je,ks,ke=', is,ie,js,je,ks,ke
   print *, 'xp%its, xp%ite=', xp%its, xp%ite
   print *, 'xp%jts, xp%jte=', xp%jts, xp%jte
   print *, 'xp%kts, xp%kte=', xp%kts, xp%kte
   print *, 'xp%ims, xp%ime=', xp%ims, xp%ime
   print *, 'xp%jms, xp%jme=', xp%jms, xp%jme
   print *, 'xp%kms, xp%kme=', xp%kms, xp%kme
   print *, 'xp%ids, xp%ide=', xp%ids, xp%ide
   print *, 'xp%jds, xp%jde=', xp%jds, xp%jde
   print *, 'xp%kds, xp%kde=', xp%kds, xp%kde

   print *, 'size(xb%u, dim=1)=', size(xb%u, dim=1)
   print *, 'size(xb%u, dim=2)=', size(xb%u, dim=2)
   print *, 'size(xb%u, dim=3)=', size(xb%u, dim=3)
   print *, 'size(xb%v, dim=1)=', size(xb%v, dim=1)
   print *, 'size(xb%v, dim=2)=', size(xb%v, dim=2)
   print *, 'size(xb%v, dim=3)=', size(xb%v, dim=3)

   print *, 'size(xa%u, dim=1)=', size(xa%u, dim=1)
   print *, 'size(xa%u, dim=2)=', size(xa%u, dim=2)
   print *, 'size(xa%u, dim=3)=', size(xa%u, dim=3)
   print *, 'size(xa%v, dim=1)=', size(xa%v, dim=1)
   print *, 'size(xa%v, dim=2)=', size(xa%v, dim=2)
   print *, 'size(xa%v, dim=3)=', size(xa%v, dim=3)

   print *, 'size(u_2, dim=1)=', size(u_2, dim=1)
   print *, 'size(u_2, dim=2)=', size(u_2, dim=2)
   print *, 'size(u_2, dim=3)=', size(u_2, dim=3)
   print *, 'size(v_2, dim=1)=', size(v_2, dim=1)
   print *, 'size(v_2, dim=2)=', size(v_2, dim=2)
   print *, 'size(v_2, dim=3)=', size(v_2, dim=3)
   endif

   do j=js,je
     k = ke+1

     do i=is,ie
       p(i,j,k) = 0.0
       xb%map_factor(i,j) = msft(i,j)
       xb%cori(i,j) = f(i,j)
       xb%tgrn(i,j) = sst(i,j)
       if(xb%tgrn(i,j) < 100.0) xb%tgrn(i,j) = tmn(i,j)
       xb%lat(i,j) = xlat(i,j)
       xb%lon(i,j) = xlong(i,j)
       xb%terr(i,j) = ht(i,j)
       xb%snow(i,j) = snowc(i,j)
       xb%lanu(i,j) = lu_index(i,j)
       xb%landmask(i,j) = landmask(i,j)
       xb%xland(i,j) = xland(i,j)
     enddo

#if 0
! Adapted the code from "real.init.code" by Y.-R. Guo 05/13/2004:

     do i=is,ie
     k = ke
     qvf1 = 0.5*(moist(i,j,k,P_QV)+moist(i,j,k,P_QV))
     qvf2 = 1./(1.+qvf1)
     qvf1 = qvf1*qvf2
     p(i,j,k) = -0.5*(mu_2(i,j)+qvf1*mub(i,j))/rdnw(k)/qvf2

     do k = ke-1,1,-1
       qvf1 = 0.5*(moist(i,j,k,P_QV)+moist(i,j,k+1,P_QV))
       qvf2 = 1./(1.+qvf1)
       qvf1 = qvf1*qvf2
       p(i,j,k) = p(i,j,k+1) - (mu_2(i,j)+qvf1*mub(i,j))/qvf2/rdn(k+1)
     enddo
     enddo
#endif

! Adapted the code from WRF module_big_step_utilities_em.F ----
!         subroutine calc_p_rho_phi      Y.-R. Guo (10/20/2004)

     cvpm =  - (1. - gas_constant/cp)
     cpovcv = cp / (cp - gas_constant)

     do k=ks,ke
       do i=is,ie
! .. The base specific volume (from real.init.code):...........
         ppb  = znu(k) * mub(i,j) + ptop
         ttb  = (tis0 + tlp*log(ppb/ps0)) * (ps0/ppb)**kappa
         albn = (gas_constant/ps0) * ttb * (ppb/ps0)**cvpm
! .............................................................
         qvf1 = 1. + moist(i,j,k,P_QV) / rd_over_rv
         aln  = -1. / (mub(i,j)+mu_2(i,j)) * ( albn*mu_2(i,j) &
                + rdnw(k) *(ph_2(i,j,k+1) - ph_2(i,j,k)) )
! .. total pressure:
         xb%p(i,j,k) = ps0 * ( (gas_constant*(ts0+t_2(i,j,k))*qvf1) / &
                            (ps0*(aln+albn)) )**cpovcv
! .. total density
         xb%rho(i,j,k)= 1.0 / (albn+aln)
! .. pressure purtubation:
         p(i,j,k) = xb%p(i,j,k) - ppb
       enddo
     enddo

     do k=ks,ke+1
       do i=is,ie
         xb%hf(i,j,k) = (phb(i,j,k)+ph_2(i,j,k))/gravity
         xa%w (i,j,k) = w_2(i,j,k)
         xb%w (i,j,k) = w_2(i,j,k)
       enddo
     enddo

     do k=ks,ke
       do i=is,ie

         xb%u(i,j,k) = 0.5*(xa%u(i,j,k)+xa%u(i+1,j,k))
         xb%v(i,j,k) = 0.5*(xa%v(i,j,k)+xa%v(i,j+1,k))
         xb%wh(i,j,k)= 0.5*(xb%w(i,j,k)+xb%w(i,j,k+1))
         xb%h(i,j,k) = 0.5*(xb%hf(i,j,k)+xb%hf(i,j,k+1))

         xb%q(i,j,k) = moist(i,j,k,P_QV)

         theta = ts0 + t_2(i,j,k)
         xb%t(i,j,k) = theta*(xb%p(i,j,k)/ps0)**kappa

!  Convert to specific humidity from mixing ratio of water vapor:
         xb%q(i,j,k)=xb%q(i,j,k)/(1.0+xb%q(i,j,k))

         if(xb%q(i,j,k) < 1.0e-6) &
            xb%q(i,j,k) = 1.0e-6
!
! Background qrn needed for Radar Radial velocity assimilation:

         if( size(moist,dim=4) >= 4 ) then
!          write(6,*)'P_QV, P_QCW, P_QRN:',P_QV, P_QCW, P_QRN
           xb%qcw(i,j,k) = moist(i,j,k,p_qc)
           xb%qrn(i,j,k) = moist(i,j,k,p_qr)
           xb%qt (i,j,k) = xb%q(i,j,k) + xb%qcw(i,j,k) + xb%qrn(i,j,k)
         endif

         if( size(moist,dim=4) >= 6 ) then
           xb%qci(i,j,k) = moist(i,j,k,p_qi)
           xb%qsn(i,j,k) = moist(i,j,k,p_qs)
         endif

         if( size(moist,dim=4) >= 7 ) then
           xb%qgr(i,j,k) = moist(i,j,k,p_qg)
         endif

       enddo
     enddo

     do i=is,ie
       xb%psac(i,j) = mub(i,j)+mu_2(i,j)
       xb%psfc(i,j) = mub(i,j)+p(i,j,ks)+grid%p_top

       if(xb%tgrn(i,j) < 100.0) &
          xb%tgrn(i,j) = xb%t(i,j,ks)+0.0065*(xb%h(i,j,ks+1)-xb%hf(i,j,ks))
     enddo
   enddo

   xb%ztop = xb%hf(is,js,ke+1)

   if(print_detail > 0) then
     print *, ' '
     if(print_detail > 10) then
     write(unit=*, fmt='(/5a/)') &
          'lvl         h                 p                t'

     do k=ks,ke
        write(unit=*, fmt='(i3,8e20.12)') k, &
              xb%h(is,js,k), xb%p(is,js,k), xb%t(is,js,k)
     enddo
     endif

     print *, ' '
     print *, 'xb%u(is,je,ke)=', xb%u(is,je,ke)
     print *, 'xb%v(ie,js,ke)=', xb%v(ie,js,ke)
     print *, 'xb%w(is,js,ke)=', xb%w(is,js,ke)
     print *, 'xb%t(is,js,ke)=', xb%t(is,js,ke)
     print *, 'xb%p(is,js,ke)=', xb%p(is,js,ke)
     print *, 'xb%q(is,js,ke)=', xb%q(is,js,ke)
     print *, 'xb%h(is,js,ke)=', xb%h(is,js,ke)
     print *, 'xb%hf(is,js,ke)=', xb%hf(is,js,ke)
     print *, 'xb%map_factor(is,js)=', xb%map_factor(is,js)
     print *, 'xb%cori(is,js)=', xb%cori(is,js)
     print *, 'xb%tgrn(is,js)=', xb%tgrn(is,js)
     print *, 'xb%lat(is,js)=', xb%lat(is,js)
     print *, 'xb%lon(is,js)=', xb%lon(is,js)
     print *, 'xb%terr(is,js)=', xb%terr(is,js)
     print *, 'xb%snow(is,js)=', xb%snow(is,js)
     print *, 'xb%lanu(is,js)=', xb%lanu(is,js)
     print *, 'xb%landmask(is,js)=', xb%landmask(is,js)
     print *, '(ie,je)=', ie,je                   !hcl
     print *, 'xb%lat(ie,je)=', xb%lat(ie,je)     !hcl
     print *, 'xb%lon(ie,je)=', xb%lon(ie,je)     !hcl
     print *, ' '
   endif

!------------------------------------------------------------------------------
!  [3.0] Calculate vertical inner product for use in vertical transform:
!------------------------------------------------------------------------------
      
   IF ( vertical_ip == 1 ) THEN
   
!     Vertical inner product is SQRT(Delta p):
      DO k=ks,ke
         xb % vertical_inner_product(is:ie,js:je,k) = &
              SQRT( xb % psac(is:ie,js:je) * xb%sigmah(k) )
      END DO
      
   ELSE IF ( vertical_ip == 2 ) THEN

!     Vertical inner product is Delta p:
      DO k=1,xb%mkz
         xb % vertical_inner_product(is:ie,js:je,k) = &
         xb % psac(is:ie,js:je) * xb%sigmah(k)
      END DO
   
   END IF

!------------------------------------------------------------------------------
!--Roughness
!------------------------------------------------------------------------------

   current_date = '2000-01-01_00:00:00'

   write(current_date(1:19), fmt='(i4.4, 5(a1, i2.2))') &
         grid%start_year, '-', &
         grid%start_month, '-', &
         grid%start_day, '_', &
         grid%start_hour, ':', &
         grid%start_minute, ':', &
         grid%start_second

   print *, 'current_date=', current_date

   xbx % mminlu = 'USGS'

   call roughness_from_lanu(19, xbx % mminlu, current_date, xp, &
#ifdef DEREF_KLUDGE
                            xb % lanu(xp%ims,xp%jms), xb % rough(xp%ims,xp%jms))
#else
                            xb % lanu, xb % rough)
#endif

!------------------------------------------------------------------------------
!--Calculate 1/grid box areas:
!------------------------------------------------------------------------------

   if(print_detail > 0) then
       write(unit=*, fmt='(/a, e24.16)') &
           'xb % ds=', xb % ds

      write(unit=*, fmt='(a, e24.16/)') &
           'xb % map_factor(is,js)=', xb % map_factor(is,js)
   endif

!------------------------------------------------------------------------------
!--Compute 10m wind, 2m temperature and 2m mixing ratio
!------------------------------------------------------------------------------

!FVDB addition begins: 
! For QC, use 10m wind, 2m temperature and 2m mixing ratio
!read from WRF output file, recompute those values for input file only.

   IF ( process_obs == 'YES' .OR. process_obs == 'yes' ) THEN

        xb%u10 = u10
        xb%v10 = v10
        xb%t2  = t2
! Convert 2m mixing ratio to 2m specific humidity for consistency with 3D data
!       xb%q2  = q2
        xb%q2  = q2 / (1 + q2)

!FVDB addition ends

   ELSE

   do j=js,je
   do i=is,ie
      if( xb%ztop < xb%hf(i,j,ke+1) ) &
          xb%ztop = xb%hf(i,j,ke+1)

      tmpvar = xb%ds / xb%map_factor(i,j)

      xb % grid_box_area(i,j) = tmpvar*tmpvar

!-----Calculate surface variable(wind, moisture, temperature)
!-----sfc variables: 10-m wind, and 2-m T, Q, at cross points

      call sfc_wtq(xb%psfc(i,j), xb%tgrn(i,j), &
                   xb%p(i,j,ks), xb%t(i,j,ks), xb%q(i,j,ks), &
                   xb%u(i,j,ks), xb%v(i,j,ks), &
                   xb%p(i,j,ks+1), xb%t(i,j,ks+1), xb%q(i,j,ks+1), &
                   xb%h(i,j,ks), xb%rough(i,j),xb%xland(i,j), &
                   xb%u10(i,j), xb%v10(i,j), xb%t2(i,j), xb%q2(i,j), &
                   xb%regime(i,j))
   enddo
   enddo

   ENDIF

!------------------------------------------------------------------------------
!--Calculate saturation vapour pressure and relative humidity:
!------------------------------------------------------------------------------

   do j=js,je
   do k=ks,ke
   do i=is,ie
      call DA_TPQ_To_RH( xb % t(i,j,k), xb % p(i,j,k), xb % q(i,j,k), &
                         xb %es(i,j,k), xb %qs(i,j,k), xb %rh(i,j,k) )
   end do
   end do
   end do

!------------------------------------------------------------------------------
!--Calculate dew point temperature:
!------------------------------------------------------------------------------

#ifdef DEREF_KLUDGE
   call DA_TRH_To_TD(xb % rh(xp%ims,xp%jms,xp%kms), &
                     xb %  t(xp%ims,xp%jms,xp%kms), &
                     xb % td(xp%ims,xp%jms,xp%kms), xp)
#else
   call DA_TRH_To_TD(xb % rh, xb % t, xb % td, xp)
#endif

   if(print_detail > 10) then
      i=is; j=js; k=ks

      print *, 'i,j,k=', i,j,k
      print *, 'xb % td(i,j,k)=', xb % td(i,j,k)
      print *, 'xb % es(i,j,k)=', xb % es(i,j,k)
      print *, 'xb % rh(i,j,k)=', xb % rh(i,j,k)
      print *, 'xb % qs(i,j,k)=', xb % qs(i,j,k)
      print *, ' '
   endif

!------------------------------------------------------------------------------
!--Sea level pressure and total precipitable water
!------------------------------------------------------------------------------

   if(print_detail > 0) then
     write(unit=*, fmt='(3a, i8)') &
          'file:', __FILE__, ', line:', __LINE__

     write(unit=*, fmt='(a)') &
          'Sea level pressure and total precipitable water'
   endif

   call DA_WRF_TPQ_2_SLP ( xb )

!  do j = js,je
!  do i = is,ie
!     call DA_TPQ_To_SLP( xb%t(i,j,:), xb%q(i,j,:), xb%p(i,j,:), xb%terr(i,j), &
!                        xb%psfc(i,j), xb%slp(i,j), xp )
!  end do
!  end do

   call INTEGRAT_dZ( xb )

!------------------------------------------------------------------------------
!--Surface Wind speed
!------------------------------------------------------------------------------

   if(print_detail > 0) then
     write(unit=*, fmt='(3a, i8)') &
          'file:', __FILE__, ', line:', __LINE__

     write(unit=*, fmt='(a)') &
          'Surface Wind speed'
   endif

   tmpvar = log(10./0.0001)

   do j=js,je
   do i=is,ie
      rgh_fac(i,j) = 1.0/log(xb%h(i,j,ks)/0.0001)

      xb%speed(i,j) = sqrt(xb%u(i,j,ks)*xb%u(i,j,ks) &
                         + xb%v(i,j,ks)*xb%v(i,j,ks) + 1.0e-6) &
                    *tmpvar*rgh_fac(i,j)
   enddo
   enddo

!------------------------------------------------------------------------------
!--Brightness temperature SH Chen
!------------------------------------------------------------------------------

   if(print_detail > 0) then
     write(unit=*, fmt='(3a, i8)') &
          'file:', __FILE__, ', line:', __LINE__

     write(unit=*, fmt='(a)') &
          'Brightness temperature'
   endif

   if ( Use_SsmiTbObs)   &
   call DA_Transform_XToTb( xb )

!------------------------------------------------------------------------------
!--GPS Refractivity linked by Y.-R. Guo 05/28/2004
!------------------------------------------------------------------------------

   if(print_detail > 0) then
     write(unit=*, fmt='(3a, i8)') &
          'file:', __FILE__, ', line:', __LINE__

     write(unit=*, fmt='(a)') &
          'GPS Refractivity'
   endif

   call da_transform_XTogpsref(xb, xp)

!------------------------------------------------------------------------------
!--Ground-based GPS ZTD must follow the GPS Refractivity calculation. YRG 04/05/05
!------------------------------------------------------------------------------

!  if (Use_GpsztdObs) call DA_Transform_XToZTD( xb )

!------------------------------------------------------------------------------
!--Calculate means for later use in setting up background errors.
!------------------------------------------------------------------------------

!  if(.not. associated(xbx % latc_mean)) then
      allocate ( xbx % latc_mean(xp%jds:xp%jde) )
!  endif

   size2d = (xp%ide-xp%ids+1)*(xp%jde-xp%jds+1)

   tmpvar = 1.0/real(size2d)

   loc_psac_mean = tmpvar*SUM( xb % psac(xp%its:xp%ite,xp%jts:xp%jte) )

   tmpvar = 1.0/real(xp%ide-xp%ids+1)

   loc_latc_mean(:) = 0.0

   do j=js,je
      loc_latc_mean(j) = tmpvar*SUM( xb % lat(is:ie, j) )
   enddo

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( loc_psac_mean, xbx % psac_mean, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )
   call MPI_ALLREDUCE( loc_latc_mean, xbx % latc_mean, (xp%jde-xp%jds+1), &
                       TRUE_MPI_REAL, MPI_SUM, MPI_COMM_WORLD, IERROR )
#else
   xbx % psac_mean = loc_psac_mean
   xbx % latc_mean = loc_latc_mean
#endif

   if(print_detail > 10) then
!     print *, 'loc_psac_mean  =', loc_psac_mean
      print *, 'xbx % psac_mean=', xbx % psac_mean

!     print *, 'loc_latc_mean  =', loc_latc_mean(js)
      print *, 'xbx % latc_mean=', xbx % latc_mean(js)
   endif

!---Calculate time step from one dimensional cloud model parameterization-------------

   IF ( DT_cloud_model ) THEN

     do j = xp%jts, xp%jte
     do i = xp%its, xp%ite

       CALL DA_CLOUD_MODEL ( xb%t(I,J,:),   xb%p(I,J,:), xb%q(I,J,:), xb%qcw(I,J,:),  &
                             xb%qrn(I,J,:), xb%h(I,J,:), xb%hf(I,J,:),                &
                             DDT, xp%kts, xp%kte  )

     do k = xp%kts, xp%kte
       xb%delt(i,j,k) = DDT(k)
     enddo

     enddo
     enddo

   ENDIF

END SUBROUTINE DA_Transfer_WRFToXb

