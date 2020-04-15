!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
!-----------------------------------------------------------------------------
!
! Copyright University Corporation for Atmospheric Research (UCAR) 2012
! Research Application Laboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! Produces forecast based on Kalman filtering prediction in analog space.
!
! 2012-apr-20	kf_analog.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
!		Used unchanged by Djalalova and Wilczak, NOAA/ESRL/PSD3,
!		  for development of CMAQ bias correction method.
!
! 2014-mar-25	kf_analog.f90:
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Remove unused ensemble dimension, omit bug and simplify.
!		Remove var dimension from only the obs(...) input array.
!		This removes an unnatural co-indexing dependency between
!		  obs and ens arrays, via apar%fvar.
!
! *** To do:
! *** After initial proving, convert reallocates to static arrays,
!     for efficiency.
!
!-----------------------------------------------------------------------------

!-----------------------------------------------------------
! Module definitions.
!-----------------------------------------------------------

module only__analog		! standard visibility
  implicit none

! Parameter structure for kf_analog.

  type apar_type

    integer  fvar		! array subscript for target var to be corrected
    integer  start_stat		! starting point to compute statistics
    integer  num_an		! Number of best analogs to use for AN
    integer  weights		! 0: Do not weight analogs
				! 1: Weight them by the inverse metric
				! 2: Weight them linearly
    integer  skipMissingAnalogs	! Applies to both ANKF and AN
				! 1: Always use num_an analogs,
				! even if some of the best ones are missing
  end type apar_type

contains

!-----------------------------------------------------------
! Analog filter subroutine.
!-----------------------------------------------------------
! WC: 2015-06-22
!   added input for
!
!      lower_limit_target
!      upper_limit_target
!      method
!
! WC: 2015-06-22
!   added input for
!
!      kfan

subroutine only_analog (obs, pred, vmiss, apar, fpar, kpar, ratio, diag,           &
    kfas, kfan, anen_mean, Ianalog, analog_in_an,weight,forecast_step,hour_start,  &
    lower_limit_target,upper_limit_target,method)

  use config, only : dp
  use find__analog
  use ieee_arithmetic
  use kf__luca
  implicit none

  real(dp),        intent(in ) :: obs(:,:)		! DH  - target var only
  real(dp),        intent(in ) :: pred(:,:,:)		! DHV - forecast vars
  real(dp),        intent(in ) :: vmiss			! common missing value
  type(apar_type), intent(in ) :: apar			! kf_analog parameters
  type(fpar_type), intent(in ) :: fpar			! find_analog parameters
  type(kpar_type), intent(in ) :: kpar			! Kalman filter params
  real(dp),        intent(in ) :: ratio
  integer,         intent(in ) :: diag			! verbosity, 0=errs only
  real,            intent(in)  :: weight(:)             ! weight for each predictor 
  ! ==== WC: 2015-06-22  ===
  integer,         intent(in ) :: method              ! option for ANKF
  real,            intent(in)  :: lower_limit_target    ! limit lower range for correction
  real,            intent(in)  :: upper_limit_target    ! limit upper range for correction
  ! ========================

  real(dp),        intent(out) :: kfas(:,:)		! DH  - KFAS raw result
  real(dp),        intent(out) :: kfan(:,:)             ! KFAN: KF applid to AnEN MEAN
  real(dp),        intent(out) :: anen_mean(:,:)	! AnEn MEAN
  integer,         intent(out) :: Ianalog(:,:,:)	! DHA - found indicies
  real(dp),        intent(out) :: analog_in_an(:,:,:)	! DHA - found analogs

! Local variables.

!!  character filename*40		! debug only

  integer a1, a2, d, h, i, i2, ilast, fvar
  integer nday, nhour, nvar, dim_analog,hour_start,p_time
  integer winLower, winUpper, nanalogs,iday,nmaxday,forecast_step
  integer num_available, num_selected, ct
  integer nan_count, count_missing
  real, allocatable ::obs_real(:,:)
  real(dp) mean_ens, mean_obs, bias

  type(fpar_type) fpar2

! Dynamic arrays.

  integer,  allocatable :: inds(:)	! sorted indices from find_analog
  integer,  allocatable :: isequence(:), Ian(:)

  real(dp), allocatable :: obs_flat(:), pred_flat(:)
  real(dp), allocatable :: obstemp(:), predtemp(:)
  real(dp), allocatable :: obs_for_ankf(:), pred_for_ankf(:)
  real(dp), allocatable :: temp(:), weights(:)

  real(dp), allocatable :: metric(:)	! returned metrics for selected analogs

  logical,  allocatable :: mask1(:), mask2(:)

  ! ===== WC: 2015-07-06 =========
  real(dp), allocatable :: anen_mean_1d(:), obs_for_anen_mean_1d(:)

  integer :: ncount
  ! ==============================
!-------------------------------------------------
! Initialize.
!-------------------------------------------------

  if (diag >= 5) print *, '*** only_analog: Start.'

  nday  = size (pred, 1)		! get array dimensions
  nhour = size (pred, 2)		! first two must match obs
  nvar  = size (pred, 3)

  dim_analog = size (Ianalog, 3)

  if (diag >= 6) print '(a,99(1x,i0))', &
    '  nday, nhour, nvar, dim_analog =', nday, nhour, nvar, dim_analog

  if (diag >= 6) then
    print *, 'apar ='
    print *, apar
!!    print *, 'fpar ='
!!    print *, fpar			! print not supported in gfortran 4.8.2
!!    print *, 'kpar ='
!!    print *, kpar			! print not supported in gfortran 4.8.2
  end if

  fvar  = apar%fvar			! short name for readability

  fpar2 = fpar				! local copy of find_analog parameters
  					! for loop control of time values

  kfas(:,:)           = vmiss		! clear result arrays
  kfan(:,:)           = vmiss 
  anen_mean(:,:)          = vmiss
  analog_in_an(:,:,:) = vmiss
  Ianalog(:,:,:)      = 0

! The first day is set to the prediction,
! because we do not have any analogues to use.

  kfas(1, :) = pred(1, :, fvar)	! DH <-- DHV

!---------------------------------------------------------------
! Main nested loop over each forcast day, and forecast hour.
!---------------------------------------------------------------
   allocate(obs_real(nday,nhour))  !ste swap observation matrix used to put future obs to missing

day_loop: &
  do d = apar%start_stat, nday		! only loop over target forecast days
  					! to be corrected; start of array is
					! the training period
hour_loop: &
    do h = 1, nhour
      if (diag >= 6) print '(2(a,i0))', 'only_analog: D = ', d, ', H = ', h

      fpar2%fday  = d			! override target time parameters
      fpar2%fhour = h			! only one target hour at a time,
      					! for find_analog

! Only continue if predictions for ALL variables are available.  If
! any variables are missing values, then default to the original target
! variable prediction for the current hour.  I.e., no bias correction.

      if (any (pred(d, h, :) == vmiss)) then
        if (diag >= 6) print '(3(a,i0))', &
          '*** Missing forecast values, day ', d, ', hour', h, &
          ', skipping bias correction.'

        kfas(d, h)  = pred(d, h, fvar)
        anen_mean(d, h) = pred(d, h, fvar)
        cycle hour_loop
      end if

! Fix for problem with trend for the first and last hours.

! (3 trends currently required.  Needs upgrade to handle other sizes,
! but that probably wants an algorithmic approach.  How about simple
! proportional re-weighting after truncation?)

      !if (size (fpar%trend) /= 3) then
      ! WC: 2015-05-18
      if ( (size (fpar%trend) < 3 ) .and. (size (fpar%trend) > 9 ) ) then
        print *, '*** find_analog: Abort, selected trend size is not supported.'
        print *, '*** Currently there must be exactly three, five, seven or nine trend weights.'
        print '(a,i0)', ' *** Number of weights in fpar%trend = ', &
          size (fpar%trend)
        call exit (1)
      end if

      if ( size (fpar%trend) == 3 ) then

       if (h == 1 .or. h == nhour) then	! first and last hours:
         fpar2%trend = (/ 1 /)		! window constrained, use single weight
       else
         fpar2%trend = fpar%trend	! middle hours: use original trends
       end if

      elseif ( size (fpar%trend) == 5 ) then
       
       if (h == 1 .or. h == nhour) then ! first and last hours:
         fpar2%trend = (/ 1 /)          ! window constrained, use single weight
       elseif (h == 2 .or. h == ( nhour-1 )) then
         fpar2%trend = (/ 1, 1, 1 /)
       else
         fpar2%trend = fpar%trend       ! middle hours: use original trends
       end if

      elseif (size (fpar%trend) == 7 ) then
      
       if (h == 1 .or. h == nhour) then ! first and last hours:
         fpar2%trend = (/ 1 /)          ! window constrained, use single weight
       elseif (h == 2 .or. h == ( nhour-1 )) then
         fpar2%trend = (/ 1, 1, 1 /)
       elseif (h == 3 .or. h == ( nhour-2 )) then
         fpar2%trend = (/ 1, 1, 1, 1, 1 /)
       else
         fpar2%trend = fpar%trend       ! middle hours: use original trends
       end if

      elseif (size (fpar%trend) == 9 ) then

       if (h == 1 .or. h == nhour) then ! first and last hours:
         fpar2%trend = (/ 1 /)          ! window constrained, use single weight
       elseif (h == 2 .or. h == ( nhour-1 )) then
         fpar2%trend = (/ 1, 1, 1 /)
       elseif (h == 3 .or. h == ( nhour-2 )) then
         fpar2%trend = (/ 1, 1, 1, 1, 1 /)
       elseif (h == 4 .or. h == ( nhour-3 )) then
         fpar2%trend = (/ 1, 1, 1, 1, 1, 1, 1 /)
       else
         fpar2%trend = fpar%trend       ! middle hours: use original trends
       end if

      else

         print *, '*** find_analog: Abort, selected trend size is not supported.'
         print *, '*** Currently there must be exactly three, five, seven or nine trend weights.'
         print '(a,i0)', ' *** Number of weights in fpar%trend = ', &
          size (fpar%trend)
         call exit (1)
      endif

      
! Find flattened indicies of best analogs.

      if (diag >= 7) print '(a,99(1x,i0))', '  shape (pred) = ', shape (pred)

      call find_analog (pred, vmiss, fpar2, fvar, diag, inds, metric, &
            winLower, winUpper,weight)

!!      open (10, file='ianalog2')
!!      write (10, '(i10)') inds

!!      open (10, file='metric2')
!!      write (10, '(f10.2)') metric

!!      write (filename, '(a,i3.3,a,i2.2)') 'an/ianalog.', d, '.', h
!!      open (10, file=filename)
!!      write (10, '(i10)') inds

!!      write (filename, '(a,i3.3,a,i2.2)') 'an/metric.',  d, '.', h
!!      open (10, file=filename)
!!      write (10, '(f10.2)') metric

!! print *, '*** DEBUG STOP, KF_ANALOG'
!! stop

!! if (h == 14) then
!!   print '(a,i0,a,i0)', '*** DEBUG STOP, kf_analog, day ', d, ', hour ', h
!!   stop
!! end if

! Flatten the obs and pred arrays in the current window.
! Reallocate on assignment, as needed.

!!      len_flat = (d - 1) * (winUpper - winLower + 1)
!!      allocate (obs_flat (len_flat))		! instead, auto reallocate...
!!      allocate (pred_flat(len_flat))
!ste
!      print*,"d-1,winl winup",vmiss
       obs_real=obs
       nmaxday=nhour*forecast_step/24
!       hour_start=0
!       forecast_step=1
      if(nmaxday>0)then
       do iday=1,nmaxday
        p_time=24/forecast_step*iday-hour_start/forecast_step+2
        obs_real(d-iday,p_time:nhour)=vmiss !ste future observation set to missing
       enddo
      endif

      ! WC: 2015-10-08 ==========
      if ( (d.le.1).or.(winUpper.lt.winLower) ) goto 1000

!ste      obs_flat  = (/ obs (1:d-1, winLower:winUpper)       /)	! 2-D to 1-D
      ! === WC: 2015-10-09 ========
      allocate(obs_flat( (d-1)*(winUpper-winLower+1) ))
      allocate(pred_flat( (d-1)*(winUpper-winLower+1)*fvar ))
      allocate(obstemp( (d-1)*(winUpper-winLower+1) + 1 ))
      allocate(predtemp( (d-1)*(winUpper-winLower+1)*fvar + 1 ))

      obs_flat  = (/ obs_real (1:d-1, winLower:winUpper)       /)	! 2-D to 1-D
      pred_flat = (/ pred(1:d-1, winLower:winUpper, fvar) /)

! Get the selected analog values.
! Append the current (d,h) prediction value as the final element.

      obstemp  = (/ obs_flat (inds), vmiss            /)
      predtemp = (/ pred_flat(inds), pred(d, h, fvar) /)
!ste      print*,"OBSTEMP",obstemp
      nanalogs = size (obstemp)

!-----------------------------------------------------
! Compute prediction for current day and hour.
! Apply Kalman filter to the current analog series.
!-----------------------------------------------------

! Option 1.  Do not use cases where there are missing values for
! ANKF.  This might be beneficial for ANKF, because we don't want
! the error variance to increase when an analog has a missing value.
! Do, however, include the final element, which is the current raw
! prediction.

    ! ==== WC: 2015-06-20 ========
    if (method.eq.2) then
 
     if (apar%skipMissingAnalogs == 1) then

        ! ==== WC: 2015-10-09 =======
        allocate(mask1(size(obstemp)))

        mask1 = (obstemp(:) /= vmiss) .and. (predtemp(:) /= vmiss)
					! mask to exclude missing value pairs

        mask1(nanalogs) = .true.	! but keep the final pair, which
        				! includes the current (d,h) prediction

        obs_for_ankf  = pack (obstemp,  mask1)
        pred_for_ankf = pack (predtemp, mask1)

        ! WC: 2015-06-23
        !   added input for
        !
        !    lower_limit_target
        !    upper_limit_target
        !    .true. for analog
        call kf_luca (obs_for_ankf, pred_for_ankf, vmiss,               &
                      lower_limit_target, upper_limit_target, .false.,  &
                      kpar, ratio, diag, temp)

! Option 2.  Keep missing values in time series, which means the
! error variance will increase after each missing value.

     else
        ! WC: 2015-06-23
        !   added input for
        !
        !    lower_limit_target
        !    upper_limit_target
        !    .true. for analog
        call kf_luca (obstemp, predtemp, vmiss,                        &
                      lower_limit_target, upper_limit_target, .false., &
                      kpar, ratio, diag, temp)
     end if

! The last value in the return array is the Kalman filtered prediction
! value for the current forecast day and hour.

      kfas(d, h) = temp(size(temp))	! save the KFAS prediction result
      if ( (kfas(d, h) /= vmiss) .and. (kfas(d, h) < lower_limit_target) )   &
          kfas(d, h) = lower_limit_target
      if ( (kfas(d, h) /= vmiss) .and. (kfas(d, h) > upper_limit_target) )   &
          kfas(d, h) = upper_limit_target
      goto 1000
    endif
!-------------------------------------------------
! AN ensemble + bias computation.
!-------------------------------------------------

      ! ==== WC: 2015-10-09 =======
      allocate(isequence(nanalogs))

      isequence = (/ (i, i = 1, nanalogs) /)	! make consecutive indices

! If selected, ignore missing analogs such that we have apar%num_an
! analogs, when possible.

      if (apar%skipMissingAnalogs == 1) then

!        I = intersect(find(obstemp ~= vmiss), find(predtemp ~= vmiss))
!        I = I(max(1,length(I)-apar%num_an+1):end)
         ! ===== WC: 2015-10-09 =======
         allocate(mask2(size(obstemp)))

        mask2 = (obstemp /= vmiss .and. predtemp /= vmiss)

        Ian = pack (isequence, mask2)		! indices of non-missing analogs

        num_available = size (Ian)			! limit to requested
        num_selected = min (num_available, apar%num_an)	! number of analogs

        i2 = num_available - num_selected + 1		! reduce index array
        if (i2 > 1) Ian = Ian(i2:)			! to size limit

        if (diag >= 7) print *, 'size (Ian) = ', size (Ian)
!!        print '(10i5)', Ian

! Otherwise, allow fewer than apar%num_an analogs, i.e. when some are missing.
! DEFERRED 2014-mar-11.  This option was not in current use.

      else
        print *,           '*** find_analog: Abort.'
        print '(a,i0,a)', ' *** Option apar%skipMissingAnalogs = ', &
          apar%skipMissingAnalogs, ' is not currently supported.'
        call exit (1)

!!        I = length(predtemp)-apar%num_an:length(predtemp)-1
!!        I = intersect(I, find(obstemp ~= vmiss))
!!        I = intersect(I, find(predtemp ~= vmiss))
      end if

      ct = size (Ian)				! number of selected analogs
!      print*,"CT",ct
! Determine type of weighting.

      ! ==== WC: 2015-10-09 ======
      allocate(weights(ct))

      if (apar%weights == 0) then		! No weighting
        weights = (/ (1, i = 1, ct) /)		! trick for array of ones
      else if (apar%weights == 1) then		! Weighted by metric
        weights = 1 / (metric(Ian) ** 2)

! The weights should not be nans, because the metric for a value
! where the obs and pred are not missing should be a valid number

!!        assert(isempty(find(isnan(metric(end-ct+1:end)), 1)))

        nan_count = count (ieee_is_nan (weights(:)))

        if (nan_count /= 0) then
          print *, '*** find_analog: NaNs detected when computing weights' &
            // ' for metrics.'
          print *, '*** Number of weights = ', ct
          print *, '*** NaN count         = ', nan_count
          call exit (1)
        end if

      else if (apar%weights == 2) then		! Linear weighting
        weights = (/ (i, i = 1, ct) /)		! 1 through N

      else
        print *, '*** find_analog: Abort, weighting scheme not supported.'
        print '(a,i0)', ' *** Option apar%weights = ', apar%weights
        call exit (1)
      end if

      weights = weights / sum (weights)		! Normalize weights

      if (diag >= 7) print *, 'weights = '
      if (diag >= 7) print '(10f8.5)', weights

!!       !!!!!  TEST!!!! SET WEIGHTS TO 1
!!       weights = weights./weights

! Compute weighted mean arrays.

      if (diag >= 7) print *, 'Compute weighted mean arrays:'
      if (diag >= 7) print *, 'Ian ='
      if (diag >= 7) print '(10i6)', Ian
      if (diag >= 7) print *, 'size (Ian)      = ', size (Ian)
      if (diag >= 7) print *, 'size (weights)  = ', size (weights)
      if (diag >= 7) print *, 'size (predtemp) = ', size (predtemp)

      mean_ens = sum (predtemp(Ian) * weights)	! Compute weighted ens mean

      if (diag >= 7) print *, 'size (obstemp)  = ', size (obstemp)

      mean_obs = sum (obstemp(Ian)  * weights)	! Compute weighted obs mean

! Complain about missing values, but do not stop.

      if (diag >= 7) print *, 'count missing:'

      count_missing = count (obstemp(Ian) == vmiss)

      if (diag >= 6 .and. count_missing /= 0) then
        print *, ''
        print '(a,i0,a,i0,a)', '*** find_analog: obstemp(Ian) contains ', &
          count_missing, ' missing values, out of a total of ', &
            size (obstemp(Ian)), ' values.'
        print '(10f8.2)', obstemp(Ian)
      end if

!------------------------------------------------------------------
! Bias correction.  Use analogs if we have 3 or more available.
!------------------------------------------------------------------

! Thomas's method removed, 2014-mar-12.  See original Matlab, to recover.

!!    %if (ct >= 3 && metric(end) <= par%metric_thres)  % hybrid AN-ANKF, v1
!!    %if (ct >= 3 && pred(d, h, par%fvar, e) <= 7) %  hybrid AN-ANKF, v2


enough_analogs: &
      ! ==== WC: 2015-10-19 =====
      !if (ct >= 3) then
      if (ct >= apar%num_an) then

        bias = mean_obs - mean_ens
        ilast = size (predtemp)
        ! ========= commented out by WC: 2015-06-23 =========
        !anen_mean(d, h) = ( (mean_ens * ct + predtemp(ilast)) / (ct + 1) ) + bias
        !  					! original
        anen_mean(d, h) = mean_obs
        if ( (anen_mean(d, h) /= vmiss) .and. (anen_mean(d, h) < lower_limit_target) )   &
          anen_mean(d, h) = lower_limit_target
        if ( (anen_mean(d, h) /= vmiss) .and. (anen_mean(d, h) > upper_limit_target) )   &
          anen_mean(d, h) = upper_limit_target
        ! ======================================================

!!        %anen_mean(d, h) = predtemp(end) + bias;	    ! Thomas Palined...and me...
!!        %anen_mean(d, h) = mean_obs;		    ! The more elegant...
!!        %analog_in_an(d, h, 1:ct) = obstemp(Ian);  ! Save the analogs used
!!						     ! for AN (until 9 Jan 12)

        a1 = apar%num_an - ct + 1		! also return the analogs used
        a2 = apar%num_an
        analog_in_an(d, h, a1:a2) = obstemp(Ian)
        ! ==== added by WC: 2015-06-23 for range check =======
        where ( (analog_in_an(d, h, a1:a2) /= vmiss).and.            &
                (analog_in_an(d, h, a1:a2) < lower_limit_target) )   &
                analog_in_an(d, h, a1:a2) = lower_limit_target

        where ( (analog_in_an(d, h, a1:a2) /= vmiss).and.            &
                (analog_in_an(d, h, a1:a2) > upper_limit_target) )   &
                analog_in_an(d, h, a1:a2) = upper_limit_target
        ! ====================================================
! Otherwise default to prediction.

      else
        ilast = size (predtemp)
        anen_mean(d, h) = predtemp(ilast)
        ! ==== added by WC: 2015-06-23 for range check =======
        if ( (anen_mean(d, h) /= vmiss) .and. (anen_mean(d, h) < lower_limit_target) )   &
          anen_mean(d, h) = lower_limit_target
        if ( (anen_mean(d, h) /= vmiss) .and. (anen_mean(d, h) > upper_limit_target) )   &
          anen_mean(d, h) = upper_limit_target
        ! ====================================================
!!        %anen_mean(d, h) = kfas(d, h);		! for the hybrid method
      end if enough_analogs

1000 continue

     ! ===== WC: 2015-10-09 =======
     if ( (d.gt.1).and.(winUpper.ge.winLower) ) then
      if (method.ne.2) then

       ! deallocate(obs_flat,pred_flat,obstemp,predtemp,isequence,mask2,weights)
       !deallocate(obs_flat,pred_flat,obstemp,predtemp,isequence,weights)
       deallocate(obs_flat,pred_flat,obstemp,predtemp,isequence,weights)
       if (apar%skipMissingAnalogs == 1) then
        deallocate(mask2)
       endif

      else
       deallocate(obs_flat,pred_flat,obstemp,predtemp)
       if (apar%skipMissingAnalogs == 1) then
        deallocate(mask1)
       endif

      endif
     endif

    end do hour_loop
  end do day_loop

  ! ====== added by WC: 2015-07-06 =========
  ! KFAN
  if (method.eq.3) then

   obs_for_anen_mean_1d = (/obs(apar%start_stat:nday,1:nhour)/)
   anen_mean_1d = (/anen_mean(apar%start_stat:nday,1:nhour)/)

   call kf_luca (obs_for_anen_mean_1d, anen_mean_1d, vmiss,        &
                 lower_limit_target, upper_limit_target, .false.,  &
                 kpar, ratio, diag, temp)

   ncount = 0
   do h = 1, nhour
    do d = apar%start_stat, nday
     ncount = ncount + 1
     kfan(d,h) = temp(ncount)
     if ( (kfan(d,h) /= vmiss) .and. (kfan(d,h) < lower_limit_target) )   &
          kfan(d,h) = lower_limit_target
     if ( (kfan(d,h) /= vmiss) .and. (kfan(d,h) > upper_limit_target) )   &
          kfan(d,h) = upper_limit_target
    enddo
   enddo

   deallocate(anen_mean_1d,obs_for_anen_mean_1d)

  endif

  ! ========================================
  if (diag >= 6) print *, '*** only_analog: Return.'

  ! ===== WC: 2015-10-09 =======
  !deallocate(obs_flat,pred_flat,obstemp,predtemp,isequence,weights)
  !if (apar%skipMissingAnalogs == 1) then
  ! deallocate(mask2)
  !endif

end subroutine only_analog
end module only__analog
