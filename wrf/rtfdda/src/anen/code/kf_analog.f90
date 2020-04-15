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

module kf__analog		! standard visibility
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

subroutine kf_analog (obs, pred, vmiss, apar, fpar, kpar, ratio, diag, &
    kfan, ensan, Ianalog, analog_in_an)

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

  real(dp),        intent(out) :: kfan(:,:)		! DH  - KF/AN raw result
  real(dp),        intent(out) :: ensan(:,:)		! DH  - bias corr result
  integer,         intent(out) :: Ianalog(:,:,:)	! DHA - found indicies
  real(dp),        intent(out) :: analog_in_an(:,:,:)	! DHA - found analogs

! Local variables.

!!  character filename*40		! debug only

  integer a1, a2, d, h, i, i2, ilast, fvar
  integer nday, nhour, nvar, dim_analog
  integer winLower, winUpper, nanalogs
  integer num_available, num_selected, ct
  integer nan_count, count_missing

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

!-------------------------------------------------
! Initialize.
!-------------------------------------------------

  if (diag >= 5) print *, '*** kf_analog: Start.'

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

  kfan(:,:)           = vmiss		! clear result arrays
  ensan(:,:)          = vmiss
  analog_in_an(:,:,:) = vmiss
  Ianalog(:,:,:)      = 0

! The first day is set to the prediction,
! because we do not have any analogues to use.

  kfan(1, :) = pred(1, :, fvar)	! DH <-- DHV

!---------------------------------------------------------------
! Main nested loop over each forcast day, and forecast hour.
!---------------------------------------------------------------

day_loop: &
  do d = apar%start_stat, nday		! only loop over target forecast days
  					! to be corrected; start of array is
					! the training period
hour_loop: &
    do h = 1, nhour
      if (diag >= 6) print '(2(a,i0))', 'kf_analog: D = ', d, ', H = ', h

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

        kfan(d, h)  = pred(d, h, fvar)
        ensan(d, h) = pred(d, h, fvar)
        cycle hour_loop
      end if

! Fix for problem with trend for the first and last hours.

! (3 trends currently required.  Needs upgrade to handle other sizes,
! but that probably wants an algorithmic approach.  How about simple
! proportional re-weighting after truncation?)

      if (size (fpar%trend) /= 3) then
        print *, '*** find_analog: Abort, selected trend size is not supported.'
        print *, '*** Currently there must be exactly three trend weights.'
        print '(a,i0)', ' *** Number of weights in fpar%trend = ', &
          size (fpar%trend)
        call exit (1)
      end if

      if (h == 1 .or. h == nhour) then	! first and last hours:
        fpar2%trend = (/ 1 /)		! window constrained, use single weight
      else
        fpar2%trend = fpar%trend	! middle hours: use original trends
      end if

! Find flattened indicies of best analogs.

      if (diag >= 7) print '(a,99(1x,i0))', '  shape (pred) = ', shape (pred)

      call find_analog (pred, vmiss, fpar2, fvar, diag, inds, metric, &
            winLower, winUpper)

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

      obs_flat  = (/ obs (1:d-1, winLower:winUpper)       /)	! 2-D to 1-D
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

      if (apar%skipMissingAnalogs == 1) then

        mask1 = (obstemp(:) /= vmiss) .and. (predtemp(:) /= vmiss)
					! mask to exclude missing value pairs

        mask1(nanalogs) = .true.	! but keep the final pair, which
        				! includes the current (d,h) prediction

        obs_for_ankf  = pack (obstemp,  mask1)
        pred_for_ankf = pack (predtemp, mask1)

        call kf_luca (obs_for_ankf, pred_for_ankf, vmiss, kpar, ratio,diag,temp)

! Option 2.  Keep missing values in time series, which means the
! error variance will increase after each missing value.

      else
        call kf_luca (obstemp, predtemp, vmiss, kpar, ratio, diag, temp)
      end if

! The last value in the return array is the Kalman filtered prediction
! value for the current forecast day and hour.

      kfan(d, h) = temp(size(temp))	! save the KFAN prediction result

!-------------------------------------------------
! AN ensemble + bias computation.
!-------------------------------------------------

      isequence = (/ (i, i = 1, nanalogs) /)	! make consecutive indices

! If selected, ignore missing analogs such that we have apar%num_an
! analogs, when possible.

      if (apar%skipMissingAnalogs == 1) then

!        I = intersect(find(obstemp ~= vmiss), find(predtemp ~= vmiss))
!        I = I(max(1,length(I)-apar%num_an+1):end)

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
      print*,"CT",ct
! Determine type of weighting.

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
      if (ct >= 3) then

        bias = mean_obs - mean_ens
        ilast = size (predtemp)
        ensan(d, h) = ( (mean_ens * ct + predtemp(ilast)) / (ct + 1) ) + bias
          					! original

!!        %ensan(d, h) = predtemp(end) + bias;	    ! Thomas Palined...and me...
!!        %ensan(d, h) = mean_obs;		    ! The more elegant...
!!        %analog_in_an(d, h, 1:ct) = obstemp(Ian);  ! Save the analogs used
!!						     ! for AN (until 9 Jan 12)

        a1 = apar%num_an - ct + 1		! also return the analogs used
        a2 = apar%num_an
        analog_in_an(d, h, a1:a2) = obstemp(Ian)
        print*,"analog_in_an",analog_in_an(d, h, a1:a2)
! Otherwise default to prediction.

      else
        ilast = size (predtemp)
        ensan(d, h) = predtemp(ilast)
!!        %ensan(d, h) = kfan(d, h);		! for the hybrid method
      end if enough_analogs

    end do hour_loop
  end do day_loop

  if (diag >= 6) print *, '*** kf_analog: Return.'

end subroutine kf_analog
end module kf__analog
