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
! Research Application Lazboratory (RAL),
! National Center for Atmospheric Research (NCAR)
! All Rights Reserved
!
! Author: Thomas Nipen
! Date: May 4, 2009
! Description: Takes input data and finds analogous conditions
!              to the conditions at index.
!
! 2012-apr-20	findAnalog_v1.m:
!		Matlab version obtained from Luca Delle Monache, NCAR.
!		Used by Djalalova and Wilczak, NOAA/ESRL/PSD3, for
!		  development of CMAQ bias correction method.
!
! 2014-mar-25	find_analog_v1.f90:
!		Convert Matlab version to Fortran 90.
!		By Dave Allured, NOAA/ESRL/PSD/CIRES.
!		Change function to subroutine, for better interface checking.
!		Remove var_set parameter, compute results over all variables.
!		Remove inactive useWspdAsAnalog kludge, dangerous indexing.
!
! Inputs:
!       data         <day> x <hour> x <vars>
!		     (<day> same as run or forecast cycle)
!       index        < |time|
!       fpar%vmiss   Missing value code
!       fpar%trend   Gives the relative weighting of trend values
!                    Must be an odd number length, where the middle number
!                    refers to the value, for example:
!                    [ T-2 T-1 T T+1 T+2]
!       fpar%window  half size of window for looking for analogs
!       fpar%fhour   Forecast hour
!       fpar%fday    Forecast day
!
!-----------------------------------------------------------------------------

!-----------------------------------------------------------
! Module definitions.
!-----------------------------------------------------------

module find__analog		! standard visibility

  use config, only : dp
  implicit none

! Parameter structure for find_analog.

  type fpar_type

    integer  window		! half size of window for looking for analogs
    integer  fhour		! Forecast hour
    integer  fday		! Forecast day
    integer  useRealTrends	! 1: Use trend, 0: use neighbours
				! 0: (p0 - a0)^2 + (p+ - a+)^2 + (p- - a-)^2
				! 1: (p0 - a0)^2 + (p+ - p- - a+ + a-)^2
    real(dp) lowerMetric	! Lower bound for allowed metric value
				! (see findAnalog)

    real(dp), allocatable :: trend(:)	! Relative weighting of the trend values
				! Must be an odd number length, where the middle
				! number refers to the value, for example:
				!   [ T-2 T-1 T T+1 T+2]

    integer, allocatable :: is_circular(:)	! indicate circular,
    						! 0 or 1 for each set_var
  end type fpar_type

contains

!-----------------------------------------------------------
! find_analog search routine.
!-----------------------------------------------------------

subroutine find_analog (data, vmiss, fpar, fvar, diag, indices, metrics, &
    winLower, winUpper,weight)

  use index__sort
  use stdev_TNcirc
  use wind__dir_error
  implicit none

  real(dp),        intent(in ) :: data(:,:,:)	     ! (days, hours, vars)
  real(dp),        intent(in ) :: vmiss		     ! common missing value
  type(fpar_type), intent(in ) :: fpar		     ! find_analog parameters
  integer,         intent(in ) :: fvar		     ! var index of forecast var
  integer,         intent(in ) :: diag		     ! verbosity, 0=errors only
  real,            intent(in)  :: weight(:)          !ste  weight for each predictor
  integer,  allocatable, intent(out) :: indices(:)   ! 1-D indices within window
  real(dp), allocatable, intent(out) :: metrics(:)   ! 1-D metrics within window
  integer,  intent(out) :: winLower, winUpper	     ! hour range, found window

! Local variables.

  integer nday, nhour, nvars, noffsets,iwindex
  integer trendSize, win_size, nanalogs
  integer w, trendLower, trendUpper
  integer v, i, j, dim_num

  integer hour1, hour2, w1, w2			! hour bounds, stencil & window
  integer fhour					! index of current forecast hour
  integer fday					! index of current forecast day

  integer prev_day				! index of previous day,same as:
  integer ndays_prev				! number of preceeding days

  integer, allocatable :: Ioffsets(:), Ihours(:), Whours(:)

  real(dp), allocatable :: stddata(:,:), varstd(:,:), summed(:,:)
  real(dp), allocatable :: normalized(:,:), metric(:,:)
  real(dp), allocatable :: analogDifference(:,:,:), ave_difs(:,:)
  real(dp), allocatable :: atrend(:,:,:), ftrend(:,:,:)
  real(dp), allocatable :: angular_err(:,:), metric_1d(:)

  logical, allocatable :: hour_mask(:)

! Automatic arrays

  real(dp) varstd_1d(size(data,3))		! nvars

!--------------------------------------------
! Initialize.
!--------------------------------------------

  if (diag >= 6) print *
  if (diag >= 5) print *, '*** find_analog: Start.'

  nday  = size (data, 1)		! get array dimensions
  nhour = size (data, 2)
  nvars = size (data, 3)

  fhour = fpar%fhour			! shorter names for common parameters
  fday  = fpar%fday

  prev_day   = fday - 1			! day before forecast day
  ndays_prev = prev_day			! number of days before this forecast
  					! (same number, different usages)

! Define the window to find analogs for.
! winLower and winUpper are absolute indicies.
! Adjust for the fact that the window can extend below index 1
! or above nhour.

  winLower = max (1,     fhour - fpar%window )
  winUpper = min (nhour, fhour + fpar%window )

  trendSize = (size (fpar%trend) - 1) / 2

  if (diag >= 6) print *, '*** trendSize = ', trendSize

! Adjust window because the analogs must support the same stencil
! as the forecast hour.
!
! If close to the lower boundary, do not look for analogs that are
! ealier than the forecast hour, because it can't support the stencil.
!
! If we're not close to the lower boundary, we must not allow hours
! close to the same boundary.

  if (fhour < trendSize+1) then
    winLower = max (winLower, fhour)
  else
    winLower = max (winLower, trendSize + 1)
  end if

! Likewise for the upper boundary.

  if (fhour > nhour - trendSize) then
    winUpper = min (winUpper, fhour)
  else
    winUpper = min (winUpper, nhour - trendSize)
  end if

  allocate (metric(ndays_prev, winLower:winUpper))   ! alloc only extent needed
  metric = vmiss				     ! clear metric array

! For each variable, subset to current stencil only.
! Collapse (days x hours) to 1-D for computing standard deviations.
! NEW 2014-mar-2: Order of (D,H) SHOULD NOT MATTER for stddata.

  win_size   = winUpper - winLower + 1		! number of hours in stencil
  nanalogs   = ndays_prev * win_size		! size of collapsed dimension
  						! should be same as size(metric)
  allocate (stddata(nanalogs, nvars))

  do v = 1, nvars				! XV <-- DHV, where X = D' * H'
    stddata(:, v) = (/ data(1:prev_day, winLower:winUpper, v) /)
  end do					! collapse 2-D to 1-D, any order

!!  stddata = reshape (permute (data(1:par.fday-1,winLower:winUpper,:), &
!!     [2 1 3]), [(par.fday-1)*(winUpper-winLower + 1) nvar]);    ! (Matlab)
!!				! permute should not have made any difference
!!				! in computing std dev of single vectors

! Compute standard deviation of each variable, so that the metric
! can be weighted by the reciprocal of standard deviation.
! Special Thomas Nipen routine, angular standard devs computed differently.

  call stdevTNcirc (stddata, vmiss, fpar%is_circular, varstd_1d)   ! V <-- XV

! Replicate standard deviations into 2-D array.

  varstd = spread (varstd_1d, 1, ndays_prev)	! DV <-- V

!!  varstd = repmat (stdTNcirc (stddata, par.vmiss, par.var_set, &
!!              par.isCircular), par.fday-1, 1);		! (Matlab)

    if (diag >= 7) then
      print '(a,99i8)', '*** find_analog loop setup:'
      print '(a,99i8)', 'fday, fhour            = ', fday, fhour
      print '(a,99i8)', 'trendSize              = ', trendSize
      print '(a,99i8)', 'win_size               = ', win_size
      print '(a,99i8)', 'nanalogs               = ', nanalogs
      print '(a,99i8)', 'winLower, winUpper     = ', winLower, winUpper
      print *
    end if

!--------------------------------------------
! Find analogs.
!--------------------------------------------

! Loop over hours in the window.
  iwindex=0
hour_loop: &
  do w = winLower, winUpper

  iwindex=iwindex+1
! Compute the stencil size.
! trendLower and trendUpper are offsets relative to fhour, or w.

    trendLower = min (w - 1,     trendSize)
    trendUpper = min (nhour - w, trendSize)

! Adjust stencil when near the edge.

    if (fhour < trendSize + 1    ) trendLower = fhour - 1
    if (fhour > nhour - trendSize) trendUpper = nhour - fhour

    hour1 = fhour - trendLower		! hour indices for forecast stencil
    hour2 = fhour + trendUpper

    w1 = w - trendLower			! hour indices for search stencil
    w2 = w + trendUpper

    if (diag >= 8) then
      print '(a,99i8)', 'nhour                  = ', nhour
      print '(a,99i8)', 'trendSize              = ', trendSize
      print '(a,99i8)', 'win_size               = ', win_size
      print '(a,99i8)', 'nanalogs               = ', nanalogs
      print '(a,99i8)', 'w                      = ', w
      print '(a,99i8)', 'winLower, winUpper     = ', winLower, winUpper
      print '(a,99i8)', 'trendLower, trendUpper = ', trendLower, trendUpper
      print '(a,99i8)', 'hour1, hour2           = ', hour1, hour2
      print '(a,99i8)', 'w1, w2                 = ', w1, w2
      print '(a,99i8)', 'fpar%useRealTrends     = ', fpar%useRealTrends
    end if

!--------------------------------------------
! Compute metric (linear).
!--------------------------------------------

!! Loop over days.  (See original Matlab to recover.)
!!
!!    for d = 1:par.fday-1
!!       for v = 1:nvar
!!          % Find the indicies of the stencil where the prediction
!!          % is not missing.
!!          Ioffsets = find(data(par.fday, hour1:hour2, v) ~= par.vmiss);
!!          analogDifference = abs(data(par.fday, hour1 + (Ioffsets-1), v) &
!!            - data(d, (w - trendLower) + (Ioffsets-1), v));
!!          summed(v)        = sum(analogDifference, 2);
!!       end
!!       metric(d, w)        = sum(summed./varstd);

!--------------------------------------------
! Compute metric (quadratic).
!--------------------------------------------

! Find the indicies of the stencil where the PREDICTION (fvar) is not
! missing.  The prediction is on the *current* forecast day (fday).

!!    Ioffsets = find(data(fday, hour1:hour2, fvar) /= par%vmiss)

    hour_mask = (data(fday, hour1:hour2, fvar) /= vmiss)
    noffsets = count (hour_mask)
    !if(iwindex==1)allocate (Ioffsets(noffsets))
      allocate (Ioffsets(noffsets))

    j = 0
    do i = 1, size (hour_mask)
      if (hour_mask(i)) then
        j = j + 1
        Ioffsets(j) = i			! valid hour indices into stencil
      end if
    end do

!-------------------------------------------------------------
! We have prediction values for all trend values.
!-------------------------------------------------------------

have_all_values: &
    if (noffsets == size (fpar%trend)) then

!-------------------------------------------------------------------------
! Metric based on difference in prediction and difference in the trend.
!-------------------------------------------------------------------------

use_real: &
      if (fpar%useRealTrends == 1) then    ! 1 = use trend, 0 = use neighbors

! Add the middle error.

        analogDifference = (spread (data(fday, fhour:fhour, :), 1, ndays_prev) &
                            - data(1:prev_day, w:w, :) ) ** 2	! DHV

! Now add the trend error.

        ftrend = spread (data(fday, hour1:hour1, :) &
                 - data(fday, hour2:hour2, :), 1, ndays_prev)	! DHV

        atrend = data(1:prev_day, hour1:hour1, :) &
                 - data(1:prev_day, hour2:hour2, :)		! DHV

        analogDifference = analogDifference + (ftrend - atrend) ** 2   ! DHV

!-------------------------------------------------------------------------
! Metric based on difference in prediction and difference in
! neighbouring values.
!-------------------------------------------------------------------------

      else
        allocate (analogDifference(ndays_prev, noffsets, nvars))
        !if(iwindex==1)allocate (analogDifference(ndays_prev, noffsets, nvars))

        do v = 1, nvars

! Use cyclic statistics when using circular variables.

          if (fpar%is_circular(v) == 1) then

            call wind_dir_error (spread (data(fday, hour1:hour2, v), 1, &
              ndays_prev), data(1:prev_day, w1:w2, v), angular_err)  ! DH <-- DH

!ste bug            analogDifference(:,:,v) = angular_err &
!ste bug              * (spread (fpar%trend, 1, ndays_prev) ** 2)	! DH <-- DH
            analogDifference(:,:,v) = (angular_err &
              * spread (fpar%trend, 1, ndays_prev)) ** 2	! DH <-- DH

! Use original for normal variables.

          else
            analogDifference(:,:,v) = &
              ( ( spread (data(fday, hour1:hour2, v), 1, ndays_prev) &
                  - data(1:prev_day, w1:w2, v) ) &
                * spread (fpar%trend, 1, ndays_prev) ) ** 2
          end if

        end do

      end if use_real

!-------------------------------------------------------------
! We don't have values for all trend values, either because
! of missing values, or because we are on an edge.
!-------------------------------------------------------------

    else

      Ihours = hour1 + (Ioffsets(:) - 1)		! hour indices for
      							! forecast stencil

      Whours = (w - trendLower) + (Ioffsets(:) - 1)	! hour indices for
      							! search stencil

      analogDifference = &
        ( spread (data(fday, Ihours(:), :), 1, ndays_prev) &
          - data(1:prev_day, Whours(:), :) ) ** 2	! raw diffs, DHV

    end if have_all_values

!--------------------------------------------
! Compute final metrics.
!--------------------------------------------

! Workaround for gfortran 4.8.2 compiler bug.

    ave_difs = sum (analogDifference, 2) / noffsets	   ! DV <-- DHV

! Average over all valid hours in current stencil.

!!  summed = sqrt (sum (analogDifference, 2) / noffsets)   ! DV <-- DHV

    !if(iwindex==1)allocate (summed(size(ave_difs,1), size(ave_difs,2)))  ! another workaround
    allocate (summed(size(ave_difs,1), size(ave_difs,2)))  ! another workaround
    summed = sqrt (ave_difs)

! Normalize each variable by its standard deviation.

    if (diag >= 7) then
      print '(a,99i8)', '   Varstd zero     count = ', count (varstd == 0)
      print '(a,99i8)', '   Varstd negative count = ', count (varstd <  0)
    end if

!!    where (varstd(:,:) > 0)			! *** breaks gfortran 4.8.2
!!      normalized = summed(:,:) / varstd(:,:)
!!    elsewhere
!!      normalized = vmiss			! flag invalid metrics
!!    end where

    allocate (normalized(size(summed,1),size(summed,2)))
    !if(iwindex==1)allocate (normalized(size(summed,1),size(summed,2)))
    
    normalized(:,:) = vmiss			! flag invalid metrics

    where (varstd(:,:) > 0) normalized = summed(:,:) / varstd(:,:)
    					! protect from divide by zero

! Sum contributions from all the individual variables, for each analog date.
! No metric on days with any missing.
! Metric was initially filled with missing values.

    dim_num = 2					! sum over the var dimension

    
    where (all (normalized /= vmiss, dim_num)) &	! no sum if any missing
!ste multiply each predictor distance for its weight
!      metric(1:prev_day, w) = sum (normalized(:,:), dim_num)	! D <-- DV old  metric with weight =1
       metric(1:prev_day, w) = matmul (normalized(:,:),weight(:))	! D <-- DV !ste new metric
!       print*,"dist after",metric(1,w)
!ste      here weights for different variables must be introduced, sum must be
!replaced
!ste 5Mar 2015 the following lines introduced to handle for window >0
   deallocate (Ioffsets)
   deallocate (analogDifference)
   deallocate (summed)
   deallocate (normalized)
!   deallocate (angular_err)

  end do hour_loop

! Handle invalid metrics.

  if (any (metric(:,:) == vmiss)) then

    if (all (metric(:,:) == vmiss)) then
      metric(:,:) = 1			! default metric when all invalid
      					! is this too strong for invalid?
    else
      print *, '*** find_analog: Either no metrics or all metrics must be' &
        // ' invalid.'
      print '(a,i0,a,i0)', ' *** Forecast day, hour = ', fday, fhour
      call exit (1)
    end if

  end if

!--------------------------------------------
! Sort analogs in window, by the metric.
!--------------------------------------------

! Sort the data within the window, which includes only the
! appropriate hours and days.

! Bound metric to the lowest allowable.

  metric = max (metric, fpar%lowerMetric)

! Flatten the 2-D window array, then sort all analogs by metric.
! Analogs now become scrambled between the day and hour dimensions.

!!  [metrics(:), indices(:)] = sort(submetric(:), 1, 'descend');   ! (Matlab)

  metric_1d = (/ metric /)			! flatten 2-D to 1-D

! For valid comparison testing, use special sort routine to match
! Matlab behavior for ties.  Sort DESCENDING by values, but ties sort
! ASCENDING by their original position indices.
  allocate (indices(nanalogs))
  call index_sort_descending (metric_1d, indices)   ! get sorted indices

  metrics = metric_1d(indices(:))		! also return list of metrics
						! in same descending order

  if (diag >= 6) print *, '*** find_analog: Return.'

end subroutine find_analog
end module find__analog
