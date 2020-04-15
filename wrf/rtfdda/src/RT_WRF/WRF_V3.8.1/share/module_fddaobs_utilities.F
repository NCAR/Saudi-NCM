MODULE module_fddaobs_utilities
!----------------------------------------------------------------------------!
! Following are onstants for use in fddaobs routines.
!
! package constants
! Note: This list MUST BE CONSISTENT with values for Registry variables:
! Here, obs_vobs_dim = 3, obs_errf_dim = 5   (maintained MANUALLY for now!)

  INTEGER, PARAMETER :: NUMNDGV = 6  ! number of potential nudging variables
  CHARACTER*2, PARAMETER :: VSYMBOL(6) = (/'U','V','T','Q','Vr','Rf'/)
  INTEGER, PARAMETER :: U = 1        ! index for U wind component
  INTEGER, PARAMETER :: V = 2        ! index for V wind component
  INTEGER, PARAMETER :: T = 3        ! index for temperature
  INTEGER, PARAMETER :: Q = 4        ! index for moisture 
  INTEGER, PARAMETER :: Vr = 5       ! index for radial wind (radar)
  INTEGER, PARAMETER :: Rf = 6       ! index for reflectivity (radar)
  INTEGER, PARAMETER :: P = 7        ! index for pressure
  INTEGER, PARAMETER :: H = 8        ! index for height
  INTEGER, PARAMETER :: Prs = 2      ! vobs index for pressure
  INTEGER, PARAMETER :: Hgt = 3      ! vobs index for height
  INTEGER, PARAMETER :: SPt = 2      ! index for surface pressure on t-stagger
  INTEGER, PARAMETER :: SPu = 3      ! index for surface pressure on u-stagger
  INTEGER, PARAMETER :: SPv = 4      ! index for surface pressure on v-stagger
  INTEGER, PARAMETER :: Rk  = 5      ! index for vertical level of ob (real value)
! VOB(var,n)    ORDER: var = 1, Prs, Hgt
! ERRF(var,n)   ORDER: var = 1, SPt, SPu, SPv, RKO
!
  INTEGER, Parameter :: MNP=25       ! max nudging terms (SEE Registry NDG_TERM_INDXS(25,2))
  INTEGER, Parameter :: MCH=256      ! maximum number of characters allowed in varlist 
  REAL, Parameter :: missing_r=-888888.
  Integer,Parameter:: missing_i = -888888
  INTEGER,Parameter:: numsnd = 300
  REAL, PARAMETER :: alpha   =   10.  ! used as multiplier in hmax calculation
  REAL, PARAMETER :: max_hag = 1000.  ! do not adjust ob_ht if ob_hag > max_hag
  REAL, PARAMETER :: min_ht  = -500.  ! do not adjust ob_ht if ob_ht  < min_ht
  REAL, PARAMETER :: max_ht  = 5000.  ! do not adjust ob_ht if ob_ht  > max_ht
  CHARACTER(LEN=20), PARAMETER :: dummy_str = "Dummy term, NOT USED"
!!! wuyh added 20160529
  Real,Dimension(NUMNDGV):: wucri=(/10.,10.,8.,0.1,10.,50./)
!!! wuyh added end 20160529

  TYPE sndg_info
    real,    dimension(numsnd) :: rio, rjo, rko, timeob, plfo, rkmob, &
                                  elevob, lev_in_ob, nlevs_ob, oblat, &
                                  oblon, qcu, qcv, qct, qcq
    integer, dimension(numsnd)      :: ob_is_kalman, gainname
    logical, dimension(numsnd)      :: flagsnd, flagbog
    integer, dimension(40,numsnd)   :: wusource, desobs, platform_ob, stnid
    integer, dimension(14,numsnd)   :: wudate
  END TYPE sndg_info

CONTAINS

LOGICAL FUNCTION ob_needs_ht_adjust(ob_ht, terr_ht, stn_elev)
!*******************************************************************************
! Purpose: Apply criteria to determine if the observation will require height
!          a heigh adjustment due to terrain ht and model ht discrepancy.
!*******************************************************************************

  IMPLICIT NONE

  REAL,    INTENT(IN)    :: ob_ht           ! height of observation (meters)
  REAL,    INTENT(IN)    :: terr_ht         ! model ht at ob location (meters)
  REAL,    INTENT(IN)    :: stn_elev        ! ob elevation (meters)

! Local variables
  real :: hmax       ! maximum height(m) to apply adjustment
  real :: ob_hag     ! ob ht above ground (ob_ht - stn_elev)

  hmax = max_ht2adjust(terr_ht, stn_elev)  

  ob_hag = ob_ht - stn_elev
  if(ob_ht .lt. min_ht .or. ob_hag .gt. hmax) then
    ob_needs_ht_adjust = .false.
  else
    ob_needs_ht_adjust = .true.
  endif

END FUNCTION ob_needs_ht_adjust

REAL FUNCTION max_ht2adjust(terr_ht, stn_elev)
!*******************************************************************************
! Purpose: Compute the maximum height for which an observations will have
!          its height adjusted, due to discrepancy between model terrain height
!          and station elevation.
!*******************************************************************************

  IMPLICIT NONE

  REAL,    INTENT(IN)    :: terr_ht         ! model ht at ob location (meters)
  REAL,    INTENT(IN)    :: stn_elev        ! ob elevation (meters)

! Local variables
  real :: d_ht   ! stn_elev - terr_ht 
  real :: hmax   ! terr_ht+alpha*|d_ht|; ob below this means near surface
!!! wuyh modified 20131010
 if(stn_elev .le. min_ht .OR. stn_elev .gt. 9000. .OR. terr_ht .le. min_ht) then
   max_ht2adjust=-10. !sanity check
 else
!!! wuyh modified 20131010
  d_ht = stn_elev - terr_ht
  hmax = alpha * abs(d_ht)             ! wuy and ajb modified 05202013
  max_ht2adjust = min(hmax, abs(d_ht)+max_hag)
 endif
END FUNCTION max_ht2adjust

CHARACTER(LEN=MCH) FUNCTION string_comp( string )

!*************************************************************************
! Purpose: Remove extra blank spaces in character string. This is useful
!          for string comparisons.
!          (ex: UU    VVr TT  QQ --> UU VVr TT QQ) 
!*************************************************************************
  IMPLICIT NONE

  CHARACTER(LEN=*), intent(in)  :: string   ! input string

! Local variables
  integer :: ic, id, ilen

! Initialize
  string_comp = ''

  id = 0
  ilen = len(string)
  do ic = 1, ilen 
    if(ic.lt.ilen) then
      if(string(ic:ic+1).ne."  ") then   ! Check for more than one blank
        id=id+1
        string_comp(id:id) = string(ic:ic)
      endif
    else
        string_comp(id+1:id+1) = string(ic:ic)
    endif
  enddo

END FUNCTION string_comp 
 
SUBROUTINE adjust_ob_height(ob_ht, terr_ht, stn_elev)

!*******************************************************************************
! Purpose: For an observation near the surface, compute an adjusted height (Ha)
!    by subtracting the difference between the model terrain height and the
!    station elevation. An over-simplication of this scheme is:
!
!   if( H > -500 and  H < Ter + alpha * | Ter - Elev | .and. (H-Elev) < 500.) then
!      Ha = H - Elev + Ter
!   else
!      Ha = H
!   endif
!
! The actual scheme involves a ramp for continuity.
!
! NOTE: For efficiency, this subroutine should be called using the conditional:
!
!   if( ob_needs_ht_adjust(ob_ht, terr_ht, stn_elev)) then
!
!       CALL adjust_ob_height(ob_ht, terr_ht, stn_elev)
!
!   endif
!*******************************************************************************

  IMPLICIT NONE

  REAL,    INTENT(INOUT) :: ob_ht           ! ob height data (meters)
  REAL,    INTENT(IN)    :: terr_ht         ! model ht at ob location (meters)
  REAL,    INTENT(IN)    :: stn_elev        ! ob elevation (meters)

! Local variables
  real :: hmax       ! max height for adjustment
  real :: d_ht       ! stn_elev - terr_ht 
  real :: gam        ! scale used in height adjustment
  real :: ob_hag     ! ob ht above ground (ob_ht - stn_elev)

!!! wuyh modified 20131117
!  if(stn_elev.le.-500. .or. ob_ht.lt.stn_elev) return    ! Apply sanity check
   if(stn_elev.le.-500.) return
   if(ob_ht.lt.stn_elev) then
    ob_ht = -888888.                 ! Apply sanity check
    return
   endif
!!! wuyh modified 20131117

  hmax = max_ht2adjust(terr_ht, stn_elev)  
  d_ht = stn_elev - terr_ht
  ob_hag = ob_ht - stn_elev

!write(6,*) 'ob ht abv grnd: ob_hag   = ',ob_hag
!write(6,*) 'max ht 2 adjst: hmax     = ',hmax
!write(6,*) 'terrain height: terr_ht  = ',terr_ht
!write(6,*) 'station elevat: stn_elev = ',stn_elev
!write(6,*) 'difference hgt: d_ht     = ',d_ht
!write(6,*) 'near surface h: hmax     = ',hmax
!write(6,*) 'original ob ht: ob_ht    = ',ob_ht
!write(6,*) 'ob ht above gr: ob_hag   = ',ob_hag

  if(ob_hag.lt.0.5*hmax) then

! Full height adjustment if min_ht <= ob_ht < hmax/2
      gam = 1.0

  elseif(ob_hag.le.hmax) then

! Calc scale 0<gam<1 for if hmax/2 <= ob_ht <= hmax
      gam = 2.0*(hmax-ob_hag)/hmax

!!! wuyh added 20130606
  else
      gam = 0.
!!! wuyh added 20130606 end

  endif

  ob_ht = ob_ht - gam*d_ht        ! Ha = H-gam*d_ht

END SUBROUTINE adjust_ob_height

!------------------------------------------------------------------------------
SUBROUTINE loc2glob(x_l, x_g, xdim, xtyp, domdesc, &
                    ids,ide, jds,jde, kds,kde,     &
                    ims,ime, jms,jme, kms,kme,     &
                    ips,ipe, jps,jpe, kps,kpe )

!------------------------------------------------------------------------------
!  PURPOSE: Make a (2D, REAL) global-grid array x_g by gathering the local
!           (2D, REAL) local-grid array x_l from each processor.
!------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde ! Domain dims.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme ! Memory dims.
  INTEGER, INTENT(IN)  :: ips,ipe, jps,jpe, kps,kpe ! Patch  dims.
  INTEGER, INTENT(IN)  :: domdesc                   ! Domain descriptor
  CHARACTER*(*)        :: xdim, xtyp
  REAL,    INTENT(IN)  :: x_l(ims:ime,jms:jme)      ! Local-grid array
  REAL,    INTENT(OUT) :: x_g(ids:ide,jds:jde)      ! Global-grid array

! Local variables.
  integer       :: i, j                  ! loop counters.
  integer       :: size_global_2d        ! #elements in 2D global array

! LOGICAL, EXTERNAL ::  wrf_dm_on_monitor

! write(6,'(/A)')'Performing local_to_global operation ...'

! 2D, REAL case:
  if (xdim .eq. "2D" .and. xtyp .eq. "REAL") then

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )

!    write(6,*) 'loc2glob:'
!    write(6,*) 'ids = ',ids,' ide = ',ide
!    write(6,*) 'jds = ',jds,' jde = ',jde
!    write(6,*) 'ims = ',ims,' ime = ',ime
!    write(6,*) 'jms = ',jms,' jme = ',jme
!    write(6,*) 'ips = ',ips,' ipe = ',ipe
!    write(6,*) 'jps = ',jps,' jpe = ',jpe

!    write(6,*) 'LOCAL HT:'
!    do J=jps,jps+2
!           write(6,*) 'J = ',j
!           write(6,'(10f10.3)') ( x_l(i,j), i = ips, ipe ) 
!           write(6,*)
!    enddo

     CALL wrf_patch_to_global_real ( x_l, x_g, domdesc, ' ', 'xy',  &
                                     ids, ide-1, jds, jde-1, 1, 1,  &
                                     ims, ime  , jms, jme  , 1, 1,  &
                                     ips, ipe  , jps, jpe  , 1, 1   )

     size_global_2d = (ide-ids+1) * (jde-jds+1)

     CALL wrf_dm_bcast_real( x_g, size_global_2d )
!       write(6,*) 'GLOBAL HT:'
!       do J=jds,jds+2
!           write(6,*) 'J = ',j
!           write(6,'(10f10.3)') ( x_g(i,j), i = ids, ide ) 
!           write(6,*)
!       enddo

#else
     DO J=jds,jde-1
       DO I=ids,ide-1
         x_g(i,j)=x_l(i,j)
       ENDDO
     ENDDO
#endif

  else
    write(6,*) 'STOP in fdda routine LOC2GLOB. Unsupported data type.'
    STOP
  endif

END SUBROUTINE loc2glob

!------------------------------------------------------------------------------
!!! wuyh modified 20131206
!!SUBROUTINE near_surface_ob(height_data, temperature_data, height_qc, &
!!                           near_surf,htij, stn_elev, terr_ht,ob_hto, &
!!                           rio,rjo,ids,ide,jds,jde)
SUBROUTINE near_surface_ob(height_data, temperature_data, height_qc, &
                           near_surf, stn_elev, terr_ht,ob_hto)
!!! wuyh modified 20131206 end

!*************************************************************************
! Purpose: Check for near-surface ob and adjust height if necessary.   
!*************************************************************************
  IMPLICIT NONE

  REAL, intent(inout) :: height_data         ! observation height above sea level (m)
  REAL, intent(inout) :: temperature_data    ! observation temperature
  REAL, intent(inout) :: height_qc           ! observation height qc value
!!  INTEGER, intent(in) :: n                   ! observation index
!!  REAL, intent(in)    :: lev_in_ob           ! level in sounding-type ob
!!  REAL, intent(in)    :: nlevs_ob            ! number of levels in sounding obs
!!  LOGICAL, intent(inout) :: same_sndg        ! false when new sndg, true throughout sndg
  LOGICAL, intent(inout) :: near_surf        ! true if height adjustment is necessary, this sndg
!!  REAL, intent(in)    :: htij(ids:ide, jds:jde) ! 2D terrain height array (m)
  REAL, intent(in)    :: stn_elev            ! ob elevation (meters)
!!  REAL, intent(in)    :: rio                 ! west-east   coord of ob, on non-staggered grid
!!  REAL, intent(in)    :: rjo                 ! south-north coord of ob, on non-staggered grid
!!  INTEGER, intent(in) :: inest               ! nest level
!!  INTEGER, intent(in) :: ids,ide, jds,jde    ! domain dims.
  real, intent(in)    :: terr_ht    ! model terrain ht

! Local variables
  real    :: hob        ! temporary variable to hold height_data
  real    :: twu        ! temporary variable to hold temperature adjustment
  real    :: ob_hto     ! initial value of height_data, for output
  logical :: hit_hmax   ! hit hmax this sounding, no ht adjustment needed

  hit_hmax = .false.   

!!! wuyh modified 20131010
  IF(height_data .gt. min_ht .and. height_data .lt. max_ht &
      .and. height_qc .ge. -0.01) then
!!! wuyh modified 20131010

! Check to see if same_sndg and other flags need to be reset. If continuing in 
! same sounding, we need to set hit_hmax if no further adjustments are necessary.

!!! wuyh modified 20131010

!!!    if( .not. same_sndg ) then

!!!!    if( abs(lev_in_ob-1) .le. 0.01 ) then
!!!        same_sndg = .true.
!!!!        hit_hmax  = .false.
!!!!        near_surf = .true.
!!!      endif
!!!!    else    ! Check to see if no further adjustment is needed for this sounding.
!!!!        if( .not.near_surf ) hit_hmax = .true.
!!!!    endif

!!! wuyh modified 20131010

    hob = height_data ! Use temporary in case height is adjusted

!   write(6,'(/a,i3,3(a,f8.3))') 'BEFORE adjust: N = ',n,              &
!                                ' lev_in_ob = ',lev_in_ob,            &
!                                ' nlevs_ob = ',nlevs_ob,' hob = ',hob

!!!!    if( .not. hit_hmax ) then

! Store ob height before adjustment.
      ob_hto = height_data

! We need model terrain at ob location to do height adjustment check.
!      if(rio .gt. 3 .and. rio .lt. (ide - 3) .and. &
!         rjo .gt. 3 .and. rjo .lt. (jde - 3) )then   
!        terr_ht = terrain_height(rio, rjo, htij, ids,ide, jds,jde)
!      else
!        terr_ht = -888888.
!      endif

      near_surf = ob_needs_ht_adjust(height_data,terr_ht,stn_elev)
      if(near_surf) then
        CALL adjust_ob_height(hob, terr_ht, stn_elev)
        if(temperature_data .gt. 100.) then
          twu = (hob-height_data)*0.0065
          temperature_data = temperature_data - twu
        endif
        height_data = hob    ! return new height
      endif
 1681 Format(F8.3,4(1x,F10.2))
!!! wuyh added 20131107
  ELSE
   near_surf = .false.
!!! wuyh added 20131107 end

  ENDIF   ! end if(height data good)

END SUBROUTINE near_surface_ob

!------------------------------------------------------------------------------
SUBROUTINE make_pair_list( terms, pair_list, nterms, num_chars )

!*************************************************************************
! Purpose: Identify and separate variable pairs from full input (namelist)
!          string of variable pair lists.
!
!          For example: terms="UU  -VrVr    TT" ==> pair_list(1) = "UU"
!                                                  pair_list(2) = "-VrVr"
!                                                  pair_list(3) = "TT"
!          Max number of pairs is MNP.
!          List is assumed to be trimmed, so no trailing blanks!
!          The total number of pairs is also returned.
!*************************************************************************
  IMPLICIT NONE

  CHARACTER(LEN=*), intent(in)  :: terms            ! Var/covar pairs for nudging (symbols)
  CHARACTER*5     , intent(out) :: pair_list(MNP)   ! Separate pair strings (dummy char incl)
  INTEGER         , intent(out) :: nterms           ! Total number of variable pairs
  INTEGER         , intent(out) :: num_chars(MNP)   ! Number of characters for each pair string

! Local variables
  logical :: hit_first_pair
  integer :: ic                  ! counter for number of characters in variable list
  integer :: ip                  ! counter for number of pairs in variable list
  integer :: ncp                 ! counter for number of characters in current pair string
  integer :: tot_chars           ! total number of characters in the list
  integer :: vsymsiz(numndgv)    ! number of characters in symbol for each nudge variable
  character (LEN=MCH) :: varlist ! temp string to hold list of terms

! Error check
  tot_chars = len(terms)

  CALL set_indxs_check_tot_chars(tot_chars)

!********************
! Initialization
!********************

! Add blank to end of list to simplify parsing logic 
  do ic = 1, tot_chars
    varlist(ic:ic) = terms(ic:ic)
  enddo
  varlist(tot_chars+1:tot_chars+1) = ' '
  hit_first_pair = .false.
  ic = 0
  ip = 0
  ncp = 0
  num_chars = -99

! Check for leading blanks
  do while (.not. hit_first_pair)
    ic = ic+1
    if( varlist(ic:ic) .eq. ' ' ) then
    else
      hit_first_pair = .true.
      ic = ic-1
    endif
  end do

! Parse to find variable pair strings
  do while (ic.le.tot_chars)
    ic = ic+1

! Check for end of current pair
    if( varlist(ic:ic) .eq. ' ' ) then
      if(ncp.gt.0) then  ! end of current pair 
        ip = ip+1
        num_chars(ip) = ncp
        if(ncp.gt.5) then
          CALL set_indxs_check_ncp(ip, ncp, varlist(ic-ncp:ic-1))
        endif

! Store in pair array
        pair_list(ip)(1:ncp) = varlist(ic-ncp:ic-1)
        ncp = 0
      endif
    else
      ncp = ncp+1
    endif
  end do

  nterms = ip       ! Now have total number of pairs in list

END SUBROUTINE make_pair_list

!------------------------------------------------------------------------------
SUBROUTINE set_indxs( pair_list, nterms, num_chars, indxs )

!*************************************************************************
! Purpose: Parse through character string containing variable pair lists
!          (ex: UU, VV, -TU, TT) and convert to appropriate integer pairs.
!          Max number of pairs is MNP.
!          List is assumed to be trimmed, so no trailing blanks!
!          NOTE: Dummy character "-" will not be included in pair_list 
!*************************************************************************
  IMPLICIT NONE

  CHARACTER*5     , intent(in)  :: pair_list(MNP)   ! Separate pair strings (dummy term incl)
  INTEGER         , intent(in)  :: nterms           ! Total number of variable pairs
  INTEGER         , intent(in)  :: num_chars(MNP)   ! Number of characters for each pair string
  INTEGER         , intent(out) :: indxs(MNP,2)     ! Var/covar pairs for nudging (indexes)

! Local variables
  integer :: ic                  ! counter for number of characters in variable list
  integer :: ip                  ! counter for number of pairs in variable list
  integer :: vsymsiz(numndgv)    ! number of characters in symbol for each nudge variable

! Store string-size of individual variable symbols (U,V,T,Q,Vr,etc)
  do ic = 1, numndgv
    vsymsiz(ic) = len(trim(vsymbol(ic)))
  enddo

  do ip = 1, nterms

    if( pair_list(ip)(1:1).eq.'-' ) then   ! Dummy term: will not used in nudging: for ex, -Vr
      indxs(ip,1) = 0
      indxs(ip,2) = 0

    else   ! Term will be used in nudging

      if(num_chars(ip).eq.2) then     ! Ex: UU or VV

        do ic = 1, numndgv
          if( vsymsiz(ic).eq.1) then
            if( pair_list(ip)(1:1).eq.vsymbol(ic)(1:vsymsiz(ic)) ) indxs(ip,1) = ic
            if( pair_list(ip)(2:2).eq.vsymbol(ic)(1:vsymsiz(ic)) ) indxs(ip,2) = ic
          endif
        enddo
        
      elseif(num_chars(ip).eq.3) then  ! Ex: VrU (2nd var must be a model var, vsymsiz=1)
        do ic = 1, numndgv
          if( vsymsiz(ic).eq.2 ) then
            if( pair_list(ip)(1:2).eq.vsymbol(ic)(1:2) ) indxs(ip,1) = ic
          else  ! vsymsiz=1 
            if( pair_list(ip)(3:3).eq.vsymbol(ic)(1:1) ) indxs(ip,2) = ic
          endif 
        enddo

      else ! num_chars > 3 is illegal
        indxs(ip,1) = 0
        indxs(ip,2) = 0
      endif

! Check for validity of pair comparing total string length sum of terms matched
      if( indxs(ip,1).eq.0 .or. indxs(ip,2).eq.0 ) then
        CALL set_indxs_check_ncp(ip, num_chars(ip), pair_list(ip))
      elseif( num_chars(ip) .ne. vsymsiz(indxs(ip,1)) + vsymsiz(indxs(ip,2)) ) then
        CALL set_indxs_check_ncp(ip, num_chars(ip), pair_list(ip))
      endif

    endif

  CALL set_indxs_check_convert(ip, indxs(ip,1), indxs(ip,2), pair_list(ip), num_chars(ip))

  enddo

END SUBROUTINE set_indxs

SUBROUTINE get_varlist( nterms, pair_list, num_chars, varlist, lenstr)

!*************************************************************************
! Purpose: Construct a single-string variable list from the pair list,
!          with only a single space between pairs and the entire string
!          enclosed in double quotes.  This string will be used to match
!          the data string at the beginning of each Kalman-gain file.
!
!          The string length (counting the quotes) is also returned.  
!
!          EXAMPLE: From pair_list = UU, VV, TU, TT, the string:
!          "UU VV TT QQ" will be constructed.            
!          NOTE: The individual terms in pair_list might contain the dummy
!                character "-", indicating that this pair is not used in
!                nudging. This character will be copied into varlist. This
!                allows string matching with the file header, even though
!                some pairs might not be used in nudging. 
!*************************************************************************
  IMPLICIT NONE

  INTEGER         , intent(in)  :: nterms           ! Total number of variable pairs
  CHARACTER*5     , intent(in)  :: pair_list(MNP)   ! Separate pair strings
  INTEGER         , intent(in)  :: num_chars(MNP)   ! Number of characters for each pair string
  CHARACTER*(*)   , intent(out) :: varlist          ! Variable pair list in quotes (ex: "UU TT")
  INTEGER         , intent(out) :: lenstr           ! Length of varlist string with quotes

! Local variables
  integer :: ic               ! counter for number of characters in variable list
  integer :: ip               ! counter for number of pairs in variable list
  character*4 :: xpair_list(MNP)   ! temp pair_list string with any dummy characters removed
  integer :: xnum_chars(MNP)  ! Number of characters for each pair string in xpair_list
  character(len=200) :: msg   ! argument to wrf_message

! Create temporary pair string with dummy characters removed
  do ip = 1, nterms
    if(pair_list(ip)(1:1).eq."-") then
      xpair_list(ip)(1:4) = pair_list(ip)(2:5)
      xnum_chars(ip) = num_chars(ip)-1
    else
      xpair_list(ip)(1:4) = pair_list(ip)(1:4)
      xnum_chars(ip) = num_chars(ip)
    endif
  enddo

! Reset varlist from pairs, surrounded in double quotes. (E.g., "UU VV TT QQ")
  varlist(1:1) = '"'
  ic = 1
  do ip = 1, nterms
    ic = ic + 1
    varlist(ic:ic+xnum_chars(ip)-1) = xpair_list(ip)(1:xnum_chars(ip))
    ic = ic + xnum_chars(ip)
    if(ic.gt.MCH) then
      call wrf_message(msg)
      write(msg,'(a)') 'get_varlist: Maximum variable list length exceeded.'
      call wrf_message(msg)
      call wrf_abort
    endif

    if(ip.eq.nterms) then
      varlist(ic:ic) = '"'
    else
      varlist(ic:ic) = ' '
    endif
  enddo
  
  lenstr = ic

END SUBROUTINE get_varlist

SUBROUTINE set_indxs_check_tot_chars(tot_chars)
  IMPLICIT NONE
  integer, intent(in)    :: tot_chars
  character(len=200) :: msg   ! argument to wrf_message

  if(tot_chars.ge.MCH.or.tot_chars.eq.0) then
    call wrf_message("")
    write(msg,'(a)') 'module_fddaobs_utilities, subroutine set_indxs_check_tot_chars:'
    call wrf_message(msg)
    if(tot_chars.ge.MCH) then
      write(msg,'(a,i3,a,i3,a)') 'Namelist string obs_nudge_terms is too long! (len = ', &
                                 tot_chars,') Maximum length allowed is ',MCH,' characters.'
      call wrf_message(msg)
    else
      write(msg,'(a)') 'Namelist string obs_nudge_terms is empty!'
      call wrf_message(msg)
    endif
    call wrf_abort
  endif
END SUBROUTINE set_indxs_check_tot_chars

SUBROUTINE set_indxs_check_ncp(ip, ncp, pair)
  IMPLICIT NONE
  integer, intent(in) :: ip     ! number of pair occurrence in variable pair list
  integer, intent(in) :: ncp    ! number of characters in pair string
  character*(*), intent(in) :: pair   ! illegal pair string
  character(len=200) :: msg     ! argument to wrf_message

  call wrf_message("")
  write(msg,'(a)') 'module_fddaobs_utilities, subroutine set_indxs_check_ncp:'
  call wrf_message(msg)
  write(msg,'(2a)') 'Bad variable pair string in obs_nudge_terms string: ',pair(1:ncp)
  call wrf_message(msg)
  call wrf_abort
END SUBROUTINE set_indxs_check_ncp

SUBROUTINE set_indxs_check_convert(ip, var1, var2, pair, nc)
  IMPLICIT NONE
  integer, intent(in)     :: ip     ! number of pair occurrence in variable pair list
  integer, intent(in)     :: var1   ! first variable in pair
  integer, intent(in)     :: var2   ! second variable in pair
  character*4, intent(in) :: pair   ! pair string
  integer, intent(in)     :: nc     ! number of characters in pair string
  character(len=200)      :: msg   ! argument to wrf_message

  if( var1.lt.0 .or. var2.lt.0 ) then   !0 value for "-" dummy value
! if( var1.le.0 .or. var2.le.0 ) then
    call wrf_message("")
    write(msg,'(a)') 'module_fddaobs_utilities, subroutine set_indxs_check_convert:'
    call wrf_message(msg)
    write(msg,'(a,i2,2a)') 'Could not convert variable symbol pair number ',ip,   &
                           ' in obs_nudge_terms: ',pair(1:nc)
    call wrf_message(msg)
    write(msg,'(a)') 'Please check obs_nudge_terms for correct variable symbol(s).'
    call wrf_message(msg)
    call wrf_abort
  endif
END SUBROUTINE set_indxs_check_convert


SUBROUTINE get_pair_string(var1, var2, pstring, len_str)
!*****************************************************************************
! Purpose: Construct the pair string symbol (and length) for a pair of
!          variable indexes. For example, if the indexes are 5 and 1,
!          respectively, and if the symbol (see global constant VSYMBOL)
!          for 5 is "Vr" and the symbol for 1 is "U", then return
!          pstring = "VrU" and len_str = 3.
!*****************************************************************************

  IMPLICIT NONE
  integer, intent(in)        :: var1      ! Index for first variable
  integer, intent(in)        :: var2      ! Index for second variable
  character*(*), intent(out) :: pstring   ! Pair string
  integer, intent(out)       :: len_str   ! Length of pstring

! Local variables
  integer :: len_v1        ! number of characters in variable 1
  integer :: len_v2        ! number of characters in variable 2

  len_v1 = len(trim(vsymbol(var1)))
  len_v2 = len(trim(vsymbol(var2)))

  pstring(1:len_v1) = vsymbol(var1)(1:len_v1)
  pstring(len_v1+1:len_v1+len_v2) = vsymbol(var2)(1:len_v2)
  len_str = len_v1 + len_v2

END SUBROUTINE get_pair_string

SUBROUTINE conf_REKF_data(subr, filen, varlist, ndglist, len_varlist, len_ndglist)
!*****************************************************************************
! Purpose: Confirm that the requested nudging term list (e.g., "UU, VV")
!          matches the variable list in the REKF data-file header. If not,
!          issue an appropriate error message and terminate the run. 
!*****************************************************************************
  IMPLICIT NONE
  character*(*), intent(in)  :: subr          ! name of calling subroutine
  character*(*), intent(in)  :: filen         ! full file path of Kalman data file
  character*(*), intent(in)  :: varlist       ! variable list
  character*(*), intent(in)  :: ndglist       ! variable list from file 
  integer, intent(in)        :: len_varlist   ! string length of varlist
  integer, intent(in)        :: len_ndglist   ! string length of ndglist 

! Local variables
  character(len=200) :: msg   ! argument to wrf_message
  integer :: xtr              ! flag for output
  
  xtr=0
  if( len_varlist .eq. len_ndglist ) then
    if( varlist(1:len_varlist).ne.ndglist(1:len_varlist) ) then
      xtr=1
      write(msg,'(2a)') subr,': FATAL ERROR: String mismatch in variable list.'
      call wrf_message(msg)
    endif
  else
    xtr=1
    write(msg,'(2a)') subr,': FATAL ERROR: String length mismatch in variable list.'
    call wrf_message(msg)
  endif
  if(xtr.eq.1) then
    write(msg,'(2a)') 'Varlist string: ',varlist(1:len_varlist)
    call wrf_message(msg)
    write(msg,'(4a)') 'String in file ',filen,': ',ndglist(1:len_ndglist)
    call wrf_message(msg)
    CALL wrf_error_fatal ('Check namelist variable obs_nudge_terms and/or file header')
  endif

END SUBROUTINE conf_REKF_data

REAL FUNCTION terrain_height(rio, rjo, htij, ids,ide, jds,jde)
!*******************************************************************************
! Purpose: Compute terrain height at the observation (rio,rjo) coordinate
!*******************************************************************************
  IMPLICIT NONE

  REAL, intent(in)    :: htij(ids:ide, jds:jde) ! 2D terrain height array (m)
  REAL, intent(in)    :: rio                 ! west-east   coord of ob, on non-staggered grid
  REAL, intent(in)    :: rjo                 ! south-north coord of ob, on non-staggered grid
  INTEGER, intent(in) :: ids,ide, jds,jde    ! domain dims.

! Local variables
  real    :: rio_mg     ! west-east   coord of ob, on mass-pt stagger
  real    :: rjo_mg     ! south-north coord of ob, on mass-pt stagger
  real    :: dxob_ms, dyob_ms  ! grid frac in x, y, respectively, on mass-pt stag
  integer :: iob_ms, job_ms    ! integer ob coord, mass-pt stag

  if(rio .gt. 2 .and. rio .lt. ide -2 .and. rjo .gt. 2 .and. rjo .lt. jde-2)then
    rio_mg = rio - 0.5    ! get x coord of obs on mass-pt stagger
    rjo_mg = rjo - 0.5    ! get y coord of obs on mass-pt stagger

    iob_ms = MIN0(MAX0(1,INT(rio_mg)),ide-1)
    job_ms = MIN0(MAX0(1,INT(rjo_mg)),jde-1)

    dxob_ms = rio_mg-FLOAT(INT(rio_mg))
    dyob_ms = rjo_mg-FLOAT(INT(rjo_mg))

! Compute model terrain height at observation location.
    terrain_height = ((1.-dyob_ms)*((1.-dxob_ms)*htij(iob_ms,job_ms)+    &
                   dxob_ms*htij(iob_ms+1,job_ms)                       &
                   )+dyob_ms*((1.-dxob_ms)*htij(iob_ms,job_ms+1)+      &
                   dxob_ms*htij(iob_ms+1,job_ms+1)))
  else
   terrain_height = -888888.
  endif

END FUNCTION terrain_height
!----------------------------------------------------------------------------!


 SUBROUTINE H2P(kts,kte,v_h,v_p,v_t,v_q,h_sfc,p_sfc,p_slp,height,pressure)
!------------------------------------------------------------------------------!
! Recover pressure at a station location from first model 
!------------------------------------------------------------------------------!
! Copyright UCAR (c) 2004 - 2006.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Laboratory (RAL),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
!
! Francois Vandenberghe, vandenb@ucar.edu, August 2006
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      Real,INTENT (inout) :: pressure, height
      INTEGER :: kts, kte
      REAL, DIMENSION (kts:kte)            :: v_h, v_p
      REAL, DIMENSION (kts:kte)            :: v_t, v_q
      REAL                                 :: h_sfc, p_sfc, p_slp

      Integer,parameter :: interp_P = 1
      REAL    :: h_obs, p_obs,h_low, p_low
      REAL    :: aa, bb,z0, dz0, dz1
      INTEGER :: k0
 
      IF (ABS(pressure-missing_r) .le. 1. .AND. height .gt. -500.) THEN       
           If (height > v_h(kte)) THEN !Discard obs above model top
               WRITE (6,'(A)') "Height above highest level"
               RETURN
           Elseif (height > v_h (kts)) THEN
               h_obs = height
               CALL to_zk  (h_obs, v_h, kts,kte, interp_p, z0) 
               CALL toGrid (z0, kts, kte, k0, dz0, dz1)
               p_obs = dz1*v_p(k0) + dz0*v_p(k0+1)
               pressure = p_obs
           Else
               if(height > h_sfc) THEN ! Interpolate between lowest sigma layer and model surface
                   h_obs = height 
                   k0   =  kts
                   dz0 = -(h_obs - v_h (k0))/(v_h (k0)-h_sfc)
                   dz1 =  (h_obs - h_sfc)   /(v_h (k0)-h_sfc)
                   p_obs = dz1*v_p(k0) + dz0*p_sfc
                   pressure = p_obs
               else  ! Interpolate between model surface and slp
                   h_obs = height
                   CALL Get_Pbelow(kts,kte,v_t,v_q,v_p,h_sfc,p_sfc,h_obs,p_obs)
                   pressure = p_obs
               endif
           Endif
      ENDIF
 END SUBROUTINE H2P

SUBROUTINE Get_Pbelow(kts,kte,T,Q,P,TERR,PSFC,HBELOW,PBELOW)

!     PURPOSE  EXTRAPOLATE PRESSURE HEIGHT HBELOW BELOW SURFACE
!              T1/T2=(P1/P2)**(lapse*R/G).                              
!                                                                       
!     INPUT       T        TEMPERATURE
!                 Q        MIXING RATIO
!                 P        PRESSURE
!                 TERR     TERRAIN HEIGHT
!                 PSFC     SURFACE PRESSURE
!                 HBELOW   HEIGHT BELOW SURFACE
!                                                                       
!     OUTPUT      PBELOW   PRESSURE AT HEIGHT HBELOW              

  IMPLICIT NONE                                            

  REAL,PARAMETER :: lapse=6.5E-3,TC=273.15+17.5,PCONST=10000.,EPS=0.622
  REAL,PARAMETER :: Rd = 287.04, gg = 9.81

  Integer:: kts,kte
  REAL, intent(in)                   :: terr,psfc
  REAL,dimension(kts:kte),intent(in) :: T, Q, P
  REAL,intent(in)                    :: hbelow
  REAL,intent(out)                   :: pbelow
  REAL                 :: SLP
  INTEGER              :: I, J, K, KLO, KHI
  REAL                 :: PL, T0, TS, xx,TLO, THI, TL
                                               
  xx=lapse*Rd/gg                           
                                                                      
! COMPUTE PRESSURE AT PCONST MB ABOVE SURFACE (PL)              
                                                                       
      if (hbelow > terr) then
          WRITE(6,*) "ERROR in Pbelow heigth must be lower than terrain"
          WRITE(6,*) "Height = ",hbelow,"hsfc   = ",terr
          stop 551                                          
      endif
      
      if (hbelow <= 0. .OR. terr <= 0.) then
        Pbelow = Psfc
        return
      endif

      PL  = Psfc - Pconst                                        

! ... FIND 2 LEVELS ON SIGMA SURFACES SURROUNDING PL AT EACH I,J    

      klo = 0
      k_loop: do k=kts, kte-1
         if ((p(k) >= pl) .and. (p(k+1) < pl)) then
            khi = k+1
            klo = k
            exit k_loop
         endif
      enddo k_loop

      IF(klo < 1) THEN                                      
         WRITE(6,*)'ERROR get PRESSURE ',PCONST,' PB ABOVE THE SURFACE' 
         WRITE(6,*)'PL=',PL,'  PSFC=',psfc
         stop 610                                               
      END IF                                                         

! GET TEMPERATURE AT PL (TL), EXTRAPOLATE T AT SURFACE (TS)     
!     AND T AT SEA LEVEL (T0) WITH 6.5 K/KM LAPSE RATE              

      TLO=t(KLO) * (EPS+q(KLO))/(EPS*(1.+q(KLO)))
      THI=t(KHI) * (EPS+q(KHI))/(EPS*(1.+q(KHI)))
      TL=THI-(THI-TLO)*LOG(PL/p(KHI))/LOG(p(KLO)/p(KHI))               
      TS=TL*(Psfc/PL)**xx                           
      T0=TS +lapse*(terr-Hbelow)

! CORRECT SEA LEVEL TEMPERATURE IF TOO HOT                      
      if ( t0 >= tc ) then
        if ( ts <= tc ) then
          t0 = tc
        else
          t0 = tc-0.005*(ts-tc)**2
        endif
      endif

! COMPUTE SEA LEVEL PRESSURE                                    

      Pbelow=Psfc*EXP(2.*gg*(terr-Hbelow)/(Rd*(TS+T0)))

END SUBROUTINE Get_Pbelow

subroutine to_zk(obs_v,mdl_v,kts,kte,interp_p,zk)

   implicit none

   integer, intent(in) :: interp_p   ! 1: interpolate by P, 0: interpolate by H
   Integer, intent(in) :: kts,kte
   real,    intent(in) :: obs_v
   real, dimension(kts:kte), intent(in)  :: mdl_v
   real, intent(out)   :: zk
   integer             :: k

   zk = missing_r

   if(interp_p == 1) then
      if (obs_v > mdl_v(kts) .or. obs_v < mdl_v(kte)) then
           if (obs_v > mdl_v(kts)) then  ! below the lowest level:
            zk=real(kts+1)-(obs_v-mdl_v(kts+1))/(mdl_v(kts)-mdl_v(kts+1))
          else if (obs_v < mdl_v(kte)) then  ! above the highest level:
            zk=real(kte-1)+(obs_v-mdl_v(kte-1))/(mdl_v(kte)-mdl_v(kte-1))
          endif
      else
        do k = kts,kte-1
          if(obs_v <= mdl_v(k) .and. obs_v >= mdl_v(k+1)) then
            zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
            exit
          endif
        enddo
      endif
   else if(interp_p == 0) then
      If (obs_v < mdl_v(kts) .or. obs_v > mdl_v(kte)) then
          if (obs_v < mdl_v(kts)) then   ! below the lowest level:
            zk=real(kts+1)-(obs_v-mdl_v(kts+1))/(mdl_v(kts)-mdl_v(kts+1))
          elseif (obs_v > mdl_v(kte)) then ! above the highest level:
            zk=real(kte-1)+(obs_v-mdl_v(kte-1))/(mdl_v(kte)-mdl_v(kte-1))
          endif
      Else
       do k = kts,kte-1
         if(obs_v >= mdl_v(k) .and. obs_v <= mdl_v(k+1)) then
            zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
            exit
         endif
       enddo
      Endif
   endif

end subroutine to_zk

subroutine toGrid (x, ib, ie, i, dx, dxm)
   
! Transfer obs. x to grid i and calculate its distance to grid i and i+1

   implicit none

   real, intent(in)  :: x
   integer, intent(in)  :: ib, ie
   real,    intent(out) :: dx, dxm
   integer, intent(out) :: i
   
   i = int (x)
   if (i < ib) then
      i = ib
   else if (i >= ie) then
      i = ie - 1
   endif
   dx = x - float(i)
   dxm= float(1) - dx

end subroutine toGrid

  SUBROUTINE find_hilo_ht(nlevs_ob, numsnd, height_data, height_qc, stn_elev,  &
                          hmax, max_ht, min_ht, lowest, highest)
!****************************************************************************
! Purpose: Find lowest and highest observations with good height data.
!****************************************************************************
  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: nlevs_ob            ! number of levels in sounding obs
  INTEGER, INTENT(IN)  :: numsnd              ! Max number of obs in sounding
  REAL, INTENT(IN)     :: height_data(numsnd)
  REAL, INTENT(IN)     :: height_qc(numsnd)
  REAL, INTENT(IN)     :: stn_elev            ! elevations of observations (meters)
  REAL, INTENT(IN)     :: hmax                ! maximum height(m) for ht adjustment
  REAL, INTENT(IN)     :: min_ht              ! minimum value for checking valid height
  REAL, INTENT(IN)     :: max_ht              ! maximum value for checking valid height
  INTEGER, INTENT(out) :: lowest              ! lowest ob in sounding with good height data
  INTEGER, INTENT(out) :: highest             ! highest ob below (or 1st above) hmax with good ht

! Local variables
  integer :: lev       ! loop counter over sounding levels

! Find lowest ob with good height data
  lowest = 999    ! default level
  find_lowest: DO lev = 1, nlevs_ob
    if(height_qc(lev).ge. -0.001 .and. height_data(lev).gt.min_ht &
       .and. height_data(lev).lt.max_ht+1000. &
       .and. (height_data(lev)-stn_elev) .le. hmax ) then
      lowest = lev
      EXIT find_lowest
    endif
  ENDDO find_lowest

! Find highest ob below hmax with good height data
  highest = -999
  If ( lowest .le. nlevs_ob) Then
    find_highest: DO lev = lowest, nlevs_ob
      if(height_qc(lev).ge. -0.001  .and. height_data(lev) .gt. min_ht &
         .and. height_data(lev).lt.max_ht+1000. &
         .and. (height_data(lev)-stn_elev) .ge. hmax ) then
       highest = lev
       EXIT find_highest
      endif
    ENDDO find_highest
  Endif
  END SUBROUTINE find_hilo_ht

  SUBROUTINE interp_ht(numsnd, lowest, highest, stn_elev, hmax, min_ht, max_ht, &
                       missing_r, height_data, height_qc, pressure_data,        &
                       temperature_data, u_met_data, v_met_data, rh_data,       &
                       pressure_qc, temperature_qc, u_met_qc, v_met_qc, rh_qc)
!****************************************************************************
! Purpose: Interpolate height between layers with good height data.
!****************************************************************************
   IMPLICIT NONE

  INTEGER, INTENT(IN) :: numsnd              ! Max number of obs in sounding
  INTEGER, INTENT(IN) :: lowest              ! lowest ob in sounding with good height data
  INTEGER, INTENT(IN) :: highest             ! highest ob below (or 1st above) hmax with good ht
  REAL, INTENT(IN)    :: stn_elev            ! elevations of observations (meters)
  REAL, INTENT(IN)    :: hmax                ! maximum height(m) for ht adjustment
  REAL, INTENT(IN)    :: min_ht              ! minimum value for checking valid height
  REAL, INTENT(IN)    :: max_ht              ! maximum value for checking valid height
  REAL, INTENT(IN)    :: missing_r           ! value for missing data
  REAL, INTENT(INOUT) :: height_data(numsnd)
  REAL, INTENT(INOUT) :: height_qc(numsnd)
  REAL, INTENT(IN)    :: pressure_data(numsnd)
  REAL, INTENT(INOUT) :: temperature_data(numsnd)
  REAL, INTENT(INOUT) :: u_met_data(numsnd)
  REAL, INTENT(INOUT) :: v_met_data(numsnd)
  REAL, INTENT(INOUT) :: rh_data(numsnd)
  REAL, INTENT(INOUT) :: pressure_qc(numsnd)
  REAL, INTENT(INOUT) :: temperature_qc(numsnd)
  REAL, INTENT(INOUT) :: u_met_qc(numsnd)
  REAL, INTENT(INOUT) :: v_met_qc(numsnd)
  REAL, INTENT(INOUT) :: rh_qc(numsnd)

! Local variables
  integer :: lev            ! loop counter over sounding levels
  integer :: ii             ! sub-loop counter over sounding levels
  Integer :: kadj_l         ! closest level below with good height data
  Integer :: kadj_h         ! closest level above with good height data

   Do lev = 1,highest
    kadj_l = 999
    kadj_h = -999
    If(height_qc(lev) .ge. -0.001 .and. height_data(lev) .gt. min_ht &
       .and. height_data(lev) .lt. max_ht &
       .and. (height_data(lev)-stn_elev) .le. hmax ) then
       kadj_l = lev
       kadj_h = lev
    Elseif(lowest .le. lev ) then
       find_low: Do ii=lev, 1,-1
        if(height_qc(ii) .ge. -0.001 .and. height_data(ii) .gt. min_ht &
           .and. height_data(ii) .lt. max_ht &
           .and. (height_data(ii)-stn_elev) .le. hmax ) then
         kadj_l = ii
         EXIT find_low
        endif
       Enddo find_low

       if(highest .gt. lev) then
         find_high: Do ii=lev+1, highest
          if(height_qc(ii) .ge. -0.001 .and. height_data(ii) .gt. min_ht &
            .and. height_data(ii) .lt. max_ht) then
           kadj_h = ii
           EXIT find_high
          endif
         Enddo find_high
       endif

       if(kadj_l .le. lev .AND. kadj_h .gt. lev .and. pressure_data(lev) .gt. 5.2) then
         call pphh2h(pressure_data(kadj_h),height_data(kadj_h), &
                     pressure_data(kadj_l),height_data(kadj_l), &
                     pressure_data(lev),height_data(lev))
         height_qc(lev) = 10.
       elseif(kadj_l .le. highest .AND. temperature_data(kadj_l).gt.200.) then
         call p2h(pressure_data(kadj_l),height_data(kadj_l), temperature_data(kadj_l), &
                     pressure_data(lev),height_data(lev))
       endif
    Else
      temperature_data(lev) = missing_r
      u_met_data(lev) = missing_r
      v_met_data(lev) = missing_r
      rh_data(lev)  = missing_r
      temperature_qc(lev) = missing_r
      u_met_qc(lev) = missing_r
      v_met_qc(lev) = missing_r
      rh_qc(lev)    = missing_r
    Endif
   Enddo   !end do lev=1,highest

  END SUBROUTINE interp_ht

  Subroutine P2h(p0,h0,t0,p1,h1)
   Implicit None
   Real,Parameter::lapse = 0.0065, xisu=0.1902
   Real::p0,t0,h0   !a known level obs
   Real:: p1,h1

   h1 = h0 + t0*(1.-EXP(xisu*LOG(p1/p0)))/lapse
  End Subroutine P2h

  Subroutine pphh2h(p1,h1,p2,h2,p0,h0)

   Implicit None
   Real:: p1,h1,p2,h2
   Real:: p0,h0
   Real:: weight
   
   weight = LOG(p0/p1)/LOG(p2/p1)
   h0 = weight*h2 + (1. - weight)*h1
  End Subroutine pphh2h

  Subroutine calc_winddir(uval,vval,dir)
! ======  subroutine to calculate wind direction =====
! WC: 2014-05-31
! subroutine to calculate meteorological wind direction in deg
! 0 deg = N
!
! input: uval - zonal wind
!        vval - meridional wind
!
! output: dir - meteorological wind direction (deg)

       implicit none

       real, intent(inout)  :: uval
       real, intent(inout)  :: vval
       real, intent(out) :: dir

       if (uval.eq.0.) uval=1.e-11
       if (vval.eq.0.) vval=1.e-11

       dir=atan(vval/uval)

       if ( (uval.ge.0.).and.(vval.ge.0.) ) then
        dir=270.-57.29578*dir
       elseif ( (uval.ge.0.).and.(vval.le.0.) ) then
        dir=270.+57.29578*abs(dir)
       elseif ( (uval.le.0.).and.(vval.le.0.) ) then
        dir=90.-57.29578*abs(dir)
       elseif ( (uval.le.0.).and.(vval.ge.0.) ) then
        dir=90.+57.29578*abs(dir)
       endif

       return
  End Subroutine calc_winddir

  REAL FUNCTION ht_to_p( h, pbbc, ppbc, z, ic, jc, dx, dy, tlow,              &
                         k_start, k_end, kds,kde, ims,ime, jms,jme, kms,kme )

!******************************************************************************
! Purpose: Interpolate pressure at a specified x (ic), y (jc), and height (h).
!          The input pressure column pbbc+ppbc (base and perturbn) must already
!          be horizontally interpolated to the x, y position. The subroutine
!          get_height_column is called here to horizontally interpolated the
!          3D height field z to get a height column at (iob, job).
!******************************************************************************

  IMPLICIT NONE

  REAL,    INTENT(IN)  :: h                                ! height value (m)
  INTEGER, INTENT(IN)  :: k_start, k_end                   ! loop bounds  
  INTEGER, INTENT(IN)  :: kds,kde                          ! vertical dim.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme        ! memory dims.
  REAL,    INTENT(IN)  :: pbbc(kds:kde)                    ! column base pressure (cb)
  REAL,    INTENT(IN)  :: ppbc(kds:kde)                    ! column pressure perturb (cb)
  REAL,    INTENT(IN)  :: z( ims:ime, kms:kme, jms:jme )   !ht(m) above sl on half-levels
  INTEGER, INTENT(IN)  :: ic                               ! i-coord of desired p
  INTEGER, INTENT(IN)  :: jc                               ! j-coord of desired p
  REAL,    INTENT(IN)  :: dx                               ! interp. fraction (x dir)
  REAL,    INTENT(IN)  :: dy                               ! interp. fraction (y dir)
!!! wuyh added 20140316
  REAL,    INTENT(IN)  :: tlow
  Real,Parameter::lapse = 0.0065, xisu=0.1902
!!! wuyh added 20140316

! Local variables
  INTEGER :: k               ! loop counter
  INTEGER :: klo             ! lower k bound
  REAL :: zhi                ! upper z bound for h
  REAL :: zlo                ! lower z bound for h
  REAL :: p                  ! interpolated pressure value
  REAL :: ln_p               ! log p
  REAL :: ln_plo             ! log plo
  REAL :: ln_phi             ! log phi
  REAL :: z_at_p( kms:kme )  ! height at p levels

! Get interpolated z column at ic, jc.
  call get_height_column(z, ic, jc, dx, dy, z_at_p,                   &
                         k_start, k_end, kds,kde,                     &
                         ims,ime, jms,jme, kms,kme )

! Now we have pbbc, ppbc, z_at_p, so compute p at h. First, find
! bounding layers klo and khi so that z_at_p(klo) <= h <= z_at_p(khi)

  ZLEVS: do k = k_start+1, k_end
    klo = k-1
    if(h .le. z_at_p(k)) then
      EXIT ZLEVS
    endif
  enddo ZLEVS
  zlo = z_at_p(klo)
  zhi = z_at_p(klo+1)

!write(6,'(a,i3,2(a,f8.3))') 'klo = ',klo,' zlo = ',zlo,' zhi = ',zhi
!write(6,'(2(a,f8.3))') 'plo = ',pbbc(klo+1) + ppbc(klo+1),' phi = ',pbbc(klo) +
!ppbc(klo)

! Interpolate natural log of pressure
  ln_plo = log( pbbc(klo+1) + ppbc(klo+1) )
  ln_phi = log( pbbc(klo) + ppbc(klo) )
  if(h.le.zlo) then

!!! wuyh modified 20140316
!!    ln_p = ln_phi    
    ln_p = ln_phi + LOG(1.+lapse*(zlo-h)/tlow)/xisu
!!! wuyh modified 20140316

  else if (h.ge.zhi) then
    ln_p = ln_plo     ! set to k=k_end pressure 
  else
    ln_p = ln_plo + (ln_phi-ln_plo)*((zhi-h)/(zhi-zlo))
  endif

! Return pressure
  p = exp(ln_p)
  ht_to_p = p
!  write(6,*) "wuyh3",zhi,p
  RETURN
  END FUNCTION ht_to_p


!!! wuyh added 20140316
  REAL FUNCTION ht_to_psur(pobs,t,h,terr)

  IMPLICIT NONE
  REAL,    INTENT(IN)  :: h                                ! height value (m)
  Real,    Intent(IN)  :: pobs,t,terr
  Real,Parameter::lapse = 0.0065,rovg = 29.2966

! Local variables
  REAL :: zdiff
  zdiff = h-terr
  ht_to_psur =  pobs*EXP(zdiff/rovg/(t+0.5*lapse*zdiff))
  END FUNCTION ht_to_psur

!!! wuyh added end 20140316

  REAL FUNCTION zlow( z, ic, jc, dx, dy,                    &
                         k_start, k_end, kds,kde, ims,ime, jms,jme, kms,kme )

!******************************************************************************
! Purpose: Interpolate pressure at a specified x (ic), y (jc), and height (h).
!          The input pressure column pbbc+ppbc (base and perturbn) must already
!          be horizontally interpolated to the x, y position. The subroutine
!          get_height_column is called here to horizontally interpolated the
!          3D height field z to get a height column at (iob, job).
!******************************************************************************

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: k_start, k_end                   ! loop bounds  
  INTEGER, INTENT(IN)  :: kds,kde                          ! vertical dim.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme        ! memory dims.
  REAL,    INTENT(IN)  :: z( ims:ime, kms:kme, jms:jme )   !ht(m) above sl on half-levels
  INTEGER, INTENT(IN)  :: ic                               ! i-coord of desired p
  INTEGER, INTENT(IN)  :: jc                               ! j-coord of desired p
  REAL,    INTENT(IN)  :: dx                               ! interp. fraction (x dir)
  REAL,    INTENT(IN)  :: dy                               ! interp. fraction (y dir)

! Local variables
  REAL :: z_at_p( kms:kme )  ! height at p levels

! Get interpolated z column at ic, jc.
  call get_height_column(z, ic, jc, dx, dy, z_at_p,                   &
                         k_start, k_end, kds,kde,                     &
                         ims,ime, jms,jme, kms,kme )
  zlow = z_at_p(1)
  End FUNCTION zlow

  SUBROUTINE get_height_column( z, ic, jc, dx, dy, z_at_p,                  &
                                k_start, k_end, kds,kde,                    &
                                ims,ime, jms,jme, kms,kme )
!*************************************************************************
! Purpose: Compute column height at ic, jc location on half-levels
!*************************************************************************

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: k_start, k_end                   ! Loop bounds  
  INTEGER, INTENT(IN)  :: kds,kde                          ! vertical dim.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme        ! memory dims.
  REAL,    INTENT(IN)  :: z( ims:ime, kms:kme, jms:jme )   ! ht (m) on half-levels
  INTEGER, INTENT(IN)  :: ic                               ! i-coord of desired p
  INTEGER, INTENT(IN)  :: jc                               ! j-coord of desired p
  REAL,    INTENT(IN)  :: dx                               ! interp. fraction (x dir)
  REAL,    INTENT(IN)  :: dy                               ! interp. fraction (y dir)
  REAL,    INTENT(OUT) :: z_at_p( kms:kme )                ! column height at p levels

! Local variables
  INTEGER :: k             ! loop counter

  do k = kds, kde
      z_at_p(k) =                                     &
         (1.-DY)*( (1.-DX)*z(IC,K,JC) +               &
                            DX *z(IC+1,K,JC) ) +      &
             DY* ( (1.-DX)*z(IC,K,JC+1) +             &
                            DX *z(IC+1,K,JC+1) )
  enddo

  END SUBROUTINE get_height_column

  SUBROUTINE calc_Qv( rh, T, P, Qv,rhw)
!*******************************************************************************
! Purpose: Calculate water vapor mixing ratio, Qv. This is done in one of two
!          ways, depending on the values of rh and the upper three digits rh_qc:
!          
!          CASE 1: If (abs(INT(qc/1000)).eq.777), we calculate Qv from
!                  pressure and dew-point temperature.
!          CASE 2: If (abs(INT(qc/1000)).eq.666), we calculate Qv from
!                  rh, pressure, and temperature. 
!
!          In each case, reset qc to 0.
!*******************************************************************************

  IMPLICIT NONE
  REAL,    INTENT(INout)  :: rh     ! relative humidity
  REAL,    INTENT(IN)     :: T      ! temperature (deg K)
  REAL,    INTENT(IN)     :: P      ! pressure (mbar)
  REAL,    INTENT(inOUT)  :: Qv     ! water vapor mixing ratio
  REAL,    INTENT(inOUT)  :: rhw    ! water vapor mixing ratio

!  real :: flag
  real :: Td       ! dew point temperature
  real :: qsw_t    ! mixing ratio T
  real :: qsw_td   ! mixing ratio Td (saturation)
  real :: qv_dummy ! not needed
  real, parameter :: eps = .62197  ! ratio of molecular wt of water to dry air 

! Set flag to upper 3 digits of qc 
!  flag = abs(NINT(qc/1000))

  IF(rh .ge. 0. .AND. rh .le. 1.) then               !Qv
     rhw = -660.
     go to 800
  Elseif(rh .ge. 200 .AND. rh .le. 302.1)then        ! rh2r conversion
     if(ABS(T -225.) .le. 125. .AND. P .gt. 0.1)then
        rhw = rh - 200.
        call rh2r(rhw,T,P,Qv,0)
     else
        rhw = -666
        Qv = -888866. 
        go to 800
     endif
  Elseif(rh .ge. 2000.)then                           ! Td
!    if(flag.eq.777) then  ! extract Td
!      Td = abs(qc)-abs(1000*NINT(qc/1000))
      Td = rh - 2000.
      iF(min(Td,T) .gt.100. .AND. Td .le. T) then
        if(P .gt. 0.1) then
! Check for good pressure and temperature values
!QSW(P,T)=3.799*EXP(17.269*(T-273.16)/(T-35.86))/P
!QSI(P,T)=3.799*EXP(21.874*(T-273.16)/(T-7.660))/P
!RHw = QSW(P,Td) /QSW(P,T)
!RHi = QSI(P,Td)/QSI(P,Td)
!rh = ( (actual vapor pressure) / (saturation vapor pressure) ) x 100
!         qsw_t  = 3.799*EXP(17.269*(T-273.16)/(T-35.86))/P     ! not necessary
!         qsw_td = 3.799*EXP(17.269*(Td-273.16)/(Td-35.86))/P
          qsw_t  = 3.799*exp(17.269*(T-273.16)/(T-35.86))
          qsw_td = 3.799*exp(17.269*(Td-273.16)/(Td-35.86))
          rhw = 100.*qsw_td/qsw_t      ! wuyh calculate it for write out
          Qv = qsw_td/(P-(qsw_t/eps))
        else
          Qv = -888865.
          rhw = -665.
        endif
      eLSE
          Qv = -888864.
          rhw = -664
      eNDIF
  Else
      Qv = -888863.
      rhw = -663.
  Endif
800 Return
   
  END SUBROUTINE calc_Qv

!*************************************************************************
  SUBROUTINE r2rhw(r,t,p,rh,iice)
!*************************************************************************

! convert mixing ratio to rh
! if iice=1, use saturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
      Implicit None
      REAL, intent(out)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(in)  :: r
      INTEGER, intent(in)  :: iice

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat

      eps=0.62197
      e0=6.1078
      eslcon1=17.2693882
      eslcon2=35.86
      esicon1=21.8745584
      esicon2=7.66
      t0=260.
      if(r .gt.-0.01)then
!       if(iice.eq.1.and.t.le.t0)then
!        esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
!       else
!        esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
!       endif
!       rsat=eps*esat/(p-esat)
      ESAT = 6.112 * EXP (17.67*(T-273.15)/(T-29.60))
      RSAT = 0.622 * ESAT /(P-ESAT)

       rh=r*100./rsat
       rh=MIN(rh,100.)
      else
       rh=-99999.
      endif
      return
  END SUBROUTINE r2rhw

  SUBROUTINE get_var_pair_indxs(ivar, MNP, ivar_list, num_terms, icount, indxs)
!------------------------------------------------------------------------------
!  PURPOSE: Get count and indices of occurences of value ivar in ivar_list
!------------------------------------------------------------------------------
  USE module_domain
  IMPLICIT NONE

  INTEGER,INTENT(IN)  :: ivar               ! nudge variable
  INTEGER,INTENT(IN)  :: MNP                ! max nudging terms
  INTEGER,INTENT(IN)  :: ivar_list(MNP)     ! nudge variable to pass to sub nudob 
  INTEGER,INTENT(IN)  :: num_terms          ! number of terms in lists
  INTEGER,INTENT(OUT) :: icount             ! number occurences of value ivar in ivar_list
  INTEGER,INTENT(OUT) :: indxs(MNP)         ! values of i for which ivar_list(i)=ivar

! Local variables
  integer :: i   ! loop counter for icount

  if(num_terms.gt.MNP) CALL abort('get_var_pair_indxs: More than MNP nudging terms!')

  icount = 0
  indxs = -999
  do i = 1, num_terms
    if(ivar_list(i).eq.ivar) then
      icount = icount + 1
      indxs(icount) = i
    endif
  enddo

  END SUBROUTINE get_var_pair_indxs

  SUBROUTINE get_ob_pair_indx(ob_vtype,ob_ntype, MNP, nterms, vlist, nlist, indx)
!------------------------------------------------------------------------------
!  PURPOSE: Get count and indices of occurences of value ivar in ivar_list
!------------------------------------------------------------------------------
  USE module_domain
  IMPLICIT NONE

  INTEGER,INTENT(IN)  :: ob_vtype           ! observation var type (1st pair member)
  INTEGER,INTENT(IN)  :: ob_ntype           ! observation ndg type (2nd pair member)
  INTEGER,INTENT(IN)  :: MNP                ! MAX number of terms in lists
  INTEGER,INTENT(IN)  :: nterms             ! number of terms in lists
  INTEGER,INTENT(IN)  :: vlist(nterms)      ! list of 1st members
  INTEGER,INTENT(IN)  :: nlist(nterms)      ! list of 2nd members
  INTEGER,INTENT(OUT) :: indx               ! values of i for which ivar_list(i)=ivar

! Local variables
  integer :: i   ! loop counter for icount

  if(nterms.gt.MNP) CALL abort('get_ob_pair_indx: More than MNP nudging terms!')

  indx = -999
  do i = 1, nterms
    if(vlist(i).eq.ob_vtype .and. nlist(i).eq.ob_ntype) then
      indx = i
    endif
  enddo

END SUBROUTINE get_ob_pair_indx

REAL FUNCTION encode_rh_Qqc(rh,Qqc)
!*******************************************************************************
! Purpose: Encode relative humidity rh and the qc value for moisture Q into
!          form AAABB, where AAA is the rh value (ranging from 0-100), and BB
!          is the qc for Q.
!*******************************************************************************
  IMPLICIT NONE
  REAL,    INTENT(IN)    :: rh            ! relative humidity
  REAL,    INTENT(IN)    :: Qqc           ! qc value for moisture Q

  encode_rh_Qqc = float(100*nint(rh)) + Qqc 

END FUNCTION encode_rh_Qqc

REAL FUNCTION retrieve_tt(xval)
!*******************************************************************************
! Purpose: Retrieve relative humidity rh, which has been encoded into xval.
!          Both the values of rh and Q qc have been encoded into xval in the
!          form AAABB, where AAA is the rh value (ranging from 0-100), and BB
!          is the qc for Q. A simple scaling by 100 is done to retrieve rh.
!*******************************************************************************
  IMPLICIT NONE
  REAL,    INTENT(IN)    :: xval            ! contains encoded rh and qcQ 
  retrieve_tt = float(int(xval/100000.))
END FUNCTION retrieve_tt

REAL FUNCTION retrieve_rh(xval)
!*******************************************************************************
! Purpose: Retrieve relative humidity rh, which has been encoded into xval.
!          Both the values of rh and Q qc have been encoded into xval in the
!          form AAABB, where AAA is the rh value (ranging from 0-100), and BB
!          is the qc for Q. A simple scaling by 100 is done to retrieve rh.
!*******************************************************************************
  IMPLICIT NONE
  REAL,    INTENT(IN)    :: xval            ! contains encoded rh and qcQ 
  retrieve_rh = float(int(xval/100.))
END FUNCTION retrieve_rh

REAL FUNCTION retrieve_Qqc(xval)
!*******************************************************************************
! Purpose: Retrieve the qc value for moisture, which has been encoded into xval.
!          Both the values of rh and Qqc have been encoded into xval in the
!          form AAABB, where AAA is the rh value (ranging from 0-100), and BB
!          is the qc for Q. A simple scaling by 100 followed by subtraction
!          is done to retrieve rh.
!*******************************************************************************
  IMPLICIT NONE
  REAL,    INTENT(IN)    :: xval            ! contains encoded rh and qcQ 
! Local variables
  real :: rh
  rh = int(xval/10.)
  retrieve_Qqc = xval - 10.*rh
END FUNCTION retrieve_Qqc

! ********** NO LONGER USED **********
REAL FUNCTION encode_radar_angles(hsa,vsa)
!*******************************************************************************
! Purpose: Encode the radar horizontal and vertical scan angles for Vr into
!          form AAABBBB, where AAA.0 is the horizontal scan angle hsa, ranging 
!          from 0.0-360.0, BB.BB is the vertical scan angle vsa, ranging from
!          0.00-90.00.
!*******************************************************************************
  IMPLICIT NONE
  REAL,    INTENT(IN)    :: hsa           ! horizontal scan angle
  REAL,    INTENT(IN)    :: vsa           ! vertical   scan angle

  encode_radar_angles = float(nint(100.*vsa)) + float(10000*nint(hsa))
  write(6,*) 'encoded radar angles value is ',encode_radar_angles

END FUNCTION encode_radar_angles

! ********** NO LONGER USED **********
REAL FUNCTION retrieve_radar_horizontal(xval)
!*******************************************************************************
! Purpose: Retrieve the radar horizontal scan angle.
!          NOTE: See function encode_radar_angles for format.
!*******************************************************************************
  IMPLICIT NONE
  REAL,    INTENT(IN)    :: xval            ! contains encoded radar angles
  retrieve_radar_horizontal = float(int(xval/10000.))
END FUNCTION retrieve_radar_horizontal

! ********** NO LONGER USED **********
REAL FUNCTION retrieve_radar_vertical(xval)
!*******************************************************************************
! Purpose: Retrieve the radar horizontal scan angle.
!          NOTE: See function encode_radar_angles for format.
!*******************************************************************************
  IMPLICIT NONE
  REAL,    INTENT(IN)    :: xval            ! contains encoded radar angles
! Local variables
  real :: hsa
  hsa = float(int(xval/10000.))
  retrieve_radar_vertical = (xval - 10000.*hsa)/100.
END FUNCTION retrieve_radar_vertical

  SUBROUTINE rh2r(rh,t,p,r,iice)

! convert rh to r
! if iice=1, use saturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
!
      REAL, intent(in)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(out) :: r
      INTEGER, intent(in)  :: iice

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat

      eps=0.62197
      e0=6.1078
      eslcon1=17.2693882
      eslcon2=35.86
      esicon1=21.8745584
      esicon2=7.66
!      t0=260.
      t0=273.15

!     print *,'rh2r input=',rh,t,p
      rh1=rh*.01

! wuyh added from osse 20101230
!       if(iice.eq.1.and.t.le.t0)then
!         esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
!       else
!         esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
!       endif
!!! osse exp
!      esat=10**(-2937.4/t-4.9283*log10(t)+23.5518)  ! in mb
!      if(t < 233. ) esat=10**(10.5553-2667./t)      ! saturation over ice
! wuyh added from osse 20101230 end
!      rsat=eps*esat/(p-esat)
!     print *,'rsat,esat=',rsat,esat
!      r=rh1*rsat
!      if(r.gt.1.) r = 1.
!      if(r.lt.0.) r = 0.
!      print *,'rh2r rh,t,p,r=',rh1,t,p,r

!!! wuyh using WRFDA formula 20120130
      ESAT = 6.112 * EXP (17.67*(T-273.15)/(T-29.60))
      RSAT = 0.622 * ESAT /(P-ESAT)
!!! wuyh using WRFDA formula end 20120130

!     print *,'rsat,esat=',rsat,esat
      r=rh1*rsat
      if(r.gt.1.) r = float(1)
      if(r.lt.0.) r = float(0)
!      print *,'in rh2r rh,t,p,r=',rh1,t,p,r

      return
  END SUBROUTINE rh2r


END MODULE module_fddaobs_utilities
