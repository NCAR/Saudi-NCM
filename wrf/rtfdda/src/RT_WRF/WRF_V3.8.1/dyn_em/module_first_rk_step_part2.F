!WRF:MEDIATION_LAYER:SOLVER

#define BENCH_START(A)
#define BENCH_END(A)

MODULE module_first_rk_step_part2

CONTAINS

  SUBROUTINE first_rk_step_part2 (   grid , config_flags              &
                             , moist , moist_tend               &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , adapt_step_flag , curr_secs      &
                             , psim , psih , wspd , gz1oz0 , chklowq &
                             , cu_act_flag , hol , th_phy        &
                             , pi_phy , p_phy , t_phy     &
                             , dz8w , p8w , t8w           &
                             , nba_mij, n_nba_mij         & !JDM
! ----->> BEGIN STAGE RAL3.8.1R0 SOURCE UPDATE 1 - Add argument rho_phy for lightning potential calculation, per Will Cheng
                             , nba_rij, n_nba_rij         & !JDM
                             , rho_phy                    & !WC
                             , ids, ide, jds, jde, kds, kde     &
! ----->> END STAGE RAL3.8.1R0 SOURCE UPDATE 1 - Add argument rho_phy for lightning potential calculation, per Will Cheng
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , imsx,imex,jmsx,jmex,kmsx,kmex    &
                             , ipsx,ipex,jpsx,jpex,kpsx,kpex    &
                             , imsy,imey,jmsy,jmey,kmsy,kmey    &
                             , ipsy,ipey,jpsy,jpey,kpsy,kpey    &
                             , k_start , k_end                  &
                            )
    USE module_state_description
    USE module_model_constants
    USE module_domain, ONLY : domain
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
#ifdef DM_PARALLEL
    USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, &
                          wrf_dm_maxval, wrf_err_message, local_communicator_x, local_communicator_y
    USE module_comm_dm, ONLY : halo_em_tke_c_sub,halo_em_tke_d_sub,halo_em_tke_e_sub            &
            ,halo_em_phys_pbl_sub,halo_em_phys_shcu_sub &
            ,halo_em_fdda_sub,halo_em_phys_diffusion_sub,halo_em_tke_3_sub &
            ,halo_em_tke_5_sub,halo_obs_nudge_sub,period_bdy_em_a1_sub,period_bdy_em_phy_bc_sub &
            ,period_bdy_em_fdda_bc_sub,period_bdy_em_chem_sub,halo_em_phys_cu_sub,halo_em_helicity_sub
#endif

    USE module_driver_constants
    USE module_diffusion_em, ONLY : phy_bc, cal_deform_and_div, compute_diff_metrics, &
                                    vertical_diffusion_2, horizontal_diffusion_2, calculate_km_kh, &
                                    tke_rhs, cal_helicity
    USE module_em, ONLY : calculate_phy_tend
    USE module_fddaobs_driver, ONLY : fddaobs_driver
    USE module_bc, ONLY : set_physical_bc3d, set_physical_bc2d
    USE module_physics_addtendc, ONLY : update_phy_ten

    USE module_sfs_driver !JDM
    USE module_stoch, ONLY : update_stoch_ten, perturb_physics_tend,RAND_PERT_UPDATE


    IMPLICIT NONE

    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           ips, ipe, jps, jpe, kps, kpe,     &
                           imsx,imex,jmsx,jmex,kmsx,kmex,    &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                           imsy,imey,jmsy,jmey,kmsy,kmey,    &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey



    LOGICAL ,INTENT(IN)                        :: adapt_step_flag
    REAL, INTENT(IN)                           :: curr_secs

    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT)   :: moist
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT)   :: moist_tend
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT)   :: chem
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT)   :: chem_tend
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer_tend
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar_tend
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_fdda3d),INTENT(INOUT)  :: fdda3d
    REAL    ,DIMENSION(ims:ime,1:1,jms:jme,num_fdda2d),INTENT(INOUT)      :: fdda2d
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: psim
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: psih
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: wspd
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: gz1oz0
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: chklowq
    LOGICAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: cu_act_flag
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: hol

    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: th_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: pi_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p_phy
! ----->> BEGIN STAGE RAL3.8.1R0 SOURCE UPDATE 2 - Declare rho_phy for lightning potential calculation, per Will Cheng
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rho_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: dz8w
! ----->> END STAGE RAL3.8.1R0 SOURCE UPDATE 2 - Declare rho_phy for lightning potential calculation, per Will Cheng
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p8w
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t8w

    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ru_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rv_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rw_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ph_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: tke_tend

    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: mu_tendf

    INTEGER , INTENT(IN)                          ::  k_start, k_end

!JDM
  INTEGER, INTENT(  IN ) :: n_nba_mij, n_nba_rij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_mij) &
  :: nba_mij

! ----->> BEGIN STAGE RAL3.8.1R0 SOURCE UPDATE 3 - Add MM5-like time series 1
  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_rij) &
  :: nba_rij

! Local

    ! ====== added for MM5-like time series ==========
    !        by WC: 2011-03-18
    REAL :: xtime_wrf_time_seconds    ! model time in sec
    ! ===============================================

    REAL, DIMENSION( ims:ime, jms:jme ) :: ht_loc
! ----->> END STAGE RAL3.8.1R0 SOURCE UPDATE 3 - Add MM5-like time series 1
    REAL :: scale_factor
    INTEGER, DIMENSION( ims:ime, jms:jme ) :: shadowmask 
    INTEGER                             :: ij
    INTEGER  num_roof_layers
    INTEGER  num_wall_layers
    INTEGER  num_road_layers
    INTEGER  iswater
    INTEGER  rk_step 

#if ( WRF_DFI_RADAR == 1 )
    INTEGER  i_start,i_end,j_start,j_end,i,j,k
#endif

 ! initialize all tendencies to zero in order to update physics
 ! tendencies first (separate from dry dynamics).

   rk_step = 1

      IF ((grid%skebs_on==1).and.(grid%id .EQ. 1 )) then
          ! update and backtransform T
          CALL RAND_PERT_UPDATE(grid,'T',                                     &
                          grid%SPTFORCS,grid%SPTFORCC,                        &
                          grid%SPT_AMP,grid%ALPH_T,                           &
                          ips, ipe, jps, jpe, kps, kpe,                       &
                          ids, ide, jds, jde, kds, kde,                       &
                          ims, ime, jms, jme, kms, kme,                       &
                          k_start, k_end,                                     &
                          imsx,imex,jmsx,jmex,kmsx,kmex,                      &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex,                      &
                          imsy,imey,jmsy,jmey,kmsy,kmey,                      &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey,                      &
                          grid%num_stoch_levels,grid%num_stoch_levels,        &
                          grid%num_stoch_levels,grid%num_stoch_levels,        &
                          config_flags%restart, grid%iseedarr_skebs,          &
                          grid%DX,grid%DY,grid%skebs_vertstruc,               &
                          grid%rt_tendf_stoch,                                &
                          grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV      )
          ! Update streamfunction, backtransform U
           CALL RAND_PERT_UPDATE(grid,'U',                                    &
                           grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,             &
                           grid%SPSTREAM_AMP,grid%ALPH_PSI,                   &
                           ips, ipe, jps, jpe, kps, kpe,                      &
                           ids, ide, jds, jde, kds, kde,                      &
                           ims, ime, jms, jme, kms, kme,                      &
                           k_start, k_end,                                    &
                           imsx,imex,jmsx,jmex,kmsx,kmex,                     &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,                     &
                           imsy,imey,jmsy,jmey,kmsy,kmey,                     &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey,                     &
                           grid% num_stoch_levels,grid% num_stoch_levels,     &
                           grid% num_stoch_levels,grid% num_stoch_levels,     &
                           config_flags%restart, grid%iseedarr_skebs,         &
                           grid%DX,grid%DY,grid%skebs_vertstruc,              &
                           grid%ru_tendf_stoch,                               &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV     )
          ! Don't update streamfunction, backtransform V
           CALL RAND_PERT_UPDATE(grid,'V',                                    &
                           grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,             &
                           grid%SPSTREAM_AMP,grid%ALPH_PSI,                   &
                           ips, ipe, jps, jpe, kps, kpe,                      &
                           ids, ide, jds, jde, kds, kde,                      &
                           ims, ime, jms, jme, kms, kme,                      &
                           k_start, k_end,                                    &
                           imsx,imex,jmsx,jmex,kmsx,kmex,                     &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,                     &
                           imsy,imey,jmsy,jmey,kmsy,kmey,                     &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey,                     &
                           grid% num_stoch_levels,grid% num_stoch_levels,     &
                           grid% num_stoch_levels,grid% num_stoch_levels,     &
                           config_flags%restart, grid%iseedarr_skebs,         &
                           grid%DX,grid%DY,grid%skebs_vertstruc,              &
                           grid%rv_tendf_stoch,                               &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT      )
       ENDIF !skebs_on 

     if ((grid%sppt_on==1).and.(grid%id .EQ. 1 )) then
          CALL RAND_PERT_UPDATE(grid,'T',                                     &
                          grid%SPPTFORCS,grid%SPPTFORCC,                      &
                          grid%SPPT_AMP,grid%ALPH_SPPT,                       &
                          ips, ipe, jps, jpe, kps, kpe,                       &
                          ids, ide, jds, jde, kds, kde,                       &
                          ims, ime, jms, jme, kms, kme,                       &
                          k_start, k_end,                                     &
                          imsx,imex,jmsx,jmex,kmsx,kmex,                      &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex,                      &
                          imsy,imey,jmsy,jmey,kmsy,kmey,                      &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey,                      &
                          grid%num_stoch_levels,grid%num_stoch_levels,        &
                          grid%num_stoch_levels,grid%num_stoch_levels,        &
                          config_flags%restart, grid%iseedarr_sppt,           &
                          grid%DX,grid%DY,grid%sppt_vertstruc,                &
                          grid%rstoch,                                        &
                          grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT       )
       ENDIF !sppt_on

      if ((grid%rand_perturb_on==1).and.(grid%id .EQ. 1 )) then
           CALL RAND_PERT_UPDATE(grid,'T',                                     &
                           grid%SPFORCS,grid%SPFORCC,                          &
                           grid%SP_AMP,grid%ALPH_RAND,                         &
                           ips, ipe, jps, jpe, kps, kpe,                       &
                           ids, ide, jds, jde, kds, kde,                       &
                           ims, ime, jms, jme, kms, kme,                       &
                           k_start, k_end,                                     &
                           imsx,imex,jmsx,jmex,kmsx,kmex,                      &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,                      &
                           imsy,imey,jmsy,jmey,kmsy,kmey,                      &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey,                      &
                           grid%num_stoch_levels,grid%num_stoch_levels,        &
                           grid%num_stoch_levels,grid%num_stoch_levels,        &
                           config_flags%restart, grid%iseedarr_rand_pert,      &
                           grid%DX,grid%DY,grid%rand_pert_vertstruc,           &
                           grid%RAND_PERT,                                     &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT       )
       ENDIF !rand_perturb_on

! calculate_phy_tend

BENCH_START(cal_phy_tend)
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call calculate_phy_tend' )
        CALL calculate_phy_tend (config_flags,grid%mut,grid%muu,grid%muv,pi_phy, &
                     grid%rthraten,                                    &
                     grid%rublten,grid%rvblten,grid%rthblten,          &
                     grid%rqvblten,grid%rqcblten,grid%rqiblten,        &
                     grid%rucuten,grid%rvcuten,grid%rthcuten,          &
                     grid%rqvcuten,grid%rqccuten,grid%rqrcuten,        &
                     grid%rqicuten,grid%rqscuten,                      &
                     grid%rushten,grid%rvshten,grid%rthshten,          &
                     grid%rqvshten,grid%rqcshten,grid%rqrshten,        &
                     grid%rqishten,grid%rqsshten,grid%rqgshten,        &
                     grid%RUNDGDTEN,grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RQVNDGDTEN, &
! ----->> BEGIN STAGE RAL3.8.1R0 SOURCE UPDATE 4 - Radar mods 1
                     grid%RMUNDGDTEN,                                  &
                     grid%RQRNDGDTEN, grid%RQSNDGDTEN, grid%RQGNDGDTEN,&
                     grid%RQCNDGDTEN, grid%RQINDGDTEN,                 &
                     scalar, scalar_tend, num_scalar,                  &
! ----->> END STAGE RAL3.8.1R0 SOURCE UPDATE 4 - Radar mods 1
                     tracer, tracer_tend, num_tracer,                  &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
                     grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
                     k_start    , min(k_end,kde-1)                     )

      ENDDO
      !$OMP END PARALLEL DO
BENCH_END(cal_phy_tend)

! tke diffusion

       IF(config_flags%diff_opt .eq. 2 .OR. config_flags%diff_opt .eq. 1) THEN

BENCH_START(comp_diff_metrics_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call compute_diff_metrics ' )
           CALL compute_diff_metrics ( config_flags, grid%ph_2, grid%phb, grid%z, grid%rdz, grid%rdzw, &
                                       grid%zx, grid%zy, grid%rdx, grid%rdy,                      &
                                       ids, ide, jds, jde, kds, kde,          &
                                       ims, ime, jms, jme, kms, kme,          &
                                       grid%i_start(ij), grid%i_end(ij),      &
                                       grid%j_start(ij), grid%j_end(ij),      &
                                       k_start    , k_end                    )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(comp_diff_metrics_tim)

#ifdef DM_PARALLEL
#  include "HALO_EM_TKE_C.inc"
#  include "PERIOD_BDY_EM_A1.inc"
#endif

BENCH_START(tke_diff_bc_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call bc for diffusion_metrics ' )
           CALL set_physical_bc3d( grid%rdzw , 'w', config_flags,           &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL set_physical_bc3d( grid%rdz , 'w', config_flags,            &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL set_physical_bc3d( grid%z , 'w', config_flags,              &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL set_physical_bc3d( grid%zx , 'e', config_flags,             &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL set_physical_bc3d( grid%zy , 'f', config_flags,             &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL set_physical_bc2d( grid%ustm, 't', config_flags,            &
                                   ids, ide, jds, jde,                 &
                                   ims, ime, jms, jme,                 &
                                   ips, ipe, jps, jpe,                 &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij)   )
          CALL set_physical_bc2d( grid%ust, 't', config_flags,              &
                                   ids, ide, jds, jde,                 &
                                   ims, ime, jms, jme,                 &
                                   ips, ipe, jps, jpe,                 &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij)   )
 
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(tke_diff_bc_tim)

BENCH_START(deform_div_tim)

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call cal_deform_and_div' )
           CALL cal_deform_and_div ( config_flags,grid%u_2,grid%v_2,grid%w_2,grid%div,  &
                                     grid%defor11,grid%defor22,grid%defor33,            &
                                     grid%defor12,grid%defor13,grid%defor23,            &
                                     nba_rij, n_nba_rij,                                & !JDM
                                     grid%u_base, grid%v_base,grid%msfux,grid%msfuy,    &
                                     grid%msfvx,grid%msfvy,grid%msftx,grid%msfty,       &
                                     grid%rdx, grid%rdy, grid%dn, grid%dnw, grid%rdz,   &
                                     grid%rdzw,grid%fnm,grid%fnp,grid%cf1,grid%cf2,     &
                                     grid%cf3,grid%zx,grid%zy,            &
                                     ids, ide, jds, jde, kds, kde,        &
                                     ims, ime, jms, jme, kms, kme,        &
                                     grid%i_start(ij), grid%i_end(ij),    &
                                     grid%j_start(ij), grid%j_end(ij),    &
                                     k_start    , k_end                  )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(deform_div_tim)

! Updraft helicity between output times

#ifdef DM_PARALLEL
#     include "HALO_EM_HELICITY.inc"
#endif

       IF ( ( config_flags%nwp_diagnostics .eq. 1 ) .OR. &
            ( ( config_flags%afwa_diag_opt .eq. 1 ) .AND. ( config_flags%afwa_severe_opt .EQ. 1 ) ) ) THEN
BENCH_START(helicity_tim)

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call cal_helicity' )
          CALL cal_helicity ( config_flags,grid%u_2,grid%v_2,grid%w_2,  &
                              grid%uh,                             &
                              grid%up_heli_max,                    &
                              grid%ph_2,grid%phb,                  &
                              grid%msfux,grid%msfuy,               &
                              grid%msfvx,grid%msfvy,               &
                              grid%ht,                             &
                              grid%rdx, grid%rdy, grid%dn, grid%dnw, grid%rdz, grid%rdzw,   &
                              grid%fnm,grid%fnp,grid%cf1,grid%cf2,grid%cf3,grid%zx,grid%zy, &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start    , k_end                  )
       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(helicity_tim)
       ENDIF

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_D.inc"
#endif

! calculate tke, kmh, and kmv

BENCH_START(calc_tke_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call calculate_km_kh' )
           CALL calculate_km_kh( config_flags,grid%dt,grid%dampcoef,grid%zdamp,         &
                                 config_flags%damp_opt,                                 &
                                 grid%xkmh,grid%xkmv,grid%xkhh,grid%xkhv,grid%bn2,      &
                                 grid%khdif,grid%kvdif,grid%div,                        &
                                 grid%defor11,grid%defor22,grid%defor33,grid%defor12,   &
                                 grid%defor13,grid%defor23,                             &
                                 grid%tke_2,p8w,t8w,th_phy,                             &
                                 t_phy,p_phy,moist,grid%dn,grid%dnw,                    &
                                 grid%dx,grid%dy,grid%rdz,grid%rdzw,                    &
                                 config_flags%mix_isotropic,num_moist,                  &
                                 grid%cf1, grid%cf2, grid%cf3, grid%warm_rain,          &
                                 grid%mix_upper_bound,                                  &
                                 grid%msftx, grid%msfty,                                &
                                 grid%zx, grid%zy,                                      & 
                                 ids,ide, jds,jde, kds,kde,                             &
                                 ims,ime, jms,jme, kms,kme,                             &
                                 grid%i_start(ij), grid%i_end(ij),                      &
                                 grid%j_start(ij), grid%j_end(ij),                      &
                                 k_start    , k_end                          )
         ENDDO
       !$OMP END PARALLEL DO
BENCH_END(calc_tke_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_E.inc"
#endif

       ENDIF

#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_PHY_BC.inc"
       IF ( config_flags%grid_fdda .eq. 1) THEN
#      include "PERIOD_BDY_EM_FDDA_BC.inc"
       ENDIF
#      include "PERIOD_BDY_EM_CHEM.inc"
#endif

BENCH_START(phy_bc_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call phy_bc' )
         CALL phy_bc (config_flags,grid%div,grid%defor11,grid%defor22,grid%defor33,            &
                      grid%defor12,grid%defor13,grid%defor23,      &
                      grid%xkmh,grid%xkmv,grid%xkhh,grid%xkhv,     &
                      grid%tke_2,grid%rho,                         &
                      grid%rublten, grid%rvblten,                  &
                      grid%rucuten, grid%rvcuten,                  &
                      grid%rushten, grid%rvshten,                  &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      ips, ipe, jps, jpe, kps, kpe,                &
                      grid%i_start(ij), grid%i_end(ij),            &
                      grid%j_start(ij), grid%j_end(ij),            &
                      k_start    , k_end                           )
       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(phy_bc_tim)

!JDM
IF ( ( config_flags%sfs_opt .GT. 0 ) .AND. ( config_flags%diff_opt .eq. 2 ) ) THEN

 CALL sfs_driver( grid, config_flags,     &
                  nba_mij, n_nba_mij,     & 
                  nba_rij, n_nba_rij      ) 

ENDIF

#ifdef DM_PARALLEL
!-----------------------------------------------------------------------
!
! MPP for some physics tendency, km, kh, deformation, and divergence
!
!                                                         * * * * * * *
!                                            * * * * *    * * * * * * *
!               *                     *      * * * * *    * * * * * * *
!             * + *      * + *        +      * * + * *    * * * + * * *
!               *                     *      * * * * *    * * * * * * *
!                                            * * * * *    * * * * * * *
!                                                         * * * * * * *
!
! (for PBL)
! rublten                  x
! rvblten                             x
!
! (for Cumulus)
! rucuten                  x
! rvcuten                             x
!
! (for Shallow Cumulus)
! rushten                  x
! rvshten                             x
!
! (for FDDA)
! rundgdten     x
! rvndgdten     x
!
! (for TKE3)
! tke_2                                          x               
! (for TKE5)
! tke_2                                                         x
!
! (for diff_opt >= 1)
! defor11                  x
! defor22                             x
! defor12       x
! defor13                  x
! defor23                             x
! div           x
! xkmv          x
! xkmh          x
! xkhv          x
! xkhh          x
! tke           x
!
!-----------------------------------------------------------------------
       IF ( config_flags%bl_pbl_physics .ge. 1 ) THEN
#      include "HALO_EM_PHYS_PBL.inc"
       ENDIF
       IF ( config_flags%shcu_physics .gt. 1 ) THEN
#      include "HALO_EM_PHYS_SHCU.inc"
       ENDIF
       IF ( config_flags%cu_physics == SASSCHEME      .or.   &
            config_flags%cu_physics == TIEDTKESCHEME  .or.   &
            config_flags%cu_physics == NTIEDTKESCHEME .or.   &
            config_flags%cu_physics == CAMZMSCHEME    .or.   &
            config_flags%cu_physics == MESO_SAS       .or.   &
            config_flags%cu_physics == NSASSCHEME ) THEN
#      include "HALO_EM_PHYS_CU.inc"
       ENDIF
       IF ( config_flags%grid_fdda .ge. 1) THEN
#      include "HALO_EM_FDDA.inc"
       ENDIF
       IF ( config_flags%diff_opt .ge. 1 ) THEN
#      include "HALO_EM_PHYS_DIFFUSION.inc"
       ENDIF

       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_3.inc"
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_5.inc"
       ELSE
         WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF
#endif

BENCH_START(update_phy_ten_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call update_phy_ten' )
#if ( WRF_DFI_RADAR == 1 )
         if (config_flags%cu_physics .gt. 0) then
           i_start = grid%i_start(ij)
           i_end   = min( grid%i_end(ij),ide-1 )
           j_start = grid%j_start(ij)
           j_end   = min( grid%j_end(ij),jde-1 )
           if (grid%dfi_stage == DFI_FWD ) &
                 CALL wrf_debug ( 200 , ' Zero out cu_physics' )
           DO j = j_start, j_end
           DO k = k_start, min( k_end,kde-1 ) - 1
           DO i = i_start, i_end
             if (grid%dfi_stage ==DFI_FWD  &
             .and. grid%dfi_tten_rad(i,k,j) >= 1.0e-7 .and.  &
                   grid%dfi_tten_rad(i,k,j) <= 10.) then
! zero out cu-param temp tendency
                grid%rthcuten(i,k,j) = 0.0
             endif
           ENDDO
           ENDDO
           ENDDO
         ENDIF
#endif
         CALL update_phy_ten(ph_tendf,t_tendf, ru_tendf, rv_tendf,moist_tend ,&
                           scalar_tend, mu_tendf,                           &
                           grid%rthraten,grid%rthblten,grid%rthcuten,grid%rthshten, &
                           grid%rublten,grid%rucuten,grid%rushten,          &
                           grid%rvblten,grid%rvcuten,grid%rvshten,          &
                           grid%rqvblten,grid%rqcblten,grid%rqiblten,       &
                           grid%rqniblten,                                  & !CAMUWPBL scheme
                           grid%rqvcuten,grid%rqccuten,grid%rqrcuten,       &
                           grid%rqicuten,grid%rqscuten,                     &
                           grid%rqcncuten,grid%rqincuten,                   & !BSINGH - Added two CU tends
                           grid%rqvshten,grid%rqcshten,grid%rqrshten,       &
                           grid%rqishten,grid%rqsshten,grid%rqgshten,       &
                           grid%rqcnshten,grid%rqinshten,                   &!BSINGH - Added two SHCU tends
                           grid%RUNDGDTEN,                                  &
                           grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RPHNDGDTEN,  &
! ----->> BEGIN STAGE RAL3.8.1R0 SOURCE UPDATE 5 - Radar mods 2
                           grid%RQVNDGDTEN,grid%RMUNDGDTEN,                 &
                           grid%RQRNDGDTEN,grid%RQSNDGDTEN,grid%RQGNDGDTEN,    &
                           grid%RQCNDGDTEN, grid%RQINDGDTEN,                   &
                           grid%rthfrten,grid%rqvfrten,                     &  ! fire
! ----->> END STAGE RAL3.8.1R0 SOURCE UPDATE 5 - Radar mods 2
                           num_moist,num_scalar,config_flags,rk_step,       &
                           grid%adv_moist_cond,                             &
                           ids, ide, jds, jde, kds, kde,                    &
                           ims, ime, jms, jme, kms, kme,                    &
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                               )

       END DO
       !$OMP END PARALLEL DO
BENCH_END(update_phy_ten_tim)

      IF (grid%skebs_on==1) then
          !$OMP PARALLEL DO   &
          !$OMP PRIVATE ( ij )
          DO ij = 1 , grid%num_tiles
               CALL wrf_debug ( 200 , ' call update_stoch_ten' )
                CALL update_stoch_ten(ru_tendf, rv_tendf, t_tendf,&
                               grid%ru_tendf_stoch,                          &
                               grid%rv_tendf_stoch,                          &
                               grid%rt_tendf_stoch,                          &
                               grid%mu_2 , grid%mub,                         &
                               ids, ide, jds, jde, kds, kde,                 &
                               ims, ime, jms, jme, kms, kme,                 &
                               grid%i_start(ij), grid%i_end(ij),             &
                               grid%j_start(ij), grid%j_end(ij),             &
                               k_start, k_end,                               &
                               grid%num_stoch_levels,grid%num_stoch_levels   )

           ENDDO
           !$OMP END PARALLEL DO
      ENDIF !skebs_on

      IF (grid%sppt_on==1) then
          !$OMP PARALLEL DO   &
          !$OMP PRIVATE ( ij )
          ! JB comment:  P_QV is in moist_tend(ims,kms,jms,2)
          DO ij = 1 , grid%num_tiles
                 call perturb_physics_tend(grid%gridpt_stddev_sppt,          &
                        grid%stddev_cutoff_sppt,grid%rstoch,                 &
                        ru_tendf,rv_tendf,t_tendf,moist_tend(ims,kms,jms,2), &
                        ids, ide, jds, jde, kds, kde,                        &
                        ims, ime, jms, jme, kms, kme,                        &
                        grid%i_start(ij), grid%i_end(ij),                    &
                        grid%j_start(ij), grid%j_end(ij),                    &
                        k_start, k_end,                                      &
                        grid%num_stoch_levels,grid%num_stoch_levels          )
           ENDDO
          !$OMP END PARALLEL DO
  ENDIF

#ifdef PLANET
       ! do rayleigh (and zonal-average newtonian) damping during
       ! first iteration of RK loop only

       IF ( (config_flags%damp_opt == 101) .OR. &
            (config_flags%damp_opt == 103)      ) THEN
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL damptop( grid%u_2, grid%v_2, grid%t_2, &
                         grid%mut, grid%muu, grid%muv, &
                         pi_phy,                                &
                         t_tendf, ru_tendf, rv_tendf, P2SI,     &
                         ids, ide, jds, jde, kds, kde,          &
                         ims, ime, jms, jme, kms, kme,          &
                         grid%i_start(ij), grid%i_end(ij),      &
                         grid%j_start(ij), grid%j_end(ij),      &
                         k_start, k_end                         )
         END DO
         !$OMP END PARALLEL DO
       END IF
#endif

       IF( config_flags%diff_opt .eq. 2 .and. config_flags%km_opt .eq. 2 ) THEN

BENCH_START(tke_rhs_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL tke_rhs  ( tke_tend,grid%bn2,                           &
                         config_flags,grid%defor11,grid%defor22,      &
                         grid%defor33,                                &
                         grid%defor12,grid%defor13,grid%defor23,      &
                         grid%u_2,grid%v_2,grid%w_2,grid%div,         &
                         grid%tke_2,grid%mut,                         &
                         th_phy,p_phy,p8w,t8w,grid%z,grid%fnm,        & 
                         grid%fnp,grid%cf1,grid%cf2,grid%cf3,         &     
                         grid%msftx,grid%msfty,grid%xkmh,             &
                         grid%xkmv,grid%xkhv,grid%rdx,grid%rdy,       &
                         grid%dx,grid%dy,grid%dt,grid%zx,grid%zy,     &
                         grid%rdz,grid%rdzw,grid%dn,                  &
                         grid%dnw,config_flags%mix_isotropic,         &
                         grid%hfx, grid%qfx, moist(ims,kms,jms,P_QV), &
                         grid%ustm, grid%rho,                         &
                         ids, ide, jds, jde, kds, kde,                &
                         ims, ime, jms, jme, kms, kme,                &
                         grid%i_start(ij), grid%i_end(ij),            &
                         grid%j_start(ij), grid%j_end(ij),            &
                         k_start    , k_end                           )

         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(tke_rhs_tim)

       ENDIF

! calculate vertical diffusion first and then horizontal
! (keep this order)

       IF(config_flags%diff_opt .eq. 2) THEN

         IF (config_flags%bl_pbl_physics .eq. 0) THEN

BENCH_START(vert_diff_tim)
           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij )
           DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 200 , ' call vertical_diffusion_2 ' )
             CALL vertical_diffusion_2( ru_tendf, rv_tendf, rw_tendf,            &
                                      t_tendf, tke_tend,                         &
                                      moist_tend, num_moist,                      &
                                      chem_tend, num_chem,                       &
                                      scalar_tend, num_scalar,                     &
                                      tracer_tend, num_tracer,                     &
                                      grid%u_2, grid%v_2,                                  &
                                      grid%t_2,grid%u_base,grid%v_base,grid%t_base,grid%qv_base,          &
                                      grid%mut,grid%tke_2,config_flags, &
                                      grid%defor13,grid%defor23,grid%defor33,                   &
                                      nba_mij, num_nba_mij,          & !JDM
                                      grid%div, moist, chem, scalar,tracer,         &
                                      grid%xkmv, grid%xkhv, grid%xkmh, config_flags%km_opt,       & ! xkmh added Oct2013
                                      grid%fnm, grid%fnp, grid%dn, grid%dnw, grid%rdz, grid%rdzw, &
                                      grid%hfx, grid%qfx, grid%ustm, grid%rho,   &
                                      ids, ide, jds, jde, kds, kde,              &
                                      ims, ime, jms, jme, kms, kme,              &
                                      grid%i_start(ij), grid%i_end(ij),          &
                                      grid%j_start(ij), grid%j_end(ij),          &
                                      k_start, k_end                             )

           ENDDO
           !$OMP END PARALLEL DO
BENCH_END(vert_diff_tim)

         ENDIF
!
BENCH_START(hor_diff_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call horizontal_diffusion_2' )
           CALL horizontal_diffusion_2( t_tendf, ru_tendf, rv_tendf, rw_tendf, &
                                      tke_tend,                              &
                                      moist_tend, num_moist,                  &
                                      chem_tend, num_chem,                   &
                                      scalar_tend, num_scalar,                 &
                                      tracer_tend, num_tracer,                 &
                                      grid%t_2, th_phy,                           &
                                      grid%mut, grid%tke_2, config_flags,              &
                                      grid%defor11, grid%defor22, grid%defor12,             &
                                      grid%defor13, grid%defor23,   &
                                      nba_mij, num_nba_mij,         & !JDM
                                      grid%div,                     &
                                      moist, chem, scalar,tracer,               &
                                      grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx,  &
                                      grid%msfty, grid%xkmh, grid%xkhh, config_flags%km_opt,     &
                                      grid%rdx, grid%rdy, grid%rdz, grid%rdzw,                   &
                                      grid%fnm, grid%fnp, grid%cf1, grid%cf2, grid%cf3,          &
                                      grid%zx, grid%zy, grid%dn, grid%dnw, grid%rho,             &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(hor_diff_tim)
       ENDIF

! ----->> BEGIN STAGE RAL3.8.1R0 SOURCE UPDATE 6 - Obs nudging driver arguments; Add MM5-like time series 2, add lightning potential (per Will Cheng)
       IF ( grid%obs_nudge_opt .EQ. 1 .AND. grid%xtime <= grid%fdda_end ) THEN
# ifdef DM_PARALLEL
#       include "HALO_OBS_NUDGE.inc"
#endif
!***********************************************************************
! This section for obs nudging

         DO ij = 1 , grid%num_tiles

           CALL fddaobs_driver (grid, grid%grid_id, grid%domdesc,       &
                   model_config_rec%grid_id,                            &
                   model_config_rec%parent_id, config_flags%restart,    &
                   grid%obs_nudge_opt,                                  &
                   grid%obs_ipf_errob,                                  &
                   grid%obs_ipf_nudob,                                  &
                   grid%fdda_start,                                     &
                   grid%fdda_end,                                       &
                   grid%obs_nudge_wind,                                 &
                   grid%obs_nudge_temp,                                 &
                   grid%obs_nudge_mois,                                 &
                   grid%obs_nudge_vr, grid%obs_nudge_rf,                &
                   grid%obs_nudge_pstr,                                 &
                   grid%obs_coef_wind,                                  &
                   grid%obs_coef_temp,                                  &
                   grid%obs_coef_mois,                                  &
                   grid%obs_coef_vr,grid%obs_coef_rf,                   &
                   grid%obs_coef_pstr,                                  &
                   grid%obs_rinsig,                                     &
                   grid%obs_npfi,                                       &
                   grid%obs_ionf,                                       &
                   grid%obs_prt_max,                                    &
                   grid%obs_prt_freq,                                   &
                   grid%obs_idynin,                                     &
                   grid%obs_dtramp,                                     &
                   grid%parent_grid_ratio,                              &
                   grid%max_dom, grid%itimestep,                        &
                ! yliu 20090726
                   grid%xtime,                                          &
                   grid%dt, grid%gmt, grid%julday, grid%fdob,           &
                   grid%obs_dmax_nobs,                                  &
! FCV additions begin
                   grid%qc_internal, grid%obs_qc_domain,                &
                   grid%tolerance_p, grid%tolerance_q,                  &
                   grid%tolerance_t, grid%tolerance_u,                  &
                   grid%tolerance_p_sound, grid%tolerance_q_sound,      &
                   grid%tolerance_t_sound, grid%tolerance_u_sound,      &
! FCV additions end
                   model_config_rec%obs_vobs_dim,                       &
                   model_config_rec%obs_errf_dim,                       &
                   model_config_rec%nobs_ndg_vars,                      &
                   grid%obs_dmax_nkalman,                               &
                   model_config_rec%nobs_scans,                         &
                   grid%fdob%nstat, grid%fdob%vobs,                     &
                   grid%fdob%vobs_qc,                                   &
                   grid%fdob%errf,                                      &
                   grid%dx, grid%kpbl,grid%ht,                          &
                   grid%mut, grid%muu, grid%muv,                        &
                   grid%msftx, grid%msfty,                              &
                   grid%msfux, grid%msfuy,                              &
                   grid%msfvx, grid%msfvy,                              &
                   p_phy, t_tendf,                                      &
                   grid%u_2, grid%v_2, grid%t_2,                        &
                   moist(ims,kms,jms,P_QV),                             &
                   grid%w_2,                                            &
                   moist(ims,kms,jms,P_QR),                             &
                   grid%pb, grid%p_top, grid%p,                         &
                   grid%phb, grid%ph_2,                                 &
                   grid%th2,                                            &
                   grid%uratx, grid%vratx, grid%tratx,                  &
                   ru_tendf, rv_tendf,                                  &
                   moist_tend(ims,kms,jms,P_QV), grid%obs_savwt,        &
                   grid%z,                                              &
! wuyh added 20110630
                   grid%fdob%flagsnd, grid%fdob%flagbog,                &
                   grid%fdob%desobs,                                    &
                   grid%fdob%oblat, grid%fdob%oblon, grid%fdob%stnid,   &
                   grid%fdob%qcu,                                       &
                   grid%fdob%wudate,grid%fdob%wusource,                 &
                   grid%fdob%ndg_term_indxs(1,1),                       &
                   grid%fdob%ndg_term_indxs(1,2),                       &
                   grid%fdob%num_ndg_terms,                             &
                   config_flags,                                        &
! wuyh added 20110630 end
                   ids,ide, jds,jde, kds,kde,                           &
                   ims,ime, jms,jme, kms,kme,                           &
                   ips, ipe, jps, jpe, kps, kpe,                        &
                   grid%i_start(ij), min(grid%i_end(ij),ide-1),         &
                   grid%j_start(ij), min(grid%j_end(ij),jde-1),         &
                   k_start    , min(k_end,kde-1) )
!ajbij             k_start    , min(k_end,kde-1), grid%num_tiles     )

         ENDDO
       ENDIF  ! obs_nudge_opt .eq. 1
!
! ======== MM5 time series =============== 

       IF  ( (grid%grid_id .NE. 1).and.(grid%use_mm5_ts .EQ. 1) )  THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

         ! ===== added by WC: 2014-02-12 ============
         ! call MM5-like time series subroutine
         ! use msec
         xtime_wrf_time_seconds = grid%xtime*60.*1000.   ! msec

!Fix from Will Cheng, 2014-02-11
         if (grid%use_adaptive_time_step) then
          if (abs(int(xtime_wrf_time_seconds)-                                                       &
                  (1000*grid%xsec_mm5_ts)*int(xtime_wrf_time_seconds/(1000*grid%xsec_mm5_ts))).gt.   &
                               (1000*1.5*grid%dt)) goto 2100
!End fix from Will Cheng, 2014-02-11
         else
           if (mod(int(xtime_wrf_time_seconds),int(1000*grid%xsec_mm5_ts)).ne.0) goto 2100
         endif

         CALL timeseries_mm5(grid%grid_id,grid%xtime,                    &
                 ids,ide, jds,jde, kds,kde,                              &
                 ims,ime, jms,jme, kms,kme,                              &
                 ips, ipe, jps, jpe, kps, kpe,                           &
                 grid%i_start(ij), min(grid%i_end(ij),ide-1),            &
                 grid%j_start(ij), min(grid%j_end(ij),jde-1),            &
                 k_start    , min(k_end,kde-1),                          &
                 grid%dx, grid%xsec_mm5_ts,                              &
                 grid%julyr,grid%julday,grid%gmt,                        &
                 grid%xlat, grid%xlong, grid%cosa, grid%sina, grid%ht,   &
                 grid%u_2, grid%v_2, grid%t_2, grid%pb, grid%p,          &
                 grid%phb, grid%ph_2,                                    &
                 moist(ims,kms,jms,P_QV),                                &
                 grid%psfc, grid%t2, grid%rainc, grid%rainnc,            &
                 grid%hfx, grid%lh, grid%q2, grid%u10, grid%v10,         &
                 grid%emiss, grid%tsk, grid%ust,grid%swdown,grid%swnorm, &
                 grid%xlat_u, grid%xlong_u, grid%xlat_v, grid%xlong_v    &
             )

2100      CONTINUE
         ENDDO
         !$OMP END PARALLEL DO
       ENDIF

! ======== Lightning Potential Calculation, Will Cheng: 2015-07-29 ========
       IF  ( (grid%wlpi_use .EQ. 1) )  THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           xtime_wrf_time_seconds = grid%xtime*60.*1000.   ! msec

           if (grid%use_adaptive_time_step) then
             if (abs(int(xtime_wrf_time_seconds)-                                               &
                     (1000*grid%wlpi_dt)*int(xtime_wrf_time_seconds/(1000*grid%wlpi_dt))).gt.   &
                                (1000*1.5*grid%dt)) goto 2200
           else
             if (mod(int(xtime_wrf_time_seconds),int(1000*grid%wlpi_dt)).ne.0) goto 2200
           endif

           CALL calcwlpi(moist(ims,kms,jms,P_QV),moist(ims,kms,jms,P_QC),moist(ims,kms,jms,P_QR), &
                         moist(ims,kms,jms,P_QI),moist(ims,kms,jms,P_QS),moist(ims,kms,jms,P_QG), &
                         moist(ims,kms,jms,P_QH),                                                 &
                         grid%w_2,grid%z,dz8w,pi_phy,th_phy,p_phy,rho_phy,                        &
                         grid%wlpi,                                                               &
                         ids,ide, jds,jde, kds,kde,                                               &
                         ims,ime, jms,jme, kms,kme,                                               &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),                             &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),                             &
                         k_start    , min(k_end,kde-1))

2200       CONTINUE
         ENDDO
         !$OMP END PARALLEL DO
       ENDIF

! ======== maximumm reflectivity, Will Cheng: 2015-08-22 ========

       IF  ( (grid%lightning_option .NE. 0) .OR. (grid%wlpi_use .NE. 0) )  THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           xtime_wrf_time_seconds = grid%xtime*60.*1000.   ! msec

          if (grid%use_adaptive_time_step) then
            if (abs(int(xtime_wrf_time_seconds)-                                                   &
                    (1000*grid%wmxdbz_dt)*int(xtime_wrf_time_seconds/(1000*grid%wmxdbz_dt))).gt.   &
                               (1000*1.5*grid%dt)) goto 2300
          else
            if (mod(int(xtime_wrf_time_seconds),int(1000*grid%wmxdbz_dt)).ne.0)  goto 2300
          endif

           CALL diagnostic_output_calc_wrefl(                                                     &
                         ids,ide, jds,jde, kds,kde,                                               &
                         ims,ime, jms,jme, kms,kme,                                               &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),                             &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),                             &
                         k_start    , min(k_end,kde-1),                                           &
                         .true.,                                                                  &
                         grid%wmxdbz,grid%refl_10cm )

2300       CONTINUE
         ENDDO
         !$OMP END PARALLEL DO
       ENDIF

       ! ======== Subset Output, Will Cheng: 2016-07-30 ========
       IF  ( (config_flags%isigout .NE. 0) )  THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           xtime_wrf_time_seconds = grid%xtime*60.*1000.   ! msec

           if (grid%use_adaptive_time_step) then
             if (abs(int(xtime_wrf_time_seconds)-                                               &
                     (1000*grid%sigout_dt)*int(xtime_wrf_time_seconds/(1000*grid%sigout_dt))).gt.   &
                                (1000*1.5*grid%dt)) goto 2400
           else
             if (mod(int(xtime_wrf_time_seconds),int(1000*grid%sigout_dt)).ne.0) goto 2400
           endif

           ! ====== collect variables for first nsigout model levels ======
           ! U
           CALL collect_first_nsigout_u(                                    &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         grid%u_2,                                       &
                         grid%usig)

           ! ===== V ======
           CALL collect_first_nsigout_v(                                    &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         grid%v_2,                                       &
                         grid%vsig)

           ! ===== W =======
           CALL collect_first_nsigout_w(                                    &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         grid%w_2,                                       &
                         grid%wsig)

           ! ====== T ========
           CALL collect_first_nsigout_t(                                 &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         grid%t_2, grid%p, grid%pb,                      &
                         grid%tsig)

           ! ====== PH ========
           CALL collect_first_nsigout_w(                                 &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         grid%ph_2,                                      &
                         grid%phsig)

           ! ======= PHB =======
           CALL collect_first_nsigout_w(                                 &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         grid%phb,                                       &
                         grid%phbsig)

           ! ==== P =====
           CALL collect_first_nsigout_m(                                 &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         grid%p,                                         &
                         grid%psig)

           ! ===== PB ======
           CALL collect_first_nsigout_m(                                 &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         grid%pb,                                        &
                         grid%pbsig)

           ! ===== QVAPOR ====
           CALL collect_first_nsigout_m(                                 &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           &      
                         moist(ims,kms,jms,P_QV),                        &
                         grid%qvsig)

           ! ===== SLP ======
           CALL calc_wmslp(                                              &
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         ips, ipe, jps, jpe, kps, kpe,                   &
                         grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                         grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                         k_start    , min(k_end,kde-1),                  &
                         config_flags%nsigout,                           & 
                         grid%ht, grid%psfc, grid%phbsig, grid%phsig,    &
                            grid%qvsig, grid%tsig,                       &
                            grid%wslp)
 
           ! ====== QRAIN and QCLOUD =====
           IF (config_flags%mp_physics .GT. 0 ) then

            CALL collect_first_nsigout_m(                                 &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          ips, ipe, jps, jpe, kps, kpe,                   &
                          grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                          grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                          k_start    , min(k_end,kde-1),                  &
                          config_flags%nsigout,                           &      
                          moist(ims,kms,jms,P_QC),                        &
                          grid%qcsig)

            CALL collect_first_nsigout_m(                                 &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          ips, ipe, jps, jpe, kps, kpe,                   &
                          grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                          grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                          k_start    , min(k_end,kde-1),                  &
                          config_flags%nsigout,                           &      
                          moist(ims,kms,jms,P_QR),                        &
                          grid%qrsig)

           ENDIF

           ! ====== QICE =====
           IF ( (config_flags%mp_physics .EQ. 2 ) .OR.       &
                (config_flags%mp_physics .EQ. 4 ) .OR.       &
                ((config_flags%mp_physics .GE. 6) .AND.      &
                 (config_flags%mp_physics .LE. 32)) ) THEN

            CALL collect_first_nsigout_m(                                 &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          ips, ipe, jps, jpe, kps, kpe,                   &
                          grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                          grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                          k_start    , min(k_end,kde-1),                  &
                          config_flags%nsigout,                           &
                          moist(ims,kms,jms,P_QI),                        &
                          grid%qisig)

           ENDIF

           ! ====== QSNOW =====
           IF ( (config_flags%mp_physics .EQ. 2 ) .OR.       &
                (config_flags%mp_physics .EQ. 4 ) .OR.       &
                ((config_flags%mp_physics .GE. 5) .AND.      &
                 (config_flags%mp_physics .LE. 95)) ) THEN

            CALL collect_first_nsigout_m(                                 &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          ips, ipe, jps, jpe, kps, kpe,                   &
                          grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                          grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                          k_start    , min(k_end,kde-1),                  &
                          config_flags%nsigout,                           &
                          moist(ims,kms,jms,P_QS),                        &
                          grid%qssig)

           ENDIF

           ! ====== QGRAUP =====
           IF ( (config_flags%mp_physics .EQ. 2 ) .OR.       &
                ((config_flags%mp_physics .GE. 6) .AND.      &
                 (config_flags%mp_physics .LE. 10)) .OR.     &
                ((config_flags%mp_physics .GE. 16) .AND.      &
                 (config_flags%mp_physics .LE. 32)) ) THEN

            CALL collect_first_nsigout_m(                                 &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          ips, ipe, jps, jpe, kps, kpe,                   &
                          grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                          grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                          k_start    , min(k_end,kde-1),                  &
                          config_flags%nsigout,                           &
                          moist(ims,kms,jms,P_QG),                        &
                          grid%qgsig)

           ENDIF

           ! ====== QHAIL =====
           IF ( (config_flags%mp_physics .EQ. 9 ) .OR.       &
                (config_flags%mp_physics .EQ. 17 ) .OR.      &
                (config_flags%mp_physics .EQ. 18 ) .OR.      &
                (config_flags%mp_physics .EQ. 19 ) .OR.      &
                (config_flags%mp_physics .EQ. 32 ) ) THEN

            CALL collect_first_nsigout_m(                                 &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          ips, ipe, jps, jpe, kps, kpe,                   &
                          grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                          grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                          k_start    , min(k_end,kde-1),                  &
                          config_flags%nsigout,                           &
                          moist(ims,kms,jms,P_QH),                        &
                          grid%qhsig)
           ENDIF

           ! =======CLDFRA =====
           IF (grid%icloud.ne.0) THEN
            CALL collect_first_nsigout_m(                                 &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          ips, ipe, jps, jpe, kps, kpe,                   &
                          grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                          grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                          k_start    , min(k_end,kde-1),                  &
                          config_flags%nsigout,                           &
                          grid%cldfra,                                    &
                          grid%cldfrasig)
           ENDIF
           ! ==============================
           CALL calc_cf_slingo(                                           &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          ips, ipe, jps, jpe, kps, kpe,                   &
                          grid%i_start(ij), min(grid%i_end(ij),ide-1),    &
                          grid%j_start(ij), min(grid%j_end(ij),jde-1),    &
                          k_start    , min(k_end,kde-1),                  &
                          grid%pb, grid%p, moist(ims,kms,jms,P_QV),       &
                                grid%t_2,                                 &
                          grid%cfslingo)                  
2400       CONTINUE
         ENDDO
         !$OMP END PARALLEL DO
       ENDIF

  END SUBROUTINE first_rk_step_part2
!***********************************************************************

  ! ==== added by WC: 2010-03-18 =========
  ! output time series in a manner similar to what I have
  ! done for MM5
  !
  ! I recommend that you use the regular option. Only use this
  ! when you don't have enough memory.
  !
  ! ===== 2012-07-16
  ! added new variables for CEPRI project

  SUBROUTINE timeseries_mm5(inest,xtime,                                &
                   ids,ide, jds,jde, kds,kde,                           & ! domain dims
                   ims,ime, jms,jme, kms,kme,                           & ! memory dims 
                   ips, ipe, jps, jpe, kps, kpe,                        & ! patch  dims
                   its,ite, jts,jte, kts,kte,                           & ! tile   dims
                   dx, dt_step,                                         &
                   julyr, julday, gmt,                                  &
                   xlat, xlong, cosa, sina, ht,                         &
                   u_2, v_2, t_2, pb, p,                                &
                   phb, ph_2,                                           &
                   qv,                                                  &
                   psfc, t2, rainc, rainnc,                             &
                   hfx, lh, q2, u10, v10,                               &
                   emiss, tsk, ust, swdown, swnorm,                     &
                   xlat_u, xlon_u, xlat_v, xlon_v                        )

  USE module_dm

  IMPLICIT NONE

  ! Externals
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  INTEGER, EXTERNAL :: get_unused_unit

  CHARACTER (len = 250 ) :: file_tslist              ! name of timeseries list file 
  DATA file_tslist /'tslist.wrf_in_mm5format'/

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

  INTEGER, intent(in)  :: inest, julyr, julday
  REAL, intent(in)     :: xtime, dx, dt_step, gmt

  REAL,   INTENT(IN) ::   xlat( ims:ime , jms:jme )
  REAL,   INTENT(IN) ::  xlong( ims:ime , jms:jme )
  REAL,   INTENT(IN) ::   cosa( ims:ime , jms:jme )
  REAL,   INTENT(IN) ::   sina( ims:ime , jms:jme )
  REAL,   INTENT(IN) ::     ht( ims:ime , jms:jme )

  REAL,   INTENT(IN) ::   u_2( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) ::   v_2( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) ::   t_2( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) ::   pb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) ::    p( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) ::  phb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: ph_2( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) ::   qv( ims:ime, kms:kme, jms:jme )

  REAL,   INTENT(IN) ::   psfc( ims:ime , jms:jme )
  REAL,   INTENT(IN) ::     t2( ims:ime , jms:jme )
  REAL,   INTENT(IN) ::  rainc( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: rainnc( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: hfx( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: lh( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: q2( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: u10( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: v10( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: emiss( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: tsk( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: ust( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: swdown( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: swnorm( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: xlat_u( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: xlon_u( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: xlat_v( ims:ime , jms:jme )
  REAL,   INTENT(IN) :: xlon_v( ims:ime , jms:jme )

  ! ==============================================
  !
  INTEGER, PARAMETER :: nstn_max = 4000, nlvz_max = 150
  CHARACTER (len=26), DIMENSION(nstn_max) :: stn_full_name
  CHARACTER (len=5),  DIMENSION(nstn_max) :: stn_code_name

  REAL, DIMENSION(nstn_max) :: xlat_stn,   &
                               xlong_stn

  INTEGER, DIMENSION(nstn_max) :: i_stn, j_stn

  INTEGER :: nstn

  ! =======Local time series variables =================
! REAL, DIMENSION(nstn_max,nlvz_max) :: z_ts, p_ts, t_ts, u_ts, v_ts, q_ts
  REAL, DIMENSION(nlvz_max) :: z_ts, p_ts, t_ts, u_ts, v_ts, q_ts
  REAL :: ht_ts, psfc_ts, tsfc_ts, rainc_ts, rainnc_ts, up_lw, flux_mom

  ! =======================================
  ! new filename with I, J index
  CHARACTER (LEN=200) :: new_ts_filename
  CHARACTER (LEN=4) :: prefix_ts_iloc, prefix_ts_jloc
  CHARACTER (LEN=2) :: prefix_ts_dom

  CHARACTER (LEN=5) :: adt

  ! integer to identify the domain number from station name
  INTEGER :: igrid_from_station

  ! ===== unit number for time series output ======
  INTEGER, PARAMETER :: iunit = 601

  !INTEGER :: iunit

  INTEGER :: ii, jj, k, ix, iy
  REAL :: dlat_stn, dlong_stn, xlat_avg, dist_stn

  INTEGER :: myproc

  ! =========================================
   INTEGER  simulation_start_year   , &
            simulation_start_month  , &
            simulation_start_day    , &
            simulation_start_hour   , &
            simulation_start_minute , &
            simulation_start_second

  CHARACTER (len=19) simulation_start_date
  ! ==============================================
  LOGICAL :: tslist_mm5_exist, tsfile_exist

  ! =======
  INTEGER :: icheck = 0

  SAVE

  CALL wrf_get_myproc ( myproc )

!  write(*,*) 'inest, xtime =  ', inest, xtime, myproc

 IF (icheck.eq.0) THEN
  INQUIRE(FILE=file_tslist(1:len_trim(file_tslist)), EXIST=tslist_mm5_exist)

  IF (.not.tslist_mm5_exist) then
   write(*,*) 'missing tslist.wrf_in_mm5format'
   write(*,*) 'if using use_mm5_ts = 1, please supply tslist.wrf_in_mm5format'
   write(*,*) 'WRF stops now'
   stop
   return
  ENDIF
 ENDIF

! ===================================
       CALL nl_get_simulation_start_year   ( 1, simulation_start_year   )
       CALL nl_get_simulation_start_month  ( 1, simulation_start_month  )
       CALL nl_get_simulation_start_day    ( 1, simulation_start_day    )
       CALL nl_get_simulation_start_hour   ( 1, simulation_start_hour   )
       CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
       CALL nl_get_simulation_start_second ( 1, simulation_start_second )

       WRITE ( simulation_start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
               simulation_start_year,simulation_start_month,simulation_start_day,&
               simulation_start_hour,simulation_start_minute,simulation_start_second

       simulation_start_date = simulation_start_date(1:len_trim(simulation_start_date))//':00'

!       write(*,*) 'sim start : ',  simulation_start_year,   &
!                                   simulation_start_month,  &
!                                   simulation_start_day,    &
!                                   simulation_start_hour,   &
!                                   simulation_start_minute, &
!                                   simulation_start_second, &
!                                   simulation_start_date

! ====== read station list ============
 IF (icheck.eq.0) THEN
  nstn = 0
  open(unit=iunit+myproc, file=file_tslist(1:len_trim(file_tslist)), status='old')
  read(iunit+myproc,*)
  read(iunit+myproc,*)
  read(iunit+myproc,*)

  do ii=1,nstn_max
   read(iunit+myproc,100,end=200) stn_full_name(ii), stn_code_name(ii),  &
                 xlat_stn(ii), xlong_stn(ii),                     &
                 i_stn(ii), j_stn(ii)
   nstn = nstn + 1
100  format(a26,a5,1X,(f11.7,1x),(f12.7,1x),2(i4,1x))
   enddo

200 close(iunit+myproc)

  icheck = 1
 ENDIF

! ======= figure out which stations to write out =========
       do ii=1,nstn
        read(stn_code_name(ii)(5:5),*) igrid_from_station

        !dlat_stn, dlong_stn

        if ( (igrid_from_station.eq.inest).and.   &
             (i_stn(ii).ge.ips)           .and.   &
             (i_stn(ii).le.ipe)           .and.   &
             (j_stn(ii).ge.jps)           .and.   &
             (j_stn(ii).le.jpe) ) then

          dlat_stn = xlat(i_stn(ii),j_stn(ii))-xlat_stn(ii)
          dlong_stn = xlong(i_stn(ii),j_stn(ii))-xlong_stn(ii)
          xlat_avg = 0.5*(xlat(i_stn(ii),j_stn(ii))+xlat_stn(ii))
          dist_stn = 110.8e3*sqrt(dlat_stn**2 +                        &
                       (dlong_stn*cos(xlat_avg*atan(1.0)/45.))**2 )

          if (dist_stn .gt. dx ) then
           write(*,*) 'wrong i j in tslist '
           write(*,*)                                                  &
            'time series: inest, dlat_stn, dlong_stn, xlat_avg = ',    &
                     inest, dlat_stn, dlong_stn, xlat_avg

           write(*,*)                                                  &
            'xlat_stn, xlong_stn, xlat_mm5, xlon_mm5 = '
           write(*,700) xlat_stn(ii), xlong_stn(ii),                   &
             xlat(i_stn(ii),j_stn(ii)), xlong(i_stn(ii),j_stn(ii))
700        format(4(f8.3,1x))

           goto 1200
          endif

         ! ====== define arrays ============
         ix = i_stn(ii)
         iy = j_stn(ii)
         DO k = 1,kpe-1
          u_ts(k) = u_2(ix,k,iy)
          v_ts(k) = v_2(ix,k,iy)
          z_ts(k) = 0.5*(phb(ix,k,iy)+ph_2(ix,k,iy)+phb(ix,k+1,iy)+ph_2(ix,k+1,iy))/9.8
          p_ts(k) = pb(ix,k,iy)+p(ix,k,iy)
          t_ts(k) = t_2(ix,k,iy)
          q_ts(k) = qv(ix,k,iy)
         ENDDO

         ! ========= hypsometric equation to get height at half-point ========
         !z_ts(1) = ht(ix,iy) + (287*t_ts(1)/9.8)*alog(psfc(ix,iy)/p_ts(1))
         !
         !DO k = 2,kpe-1
         ! z_ts(k)=z_ts(k-1)+(287*0.5*(t_ts(k-1)+t_ts(k))/9.8)*alog(p_ts(k-1)/p_ts(k))
         !ENDDO
         ! ====================================================================
         ht_ts     = ht(ix,iy)
         psfc_ts   = psfc(ix,iy)
         tsfc_ts   = t2(ix,iy)

         rainc_ts  = rainc(ix,iy)
         rainnc_ts = rainnc(ix,iy)

         up_lw     = 5.67037321e-8*emiss(ix,iy)*tsk(ix,iy)**4
         flux_mom  = ust(ix,iy)*ust(ix,iy)

         ! ===================================
         write(prefix_ts_dom,'(i2.2)')  inest
         write(prefix_ts_iloc,'(i4.4)') i_stn(ii)
         write(prefix_ts_jloc,'(i4.4)') j_stn(ii)

         write(adt,710) int(dt_step)
710      format(i5.5)

         ! yliu 20131202  put the time series profiles on local disk under
         ! restart file dir
         new_ts_filename = 'restrts/'//stn_code_name(ii)(1:5)     &
                         //'.'//'d'//prefix_ts_dom    &
                         //'.'//'i'//prefix_ts_iloc   &
                         //'.'//'j'//prefix_ts_jloc   &
                         //'.'//'deltat'//adt(1:5)    &
                         //'.TS'

         !INQUIRE(FILE='new_ts_filename(1:len_trim(new_ts_filename))', EXIST=tsfile_exist)
         !
         !IF (.not.tsfile_exist) then
         ! open(unit=iunit, file=                            &
         !  new_ts_filename(1:len_trim(new_ts_filename)),    &
         !  form='unformatted',                              &
         !  status='new')
         !
         !  write(UNIT=iunit)                                &
         !      stn_full_name(ii)(1:25), inest, ii,          &
         !      stn_code_name(ii)(1:5),                      &
         !      xlat_stn(ii), xlong_stn(ii),                 &
         !      i_stn(ii), j_stn(ii),                        &
         !      xlat(ix,iy), xlong(ix,iy),                   &
         !      ht(ix,iy)
         ! close(iunit)
         !ENDIF


         ! ===== write out file =========
         open(unit=iunit+myproc, file=                     &
          new_ts_filename(1:len_trim(new_ts_filename)),    &
          status='unknown', position='append',             &
          form='unformatted' )

         !open(unit=iunit+myproc, file=                     &
         ! new_ts_filename(1:len_trim(new_ts_filename)),    &
         ! status='unknown', position='append',             &
         ! form='formatted' )

         !  write(UNIT=iunit)                                &
         !      stn_full_name(ii)(1:25), inest, ii,          &
         !      stn_code_name(ii)(1:5),                      &
         !      xlat_stn(ii), xlong_stn(ii),                 &
         !      i_stn(ii), j_stn(ii),                        &
         !      xlat(ix,iy), xlong(ix,iy),                   &
         !      ht(ix,iy)

         write(iunit+myproc)                                 &
              stn_full_name(ii)(1:26),                       &
              stn_code_name(ii)(1:5),                        &
              inest,                                         &
              xlat_stn(ii),                                  &
              xlong_stn(ii),                                 &
              psfc_ts,                                       &
              ht_ts,                                         &
              i_stn(ii),                                     &
              j_stn(ii),                                     &
              simulation_start_date,                         &
              xtime,                                         &
              (kpe-1),                                       &
              psfc_ts,         &   ! supposed to be psdot...not needed
              xlat_u(ix,iy),   &   ! anyways. does not matter
              xlon_u(ix,iy),   &
              xlat_v(ix,iy),   &
              xlon_v(ix,iy),   &
              hfx(ix,iy),      &
              lh(ix,iy),       &
              swdown(ix,iy),   &
              swnorm(ix,iy),   &
              up_lw,           &
              rainc(ix,iy),    &
              rainnc(ix,iy),   &
              t2(ix,iy),       &
              q2(ix,iy),       &
              u10(ix,iy),      &
              v10(ix,iy),      &
              flux_mom

800     FORMAT(A26,1X,A5,1X,I2.2,1X,F11.7,1X,F12.7,1X,                       &
                F9.2,1X,F9.2,1X,2(I4.4,1X),A19,1X,F15.2,1X,I3.3,             &
                1X,F9.2,1X,15(F12.7,1X))

         do k=1,kpe-1
          write(iunit+myproc) k, z_ts(k),                           &
                          p_ts(k),                                  &
                          (t_ts(k)+300.)*(p_ts(k)/100000.)**0.287,  &
                          u_ts(k),                                  &
                          v_ts(k),                                  &
                          q_ts(k)
1000     FORMAT(I3.3,1X,6(E14.7,1X))
         enddo

         close(iunit+myproc)

        endif

1200    continue

       enddo

  RETURN
  END SUBROUTINE timeseries_mm5
!===================================================================
 
  SUBROUTINE calcwlpi(qv,qc, qr, qi, qs, qg, qh                           &
                 ,w,z,dz8w,pi_phy,th_phy,p_phy,rho_phy                    &
                 ,lpi&
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
                                                                   )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN) ::                                          &
                                                              qv, &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              qg,qh

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::  w, z
      REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      dz8w,pi_phy,p_phy,rho_phy
      REAL, INTENT(IN),  DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      th_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)::      &
     &                      LPI




      REAL, DIMENSION(kms:kme)::    tempk,rh
      REAL, DIMENSION(kms:kme):: qv1d,p1d,rho1d,qti1d
      REAL, DIMENSION(kms:kme):: temp,qc1d,ql1d,qi1d,qs1d,qg1d,lpi1d
      REAL, DIMENSION(0:kme):: w1d,height
      REAL, DIMENSION(kms:kme):: e1d,height_t,w1d_t
      REAL z_full,qrs,teten,RELHUM,LOC,Td_850,Td_700,PC_DWPT
      INTEGER level
      REAL :: dt_lpi,t_base,t_top
      INTEGER I_COLLAPSE
      LOGICAL LOOK_T
      INTEGER I_START,I_END,J_START,J_END


  INTEGER ::               i,j,k
!-------------------------------------------------------------------
      DO j = jts,jte
      DO i = its,ite
        z_full=0.
        height(0)=z_full
        w1d(0)=w(i,1,j)
!     DO k = kts,kte-1
      DO k = kts,kte
          if (k.lt.kte-1)then
           w1d(k)=w(i,k+1,j)
          else
           w1d(k)=0.
          end if
          temp(k) = th_phy(i,k,j)*pi_phy(i,k,j)-273.16
          tempk(k) = th_phy(i,k,j)*pi_phy(i,k,j)
          qv1d(k)=qv(i,k,j)
          p1d(k)=p_phy(i,k,j)
          rho1d(k)=rho_phy(i,k,j)
          z_full=z_full+dz8w(i,k,j)
          height(k)=z_full
          qc1d(k)=qc(i,k,j)
          ql1d(k)=qc(i,k,j)+qr(i,k,j)
          qi1d(k)=qi(i,k,j)
          qti1d(k)=qi(i,k,j)+qs(i,k,j)+qg(i,k,j)+qh(i,k,j)
          qs1d(k)=qs(i,k,j)
!         qg1d(k)=qg(i,k,j)+qh(i,k,j)
! Hail doesn't usually charge
          qg1d(k)=qg(i,k,j)
! For conservative advection multiply by rho1d and divide by it below
      ENDDO
!     do k = kts,kte-1
      do k = kts,kte
       height_t(k)=0.5*(height(k-1)+height(k))
       w1d_t(k)=0.5*(w1d(k-1)+w1d(k))
      end do
      t_base=-0
      t_top=-20
      call calc_wlpi(ql1d,qi1d,qs1d,qg1d,w1d,temp,height,lpi(i,j),t_base,t_top,kme,kte)
      END DO
      END DO
      return
      end subroutine calcwlpi
      subroutine &
     &  calc_wlpi(ql3d,qi3d,qs3d,qg3d,w3d,t3d,height,lpi,t_base,t_top,kme,kte)
      implicit none
      integer kme,kte
      real t_base,t_top
      real ql3d(kme)
      real qg3d(kme)
      real qi3d(kme)
      real qs3d(kme)
      real w3d(0:kme)
      real t3d(kme)
      real height(0:kme)
      real lpi
      real del_z(kme)
      real w_ave(kme)
      integer ic,jc,icnt,i,j,k,i_collapse
      real i_dist,j_dist,del_z_tot
      real top, bot
      real num,den,ave_z
      real num_s,den_s
      real num_i,den_i
      real q_isg
      icnt=0
      do k=1,kte
        top=height(k)
        bot=height(k-1)
        del_z(k)=top-bot
        w_ave(k)=0.5*(w3d(k)+w3d(k-1))
      end do
!
!     Check for collapsing cell
! Here, we don't check, since it requires a halo.
      ave_z=0
      del_z_tot=0
      lpi=0
!     do k=1,kte-1
      do k=1,kte
       if (t3d(k).le.t_base.and.t3d(k).gt.t_top)then ! set temp range

        den_i = qi3d(k)+qg3d(k)
        den_s = qs3d(k)+qg3d(k)
        if (qs3d(k).eq.0.or.qg3d(k).eq.0.)then !checks for zeroes
         den_s=10000.
         num_s = 0.
        else
         num_s = sqrt(qs3d(k)*qg3d(k))
        end if
        if (qi3d(k).eq.0.or.qg3d(k).eq.0.)then  ! checks for zeroes
         den_i=10000.
         num_i = 0.
        else
         num_i = sqrt(qi3d(k)*qg3d(k))
        end if
        q_isg = qg3d(k)*(num_i/den_i+num_s/den_s)  ! ice "fract"-content

        if (ql3d(k).eq.0.or.q_isg.eq.0)then
          num=0
          den=10000.
        else
         num = sqrt(ql3d(k)*q_isg)
         den = ql3d(k)+q_isg
        end if
        del_z_tot=del_z_tot+del_z(k)
        if (num.gt.0)then
         ave_z=ave_z+del_z(k)*(2.*num/den)*w_ave(k)**2 ! lightning potential index J/unit-mass
        end if
       end if
      end do
!
      if (del_z_tot.eq.0)del_z_tot=100000
      lpi=ave_z/del_z_tot

!
      return
      end subroutine calc_wlpi
!===================================================================
   SUBROUTINE diagnostic_output_calc_wrefl(                           &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      its,ite, jts,jte, kts,kte,                      & ! tile dims
                      diagflag,                                       &
                      refd_max,refl_10cm                              &
                                                                     )
!----------------------------------------------------------------------


   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DIAGFLAG      logical flag to indicate if this is a history output time
!-- REF_MAX       max derived radar reflectivity
!-- REFL_10CM     model computed 3D reflectivity
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      its,ite, jts,jte, kts,kte

   LOGICAL,   INTENT(IN   )    ::   diagflag


   INTEGER :: i,j,k

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) ::   &
                                                      refl_10cm

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::        &
                                                       refd_max

       DO j=jts,jte
       DO i=its,ite
         refd_max(i,j)    = -35.
       ENDDO
       ENDDO

     DO j=jts,jte
     DO k=kts,kte
     DO i=its,ite

! Calculate the max radar reflectivity between output times

       IF ( refl_10cm(i,k,j) .GT. refd_max(i,j) ) THEN
         refd_max(i,j) = refl_10cm(i,k,j)
       ENDIF
     ENDDO
     ENDDO
     ENDDO


   END SUBROUTINE diagnostic_output_calc_wrefl

  ! ==== subroutine to save the first five model levels ===
  ! U point
  SUBROUTINE collect_first_nsigout_u(                                   &
                   ids,ide, jds,jde, kds,kde,                           & ! domain dims
                   ims,ime, jms,jme, kms,kme,                           & ! memory dims 
                   ips, ipe, jps, jpe, kps, kpe,                        & ! patch  dims
                   its,ite, jts,jte, kts,kte,                           & ! tile   dims
                   nsigout,                                             &
                   u_2,                                                 &
                   u_sig)

  IMPLICIT NONE

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, intent(in)  :: nsigout                    ! number of vertical levels to output

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), INTENT(IN) :: u_2

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::  &  
                       u_sig( ims:ime, nsigout, jms:jme )

  INTEGER :: i, j, k

  IF (nsigout .GT. (kme-1) ) THEN
   PRINT *, 'nsigout .GT. (kme-1) : STOP'
   STOP 
  ENDIF
 
  DO i=its,ite
   u_sig(i,1:nsigout,jts:jte) = 0.5*(u_2(i,1:nsigout,jts:jte)+u_2(i+1,1:nsigout,jts:jte))
  ENDDO

  RETURN
  END SUBROUTINE collect_first_nsigout_u

  ! ==== subroutine to save the first five model levels ===
  ! V point
  SUBROUTINE collect_first_nsigout_v(                                   &
                   ids,ide, jds,jde, kds,kde,                           & ! domain dims
                   ims,ime, jms,jme, kms,kme,                           & ! memory dims 
                   ips, ipe, jps, jpe, kps, kpe,                        & ! patch  dims
                   its,ite, jts,jte, kts,kte,                           & ! tile   dims
                   nsigout,                                             &
                   v_2,                                                 &
                   v_sig)

  IMPLICIT NONE

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, intent(in)  :: nsigout                    ! number of vertical levels to output

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), INTENT(IN) :: v_2

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::  &  
                       v_sig( ims:ime, nsigout, jms:jme )

  INTEGER :: i, j, k

  IF (nsigout .GT. (kme-1) ) THEN
   PRINT *, 'nsigout .GT. (kme-1) : STOP'
   STOP 
  ENDIF
 
  DO j=jts,jte
   v_sig(its:ite,1:nsigout,j) = 0.5*(v_2(its:ite,1:nsigout,j)+v_2(its:ite,1:nsigout,j+1))
  ENDDO

  RETURN
  END SUBROUTINE collect_first_nsigout_v

  ! ==== subroutine to save the first five model levels ===
  ! W point
  SUBROUTINE collect_first_nsigout_w(                                   &
                   ids,ide, jds,jde, kds,kde,                           & ! domain dims
                   ims,ime, jms,jme, kms,kme,                           & ! memory dims 
                   ips, ipe, jps, jpe, kps, kpe,                        & ! patch  dims
                   its,ite, jts,jte, kts,kte,                           & ! tile   dims
                   nsigout,                                             &
                   w_2,                                                 &
                   w_sig)

  IMPLICIT NONE

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, intent(in)  :: nsigout                    ! number of vertical levels to output

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), INTENT(IN) :: w_2

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::  &  
                       w_sig( ims:ime, nsigout, jms:jme )

  INTEGER :: i, j, k

  IF (nsigout .GT. (kme-1) ) THEN
   PRINT *, 'nsigout .GT. (kme-1) : STOP'
   STOP 
  ENDIF
 
  DO k=1,nsigout 
   w_sig(its:ite,k,jts:jte) = 0.5*(w_2(its:ite,k,jts:jte)+w_2(its:ite,k+1,jts:jte))
  ENDDO

  RETURN
  END SUBROUTINE collect_first_nsigout_w

  ! ==== subroutine to save the first five model levels ===
  ! M point
  SUBROUTINE collect_first_nsigout_m(                                   &
                   ids,ide, jds,jde, kds,kde,                           & ! domain dims
                   ims,ime, jms,jme, kms,kme,                           & ! memory dims 
                   ips, ipe, jps, jpe, kps, kpe,                        & ! patch  dims
                   its,ite, jts,jte, kts,kte,                           & ! tile   dims
                   nsigout,                                             &
                   m_2,                                                 &
                   m_sig)

  IMPLICIT NONE

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, intent(in)  :: nsigout                    ! number of vertical levels to output

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), INTENT(IN) :: m_2

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::  &  
                       m_sig( ims:ime, nsigout, jms:jme )

  INTEGER :: i, j, k

  IF (nsigout .GT. (kme-1) ) THEN
   PRINT *, 'nsigout .GT. (kme-1) : STOP'
   STOP 
  ENDIF
 
  m_sig(its:ite,1:nsigout,jts:jte) = m_2(its:ite,1:nsigout,jts:jte)

  RETURN
  END SUBROUTINE collect_first_nsigout_m

  ! ==== subroutine to save the first five model levels ===
  ! T point
  SUBROUTINE collect_first_nsigout_t(                                   &
                   ids,ide, jds,jde, kds,kde,                           & ! domain dims
                   ims,ime, jms,jme, kms,kme,                           & ! memory dims 
                   ips, ipe, jps, jpe, kps, kpe,                        & ! patch  dims
                   its,ite, jts,jte, kts,kte,                           & ! tile   dims
                   nsigout,                                             &
                   t_2, pp, pb,                                         &
                   t_sig)

  IMPLICIT NONE

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, intent(in)  :: nsigout                    ! number of vertical levels to output

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), INTENT(IN) :: t_2, pp, pb

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::  &  
                       t_sig( ims:ime, nsigout, jms:jme )

  INTEGER :: i, j, k

  IF (nsigout .GT. (kme-1) ) THEN
   PRINT *, 'nsigout .GT. (kme-1) : STOP'
   STOP 
  ENDIF
 
  t_sig(its:ite,1:nsigout,jts:jte) = (t_2(its:ite,1:nsigout,jts:jte)+300.)*    &
          ((pp(its:ite,1:nsigout,jts:jte)+pb(its:ite,1:nsigout,jts:jte))/100000.)**0.287

  RETURN
  END SUBROUTINE collect_first_nsigout_t

  ! ==== subroutine to calculate MSLP ===
  ! MSLP
  SUBROUTINE calc_wmslp(                                                &
                   ids,ide, jds,jde, kds,kde,                           & ! domain dims
                   ims,ime, jms,jme, kms,kme,                           & ! memory dims 
                   ips, ipe, jps, jpe, kps, kpe,                        & ! patch  dims
                   its,ite, jts,jte, kts,kte,                           & ! tile   dims
                   nsigout,                                             &
                   hgt, psfc, gzb, gzp, qvapor, t_phy,                  &
                   wslp)

  IMPLICIT NONE

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, intent(in)  :: nsigout                    ! number of vertical levels to output

  REAL, DIMENSION(ims:ime, jms:jme ), INTENT(IN) :: hgt, psfc
  REAL, DIMENSION(ims:ime, 1:nsigout, jms:jme ), INTENT(IN) :: gzb, gzp, qvapor, t_phy

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::  &  
                       wslp

  REAL :: z
  INTEGER :: i, j, k

  DO J = jts, jte
   DO I = its, ite
    z = (gzb(I,1,J)+gzp(I,1,J))/9.8
    CALL WMSLP( hgt(I,J), psfc(I,J), z, qvapor(I,1,J), t_phy(I,1,J), wslp(I,J))
   ENDDO
  ENDDO

  RETURN
  END SUBROUTINE calc_wmslp

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    NGSLP       NMC SEA LEVEL PRESSURE REDUCTION
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-02-02
!
! ABSTRACT:
!
!     THIS ROUTINE COMPUTES SEA LEVEL PRESSURE USING THE
!     HYDROSTATIC EQUATION WITH THE SHUELL CORRECTION.  THE
!     FOLLOWING IS BASED ON DOCUMENTATION IN SUBROUTINE
!     OUTHYDRO OF THE NGM:
!
!     THE FUNDAMENTAL HYDROSTATIC EQUATION IS
!        D(HEIGHT)
!        ---------  =  TAU = VIRTUAL TEMPERATURE * (RGAS/GRAVITY)
!        D (Z)
!      WHERE
!        Z = MINUS LOG OF PRESSURE (-LN(P)).
!
!     SEA-LEVEL PRESSURE IS COMPUTED FROM THE FORMULA
!        PRESS(MSL) = PRESS(GROUND) * EXP( F)
!     WHERE
!        F        = HEIGHT OF GROUND / MEAN TAU
!        MEAN TAU = ( TAU(GRND) + TAU(SL) ) / 2
!
!     IN THE NGM TAU(GRND) AND TAU(SL) ARE FIRST SET USING A
!     6.5DEG/KM LAPSE RATE FROM LOWEST MDL LEVEL.  THIS IS MODIFIED
!     BY A CORRECTION BASED ON THE CRITICAL TAU OF THE SHUELL
!     CORRECTION:
!                  TAUCR=(RGASD/GRAVITY) * 290.66
!  
!     1) WHERE ONLY TAU(SL) EXCEEDS TAUCR, CHANGE TAU(SL) TO TAUCR.
!
!     2) WHERE BOTH TAU(SL) AND TAU(GRND) EXCEED TAUCR,
!        CHANGE TAU(SL) TO TAUCR-CONST*(TAU(GRND)-TAUCR  )**2
!        WHERE CONST = .005 (GRAVITY/RGASD)
!  
!     THE AVERAGE OF TAU(SL) AND TAU(GRND) IS THEN USED TOGETHER
!     WITH THE GROUND HEIGHT AND PRESSURE TO DERIVE THE PRESSURE
!     AT SEA LEVEL.
!    
!     HEIGHT OF THE 1000MB SURFACE IS COMPUTED FROM THE MSL PRESSURE
!     FIELD USING THE FORMULA:
!    
!       P(MSL) - P(1000MB) = MEAN DENSITY * GRAVITY * HGT(1000MBS)
!    
!     WHERE P(MSL) IS THE SEA LEVEL PRESSURE FIELD WE HAVE JUST
!     COMPUTED.
!    
!
!     MEB 6/13/02: THIS CODE HAS BEEN SIMPLIFIED CONSIDERABLY FROM
!     THE ONE USED IN ETAPOST.  HORIZONTAL SMOOTHING HAS BEEN
!     REMOVED AND THE FIRST MODEL LEVEL IS USED RATHER
!     THAN THE MEAN OF THE VIRTUAL TEMPERATURES IN
!     THE LOWEST 30MB ABOVE GROUND TO COMPUTE TAU(GRND).
!    
!   . 
!    
! PROGRAM HISTORY LOG:
!   93-02-02  RUSS TREADON
!   98-06-08  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   01-11-02  H CHUANG - MODIFIED LINE 234 FOR COMPUTATION OF
!                         SIGMA/HYBRID SLP
!   01-12-18  H CHUANG - INCLUDED SMOOTHING ALONG BOUNDARIES TO BE
!                         CONSISTENT WITH MESINGER SLP
!   02-06-13  MIKE BALDWIN - WRF VERSION
!   06-12-18  H CHUANG - BUG FIX TO CORRECT TAU AT SFC
!   14-04-17  G CREIGHTON - MODIFIED TO INSERT INTO AFWA DIAGNOSTICS IN WRF
!    
!$$$ 

  SUBROUTINE WMSLP ( zsfc, psfc, zlev1, qlev1, tlev1, wslp )

      implicit none
     
     
!     DECLARE VARIABLES

      REAL,    INTENT ( IN )  :: zsfc         !~ Surface height ( m )
      REAL,    INTENT ( IN )  :: psfc         !~ Surface height ( m )
      REAL,    INTENT ( IN )  :: zlev1        !~ Level 1 height ( m )
      REAL,    INTENT ( IN )  :: qlev1        !~ Level 1 mixing ratio ( kg/kg )
      REAL,    INTENT ( IN )  :: tlev1        !~ Level 1 temperature ( K )

      REAL,    INTENT(INOUT)  :: wslp         !~ SLP (Pa)

      real,PARAMETER :: G=9.81
      real,PARAMETER :: GI=1./G
      real,PARAMETER :: RD=287.0
      real,PARAMETER :: ZSL=0.0
      real,PARAMETER :: TAUCR=RD*GI*290.66,CONST=0.005*G/RD
      real,PARAMETER :: GORD=G/RD,DP=60.E2
      real,PARAMETER :: GAMMA=6.5E-3

      real TVRT,TVRSFC,TAUSFC,TVRSL,TAUSL,TAUAVG
!    
!**********************************************************************
!     START NGSLP HERE.
!
         WSLP = PSFC
!
!        COMPUTE LAYER TAU (VIRTUAL TEMP*RD/G).
         TVRT = TLEV1*(1.0+0.608*QLEV1)
         !TAU  = TVRT*RD*GI
!    
!        COMPUTE TAU AT THE GROUND (Z=ZSFC) AND SEA LEVEL (Z=0)
!        ASSUMING A CONSTANT LAPSE RATE OF GAMMA=6.5DEG/KM.
         TVRSFC = TVRT + (ZLEV1 - ZSFC)*GAMMA
         TAUSFC = TVRSFC*RD*GI
         TVRSL  = TVRT + (ZLEV1 - ZSL)*GAMMA
         TAUSL  = TVRSL*RD*GI
!    
!        IF NEED BE APPLY SHEULL CORRECTION.
         IF ((TAUSL.GT.TAUCR).AND.(TAUSFC.LE.TAUCR)) THEN
            TAUSL=TAUCR
         ELSEIF ((TAUSL.GT.TAUCR).AND.(TAUSFC.GT.TAUCR)) THEN
            TAUSL = TAUCR-CONST*(TAUSFC-TAUCR)**2
         ENDIF
!    
!        COMPUTE MEAN TAU.
         TAUAVG = 0.5*(TAUSL+TAUSFC)
!    
!        COMPUTE SEA LEVEL PRESSURE.
         WSLP = PSFC*EXP(ZSFC/TAUAVG)

  END SUBROUTINE WMSLP

  ! ==== subroutine to calculate cloud fraction from Slingo ===
  ! 
  SUBROUTINE calc_cf_slingo(                                            &
                   ids,ide, jds,jde, kds,kde,                           & ! domain dims
                   ims,ime, jms,jme, kms,kme,                           & ! memory dims 
                   ips, ipe, jps, jpe, kps, kpe,                        & ! patch  dims
                   its,ite, jts,jte, kts,kte,                           & ! tile   dims
                   pb, p, qv, theta,                                    &
                   cfslingo)

  IMPLICIT NONE

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: ips,ipe, jps,jpe, kps,kpe  ! patch  dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), INTENT(IN) :: pb, p, qv, theta

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::  &  
                       cfslingo( ims:ime, jms:jme )

  REAL, DIMENSION(kme-1) :: rh, press

  REAL :: temp_k, es, qs

  REAL :: M, cf, cf_old


  INTEGER :: i, j, k

  DO j=jts,jte
   DO i=its,ite
     
    DO k=1,kme-1
     temp_k = (theta(i,k,j)+300.)*                     &
               ((pb(i,k,j)+p(i,k,j))/100000.)**0.287

     es=610.78*                                        &
             exp(17.269*(temp_k-273.16)/(temp_k-35.86))      ! saturation vapor pressure
                                                             ! in Pa
 
     qs=0.622*es/(pb(i,k,j)+p(i,k,j)-es)

     rh(k)= amax1(amin1(1.*qv(i,k,j)/qs,1.),0.0) ! calculate rh in fraction 

     press(k) = pb(i,k,j)+p(i,k,j)

    ENDDO

    ! ========
    cf = 0.

    DO k=1,kme-1

     if (press(k).lt.40000.) goto 1000

     if (press(k).ge.80000.) then
      M = 0.8
     endif

     if ( (press(k).lt.80000.).and.(press(k).ge.40000.) ) then
      M = 0.65
     endif

     cf_old = cf

     if (rh(k).lt.M) then
      cf = 0.
     endif

     if ( (rh(k).ge.M).and.(rh(k).lt.1.) ) then
      cf = ( (rh(k)-M)/(1.-M) )**2
     endif

     if (rh(k).ge.1) then
      cf = 1.
     endif

     cf = amax1(cf,cf_old)

    ENDDO

1000 if (cf.lt.0.) cf = 0.
     cfslingo(i,j) = cf

   ENDDO
  ENDDO

  RETURN
  END SUBROUTINE calc_cf_slingo

END MODULE module_first_rk_step_part2
! ----->> END STAGE RAL3.8.1R0 SOURCE UPDATE 6 - Obs nudging driver arguments; Add MM5-like time series 2, add lightning potential (per Will Cheng)

